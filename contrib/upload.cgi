#!/usr/bin/perl

use strict;
use File::Temp qw/ tempdir tempfile /;

# this is a sample cgi script to accept darcs patches via POST
# it simply takes patches and sends them using sendmail or
# places them in a Maildir style mailbox.

my $tmp_dir;        # temporary directory, when placing patches to maildir
                    # files are linked from $tmp_dir to $maildir
$tmp_dir = "/tmp";

# target email addresses--leave blank to use To: header in patch contents.
my $target_email;

# target repository for patch testing.  Leave blank to use DarcsURL header
# in patch contents.
my $target_repo;

my $sendmail_cmd;   # command to send patches with
$sendmail_cmd = "/usr/sbin/sendmail -i -t $target_email";

my $maildir;        # maildir to put patches to, replace sendmail
#$maildir = "/tmp/maildir";

my $patch_test_cmd; # command to test patches with
$patch_test_cmd = "darcs apply --dry-run --repodir 'TARGETREPO' 'TARGETPATCH'";

my $repo_get_cmd; # command to get testing repo
                  # used only when $target_repo is blank
$repo_get_cmd = "darcs get --lazy --repodir 'TARGETDIR' 'TARGETREPO'";


sub error_page {
    my ($m) = @_;
    print "Status: 500 Error accepting patch\n";
    print "Content-Type: text/plain\n\n";
    print($m || "There was an error processing your request");
    print "\n";
    exit 0;
}

sub success_page {
    print "Content-Type: text/plain\n\n";
    print "Thank you for your contribution!\n";
    exit 0;
}


if ($ENV{CONTENT_TYPE} eq 'message/rfc822') {
    my $m = start_message() or error_page("could not create temporary file");
    my $fh = $m->{fh};
    my ($totalbytes, $bytesread, $buffer);
    do {
        $bytesread = read(STDIN, $buffer, 1024);
        print $fh $buffer;
        $totalbytes += $bytesread;
    } while ($bytesread);
    my $r = end_message($m);
    $r ? error_page($r) : success_page();
} elsif ($ENV{CONTENT_TYPE}) {
    error_page("invalid content type, I expect something of message/rfc822");
} else {
    error_page("This url is for accepting darcs patches.");
}



sub maildir_file {
    my ($tmp_file) = @_;
    my $base_name = sprintf("patch-%d-%d-0000", $$, time());
    my $count = 0;
    until (link("$tmp_file", "$maildir/$base_name")) {
        $base_name =~ s/-(\d+)$/"-" . (1 + $1)/e;
        return undef if $count++ > 100;
    }
    return "$maildir/$base_name";
}

sub start_message {
    my ($fh, $fname) = tempfile("$tmp_dir/dpatch".'X'x8, UNLINK => 1) or
        return undef;
    return { fh => $fh, filename => $fname };
}

sub end_message {
    my ($m) = @_;
    close $m->{fh} or return "$!: $m->{filename} - Could not close filehandle";

    unless ($target_repo) {
        # Look for DarcsURL header
        my $darcsurl;
        open(MF,$m->{filename}) or return "$!: $m->{filename} - Could not open file";
        while (<MF>) {
            if (/^DarcsURL: (.+)$/) {
                $darcsurl = $1;
                last;
            }
        }
        close(MF);
        return "Could not find DarcsURL header" unless $darcsurl;

        my $test_dir = tempdir(CLEANUP => 1).'/repo' or
            return "$!: Could not create test directory";
        $repo_get_cmd =~ s/TARGETDIR/$test_dir/;
        $repo_get_cmd =~ s/TARGETREPO/$darcsurl/;
        system("$repo_get_cmd >/dev/null 2>/dev/null") == 0 or
            return "Could not get target repo: '$repo_get_cmd' failed";
        $target_repo = $test_dir;
    }
    $patch_test_cmd =~ s/TARGETREPO/$target_repo/;
    $patch_test_cmd =~ s/TARGETPATCH/$m->{filename}/;
    system("$patch_test_cmd >/dev/null 2>/dev/null") == 0 or
        return "Patch is not valid: '$patch_test_cmd' failed";

    if ($maildir) {
        maildir_file("$m->{filename}") or
            return "$!: Could not create a new file in maildir";
    } else {
        system("$sendmail_cmd < '$m->{filename}'") == 0 or
            return "$!: Could not send mail";
    }

    return 0;
}

#!/usr/bin/perl

use strict;
use warnings;

# A script to update the status of an issue in a Roundup bug tracker
# based on the format of a darcs patch name.
# It is intended to be run from a darcs posthook.

# The format we look for is:
#   resolved issue123 
# in the first line of the patch. 

use Getopt::Long;

use MIME::Lite;
use XML::Simple;

my $UPDATE_STRING="status=resolved";
if (scalar(@ARGV) == 1) {
  $UPDATE_STRING=$ARGV[0];
}

unless ($ENV{DARCS_PATCHES_XML}) {
    die "DARCS_PATCHES_XML was expected to be set in the environment, but was not found. 
          Are you running this from a Darcs 2.0 or newer posthook?"
}

my $xml = eval { XMLin($ENV{DARCS_PATCHES_XML}, forcearray=>['patch']); };
die "hmmm.. we couldn't parse your XML. The error was: $@"  if $@;

# $xml structure returned looks like this: 
#  'patch' => {
#    'resolved issue123: adding t.t' => {
#        'hash' => '20080215033723-20bb4-54f935f89817985a3e98f3de8e8ac9dad5e8e0e5.gz',
#        'inverted' => 'False',
#        'date' => '20080215033723',
#        'author' => 'Mark Stosberg <mark@summersault.com>',
#        'local_date' => 'Thu Feb 14 22:37:23 EST 2008'
#        },
#     'some other patch' => { ... }, 

for my $patch_name (keys %{ $xml->{patch} }) {
    my $issue_re = qr/resolved? \s+ (issue\d+)/msxi;

    next unless ($patch_name =~ $issue_re);
    my $issue = $1;
    my $patch = $xml->{patch}{$patch_name};

    # Using the Command Line would be a simpler alternative. 
    # my $out = `roundup-admin -i /var/lib/roundup/trackers/darcs set $issue status=resolved`;
    # warn "unexpected output: $out" if $out;

    my $author = $patch->{author};
    # If the Author name contains an @ sign, we take it to be an e-mail address.
    # Otherwise, we default to darcs-devel as the sender. 
    my $email = ($author =~ m/\@/) ? $author : 'darcs-devel@darcs.net';

    my $comment = $patch->{comment} ? "\n$patch->{comment}" : '';

    my $patch_name_minus_status = $patch_name; 
    $patch_name_minus_status =~ s/$issue_re(:?\s?)//;

     # Each patches can potentially update the status of a different issue, so generates a different e-mail
    my $msg = MIME::Lite->new(
         From     => 'noreply@darcs.net',
         To      =>'bugs@lists.osuosl.org',
         #To       =>'mark@stosberg.com',
         Subject  =>"[$issue] [$UPDATE_STRING]",
         Type     =>'text/plain',
         Data     => qq!The following patch sent by $email updated issue $issue with
$UPDATE_STRING

* $patch_name $comment

!
     );
     $msg->send;
     # An alternative to actually sending, for debugging. 
     # use File::Slurp;
     # write_file("msg-$patch->{hash}.out",$msg->as_string);
}



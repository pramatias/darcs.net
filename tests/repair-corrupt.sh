. lib

rm -rf bad
mkdir bad
cd bad
darcs init

echo foo > bar
darcs add bar
darcs rec -a -m 'foo'

echo hey > foo
darcs add foo
darcs rec -a -m 'more foo'

hashed=false
test -e _darcs/hashed_inventory && hashed=true
cp -R _darcs _clean_darcs

# produce a corrupt patch
echo 'rmfile foo' > _darcs/patches/pending
$hashed || echo -n > _darcs/pristine/foo
darcs rec -a -m 'remove foo'

not darcs check # unapplicable patch!
cp -R _darcs/ _backup_darcs
darcs repair # repairs the patch
darcs check
rm -rf _darcs
mv _backup_darcs _darcs # get the bad patch back

# stash away contents of _darcs
cp -R _darcs/ _backup_darcs

echo here > bar
darcs rec -a -m 'here'

# corrupt pristine content
corrupt_pristine() {
    $hashed && inv=`grep ^pristine _darcs/hashed_inventory`
    cp _backup_darcs/patches/* _darcs/patches/
    cp _backup_darcs/*inventory* _darcs/
    if $hashed; then
        cp _darcs/hashed_inventory hashed.tmp
        sed -e "s,^pristine:.*$,$inv," < hashed.tmp > _darcs/hashed_inventory
        rm hashed.tmp
    fi
}

corrupt_pristine
not darcs check # just a little paranoia

darcs repair # repair succeeds
darcs check # and the resulting repo is consistent

# *AND* it contains what we expect...
darcs show contents bar > foobar
echo foo > foobar1
diff foobar foobar1

rm -rf _backup_darcs
mv _clean_darcs _backup_darcs
corrupt_pristine # without the unapplicable patch
not darcs check
darcs repair
darcs check

cd ..

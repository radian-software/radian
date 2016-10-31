#!/bin/bash

set -e
set -o pipefail

source ensure-uuid-set.sh

# FIXME: needs to check to make sure iTerm2 is not open

plist=wip.plist #~/Library/Preferences/com.googlecode.iterm2.plist

if [[ -f $plist ]]; then
    echo "[configure-iterm2] Backing up $plist to originals/$uuid/com.googlecode.iterm2.plist."
    cp $plist originals/$uuid/com.googlecode.iterm2.plist
elif [[ -d $plist || -L $plist && ! -e $plist ]]; then
    echo "[configure-iterm2] It looks like $plist already exists, but is either a directory or an invalid symlink."
    echo "[configure-iterm2] Something is *very* wrong with your iTerm2. Aborting."
    exit 1
fi

./modify-plist.sh "$plist" set AllowClipboardAccess bool true
./modify-plist.sh "$plist" add "New Bookmarks" array
./modify-plist.sh "$plist" foreach "New Bookmarks" set "Option Key Sends" integer 2
./modify-plist.sh "$plist" foreach "New Bookmarks" set "Right Option Key Sends" integer 2

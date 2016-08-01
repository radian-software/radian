#!/bin/bash

set -e

echo '[setup] Checking to see if GNU Emacs 24.5.1 or newer is installed.'

if /Applications/Emacs.app/Contents/MacOS/Emacs --version \
        | egrep 'GNU Emacs (24\.(5\.[1-9]|[6-9])|2[5-9]|[3-9][0-9])'; then
    echo '[setup] It looks like an appropriate version of Emacs is already installed.'
else
    echo '[setup] It looks like an appropriate version of Emacs is not installed.'
    if [[ -d "/Applications/Emacs.app" ]]; then
        echo "[setup] Moving the existing version to originals/$UUID."
        mv /Applications/Emacs.app originals/$UUID/Emacs.app
    fi

    echo '[setup] Downloading Emacs from emacsformacosx.com.'
    wget "https://emacsformacosx.com/emacs-builds/Emacs-24.5-1-universal.dmg"
    echo '[setup] Mounting the Emacs disk image.'
    hdiutil mount Emacs-24.5-1-universal.dmg
    echo '[setup] Copying Emacs.app from the disk image to the Applications folder.'
    cp -R /Volumes/Emacs/Emacs.app /Applications/Emacs.app
    echo '[setup] Unmounting the Emacs disk image.'
    hdiutil unmount /Volumes/Emacs
    echo '[setup] Deleting the Emacs disk image.'
    rm Emacs-24.5-1-universal.dmg
fi

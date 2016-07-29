#!/bin/bash

set -e

echo '[setup] Checking to see if Emacs is installed.'

if [[ -d "/Applications/Emacs.app" ]]; then
    echo '[setup] It looks like Emacs is already installed.'
else
    echo '[setup] It looks like Emacs is not installed.'
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

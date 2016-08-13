#!/bin/bash

set -e
set -o pipefail

if ./ensure-installed.sh /Applications/Emacs.app/Contents/MacOS/Emacs --version "GNU Emacs" 24.5.1 assert; then
    echo "[install-emacs] It looks like an appropriate version of Emacs is already installed, but it is not symlinked correctly."
else
    echo "[install-emacs] It looks like an appropriate version of Emacs is not installed."
    if [[ -d "/Applications/Emacs.app" ]]; then
        echo "[install-emacs] Moving the existing version to originals/$uuid."
        mv /Applications/Emacs.app originals/$uuid/Emacs.app
    fi
    echo "[install-emacs] Downloading Emacs from emacsformacosx.com."
    # Specifying the filename makes wget override any existing file by that name
    wget -O Emacs-24.5-1-universal.dmg "https://emacsformacosx.com/emacs-builds/Emacs-24.5-1-universal.dmg"
    echo "[install-emacs] Mounting the Emacs disk image."
    hdiutil mount Emacs-24.5-1-universal.dmg
    echo "[install-emacs] Copying Emacs.app from the disk image to the Applications folder."
    cp -R /Volumes/Emacs/Emacs.app /Applications/Emacs.app
    echo "[install-emacs] Unmounting the Emacs disk image."
    hdiutil unmount /Volumes/Emacs
    echo "[install-emacs] Deleting the Emacs disk image."
    rm Emacs-24.5-1-universal.dmg
fi

if [[ $(brew list --versions emacs) ]]; then
    echo "[install-emacs] Removing the symlinks for the old version of Emacs using Homebrew."
    brew unlink emacs
fi

./ensure-symlinked.sh /usr/local/bin/emacs emacs

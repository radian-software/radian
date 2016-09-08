#!/bin/bash

set -e
set -o pipefail

source ensure-uuid-set.sh

if ./ensure-installed.sh "/Applications/Racket v6.6/bin/racket" --version "Welcome to Racket v" 6.6 assert; then
    echo "[install-racket] It looks like an appropriate version of Racket is already installed, but it is not symlinked correctly."
else
    echo "[install-racket] It looks like an appropriate version of Racket is not installed."
    if [[ -d "/Applications/Racket v6.6" ]]; then
        echo "[install-racket] Moving the existing version to originals/$uuid."
        mv "/Applications/Racket v6.6" "originals/$uuid/Racket v6.6"
    fi
    echo "[install-racket] Downloading Racket from download.racket-lang.org."
    # Specifying the filename makes wget override any existing file by that name
    wget -O racket-6.6-x86_64-macosx.dmg "https://mirror.racket-lang.org/installers/6.6/racket-6.6-x86_64-macosx.dmg"
    echo "[install-racket] Mounting the Racket disk image."
    hdiutil mount racket-6.6-x86_64-macosx.dmg
    echo "[install-racket] Copying Racket v6.6 from the disk image to the Applications folder."
    cp -R "/Volumes/Racket v6.6/Racket v6.6" "/Applications/Racket v6.6"
    echo "[install-racket] Unmounting the Racket disk image."
    hdiutil unmount "/Volumes/Racket v6.6"
    echo "[install-racket] Deleting the Racket disk image."
    rm racket-6.6-x86_64-macosx.dmg
fi

if [[ $(brew list --versions racket) ]]; then
    echo "[install-racket] Removing the symlinks for the old version of Racket using Homebrew."
    brew unlink racket
fi

# This needs to be here instead of the main code path in setup.sh,
# because we want to allow using a pre-existing installation of Racket
# (if it's the correct version).
./ensure-symlinked.sh /usr/local/bin/racket "/Applications/Racket v6.6/bin/racket"

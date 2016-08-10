#!/bin/bash

set -e
set -o pipefail

echo '[setup] Checking for a ~/.lein/profiles.clj.'
if [[ ~/.lein/profiles.clj -ef ../profiles.clj ]]; then
    echo '[setup] It appears that ~/.lein/profiles.clj is already correctly symlinked.'
else
    if [[ -e ~/.lein/profiles.clj || -L ~/.lein/profiles.clj ]]; then
        echo "[setup] Found one, moving it to originals/$uuid."
        mv ~/.lein/profiles.clj originals/$uuid/profiles.clj
    else
        echo "[setup] Looks like you don't have one."
    fi

    echo '[setup] Creating symlink for profiles.clj.'
    if ! [[ -d ~/.lein ]]; then
        if [[ -f ~/.lein || -L ~/.lein ]]; then
            echo '[setup] Your ~/.lein appears to be either a file or an invalid symlink.'
            echo "[setup] Moving it to originals/$uuid."
            mv ~/.lein originals/$uuid/.lein
        fi
        mkdir ~/.lein
    fi
    ln -s "$(cd .. && pwd)/profiles.clj" ~/.lein/profiles.clj
fi

#!/bin/bash

set -e

echo '[setup] Checking for a ~/.lein/profiles.clj.'
if [[ ~/.lein/profiles.clj -ef ../profiles.clj ]]; then
    echo '[setup] It appears that ~/.lein/profiles.clj is already correctly symlinked.'
else
    if [[ -e ~/.lein/profiles.clj || -L ~/.lein/profiles.clj ]]; then
        echo "[setup] Found one, moving it to originals/$UUID."
        mv ~/.lein/profiles.clj originals/$UUID/profiles.clj
    else
        echo "[setup] Looks like you don't have one."
    fi

    echo '[setup] Creating symlink for profiles.clj.'
    mkdir ~/.lein 2>/dev/null || true
    ln -s "$(cd .. && pwd)/profiles.clj" ~/.lein/profiles.clj
fi

#!/bin/bash

set -e

echo '[setup] Checking for a ~/.lein.'
if [[ -e ~/.lein ]]; then
    echo "[setup] Found one, moving it to original_dotfiles/$UUID."
    mv ~/.lein original_dotfiles/$UUID/.lein
else
    echo "[setup] Looks like you don't have one."
fi

echo '[setup] Creating symlink for .lein.'
ln -s "$(cd .. && pwd)/.lein" ~/.lein

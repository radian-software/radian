#!/bin/bash

set -e

echo '[setup] Checking for a ~/.lein.'
if [[ -e ~/.lein || -L ~/.lein ]]; then
    echo "[setup] Found one, moving it to originals/$UUID."
    mv ~/.lein originals/$UUID/.lein
else
    echo "[setup] Looks like you don't have one."
fi

echo '[setup] Creating symlink for .lein.'
ln -s "$(cd .. && pwd)/.lein" ~/.lein

#!/bin/bash

set -e

echo '[setup] Checking for a ~/.emacs.'
if [[ -e ~/.emacs || -L ~/.emacs ]]; then
    echo "[setup] Found one, moving it to originals/$UUID."
    mv ~/.emacs originals/$UUID/.emacs
else
    echo "[setup] Looks like you don't have one."
fi

echo '[setup] Checking for a ~/.emacs.el.'
if [[ -e ~/.emacs.el || -L ~/.emacs.el ]]; then
    echo "[setup] Found one, moving it to originals/$UUID."
    mv ~/.emacs.el originals/$UUID/.emacs.el
else
    echo "[setup] Looks like you don't have one."
fi

echo '[setup] Checking for a ~/.emacs.d/init.el.'
if [[ ~/.emacs.d/init.el -ef ../init.el ]]; then
    echo '[setup] It appears that ~/.emacs.d/init.el is already correctly symlinked.'
else
    if [[ -e ~/.emacs.d/init.el || -L ~/.emacs.d/init.el ]]; then
        echo "[setup] Found one, moving it to originals/$UUID."
        mv ~/.emacs.d/init.el originals/$UUID/init.el
    else
        echo "[setup] Looks like you don't have one."
    fi
    echo '[setup] Creating symlink for init.el.'
    mkdir ~/.emacs.d 2>/dev/null || true
    ln -s "$(cd .. && pwd)/init.el" ~/.emacs.d/init.el
fi

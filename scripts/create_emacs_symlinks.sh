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

echo '[setup] Checking for a ~/.emacs.d.'
if [[ -e ~/.emacs.d || -L ~/.emacs.d ]]; then
    echo "[setup] Found one, moving it to originals/$UUID."
    mv ~/.emacs.d originals/$UUID/.emacs.d
else
    echo "[setup] Looks like you don't have one."
fi

echo '[setup] Creating symlink for .emacs.d.'
ln -s "$(cd .. && pwd)/.emacs.d" ~/.emacs.d

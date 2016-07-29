#!/bin/bash

set -e

echo '[setup] Checking for a ~/.emacs.'
if [[ -e ~/.emacs ]]; then
    echo "[setup] Found one, moving it to original_dotfiles/$UUID."
    mv ~/.emacs original_dotfiles/$UUID/.emacs
else
    echo "[setup] Looks like you don't have one."
fi

echo '[setup] Checking for a ~/.emacs.el.'
if [[ -e ~/.emacs.el ]]; then
    echo "[setup] Found one, moving it to original_dotfiles/$UUID."
    mv ~/.emacs.el original_dotfiles/$UUID/.emacs.el
else
    echo "[setup] Looks like you don't have one."
fi

echo '[setup] Checking for a ~/.emacs.d.'
if [[ -e ~/.emacs.d ]]; then
    echo "[setup] Found one, moving it to original_dotfiles/$UUID."
    mv ~/.emacs.d original_dotfiles/$UUID/.emacs.d
else
    echo "[setup] Looks like you don't have one."
fi

echo '[setup] Creating symlink for .emacs.d.'
ln -s "$(cd .. && pwd)/.emacs.d" ~/.emacs.d

#!/bin/bash

echo '[setup] Checking for a ~/.tmux.conf.'
if [[ ~/.tmux.conf -ef ../.tmux.conf ]]; then
    echo '[setup] It appears that ~/.tmux.conf is already correctly symlinked.'
else
    if [[ -e ~/.tmux.conf || -L ~/.tmux.conf ]]; then
        echo "[setup] Found one, moving it to originals/$uuid."
        mv ~/.tmux.conf originals/$uuid/.tmux.conf
    else
        echo "[setup] Looks like you don't have one."
    fi
    echo '[setup] Creating symlink for .tmux.conf.'
    ln -s "$(cd .. && pwd)/.tmux.conf" ~/.tmux.conf
fi

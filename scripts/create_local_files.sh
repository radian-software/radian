#!/bin/bash

if [[ -e .zshrc.aliases || -L .zshrc.aliases ]]; then
    echo "[setup] Found an old .zshrc.aliases, moving to originals/$uuid as .zshrc.aliases.tmp."
    mv .zshrc.aliases originals/$uuid/.zshrc.aliases.tmp
fi
touch .zshrc.aliases

if [[ -e .tmux.powerline.conf || -L .tmux.powerline.conf ]]; then
    echo "[setup] Found an old .tmux.powerline.conf, moving to originals/$uuid as .tmux.powerline.conf.tmp."
    mv .tmux.powerline.conf originals/$uuid/.tmux.powerline.conf
fi
touch .tmux.powerline.conf

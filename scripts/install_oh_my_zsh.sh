#!/bin/bash

set -e

echo '[setup] Checking if Oh My Zsh is installed.'

if [[ ! -n "$ZSH" ]]; then
    ZSH=~/.oh-my-zsh
fi

if [[ -d "$ZSH" ]]; then
    echo '[setup] It looks like Oh My Zsh is already installed.'
else
    echo '[setup] It looks like Oh My Zsh is not yet installed.'
    echo '[setup] Checking for a ~/.zshrc.pre-oh-my-zsh.'
    if [[ -e ~/.zshrc.pre-oh-my-zsh || -L ~/.zshrc.pre-oh-my-zsh ]]; then
        echo "[setup] Found one, moving it to originals/$UUID as .zshrc.pre-oh-my-zsh.preexisting."
        mv ~/.zshrc.pre-oh-my-zsh originals/$UUID/.zshrc.pre-oh-my-zsh.preexisting
    else
        echo "[setup] Looks like you don't have one."
    fi
    echo '[setup] Checking for a ~/.zshrc.'
    if [[ -e ~/.zshrc || -L ~/.zshrc ]]; then
        echo "[setup] Found one, moving it to originals/$UUID as .zshrc.pre-oh-my-zsh."
        mv ~/.zshrc originals/$UUID/.zshrc.pre-oh-my-zsh
    else
        echo "[setup] Looks like you don't have one."
    fi
    echo "[setup] Exporting \$SHELL=$(which zsh). This prevents Oh My Zsh from trying to change the login shell again."
    export SHELL="$(which zsh)"
    echo '[setup] Running the Oh My Zsh setup script.'
    sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
fi

#!/bin/bash

echo '[setup] Checking if Oh My Zsh is installed.'

if [[ -d ~/.oh-my-zsh ]]; then
    echo '[setup] It looks like Oh My Zsh is already installed.'
else
    echo '[setup] It looks like Oh My Zsh is not yet installed.'
    echo '[setup] Checking for a ~/.zshrc.pre-oh-my-zsh.'
    if [[ -e ~/.zshrc.pre-oh-my-zsh || -L ~/.zshrc.pre-oh-my-zsh ]]; then
        echo "[setup] Found one, moving it to originals/$uuid as .zshrc.pre-oh-my-zsh.preexisting."
        mv ~/.zshrc.pre-oh-my-zsh originals/$uuid/.zshrc.pre-oh-my-zsh.preexisting
    else
        echo "[setup] Looks like you don't have one."
    fi
    echo '[setup] Checking for a ~/.zshrc.'
    if [[ -e ~/.zshrc || -L ~/.zshrc ]]; then
        echo "[setup] Found one, moving it to originals/$uuid as .zshrc.pre-oh-my-zsh."
        mv ~/.zshrc originals/$uuid/.zshrc.pre-oh-my-zsh
    else
        echo "[setup] Looks like you don't have one."
    fi
    echo "[setup] Exporting \$SHELL=$(which zsh). This prevents Oh My Zsh from trying to change the login shell again."
    export SHELL="$(which zsh)"
    echo '[setup] Exporting $ZSH=~/.oh-my-zsh. This setup requires you to use ~/.oh-my-zsh as your Oh My Zsh directory.'
    export ZSH=~/.oh-my-zsh
    echo '[setup] Running the Oh My Zsh setup script.'
    echo '[setup] Please press Control+D after installation is complete.'
    read -p '[setup] Press RET to continue.'
    sh -c "$(wget https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
fi

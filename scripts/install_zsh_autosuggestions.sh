#!/bin/bash

echo '[setup] Checking to see if zsh-autosuggestions is installed.'
if [[ -d ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions ]]; then
    echo '[setup] It looks like zsh-autosuggestions is already installed.'
else
    echo '[setup] Installing zsh-autosuggestions to ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions.'
    mkdir -p ~/.oh-my-zsh/custom/plugins 2>/dev/null || true
    git clone https://github.com/zsh-users/zsh-autosuggestions ~/.oh-my-zsh/custom/plugins/zsh-autosuggestions
fi

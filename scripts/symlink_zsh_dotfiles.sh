#!/bin/bash

echo '[setup] Checking for a ~/.zshrc.'
if [[ ~/.zshrc -ef ../.zshrc ]]; then
    echo '[setup] It appears that ~/.zshrc is already correctly symlinked.'
else
    if [[ -e ~/.zshrc || -L ~/.zshrc ]]; then
        echo "[setup] Found one, moving it to originals/$uuid."
        mv ~/.zshrc originals/$uuid/.zshrc
    else
        echo "[setup] Looks like you don't have one."
    fi

    echo '[setup] Creating symlink for .zshrc.'
    ln -s "$(cd .. && pwd)/.zshrc" ~/.zshrc
fi

echo '[setup] Checking for a ~/.zshrc.aliases.'
if [[ ~/.zshrc.aliases -ef .zshrc.aliases ]]; then
    echo '[setup] It appears that ~/.zshrc.aliases is already correctly symlinked.'
else
    if [[ -e ~/.zshrc.aliases || -L ~/.zshrc.aliases ]]; then
        echo "[setup] Found one, moving it to originals/$uuid."
        mv ~/.zshrc.aliases originals/$uuid/.zshrc.aliases
    else
        echo "[setup] Looks like you don't have one."
    fi

    echo '[setup] Creating symlink for .zshrc.aliases.'
    ln -s "$(pwd)/.zshrc.aliases" ~/.zshrc.aliases
fi

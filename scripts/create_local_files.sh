#!/bin/bash

if [[ -e .zshrc.aliases || -L .zshrc.aliases ]]; then
    echo "[setup] Found an old .zshrc.aliases, moving to originals/$uuid as .zshrc.aliases.tmp."
    mv .zshrc.aliases originals/$uuid/.zshrc.aliases.tmp
fi
touch .zshrc.aliases

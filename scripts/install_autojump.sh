#!/bin/bash

echo "[setup] Checking to see if the 'autojump' command is available."
if hash autojump 2>/dev/null; then
    echo "[setup] It appears that 'autojump' is already installed."
else
    echo "[setup] It appears that 'autojump' is not yet installed."
    echo "[setup] Installing 'autojump' using Homebrew."
    brew install autojump
fi

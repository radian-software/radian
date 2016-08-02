#!/bin/bash

set -e

echo "[setup] Checking to see if the 'tree' command is available."
if hash tree 2>/dev/null; then
    echo "[setup] It appears that 'tree' is already installed."
else
    echo "[setup] It appears that 'tree' is not yet installed."
    echo "[setup] Installing 'tree' using Homebrew."
    brew install tree
fi

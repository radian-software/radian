#!/bin/bash

set -e

echo "[setup] Checking to see if the 'wget' command is available."
if hash wget 2>/dev/null; then
    echo "[setup] It appears that 'wget' is already installed."
else
    echo "[setup] It appears that 'wget' is not yet installed."
    echo "[setup] Installing 'wget' using Homebrew."
    brew install wget
fi

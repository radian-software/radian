#!/bin/bash

echo '[setup] Checking to see if tmux is installed.'
if hash tmux; then
    echo '[setup] It appears that tmux is already installed.'
else
    if [[ $(brew list --versions tmux) ]]; then
        echo '[setup] It appears that tmux has already been installed using Homebrew, but it is not symlinked correctly.'
        echo '[setup] Removing old symlinks using Homebrew.'
        brew unlink tmux
        echo '[setup] Recreating symlinks using Homebrew.'
        brew link tmux
    else
        echo '[setup] It appears that tmux is not yet installed.'
        echo '[setup] Installing tmux using Homebrew.'
        brew install tmux
    fi
    if ! hash tmux; then
        echo '[setup] Fatal error: tmux should be installed by this point.'
        exit 1
    fi
fi

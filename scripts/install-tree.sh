#!/bin/bash

echo '[setup] Checking to see if tree is installed.'
if hash tree; then
    echo '[setup] It appears that tree is already installed.'
else
    if [[ $(brew list --versions tree) ]]; then
        echo '[setup] It appears that tree has already been installed using Homebrew, but it is not symlinked correctly.'
        echo '[setup] Removing old symlinks using Homebrew.'
        brew unlink tree
        echo '[setup] Recreating symlinks using Homebrew.'
        brew link tree
    else
        echo '[setup] It appears that tree is not yet installed.'
        echo '[setup] Installing tree using Homebrew.'
        brew install tree
    fi
    if ! hash tree; then
        echo '[setup] Fatal error: tree should be installed by this point.'
        exit 1
    fi
fi

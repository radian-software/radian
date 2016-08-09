#!/bin/bash

echo '[setup] Checking to see if autojump is installed.'
if hash autojump; then
    echo '[setup] It appears that autojump is already installed.'
else
    if [[ $(brew list --versions autojump) ]]; then
        echo '[setup] It appears that autojump has already been installed using Homebrew, but it is not symlinked correctly.'
        echo '[setup] Removing old symlinks using Homebrew.'
        brew unlink autojump
        echo '[setup] Recreating symlinks using Homebrew.'
        brew link autojump
    else
        echo '[setup] It appears that autojump is not yet installed.'
        echo '[setup] Installing autojump using Homebrew.'
        brew install autojump
    fi
    if ! hash autojump; then
        echo '[setup] Fatal error: autojump should be installed by this point.'
        exit 1
    fi
fi

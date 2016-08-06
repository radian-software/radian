#!/bin/bash

echo '[setup] Checking to see if wget is installed.'
if hash wget; then
    echo '[setup] It appears that wget is already installed.'
else
    if [[ $(brew list --versions wget) ]]; then
        echo '[setup] It appears that wget has already been installed using Homebrew, but it is not symlinked correctly.'
        echo '[setup] Removing old symlinks using Homebrew.'
        brew unlink wget
        echo '[setup] Recreating symlinks using Homebrew.'
        brew link wget
    else
        echo '[setup] It appears that wget is not yet installed.'
        echo '[setup] Installing wget using Homebrew.'
        brew install wget
    fi
    if ! hash wget; then
        echo '[setup] Fatal error: wget should be installed by this point.'
        exit 1
    fi
fi

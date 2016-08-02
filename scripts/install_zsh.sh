#!/bin/bash

set -e

echo '[setup] Checking to see if Zsh 5.2 or newer is installed.'

if zsh --version \
        | egrep 'zsh (5\.([2-9]|[1-9][0-9]+)|[6-9]|[1-9][0-9]+)'; then
    echo '[setup] It looks like an appropriate version of Zsh is already installed.'
else
    echo '[setup] It looks like an appropriate version of Zsh is not installed.'
    if [[ $(brew list --versions zsh) ]]; then
        echo '[setup] Uninstalling the old version of Zsh using Homebrew.'
        echo -n '[setup] Is this OK? (y/n) '
        read ANSWER
        if ! (echo "$ANSWER" | grep -qi "^y"); then
            echo '[setup] Aborting.'
            exit 1
        fi
        brew uninstall zsh
    fi
    echo '[setup] Installing Zsh using Homebrew.'
    brew install zsh
fi

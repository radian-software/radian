#!/bin/bash

echo '[setup] Checking to see if Zsh 5.2 or newer is installed.'

zsh_version_regex='zsh (5\.([2-9]|[1-9][0-9]+)|[6-9]|[1-9][0-9]+)'
zsh_version_regex_brew='zsh.* (5\.([2-9]|[1-9][0-9]+)|[6-9]|[1-9][0-9]+)'

if zsh --version | egrep "$zsh_version_regex"; then
    echo '[setup] It looks like an appropriate version of Zsh is already installed.'
else
    if brew list --versions zsh | egrep "$zsh_version_regex_brew"; then
        echo '[setup] It looks like an appropriate version of Zsh has already been installed using Homebrew, but it is not symlinked correctly.'
        echo '[setup] Removing old symlinks using Homebrew.'
        brew unlink zsh
        echo '[setup] Recreating symlinks using Homebrew.'
        brew link zsh
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
    if ! (zsh --version | egrep "$zsh_version_regex"); then
        echo '[setup] Fatal error: an appropriate version of Zsh should be installed by this point.'
        exit 1
    fi
fi

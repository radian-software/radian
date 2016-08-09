#!/bin/bash

echo '[setup] Checking to see if tmux 2.2 or newer is installed.'

tmux_version_regex='tmux (2\.([2-9]|[1-9][0-9]+)|[3-9]|[1-9][0-9]+)'
tmux_version_regex_brew='tmux.* (5\.([2-9]|[1-9][0-9]+)|[6-9]|[1-9][0-9]+)'

if tmux -V | egrep "$tmux_version_regex"; then
    echo '[setup] It looks like an appropriate version of tmux is already installed.'
else
    if brew list --versions tmux | egrep "$tmux_version_regex_brew"; then
        echo '[setup] It looks like an appropriate version of tmux has already been installed using Homebrew, but it is not symlinked correctly.'
        echo '[setup] Removing old symlinks using Homebrew.'
        brew unlink tmux
        echo '[setup] Recreating symlinks using Homebrew.'
        brew link tmux
    else
        echo '[setup] It looks like an appropriate version of tmux is not installed.'
        if [[ $(brew list --versions tmux) ]]; then
            echo '[setup] Uninstalling the old version of tmux using Homebrew.'
            echo -n '[setup] Is this OK? (y/n) '
            read answer
            if ! (echo "$answer" | grep -qi "^y"); then
                echo '[setup] Aborting.'
                exit 1
            fi
            brew uninstall tmux
        fi
        echo '[setup] Installing tmux using Homebrew.'
        brew install tmux
    fi
    if ! (tmux -V | egrep "$tmux_version_regex"); then
        echo '[setup] Fatal error: an appropriate version of tmux should be installed by this point.'
        exit 1
    fi
fi

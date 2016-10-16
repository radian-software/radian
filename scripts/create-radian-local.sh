#!/bin/bash

set -e
set -o pipefail

source ensure-uuid-set.sh

if [[ $repo_name == radian-local ]]; then
    echo "[create-radian-local] Fatal error: this repository cannot be called 'radian-local'. Please rename it."
    exit 1
fi
if [[ -f ../../radian-local || -L ../../radian-local && ! -e ../../radian-local ]]; then
    echo "[create-radian-local] You appear to have something called 'radian-local' next to '$repo_name' that is either a file or an invalid symlink."
    echo "[create-radian-local] Moving it to originals/$uuid."
    mv ../../radian-local originals/$uuid/radian-local
fi

# Backwards compatibility -- rename existing dotfiles-local to radian-local
#
# Removing this code will affect fewer than ten people, so there is no harm
# in doing so eventually.
if [[ ! -d ../../radian-local && -d ../../dotfiles-local && ! -e originals/.keep-dotfiles-local ]]; then
    echo "[create-radian-local] You have a folder called 'dotfiles-local' next to '$repo_name'."
    echo "[create-radian-local] This was likely created by an earlier version of the Radian create-radian-local script."
    echo "[create-radian-local] If so, it needs to be renamed from 'dotfiles-local' to 'radian-local' to work with the latest version of Radian."
    echo -n "[create-radian-local] Rename the folder? (y/n) "
    read answer
    if echo "$answer" | grep -qi "^y"; then
        mv ../../dotfiles-local ../../radian-local
        echo "[create-radian-local] Please note that the prefix for local configuration parameters has been changed from 'radon' to 'radian'."
        echo "[create-radian-local] Therefore, you will either need to replace all occurrences of 'radon' with 'radian' in radian-local."
        echo "[create-radian-local] Alternatively, you can delete 'radian-local' and re-run this script to set them up interactively again."
        read -p "[create-radian-local] Press RET to continue."
    else
        echo "[create-radian-local] OK, I will not rename the folder."
        echo -n "[create-radian-local] Would you like Radian to remember your choice and not ask again next time? (y/n) "
        read answer
        if echo "$answer" | grep -qi "^y"; then
            echo "[create-radian-local] Creating originals/.keep-dotfiles-local to make your choice persistent."
            touch originals/.keep-dotfiles-local
        else
            echo "[create-radian-local] Please note that if you want Radian to ask again, you will have to delete the 'radian-local' folder before re-running the create-radian-local script."
            read -p "[create-radian-local] Press RET to continue."
        fi
    fi
fi

if [[ ! -d ../../radian-local ]]; then
    echo "[create-radian-local] You do not have a radian-local folder next to '$repo_name'."
    echo -n "[create-radian-local] Do you already have a radian-local folder somewhere else on your filesystem? (y/n) "
    read answer
    if echo "$answer" | grep -qi "^y"; then
        finished=false
        while [[ $finished != true ]]; do
            path=
            while [[ -z $path ]]; do
                echo -n "[create-radian-local] Enter the path to your radian-local folder: "
                read path
            done
            path=${path/#\~/$HOME} # expands tildes
            if [[ -e $path ]]; then
                echo "[create-radian-local] Creating symlink."
                ln -s "$path" ../../radian-local
                finished=true
            else
                echo "[create-radian-local] That path does not exist."
            fi
        done
    else
        echo "[create-radian-local] Creating empty radian-local folder."
        mkdir ../../radian-local
    fi
fi

touch ../../radian-local/.projectile

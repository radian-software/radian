#!/bin/bash

# Description:
#
# Ensure that a directory is not version-controlled. If the directory
# does not exist, does nothing. If the directory is not
# version-controlled, does nothing. If the directory is
# version-controlled (as determined by checking for a .git or .svn
# directory), offers (but does not require) to move the entire
# directory to the originals folder.

# Arguments:
#
# $1 = directory to check

# Exit code:
#
# Zero unless an unexpected error occurs.

### Setup ###

set -e
set -o pipefail

source ensure-uuid-set.sh

### Parse arguments ###

directory=$1

if [[ -z $directory ]]; then
    echo "[ensure-not-version-controlled] Fatal error: directory not provided."
    exit 1
fi

### Compute useful strings ###

dirname=$(basename "$(grealpath "$directory")")
marker_file=originals/.keep-version-controlled-$dirname

### Main logic ###

if [[ ( -e $directory/.git || -e $directory/.svn ) && ! -e $marker_file ]]; then
    echo "[ensure-not-version-controlled] Your $directory directory is under version control."
    echo "[ensure-not-version-controlled] This is typically from a previous dotfiles repository."
    echo "[ensure-not-version-controlled] Radian needs to place files in $directory, so you may want to move the existing version first and make a new directory."
    echo -n "[ensure-not-version-controlled] Move $directory? (y/n) "
    read answer
    if echo "$answer" | grep -qi "^y"; then
        echo "[ensure-not-version-controlled] Moving it to originals/$uuid/$dirname."
        mv "$directory" "originals/$uuid/$dirname"
    else
        echo -n "[ensure-not-version-controlled] Would you like Radian to remember this choice and not ask again next time? (y/n) "
        read answer
        if echo "$answer" | grep -qi "^y"; then
            echo "[ensure-not-version-controlled] Creating $marker_file to make your choice persistent."
            touch "$marker_file"
        else
            echo "[ensure-not-version-controlled] OK."
        fi
    fi
fi

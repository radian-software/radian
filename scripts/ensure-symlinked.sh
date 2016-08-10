#!/bin/bash

# Description:
# Ensure that a file is symlinked to another file, or ensure that a file
# does not exist. Back up any existing version of the file, and, if
# appropriate, create the symlink.

# Arguments:
# $1 = location of symlink
# $2 = location of file symlink should point to (optional)

# Postconditions:
# - The symlink will be created, or, if $2 is not provided, the file will
#   not exist.

# Exit code:
# - zero if no errors occurred;
# - non-zero if an error occurred.

### Setup ###

set -e
set -o pipefail

if [[ -z $uuid ]]; then
    echo "[ensure-symlinked] Fatal error: \$uuid not set."
    exit 1
fi

### Parse arguments ###

link="$1"
if [[ -z $link ]]; then
    echo "[ensure-symlinked] Fatal error: symlink location not provided."
    exit 1
fi
link="$(cd "${link%/*}" && pwd)/${link##*/}"

real="$2"
if [[ $real ]]; then
    if [[ ! -e $real ]]; then
        echo "[ensure-symlinked] Fatal error: \$real does not exist."
    fi
    real="$(cd "${real%/*}" && pwd)/${real##*/}"
fi

### Main logic ###

echo "[ensure-symlinked] Checking $link."
if [[ $link -ef $real ]]; then
    echo "[ensure-symlinked] $link is already symlinked correctly to $real."
else
    if [[ -e $link || -L $link ]]; then
        if [[ $real ]]; then
            echo "[ensure-symlinked] $link already exists, and is not symlinked correctly."
        else
            echo "[ensure-symlinked] $link already exists, and needs to be removed."
        fi
        echo "[ensure-symlinked] Moving the existing version to originals/$uuid/${link##*/}."
        mv "$link" "originals/$uuid/${link##*/}"
    else
        echo "[ensure-symlinked] $link does not exist, and needs to be created."
    fi
    if [[ $real ]]; then
        echo "[ensure-symlinked] Ensuring that intermediate directories exist."
        mkdir -p "${link%/*}"
        echo "[ensure-symlinked] Creating symlink from $real to $link."
        ln -s "$real" "$link"
    fi
fi

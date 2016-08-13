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

source ensure-uuid-set.sh

# Having $uuid is necessary for backing up existing files.
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

real="$2"
if [[ $real ]]; then
    if [[ ! -e $real ]]; then
        echo "[ensure-symlinked] Fatal error: $real does not exist."
    fi

    # Account for relative paths to files in the current directory.
    if [[ $real != /* ]]; then
        real="./$real"
    fi

    # Make $real an absolute path.
    real="$(cd "${real%/*}" && pwd)/${real##*/}"
fi

### Report task ###

if [[ $real ]]; then
    echo "[ensure-symlinked] Ensuring that $link is symlinked to $real."
else
    echo "[ensure-symlinked] Ensuring that $link does not exist."
fi

### Subroutines ###

move_existing() {
    echo "[ensure-symlinked] Moving the existing version to originals/$uuid/${link##*/}."
    mv "$link" "originals/$uuid/${link##*/}"
}

### Main logic ###

if [[ $real ]]; then
    echo "[ensure-symlinked] Ensuring that intermediate directories exist."
    mkdir -p "${link%/*}"

    # Account for relative paths to files in the current directory.
    if [[ $link != /* ]]; then
        link="./$link"
    fi

    # Make $link an absolute path. We can only do this after ensuring that
    # intermediate directories exist.
    link="$(cd "${link%/*}" && pwd)/${link##*/}"

    echo "[ensure-symlinked] Checking $link."
    if [[ $link -ef $real ]]; then
        echo "[ensure-symlinked] $link is already symlinked correctly to $real."
    else
        if [[ -e $link || -L $link ]]; then
            echo "[ensure-symlinked] $link already exists, and is not symlinked correctly."
            move_existing
        else
            echo "[ensure-symlinked] $link does not exist, and needs to be created."
        fi
        echo "[ensure-symlinked] Creating symlink from $link to $real."
        ln -s "$real" "$link"
    fi
else
    if [[ -e $link || -L $link ]]; then
        echo "[ensure-symlinked] $link already exists, and needs to be removed."
        move_existing
    else
        echo "[ensure-symlinked] $link does not exist."
    fi
fi

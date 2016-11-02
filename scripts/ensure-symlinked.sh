#!/bin/bash

# Description:
# Ensure that a file is symlinked to another file, or ensure that a file
# does not exist. Back up any existing version of the file, and, if
# appropriate, create the symlink. Optionally, run another command to
# create the file that the symlink should point to.

# Arguments:
# $1 = location of symlink
# $2 = location of file symlink should point to (optional)
# $3 = command to create file symlink should point to (optional)

# Postconditions:
# - The symlink will be created, or, if $2 is not provided, the file will
#   not exist. If $3 is provided, the file the symlink points to will be
#   created.

# Exit code:
# - zero if no errors occurred;
# - non-zero if an error occurred.

### Setup ###

set -e
set -o pipefail

source ensure-uuid-set.sh

### Parse arguments ###

link="$1"
if [[ -z $link ]]; then
    echo "[ensure-symlinked] Fatal error: symlink location not provided."
    exit 1
fi

real="$2"

setup="$3"

### Create symlink target, if necessary ###

if [[ $setup ]]; then
    echo "[ensure-symlinked] Ensuring that $real exists."
    if [[ -d $real || -L $real && ! -e $real ]]; then
        echo "[ensure-symlinked] Your $real appears to be either a directory or an invalid symlink."
        echo "[ensure-symlinked] Moving it to originals/$uuid as $real.real."
        mv "$real" "originals/$uuid/$real.real"
    fi
    if [[ -e $real ]]; then
        echo "[ensure-symlinked] $real already exists. If you would like to set it up again, please remove it and re-run setup.sh."
    else
        echo "[ensure-symlinked] $real does not exist. Setting it up with '$setup'."
        $setup
        if [[ ! -e $real ]]; then
            echo "[ensure-symlinked] Fatal error: $real should exist by now."
            exit 1
        fi
    fi
fi

### Fully resolve path to $real ###

# This is necessary because the first argument to 'ln' must be a fully
# resolved path. (Or, if it's a relative path, it has to be relative
# to $link, which would be even harder to compute than just getting an
# absolute path.)

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
        ln -s "$(grealpath "$real")" "$(grealpath "$link")"
    fi
else
    if [[ -e $link || -L $link ]]; then
        echo "[ensure-symlinked] $link already exists, and needs to be removed."
        move_existing
    else
        echo "[ensure-symlinked] $link does not exist."
    fi
fi

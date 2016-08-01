#!/bin/bash

set -e

### Setup ###

echo '[setup] Setting up raxod502/dotfiles.'

trap 'echo "[setup] It looks like an error occurred. Please try to fix it, and then run this script again."' EXIT

cd "$(dirname "$0")"

export UUID=$(uuidgen)
mkdir originals 2>/dev/null || true
mkdir originals/$UUID
echo "[setup] The UUID for this session is $UUID."

### Installing software ###

echo '[setup] Installing software.'

./install_xcode_cl_tools.sh

# This code is in a rather awkward place. Since it can cause the
# setup to restart, it should be placed as early as possible, but
# we need to install the Xcode Command Line Tools in order to have
# access to git. Also, we need to turn off 'trap', so it's hard
# to have it in a subsidiary script.
echo '[setup] Making sure we are running inside a git repository.'
if git rev-parse --is-inside-work-tree; then
    echo '[setup] Looks good!'
else
    echo "[setup] We don't seem to be inside a git repository."
    echo '[setup] Cloning raxod502/dotfiles.'
    git clone https://github.com/raxod502/dotfiles.git
    echo '[setup] Starting again using the cloned script.'
    trap EXIT
    dotfiles/scripts/setup.sh
    exit 0
fi

./install_homebrew.sh
./install_homebrew_packages.sh
./install_emacs.sh

### Configuring software ###

echo '[setup] Configuring software.'

./change_login_shell.sh

### Creating dotfile symlinks ###

echo '[setup] Creating dotfile symlinks.'
./create_emacs_symlinks.sh
./create_lein_symlinks.sh

### Testing software ###

echo '[setup] Testing software.'
./test_emacs.sh

### Cleanup ###

rmdir originals/$UUID 2>/dev/null && echo "[setup] No backups were made, deleting originals/$UUID." || true
rmdir originals 2>/dev/null && echo "[setup] No backup folders remaining, deleting originals." || true

echo "[setup] We're all done. Enjoy!"
echo '[setup] If the dotfiles repository is not in the correct place, simply move it and run this script again. Your symlinks will be updated automatically.'

trap EXIT

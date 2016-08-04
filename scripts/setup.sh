#!/bin/bash

set -e
shopt -s expand_aliases

### Setup ###

echo '[setup] Setting up raxod502/dotfiles.'

trap 'echo "[setup] It looks like an error occurred. Please try to fix it, and then run this script again."' EXIT

original_directory="$(pwd)"
cd "$(dirname "$0")"

export uuid=$(uuidgen)
mkdir originals 2>/dev/null || true
mkdir originals/$uuid
echo "[setup] The UUID for this session is $uuid."

if [[ -e .zshrc.aliases || -L .zshrc.aliases ]]; then
    echo "[setup] Found an old .zshrc.aliases, moving to originals/$uuid as .zshrc.aliases.tmp"
    mv .zshrc.aliases originals/$uuid/.zshrc.aliases.tmp
fi
touch .zshrc.aliases

### Bootstrapping ###

source install_xcode_cl_tools.sh

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

source install_homebrew.sh
source install_wget.sh

### Zsh ###

source install_zsh.sh
source change_login_shell.sh
source install_oh_my_zsh.sh
source symlink_zsh_dotfiles.sh

### Leiningen ###

source install_jdk.sh
source install_leiningen.sh
source symlink_leiningen_dotfiles.sh
source retrieve_leiningen_dependencies.sh

### Emacs ###

source install_emacs.sh
source symlink_emacs_dotfiles.sh
source install_emacs_packages.sh

### Utilities ###

source install_tree.sh

### Cleanup ###

rmdir originals/$uuid 2>/dev/null && echo "[setup] No backups were made, deleting originals/$uuid." || true
rmdir originals 2>/dev/null && echo "[setup] No backup folders remaining, deleting originals." || true

trap EXIT

echo
echo "[setup] We're all done. Enjoy!"
echo '[setup] If the dotfiles repository is not in the correct place, simply move it and run this script again. Your symlinks will be updated automatically.'
echo '[setup] Starting a new shell session.'
cd "$original_directory"
zsh

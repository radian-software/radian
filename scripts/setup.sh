#!/bin/bash

set -e
shopt -s expand_aliases

### Setup ###

echo '[setup] Setting up raxod502/dotfiles.'

trap 'echo "[setup] It looks like an error occurred. Please try to fix it, and then run this script again."' EXIT

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
source install_homebrew.sh
source install_wget.sh

### Zsh ###

source install_zsh.sh
source change_login_shell.sh
source install_antigen.sh
source symlink_zsh_dotfiles.sh

### Leiningen ###

source install_jdk.sh
source install_leiningen.sh
source symlink_leiningen_dotfiles.sh

### Emacs ###

source install_emacs.sh
source symlink_emacs_dotfiles.sh

### Utilities ###

source install_tree.sh

### Cleanup ###

rmdir originals/$uuid 2>/dev/null && echo "[setup] No backups were made, deleting originals/$uuid." || true
rmdir originals 2>/dev/null && echo "[setup] No backup folders remaining, deleting originals." || true

trap EXIT

echo
echo "[setup] We're all done. Enjoy!"
echo '[setup] If the dotfiles repository is not in the correct place, simply move it and run this script again. Your symlinks will be updated automatically.'
echo "[setup] Starting a new shell session in $(pwd)."
zsh

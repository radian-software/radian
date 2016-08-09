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

### Bootstrapping ###

source install-xcode-cl-tools.sh
source install-homebrew.sh
source install-wget.sh

### Git ###

source symlink-git-dotfiles.sh

### Zsh ###

source install-zsh.sh
source change-login-shell.sh
source install-antigen.sh
source install-autojump.sh
source symlink-zsh-dotfiles.sh

### Tmux ###

source install-tmux.sh
source symlink-tmux-dotfiles.sh

### Leiningen ###

source install-jdk.sh
source install-leiningen.sh
source symlink-leiningen-dotfiles.sh

### Emacs ###

source install-emacs.sh
source symlink-emacs-dotfiles.sh

### Utilities ###

source install-tree.sh

### Cleanup ###

rmdir originals/$uuid 2>/dev/null && echo "[setup] No backups were made, deleting originals/$uuid." || true
rmdir originals 2>/dev/null && echo "[setup] No backup folders remaining, deleting originals." || true

trap EXIT

echo
echo "[setup] We're all done. Enjoy!"
echo '[setup] If the dotfiles repository is not in the correct place, simply move it and run this script again. Your symlinks will be updated automatically.'
echo "[setup] Starting a new shell session in $(pwd)."
exec zsh -l

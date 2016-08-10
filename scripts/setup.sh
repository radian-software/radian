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
source ensure-installed.sh wget

### Git ###

source symlink-git-dotfiles.sh

### Zsh ###

source ensure-installed.sh zsh --version zsh 5.2
source change-login-shell.sh
source install-antigen.sh
source ensure-installed.sh autojump
source symlink-zsh-dotfiles.sh

### Tmux ###

source ensure-installed.sh tmux -V tmux 2.2
source symlink-tmux-dotfiles.sh

### Leiningen ###

source install-jdk.sh
source ensure-installed.sh lein --version Leiningen 2.6.1 brew leiningen
source symlink-leiningen-dotfiles.sh

### Emacs ###

source install-emacs.sh
source symlink-emacs-dotfiles.sh

### Utilities ###

source ensure-installed.sh tree

### Cleanup ###

rmdir originals/$uuid 2>/dev/null && echo "[setup] No backups were made, deleting originals/$uuid." || true
rmdir originals 2>/dev/null && echo "[setup] No backup folders remaining, deleting originals." || true

trap EXIT

echo
echo "[setup] We're all done. Enjoy!"
echo '[setup] If the dotfiles repository is not in the correct place, simply move it and run this script again. Your symlinks will be updated automatically.'
echo "[setup] Starting a new shell session in $(pwd)."
exec zsh -l

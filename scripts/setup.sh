#!/bin/bash

set -e
set -o pipefail

### Setup ###

echo '[setup] Setting up raxod502/dotfiles.'

trap 'echo && echo "[setup] It looks like an error occurred. Please try to fix it, and then run this script again."' EXIT

cd "$(dirname "$0")"

export uuid=$(uuidgen)
mkdir originals 2>/dev/null || true
mkdir originals/$uuid
echo "[setup] The UUID for this session is $uuid."

### Bootstrapping ###

./ensure-xcode-cl-tools-installed.sh
./ensure-installed.sh brew --version Homebrew any-version ./install-homebrew.sh
./ensure-installed.sh wget

### Git ###

./ensure-symlinked.sh ~/.gitconfig ../.gitconfig
./ensure-gitconfig-local-exists.sh

### Zsh ###

./ensure-installed.sh zsh --version zsh 5.2
./ensure-login-shell-set.sh
./ensure-antigen-installed.sh
./ensure-installed.sh autojump
./ensure-symlinked.sh ~/.zshrc ../.zshrc

### Tmux ###

./ensure-installed.sh tmux -V tmux 2.2
./ensure-symlinked.sh ~/.tmux.conf ../.tmux.conf

### Leiningen ###

./ensure-installed.sh javac -version javac 1.6 ./install-jdk.sh
./ensure-installed.sh lein --version Leiningen 2.6.1 brew leiningen
./ensure-symlinked.sh ~/.lein/profiles.clj ../profiles.clj

### Emacs ###

./ensure-installed.sh emacs --version "GNU Emacs" 24.5.1 ./install-emacs.sh
if [[ /usr/local/bin/emacs -ef emacs ]]; then
    ./ensure-symlinked.sh /usr/local/bin/emacsw emacsw
else
    ./ensure-symlinked.sh /usr/local/bin/emacsw "$(which emacs)"
fi
./ensure-symlinked.sh ~/.emacs
./ensure-symlinked.sh ~/.emacs.el
./ensure-symlinked.sh ~/.emacs.d/init.el ../init.el

### Utilities ###

./ensure-installed.sh tree
./ensure-installed.sh tmuxinator version tmuxinator 0.8.1 gem

### Cleanup ###

rmdir originals/$uuid 2>/dev/null && echo "[setup] No backups were made, deleting originals/$uuid." || true
rmdir originals 2>/dev/null && echo "[setup] No backup folders remaining, deleting originals." || true

trap EXIT

echo
echo "[setup] We're all done. Enjoy!"
echo "[setup] Note that programs such as Zsh, Leiningen, and Emacs may still have to download dependencies."
echo "[setup] If the dotfiles repository is not in the correct place, simply move it and run this script again. Your symlinks will be updated automatically."
echo "[setup] Starting a new shell session in $(pwd)."
read -p "[setup] Press RET to continue."

exec zsh -l

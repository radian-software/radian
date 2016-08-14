#!/bin/bash

set -e
set -o pipefail

### Prevent sourcing ###

interpreter_name="$(basename "$SHELL")"
if [[ $0 != $BASH_SOURCE ]]; then
    echo "[setup] Something is wrong with the way this script is being run."
    echo "[setup] Perhaps you sourced it instead of running it as an executable."
    echo "[setup] Or your system's version of bash could be too old."
    bash --version
    set +e
    set +o pipefail
    return 1 2>/dev/null || exit 1
fi

### Compute features ###

source compute-features.sh

### Setup ###

echo "[setup] Setting up raxod502/dotfiles. Prepare to be amazed."

trap 'echo && echo "[setup] It looks like an error occurred. Please try to fix it, and then run this script again."' EXIT

cd "$(dirname "$0")"

export uuid=$(uuidgen)
mkdir originals 2>/dev/null || true
mkdir originals/$uuid
echo "[setup] The UUID for this session is $uuid."

### Bootstrapping ###

./ensure-xcode-cl-tools-installed.sh

if feature brew; then
    ./ensure-installed.sh brew --version Homebrew any-version ./install-homebrew.sh
fi

if feature wget; then
    ./ensure-installed.sh wget
fi

### Git ###

if feature git; then
    ./ensure-symlinked.sh ~/.gitconfig ../.gitconfig
    ./ensure-gitconfig-local-exists.sh
fi

### Zsh ###

if feature zsh; then
    ./ensure-installed.sh zsh --version zsh 5.2
    ./ensure-login-shell-set.sh
    ./ensure-antigen-installed.sh
    ./ensure-installed.sh autojump
    ./ensure-symlinked.sh ~/.zshrc ../.zshrc
fi

### Tmux ###

if feature tmux; then
    ./ensure-installed.sh tmux -V tmux 2.2
    ./ensure-symlinked.sh ~/.tmux.conf ../.tmux.conf
fi

### Leiningen ###

if feature leiningen; then
    ./ensure-installed.sh javac -version javac 1.6 ./install-jdk.sh
    ./ensure-installed.sh lein --version Leiningen 2.6.1 brew leiningen
    ./ensure-symlinked.sh ~/.lein/profiles.clj ../profiles.clj
fi

### Emacs ###

if feature emacs; then
    ./ensure-installed.sh emacs --version "GNU Emacs" 24.5.1 ./install-emacs.sh
    if [[ /usr/local/bin/emacs -ef emacs ]]; then
        ./ensure-symlinked.sh /usr/local/bin/emacsw emacsw
    else
        ./ensure-symlinked.sh /usr/local/bin/emacsw "$(which emacs)"
    fi
    ./ensure-symlinked.sh ~/.emacs
    ./ensure-symlinked.sh ~/.emacs.el
    ./ensure-symlinked.sh ~/.emacs.d/init.el ../init.el
fi

### Utilities ###

if feature tree; then
    ./ensure-installed.sh tree
fi

if feature tmuxinator; then
    ./ensure-installed.sh tmuxinator version tmuxinator 0.8.1 gem
fi

### Cleanup ###

rmdir originals/$uuid 2>/dev/null && echo "[setup] No backups were made, deleting originals/$uuid." || true
rmdir originals 2>/dev/null && echo "[setup] No backup folders remaining, deleting originals." || true

trap EXIT

echo
echo "[setup] We're all done. Enjoy!"
echo "[setup] Note that programs such as Zsh, Leiningen, and Emacs may still have to download dependencies."
echo "[setup] If the dotfiles repository is not in the correct place, simply move it and run this script again. Your symlinks will be updated automatically."
if feature zsh; then
    echo "[setup] Starting a new shell session in $(pwd)."
    echo "[setup] Please be aware that the new shell session will be nested inside your original shell session."
    echo "[setup] Any aliases or environment variables will not be cleared."
fi
read -p "[setup] Press RET to continue."

if feature zsh; then
    exec zsh -l
fi

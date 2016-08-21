#!/bin/bash

### Prevent sourcing ###

interpreter_name="$(basename "$SHELL")"
if [[ $0 != $BASH_SOURCE ]]; then
    echo "[setup] Something is wrong with the way this script is being run."
    echo "[setup] Perhaps you sourced it instead of running it as an executable."
    echo "[setup] Or your system's version of bash could be too old."
    bash --version
    return 1 2>/dev/null || exit 1
fi

### Script environment ###

set -e
set -o pipefail
cd "$(dirname "$0")"

### Error handling ###

handle_error() {
    set +e
    set +o pipefail
    find originals -type d -empty -delete 2>/dev/null || true
    echo
    echo "[setup] It looks like an error occurred. Please try to fix it, and then run this script again."
}

trap handle_error EXIT

### Compute features ###

specs=(
    "brew"
    "wget -> brew"
    "git"
    "zsh -> brew"
    "tmux -> brew"
    "leiningen -> brew"
    "emacs -> wget"
    "tree -> brew"
    "tmuxinator -> tmux"
)

source compute-features.sh

### Create necessary directories ###

export uuid="$(date +"%F=%T")=$(uuidgen)"
mkdir -p originals/$uuid
echo "[setup] The UUID for this session is $uuid."

mkdir ../local 2>/dev/null || true

### Warn the user of upcoming awesomeness ###

echo "[setup] Setting up raxod502/dotfiles. Prepare to be amazed."

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
    # We want to do the local setup first, so that it can read any preexisting
    # config to copy over from the original ~/.gitconfig.
    ./ensure-symlinked.sh ~/.gitconfig.local ../local/.gitconfig.local ./setup-gitconfig-local.sh
    ./ensure-symlinked.sh ~/.gitconfig ../.gitconfig
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
    # If the .emacs or .emacs.el files are present, then Emacs will not load init.el.
    # Therefore, we need to ensure that these files do not exist.
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

### Make sure we're in a git repository ###

./ensure-running-in-repo.sh

### Cleanup ###

trap EXIT
set +e
set +o pipefail

### Finished! ###

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

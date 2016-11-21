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

prune_originals() {
    find originals -type d -empty -delete 2>/dev/null || true
}

handle_error() {
    set +e
    set +o pipefail
    prune_originals
    echo
    echo "[setup] It looks like an error occurred. Please try to fix it, and then run this script again."
}

trap handle_error EXIT

### Define helper functions ###

define() {
    IFS='' read -r -d '' $1 || true
}

export -f define

### Compute features ###

specs=(
    "wget"
    "git-dotfiles"
    "zsh"
    "antigen -> zsh"
    "autojump"
    "zsh-dotfiles -> zsh antigen"
    "tmux"
    "tmux-dotfiles -> tmux"
    "java"
    "leiningen java"
    "leiningen-dotfiles -> leiningen"
    "racket -> wget"
    "cmake"
    "libclang"
    "emacs -> wget"
    "emacs-dotfiles -> emacs"
    "ag"
    "tree"
    "tmuxinator tmux"
)

source compute-features.sh

### General setup ###

source generate-uuid.sh
mkdir -p originals/$uuid
echo "[setup] The UUID for this session is $uuid."
repo_name="$(cd .. && basename "$PWD")"

source create-radian-local.sh

### Warn the user of upcoming awesomeness ###

echo "[setup] Setting up Radian. Prepare to be amazed."

### Bootstrapping ###

./ensure-xcode-cl-tools-installed.sh
./ensure-installed.sh brew --version Homebrew any-version ./install-homebrew.sh
./ensure-installed.sh grealpath --version "realpath (GNU coreutils)" any-version brew coreutils
./ensure-installed.sh wget

### Radian symlinks ###

./ensure-symlinked.sh ~/.radian ..
./ensure-symlinked.sh ~/.radian-local ../../radian-local

### Git ###

if feature git-dotfiles; then
    # We want to do the local setup first, so that it can read any preexisting
    # config to copy over from the original ~/.gitconfig.
    ./ensure-symlinked.sh ~/.gitconfig.local ../../radian-local/.gitconfig.local ./create-gitconfig-local.sh
    ./ensure-symlinked.sh ~/.gitconfig ../.gitconfig
    ./ensure-symlinked.sh ~/.gitexclude ../.gitexclude
fi

### Zsh ###

if feature zsh; then
    ./ensure-installed.sh zsh --version zsh 5.2
    ./ensure-login-shell-set.sh
    # $SHELL should be set by zsh in a new shell session, but some
    # programs don't bother to check the login shell and instead just
    # use the value of $SHELL, which is not set by 'exec -l zsh',
    # unfortunately. One of these programs is tmux, with the result
    # that if you run setup.sh from bash and immediately start tmux,
    # newly created windows will still be using bash. Exporting $SHELL
    # should fix the problem, though.
    export SHELL="$(which zsh)"
fi

if feature antigen; then
    ./ensure-antigen-installed.sh
fi

if feature autojump; then
    ./ensure-installed.sh autojump
fi

if feature zsh-dotfiles; then
    ./ensure-symlinked.sh ~/.zshrc ../.zshrc
    ./ensure-symlinked.sh ~/.zshrc.before.local ../../radian-local/.zshrc.before.local ./create-zshrc-before-local.sh
    ./ensure-symlinked.sh ~/.zshrc.antigen.local ../../radian-local/.zshrc.antigen.local ./create-zshrc-antigen-local.sh
    ./ensure-symlinked.sh ~/.zshrc.local ../../radian-local/.zshrc.local ./create-zshrc-local.sh
fi

### Tmux ###

if feature tmux; then
    ./ensure-installed.sh reattach-to-user-namespace
    ./ensure-installed.sh tmux -V tmux 2.2
fi

if feature tmux-dotfiles; then
    ./ensure-symlinked.sh ~/.tmux.conf ../.tmux.conf
    ./ensure-symlinked.sh ~/.tmux.local.conf ../../radian-local/.tmux.local.conf ./create-tmux-local-conf.sh
fi

### Leiningen ###

if feature java; then
    ./ensure-installed.sh javac -version javac 1.6 ./install-java.sh
fi

if feature leiningen; then
    if [[ -e ~/.lein/profiles.clj || -L ~/.lein/profiles.clj ]]; then
        echo "[setup] Temporarily moving profiles.clj."
    fi
    ./ensure-symlinked.sh ~/.lein/profiles.clj
    ./ensure-installed.sh lein --version Leiningen 2.6.1 brew leiningen
    if [[ -e originals/$uuid/profiles.clj || -L originals/$uuid/profiles.clj ]]; then
        echo "[setup] Restoring profiles.clj."
        mv originals/$uuid/profiles.clj ~/.lein/profiles.clj
    fi
fi

if feature leiningen-dotfiles; then
    ./ensure-not-version-controlled.sh ~/.lein
    ./ensure-symlinked.sh ~/.lein/profiles.clj ../profiles.clj
fi

### Racket ###

if feature racket; then
    ./ensure-installed.sh racket --version "Welcome to Racket v" 6.6 ./install-racket.sh
fi

### C ###

if feature cmake; then
    ./ensure-installed.sh cmake --version "cmake version" 3.7.0
fi

if feature libclang; then
    ./ensure-installed.sh libclang version libclang 3.9.0 brew llvm --require --headless
fi

### Emacs ###

if feature emacs; then
    ./ensure-installed.sh emacs --version "GNU Emacs" 24.5.1 ./install-emacs.sh
    ./ensure-symlinked.sh /usr/local/bin/emacsw emacsw
    ./ensure-symlinked.sh /usr/local/bin/emacsc emacsc
    ./ensure-symlinked.sh /usr/local/bin/emacscw emacscw
fi

if feature emacs-dotfiles; then
    # If the .emacs or .emacs.el files are present, then Emacs will not load init.el.
    # Therefore, we need to ensure that these files do not exist.
    ./ensure-symlinked.sh ~/.emacs
    ./ensure-symlinked.sh ~/.emacs.el
    ./ensure-not-version-controlled.sh ~/.emacs.d
    ./ensure-symlinked.sh ~/.emacs.d/init.el ../init.el
    ./ensure-symlinked.sh ~/.emacs.d/init.before.local.el ../../radian-local/init.before.local.el ./create-init-before-local-el.sh
    ./ensure-symlinked.sh ~/.emacs.d/init.local.el ../../radian-local/init.local.el ./create-init-local-el.sh
fi

### Utilities ###

if feature ag; then
    ./ensure-installed.sh ag --version ag any-version brew the_silver_searcher
fi

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
prune_originals

### Finished! ###

echo
echo "[setup] We're all done. Enjoy!"
echo "[setup] Note that programs such as Zsh, Leiningen, and Emacs may still have to download dependencies."
echo "[setup] If the Radian repository is not in the correct place, simply move it and run this script again. Your symlinks will be updated automatically."
if feature zsh; then
    echo "[setup] Starting a new shell session in $(pwd)."
    echo "[setup] Please be aware that the new shell session will be nested inside your original shell session."
    echo "[setup] Any aliases or environment variables will not be cleared."
fi
read -p "[setup] Press RET to continue."

if feature zsh; then
    exec zsh -l
fi

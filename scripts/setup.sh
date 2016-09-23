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

### Compute features ###

specs=(
    "brew"
    "wget -> brew"
    "git"
    "zsh -> brew"
    "tmux -> brew"
    "leiningen -> brew"
    "racket -> wget"
    "emacs -> wget"
    "tree -> brew"
    "tmuxinator -> tmux"
)

source compute-features.sh

### Create necessary directories ###

source generate-uuid.sh
mkdir -p originals/$uuid
echo "[setup] The UUID for this session is $uuid."

repo_name="$(basename "$PWD")"
if [[ $repo_name == dotfiles-local ]]; then
    echo "[setup] Fatal error: this repository cannot be called 'dotfiles-local'. Please rename it."
    exit 1
fi
if [[ -f ../../dotfiles-local || -L ../../dotfiles-local && ! -e ../../dotfiles-local ]]; then
    echo "[setup] You appear to have something called 'dotfiles-local' next to '$repo_name' that is either a file or an invalid symlink."
    echo "[setup] Moving it to originals/$uuid."
    mv ../../dotfiles-local originals/$uuid/dotfiles-local
fi
mkdir ../../dotfiles-local 2>/dev/null || true
touch ../../dotfiles-local/.projectile

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
    ./ensure-symlinked.sh ~/.gitconfig.local ../../dotfiles-local/.gitconfig.local ./create-gitconfig-local.sh
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
    ./ensure-antigen-installed.sh
    ./ensure-installed.sh autojump
    ./ensure-symlinked.sh ~/.zshrc ../.zshrc
    ./ensure-symlinked.sh ~/.zshrc.before.local ../../dotfiles-local/.zshrc.before.local ./create-zshrc-before-local.sh
    ./ensure-symlinked.sh ~/.zshrc.antigen.local ../../dotfiles-local/.zshrc.antigen.local ./create-zshrc-antigen-local.sh
    ./ensure-symlinked.sh ~/.zshrc.local ../../dotfiles-local/.zshrc.local ./create-zshrc-local.sh
fi

### Tmux ###

if feature tmux; then
    ./ensure-installed.sh tmux -V tmux 2.2
    ./ensure-symlinked.sh ~/.tmux.conf ../.tmux.conf
    ./ensure-symlinked.sh ~/.tmux.local.conf ../../dotfiles-local/.tmux.local.conf ./create-tmux-local-conf.sh
fi

### Leiningen ###

if feature leiningen; then
    ./ensure-installed.sh javac -version javac 1.6 ./install-jdk.sh
    ./ensure-installed.sh lein --version Leiningen 2.6.1 brew leiningen
    ./ensure-symlinked.sh ~/.lein/profiles.clj ../profiles.clj
fi

### Racket ###

if feature racket; then
    ./ensure-installed.sh racket --version "Welcome to Racket v" 6.6 ./install-racket.sh
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
    ./ensure-symlinked.sh ~/.emacs.d/init.before.local.el ../../dotfiles-local/init.before.local.el ./create-init-before-local-el.sh
    ./ensure-symlinked.sh ~/.emacs.d/init.pre.local.el ../../dotfiles-local/init.pre.local.el ./create-init-pre-local-el.sh
    ./ensure-symlinked.sh ~/.emacs.d/init.post.local.el ../../dotfiles-local/init.post.local.el ./create-init-post-local-el.sh
    ./ensure-symlinked.sh ~/.emacs.d/init.local.el ../../dotfiles-local/init.local.el ./create-init-local-el.sh
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
prune_originals

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

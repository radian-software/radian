#!/usr/bin/env bash

set -e
set -o pipefail

# If you pass an argument to this script, it is assumed to be where
# the Radian repository will go. This is used by docker-install.bash,
# which runs this script in a temporary directory rather than from
# Radian.

script="$(realpath "$0")"
scripts="$(dirname "$script")"
radian="${1:-$(dirname "$scripts")}"

safe_link() {
    if [[ -e "$2" && ! -L "$2" ]]; then
        echo "already exists and not a symlink: $2" >&2
        exit 1
    fi

    ln -sf "$1" "$2"
}

mkdir -p "$HOME/.emacs.d/straight/versions"
safe_link "$radian/emacs/early-init.el" "$HOME/.emacs.d/early-init.el"
safe_link "$radian/emacs/init.el" "$HOME/.emacs.d/init.el"
safe_link "$radian/emacs/versions.el" \
          "$HOME/.emacs.d/straight/versions/radian.el"

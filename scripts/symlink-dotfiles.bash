#!/usr/bin/env bash

set -e
set -o pipefail

script="$(realpath "$0")"
scripts="$(dirname "$script")"
radian="$(dirname "$scripts")"

safe_link() {
    if [[ -e "$2" && ! -L "$2" ]]; then
        echo "already exists and not a symlink: $2" >&2
        exit 1
    fi

    ln -sf "$1" "$2"
}

mkdir -p "$HOME/.emacs.d/straight/versions"
safe_link "$radian/emacs/init.el" "$HOME/.emacs.d/init.el"
safe_link "$radian/emacs/versions.el" \
          "$HOME/.emacs.d/straight/versions/radian.el"

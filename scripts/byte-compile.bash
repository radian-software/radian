#!/usr/bin/env bash

set -e
set -o pipefail

emacs --batch -l "$HOME/.emacs.d/init.el" \
      -f radian-byte-compile 2>&1 \
    | (! grep .)

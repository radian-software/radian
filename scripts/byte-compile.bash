#!/usr/bin/env bash

set -e
set -o pipefail

emacs --batch \
      --eval "(setq straight-safe-mode t)" \
      --load "$HOME/.emacs.d/init.el" \
      --funcall radian-byte-compile 2>&1 \
    | (! grep .)

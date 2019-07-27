#!/usr/bin/env bash

set -e
set -o pipefail

emacs --batch \
      --eval "(setq straight-safe-mode t)" \
      --load "$HOME/.emacs.d/init.el" \
      --funcall el-patch-validate-all 2>&1 \
    | (! grep -F invalid)

#!/usr/bin/env bash

set -e
set -o pipefail

(emacs --batch \
       --eval "(progn                                        \
                (setq straight-safe-mode t                   \
                      radian-compiling t)                    \
                (load (expand-file-name \"init.el\"          \
                      user-emacs-directory) nil t))"         \
       --funcall radian-batch-byte-compile 2>&1              \
     | (grep -v "In toplevel form"                  || true) \
     | (grep -v "In end of data"                    || true) \
     | (grep -v "Warning: Package cl is deprecated" || true) \
     | (! grep .)) || (rm -f emacs/radian.elc; false)

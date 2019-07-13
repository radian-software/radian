#!/usr/bin/env bash

set -e
set -o pipefail

file="emacs/radian.el"

code="$(cat <<EOF

(require 'checkdoc)

;; radian.el is not a package, so don't lint it as one.
(advice-add #'checkdoc-comments :override #'ignore)

(checkdoc-file "$file")

EOF
)"

emacs -Q --batch --eval "(progn $code)" 2>&1 | (! grep .)

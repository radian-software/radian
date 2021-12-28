#!/usr/bin/env bash

set -e
set -o pipefail

file="emacs/radian.el"

code="$(cat <<EOF

(require 'checkdoc)

;; radian.el is not a package, so don't lint it as one.
(advice-add #'checkdoc-comments :override #'ignore)

;; If any unsafe local variables are found in the Local Variables list
;; then they all are ignored -_- and this variable didn't exist prior
;; to Emacs 28
(put 'no-native-compile 'safe-local-variable #'booleanp)

(checkdoc-file "$file")

EOF
)"

emacs -Q --batch --eval "(progn $code)" 2>&1 | (! grep .)

#!/bin/bash

set -e
set -o pipefail

echo "[create-init-post-local-el] Setting up init.post.local.el."
contents=$(cat <<'EOF'
;;; This file is run just after packages are loaded in init.el, but before
;;; package-specific configuration is applied.
EOF
        )
echo "$contents" > ../../dotfiles-local/init.post.local.el
echo "[create-init-post-local-el] Wrote the following to dotfiles-local/init.post.local.el:"
cat ../../dotfiles-local/init.post.local.el

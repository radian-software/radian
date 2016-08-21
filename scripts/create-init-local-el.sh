#!/bin/bash

set -e
set -o pipefail

echo "[create-init-local-el] Setting up init.local.el."
contents=$(cat <<'EOF'
;;; This file is run at the very end of init.el, so you can use it to override
;;; things or add your own customizations.
EOF
        )
echo "$contents" > ../../dotfiles-local/init.local.el
echo "[create-init-local-el] Wrote the following to dotfiles-local/init.local.el:"
cat ../../dotfiles-local/init.local.el

#!/bin/bash

set -e
set -o pipefail

echo "[create-zshrc-local] Setting up .zshrc.local."
contents=$(cat <<'EOF'
# This file is run at the very end of .zshrc, so you can use it to
# override things or add your own customizations.
EOF
        )
echo "$contents" > ../../dotfiles-local/.zshrc.local
echo "[create-zshrc-local] Wrote the following to dotfiles-local/.zshrc.local:"
cat ../../dotfiles-local/.zshrc.local

#!/bin/bash

set -e
set -o pipefail

echo "[create-tmux-local-conf] Setting up .tmux.local.conf."
contents=$(cat <<'EOF'
# This file is run at the end of .tmux.conf.
EOF
        )
echo "$contents" > ../../dotfiles-local/.tmux.local.conf
echo "[create-tmux-local-conf] Wrote the following to dotfiles-local/.tmux.local.conf:"
cat ../../dotfiles-local/.tmux.local.conf

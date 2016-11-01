#!/bin/bash

set -e
set -o pipefail

echo "[create-tmux-local-conf] Setting up .tmux.local.conf."
define contents <<'EOF'
# This file is run at the very end of .tmux.conf, so you can use it to
# override things or add your own customizations.
EOF
echo "$contents" > ../../radian-local/.tmux.local.conf
echo "[create-tmux-local-conf] Wrote the following to radian-local/.tmux.local.conf:"
cat ../../radian-local/.tmux.local.conf

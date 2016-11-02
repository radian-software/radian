#!/bin/bash

set -e
set -o pipefail

echo "[create-init-local-el] Setting up init.local.el."
define contents <<'EOF'
;;; This file is run at the very end of init.el, so you can use it to override
;;; things or add your own packages and customizations.
EOF
echo -n "$contents" > ../../radian-local/init.local.el
echo "[create-init-local-el] Wrote the following to radian-local/init.local.el:"
cat ../../radian-local/init.local.el

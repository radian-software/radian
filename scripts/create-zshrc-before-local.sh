#!/bin/bash

set -e
set -o pipefail

define contents <<'EOF'
#!/usr/bin/env zsh
#
# This file is run near the beginning of .zshrc.
#
# Radian uses zplug [1] to manage Zsh plugins. The default bundle list
# is stored in $bundles; you can add and remove items using add_bundle
# and remove_bundle. Each element of the bundle list is split on
# spaces and passed as arguments to zplug. Here are some example uses
# of the above functions:
#
# add_bundle "plugins/lol, from:oh-my-zsh"
# remove_bundle "zsh-users/zsh-autosuggestions"
#
# [1]: https://github.com/zplug/zplug
EOF

echo "[create-zshrc-before-local] Setting up .zshrc.before.local."
echo -n "$contents" > ../../radian-local/.zshrc.before.local
echo "[create-zshrc-before-local] Wrote the following to radian-local/.zshrc.before.local:"
cat ../../radian-local/.zshrc.before.local

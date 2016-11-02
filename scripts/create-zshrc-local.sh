#!/bin/bash

set -e
set -o pipefail

echo "[create-zshrc-local] Setting up .zshrc.local."
define contents <<'EOF'
#!/usr/bin/env zsh
#
# This file is run at the very end of .zshrc, so you can use it to
# override things or add your own customizations.
EOF

if [[ $EDITOR ]]; then
    echo "[create-zshrc-local] You currently have \$EDITOR set to $EDITOR."
    echo -n "[create-zshrc-local] Would you like to carry this over to .zshrc.local? (y/n) "
    read answer
    if (echo "$answer" | grep -qi "^y"); then
        define format <<'EOF'

export EDITOR='%s'
EOF
        printf -v format "$format" "$EDITOR"
        contents="$contents$format"
    fi
fi

echo -n "$contents" > ../../radian-local/.zshrc.local
echo "[create-zshrc-local] Wrote the following to radian-local/.zshrc.local:"
cat ../../radian-local/.zshrc.local

#!/bin/bash

set -e
set -o pipefail

source ensure-uuid-set.sh

echo "[ensure-gitconfig-local-exists] Checking for a ~/.gitconfig.local."
if [[ -d ~/.gitconfig.local || -L ~/.gitconfig.local && ! -e ~/.gitconfig.local ]]; then
    echo "[ensure-gitconfig-local-exists] Your ~/.gitconfig.local appears to be either a directory or an invalid symlink."
    echo "[ensure-gitconfig-local-exists] Moving it to originals/$uuid."
    mv ~/.gitconfig.local originals/$uuid/.gitconfig.local
fi
if [[ ! -e ~/.gitconfig.local ]]; then
    echo "[ensure-gitconfig-local-exists] It looks like you don't have a ~/.gitconfig.local"
    echo "[ensure-gitconfig-local-exists] To set one up, please provide your full name and email address."
    echo -n "[ensure-gitconfig-local-exists] Full name: "
    read name
    echo -n "[ensure-gitconfig-local-exists] Email address: "
    read email_address
    format=$(cat <<'EOF'
[user]
        name = %s
        email = %s
EOF
          )
    printf "$format\n" "$name" "$email_address" > ~/.gitconfig.local
    echo "[ensure-gitconfig-local-exists] Do you have a preferred editor for Git commit messages?"
    echo "[ensure-gitconfig-local-exists] If you do not set one, Git will default to \$EDITOR or, as a fallback, vim."
    echo -n "[ensure-gitconfig-local-exists] Choose an editor explicitly? (y/n) "
    read answer
    if (echo "$answer" | grep -qi "^y"); then
        echo -n "[ensure-gitconfig-local-exists] Editor: "
        read editor
        format=$(cat <<'EOF'
[core]
        editor = %s
EOF
              )
        printf "$format\n" "$editor" >> ~/.gitconfig.local
    fi
    echo "[ensure-gitconfig-local-exists] Written the following to ~/.gitconfig.local:"
    cat ~/.gitconfig.local
else
    echo "[ensure-gitconfig-local-exists] It looks like you already have a ~/.gitconfig.local; skipping setup."
    echo "[ensure-gitconfig-local-exists] To set up your username, email address, and editor again, please remove ~/.gitconfig.local."
    echo "[ensure-gitconfig-local-exists] Alternatively, you can just edit the file manually."
fi

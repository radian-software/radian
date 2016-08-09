#!/bin/bash

echo '[setup] Checking for a ~/.gitconfig.'
if [[ ~/.gitconfig -ef ../.gitconfig ]]; then
    echo '[setup] It appears that ~/.gitconfig is already correctly symlinked.'
else
    if [[ -e ~/.gitconfig || -L ~/.gitconfig ]]; then
        echo "[setup] Found one, moving it to originals/$uuid."
        mv ~/.gitconfig originals/$uuid/.gitconfig
    else
        echo "[setup] Looks like you don't have one."
    fi
    echo '[setup] Creating symlink for .gitconfig.'
    ln -s "$(cd .. && pwd)/.gitconfig" ~/.gitconfig
fi

echo '[setup] Checking for a ~/.gitconfig.local.'
if [[ -d ~/.gitconfig.local || -L ~/.gitconfig.local && ! -e ~/.gitconfig.local ]]; then
    echo '[setup] Your ~/.gitconfig.local appears to be either a directory or an invalid symlink.'
    echo "[setup] Moving it to originals/$uuid."
    mv ~/.gitconfig.local originals/$uuid/.gitconfig.local
fi
if ! [[ -e ~/.gitconfig.local ]]; then
    echo "[setup] It looks like you don't have a ~/.gitconfig.local"
    echo '[setup] To set one up, please provide your full name and email address.'
    echo -n '[setup] Full name: '
    read name
    echo -n '[setup] Email address: '
    read email_address
    format=$(cat <<'EOF'
[user]
        name = %s
        email = %s
EOF
          )
    printf "$format\n" "$name" "$email_address" > ~/.gitconfig.local
    echo '[setup] Do you have a preferred editor for Git commit messages?'
    echo '[setup] If you do not set one, Git will default to $EDITOR or, as a fallback, vim.'
    echo -n '[setup] Choose an editor explicitly? (y/n) '
    read answer
    if (echo "$answer" | grep -qi "^y"); then
        echo -n '[setup] Editor: '
        read editor
        format=$(cat <<'EOF'
[core]
        editor = %s
EOF
              )
        printf "$format\n" "$editor" >> ~/.gitconfig.local
    fi
    echo '[setup] Written the following to ~/.gitconfig.local:'
    cat ~/.gitconfig.local
else
    echo '[setup] It looks like you already have a ~/.gitconfig.local; skipping setup.'
    echo '[setup] To set up your username, email address, and editor again, please remove ~/.gitconfig.local.'
    echo '[setup] Alternatively, you can just edit the file manually.'
fi

#!/bin/bash

set -e
set -o pipefail

echo '[setup] Checking if Antigen is installed to ~/.antigen-repo.'
if [[ -d ~/.antigen-repo ]]; then
    echo '[setup] It appears that Antigen is already installed.'
else
    echo '[setup] It appears that Antigen is not yet installed.'
    if [[ -f ~/.antigen-repo || -L ~/.antigen-repo ]]; then
        echo '[setup] Your ~/.antigen-repo appears to be either a file or an invalid symlink.'
        echo "[setup] Moving it to originals/$uuid."
        mv ~/.antigen-repo originals/$uuid/.antigen-repo
    fi
    echo '[setup] Installing Antigen.'
    git clone https://github.com/zsh-users/antigen.git ~/.antigen-repo
fi

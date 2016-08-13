#!/bin/bash

set -e
set -o pipefail

source ensure-uuid-set.sh

echo "[ensure-antigen-installed] Checking if Antigen is installed to ~/.antigen-repo."
if [[ -d ~/.antigen-repo ]]; then
    echo "[ensure-antigen-installed] It appears that Antigen is already installed."
else
    echo "[ensure-antigen-installed] It appears that Antigen is not yet installed."
    if [[ -f ~/.antigen-repo || -L ~/.antigen-repo ]]; then
        echo "[ensure-antigen-installed] Your ~/.antigen-repo appears to be either a file or an invalid symlink."
        echo "[ensure-antigen-installed] Moving it to originals/$uuid."
        mv ~/.antigen-repo originals/$uuid/.antigen-repo
    fi
    echo "[ensure-antigen-installed] Installing Antigen."
    git clone https://github.com/zsh-users/antigen.git ~/.antigen-repo
fi

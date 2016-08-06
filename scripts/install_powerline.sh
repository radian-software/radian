#!/bin/bash

powerline_installed() {
    hash powerline && hash powerline-config && hash powerline-daemon && hash powerline-lint && hash powerline-render
}

echo '[setup] Checking if powerline is installed.'
if powerline_installed; then
    echo '[setup] It appears that powerline is already installed.'
else
    if pip show powerline-status; then
        echo '[setup] It appears that powerline has already been installed using pip, but it is not symlinked correctly.'
        echo '[setup] Uninstalling the original version of powerline.'
        pip uninstall powerline-status
        if pip show powerline-status; then
            echo '[setup] Fatal error: powerline should no longer be installed.'
            exit 1
        fi
    fi
    echo '[setup] Installing powerline using pip.'
    pip install powerline-status
    if ! powerline_installed; then
        echo '[setup] Fatal error: powerline should be installed by this point.'
        exit 1
    fi
fi

echo '[setup] Determining location of powerline.conf.'
repository_root="$(pip show powerline-status | grep '^Location' | cut -c 11-)"
printf 'source "%s/powerline/bindings/tmux/powerline.conf"\n' "$repository_root" >> .tmux.powerline.conf
echo '[setup] Written the following to .tmux.powerline.conf:'
cat .tmux.powerline.conf

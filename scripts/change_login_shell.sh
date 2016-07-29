#!/bin/bash

set -e

echo "[setup] Checking the login shell."
if finger "$USER" | grep -q 'Shell: /.*/zsh'; then
    echo "[setup] The login shell appears to already be zsh."
else
    if ! hash finger 2>/dev/null; then
        echo "[setup] The 'finger' utility does not appear to be available. Can't determine the login shell."
    fi
    echo '[setup] The login shell appears to be something other than zsh.'
    echo '[setup] Changing the login shell to zsh.'
    until chsh -s $(which zsh); do
        echo "[setup] Encountered an error. Trying again in two seconds."
        sleep 2
    done
fi

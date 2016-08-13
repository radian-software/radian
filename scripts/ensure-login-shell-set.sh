#!/bin/bash

set -e
set -o pipefail

echo "[ensure-login-shell-set] Checking the login shell."
if finger "$USER" | grep "Shell: $(which zsh)"; then
    echo "[ensure-login-shell-set] The login shell appears to already be $(which zsh)."
else
    if command -v finger &>/dev/null; then
        echo "[ensure-login-shell-set] The login shell appears to be something other than $(which zsh)."
    else
        echo "[ensure-login-shell-set] The 'finger' utility does not appear to be available. Can't determine the login shell."
        echo "[ensure-login-shell-set] Assuming it is not set correctly, to be safe."
    fi
    if ! fgrep -q "$(which zsh)" /etc/shells; then
        echo "[ensure-login-shell-set] It looks like $(which zsh) is not designated as an acceptable login shell."
        echo "[ensure-login-shell-set] Adding $(which zsh) to /etc/shells."
        sudo -s 'echo "$(which zsh)" >> /etc/shells'
    fi
    echo "[ensure-login-shell-set] Changing the login shell to $(which zsh)."
    until chsh -s "$(which zsh)"; do
        echo "[ensure-login-shell-set] Encountered an error. Trying again in two seconds."
        sleep 2
    done
fi

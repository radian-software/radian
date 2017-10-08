#!/bin/sh

# shellcheck disable=SC1090

if [ -f "$HOME/.config/scripts/path.sh" ]; then
    . "$HOME/.config/scripts/path.sh"
fi

if [ -f "$HOME/.config/scripts/gpg.sh" ]; then
    . "$HOME/.config/scripts/gpg.sh"
fi

if [ -x "$HOME/.config/browserpass/browserpass" ]; then
    "$HOME/.config/browserpass/browserpass"
fi

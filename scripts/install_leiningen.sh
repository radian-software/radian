#!/bin/bash

set -e

echo '[setup] Checking to see if Leiningen 2.6.1 or newer is installed.'

if lein --version \
        | egrep 'Leiningen (2\.(6\.[1-9][0-9]*|[7-9]|[1-9][0-9]+)|[3-9]|[1-9][0-9]+)'; then
    echo '[setup] It looks like an appropriate version of Leiningen is already installed.'
else
    echo '[setup] It looks like an appropriate version of Leiningen is not installed.'
    if [[ $(brew list --versions leiningen) ]]; then
        echo '[setup] Uninstalling the old version of Leiningen using Homebrew.'
        echo -n '[setup] Is this OK? (y/n) '
        read ANSWER
        if ! (echo "$ANSWER" | grep -qi "^y"); then
            echo '[setup] Aborting.'
            exit 1
        fi
        brew uninstall leiningen
    fi
    echo '[setup] Installing Leiningen using Homebrew.'
    brew install leiningen
    if [[ $(which lein) != /usr/local/bin/lein ]]; then
        echo "[setup] It looks like $(which lein) is earlier on your \$PATH than /usr/local/bin/lein."
        echo '[setup] Creating an alias to override this behavior.'
        alias lein=/usr/local/bin/lein
        echo 'alias lein=/usr/local/bin/lein' >> .zshrc.aliases
        echo '[setup] Executed and added to .zshrc.aliases.'
    fi
fi

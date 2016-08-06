#!/bin/bash

echo '[setup] Checking to see if Leiningen 2.6.1 or newer is installed.'

lein_version_regex='Leiningen (2\.(6\.[1-9][0-9]*|[7-9]|[1-9][0-9]+)|[3-9]|[1-9][0-9]+)'
lein_version_regex_brew='leiningen.* (2\.(6\.[1-9][0-9]*|[7-9]|[1-9][0-9]+)|[3-9]|[1-9][0-9]+)'

if lein --version | egrep "$lein_version_regex"; then
    echo '[setup] It looks like an appropriate version of Leiningen is already installed.'
else
    if brew list --versions leiningen | egrep "$lein_version_regex_brew"; then
        echo '[setup] It looks like an appropriate version of Leiningen has already been installed using Homebrew, but it is not symlinked correctly.'
        echo '[setup] Removing old symlinks using Homebrew.'
        brew unlink leiningen
        echo '[setup] Recreating symlinks using Homebrew.'
        brew link leiningen
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
    fi
    if ! (lein --version | egrep "$lein_version_regex"); then
        echo '[setup] Fatal error: an appropriate version of Leiningen should be installed by this point.'
        exit 1
    fi
fi

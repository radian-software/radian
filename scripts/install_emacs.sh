#!/bin/bash

set -e

echo '[setup] Checking to see if GNU Emacs 24.5.1 or newer is installed.'

EMACS_VERSION_REGEX='GNU Emacs (24\.(5\.[1-9][0-9]*|[6-9]|[1-9][0-9]+)|2[5-9]|[3-9][0-9]+)'

if emacs --version \
        | egrep "$EMACS_VERSION_REGEX"; then
    echo '[setup] It looks like an appropriate version of Emacs is already installed.'
else
    if /Applications/Emacs.app/Contents/MacOS/Emacs --version \
            | egrep "$EMACS_VERSION_REGEX"; then
        echo '[setup] It looks like an appropriate version of Emacs is already installed, but it is not symlinked correctly.'
    else
        echo '[setup] It looks like an appropriate version of Emacs is not installed.'
        if [[ $(brew list --versions emacs) ]]; then
            echo '[setup] Uninstalling the old version of Emacs using Homebrew.'
            echo -n '[setup] Is this OK? (y/n) '
            read ANSWER
            if ! (echo "$ANSWER" | grep -qi "^y"); then
                echo '[setup] Aborting.'
                exit 1
            fi
            brew uninstall emacs
        fi
        if [[ -d "/Applications/Emacs.app" ]]; then
            echo "[setup] Moving the existing version to originals/$UUID."
            mv /Applications/Emacs.app originals/$UUID/Emacs.app
        fi
        echo '[setup] Downloading Emacs from emacsformacosx.com.'
        wget "https://emacsformacosx.com/emacs-builds/Emacs-24.5-1-universal.dmg"
        echo '[setup] Mounting the Emacs disk image.'
        hdiutil mount Emacs-24.5-1-universal.dmg
        echo '[setup] Copying Emacs.app from the disk image to the Applications folder.'
        cp -R /Volumes/Emacs/Emacs.app /Applications/Emacs.app
        echo '[setup] Unmounting the Emacs disk image.'
        hdiutil unmount /Volumes/Emacs
        echo '[setup] Deleting the Emacs disk image.'
        rm Emacs-24.5-1-universal.dmg
    fi
    echo '[setup] Checking for a /usr/local/bin/emacs.'
    if [[ /usr/local/bin/emacs -ef emacs ]]; then
        echo '[setup] It appears that /usr/local/bin/emacs is already correctly symlinked.'
    else
        if [[ -e /usr/local/bin/emacs || -L /usr/local/bin/emacs ]]; then
            echo "[setup] Found one, moving it to originals/$UUID."
            mv /usr/local/bin/emacs originals/$UUID/emacs
        else
            echo "[setup] Looks like you don't have one."
        fi
        echo '[setup] Creating symlink for emacs.'
        ln -s "$(pwd)/emacs" /usr/local/bin/emacs
    fi
    if [[ $(which emacs) != /usr/local/bin/emacs ]]; then
        echo "[setup] It looks like $(which emacs) is earlier on your \$PATH than /usr/local/bin/emacs."
        echo '[setup] Creating an alias to override this behavior.'
        alias emacs=/usr/local/bin/emacs
        echo 'alias emacs=/usr/local/bin/emacs' >> .zshrc.aliases
        echo '[setup] Executed and added to .zshrc.aliases.'
    fi
fi

echo '[setup] Checking for a /usr/local/bin/emacsw.'
if [[ /usr/local/bin/emacs -ef emacs ]]; then
    if [[ /usr/local/bin/emacsw -ef emacsw ]]; then
        echo '[setup] It appears that /usr/local/bin/emacsw is already correctly symlinked.'
    else
        if [[ -e /usr/local/bin/emacsw || -L /usr/local/bin/emacsw ]]; then
            echo "[setup] Found one, moving it to originals/$UUID."
            mv /usr/local/bin/emacsw originals/$UUID/emacsw
        else
            echo "[setup] Looks like you don't have one."
        fi
        echo '[setup] Creating symlink for emacsw.'
        ln -s "$(pwd)/emacsw" /usr/local/bin/emacsw
    fi
else
    if [[ /usr/local/bin/emacsw -ef "$(which emacs)" ]]; then
        echo '[setup] It appears that /usr/local/bin/emacsw is already correctly symlinked.'
    else
        if [[ -e /usr/local/bin/emacsw || -L /usr/local/bin/emacsw ]]; then
            echo "[setup] Found one, moving it to originals/$UUID."
            mv /usr/local/bin/emacsw originals/$UUID/emacsw
        else
            echo "[setup] Looks like you don't have one."
        fi
        echo '[setup] Creating symlink for emacsw.'
        ln -s "$(which emacs)" /usr/local/bin/emacsw
    fi
fi

if [[ $(which emacsw) != /usr/local/bin/emacsw ]]; then
    echo "[setup] It looks like $(which emacsw) is earlier on your \$PATH than /usr/local/bin/emacsw."
    echo '[setup] Creating an alias to override this behavior.'
    alias emacsw=/usr/local/bin/emacsw
    echo 'alias emacsw=/usr/local/bin/emacsw' >> .zshrc.aliases
    echo '[setup] Executed and added to .zshrc.aliases'
fi

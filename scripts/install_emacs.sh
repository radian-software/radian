#!/bin/bash

echo '[setup] Checking to see if GNU Emacs 24.5.1 or newer is installed.'

emacs_version_regex='GNU Emacs (24\.(5\.[1-9][0-9]*|[6-9]|[1-9][0-9]+)|2[5-9]|[3-9][0-9]+)'

if emacs --version | egrep "$emacs_version_regex"; then
    echo '[setup] It looks like an appropriate version of Emacs is already installed.'
else
    if /Applications/Emacs.app/Contents/MacOS/Emacs --version \
            | egrep "$emacs_version_regex"; then
        echo '[setup] It looks like an appropriate version of Emacs is already installed, but it is not symlinked correctly.'
    else
        echo '[setup] It looks like an appropriate version of Emacs is not installed.'
        if [[ $(brew list --versions emacs) ]]; then
            echo '[setup] Uninstalling the old version of Emacs using Homebrew.'
            echo -n '[setup] Is this OK? (y/n) '
            read answer
            if ! (echo "$answer" | grep -qi "^y"); then
                echo '[setup] Aborting.'
                exit 1
            fi
            brew uninstall emacs
        fi
        if [[ -d "/Applications/Emacs.app" ]]; then
            echo "[setup] Moving the existing version to originals/$uuid."
            mv /Applications/Emacs.app originals/$uuid/Emacs.app
        fi
        echo '[setup] Downloading Emacs from emacsformacosx.com.'
        # Specifying the filename makes wget override any existing file by that name
        wget -O Emacs-24.5-1-universal.dmg "https://emacsformacosx.com/emacs-builds/Emacs-24.5-1-universal.dmg"
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
            echo "[setup] Found one, moving it to originals/$uuid."
            mv /usr/local/bin/emacs originals/$uuid/emacs
        else
            echo "[setup] Looks like you don't have one."
        fi
        echo '[setup] Creating symlink for emacs.'
        ln -s "$(pwd)/emacs" /usr/local/bin/emacs
    fi
    if ! (emacs --version | egrep "$emacs_version_regex"); then
        echo '[setup] Fatal error: an appropriate version of Emacs should be installed by this point.'
        exit 1
    fi
fi

echo '[setup] Checking for a /usr/local/bin/emacsw.'
if [[ /usr/local/bin/emacs -ef emacs ]]; then
    if [[ /usr/local/bin/emacsw -ef emacsw ]]; then
        echo '[setup] It appears that /usr/local/bin/emacsw is already correctly symlinked.'
    else
        if [[ -e /usr/local/bin/emacsw || -L /usr/local/bin/emacsw ]]; then
            echo "[setup] Found one, moving it to originals/$uuid."
            mv /usr/local/bin/emacsw originals/$uuid/emacsw
        else
            echo "[setup] Looks like you don't have one."
        fi
        echo '[setup] Creating symlink for emacsw.'
        ln -s "$(pwd)/emacsw" /usr/local/bin/emacsw
        if ! (emacsw --version | egrep "$emacs_version_regex"); then
            echo '[setup] Fatal error: emacsw should be symlinked by this point.'
            exit 1
        fi
    fi
else
    if [[ /usr/local/bin/emacsw -ef "$(which emacs)" ]]; then
        echo '[setup] It appears that /usr/local/bin/emacsw is already correctly symlinked.'
    else
        if [[ -e /usr/local/bin/emacsw || -L /usr/local/bin/emacsw ]]; then
            echo "[setup] Found one, moving it to originals/$uuid."
            mv /usr/local/bin/emacsw originals/$uuid/emacsw
        else
            echo "[setup] Looks like you don't have one."
        fi
        echo '[setup] Creating symlink for emacsw.'
        ln -s "$(which emacs)" /usr/local/bin/emacsw
        if ! (emacsw --version | egrep "$emacs_version_regex"); then
            echo '[setup] Fatal error: emacsw should be symlinked by this point.'
            exit 1
        fi
    fi
fi

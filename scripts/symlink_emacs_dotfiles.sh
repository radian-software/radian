#!/bin/bash

echo '[setup] Checking for a ~/.emacs.'
if [[ -e ~/.emacs || -L ~/.emacs ]]; then
    echo "[setup] Found one, moving it to originals/$uuid."
    mv ~/.emacs originals/$uuid/.emacs
else
    echo "[setup] Looks like you don't have one."
fi

echo '[setup] Checking for a ~/.emacs.el.'
if [[ -e ~/.emacs.el || -L ~/.emacs.el ]]; then
    echo "[setup] Found one, moving it to originals/$uuid."
    mv ~/.emacs.el originals/$uuid/.emacs.el
else
    echo "[setup] Looks like you don't have one."
fi

echo '[setup] Checking for a ~/.emacs.d/init.el.'
if [[ ~/.emacs.d/init.el -ef ../init.el ]]; then
    echo '[setup] It appears that ~/.emacs.d/init.el is already correctly symlinked.'
else
    if [[ -e ~/.emacs.d/init.el || -L ~/.emacs.d/init.el ]]; then
        echo "[setup] Found one, moving it to originals/$uuid."
        mv ~/.emacs.d/init.el originals/$uuid/init.el
    else
        echo "[setup] Looks like you don't have one."
    fi
    echo '[setup] Creating symlink for init.el.'
    if ! [[ -d ~/.emacs.d ]]; then
        if [[ -f ~/.emacs.d || -L ~/.emacs.d ]]; then
            echo '[setup] Your ~/.emacs.d appears to be either a file or an invalid symlink.'
            echo "[setup] Moving it to originals/$uuid."
            mv ~/.emacs.d originals/$uuid/.emacs.d
        fi
        mkdir ~/.emacs.d
    fi
    ln -s "$(cd .. && pwd)/init.el" ~/.emacs.d/init.el
fi

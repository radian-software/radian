#!/bin/bash

set -e
set -o pipefail

echo "[create-init-before-local-el] Setting up init.before.local.el."

contents=$(cat <<'EOF'
;;; This file is run near the beginning of init.el. This is the best place to
;;; override the various parameters shown below. Additionally, you can
;;; customize which packages are loaded, using the radon-add-package and
;;; radon-remove-package functions. They work like this:
;;;
;;; (radon-add-package 'neotree)
;;; (radon-remove-package 'aggressive-indent)
;;;
;;; Or, you can completely overwrite the package list by setting the
;;; radon-packages variable to a new value.
EOF
        )
contents="$contents"$'\n'

echo "[create-init-before-local-el] You can use the default package list, or customize it."
echo "[create-init-before-local-el] Either way, you can configure it later in init.before.local.el."
echo -n "[create-init-before-local-el] Configure packages now? (y/n) "
read answer
if (echo "$answer" | egrep -qi "^y"); then

    collecting_packages=false
    collected_packages=
    while read line; do
        if [[ $collecting_packages == true ]]; then
            if [[ $line == "))" ]]; then
                break
            else
                collected_packages="$collected_packages$line"$'\n'
            fi
        elif [[ $collecting_packages == setq ]]; then
            collecting_packages=begin
        elif [[ $collecting_packages == begin ]]; then
            collecting_packages=true
        elif [[ $line == "(defvar radon-packages "* ]]; then
            collecting_packages=setq
        fi
    done <../init.el

    echo "[create-init-before-local-el] Here is the default package list:"
    echo -n "$collected_packages"

    exclude_contents=
    echo "[create-init-before-local-el] If you want to exclude a package, enter its name now. Or, just press RET to continue."
    while true; do
        echo -n "[create-init-before-local-el] Exclude package: "
        read package
        if [[ $package ]]; then
            exclude_contents="${exclude_contents}(radon-remove-package '$package)"$'\n'
        else
            break
        fi
    done
    if [[ $exclude_contents ]]; then
        contents="$contents"$'\n'"$exclude_contents"
    fi

    include_contents=
    echo "[create-init-before-local-el] If you want to include an additional package, enter its name now. Or, just press RET to continue."
    while true; do
        echo -n "[create-init-before-local-el] Include package: "
        read package
        if [[ $package ]]; then
            include_contents="${include_contents}(radon-add-package '$package)"$'\n'
        else
            break
        fi
    done
    if [[ $include_contents ]]; then
        contents="$contents"$'\n'"$include_contents"
    fi

fi

echo "[create-init-before-local-el] There are a number of configurable parameters. You can configure them or leave them at the defaults for now."
echo "[create-init-before-local-el] Either way, you can configure them later in init.before.local.el."
echo -n "[create-init-before-local-el] Configure parameters now? (y/n) "
read answer
if (echo "$answer" | egrep -qi "^y"); then
    configure=true
else
    configure=false
fi
collected_comments=
while read -u 10 line; do
    if [[ $line == \;\;\;* ]]; then
        collected_comments="$collected_comments$line"$'\n'
    else
        if (echo "$line" | egrep -q "\\(setq radon-customize-"); then
            variable=$(echo "$line" | egrep -o "radon-[a-z-]+" | head -n 1)
            value=
            if [[ $configure == true ]]; then
                echo -n "$collected_comments"
                echo "$line"
                echo "[create-init-before-local-el] You can enter a new value to override the default shown above. Be sure to quote symbols."
                echo -n "[create-init-before-local-el] Enter new value or leave blank to use default: "
                read value
            fi
            if [[ $value ]]; then
                contents="$contents"$'\n'"${collected_comments}(setq $variable $value)"$'\n'
            else
                contents="$contents"$'\n'"${collected_comments}$line"$'\n'
            fi
        fi
        collected_comments=
    fi
done 10<../init.el

echo -n "$contents" > ../../dotfiles-local/init.before.local.el
echo "[create-init-before-local-el] Wrote the following to dotfiles-local/init.before.local.el:"
cat ../../dotfiles-local/init.before.local.el

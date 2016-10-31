#!/bin/bash

set -e
set -o pipefail

echo "[create-init-before-local-el] Setting up init.before.local.el."

define contents <<'EOF'
;;; This file is run near the beginning of init.el. Here, you can
;;; customize various aspects of Radian.
;;;
;;; There are a number of customizable parameters, which begin with
;;; "radian-customize-". These parameters are set to their default
;;; values using `setq' at the very beginning of init.el, and each
;;; `setq' declaration is accompanied by a comment explaining the
;;; meaning of the parameter.
;;;
;;; If you use the setup script to generate this file, then all of the
;;; `setq' declarations and their explanatory comments are copied into
;;; this file (see below). When a new parameter is added, you have two
;;; options: either copy the declaration from init.el into this file,
;;; or delete this file and run the setup script again. (Obviously,
;;; the second option will cause you to lose anything you previously
;;; had in this file.)
;;;
;;; You can also use this file to inhibit the loading of packages that
;;; would otherwise be included with Radian by default. The following
;;; functions are provided:
;;;
;;; * `radian-disable-package'
;;; * `radian-reenable-package'
;;; * `radian-package-enabled-p'
;;; * `radian-package-disabled-p'
;;;
;;; See their docstrings for more information. As a simple example,
;;; however, here is how you can disable Aggressive Indent:
;;;
;;; (radian-disable-package 'aggressive-indent)
EOF
contents="$contents"$'\n'

echo "[create-init-before-local-el] Radian Emacs includes a number of packages by default."
echo "[create-init-before-local-el] You can configure this list or leave it at the default for now."
echo "[create-init-before-local-el] Either way, you can configure it later in init.before.local.el."
echo -n "[create-init-before-local-el] Configure package list now? (y/n) "
read answer
if (echo "$answer" | egrep -qi "^y"); then
    configure=true
else
    configure=false
fi
is_anything_disabled=false
collected_comments=
while read -u 10 line; do
    if [[ $line == \;\;* ]]; then
        collected_comments="$collected_comments$line"$'\n'
    else
        if (echo "$line" | fgrep -q "(use-package"); then
            package=${line#"(use-package "}
            if [[ $configure == true ]]; then
                echo "Package: $package"
                echo -n "$collected_comments"
                echo -n "[create-init-before-local-el] Include this package? (y/n) "
                read answer
                if ! (echo "$answer" | egrep -qi "^y"); then
                    contents="$contents(radian-disable-package '$package)"$'\n'
                    is_anything_disabled=true
                fi
            fi
        fi
        collected_comments=
    fi
done 10<../init.el

if [[ $is_anything_disabled == false ]]; then
    contents="$contents"$'\n'
fi

echo "[create-init-before-local-el] There are a number of configurable parameters."
echo "[create-init-before-local-el] You can configure them or leave them at the defaults for now."
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
    if [[ $line == \;\;* ]]; then
        collected_comments="$collected_comments$line"$'\n'
    else
        if (echo "$line" | egrep -q "\\(setq radian-customize-[a-z-]+"); then
            variable=$(echo "$line" | egrep -o "radian-customize-[a-z-]+" | head -n 1)
            value=
            if [[ $configure == true ]] && ! (echo "$collected_comments" | fgrep -qi "manual configuration only"); then
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

echo -n "$contents" > ../../radian-local/init.before.local.el
echo "[create-init-before-local-el] Wrote the following to radian-local/init.before.local.el:"
cat ../../radian-local/init.before.local.el

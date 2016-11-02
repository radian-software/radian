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
        define format <<'EOF'
%s
EOF
        printf -v format "$format" "$line"
        collected_comments="$collected_comments$format"
    else
        if (echo "$line" | egrep -q "^\\(use-package"); then
            # Strip the trailing newline from collected_comments.
            collected_comments=$(echo "$collected_comments")
            package=${line#"(use-package "}
            package=${package%")"}
            if [[ $configure == true ]]; then
                echo "Package: $package"
                echo "$collected_comments"
                echo -n "[create-init-before-local-el] Include this package? (y/n) "
                read answer
                if ! (echo "$answer" | egrep -qi "^y"); then
                    define format <<'EOF'
(radian-disable-package '%s)
EOF
                    printf -v format "$format" "$package"
                    contents="$contents$format"
                    is_anything_disabled=true
                fi
            fi
        fi
        collected_comments=
    fi
done 10<../init.el

if [[ $is_anything_disabled == false ]]; then
    define format <<'EOF'

EOF
    contents="$contents$format"
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
        define format <<'EOF'
%s
EOF
        printf -v format "$format" "$line"
        collected_comments="$collected_comments$format"
    elif (echo "$line" | egrep -q "\\(defvar radian-customize-[a-z-]+ nil\\)"); then
        variable=$(echo "$line" | egrep -o "radian-customize-[a-z-]+" | head -n 1)
    else
        if [[ $variable ]]; then
            # Strip the trailing newline from collected_comments.
            collected_comments=$(echo "$collected_comments")
            value=
            if [[ $configure == true ]] && ! (echo "$collected_comments" | fgrep -qi "manual configuration only"); then
                echo "$collected_comments"
                echo "$line"
                echo "[create-init-before-local-el] You can enter a new value to override the default shown above. Be sure to quote symbols."
                echo -n "[create-init-before-local-el] Enter new value or leave blank to use default: "
                read value
            fi
            if [[ $value ]]; then
                define format <<'EOF'

%s
(setq %s %s)
EOF
                printf -v format "$format" "$collected_comments" "$variable" "$value"
            else
                define format <<'EOF'

%s
%s
EOF
                printf -v format "$format" "$collected_comments" "$line"
            fi
            contents="$contents$format"
            variable=
        fi
        collected_comments=
    fi
done 10<../init.el

echo -n "$contents" > ../../radian-local/init.before.local.el
echo "[create-init-before-local-el] Wrote the following to radian-local/init.before.local.el:"
cat ../../radian-local/init.before.local.el

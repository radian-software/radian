#!/bin/bash

set -e
set -o pipefail

define contents <<'EOF'
#!/usr/bin/env zsh
#
# This file is run near the beginning of .zshrc. This is the best
# place to override the various parameters shown below. If a new
# parameter has been added, delete this file and re-run setup.sh to
# get it to show up here. Or, you can just add the 'export' line
# yourself.
#
# Radian uses zplug [1] to manage Zsh plugins. The default bundle list
# is stored in $bundles; you can add and remove items using add_bundle
# and remove_bundle. Each element of the bundle list is split on
# spaces and passed as arguments to zplug. Here are some example uses
# of the above functions:
#
# add_bundle "plugins/lol, from:oh-my-zsh"
# remove_bundle "zsh-users/zsh-autosuggestions"
#
# [1]: https://github.com/zplug/zplug

EOF

echo "[create-zshrc-before-local] Setting up .zshrc.before.local."
echo "[create-zshrc-before-local] You can use the default bundle list, or customize it."
echo "[create-zshrc-before-local] Either way, you can configure it later in .zshrc.before.local."
echo -n "[create-zshrc-before-local] Configure bundles now? (y/n) "
read answer
if (echo "$answer" | egrep -qi "^y"); then

    collecting_bundles=false
    collected_bundles=
    while read line; do
        if [[ $collecting_bundles == true ]]; then
            if [[ $line == ")" ]]; then
                break
            else
                collected_bundles="$collected_bundles$line"$'\n'
            fi
        else
            if [[ $line == "bundles=(" ]]; then
                collecting_bundles=true
            fi
        fi
    done <../.zshrc

    echo "[create-zshrc-before-local] Here is the default bundle list:"
    echo -n "$collected_bundles"

    exclude_contents=
    echo "[create-zshrc-before-local] If you want to exclude a bundle, enter its name (without quotes) now. Or, just press RET to continue."
    while true; do
        echo -n "[create-zshrc-before-local] Exclude bundle: "
        read bundle
        if [[ $bundle ]]; then
            define format <<'EOF'
remove_bundle "%s"
EOF
            printf -v format "$format" "$bundle"
            exclude_contents="$exclude_contents$format"
        else
            break
        fi
    done
    # Strip the trailing newline from exclude_contents.
    exclude_contents=$(echo "$exclude_contents")
    define format <<'EOF'
%s
EOF
    printf -v format "$format" "$exclude_contents"
    contents="$contents$format"
else
    define format <<'EOF'

EOF
    contents="$contents$format"
fi

echo "[create-zshrc-before-local] There are a number of configurable parameters. You can configure them or leave them at the defaults for now."
echo "[create-zshrc-before-local] Either way, you can configure them later in .zshrc.before.local."
echo -n "[create-zshrc-before-local] Configure parameters now? (y/n) "
read answer
if (echo "$answer" | egrep -qi "^y"); then
    configure=true
else
    configure=false
fi

collected_comments=
while read -u 10 line; do
    if [[ $line == \#* ]]; then
        define format <<'EOF'
%s
EOF
        printf -v format "$format" "$line"
        collected_comments="$collected_comments$format"
    else
        if (echo "$line" | egrep -q "\\\$RADIAN_CUSTOMIZE_[A-Z_]+ != (false|true)"); then
            # Strip the trailing newline from collected_comments.
            collected_comments=$(echo "$collected_comments")
            variable=$(echo "$line" | egrep -o "RADIAN_CUSTOMIZE_[A-Z_]+" | head -n 1)
            setting=default
            if [[ $configure == true ]]; then
                echo "$collected_comments"
                echo -n "[create-zshrc-before-local] Do you want this customization ($variable)? (y/n) "
                read answer
                if (echo "$answer" | egrep -qi "^y"); then
                    setting=true
                elif (echo "$answer" | egrep -qi "^n"); then
                    setting=false
                fi
            fi
            define format <<'EOF'

%s
export %s=%s
EOF
            printf -v format "$format" "$collected_comments" "$variable" "$setting"
            contents="$contents$format"
        fi
        collected_comments=
    fi
done 10<../.zshrc

echo -n "$contents" > ../../radian-local/.zshrc.before.local
echo "[create-zshrc-before-local] Wrote the following to radian-local/.zshrc.before.local:"
cat ../../radian-local/.zshrc.before.local

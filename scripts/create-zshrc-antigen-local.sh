#!/bin/bash

set -e
set -o pipefail

echo "[create-zshrc-antigen-local] Setting up .zshrc.antigen.local."

contents=$(cat <<'EOF'
#!/usr/bin/env zsh
# This file is run just after the bundle list is defined in .zshrc, but just
# before bundles are loaded. This means you can customize the bundle list.
# The add_bundle and remove_bundle commands are provided for this purpose.
# You can use them like:
#
# add_bundle lol
# remove_bundle osx
#
# Note that word splitting will be performed on each element of the bundle
# list, so that you can provide more than one argument to the 'antigen bundle'
# command like so:
#
# add_bundle "robbyrussell/oh-my-zsh plugins/ruby"
EOF
        )
contents="$contents"$'\n'

echo "[create-zshrc-antigen-local] You can use the default bundle list, or customize it."
echo "[create-zshrc-antigen-local] Either way, you can configure it later in .zshrc.antigen.local."
echo -n "[create-zshrc-antigen-local] Configure bundles now? (y/n) "
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

    echo "[create-zshrc-antigen-local] Here is the default bundle list:"
    echo -n "$collected_bundles"

    exclude_contents=
    echo "[create-zshrc-antigen-local] If you want to exclude a bundle, enter its name now. Or, just press RET to continue."
    while true; do
        echo -n "[create-zshrc-antigen-local] Exclude bundle: "
        read bundle
        if [[ $bundle ]]; then
            exclude_contents="${exclude_contents}remove_bundle \"$bundle\""$'\n'
        else
            break
        fi
    done
    if [[ $exclude_contents ]]; then
        contents="$contents"$'\n'"$exclude_contents"
    fi

    include_contents=
    echo "[create-zshrc-antigen-local] If you want to include an additional bundle, enter its name now. Or, just press RET to continue."
    while true; do
        echo -n "[create-zshrc-antigen-local] Include bundle: "
        read bundle
        if [[ $bundle ]]; then
            include_contents="${include_contents}add_bundle \"$bundle\""$'\n'
        else
            break
        fi
    done
    if [[ $include_contents ]]; then
        contents="$contents"$'\n'"$include_contents"
    fi

fi

echo -n "$contents" > ../../radian-local/.zshrc.antigen.local
echo "[create-zshrc-antigen-local] Wrote the following to radian-local/.zshrc.antigen.local:"
cat ../../radian-local/.zshrc.antigen.local

#!/bin/bash

# Description:
# Edits plist files. Provides a more usable interface to PlistBuddy.

set -e
set -o pipefail

buddy="/usr/libexec/PlistBuddy"

echo "Called with arguments: $@"

case $2 in
    add ) if [[ $5 ]]; then
              $buddy -c "add ':$3' '$4' '$5'" "$1"
          else
              $buddy -c "add ':$3' '$4'" "$1"
          fi ;;
    set ) $buddy -c "add ':$3' '$4' '$5'" "$1" || $buddy -c "Set ':$3' '$5'" "$1" ;;
    foreach )
        $buddy -c "add ':$3' array" "$1" || true
        i=0
        while $buddy -c "print '$3:$i'" "$1" && ./modify-plist.sh "$1" "$4" "$3:$i:$5" "${@:6}"; do
            ((i++))
        done ;;
    * ) echo "[modify-plist] Fatal error: invalid task '$2' provided, must be one of (set foreach)."
        exit 1 ;;
esac

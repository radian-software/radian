#!/usr/bin/env bash

set -e
set -o pipefail

find=(
    find .
    -name .git -prune -o
    -name "*.elc" -o
    -type f -print
)

readarray -t files < <("${find[@]}" | sort)

code="$(cat <<"EOF"

(length($0) >= 80 && $0 !~ /https?:\/\//) \
{ printf "%s:%d: %s\n", FILENAME, NR, $0 }

EOF
)"

for file in "${files[@]}"; do
    awk "$code" "$file"
done | (! grep .)

#!/usr/bin/env bash

set -e
set -o pipefail

if [[ -n "$1" && ! "$1" =~ [0-9]+\.[0-9]+ ]]; then
    echo "docker.bash: malformed tag: $1" >&2
    exit 1
fi

tag="${1:-latest}"

docker() {
    if [[ "$OSTYPE" != darwin* ]] && [[ "$EUID" != 0 ]]; then
        command sudo docker "$@"
    else
        command docker "$@"
    fi
}

docker build . -t "radian:$tag" --build-arg "VERSION=$tag" $args
docker run -it --rm \
       -v "$PWD:/root/radian" \
       -v "$HOME/.emacs.d/straight/repos:/root/.emacs.d/straight/repos" \
       -w /root/radian \
       "radian:$tag"

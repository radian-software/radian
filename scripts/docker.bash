#!/usr/bin/env bash

set -e
set -o pipefail

tag="${1:-latest}"

args=(bash)
if [[ -n "$2" ]]; then
    args=("${args[@]}" -c "$2")
fi

docker() {
    if [[ "$OSTYPE" != darwin* ]] && [[ "$EUID" != 0 ]]; then
        command sudo docker "$@"
    else
        command docker "$@"
    fi
}

docker build . -t "radian:$tag" \
       --build-arg "UID=$UID" \
       --build-arg "VERSION=$tag"

repos=".emacs.d/straight/repos"

# If we don't do this, then the directory gets created in the host
# filesystem with root ownership :/
mkdir -p "$HOME/${repos}"

docker run -it --rm \
       -v "$PWD:/home/docker/radian" \
       -v "$HOME/${repos}:/home/docker/${repos}" \
       "radian:$tag" "${args[@]}"

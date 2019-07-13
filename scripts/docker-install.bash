#!/usr/bin/env bash

set -e
set -o pipefail

packages="

# needed to run build system
make

# needed for 'make help'
bsdmainutils

# needed for magit and friends
git

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

/tmp/symlink-dotfiles.bash

rm /tmp/docker-install.bash /tmp/symlink-dotfiles.bash

#!/bin/bash

set -e
set -o pipefail

echo "[install-homebrew] Using the install command from http://brew.sh to install Homebrew."
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

#!/usr/bin/env bash

set -e
set -o pipefail

emacs --batch -l "$HOME/.emacs.d/init.el"

#!/bin/bash

set -e
set -o pipefail

echo "[ensure-xcode-cl-tools-installed] Checking if the Xcode Command Line Tools are installed."

while xcode-select --install; do
    echo "[ensure-xcode-cl-tools-installed] The Xcode Command Line Tools do not appear to be installed."
    echo "[ensure-xcode-cl-tools-installed] Please use the dialog window to install the Xcode Command Line Tools."
    sleep 5
    read -p "[ensure-xcode-cl-tools-installed] Please press RET when you have finished installing the Xcode Command Line Tools."
done

echo "[ensure-xcode-cl-tools-installed] The Xcode Command Line Tools appear to be installed."

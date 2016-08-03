#!/bin/bash

set -e

echo '[setup] Checking if the Xcode Command Line Tools are installed.'

while xcode-select --install; do
    echo '[setup] The Xcode Command Line Tools do not appear to be installed.'
    echo '[setup] Please use the dialog window to install the Xcode Command Line Tools.'
    sleep 3
    read -p '[setup] Please press RET when you have finished installing the Xcode Command Line Tools.'
done

echo '[setup] The Xcode Command Line Tools appear to be installed.'

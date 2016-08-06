#!/bin/bash

echo '[setup] Checking to see if Python is installed via Homebrew.'
if [[ $(brew list --versions python) ]]; then
    echo '[setup] It appears that Python is already installed via Homebrew.'
    echo '[setup] There is no way to tell for sure, though.'
    echo '[setup] If you want to be confident that things are set up correctly, use `brew uninstall python` and run this script again.'
else
    echo '[setup] It appears that Python has not been installed via Homebrew.'
    echo '[setup] Installing Python using Homebrew.'
    brew install python
fi

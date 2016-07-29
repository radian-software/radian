#!/bin/bash

echo '[setup] Checking if Homebrew is installed.'

if hash brew 2>/dev/null; then
    echo '[setup] Homebrew appears to already be installed.'
else
    echo '[setup] Homebrew does not appear to be installed.'
    echo '[setup] Installing Homebrew.'
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    echo '[setup] Checking that everything is OK with Homebrew.'
    brew doctor
fi

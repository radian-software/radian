#!/bin/bash

set -e

echo '[setup] Installing Leiningen dependencies, if necessary.'
lein with-profile +all --version

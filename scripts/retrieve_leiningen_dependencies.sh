#!/bin/bash

echo '[setup] Retrieving Leiningen dependencies, if necessary.'
lein with-profile +all --version

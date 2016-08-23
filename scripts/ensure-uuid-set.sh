#!/bin/bash

if [[ -z $uuid ]]; then
    script_name="${0##*/}"
    echo "[${script_name%.sh}] Fatal error: \$uuid not set."
    exit 1
fi

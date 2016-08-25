#!/bin/bash

set -e
set -o pipefail

cd ..
repo_name="$(basename "$PWD")"
echo "[ensure-running-in-repo] Ensuring that '$repo_name' is a Git repository."
if git rev-parse --is-inside-work-tree; then
    echo "[ensure-running-in-repo] It looks like '$repo_name' is already a Git repository."
else
    echo "[ensure-running-in-repo] It looks like '$repo_name' is not a Git repository."
    echo -n "[ensure-running-in-repo] Would you like to make it one? (y/n) "
    read answer
    if (echo "$answer" | egrep -qi "^y"); then
        echo "[ensure-running-in-repo] Initializing Git repository."
        git init
        echo "[ensure-running-in-repo] You can synchronize your local repository with the official raxod502/dotfiles repository, or specify your own repository."
        echo -n "[ensure-running-in-repo] Specify your own repository instead of using the default? (y/n) "
        read answer
        if (echo "$answer" | egrep -qi "^y"); then
            echo -n "[ensure-running-in-repo] Repository clone URL: "
            read repo
        else
            repo=https://github.com/raxod502/dotfiles.git
        fi
        echo "[ensure-running-in-repo] Adding $repo as a remote."
        git remote add origin "$repo"
        echo "[ensure-running-in-repo] Fetching history."
        git fetch
        echo "[ensure-running-in-repo] Which branch or revision would you like to check out?"
        echo "[ensure-running-in-repo] Warning: this will overwrite files in the repository to match the branch or revision that you name."
        echo "[ensure-running-in-repo] However, files that are not present in the target branch or revision will be left alone."
        echo "[ensure-running-in-repo] To abort, press Control+C. Note however that this will leave Git in an odd state that you will have to fix by hand."
        finished=false
        while [[ $finished != true ]]; do
            branch=
            while [[ -z $branch ]]; do
                echo -n "[ensure-running-in-repo] Branch or revision: "
                read branch
            done
            echo "[ensure-running-in-repo] Checking out '$branch'."
            if git checkout --force "$branch"; then
                finished=true
            fi
        done
    else
        echo "[ensure-running-in-repo] OK."
    fi
fi

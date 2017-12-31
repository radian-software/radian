#!/usr/bin/env zsh

set -e
set -o pipefail

script_name=$0

function die {
    echo $script_name: $@ 1>&2
    exit 1
}

function usage {
    cat 1>&2 <<EOF
usage: $script_name <app-name-or-path> <internal-binary-name> <backup-binary-name> (--patch | --unpatch)

This script patches a macOS application bundle (.app) so that
~/.profile is executed when the application launches. This allows you
to set up things like ssh-agent, gpg-agent, and the \$PATH correctly
for graphical applications even when they are not launched from the
terminal.

The first argument is the path to a .app bundle, or just an
application name, in which case the app is assumed to be in
/Applications.

The second argument is the name of the main binary in Contents/MacOS
within the bundle. This script renames that binary to another name,
given by the third argument, and then puts a shell script in its place
that sources ~/.profile and then runs the original binary.

If --unpatch is given, then the binary is moved back to its original
location and the wrapper script is deleted.

If you move the application, you must unpatch and repatch it using
this script. Furthermore, note that the patch will not work if the
binary being invoked has any single quotes in it.
EOF
    if (( $# == 1 )) && [[ $1 == *help ]]; then
        exit 0
    else
        exit 1
    fi
}

if (( $# != 4 )); then
    usage
fi

if [[ $1 == *.app ]]; then
    app=$1
else
    app="/Applications/$1.app"
fi

binary=$2

if [[ $binary == */* ]]; then
    die "cannot contain slashes: $binary"
fi

backup=$3

if [[ $backup == */* ]]; then
    die "cannot contain slashes: $backup"
fi

if [[ $4 == --patch ]]; then
    patch=yes
elif [[ $4 == --unpatch ]]; then
    patch=
else
    usage
fi

if [[ ! -d $app ]]; then
    die "no such directory: $app"
fi

cd $app/Contents/MacOS

if [[ -n $patch ]]; then
    if [[ ! -x $binary ]]; then
        die "no such executable: $PWD/$binary"
    fi

    if [[ -e $backup || -L $backup ]]; then
        die "already exists: $PWD/$backup"
    fi
else
    if [[ ! -x $backup ]]; then
        die "no such executable: $PWD/$backup"
    fi
fi

if [[ -n $patch ]]; then
    mv $binary $backup
    cat >$binary <<EOF
#!/bin/sh

if [ -f ~/.profile ]; then
    . ~/.profile
fi

'$PWD/$backup' "\$@"
EOF
    chmod +x $binary
    echo "Patched successfully: $app"
else
    mv $backup $binary
    echo "Unpatched successfully: $app"
fi

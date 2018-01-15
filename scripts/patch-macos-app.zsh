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
usage: $script_name <app-name-or-path>
       [--binary <name>] [--alt-binary <name>]
       [--prefix-args <arg>... --]
       [--suffix-args <arg>... --]
       [--patch | --unpatch | --repatch]

This script patches a macOS application bundle (.app) so that
~/.profile is executed when the application launches. This allows you
to set up things like ssh-agent, gpg-agent, and the \$PATH correctly
for graphical applications even when they are not launched from the
terminal.

The first argument is the path to a .app bundle, or just an
application name, in which case the app is assumed to be in
/Applications.

--binary is the name of the main binary in Contents/MacOS within the
bundle. This defaults to the name of the application without the
extension. This script renames that binary to another name, given by
--alt-binary (which defaults to the value of --binary plus the string
'Core'), and then puts a shell script in its place that sources
~/.profile and then runs the original binary.

You can provide --prefix-args and --suffix-args to inject some
additional arguments into the argument list of the main binary, at the
beginning and end respectively. Make sure to include '--' after the
arguments to inject, since otherwise there is no way to tell where the
list ends.

If --unpatch is given (rather than --patch, the default), then the
binary is moved back to its original location and the wrapper script
is deleted. If --repatch is given it is equivalent to performing first
--unpatch and then --patch.

If you move the application, you must unpatch and repatch it using
this script.
EOF
    if (( $# == 1 )) && [[ $1 == *help ]]; then
        exit 0
    else
        exit 1
    fi
}

if (( $# < 1 )); then
    usage
fi

if [[ $1 == *.app ]]; then
    app=$1
else
    app="/Applications/$1.app"
fi

shift

binary=
alt_binary=
typeset -a prefix_args=()
typeset -a suffix_args=()
patch=yes

last=

for arg; do
    case $last in
        "")
            case $arg in
                --prefix-args)
                    prefix_args=()
                    last=$arg
                    ;;
                --suffix-args)
                    suffix_args=()
                    last=$arg
                    ;;
                --patch)
                    patch=yes
                    ;;
                --unpatch)
                    patch=
                    ;;
                --repatch)
                    patch=repatch
                    ;;
                *)
                    last=$arg
                    ;;
            esac
            ;;
        --binary)
            binary=$arg
            last=
            ;;
        --alt-binary)
            alt_binary=$arg
            last=
            ;;
        --prefix-args)
            if [[ $arg == -- ]]; then
                last=
            else
                prefix_args+=($arg)
            fi
            ;;
        --suffix-args)
            if [[ $arg == -- ]]; then
                last=
            else
                suffix_args+=($arg)
            fi
            ;;
        *)
            die "unknown argument: $last"
            ;;
    esac
done

if [[ -n $last ]]; then
    die "needs more arguments: $last"
fi

binary=${binary:-${${app##*/}%.app}}
alt_binary=${alt_binary:-${binary}Core}

if [[ $binary == */* ]]; then
    die "cannot contain slashes: $binary"
fi

if [[ $alt_binary == */* ]]; then
    die "cannot contain slashes: $alt_binary"
fi

if [[ ! -d $app ]]; then
    die "no such directory: $app"
fi

cd $app/Contents/MacOS

function main {
    if [[ -n $patch ]]; then
        if [[ ! -x $binary ]]; then
            die "no such executable: $PWD/$binary"
        fi

        if [[ -e $alt_binary || -L $alt_binary ]]; then
            die "already exists: $PWD/$alt_binary"
        fi
    else
        if [[ ! -x $alt_binary ]]; then
            die "no such executable: $PWD/$alt_binary"
        fi
    fi

    if [[ -n $patch ]]; then
        mv $binary $alt_binary
        local abs_path="$PWD/$alt_binary"
        if (( ${#prefix_args[@]} > 0 )); then
            local prefix=" ${(@q)prefix_args}"
        else
            local prefix=
        fi
        if (( ${#suffix_args[@]} > 0 )); then
            local suffix=" ${(@q)suffix_args}"
        else
            local suffix=
        fi
        cat >$binary <<EOF
#!/bin/sh

if [ -f ~/.profile ]; then
    . ~/.profile
fi

${(q)abs_path}${prefix} "\$@"${suffix}
EOF
        chmod +x $binary
        echo "Patched successfully: $app"
    else
        mv $alt_binary $binary
        echo "Unpatched successfully: $app"
    fi
}

if [[ $patch == repatch ]]; then
    patch=
    main
    patch=yes
    main
else
    main
fi

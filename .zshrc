################################################################################
#### Define default bundle list

bundles=(
    "plugins/fasd, from:oh-my-zsh" # Quickly jump to directories
    "plugins/git, from:oh-my-zsh" # Aliases for git
    "plugins/lein, from:oh-my-zsh" # Completion for lein
    "plugins/osx, from:oh-my-zsh" # Interop with OSX and iTunes
    "plugins/sudo, from:oh-my-zsh" # Quickly re-run commands with sudo
    "plugins/tmuxinator, from:oh-my-zsh" # Completion for tmuxinator
    "plugins/wd, from:oh-my-zsh" # Quickly jump to directories
    "plugins/web-search, from:oh-my-zsh" # Search the web
    "zsh-users/zsh-autosuggestions" # Autosuggestions from history
    "'~/.radian/zsh/*.zsh', from:local" # Radian customizations
    "'~/.radian-local/zsh/*.zsh', from:local" # Local customizations
)

################################################################################
#### Define bundle list management functions

# Usage: add_bundle <zplug-args>
#
# Adds a bundle to $bundles. Word splitting will be performed on
# zplug-args to determine the arguments that will be passed to zplug.
add_bundle() {
    if ! (( ${bundles[(I)$1]} )); then
        bundles+=($1)
    fi
}

# Usage: remove_bundle <zplug-args>
#
# Removes a bundle from $bundles by name. The name should be exactly
# the same as it appears in $bundles, with spaces if necessary.
remove_bundle() {
    bundles=("${(@)bundles:#$1}")
}

################################################################################
#### Load user-specific configuration file (1 of 2)

if [[ -f ~/.zshrc.before.local ]]; then
    source ~/.zshrc.before.local
fi

################################################################################
#### zplug

export ZPLUG_HOME=/usr/local/opt/zplug

if [[ -f $ZPLUG_HOME/init.zsh ]]; then
    source $ZPLUG_HOME/init.zsh

    for bundle in $bundles; do
        zplug $=bundle
    done

    if ! zplug check; then
        zplug install
    fi

    zplug load
fi

################################################################################
#### Load user-specific configuration file (2 of 2)

if [[ -f ~/.zshrc.local ]]; then
    source ~/.zshrc.local
fi

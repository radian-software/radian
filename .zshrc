# Local overrides (1 of 3)
if [[ -f ~/.zshrc.before.local ]]; then
    source ~/.zshrc.before.local
fi

### Antigen ###

# Load Antigen.
source ~/.antigen-repo/antigen.zsh

# Just to be safe: The core oh-my-zsh library is necessary for a
# number of oh-my-zsh plugins and themes to work correctly.
antigen use oh-my-zsh

# Define default bundle list. Please note that the formatting of this
# definition *must* be preserved, as it is read programmatically by
# create-zshrc-antigen-local.sh.
bundles=(
    autojump # Use 'j' to jump to frequent directories with fuzzy-matching
    git # Aliases for git
    lein # Completion for lein
    osx # Interop with OSX and iTunes
    sudo # Use ESC ESC to prefix previous or current command with 'sudo'
    tmuxinator # Completion for tmuxinator
    wd # Mark directories with 'wd add' and jump to them with 'wd'
    web-search # Search Google, DuckDuckGo, Maps, etc.
    zsh-users/zsh-autosuggestions # Suggest completions based on past commands
)

# Usage: add_bundle <antigen-bundle-args>
# Adds a bundle to $bundles. Word splitting will be performed on
# antigen-bundle-args to determine the arguments that will be passed to
# 'antigen bundle'.
add_bundle() {
    if ! (( ${bundles[(I)$1]} )); then
        bundles+=($1)
    fi
}

# Usage: remove_bundle <antigen-bundle-args>
# Removes a bundle from $bundles by name. The name should be exactly the
# same as it appears in $bundles, with spaces if necessary.
remove_bundle() {
    bundles=("${(@)bundles:#$1}")
}

# Local overrides (2 of 3)
if [[ -f ~/.zshrc.antigen.local ]]; then
    source ~/.zshrc.antigen.local
fi

# Load bundles.
for bundle in $bundles; do
    antigen bundle $=bundle
done

# Tell Antigen to apply the changes in which bundles are loaded.
antigen apply

### Zsh ###

# No (practical) limit to many commands are kept in history.
HISTSIZE=1000000 # session history
SAVEHIST=1000000 # saved history

# Better help command (like man, but finds more and better results).
unalias run-help &>/dev/null
autoload run-help

# Colored man pages. Based on:
# https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/colored-man-pages/colored-man-pages.plugin.zsh
# However, this version eliminates the horrid grey-on-blue highlighting
# for search matches.
if [[ $radian_colored_man_pages != false ]]; then
    man() {
        env \
	    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
	    LESS_TERMCAP_md=$(printf "\e[1;31m") \
	    LESS_TERMCAP_me=$(printf "\e[0m") \
	    LESS_TERMCAP_ue=$(printf "\e[0m") \
	    LESS_TERMCAP_us=$(printf "\e[1;32m") \
	    man "$@"
    }
fi

# Better prompt (like oh-my-zsh/mgutz, but turns red on nonzero exit code).
if [[ $radian_customize_prompt != false ]]; then
    PROMPT='%(?.%{$fg[blue]%}.%{$fg[red]%})%c%{$reset_color%}$(git_prompt_info)%(?.%{$fg[blue]%}.%{$fg[red]%}) %# %{$reset_color%}'
    ZSH_THEME_GIT_PROMPT_PREFIX="["
    ZSH_THEME_GIT_PROMPT_SUFFIX=
    ZSH_THEME_GIT_PROMPT_DIRTY="*]"
    ZSH_THEME_GIT_PROMPT_CLEAN="]"
fi

# Use 'resource' to reload .zshrc.
if [[ $radian_resource_alias != false ]]; then
    alias resource="source ~/.zshrc"
fi

# Use 'source' or '.' with no arguments to reload .zshrc.
if [[ $radian_source_and_dot_aliases != false ]]; then
    # Normally, "source" and "." require at least one argument, which
    # is the file to be sourced. However, if you provide additional
    # arguments, then they are passed to the file you are sourcing, as
    # command-line arguments in $@.
    #
    # There appears to be an interesting edge case, however, that will
    # cause the following functions to misbehave if they are not
    # implemented carefullly. If you provide only one argument to
    # "source" (or "."), i.e. you specify a file to be sourced but you
    # do not want any arguments to be passed when that file is
    # sourced, and you are calling "source" from within a function
    # (like the ones below), then the arguments to the function will
    # transparently be passed through to the file being sourced!
    #
    # This edge case shows up when the following functions are called
    # with exactly one argument (the file to be sourced), and it
    # causes that filename to be passed as a command-line argument to
    # the file being sourced. To avoid this behavior (which tends to
    # cause obscure errors that are hard to track down), we have to
    # remove the filename from the list of arguments passed to the
    # function, and pass it separately to "source" or ".". That way,
    # the filename is no longer in $@, so it's no longer pass through
    # to the file being sourced.
    #
    # Note, by the way, that prefixing a command with a backslash
    # prevents it from being interpreted as an alias. This prevents
    # infinite recursion!
    source_zshrc_or_args() {
        if [[ $# == 0 ]]; then
            \source ~/.zshrc
        else
            radian_filename=$1
            shift
            \source $radian_filename $@
        fi
    }
    dot_zshrc_or_args() {
        if [[ $# == 0 ]]; then
            \. ~/.zshrc
        else
            radian_filename=$1
            shift
            \. $radian_filename $@
        fi
    }
    alias source=source_zshrc_or_args
    alias .=dot_zshrc_or_args
fi

# Alias for tmuxinator.
if [[ $radian_mux_alias != false ]]; then
    alias mux=tmuxinator
fi

# Aliases for copying, pasting, moving, and linking files in multiple
# steps.
if [[ $radian_copy_paste_aliases != false ]]; then
    copy() {
        RADIAN_COPY_TARGETS=()
        for target; do
            if [[ $target == /* ]]; then
                RADIAN_COPY_TARGETS+=($target)
            else
                RADIAN_COPY_TARGETS+=($PWD/$target)
            fi
        done
    }
    paste() {
        cp -R $RADIAN_COPY_TARGETS ${1:-.}
    }
    move() {
        mv $RADIAN_COPY_TARGETS ${1:-.}
    }
    pasteln() {
        ln -s $RADIAN_COPY_TARGETS ${1:-.}
    }
fi

# Alias for replacing a symlink with a copy of the file it points to.
if [[ $radian_delink_alias != false ]]; then
    delink() {
        if [[ -z $1 ]]; then
            echo "usage: delink <symlinks>"
            return 1
        fi
        for link; do
            if [[ -L $link ]]; then
                if [[ -e $link ]]; then
                    target=$(grealpath $link)
                    if rm $link; then
                        if cp -R $target $link; then
                            echo "Copied $target to $link"
                        else
                            ln -s $target $link
                        fi
                    fi
                else
                    echo "Broken symlink: $link"
                fi
            else
                echo "Not a symlink: $link"
            fi
        done
    }
fi

# Alias for setting up a tmux session suitable for standard development.
# Takes a project name and an optional command. If a tmux session with
# the project name already exists, switches to it. Otherwise, you need
# to have wd installed. If a warp point with the project name already
# exists, jumps to it. Otherwise, uses autojump to make a guess at the
# correct directory (and creates a warp point, with your permission).
# After getting to the correct directory, sets up a tmux session with
# windows: emacs, git, zsh, zsh. Runs 'emacs' in the first window and
# 'git checkup' in the second. If you provide a second argument, runs
# it as a shell command (provide multiple commands with '&&' or ';')
# in all four windows before anything else.
if [[ $radian_proj_alias != false ]]; then
    proj() {
        if echo "$1" | egrep -q "^\s*$"; then
            echo "Please provide a project name."
            return 1
        fi
        # Check if the session already exists.
        if tmux list-sessions -F "#{session_name}" 2>/dev/null | egrep -q "^$1$"; then
            if [[ $TMUX ]]; then
                tmux switch-client -t "$1"
            else
                tmux attach-session -t "$1"
            fi
        else
            if type wd &>/dev/null; then
                # Check if the warp point exists.
                if wd list | egrep -q "^\s*$1\s"; then
                    (
                        # Start the tmux server, if necessary.
                        tmux start-server

                        # Change to the specified directory.
                        wd "$1"

                        # Create the session.
                        TMUX= tmux new-session -d -s "$1" -n emacs

                        # Create the remaining windows.
                        tmux new-window -t "$1:2" -n git
                        tmux new-window -t "$1:3" -n zsh
                        tmux new-window -t "$1:4" -n zsh

                        # Select the 'git' window initially.
                        tmux select-window -t "$1:2"

                        # Run the $2 command in all windows, if necessary.
                        if [[ $2 ]]; then
                            for i in {1..4}; do
                                tmux send-keys -t "$1:$i" "$2" Enter
                            done
                        fi

                        # Run window-specific commands.
                        tmux send-keys -t "$1:1" emacs Enter
                        tmux send-keys -t "$1:2" "git checkup" Enter

                        # Attach to the session.
                        if [[ $TMUX ]]; then
                            tmux switch-client -t "$1"
                        else
                            tmux attach-session -t "$1"
                        fi
                    )
                else
                    echo "Warp point '$1' not found."
                    if which autojump &>/dev/null && type j &>/dev/null; then
                        guess="$(cd / && autojump $1)"
                        if [[ $guess != . ]]; then
                            echo "$guess"
                            echo -n "Is this the correct directory? (y/n) "
                            read answer
                            if echo "$answer" | egrep -qi "^y"; then
                                echo -n "Please enter the project name: "
                                read project
                                (cd "$guess" && wd add "$project")
                                proj "$project" "$2"
                                return 0
                            fi
                        else
                            echo "Can't find any directory by that name."
                        fi
                        echo "You'll have to navigate to the directory manually before running proj."
                        return 1
                    else
                        echo "You need autojump installed for this to work."
                        return 1
                    fi
                fi
            else
                echo "You need wd installed for this to work."
                return 1
            fi
        fi
    }
fi

# Local overrides (3 of 3)
if [[ -f ~/.zshrc.local ]]; then
    source ~/.zshrc.local
fi

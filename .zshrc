### Define default bundle list ###

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
)

### Define bundle list management functions ###

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

### Load user-specific configuration file (1 of 2) ###

if [[ -f ~/.zshrc.before.local ]]; then
    source ~/.zshrc.before.local
fi

### Provide backwards compatibility for old parameter names ###

if [[ $radian_colored_man_pages ]]; then
    export RADIAN_CUSTOMIZE_COLORED_MAN_PAGES=$radian_colored_man_pages
fi

if [[ $radian_customize_prompt ]]; then
    export RADIAN_CUSTOMIZE_PROMPT=$radian_customize_prompt
fi

if [[ $radian_resource_alias ]]; then
    export RADIAN_CUSTOMIZE_RESOURCE_ALIAS=$radian_resource_alias
fi

if [[ $radian_source_and_dot_aliases ]]; then
    export RADIAN_CUSTOMIZE_SOURCE_AND_DOT_ALIASES=$radian_source_and_dot_aliases
fi

if [[ $radian_mux_alias ]]; then
    export RADIAN_CUSTOMIZE_MUX_ALIAS=$radian_mux_alias
fi

if [[ $radian_copy_paste_aliases ]]; then
    export RADIAN_CUSTOMIZE_COPY_PASTE_ALIASES=$radian_copy_paste_aliases
fi

if [[ $radian_delink_alias ]]; then
    export RADIAN_CUSTOMIZE_DELINK_ALIAS=$radian_delink_alias
fi

if [[ $radian_proj_alias ]]; then
    export RADIAN_CUSTOMIZE_PROJ_ALIAS=$radian_proj_alias
fi

if [[ $RADIAN_CUSTOMIZE_SOURCE_AND_DOT_ALIASES ]]; then
    export RADIAN_CUSTOMIZE_MAGIC_DOT=$RADIAN_CUSTOMIZE_SOURCE_AND_DOT_ALIASES
fi

### zplug ###

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

### Load Zsh features ###

# Better help command (like man, but finds more and better results).
unalias run-help &>/dev/null
autoload run-help

### Prompt ###

# Better prompt (like oh-my-zsh/mgutz, but turns red on nonzero exit code).
if [[ $RADIAN_CUSTOMIZE_PROMPT != false ]]; then
    # The following three functions are adapted from Oh My Zsh [1].
    #
    # [1]: https://github.com/robbyrussell/oh-my-zsh/blob/3705d47bb3f3229234cba992320eadc97a221caf/lib/git.zsh

    # Compares the provided version of git to the version installed and on path
    # Outputs -1, 0, or 1 if the installed version is less than, equal to, or
    # greater than the input version, respectively.
    function git_compare_version() {
        local INPUT_GIT_VERSION INSTALLED_GIT_VERSION
        INPUT_GIT_VERSION=(${(s/./)1})
        INSTALLED_GIT_VERSION=($(command git --version 2>/dev/null))
        INSTALLED_GIT_VERSION=(${(s/./)INSTALLED_GIT_VERSION[3]})

        for i in {1..3}; do
            if [[ $INSTALLED_GIT_VERSION[$i] -gt $INPUT_GIT_VERSION[$i] ]]; then
                echo 1
                return 0
            fi
            if [[ $INSTALLED_GIT_VERSION[$i] -lt $INPUT_GIT_VERSION[$i] ]]; then
                echo -1
                return 0
            fi
        done
        echo 0
    }
    POST_1_7_2_GIT=$(git_compare_version "1.7.2")
    unfunction git_compare_version

    # Prints the branch or revision of the current HEAD, surrounded by
    # square brackets and followed by an asterisk if the working
    # directory is dirty, if the user is inside a Git repository.
    function radian_prompt_git_info() {
        local ref
        ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
            ref=$(command git rev-parse --short HEAD 2> /dev/null) || \
            return 0
        echo "[${ref#refs/heads/}$(radian_prompt_git_dirty)]"
    }

    # Prints an asterisk if the working directory is dirty. Untracked
    # files are not counted as dirty if
    # $RADIAN_PROMPT_UNTRACKED_FILES_DIRTY is equal to "false".
    function radian_prompt_git_dirty() {
        local FLAGS
        FLAGS=('--porcelain')
        if [[ $POST_1_7_2_GIT -gt 0 ]]; then
            FLAGS+='--ignore-submodules=dirty'
        fi
        if [[ "$RADIAN_PROMPT_UNTRACKED_FILES_DIRTY" != "false" ]]; then
            FLAGS+='--untracked-files=no'
        fi
        if [[ $(command git status ${FLAGS} 2> /dev/null | tail -n1) ]]; then
            echo "*"
        fi
    }

    # Enable parameter expansion and other substitutions in the $PROMPT.
    setopt promptsubst

    # The actual prompt.
    PROMPT='%(?.%{$fg[blue]%}.%{$fg[red]%})%c%{$reset_color%}$(radian_prompt_git_info)%(?.%{$fg[blue]%}.%{$fg[red]%}) %# %{$reset_color%}'
fi

### Command history ###

# Make the length limit for session history ludicrously large.
export HISTSIZE=1000000

# Save history to disk. The value of this option is the default
# installed by zsh-newuser-install.
export HISTFILE=~/.zsh_history

# Also make the length limit for the history file on disk ludicrously
# large.
export SAVEHIST=1000000

# Don't save commands to the history if they start with a leading
# space.
setopt histignorespace

### Appearance ###

# Colored man pages. Based on:
# https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/colored-man-pages/colored-man-pages.plugin.zsh
# However, this version eliminates the horrid grey-on-blue highlighting
# for search matches.
if [[ $RADIAN_CUSTOMIZE_COLORED_MAN_PAGES != false ]]; then
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

### Completions ###

# Fuzzy tab completions.
if [[ $RADIAN_CUSTOMIZE_FUZZY_COMPLETIONS != false ]]; then
    zstyle ':completion:*' matcher-list 'r:|?=** m:{a-z\-}={A-Z\_}'
fi

### Command line behavior ###

# When no arguments are provided to "." or "source", they default to
# sourcing .zshrc. Based on [1], thanks @PythonNut!
#
# [1]: http://unix.stackexchange.com/a/326948/176805
if [[ $RADIAN_CUSTOMIZE_MAGIC_DOT != false ]]; then
    function _accept-line() {
        if [[ $BUFFER == "." ]]; then
            BUFFER=". ~/.zshrc"
        elif [[ $BUFFER == "source" ]]; then
            BUFFER="source ~/.zshrc"
        fi
        zle .accept-line
    }
    zle -N accept-line _accept-line
fi

### Aliases: zsh ###

# Use "resource" to reload .zshrc.
if [[ $RADIAN_CUSTOMIZE_RESOURCE_ALIAS != false ]]; then
    alias resource="source ~/.zshrc"
fi

### Aliases: filesystem navigation ###

# Add some useful parameters to "ls" by default.
if [[ $RADIAN_CUSTOMIZE_LS_ALIAS != false ]]; then
    alias ls='ls -lah'
fi

# Use "..", "...", "....", etc. to move to parent directories.
if [[ $RADIAN_CUSTOMIZE_DOT_DOT_ALIASES != false ]]; then
    alias ..='cd ..'
    alias ...='cd ...'
    alias ....='cd ....'
    alias .....='cd .....'
    alias ......='cd ......'
    alias .......='cd .......'
    alias ........='cd ........'
    alias .........='cd .........'
    alias ..........='cd ..........'
fi

### Aliases: moving files ###

# Aliases for copying, pasting, moving, and linking files in multiple
# steps.
if [[ $RADIAN_CUSTOMIZE_COPY_PASTE_ALIASES != false ]]; then
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
if [[ $RADIAN_CUSTOMIZE_DELINK_ALIAS != false ]]; then
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

### Aliases: version control ###

# Support for "hub" commands, see [1].
#
# [1]: https://github.com/github/hub
if [[ $RADIAN_CUSTOMIZE_HUB_ALIAS != false ]]; then
    eval "$(hub alias -s)" 2>/dev/null || true
fi

### Aliases: tmux ###

# Alias for tmuxinator.
if [[ $RADIAN_CUSTOMIZE_MUX_ALIAS != false ]]; then
    alias mux=tmuxinator
fi

# Alias for setting up a tmux session suitable for standard development.
# Takes a project name and an optional command. If a tmux session with
# the project name already exists, switches to it. Otherwise, you need
# to have wd installed. If a warp point with the project name already
# exists, jumps to it. Otherwise, uses fasd to make a guess at the
# correct directory (and creates a warp point, with your permission).
# After getting to the correct directory, sets up a tmux session with
# windows: emacs, git, zsh, zsh. Runs 'emacs' in the first window and
# 'git checkup' in the second. If you provide a second argument, runs
# it as a shell command (provide multiple commands with '&&' or ';')
# in all four windows before anything else.
if [[ $RADIAN_CUSTOMIZE_PROJ_ALIAS != false ]]; then
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
                    if which fasd &>/dev/null && type z &>/dev/null; then
                        guess="$(z $1 && echo $PWD)"
                        if [[ $guess ]]; then
                            echo "$guess"
                            echo -n "Is this the correct directory? (Y/n) "
                            read answer
                            if echo "$answer" | egrep -qiv "^n"; then
                                echo -n "Please enter the project name or leave blank to use $1: "
                                read project
                                project=${project:-$1}
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
                        echo "You need fasd installed for this to work."
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

### Load user-specific configuration file (2 of 2) ###

if [[ -f ~/.zshrc.local ]]; then
    source ~/.zshrc.local
fi

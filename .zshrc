### Antigen ###

source ~/.antigen-repo/antigen.zsh

# Just to be safe: The core oh-my-zsh library is necessary for a
# number of oh-my-zsh plugins and themes to work correctly.
antigen use oh-my-zsh

# Efficient filesystem navigation (passive), jump with 'j'.
antigen bundle autojump

# Completion for 'brew' command.
antigen bundle brew

# Aliases for common git commands.
antigen bundle git

# Completion for 'lein' command.
antigen bundle lein

# Aliases for interop with OSX and iTunes.
antigen bundle osx

# Press ESC ESC to prefix current or previous command with 'sudo'.
antigen bundle sudo

# Aliases for common tmux commands, and support for starting tmux on login.
antigen bundle tmux

# Completion for 'tmuxinator' command.
antigen bundle tmuxinator

# Efficient filesystem navigation (active), mark with 'wd add'
# and jump with 'wd'.
antigen bundle wd

# Search the web from the command line!
antigen bundle web-search

# Display completion suggestions based on past commands.
antigen bundle zsh-users/zsh-autosuggestions

# Actually load selected bundles.
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
man() {
    env \
	LESS_TERMCAP_mb=$(printf "\e[1;31m") \
	LESS_TERMCAP_md=$(printf "\e[1;31m") \
	LESS_TERMCAP_me=$(printf "\e[0m") \
	LESS_TERMCAP_ue=$(printf "\e[0m") \
	LESS_TERMCAP_us=$(printf "\e[1;32m") \
	man "$@"
}

# Better prompt (like oh-my-zsh/mgutz, but turns red on nonzero exit code).
PROMPT='%(?.%{$fg[blue]%}.%{$fg[red]%})%c%{$reset_color%}$(git_prompt_info)%(?.%{$fg[blue]%}.%{$fg[red]%}) %# %{$reset_color%}'
ZSH_THEME_GIT_PROMPT_PREFIX="["
ZSH_THEME_GIT_PROMPT_SUFFIX=
ZSH_THEME_GIT_PROMPT_DIRTY="*]"
ZSH_THEME_GIT_PROMPT_CLEAN="]"

# Alias for reloading .zshrc.
alias resource="source ~/.zshrc"

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
proj() {
    # Check if the session already exists.
    if echo "$1" | egrep -q "^\s*$"; then
        echo "Please provide a project name."
        return 1
    fi
    if tmux list-sessions -F "#{session_name}" | egrep -q "^$1$"; then
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

                    # Select the 'git' window initially. This can't be
                    # done with directly because -- for whatever
                    # reason -- 'tmux select-window' will act on the
                    # *current* tmux session, rather than the one we
                    # are switching to. Yes, even if we do it at the
                    # very end. Instead, we create a throwaway window
                    # for the command, which will be closed automatically
                    # once the command completes.
                    tmux new-window -t "$1:5" -n select-initial-window "tmux select-window -t 2"

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
                if type j &>/dev/null; then
                    guess="$(autojump $1)"
                    echo "$guess"
                    echo -n "Is this the correct directory? (y/n) "
                    read answer
                    if echo "$answer" | egrep -qi "^y"; then
                        echo -n "Please enter the project name: "
                        read project
                        (cd "$guess" && wd add "$project")
                        proj "$project" "$2"
                    else
                        echo "You'll have to navigate to the directory manually."
                        return 1
                    fi
                else
                    echo "j command not found."
                    return 1
                fi
            fi
        else
            echo "wd command not found."
            return 1
        fi
    fi
}

### Local overrides ###

if [[ -f ~/.zshrc.local ]]; then
    source ~/.zshrc.local
fi

alias mux=tmuxinator

# Function for setting up a tmux session suitable for standard
# development. Takes a project name and an optional command. If a tmux
# session with the project name already exists, switches to it.
# Otherwise, you need to have wd installed. If a warp point with the
# project name already exists, jumps to it. Otherwise, uses fasd to
# make a guess at the correct directory (and creates a warp point,
# with your permission). After getting to the correct directory, sets
# up a tmux session with windows: emacs, git, zsh, zsh. Runs 'emacs'
# in the first window and 'git checkup' in the second. If you provide
# a second argument, runs it as a shell command (provide multiple
# commands with '&&' or ';') in all four windows before anything else.
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

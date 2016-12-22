################################################################################
#### Define default bundle list

bundles=(
    "plugins/docker, from:oh-my-zsh" # Completion for docker
    "plugins/fasd, from:oh-my-zsh" # Quickly jump to directories
    "plugins/lein, from:oh-my-zsh" # Completion for lein
    "plugins/sudo, from:oh-my-zsh" # Quickly re-run commands with sudo
    "plugins/tmuxinator, from:oh-my-zsh" # Completion for tmuxinator
    "plugins/vault, from:oh-my-zsh" # Completion for vault
    "plugins/wd, from:oh-my-zsh" # Quickly jump to directories
    "zsh-users/zsh-autosuggestions" # Autosuggestions from history
)

################################################################################
#### Define bundle list management functions

# Usage: add_bundle <zplug-args>
#
# Adds a bundle to $bundles. Word splitting will be performed on
# zplug-args to determine the arguments that will be passed to zplug.
function add_bundle() {
    if ! (( ${bundles[(I)$1]} )); then
        bundles+=($1)
    fi
}

# Usage: remove_bundle <zplug-args>
#
# Removes a bundle from $bundles by name. The name should be exactly
# the same as it appears in $bundles, with spaces if necessary.
function remove_bundle() {
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
export ZSH=$ZPLUG_HOME/repos/robbyrussell/oh-my-zsh
export ZSH_CACHE_DIR=$ZSH/cache

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
#### Prompt

# Enable parameter expansion and other substitutions in the $PROMPT.
setopt prompt_subst

# Here we define a prompt that displays the current directory and git
# branch, and turns red on a nonzero exit code. Adapted heavily from
# [1], with supporting functions extracted from Oh My Zsh [2] so that
# we don't have to load the latter as a dependency.
#
# [1]: https://github.com/robbyrussell/oh-my-zsh/blob/master/themes/mgutz.zsh-theme
# [2]: https://github.com/robbyrussell/oh-my-zsh/blob/3705d47bb3f3229234cba992320eadc97a221caf/lib/git.zsh

# Function that prints the branch or revision of the current HEAD,
# surrounded by square brackets and followed by an asterisk if the
# working directory is dirty, if the user is inside a Git repository.
function radian_prompt_git_info() {
    local ref
    ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
        ref=$(command git rev-parse --short HEAD 2> /dev/null) || \
        return 0
    echo "[${ref#refs/heads/}$(radian_prompt_git_dirty)]"
}

# Function that prints an asterisk if the working directory is dirty.
# If $RADIAN_PROMPT_IGNORE_UNTRACKED_FILES is true, then untracked
# files are not counted as dirty.
function radian_prompt_git_dirty() {
    local FLAGS
    FLAGS=('--porcelain' '--ignore-submodules=dirty')
    if [[ $RADIAN_PROMPT_IGNORE_UNTRACKED_FILES == true ]]; then
        FLAGS+='--untracked-files=no'
    fi
    if [[ $(command git status ${FLAGS} 2> /dev/null | tail -n1) ]]; then
        echo "*"
    fi
}

# Define the actual prompt format.
PROMPT='%(?.%{$fg[blue]%}.%{$fg[red]%})%c%{$reset_color%}$(radian_prompt_git_info)%(?.%{$fg[blue]%}.%{$fg[red]%}) %# %{$reset_color%}'

################################################################################
#### Input/output

# When no arguments are provided to "." or "source", they default to
# sourcing .zshrc. Based on [1], thanks @PythonNut!
#
# [1]: http://unix.stackexchange.com/a/326948/176805
function _accept-line() {
    if [[ $BUFFER == "." ]]; then
        BUFFER=". ~/.zshrc"
    elif [[ $BUFFER == "source" ]]; then
        BUFFER="source ~/.zshrc"
    fi
    zle .accept-line
}
zle -N accept-line _accept-line

# Allow comments even in the interactive shell (start with #).
setopt interactive_comments

# Allow escaping a single quote within a singly-quoted string by
# prefixing it with an additional single quote: echo 'It''s me!'
setopt rc_quotes

# Turn off flow control (which makes it so that ctrl+s and ctrl+q
# freeze and unfreeze command output, respectively).
unsetopt flow_control

################################################################################
#### Completion

# For ambiguous completions, use an interactive menu (which can be
# escaped with C-g) instead of overwriting the current command.
zstyle ':completion:*' menu select

# Allow usage of shift-tab (backtab) to go backward in the completion
# menu.
bindkey '^[[Z' reverse-menu-complete

# Substring completions. Not fuzzy. Sometimes they have weird
# behavior. This is the best I can manage for now, since I've been
# working on completions literally all day. See [1]. (Why is zsh so
# hard? Sigh.)
#
# [1]: http://unix.stackexchange.com/q/330481/176805
zstyle ':completion:*' matcher-list 'l:|=* r:|=* m:{a-z\-}={A-Z\_}'

################################################################################
#### Globbing

# This makes globs case-insensitive.
unsetopt case_glob

# This makes globbing regexes case-insensitive.
unsetopt case_match

# Allow globs to match dotfiles.
setopt glob_dots

# Sort numeric filenames numerically, instead of lexicographically.
setopt numeric_glob_sort

################################################################################
#### Command history

# Never discard history within a session, or at least not before any
# reasonable amount of time.
HISTSIZE=1000000

# Save history to disk. The value of this option is the default
# installed by zsh-newuser-install.
HISTFILE=~/.zsh_history

# Never discard history in the file on disk, either.
SAVEHIST=1000000

# Don't save commands to the history if they start with a leading
# space. This is useful if you have to pass a password as a parameter
# to a command.
setopt hist_ignore_space

# All zsh sessions share the same history file. Timestamps are also
# recorded for each command.
setopt share_history

# Use OS-provided locking mechanisms for the history file, if
# available. The manual says this might improve performance and
# decrease the chance of corruption.
setopt hist_fcntl_lock

# Remove superfluous whitespace when saving commands to the history.
setopt hist_reduce_blanks

# When history expansion is used (e.g. sudo !!), do the expansion
# instead of executing the command immediately. (Of course, the above
# use case is better serviced by just pressing ESC twice.)
setopt hist_verify

################################################################################
#### Filesystem navigation

# You can cd to a directory just by typing its name; no need to
# preface it with cd.
setopt autocd

# This makes it so that when you cd to a new directory, the old
# directory is saved in the directory stack (view with dirs or ds).
setopt autopushd

# This makes it so that the working directory path is automatically
# fully resolved. This means that symlink components will be followed,
# and capitalization will be corrected if you are on a
# case-insensitive filesystem.
setopt chaselinks

# Better ls defaults.
if command -v exa &>/dev/null; then
    alias l='exa --all --git --header --long'
    alias lg='exa --all --git --grid --header --long'
    alias lt='exa --all --git --header --long --tree'
    alias ltl='exa --all --git --header --long --tree --level'
else
    if command -v gls &>/dev/null; then
        alias l='command gls -AlhF --color=auto'
    else
        alias l='ls -AlhF'
    fi
    if command -v tree &>/dev/null; then
        alias lt=tree
        alias ltl='tree -L'
    fi
fi

# These are global aliases; you can use them anywhere in a command.
alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'
alias -g .......='../../../../../..'
alias -g ........='../../../../../../..'
alias -g .........='../../../../../../../..'
alias -g ..........='../../../../../../../../..'

# These are some aliases for moving to previously visited directories.
# The first alias uses "--" so that we can alias "-" without it being
# interpreted as a flag for the alias command.
alias -- -='cd -'
alias 1='cd -'
alias 2='cd -2'
alias 3='cd -3'
alias 4='cd -4'
alias 5='cd -5'
alias 6='cd -6'
alias 7='cd -7'
alias 8='cd -8'
alias 9='cd -9'

# To complement the previous set of aliases, here is a convenient way
# to list the last few directories visited, with their numbers. The
# alias "d", which is used by oh-my-zsh for this purpose, is taken
# from fasd, so instead I chose a different convenient abbreviation of
# "dirs".
alias ds='dirs -v | head -10'

# These aliases are for interacting with directories on the
# filesystem.
alias md='mkdir -p'
alias rd='rmdir'
function mcd() {
    mkdir -p $@
    cd ${@[$#]}
}

# You can "copy" any number of files, then "paste", "move" or
# "pasteln" them to pass them as arguments to cp, mv, or ln
# respectively. Just like a graphical filesystem manager. Each of the
# latter three functions defaults to the current directory as the
# destination.
function copy() {
    RADIAN_COPY_TARGETS=()
    for target; do
        if [[ $target == /* ]]; then
            RADIAN_COPY_TARGETS+=($target)
        else
            RADIAN_COPY_TARGETS+=($PWD/$target)
        fi
    done
}
function paste() {
    cp -R $RADIAN_COPY_TARGETS ${1:-.}
}
function move() {
    mv $RADIAN_COPY_TARGETS ${1:-.}
}
function pasteln() {
    ln -s $RADIAN_COPY_TARGETS ${1:-.}
}

# This function takes a symlink, resolves it, and replaces it with a
# copy of whatever it points to.
function delink() {
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

################################################################################
#### Man

# By default, run-help is an alias to man. We want to turn that off so
# that we can access the function definition of run-help (by default,
# aliases take precedence over functions). But if you re-source this
# file, then the alias might already be removed, so we suppress any
# error that this might throw.
unalias run-help 2>/dev/null || true

# Now we tell Zsh to autoload the run-help function, meaning that when
# it is invoked, Zsh will load the function from the file where it is
# defined. (That file comes with Zsh.) There are additional functions
# that we can autoload that will increase the functionality of
# run-help, but unfortunately they have a serious bug that causes them
# to crash when there is an alias defined for the function that you
# are requesting help for. (For example, loading run-help-git causes
# an error when requesting help for git because we later alias
# git=hub.) So we don't bother with those.
autoload -Uz run-help

# We define a function that wraps man to provide some basic
# highlighting for man pages. This makes them a little easier on the
# eyes. (This is done by binding some environment variables that less
# looks at.) See [1].
#
# [1]: https://github.com/robbyrussell/oh-my-zsh/blob/3ebbb40b31fa1ce9f10040742cdb06ea04fa7c41/plugins/colored-man-pages/colored-man-pages.plugin.zsh
function man() {
    env \
	LESS_TERMCAP_mb=$(printf "\e[1;31m") \
	LESS_TERMCAP_md=$(printf "\e[1;31m") \
	LESS_TERMCAP_me=$(printf "\e[0m") \
	LESS_TERMCAP_ue=$(printf "\e[0m") \
	LESS_TERMCAP_us=$(printf "\e[1;32m") \
	man $@
}

# Now we make some convenient aliases to the run-help system. Note
# that this means there are actually three different things called
# man: an executable, a function, and an alias. It all seems to work
# out as necessary, though, so that you get the run-help features,
# with the syntax highlighting as defined above, when you type man.
alias man=run-help
alias help=run-help

################################################################################
#### Git

alias g=git

alias gh='git help'

alias gi='git init'

alias gst='git status'

alias gsh='git show'
alias gshs='git show --stat'

alias gl='git log --graph --decorate'
alias gls='git log --graph --decorate --stat'
alias glp='git log --graph --decorate --patch'
alias glps='git log --graph --decorate --patch --stat'
alias glo='git log --graph --decorate --oneline'
alias gla='git log --graph --decorate --all'
alias glas='git log --graph --decorate --all --stat'
alias glap='git log --graph --decorate --all --patch'
alias glaps='git log --graph --decorate --all --patch --stat'
alias glao='git log --graph --decorate --all --oneline'
glg() {
    if (( $# >= 1 )); then
        git log --grep=$1 --graph --decorate ${@:2}
    else
        echo "No query provided."
        return 1
    fi
}
glgs() {
    if (( $# >= 1 )); then
        git log --grep=$1 --graph --decorate --stat ${@:2}
    else
        echo "No query provided."
        return 1
    fi
}
glgp() {
    if (( $# >= 1 )); then
        git log --grep=$1 --graph --decorate --patch ${@:2}
    else
        echo "No query provided."
        return 1
    fi
}
glgps() {
    if (( $# >= 1 )); then
        git log --grep=$1 --graph --decorate --patch --stat ${@:2}
    else
        echo "No query provided."
        return 1
    fi
}
glgo() {
    if (( $# >= 1 )); then
        git log --grep=$1 --graph --decorate --oneline ${@:2}
    else
        echo "No query provided."
        return 1
    fi
}
glga() {
    if (( $# >= 1 )); then
        git log --grep=$1 --graph --decorate --all ${@:2}
    else
        echo "No query provided."
        return 1
    fi
}
glgsa() {
    if (( $# >= 1 )); then
        git log --grep=$1 --graph --decorate --all --stat ${@:2}
    else
        echo "No query provided."
        return 1
    fi
}
glgpa() {
    if (( $# >= 1 )); then
        git log --grep=$1 --graph --decorate --all --patch ${@:2}
    else
        echo "No query provided."
        return 1
    fi
}
glgpsa() {
    if (( $# >= 1 )); then
        git log --grep=$1 --graph --decorate --all --patch --stat ${@:2}
    else
        echo "No query provided."
        return 1
    fi
}
glgoa() {
    if (( $# >= 1 )); then
        git log --grep=$1 --graph --decorate --all --oneline ${@:2}
    else
        echo "No query provided."
        return 1
    fi
}

alias ga='git add'
alias gap='git add --patch'
alias gaa='git add --all'

alias grm='git rm'

alias gmv='git mv'

alias gr='git reset'
alias grs='git reset --soft'
alias grh='git reset --hard'
alias grp='git reset --patch'

alias gc='git commit --verbose'
alias gca='git commit --verbose --amend'
alias gcf='git commit -C HEAD --amend'
alias gce='git commit --verbose --allow-empty'
alias gcm='git commit -m'
alias gcam='git commit --amend -m'
alias gcem='git commit --allow-empty -m'
function gcw() {
    # This logic is taken from [1]. I think it is designed to
    # correctly deal with the three kinds of changes that might need
    # to be added: changes to existing files, untracked files, and
    # deleted files. (These are surprisingly difficult to account for
    # all at the same time.)
    #
    # [1]: https://github.com/robbyrussell/oh-my-zsh/blob/3477ff25274fa75bd9e6110f391f6ad98ca2af72/plugins/git/git.plugin.zsh#L240
    git add --all
    git rm $(git ls-files --deleted) 2>/dev/null
    git commit --message="(wip) ${(j: :)@}" --quiet && git show --stat
}
function gcaw() {
    git add --all
    git rm $(git ls-files --deleted) 2>/dev/null
    git commit --message="(wip) ${(j: :)@}" --quiet --amend && git show --stat
}
function gcwp() {
    gcw $@
    git push --force
}
function gcawp() {
    gcaw $@
    git push --force
}

alias gcp='git cherry-pick'
alias gcpc='git cherry-pick --continue'
alias gcpa='git cherry-pick --abort'

alias gt='git tag'
alias gtd='git tag -d'

alias gn='git notes'
alias gna='git notes add'
alias gnr='git notes remove'

alias gsta='git stash save'
alias gstau='git stash save --include-untracked'
alias gstap='git stash save --patch'
alias gstl='git stash list'
alias gsts='git stash show --text'
alias gstss='git stash show --stat'
alias gstaa='git stash apply'
alias gstp='git stash pop'
alias gstd='git stash drop'

alias gd='git diff'
alias gds='git diff --stat'

alias gbl='git blame'

alias gb='git branch'
alias gbd='git branch --delete'
alias gbdd='git branch --delete --force'
function gbu() {
    if (( $# == 1 )) && ! (echo $1 | fgrep -q /); then
        if branch_name=$(git rev-parse --abbrev-ref HEAD 2>/dev/null); then
            git branch --set-upstream-to=$1/${branch_name##refs/heads/}
        else
            echo "There is no current branch."
            return 1
        fi
    else
        git branch --set-upstream-to=$@
    fi
}

alias gco='git checkout'
alias gcop='git checkout --patch'
alias gcob='git checkout -B'

alias gbs='git bisect'
alias gbss='git bisect start'
alias gbsg='git bisect good'
alias gbsb='git bisect bad'
alias gbsr='git bisect reset'

alias gm='git merge'
alias gma='git merge --abort'

alias grb='git rebase'
alias grbi='git rebase --interactive'
alias grbc='git rebase --continue'
alias grbs='git rebase --skip'
alias grba='git rebase --abort'

alias gcl='git clone --recursive'

alias gre='git remote'
alias grel='git remote list'
alias gres='git remote show'

alias gf='git fetch --prune'
alias gfa='git fetch --all --prune'

alias gu='git pull'
alias gur='git pull --rebase'
alias gum='git pull --no-rebase'

alias gp='git push'
alias gpf='git push --force'
function gpu() {
    local branch_name
    if (( $# == 1 )); then
        if branch_name=$(git rev-parse --abbrev-ref HEAD 2>/dev/null); then
            git push --set-upstream $1 ${branch_name##refs/heads/}
        else
            echo "There is no current branch."
            return 1
        fi
    else
        git push --set-upstream $@
    fi
}
alias gpd='git push --delete'

################################################################################
#### Hub

# This extends Git to work especially well with Github. See [1] for
# more information.
#
# [1]: https://github.com/github/hub
alias git=hub

################################################################################
#### Tmux

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
function proj() {
    if ! type tmux &>/dev/null; then
        echo "You need tmux for this to work."
    fi
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

################################################################################
#### Emacs

alias emacs='command emacs -nw'
alias emacsw='command emacs'
alias emac='emacsclient --alternate-editor="" -nw'
alias emacw='emacsclient --alternate-editor=""'

alias e=emacs
alias ew=emacsw
alias ec=emac
alias ecw=emacw

################################################################################
#### Leiningen

# Prevent Leiningen tasks (I'm looking at you, lein uberjar) from
# showing up in the Mac app switcher. See [1].
#
# [1]: http://stackoverflow.com/q/24619300/3538165
export LEIN_JVM_OPTS=-Dapple.awt.UIElement=true

################################################################################
#### Vim

alias vim=nvim

################################################################################
#### Fasd

# Turn off case sensitivity permanently in Fasd. This functionality is
# only available in my fork of Fasd.
export _FASD_NOCASE=1

################################################################################
#### Load user-specific configuration file (2 of 2)

if [[ -f ~/.zshrc.local ]]; then
    source ~/.zshrc.local
fi

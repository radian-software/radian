## Locate Radian repository

# Set $RADIAN to the location of the Radian repository, if found.
if [[ -L $0 && -d ${0:A:h}/radian-emacs ]]; then
    export RADIAN=${0:A:h}
else
    unset RADIAN
fi

## External configuration
### ~/.zshrc.local

if [[ -f ~/.zshrc.local ]]; then
    . ~/.zshrc.local
fi

## Set up plugin manager

if [[ -f ~/.local/share/znap/znap/znap.zsh ]]; then
    . ~/.local/share/znap/znap/znap.zsh
elif (( $+commands[git] )); then
    mkdir -p ~/.local/share/znap
    git clone https://github.com/marlonrichert/zsh-snap.git ~/.local/share/znap/znap
    . ~/.local/share/znap/znap/znap.zsh
fi

### znap

if typeset -f znap >/dev/null; then
    # Provides the 'wdx' function to set warp points to directories
    # and quickly jump to them.
    znap source radian-software/wdx

    # If a previous command starts with what you have typed, show it
    # in dimmed color after the cursor, and allow completing it.
    znap source zsh-users/zsh-autosuggestions

    # Configure tab-completions for many external commands.
    znap clone zsh-users/zsh-completions
    fpath+=(~[zsh-users/zsh-completions]/src)

    if typeset -f radian_znap_hook > /dev/null; then
        radian_znap_hook
    fi
fi

# Disable automatically enabling git-maintenance by znap, because it
# will write it into the Radian .gitconfig and include hardcoded paths
# there. You can enable git-maintenance if you want but it needs to be
# done in the .zshrc.local.
#
# https://github.com/marlonrichert/zsh-snap?tab=readme-ov-file#automatic-git-maintenance
zstyle ':znap:*:*' git-maintenance off

## Shell configuration
### Prompt

# Enable parameter expansion and other substitutions in the $PROMPT.
setopt prompt_subst

# Load some associative arrays (color, fg, and bg) that give us
# convenient access to color-changing escape codes.
autoload -U colors && colors

# Here we define a prompt that displays the current directory and git
# branch, and turns red on a nonzero exit code. Adapted heavily from
# [1], with supporting functions extracted from Oh My Zsh [2] so that
# we don't have to load the latter as a dependency.
#
# [1]: https://github.com/robbyrussell/oh-my-zsh/blob/master/themes/mgutz.zsh-theme
# [2]: https://github.com/robbyrussell/oh-my-zsh/blob/3705d47bb3f3229234cba992320eadc97a221caf/lib/git.zsh

# Display the user@hostname. Then change the color and display the
# working directory.
rpp='%{${RADIAN_PROMPT_PREFIX:-}%}%{$fg[yellow]%}{%n@${RADIAN_HOSTNAME:-%m}}'
radian_prompt_prefix="${rpp}"' %(?.%{$fg[blue]%}.%{$fg[red]%})%c'

# Change the color and then display a '%' or '#', then reset the color
# for the user's input.
radian_prompt_suffix='%(?.%{$fg[blue]%}.%{$fg[red]%}) %# %{$reset_color%}'

PROMPT=

if (( $+commands[git] )); then

    # Usage: radian_prompt_git_dirty
    #
    # Print an asterisk if the working directory is dirty.
    function radian_prompt_git_dirty {
        emulate -LR zsh
        if [[ $(command git status --porcelain 2>/dev/null | tail -n1) ]]; then
            echo "*"
        fi
    }

    # Usage: radian_prompt_git_info
    #
    # If inside a Git repository, print the branch or abbreviated
    # revision of the current HEAD, surrounded by square brackets and
    # followed by an asterisk if the working directory is dirty.
    function radian_prompt_git_info {
        emulate -LR zsh
        local ref
        ref=$(command git symbolic-ref HEAD 2> /dev/null) || \
            ref=$(command git rev-parse --short HEAD 2> /dev/null) || \
            return 0
        echo "[${ref#refs/heads/}$(radian_prompt_git_dirty)]"
    }

    # Reset the color and display the Git branch and modification
    # status.
    PROMPT='%{$reset_color%}$(radian_prompt_git_info)'

fi

PROMPT="${radian_prompt_prefix}${PROMPT}"
PROMPT="${PROMPT}${radian_prompt_suffix}"

### Command line

# Force the usage of Emacs keybindings. Otherwise they will be set
# depending on whether the literal string "vi" appears in the value of
# EDITOR, which is a terrible idea for many reasons (not least of
# which being that my EDITOR is Vim while I want to use Emacs
# keybindings in Zsh).
bindkey -e

# Allow a very fast way (just typing ".") to reload the shell
# configuration. Based on [1].
#
# [1]: https://unix.stackexchange.com/a/326948/176805
function _accept-line {
    emulate -LR zsh
    if [[ $BUFFER == "." ]]; then
        BUFFER="exec zsh"
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

#### Completion

# For a modern primer on zsh completion system configuration:
# https://thevaluable.dev/zsh-completion-guide-examples/

# Display a list of the available candidates instead of just cycling
# through them blindly.
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

# If there is only one candidate just insert it.
zstyle ':autocomplete:*complete*:*' insert-unambiguous yes

# Allow usage of shift-tab (backtab) to go backward in the completion
# menu. See <https://stackoverflow.com/a/842370/3538165>.
bindkey '^[[Z' reverse-menu-complete

#### Globbing

# This makes globs case-insensitive.
unsetopt case_glob

# This makes globbing regexes case-insensitive.
unsetopt case_match

# Allow globs to match dotfiles.
setopt glob_dots

# Sort numeric filenames numerically, instead of lexicographically.
setopt numeric_glob_sort

# Disable history expansion, so we can use ! in our commands.
setopt no_bang_hist

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
# instead of executing the command immediately. This currently has no
# effect since history expansion is disabled.
setopt hist_verify

# Deduplicate history entries. Helps with retrieving previous
# commands.
setopt hist_ignore_all_dups

### Filesystem navigation

# This makes it so that when you cd to a new directory, the old
# directory is saved in the directory stack (view with dirs or ds).
setopt autopushd

# This makes it so that "cd -n" gives you the directory you were in n
# cd's ago, instead of the nth directory you have visited in the shell
# session. (You can use "cd +n" to recover the latter behavior.)
setopt pushdminus

# This makes it so that the working directory path is automatically
# fully resolved. This means that symlink components will be followed,
# and capitalization will be corrected if you are on a
# case-insensitive filesystem.
setopt chaselinks

# If you just type in a directory name, cd to it (unless it's also a
# valid executable name).
setopt autocd

### Help system

# By default, run-help is an alias to man. We want to turn that off so
# that we can access the function definition of run-help (by default,
# aliases take precedence over functions). But if you re-source this
# file, then the alias might already be removed, so we suppress any
# error that this might throw.
unalias run-help 2>/dev/null || true

# Now we autoload run-help and several extensions to it which provide
# more precise help for certain external commands.
autoload -Uz run-help
autoload -Uz run-help-git
autoload -Uz run-help-ip
autoload -Uz run-help-openssl
autoload -Uz run-help-p4
autoload -Uz run-help-sudo
autoload -Uz run-help-svk
autoload -Uz run-help-svn

## Aliases
### Filesystem navigation
#### cd

alias -- -='cd -'
alias -- -1='cd -1'
alias -- -2='cd -2'
alias -- -3='cd -3'
alias -- -4='cd -4'
alias -- -5='cd -5'
alias -- -6='cd -6'
alias -- -7='cd -7'
alias -- -8='cd -8'
alias -- -9='cd -9'

#### dirs

# This alias is a convenient way to list the last few directories
# visited, with their numbers. You can then use the 'cd -n' aliases to
# jump to those directories.
alias ds='dirs -v | head -10'

#### ls, eza

if (( $+commands[eza] )); then

    function l {
        emulate -LR zsh
        eza --all --header --long --color-scale=all $@
    }

    function lg {
        emulate -LR zsh
        l --grid $@
    }

    function lt {
        emulate -LR zsh
        l --tree --ignore-glob ".git|.svn|node_modules" $@
    }

    function lti {
        emulate -LR zsh
        l --tree --ignore-glob ".git|.svn|node_modules|$1" ${@:2}
    }

    function ltl {
        emulate -LR zsh
        lt --level $@
    }

    function ltli {
        l --tree --level $1 --ignore-glob ".git|.svn|node_modules|$2" ${@:3}
    }

else
    # We alias gls to a git command elsewhere, so we use "command"
    # here to prevent it from being interpreted as said git command.
    if (( $+commands[gls] )); then
        alias l='command gls -AlhF --color=auto'
    else
        alias l='ls -AlhF'
    fi
    if (( $+commands[tree] )); then
        alias lt='tree -a'
        alias ltl='tree -aL'
    fi
fi

#### wdx

if command -v wdx &>/dev/null; then
    alias wd='wdx'
    alias ws='wdx set'
    alias wsf='wdx set -f'
fi

### Filesystem management

#### cp, mv, ln

# You can "copy" any number of files, then "paste", "move" or
# "pasteln" them to pass them as arguments to cp, mv, or ln
# respectively. Just like a graphical filesystem manager. Each of the
# latter three functions defaults to the current directory as the
# destination.

# Usage: copy <path>...
#
# Copy all of the paths provided to the clipboard, stored in the array
# $RADIAN_CLIPBOARD.
function copy {
    emulate -LR zsh
    RADIAN_CLIPBOARD=()
    for target; do
        RADIAN_CLIPBOARD+=(${target:a})
    done
}

# Usage: paste [<path>]
#
# Invoke 'cp -R' on all paths in $RADIAN_CLIPBOARD as well as the
# argument provided, which defaults to the current directory.
function paste {
    emulate -LR zsh
    cp -R $RADIAN_CLIPBOARD ${1:-.}
}

# Usage: move [<path>]
#
# Invoke 'mv' on all paths in $RADIAN_CLIPBOARD as well as the
# argument provided, which defaults to the current directory.
function move {
    emulate -LR zsh
    mv $RADIAN_CLIPBOARD ${1:-.}
}

# Usage: pasteln [<path>]
#
# Invoke 'ln -s' on all paths in $RADIAN_CLIPBOARD as well as the
# argument provided, which defaults to the current directory.
function pasteln {
    emulate -LR zsh
    ln -s $RADIAN_CLIPBOARD ${1:-.}
}

# Usage: delink <path>
#
# Resolve the symlink at the given path and replace it with a copy of
# the file it points to.
function delink {
    emulate -LR zsh
    if [[ -z $1 ]]; then
        echo >&2 "usage: delink <symlinks>"
        return 1
    fi
    for link; do
        if [[ -L $link ]]; then
            if [[ -e $link ]]; then
                target=${link:A}
                if rm $link; then
                    if cp -R $target $link; then
                        echo >&2 "Copied $target to $link"
                    else
                        ln -s $target $link
                    fi
                fi
            else
                echo >&2 "Broken symlink: $link"
            fi
        else
            echo >&2 "Not a symlink: $link"
        fi
    done
}

# Usage: transpose <path1> <path2>
#
# Swap the files or directories at the two provided paths. Not atomic.
# Both paths must exist.
function transpose {
    emulate -LR zsh
    if (( $# != 2 )); then
        echo >&2 "usage: transpose <path1> <path2>"
        return 1
    fi
    for arg in $1 $2; do
        if [[ ! -e $arg && ! -L $arg ]]; then
            echo >&2 "no such file or directory: $arg"
            return 1
        fi
        if [[ -e $path.tmp || -L $path.tmp ]]; then
            echo >&2 "already exists: $path.tmp"
            return 1
        fi
    done
    mv $1 $1.tmp
    mv $2 $2.tmp
    mv $1.tmp $2
    mv $2.tmp $1
}

#### mkdir

alias md='mkdir -p'

function mcd {
    emulate -LR zsh
    mkdir -p $@
    cd ${@[$#]}
}

compdef _mkdir mcd

#### rmdir

alias rd='rmdir'

### Help system

alias help=run-help

### Utilities
#### Docker

if (( $+commands[docker] )); then
    alias dr='docker run -it --rm'
fi

#### Emacs

if (( $+commands[emacs] )); then
    alias e='emacs -nw'
    alias eq='emacs -nw -Q'
    alias ew='emacs'
    alias eqw='emacs -Q'
    alias ue='USER_EMACS_DIRECTORY=$PWD e'
    alias uew='USER_EMACS_DIRECTORY=$PWD ew'
fi

if (( $+commands[emacsclient] )); then
    alias ec='emacsclient --alternate-editor= -nw'
    alias ecw='emacsclient --alternate-editor='
fi

#### fd

if (( $+commands[fdfind] )); then
    alias fd='fdfind'
fi

#### Git

if (( $+commands[git] )); then
    alias g=git

    alias ge='git help'
    alias ghi='git help init'
    alias ghst='git help status'
    alias ghsh='git help show'
    alias ghl='git help log'
    alias gha='git help add'
    alias ghrm='git help rm'
    alias ghmv='git help mv'
    alias ghr='git help reset'
    alias ghcm='git help commit'
    alias ghcp='git help cherry-pick'
    alias ghrv='git help revert'
    alias ght='git help tag'
    alias ghn='git help notes'
    alias ghsta='git help stash'
    alias ghd='git help diff'
    alias ghbl='git help blame'
    alias ghb='git help branch'
    alias ghco='git help checkout'
    alias ghlsf='git help ls-files'
    alias ghx='git help clean'
    alias ghbs='git help bisect'
    alias ghm='git help merge'
    alias ghrb='git help rebase'
    alias ghsm='git help submodule'
    alias ghcl='git help clone'
    alias ghre='git help remote'
    alias ghf='git help fetch'
    alias ghu='git help pull'
    alias ghp='git help push'

    alias gi='git init'

    alias gst='git status'

    alias gsh='git show'
    alias gshs='git show --stat'

    for nograph in "" n; do
        local graph_flags=
        if [[ -z $nograph ]]; then
            graph_flags=" --graph"
        fi
        for all in "" a; do
            local all_flags=
            if [[ -n $all ]]; then
                all_flags=" --all"
            fi
            for oneline in "" o; do
                local oneline_flags=
                if [[ -n $oneline ]]; then
                    oneline_flags=" --oneline"
                fi
                for diff in "" s p ps sp; do
                    local diff_flags=
                    case $diff in
                        s) diff_flags=" --stat";;
                        p) diff_flags=" --patch";;
                        ps|sp) diff_flags=" --patch --stat";;
                    esac
                    for search in "" g G S; do
                        local search_flags=
                        case $search in
                            g) search_flags=" --grep";;
                            G) search_flags=" -G";;
                            S) search_flags=" -S";;
                        esac
                        alias="gl${nograph}${all}${oneline}${diff}${search}="
                        alias+="git log --decorate"
                        alias+="${graph_flags}${all_flags}"
                        alias+="${oneline_flags}${diff_flags}${search_flags}"
                        alias $alias
                    done
                done
            done
        done
    done

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
    alias gcaa='git commit --verbose --amend --all'
    alias gcf='git commit -C HEAD --amend'
    alias gcfa='git commit -C HEAD --amend --all'
    alias gce='git commit --verbose --allow-empty'
    alias gcm='git commit -m'
    alias gcma='git commit --all -m'
    alias gcam='git commit --amend -m'
    alias gcama='git commit --amend --all -m'
    alias gcem='git commit --allow-empty -m'

    alias gcn='git commit --no-verify --verbose'
    alias gcna='git commit --no-verify --verbose --amend'
    alias gcnaa='git commit --no-verify --verbose --amend --all'
    alias gcnf='git commit --no-verify -C HEAD --amend'
    alias gcnfa='git commit --no-verify -C HEAD --amend --all'
    alias gcne='git commit --no-verify --verbose --allow-empty'
    alias gcnm='git commit --no-verify -m'
    alias gcnma='git commit --no-verify --all -m'
    alias gcnam='git commit --no-verify --amend -m'
    alias gcnama='git commit --no-verify --amend --all -m'
    alias gcnem='git commit --no-verify --allow-empty -m'

    alias gcp='git cherry-pick'
    alias gcpc='git cherry-pick --continue'
    alias gcpa='git cherry-pick --abort'

    alias grv='git revert'
    alias grva='git revert --abort'
    alias grvm='git revert -m'

    alias gt='git tag'
    alias gtd='git tag --delete'

    alias gn='git notes'
    alias gna='git notes add'
    alias gne='git notes edit'
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

    alias gd='git diff --minimal'
    alias gds='git diff --minimal --stat'
    alias gdc='git diff --minimal --cached'
    alias gdcs='git diff --minimal --cached --stat'
    alias gdn='git diff --minimal --no-index'

    alias gbl='git blame'

    alias gb='git branch'
    alias gbsu='git branch --set-upstream-to'
    alias gbusu='git branch --unset-upstream'
    alias gbd='git branch --delete'
    alias gbdd='git branch --delete --force'

    alias gco='git checkout'
    alias gcot='git checkout --track'
    alias gcop='git checkout --patch'
    alias gcob='git checkout -B'

    alias glsf='git ls-files'

    alias gx='git clean'
    alias gxu='git clean -ffd'
    alias gxi='git clean -ffdX'
    alias gxa='git clean -ffdx'

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

    alias gsm='git submodule'
    alias gsma='git submodule add'
    alias gsms='git submodule status'
    alias gsmi='git submodule init'
    alias gsmd='git submodule deinit'
    alias gsmu='git submodule update --recursive'
    alias gsmui='git submodule update --init --recursive'
    alias gsmf='git submodule foreach'
    alias gsmy='git submodule sync'

    alias gcl='git clone --recursive'
    alias gcls='git clone --depth=1 --single-branch --no-tags'

    alias gre='git remote'
    alias grel='git remote list'
    alias gres='git remote show'

    alias gf='git fetch'
    alias gfa='git fetch --all'
    alias gfu='git fetch --unshallow'
    alias gfp='git fetch --prune'

    alias gu='git pull'
    alias gur='git pull --rebase --autostash'
    alias gum='git pull --no-rebase'
    alias gup='git pull --prune'

    alias gp='git push'
    alias gpa='git push --all'
    alias gpf='git push --force-with-lease'
    alias gpff='git push --force'
    alias gpu='git push --set-upstream'
    alias gpd='git push --delete'
    alias gpt='git push --tags'
fi

#### Tmux

if (( $+commands[tmux] )); then
    alias ta='tmux attach'
    function ts {
        tmux attach -s ${1:-tmux} 2>/dev/null || tmux new-session -s ${1:-tmux}
    }
fi

#### Trash

if (( $+commands[trash] )); then
    alias t='trash'
fi

#### Vi, Vim, Neovim

if (( $+commands[nvim] )); then
    alias v='nvim'
elif (( $+commands[vim] )); then
    alias v='vim'
elif (( $+commands[vi] )); then
    alias v='vi'
fi

## External command configuration
### man

# We define a function that wraps man to provide some basic
# highlighting for man pages. This makes them a little easier on the
# eyes. (This is done by binding some environment variables that less
# looks at.) See [1].
#
# [1]: https://github.com/robbyrussell/oh-my-zsh/blob/3ebbb40b31fa1ce9f10040742cdb06ea04fa7c41/plugins/colored-man-pages/colored-man-pages.plugin.zsh
function man {
    env \
	LESS_TERMCAP_mb=$(printf "\e[1;31m") \
	LESS_TERMCAP_md=$(printf "\e[1;31m") \
	LESS_TERMCAP_me=$(printf "\e[0m") \
	LESS_TERMCAP_ue=$(printf "\e[0m") \
	LESS_TERMCAP_us=$(printf "\e[1;32m") \
	man $@
}

## External configuration hook

if typeset -f radian_after_init_hook > /dev/null; then
    radian_after_init_hook
fi

## Closing remarks

# Local Variables:
# outline-regexp: "##+"
# End:

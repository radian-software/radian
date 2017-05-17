################################################################################
#### Define default bundle list

bundles=(
    # Quickly jump to directories:
    "mfaerevaag/wd, use:wd.sh, rename-to:wd, as:command"
    # Display autosuggestions from history:
    "zsh-users/zsh-autosuggestions"
    # Completion definitions for lots of additional commands.
    "zsh-users/zsh-completions"
)

################################################################################
#### Define bundle list management functions

# Usage: radian_add_bundle <zplug-args>
#
# Adds a bundle to $bundles. Word splitting will be performed on
# zplug-args to determine the arguments that will be passed to zplug.
function radian_add_bundle() {
    emulate -LR zsh
    if ! (( ${bundles[(I)$1]} )); then
        bundles+=($1)
    fi
}

# Usage: radian_remove_bundle <zplug-args>
#
# Removes a bundle from $bundles by name. The name should be exactly
# the same as it appears in $bundles, with spaces if necessary.
function radian_remove_bundle() {
    emulate -LR zsh
    bundles=("${(@)bundles:#$1}")
}

################################################################################
#### Load local customizations

if [[ -f ~/.zshrc.local ]]; then
    . ~/.zshrc.local
fi

################################################################################
#### zplug

export ZPLUG_HOME=/usr/local/opt/zplug
export ZSH_CACHE_DIR=$ZSH/cache

if [[ -f $ZPLUG_HOME/init.zsh ]]; then
    . $ZPLUG_HOME/init.zsh

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

# Load some arrays that give us convenient access to color-changing
# escape codes.
autoload -U colors && colors

# Here we define a prompt that displays the current directory and git
# branch, and turns red on a nonzero exit code. Adapted heavily from
# [1], with supporting functions extracted from Oh My Zsh [2] so that
# we don't have to load the latter as a dependency.
#
# [1]: https://github.com/robbyrussell/oh-my-zsh/blob/master/themes/mgutz.zsh-theme
# [2]: https://github.com/robbyrussell/oh-my-zsh/blob/3705d47bb3f3229234cba992320eadc97a221caf/lib/git.zsh

if (( $+commands[git] )); then

    # Function that prints the branch or revision of the current HEAD,
    # surrounded by square brackets and followed by an asterisk if the
    # working directory is dirty, if the user is inside a Git repository.
    function radian_prompt_git_info() {
        emulate -LR zsh
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
        emulate -LR zsh
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

else

    # Backup prompt for if the user doesn't have Git.
    PROMPT='%(?.%{$fg[blue]%}.%{$fg[red]%})%c%(?.%{$fg[blue]%}.%{$fg[red]%}) %# %{$reset_color%}'

fi

################################################################################
#### Input/output

# When no arguments are provided to "." or "source", they default to
# sourcing .zshrc. Based on [1], thanks @PythonNut!
#
# [1]: http://unix.stackexchange.com/a/326948/176805
function _accept-line() {
    emulate -LR zsh
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

# This makes it so that "cd -n" gives you the directory you were in n
# cd's ago, instead of the nth directory you have visited in the shell
# session. (You can use "cd +n" to recover the latter behavior.)
setopt pushdminus

# This makes it so that the working directory path is automatically
# fully resolved. This means that symlink components will be followed,
# and capitalization will be corrected if you are on a
# case-insensitive filesystem.
setopt chaselinks

# Better ls defaults.
if (( $+commands[exa] )); then
    alias l='exa --all --header --long --color-scale'
    alias lg='exa --all --grid --header --long --color-scale'
    alias lt='exa --all --header --long --tree --color-scale --ignore-glob .git'
    function lti() {
        emulate -LR zsh
        exa --all --header --long --tree --color-scale --ignore-glob ".git|$1" ${@:2}
    }
    alias ltl='exa --all --header --long --tree --color-scale --ignore-glob .git --level'
    function ltli() {
        emulate -LR zsh
        exa --all --header --long --tree --color-scale --level $1 --ignore-glob ".git|$2" ${@:3}
    }
else
    # We alias gls to a git command elsewhere, so we use "command"
    # here to prevent it from being interpreted as said git command.
    # If you want to run coreutils ls, use "\gls".
    if (( $+commands[gls] )); then
        alias l='command gls -AlhF --color=auto'
    else
        alias l='ls -AlhF'
    fi
    if (( $+commands[tree] )); then
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

# Enable wd.
if (( $+commands[wd] )); then
    function wd {
        emulate -LR zsh
        . wd
    }
fi

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
    emulate -LR zsh
    mkdir -p $@
    cd ${@[$#]}
}

# You can "copy" any number of files, then "paste", "move" or
# "pasteln" them to pass them as arguments to cp, mv, or ln
# respectively. Just like a graphical filesystem manager. Each of the
# latter three functions defaults to the current directory as the
# destination.
function copy() {
    emulate -LR zsh
    radian_clipboard=()
    for target; do
        if [[ $target == /* ]]; then
            radian_clipboard+=($target)
        else
            radian_clipboard+=($PWD/$target)
        fi
    done
}
function paste() {
    emulate -LR zsh
    cp -R $radian_clipboard ${1:-.}
}
function move() {
    emulate -LR zsh
    mv $radian_clipboard ${1:-.}
}
function pasteln() {
    emulate -LR zsh
    ln -s $radian_clipboard ${1:-.}
}

# This function takes a symlink, resolves it, and replaces it with a
# copy of whatever it points to.
function delink() {
    emulate -LR zsh
    if [[ -z $1 ]]; then
        echo "usage: delink <symlinks>"
        return 1
    fi
    for link; do
        if [[ -L $link ]]; then
            if [[ -e $link ]]; then
                target=${link:A}
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
# an error when requesting help for git if you alias git=hub.) So we
# don't bother with those.
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

alias help=run-help

################################################################################
#### Git

if (( $+commands[git] )); then
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
    function glg {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log --grep=$1 --graph --decorate ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glgs {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log --grep=$1 --graph --decorate --stat ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glgp {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log --grep=$1 --graph --decorate --patch ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glgps {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log --grep=$1 --graph --decorate --patch --stat ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glgo {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log --grep=$1 --graph --decorate --oneline ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glga {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log --grep=$1 --graph --decorate --all ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glgsa {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log --grep=$1 --graph --decorate --all --stat ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glgpa {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log --grep=$1 --graph --decorate --all --patch ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glgpsa {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log --grep=$1 --graph --decorate --all --patch --stat ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glgoa {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log --grep=$1 --graph --decorate --all --oneline ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glS {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log -S $1 --graph --decorate ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glSs {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log -S $1 --graph --decorate --stat ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glSp {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log -S $1 --graph --decorate --patch ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glSps {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log -S $1 --graph --decorate --patch --stat ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glSo {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log -S $1 --graph --decorate --oneline ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glSa {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log -S $1 --graph --decorate --all ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glSsa {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log -S $1 --graph --decorate --all --stat ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glSpa {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log -S $1 --graph --decorate --all --patch ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glSpsa {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log -S $1 --graph --decorate --all --patch --stat ${@:2}
        else
            echo "No query provided."
            return 1
        fi
    }
    function glSoa {
        emulate -LR zsh
        if (( $# >= 1 )); then
            git log -S $1 --graph --decorate --all --oneline ${@:2}
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
    alias gcaa='git commit --verbose --amend --all'
    alias gcf='git commit -C HEAD --amend'
    alias gcfa='git commit -C HEAD --amend --all'
    alias gce='git commit --verbose --allow-empty'
    alias gcm='git commit -m'
    alias gcma='git commit --all -m'
    alias gcam='git commit --amend -m'
    alias gcama='git commit --amend --all -m'
    alias gcem='git commit --allow-empty -m'

    alias gcp='git cherry-pick'
    alias gcpc='git cherry-pick --continue'
    alias gcpa='git cherry-pick --abort'

    alias grv='git revert'
    alias grvm='git revert -m'

    alias gt='git tag'
    alias gtd='git tag -d'

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

    alias gd='git diff'
    alias gds='git diff --stat'
    alias gdc='git diff --cached'
    alias gdcs='git diff --cached --stat'

    alias gbl='git blame'

    alias gb='git branch'
    alias gbsu='git branch --set-upstream-to'
    alias gbusu='git branch --unset-upstream'
    alias gbd='git branch --delete'
    alias gbdd='git branch --delete --force'

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

    alias gsm='git submodule'
    alias gsma='git submodule add'
    alias gsms='git submodule status'
    alias gsmi='git submodule init'
    alias gsmd='git submodule deinit'
    alias gsmu='git submodule update'
    alias gsmf='git submodule foreach'
    alias gsmy='git submodule sync'

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
    alias gpu='git push --set-upstream'
    alias gpd='git push --delete'
fi

################################################################################
#### Hub

# This extends Git to work especially well with Github. See [1] for
# more information.
#
# [1]: https://github.com/github/hub
if (( $+commands[hub] )); then
    alias hcl='hub clone'
    alias hc='hub create --copy'
    alias hcp='hub create -p --copy'
    alias hf='hub fork'
    alias hp='hub pull-request --copy'
    alias hb='hub browse'
    alias hh='hub help'
    alias hi='hub issue'
fi

################################################################################
#### Tmux

if (( $+commands[tmux] )); then
    alias ta='tmux attach'
    alias ts='tmux new-session -s'
fi

################################################################################
#### Tmuxinator

if (( $+commands[tmuxinator] )); then
    alias mux=tmuxinator
fi

################################################################################
#### Emacs

if (( $+commands[emacs] )); then
    alias e='emacs -nw'
    alias ew='emacs'
fi

if (( $+commands[emacsclient] )); then
    alias ec='emacsclient --alternate-editor="" -nw'
    alias ecw='emacsclient --alternate-editor=""'
fi

################################################################################
#### Leiningen

if (( $+commands[lein] )); then
    # Prevent Leiningen tasks (I'm looking at you, lein uberjar) from
    # showing up in the Mac app switcher. See [1]. Also, attempt to
    # reduce the incidence of exceptions with missing traces in
    # Clojure. See [2].
    #
    # [1]: http://stackoverflow.com/q/24619300/3538165
    # [2]: https://dzone.com/articles/clojurejava-prevent-exceptions
    export LEIN_JVM_OPTS='-Dapple.awt.UIElement=true -XX:-OmitStackTraceInFastThrow'
fi

################################################################################
#### Vim

if (( $+commands[nvim] )); then
    alias v='nvim'
elif (( $+commands[vim] )); then
    alias v='vim'
elif (( $+commands[vi] )); then
    alias v='vi'
fi

################################################################################
#### Fasd

# Turn off case sensitivity permanently in Fasd. This functionality is
# only available in my fork of Fasd.
if (( $+commands[fasd] )); then
    export _FASD_NOCASE=1
fi

################################################################################
#### Run post-init hook for local configuration

if typeset -f radian_post_init_hook > /dev/null; then
    radian_post_init_hook
fi

## Preliminary configuration
### Radian repository

# Set $RADIAN to the location of the Radian repository, if found.
if [[ -L $0 && -d ${0:A:h}/radian-emacs ]]; then
    export RADIAN=${0:A:h}
else
    unset RADIAN
fi

### zplug configuration

# Identify the location of zplug. Below is the location to which zplug
# is installed by Homebrew on macOS.
export ZPLUG_HOME=/usr/local/opt/zplug

### Plugin configuration

# Have wdx generate a function by the name 'wd' instead of 'wdx'.
export WDX_NAME=wd

### Plugin list

RADIAN_PLUGINS=(
    # Quickly jump to directories.
    "raxod502/wdx"
    # Display autosuggestions from history.
    "zsh-users/zsh-autosuggestions"
    # Completion definitions for lots of additional commands.
    "zsh-users/zsh-completions"
)

# Usage: radian_add_plugin <zplug-args>
#
# Add a plugin to $RADIAN_PLUGINS. Word splitting will be performed on
# zplug-args to determine the arguments that will be passed to zplug.
function radian_add_plugin {
    emulate -LR zsh
    if ! (( ${RADIAN_PLUGINS[(I)$1]} )); then
        RADIAN_PLUGINS+=($1)
    fi
}

# Usage: radian_remove_plugin <zplug-args>
#
# Remove a plugin from $RADIAN_PLUGINS by name. The name should be
# exactly the same as it appears in $plugins, with spaces if
# necessary.
function radian_remove_plugin {
    emulate -LR zsh
    RADIAN_PLUGINS=("${(@)RADIAN_PLUGINS:#$1}")
}

## External configuration
### ~/.zshrc.local

if [[ -f ~/.zshrc.local ]]; then
    . ~/.zshrc.local
fi

### ~/.profile

if [[ -f ~/.profile ]]; then
    . ~/.profile
fi

## zplug

if [[ -f $ZPLUG_HOME/init.zsh ]]; then
    . $ZPLUG_HOME/init.zsh

    for plugin in $RADIAN_PLUGINS; do
        zplug $=plugin
    done

    if ! zplug check; then
        zplug install
    fi

    zplug load
fi

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

# Change the color and then display the working directory.
radian_prompt_prefix='%(?.%{$fg[blue]%}.%{$fg[red]%})%c'

# Change the color and then display a '%' or '#', then reset the color
# for the user's input.
radian_prompt_suffix='%(?.%{$fg[blue]%}.%{$fg[red]%}) %# %{$reset_color%}'

if (( $+commands[git] )); then

    # Usage: radian_prompt_git_dirty
    #
    # Print an asterisk if the working directory is dirty.
    function radian_prompt_git_dirty {
        emulate -LR zsh
        local FLAGS
        FLAGS=('--porcelain' '--ignore-submodules=dirty')
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
    radian_prompt_git_info='%{$reset_color%}$(radian_prompt_git_info)'

    # The actual prompt.
    PROMPT="${radian_prompt_prefix}${radian_prompt_git_info}${radian_prompt_suffix}"

else

    PROMPT="${radian_prompt_prefix}${radian_prompt_suffix}"

fi

### Command line

# When no arguments are provided to "." or "source", they default to
# sourcing .zshrc. Based on [1], thanks @PythonNut!
#
# [1]: http://unix.stackexchange.com/a/326948/176805
function _accept-line {
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

# Make bracketed paste slightly smarter. This causes url-quote-magic
# below to work correctly.
autoload -Uz bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-magic

# Automatically escape URLs.
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

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

#### dirs

# This alias is a convenient way to list the last few directories
# visited, with their numbers. You can then use the 'cd -n' aliases to
# jump to those directories.
alias ds='dirs -v | head -10'

#### ls, exa

if (( $+commands[exa] )); then
    alias l='exa --all --header --long --color-scale'
    alias lg='exa --all --grid --header --long --color-scale'
    alias lt='exa --all --header --long --tree --color-scale --ignore-glob .git'
    function lti {
        emulate -LR zsh
        exa --all --header --long --tree --color-scale --ignore-glob ".git|$1" ${@:2}
    }
    alias ltl='exa --all --header --long --tree --color-scale --ignore-glob .git --level'
    function ltli {
        emulate -LR zsh
        exa --all --header --long --tree --color-scale --level $1 --ignore-glob ".git|$2" ${@:3}
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
        alias lt=tree
        alias ltl='tree -L'
    fi
fi

#### wdx

if command -v $WDX_NAME &>/dev/null; then
    alias ws="$WDX_NAME set"
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

#### mkdir

alias md='mkdir -p'

function mcd {
    emulate -LR zsh
    mkdir -p $@
    cd ${@[$#]}
}

#### rmdir

alias rd='rmdir'

### Help system

alias help=run-help

### Utilities
#### Emacs

if (( $+commands[emacs] )); then
    alias e='emacs -nw'
    alias eq='emacs -nw -Q'
    alias ew='emacs'
    alias eqw='emacs -Q'
fi

if (( $+commands[emacsclient] )); then
    alias ec='emacsclient --alternate-editor= -nw'
    alias ecw='emacsclient --alternate-editor='
fi

#### Git

if (( $+commands[git] )); then
    alias g=git

    alias gh='git help'

    alias gi='git init'

    alias gst='git status'

    alias gsh='git show'
    alias gshs='git show --stat'

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
                    alias="gl${all}${oneline}${diff}${search}="
                    alias+="git log --graph --decorate${all_flags}"
                    alias+="${oneline_flags}${diff_flags}${search_flags}"
                    alias $alias
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

    alias glsf='git ls-files'

    alias gx='git clean'
    alias gxf='git clean -fd'

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
    alias gsmui='git submodule update --init --recursive'
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
    alias gpa='git push --all'
    alias gpf='git push --force-with-lease'
    alias gpff='git push --force'
    alias gpu='git push --set-upstream'
    alias gpd='git push --delete'
fi

#### Hub

if (( $+commands[hub] )); then
    alias hcl='hub clone --recursive'
    alias hc='hub create --copy'
    alias hcp='hub create -p --copy'
    alias hf='hub fork'
    alias hp='hub pull-request --copy'
    alias hb='hub browse'
    alias hh='hub help'
    alias hi='hub issue'
fi

#### Tmux

if (( $+commands[tmux] )); then
    alias ta='tmux attach'
    alias ts='tmux new-session -s'
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
### Leiningen

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

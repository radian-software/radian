# Enable parameter expansion and other substitutions in the $PROMPT.
setopt promptsubst

# Here we define a prompt that displays the current directory and git
# branch, and turns red on a nonzero exit code. Adapted heavily from
# [1], with supporting functions extracted from Oh My Zsh [2] so that
# we don't have to load the latter as a dependency.
#
# [1]: https://github.com/robbyrussell/oh-my-zsh/blob/master/themes/mgutz.zsh-theme
# [2]: https://github.com/robbyrussell/oh-my-zsh/blob/3705d47bb3f3229234cba992320eadc97a221caf/lib/git.zsh

# Function that compares the provided version of git to the version
# installed and on path Outputs -1, 0, or 1 if the installed version
# is less than, equal to, or greater than the input version,
# respectively.
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
    FLAGS=('--porcelain')
    if [[ $POST_1_7_2_GIT -gt 0 ]]; then
        FLAGS+='--ignore-submodules=dirty'
    fi
    if [[ $RADIAN_PROMPT_IGNORE_UNTRACKED_FILES == true ]]; then
        FLAGS+='--untracked-files=no'
    fi
    if [[ $(command git status ${FLAGS} 2> /dev/null | tail -n1) ]]; then
        echo "*"
    fi
}

# Define the actual prompt format.
PROMPT='%(?.%{$fg[blue]%}.%{$fg[red]%})%c%{$reset_color%}$(radian_prompt_git_info)%(?.%{$fg[blue]%}.%{$fg[red]%}) %# %{$reset_color%}'

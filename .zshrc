### Antigen ###

source ~/.antigen-repo/antigen.zsh

# Just to be safe: The core oh-my-zsh library is necessary for a
# number of oh-my-zsh plugins and themes to work correctly.
antigen use oh-my-zsh

# Efficient filesystem navigation (passive), jump with 'j'.
antigen bundle autojump

# Completion for 'brew' command.
antigen bundle brew

# Highlight important things on man pages.
antigen bundle colored-man-pages

# Completion for 'lein' command.
antigen bundle lein

# Aliases for interop with OSX and iTunes.
antigen bundle osx

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

# Better prompt (like oh-my-zsh/mgutz, but turns red on nonzero exit code).
PROMPT='%(?.%{$fg[blue]%}.%{$fg[red]%})%c%{$reset_color%}$(git_prompt_info)%(?.%{$fg[blue]%}.%{$fg[red]%}) %# %{$reset_color%}'
ZSH_THEME_GIT_PROMPT_PREFIX="["
ZSH_THEME_GIT_PROMPT_SUFFIX=
ZSH_THEME_GIT_PROMPT_DIRTY="*]"
ZSH_THEME_GIT_PROMPT_CLEAN="]"

### Additional rc-files ###

if [[ -f ~/.zshrc.aliases ]]; then
    source ~/.zshrc.aliases
fi

if [[ -f ~/.zshrc.local ]]; then
    source ~/.zshrc.local
fi

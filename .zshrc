### Zsh ###

# No (practical) limit to many commands are kept in history.
HISTSIZE=1000000 # session history
SAVEHIST=1000000 # saved history

# Better help command (like man, but finds more and better results).
unalias run-help &>/dev/null
autoload run-help

### Antigen ###

source ~/.antigen-repo/antigen.zsh

# The core oh-my-zsh library is necessary for a number of oh-my-zsh
# plugins and themes.
antigen use oh-my-zsh

# Theme with reasonable colors and a prompt like:
# dotfiles[master*] %
antigen theme mgutz

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

# Syntax highlighting for entered commands.
antigen bundle zsh-users/zsh-syntax-highlighting

# Apply changes to theme and bundles.
antigen apply

### Additional rc-files ###

if [[ -f ~/.zshrc.aliases ]]; then
    source ~/.zshrc.aliases
fi

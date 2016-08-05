### Zsh ###

# No (practical) limit to many commands are kept in history.
HISTSIZE=1000000 # session history
SAVEHIST=1000000 # saved history

# Better help command (like man, but finds more and better results)
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

# Display completion suggestions based on past commands
antigen bundle zsh-users/zsh-autosuggestions

# Apply changes to theme and bundles
antigen apply

### Additional rc-files ###

if [[ -f ~/.zshrc.aliases ]]; then
    source ~/.zshrc.aliases
fi

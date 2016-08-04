### Oh My Zsh ###

export ZSH=~/.oh-my-zsh

# Theme with reasonable colors and a prompt like:
# dotfiles[master*] %
ZSH_THEME="mgutz"

plugins=(
    zsh-autosuggestions # displays completion suggestions based on past commands
)

source "$ZSH/oh-my-zsh.sh"

### Zsh ###

# No (practical) limit to many commands are kept in history.
HISTSIZE=1000000 # session history
SAVEHIST=1000000 # saved history

# Better help command (like man, but finds more and better results)
unalias run-help &>/dev/null
autoload run-help

### Additional rc-files ###

if [[ -f ~/.zshrc.aliases ]]; then
    source ~/.zshrc.aliases
fi

# Never discard history within a session, or at least not before any
# reasonable amount of time.
export HISTSIZE=1000000

# Save history to disk. The value of this option is the default
# installed by zsh-newuser-install.
export HISTFILE=~/.zsh_history

# Never discard history in the file on disk, either.
export SAVEHIST=1000000

# Don't save commands to the history if they start with a leading
# space.
setopt histignorespace

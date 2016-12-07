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
man() {
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

# When no arguments are provided to "." or "source", they default to
# sourcing .zshrc. Based on [1], thanks @PythonNut!
#
# [1]: http://unix.stackexchange.com/a/326948/176805
if [[ $RADIAN_CUSTOMIZE_MAGIC_DOT != false ]]; then
    function _accept-line() {
        if [[ $BUFFER == "." ]]; then
            BUFFER=". ~/.zshrc"
        elif [[ $BUFFER == "source" ]]; then
            BUFFER="source ~/.zshrc"
        fi
        zle .accept-line
    }
    zle -N accept-line _accept-line
fi

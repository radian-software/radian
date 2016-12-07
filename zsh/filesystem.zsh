alias l=ls
alias ls='ls -lah'

# FIXME: https://github.com/robbyrussell/oh-my-zsh/blob/master/lib/directories.zsh

copy() {
    RADIAN_COPY_TARGETS=()
    for target; do
        if [[ $target == /* ]]; then
            RADIAN_COPY_TARGETS+=($target)
        else
            RADIAN_COPY_TARGETS+=($PWD/$target)
        fi
    done
}
paste() {
    cp -R $RADIAN_COPY_TARGETS ${1:-.}
}
move() {
    mv $RADIAN_COPY_TARGETS ${1:-.}
}
pasteln() {
    ln -s $RADIAN_COPY_TARGETS ${1:-.}
}

delink() {
    if [[ -z $1 ]]; then
        echo "usage: delink <symlinks>"
        return 1
    fi
    for link; do
        if [[ -L $link ]]; then
            if [[ -e $link ]]; then
                target=$(grealpath $link)
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

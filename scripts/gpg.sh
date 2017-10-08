gpg_connect() {
    if [ -f "$HOME/.gnupg/.gpg-agent-info" ]; then
        eval "$(cat "$HOME/.gnupg/.gpg-agent-info")"
        eval "$(cut -d= -f 1 < "$HOME/.gnupg/.gpg-agent-info" | xargs echo export)"
    fi
}

gpg_connected() {
    gpg-agent >/dev/null 2>&1
}

gpg_forget() {
    pgrep gpg-agent | while read -r pid; do
        kill -HUP "$pid"
    done
}

gpg_restart() {
    pkill gpg-agent
    eval "$(gpg-agent --daemon --write-env-file "$HOME/.gnupg/.gpg-agent-info")"
}

gpg_connect
if ! gpg_connected; then
    gpg_restart
fi

export GPG_TTY="$TTY"

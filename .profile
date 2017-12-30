## gpg-agent

if command -v gpg-agent >/dev/null 2>&1; then

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
        mkdir -p "$HOME/.gnupg"
        eval "$(gpg-agent --daemon --write-env-file "$HOME/.gnupg/.gpg-agent-info")"
    }

    gpg_connect
    if ! gpg_connected; then
        gpg_restart
    fi

    export GPG_TTY="$TTY"

fi

## ssh-agent

if command -v ssh-agent >/dev/null 2>&1; then

    ssh_connect() {
        if [ -f "$HOME/.ssh/agent-info" ]; then
            eval "$(cat "$HOME/.ssh/agent-info")" >/dev/null
        fi
    }

    ssh_connected() {
        ps -p "$SSH_AGENT_PID" >/dev/null 2>&1
    }

    ssh_forget() {
        ssh-add -D
    }

    ssh_restart() {
        pkill ssh-agent
        mkdir -p "$HOME/.ssh"
        ssh-agent -t 86400 > "$HOME/.ssh/agent-info"
        ssh_connect
    }

    ssh_connect
    if ! ssh_connected; then
        ssh_restart
    fi

fi

## External configuration
### ~/.profile.local

if [ -f ~/.profile.local ]; then
    . ~/.profile.local
fi

## ssh-agent

if command -v ssh-agent >/dev/null 2>&1; then

    ssh_connect() {
        if [ -n "$HOME" ] && [ -f "$HOME/.ssh/agent-info" ]; then
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
        if [ -n "$HOME" ]; then
            pkill ssh-agent
            mkdir -p "$HOME/.ssh"
            ssh-agent -t 86400 > "$HOME/.ssh/agent-info"
            ssh_connect
        fi
    }

    ssh_connect
    if ! ssh_connected; then
        ssh_restart
    fi

fi

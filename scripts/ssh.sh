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
    ssh-agent -t 86400 > "$HOME/.ssh/agent-info"
    ssh_connect
}

ssh_connect
if ! ssh_connected; then
    ssh_restart
fi

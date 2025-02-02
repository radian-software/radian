## PATH setup

case "$OSTYPE" in
    darwin*)
        export PATH=
        eval "$(/usr/libexec/path_helper -s)"
        ;;
esac

if [ -f /etc/profile ]; then
    . /etc/profile
fi

## External configuration
### ~/.profile.local

if [ -f ~/.profile.local ]; then
    . ~/.profile.local
fi

## gpg-agent

if command -v gpg-connect-agent >/dev/null 2>&1; then

    gpg_restart() {
        gpg-connect-agent reloadagent /bye
    }

    gpg_forget() {
        gpg-connect-agent reloadagent /bye
    }

fi

## ssh-agent

if command -v ssh-agent >/dev/null 2>&1; then

    ssh_connect() {
        if [ -n "$HOME" ] && [ -f "$HOME/.ssh/agent-info" ]; then
            eval "$(cat "$HOME/.ssh/agent-info")" >/dev/null
        fi
    }

    ssh_connected() {
        ps -p "$SSH_AGENT_PID" 2>&1 | grep -qF ssh-agent
    }

    ssh_forget() {
        ssh-add -D
    }

    ssh_restart() {
        if [ -n "$HOME" ]; then
            pkill -U "$USER" ssh-agent
            mkdir -p "$HOME/.ssh"
            ssh-agent ${SSH_AGENT_ARGS:--t 86400} > "$HOME/.ssh/agent-info"
            ssh_connect
        fi
    }

    ssh_connect
    if ! ssh_connected; then
        ssh_restart
    fi

fi

export NPM_CONFIG_UPDATE_NOTIFIER=false
export PIP_DISABLE_PIP_VERSION_CHECK=1

if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    ssh-agent > ~/.ssh-agent
fi
if [ -z "$SSH_AGENT_PID" ]; then
  eval "$(<~/.ssh-agent)" > /dev/null
fi

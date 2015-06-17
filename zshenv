# Disable global RCs
unsetopt GLOBAL_RCS

# Source /etc/profile anyway
if test -f /etc/profile; then
  emulate sh -c 'source /etc/profile'
fi

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ "$SHLVL" -eq 1 && ! -o LOGIN && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi

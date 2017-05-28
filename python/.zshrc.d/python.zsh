export PYTHONSTARTUP="$HOME/.pythonrc.py"
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

if [ ! -d "$PYENV_ROOT" ]; then
  git clone https://github.com/pyenv/pyenv.git "$PYENV_ROOT"
  git clone https://github.com/pyenv/pyenv-virtualenv.git "$PYENV_ROOT/plugins/pyenv-virtualenv"
fi

 eval "$(pyenv init - zsh)"

export PYTHONSTARTUP="${HOME}/.pythonrc.py"
export PYENV_ROOT="${ZIT_MODULES_PATH}/pyenv"

path=(
  ${PYENV_ROOT}/bin
  ${path}
)

zit-in "https://github.com/pyenv/pyenv.git" "pyenv"
zit-in "https://github.com/pyenv/pyenv-virtualenv.git" "pyenv/plugins/pyenv-virtualenv"

eval "$(pyenv init - zsh)"

# startup file read in interactive login shells
#
# The following code helps us by optimizing the existing framework.
# This includes zcompile, zcompdump, etc.
#

(
  # Function to determine the need of a zcompile. If the .zwc file
  # does not exist, or the base file is newer, we need to compile.
  # These jobs are asynchronous, and will not impact the interactive shell
  zcompare() {
    if [[ -s ${1} && ( ! -s ${1}.zwc || ${1} -nt ${1}.zwc) ]]; then
      zcompile ${1}
    fi
  }

  setopt EXTENDED_GLOB

  # zcompile the completion cache; siginificant speedup.
  for file in ${ZDOTDIR:-${HOME}}/.zcomp^(*.zwc)(.); do
    zcompare ${file}
  done

  # zcompile .zshrc
  zcompare ${ZDOTDIR:-${HOME}}/.zshrc

  # compile .zit files
  for file in ${HOME}/.zit.d/**/*.zsh; do
    zcompare ${file}
  done

  # compile .zshrc.d
  for file in ${HOME}/.zshrc.d/*.zsh; do
    zcompare ${file}
  done

) &!

# Source a zlogin file depending of the current TTY
# Example: /home/m45t3r/.tty1_zlogin
if [ -z "${DISPLAY}" ]; then
  TTY_LOGIN="${HOME}/.tty${XDG_VTNR}_login.zsh"
  [ -f "${TTY_LOGIN}" ] && source "${TTY_LOGIN}"
fi

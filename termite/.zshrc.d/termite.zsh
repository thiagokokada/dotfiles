# allow termite to open new tabs on current directory
if [[ "${TERM}" == "xterm-termite" ]]; then
  source /etc/profile.d/vte.sh
  __vte_osc7
fi

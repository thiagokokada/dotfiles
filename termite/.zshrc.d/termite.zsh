# allow termite to open new tabs on current directory
if [[ "${TERM}" == "xterm-termite" ]]; then
 [[ -f "/etc/profile.d/vte.sh" ]] && source /etc/profile.d/vte.sh
fi

# fzf
FZF_KEY_BINDINGS="/usr/share/fzf/key-bindings.zsh"
if [[ -s "${FZF_KEY_BINDINGS}" ]]; then
  source "${FZF_KEY_BINDINGS}"
fi

UPGRADE_CMDS+="yay && try-run flatpak update"

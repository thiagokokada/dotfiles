# Execute code that does not affect the current session in the background.
{
  # Compile the completion dump to increase startup speed.
  zcompdump="${ZDOTDIR:-$HOME}/.zcompdump"
  if [[ -s "$zcompdump" && (! -s "${zcompdump}.zwc" || "$zcompdump" -nt "${zcompdump}.zwc") ]]; then
    zcompile "$zcompdump"
  fi
} &!

# Auto start X11
if [ -z "$DISPLAY" ] && [ "$(fgconsole)" -eq 1 ] && [ -f "$HOME/.xinitrc" ]; then
  exec startx
fi

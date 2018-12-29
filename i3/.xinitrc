#!/bin/sh

userresources="${HOME}/.Xresources"
usermodmap="${HOME}/.Xmodmap"
sysresources="/etc/X11/xinit/.Xresources"
sysmodmap="/etc/X11/xinit/.Xmodmap"
xinitrcd="/etc/X11/xinit/xinitrc.d/"
xinitrc_local="${HOME}/.xinitrc.local"

# merge in defaults and keymaps
for file in "${sysresources}" "${userresources}"; do
  xrdb -merge "${file}"
done

for file in "${sysmodmap}" "${usermodmap}"; do
  xmodmap "${file}"
done

# start some nice programs
if [ -d "${xinitrcd}" ] ; then
 for f in "${xinitrcd}"?*.sh ; do
  [ -x "${f}" ] && . "${f}"
 done
 unset f
fi

if [ -f "${xinitrc_local}" ]; then
  . "${xinitrc_local}"
fi

exec "${@}"

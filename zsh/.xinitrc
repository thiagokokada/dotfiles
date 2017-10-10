#!/bin/sh

userresources="${HOME}/.Xresources"
usermodmap="${HOME}/.Xmodmap"
sysresources="/etc/X11/xinit/.Xresources"
sysmodmap="/etc/X11/xinit/.Xmodmap"
xinitrcd="/etc/X11/xinit/xinitrc.d/"

# merge in defaults and keymaps
if [ -f "${sysresources}" ]; then
    xrdb -merge "${sysresources}"
fi

if [ -f "${sysmodmap}" ]; then
    xmodmap "${sysmodmap}"
fi

if [ -f "${userresources}" ]; then
    xrdb -merge "${userresources}"
fi

if [ -f "${usermodmap}" ]; then
    xmodmap "${usermodmap}"
fi

# start some nice programs
if [ -d "${xinitrcd}" ] ; then
 for f in "${xinitrcd}"?*.sh ; do
  [ -x "${f}" ] && . "${f}"
 done
 unset f
fi

exec ${@}

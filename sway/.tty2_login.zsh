# Enable integration between GTK2 and Qt5
export QT_QPA_PLATFORMTHEME=gtk2

# Keyboard layout config
export XKB_DEFAULT_LAYOUT=br,us
export XKB_DEFAULT_VARIANT=abnt2,intl
export XKB_DEFAULT_OPTIONS=grp:alt_shift_toggle,

tty="${XDG_VTNR}"
tmpfile=$(mktemp --tmpdir "tty${tty}_login.XXXXXXXX.log")

sway &>! "${tmpfile}"

logout

# Enable integration between GTK2 and Qt5
export QT_QPA_PLATFORMTHEME=gtk2

exec xinit i3 -- ":${XDG_VTNR}" "vt${XDG_VTNR}" -keeptty

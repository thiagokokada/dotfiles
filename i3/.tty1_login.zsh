# Enable integration between GTK2 and Qt5
export QT_QPA_PLATFORMTHEME=gtk2

# Fix "`+c" => ç instead of ć
export QT_IM_MODULE=cedilla
export GTK_IM_MODULE=cedilla

runx i3

#!/bin/sh

TEMP_FILE=$(mktemp --suffix '.png')
RESOLUTION=$(xdpyinfo | awk '/dimensions/{print $2}')
OLD_STANDBY=$(xset q | awk 'BEGIN {FPAT="[0-9]+"} /Standby/{print $1}')
NEW_STANDBY=5

clean_up() {
    rm -f "${TEMP_FILE}"
    xset dpms "${OLD_STANDBY}"
}

trap clean_up HUP INT TERM EXIT

ffmpeg -loglevel quiet -y -s "${RESOLUTION}" -f x11grab -i "${DISPLAY}" -vframes 1 -vf 'gblur=sigma=8' "${TEMP_FILE}"
xset +dpms dpms "${NEW_STANDBY}"
i3lock -nei "${TEMP_FILE}"

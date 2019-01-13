#!/bin/sh

TEMP_FILE=$(mktemp --suffix '.png')
OLD_STANDBY=$(xset q | awk 'BEGIN {FPAT="[0-9]+"} /Standby/{print $1}')
NEW_STANDBY=5
PLAY=0

pause() {
    status="$(playerctl status)"
    playerctl -a pause 2> /dev/null

    if [ "${status}" = "Playing" ]; then
        PLAY=1
    fi
}

resume() {
    if [ "${PLAY}" -eq 1 ]; then
        playerctl play
    fi
}

take_screenshot() {
    resolution=$(xdpyinfo | awk '/dimensions/{print $2}')
    ffmpeg -loglevel quiet -y -s "${resolution}" -f x11grab -i "${DISPLAY}" -vframes 1 -vf 'gblur=sigma=8' "${TEMP_FILE}"
}

prepare() {
    take_screenshot
    xset +dpms dpms "${NEW_STANDBY}"
    dunstify "DUNST_COMMAND_PAUSE"
    pause
}

clean_up() {
    rm -f "${TEMP_FILE}"
    xset dpms "${OLD_STANDBY}"
    dunstify "DUNST_COMMAND_RESUME"
    resume
}

trap clean_up HUP INT TERM EXIT

prepare
i3lock -nei "${TEMP_FILE}"

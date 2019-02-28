#!/usr/bin/env bash

TEMP_FILE=$(mktemp --suffix '.png')
OLD_STANDBY=$(xset q | awk 'BEGIN {FPAT="[0-9]+"} /Standby/{print $1}')
NEW_STANDBY=5
RESUME_PLAYING=0
I3LOCK_OPTIONS=(-e -i "${TEMP_FILE}")

pause_player() {
    local status
    status="$(playerctl status)"

    playerctl -a pause 2> /dev/null

    if [[ "${status}" = "Playing" ]]; then
        RESUME_PLAYING=1
    fi
}

resume_player() {
    if [[ "${RESUME_PLAYING}" -eq 1 ]]; then
        playerctl play
    fi
}

take_screenshot() {
    local resolution
    resolution=$(xdpyinfo | awk '/dimensions/{print $2}')

    ffmpeg -loglevel quiet -y \
        -s "${resolution}" -f x11grab -i "${DISPLAY}" -vframes 1 \
        -vf 'gblur=sigma=8' "${TEMP_FILE}"
}

pre_lock() {
    take_screenshot
    xset +dpms dpms "${NEW_STANDBY}"
    pause_player
}

post_lock() {
    rm -f "${TEMP_FILE}"
    xset dpms "${OLD_STANDBY}"
    resume_player
}

trap post_lock HUP INT TERM EXIT

pre_lock

if [[ -e "/dev/fd/${XSS_SLEEP_LOCK_FD:--1}" ]]; then
    # we have to make sure the locker does not inherit a copy of the lock fd
    i3lock "${I3LOCK_OPTIONS[@]}" {XSS_SLEEP_LOCK_FD}<&-

    # now close our fd (only remaining copy) to indicate we're ready to sleep
    exec {XSS_SLEEP_LOCK_FD}<&-
else
    i3lock -n "${I3LOCK_OPTIONS[@]}"
fi

post_lock

#!/bin/sh

TEMP_FILE=`mktemp --suffix '.png'`
RESOLUTION=`xdpyinfo | awk '/dimensions/{print $2}'`
DPMS_VALUES=`xset q | awk 'BEGIN{FPAT="[0-9]+"} /Standby.*Suspend.*Off/{print $1, $2, $3}'`
SCREEN_TIMEOUT=5

clean_up() {
	rm -f "${TEMP_FILE}"
	xset dpms ${DPMS_VALUES}
}

trap clean_up SIGHUP SIGINT SIGTERM

ffmpeg -loglevel quiet -y -s "${RESOLUTION}" -f x11grab -i "${DISPLAY}" -vframes 1 -vf 'gblur=sigma=8:steps=3' "${TEMP_FILE}"
xset +dpms dpms "${SCREEN_TIMEOUT}" "${SCREEN_TIMEOUT}" "${SCREEN_TIMEOUT}"
i3lock -I "${SCREEN_TIMEOUT}" -nei "${TEMP_FILE}"
clean_up

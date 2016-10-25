#!/bin/sh

TEMP_FILE=/tmp/lock_screen.png
DPMS_VALUES=`xset q | awk 'BEGIN{FPAT="[0-9]+"} /Standby/{print $1, $2, $3}'`

clean_up() {
	rm -f $TEMP_FILE
	xset dpms ${DPMS_VALUES}
}

trap clean_up SIGHUP SIGINT SIGTERM

scrot "${TEMP_FILE}"
#convert "${TEMP_FILE}" -blur 0x8 "${TEMP_FILE}"
ffmpeg -loglevel quiet -y -i "${TEMP_FILE}" -vf "gblur=sigma=8:steps=2" "${TEMP_FILE}"
xset +dpms dpms 5 5 5
i3lock -I 5 -nei "${TEMP_FILE}"
clean_up

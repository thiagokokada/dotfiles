#!/bin/sh

TEMP_FILE=/tmp/lock_screen.png

clean_up() {
	rm -f $TEMP_FILE
	xset dpms 0 0 0
}

trap clean_up SIGHUP SIGINT SIGTERM

scrot "${TEMP_FILE}"
convert "${TEMP_FILE}" -blur 0x5 "${TEMP_FILE}"
xset +dpms dpms 5 5 5
i3lock -I 5 -nei "${TEMP_FILE}"
clean_up

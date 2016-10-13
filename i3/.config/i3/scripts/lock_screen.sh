#!/bin/sh

TEMP_FILE='/tmp/lockscreen.png'

trap "rm -f "$TEMP_FILE"; exit" SIGHUP SIGINT SIGTERM

scrot "$TEMP_FILE"
convert "$TEMP_FILE" -scale 10% -scale 1000% "$TEMP_FILE"
i3lock -I 5 -eti "$TEMP_FILE"

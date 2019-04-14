#!/bin/sh

WIN_LIST=$(wmctrl -lp)
WIN_PIDS=$(echo "${WIN_LIST}" | awk '{ print $3 }' | paste -s -d, -)

echo "${WIN_LIST}" | awk '{ print $1 }' | xargs -P0 -L1 wmctrl -ic

(i3-nagbar -m "Waiting all programs to exit..." -B "Finish them!" "pkill -u ${EUID}") &

while true; do
    ps -p "${WIN_PIDS}" || break
    sleep 0.1
done

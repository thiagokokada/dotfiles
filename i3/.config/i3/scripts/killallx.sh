#!/bin/sh

WIN_LIST=$(wmctrl -lp)
WIN_PIDS=$(echo "${WIN_LIST}" | awk '{ print $3 }' | sort -u | paste -s -d' ' -)

echo "${WIN_LIST}" | awk '{ print $1 }' | xargs -P0 -L1 wmctrl -ic

OPEN_PROGRAMS=$(ps -o comm= -p ${WIN_PIDS} | paste -s -d',' -)

(i3-nagbar -m "Waiting programs to exit: ${OPEN_PROGRAMS}" \
    -b "Kill them!" "kill -9 ${WIN_PIDS}" \
    -B "Finish them!" "kill ${WIN_PIDS}") &

while true; do
    ps -p ${WIN_PIDS} >/dev/null || break
    sleep 0.1
done

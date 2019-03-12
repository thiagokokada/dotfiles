#!/bin/sh

PIDS=$(wmctrl -lp | awk '{ print $3 }' | sort -u)

i3-msg '[class=".*"] kill'

while true; do
    ALL_KILLED=1

    for pid in ${PIDS}; do
        kill -0 "${pid}" && ALL_KILLED=0
    done

    [ "${ALL_KILLED}" -eq 1 ] && break

    sleep 0.1
done

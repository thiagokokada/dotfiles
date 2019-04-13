#!/usr/bin/env bash

OPEN_WINDOWS=$(wmctrl -lp)

echo ${OPEN_WINDOWS} | awk '{ print $1 }' | xargs -P0 -L1 wmctrl -ic

for _ in {1..50}; do
    ps -p $(echo ${OPEN_WINDOWS} | awk '{ print $3 }' | paste -s -d, -)
    if [[ ${?} != 0 ]]; then
        break
    fi
    sleep 0.1
done

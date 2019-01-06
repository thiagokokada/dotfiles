#!/bin/sh

wmctrl -l | awk '{ print substr($0, index($0, $4)) }' | xargs -P0 -L1 wmctrl -c
sleep 0.5

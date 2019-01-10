#!/bin/sh

wmctrl -l | awk '{ print $1 }' | xargs -P0 -L1 wmctrl -i -c
sleep 1

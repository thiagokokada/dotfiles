#!/bin/sh

wmctrl -l | awk '{ print $1 }' | xargs -P0 -L1 wmctrl -ci
sleep 0.5

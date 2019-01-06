#!/bin/sh

wmctrl -l | awk '{ print $1 }' | xargs -P0 -L1 wmctrl -ci
exit 0

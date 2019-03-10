#!/bin/sh

i3-msg '[class=".*"] kill'

while [ "$(wmctrl -l | wc -c)" -ne 0 ]; do
    sleep 0.1
done

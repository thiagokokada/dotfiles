#!/bin/sh

wmctrl -l | awk '{ print substr($0, index($0, $4)) }' | xargs -P"$(nproc)" -L1 wmctrl -c
exit 0

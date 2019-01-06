#!/bin/sh

wmctrl -l | awk '{ print $1 }' | xargs -P"$(nproc)" -L1 wmctrl -ci
exit 0

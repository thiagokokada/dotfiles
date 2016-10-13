#!/bin/sh
#
# rotate_desktop.sh
#
# Rotates modern Linux desktop screen and input devices to match. Handy for
# convertible notebooks. Call this script from panel launchers, keyboard
# shortcuts, or touch gesture bindings (xSwipe, touchegg, etc.).
#
# Using transformation matrix bits taken from:
#   https://wiki.ubuntu.com/X/InputCoordinateTransformation
#

# Configure these to match your hardware (names taken from `xrandr` and
# `xinput` output).
XDISPLAY='LVDS1'
TOUCHPAD='ETPS/2 Elantech Touchpad'
TOUCHSCREEN='Atmel Atmel maXTouch Digitizer'

if [ -z "${1-}" ]; then
  echo "Missing orientation."
  echo "Usage: ${0##*/} [normal|inverted|left|right]"
  exit 1
fi

do_rotate()
{
  # Function parameters
  local output=$1
  local rotation=$2

  # Set constants to be used afterwards
  local readonly transform="Coordinate Transformation Matrix"
  local readonly normal="1 0 0 0 1 0 0 0 1"
  local readonly inverted="-1 0 1 0 -1 1 0 0 1"
  local readonly left="0 -1 1 1 0 0 0 0 1"
  local readonly right="0 1 0 -1 0 1 0 0 1"

  xrandr --output "$output" --rotate "$rotation"

  # After rotation, some xinput devices may disappear for a moment until
  # display is available again (seems to be true to some touchscreens at
  # least). So we try some times before giving up.
  for device in "$TOUCHPAD" "$TOUCHSCREEN"; do
    local i=1
    local tries=5
    until xinput set-prop "$device" "$transform" ${!rotation} \
      || (( i++ >= $tries )); do
      sleep 0.5
    done
    if [ $i -gt $tries ]; then
      echo "Tried $tries times to set $rotation in $device. Giving up." 1>&2
    fi
  done
}

do_rotate $XDISPLAY $1

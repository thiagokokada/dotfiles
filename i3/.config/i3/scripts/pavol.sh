#!/bin/sh

ACTIVESYNC=$(pacmd list-sinks | awk '/*/{print $3}')
PERCENT="${2:-5}"

usage() {
    echo 'Usage: pavol [OPTION] [PERCENT]

Find default PulseAudio sink, and raise/lower/mute it.

raise   Raise volume in default sink by PERCENT
        Defaults to 5%
lower   Lower volume in default sink by PERCENT
        Defaults to 5%
mute    Mute default sink
'
}

case "$1" in
    raise)
        pactl set-sink-volume "${ACTIVESYNC}" +"${PERCENT}"%
        ;;
    lower)
        pactl set-sink-volume "${ACTIVESYNC}" -"${PERCENT}"%
        ;;
    mute)
        pactl set-sink-mute "${ACTIVESYNC}" toggle
        ;;
    *)
        usage
        ;;
esac

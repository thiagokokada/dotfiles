#!/bin/sh

ACTIVESYNC=$(pacmd list-sinks | grep \* | awk '{print $3}')

usage() {
    echo 'Usage: pavol [OPTION]

Find default PulseAudio sink, and raise/lower/mute it.

raise   Raise volume in default sink by 5%
lower   Lower volume in default sink by 5%
mute    Mute default sink
'
}

case "$1" in
    raise)
        pactl set-sink-volume "$ACTIVESYNC" +5%
        ;;
    lower)
        pactl set-sink-volume "$ACTIVESYNC" -5%
        ;;
    mute)
        pactl set-sink-mute "$ACTIVESYNC" toggle
        ;;
    *)
        usage
        ;;
 esac

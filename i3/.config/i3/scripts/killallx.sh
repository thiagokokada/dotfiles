#!/bin/sh

kill_loop() {
    # Run the kill loop for 10*0.5s=5s
    for _ in $(seq 1 10); do
        # In wmctrl, -i argument must be set separate from -c or strange things
        # will happen, at least in version 1.07
        if wmctrl -l | awk '{ print $1 }' | xargs -P0 -L1 wmctrl -i -c; then
            break
        fi
        sleep 0.5
    done
}

kill_loop

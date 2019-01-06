#!/bin/sh

kill_loop() {
    # Run the kill loop for 50*0.1s=5s
    for _ in $(seq 1 50); do
        if wmctrl -l | awk '{ print $1 }' | xargs -P0 -L1 wmctrl -ci; then
            break
        fi
        sleep 0.1
    done
}

kill_loop

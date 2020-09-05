#!/bin/sh

cd $(dirname "${0}")
nix-shell --run "i3pyblocks -c config.py" 2>/dev/null

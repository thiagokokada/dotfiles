#!/bin/sh

cd $(dirname "${0}")
nix-shell --run "python3 config.py" 2>/dev/null

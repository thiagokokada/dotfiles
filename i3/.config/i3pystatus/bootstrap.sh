#!/bin/sh

# Run this script to boostrap i3pystatus virtual environment

set -e

GI_PATH="/usr/lib/python3*/site-packages/gi"

activate() {
	source "venv/bin/activate"
}

if [ ! -d "venv" ]; then
	virtualenv3 venv
	activate
	pip install -r requirements.txt
	if [ -d ${GI_PATH} ]; then
		ln -s ${GI_PATH} venv/lib/python3*/site-packages/
	else
		echo "python-gobject is not installed! No notifications." 1>&2
	fi
else
	echo "Virtualenv already set-up. Skipping." 1>&2
fi

#!/bin/sh

##############################################################
# Run this script to boostrap i3pystatus virtual environment #
##############################################################

BASE_PATH=`dirname $0`
VENV_PATH="${BASE_PATH}/venv"
VENV_BIN_PATH="${VENV_PATH}/bin"

if [ ! -d "${VENV_PATH}" ]; then
    echo "Creating virtualenv and installing dependencies..."
    virtualenv3 --system-site-packages "${VENV_PATH}"
    ${VENV_BIN_PATH}/pip install -r "${BASE_PATH}/requirements.txt"
    echo "Finished ipy3status setup. Please restart i3."
fi

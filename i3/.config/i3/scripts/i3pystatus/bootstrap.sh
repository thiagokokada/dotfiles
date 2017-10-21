#!/bin/sh

##############################################################
# Run this script to boostrap i3pystatus virtual environment #
##############################################################

BASE_PATH=`dirname $0`
VENV_PATH="${BASE_PATH}/venv"
VENV_BIN_PATH="${VENV_PATH}/bin"

if [ ! -d "${VENV_PATH}" ]; then
    echo "Creating virtualenv and installing dependencies..."
    python -m venv "${VENV_PATH}"
    ${VENV_BIN_PATH}/python -m pip install -r "${BASE_PATH}/requirements.txt"
    echo "Finished ipy3status setup. Please restart i3."
else
    echo "Virtualenv already installed, updating..."
    ${VENV_BIN_PATH}/python -m pip install -U -r "${BASE_PATH}/requirements.txt"
fi

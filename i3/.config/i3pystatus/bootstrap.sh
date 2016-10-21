#!/bin/sh

##############################################################
# Run this script to boostrap i3pystatus virtual environment #
##############################################################

BASE_PATH=`pwd`
VENV_PATH="${BASE_PATH}/venv"
VENV_BIN_PATH="${VENV_PATH}/bin"
PY_LIB_PATH="lib/python3.[0-9]/site-packages"
GOBJECT_USR_PATH="/usr/${PY_LIB_PATH}/gi"

if [ ! -d "${VENV_PATH}" ]; then
    echo "Creating virtualenv and installing dependencies..."
    virtualenv3 "${VENV_PATH}"
    ${VENV_BIN_PATH}/pip install -r "${BASE_PATH}/requirements.txt"
    if [ -f ${GOBJECT_USR_PATH}/__init__.py ]; then
        ln -s ${GOBJECT_USR_PATH} ${VENV_PATH}/${PY_LIB_PATH}
    else
        echo "WARNING: python-gobject not installed! No notifications." 1>&2
    fi
    echo "Finished ipy3status setup. Please restart i3."
fi

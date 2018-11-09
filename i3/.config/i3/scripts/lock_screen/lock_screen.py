#!/usr/bin/env python

from subprocess import call
from tempfile import NamedTemporaryFile

from dpms import DPMS
from mss import mss
from PIL import Image, ImageFilter

GAUSSIAN_BLUR_RADIUS = 5
SCREEN_TIMEOUT = (5, 5, 5)  # Standby, Suspend, Off

# Get current DPMS settings
dpms = DPMS()
current_timeouts = dpms.GetTimeouts()

with mss() as sct:
    # Get the "All-in-one" monitor
    monitor = sct.monitors[0]
    # Get raw pixels of the screen
    sct_img = sct.grab(monitor)
    # Create Image object using Pillow
    image = Image.frombytes("RGB", sct_img.size, sct_img.rgb)

with NamedTemporaryFile(suffix=".png") as tempfile:
    # Apply filters to Image and save temporary file
    image \
        .filter(ImageFilter.GaussianBlur(radius=GAUSSIAN_BLUR_RADIUS)) \
        .save(tempfile.name, optimize=False, compress_level=1)

    try:
        # Set monitor timeout to SCREEN_TIMEOUT
        dpms.SetTimeouts(*SCREEN_TIMEOUT)
        # Load image in i3lock
        call(["i3lock", "-nei", tempfile.name])
    finally:
        # Restore DPMS settings
        dpms.SetTimeouts(*current_timeouts)

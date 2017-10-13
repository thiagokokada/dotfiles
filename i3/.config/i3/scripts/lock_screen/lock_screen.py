#!/usr/bin/env python

from subprocess import check_call, CalledProcessError
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
    img = Image.frombytes("RGB", sct_img.size, sct_img.rgb)

with NamedTemporaryFile(suffix=".png") as tempfile:
    # Apply filters to Image
    img = img.filter(ImageFilter.GaussianBlur(radius=GAUSSIAN_BLUR_RADIUS))
    # Save temporary file
    img.save(tempfile.name, optimize=False, compress_level=1)
    # Set monitor timeout to SCREEN_TIMEOUT
    dpms.SetTimeouts(*SCREEN_TIMEOUT)
    try:
        # Load image in i3lock
        check_call(["i3lock", "-nei", tempfile.name])
    except CalledProcessError:
        # Something went wrong, lock it anyway
        check_call(["i3lock", "-ne"])
    finally:
        # Restore DPMS settings
        dpms.SetTimeouts(*current_timeouts)

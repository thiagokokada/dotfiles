#!/usr/bin/python

"""
Lock screen script, based on lock_screen.sh

This is much faster than lock_screen.sh, mainly thanks to the use of
pillow-simd instead of ImageMagick.
"""

from re import findall
from subprocess import run, PIPE
from tempfile import NamedTemporaryFile

from PIL import Image, ImageFilter

# i3lock standby interval
STANDBY_INTERVAL = "5"

# Get current dpms values from xset
result = run(["xset", "-q"], stdout=PIPE, universal_newlines=True)
regex = r"Standby:\s*([0-9]+)\s*Suspend:\s*([0-9]+)\s*Off:\s*([0-9]+)\s*"
standby, suspend, off = findall(regex, result.stdout)[0]

with NamedTemporaryFile(delete=True, suffix=".png") as tempfile:
    run(["scrot", tempfile.name])
    im = Image.open(tempfile.name)
    im = im.filter(ImageFilter.GaussianBlur(radius=8))
    im.save(tempfile.name, "PNG")
    run(["xset", "+dpms", "dpms",
        STANDBY_INTERVAL, STANDBY_INTERVAL, STANDBY_INTERVAL])
    run(["i3lock", "-neI", STANDBY_INTERVAL, "-i", tempfile.name])

run(["xset", "dpms", standby, suspend, off])

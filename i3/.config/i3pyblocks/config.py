#!/usr/bin/env python3

import asyncio
import logging
import signal
from pathlib import Path

import psutil as ps

from i3pyblocks import Runner, types
from i3pyblocks.blocks import (
    aiohttp,
    aionotify,
    datetime,
    i3ipc,
    psutil,
    pulsectl,
    subprocess,
)

# Configure logging, so we can have debug information available in
# ~/.i3pyblocks.log
# Use `logging.INFO` to reduce verbosity
logging.basicConfig(filename=Path.home() / ".i3pyblocks.log", level=logging.DEBUG)


# Helper to find partitions, filtering some that we don't want to show
# Will be used later on the DiskUsageBlock
def partitions(excludes=("/boot", "/nix/store")):
    partitions = ps.disk_partitions()
    return [p for p in partitions if p.mountpoint not in excludes]


async def main():
    # Create a Runner instance, so we can register the modules
    runner = Runner()

    # Show the current i3 focused window title
    # Using `.format()` (https://pyformat.info/) to limit the number of
    # characters to 41
    runner.register_block(i3ipc.WindowTitleBlock(format=" {window_title:.41s}"))

    # Show the current network speed for either en* (ethernet) or wl* devices
    # Limiting the interface name to only 2 characters since it can get quite
    # verbose
    runner.register_block(
        psutil.NetworkSpeedBlock(
            format_up=" {interface:.2s}:  {upload}  {download}",
            format_down="",
            interface_regex="en*|wl*",
        )
    )

    # For each partition found, add it to the Runner
    # Using `short_label=True` shows only the first letter of the path
    # i.e.: /mnt/backup -> /m/b
    for partition in partitions():
        runner.register_block(
            psutil.DiskUsageBlock(
                format=" {label}: {free:.1f}GiB",
                path=partition.mountpoint,
                short_label=True,
            )
        )

    runner.register_block(psutil.VirtualMemoryBlock(format=" {available:.1f}GiB"))

    # Using custom icons to show the temperature visually
    # So when the temperature is above 75,  is shown, when it is above 50,
    #  is shown, etc.
    # Needs Font Awesome 5 installed
    runner.register_block(
        psutil.SensorsTemperaturesBlock(
            format="{icon} {current:.0f}°C",
            icons=((0, ""), (25, ""), (50, ""), (75, "")),
        )
    )

    runner.register_block(
        psutil.CpuPercentBlock(format=" {percent}%"),
    )

    runner.register_block(psutil.LoadAvgBlock(format=" {load1}"))

    runner.register_block(
        psutil.SensorsBatteryBlock(
            format_plugged=" {percent:.0f}%",
            format_unplugged="{icon} {percent:.0f}% {remaining_time}",
            format_unknown="{icon} {percent:.0f}%",
            icons=((0, ""), (10, ""), (25, ""), (50, ""), (75, "")),
        )
    )

    # ToggleBlock works by running the command specified in `command_state`,
    # if it returns any text it will show `format_on`, otherwise `format_off`
    # is shown
    # When `format_on` is being shown, clicking on it runs `command_off`,
    # while when `format_off` is being shown, clicking on it runs `command_on`
    runner.register_block(
        subprocess.ToggleBlock(
            command_state="xset q | grep -Fo 'DPMS is Enabled'",
            command_on="xset s on +dpms",
            command_off="xset s off -dpms",
            format_on="  ",
            format_off="  ",
        )
    )

    # ShellBlock just show the output of `command` (if it is empty this block
    # is hidden)
    # `command_on_click` runs some command when the mouse click is captured,
    # in this case when the user scrolls up or down
    runner.register_block(
        subprocess.ShellBlock(
            command="xkblayout-state print %s",
            format=" {output}",
            command_on_click=(
                (types.MouseButton.SCROLL_UP, "xkblayout-state set +1"),
                (types.MouseButton.SCROLL_DOWN, "xkblayout-state set -1"),
            ),
        )
    )

    # By default BacklightBlock showns a message "No backlight found" when
    # there is no backlight
    # We set to empty instead, so when no backlight is available (i.e.
    # desktop), we hide this block
    runner.register_block(
        aionotify.BacklightBlock(
            format=" {percent:.0f}%",
            format_no_backlight="",
            command_on_click=(
                (types.MouseButton.SCROLL_UP, "light -A 5%"),
                (types.MouseButton.SCROLL_DOWN, "light -U 5"),
            ),
        )
    )

    # `signals` allows us to send multiple signals that this block will
    # listen and do something
    # In this case, we can force update the module when the volume changes,
    # for example, by running:
    # $ pactl set-sink-volume @DEFAULT_SINK@ +5% && pkill -SIGUSR1 example.py
    runner.register_block(
        pulsectl.PulseAudioBlock(
            format=" {volume:.0f}%",
            format_mute=" mute",
        ),
        signals=(signal.SIGUSR1, signal.SIGUSR2),
    )

    runner.register_block(
        aiohttp.RequestBlock(
            "https://wttr.in/?format=%c+%t",
            format_error="",
            sleep=60*60,
        ),
    )

    runner.register_block(
        datetime.DateTimeBlock(format_time=" %T", format_date=" %a, %d/%m")
    )

    # Start the Runner instance
    await runner.start()


if __name__ == "__main__":
    # Start the i3pyblocks
    asyncio.run(main())

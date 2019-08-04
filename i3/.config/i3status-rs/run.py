#!/usr/bin/env python3

import os, sys
from functools import partial
from subprocess import run
from tempfile import NamedTemporaryFile

COLOR_BAD = "#FF0000"
COLOR_BG = "#1D1F21"
COLOR_GOOD = "#00FF00"
COLOR_IDLE = "#969896"
COLOR_INFO = "#81A2BE"
COLOR_NORMAL = "#FFFFFF"
COLOR_WARN = "#FFFF00"

BASE_TEMPLATE = f"""\
[theme]
name = "plain"

[theme.overrides]
idle_bg = "{COLOR_BG}"
idle_fg = "{COLOR_IDLE}"
info_bg = "{COLOR_BG}"
info_fg = "{COLOR_INFO}"
good_bg = "{COLOR_BG}"
good_fg = "{COLOR_GOOD}"
warning_bg = "{COLOR_BG}"
warning_fg = "{COLOR_WARN}"
critical_bg = "{COLOR_BG}"
critical_fg = "{COLOR_BAD}"
separator_bg = "{COLOR_BG}"
separator = " "

[icons]
name = "awesome"

[icons.overrides]
music_play = "  "
music_pause = "  "
music_next = "  "
music_prev = "  "
"""

BLOCK_TEMPLATE = """\
[[block]]
block = "{name}"
{extra_config}

"""

debug = (
    partial(print, file=sys.stderr)
    if "DEBUG" in os.environ
    else lambda *_args, **_kwargs: None
)


# Helpers
def get_sys_class(path, excludes=[]):
    return [i for i in os.listdir(path) if i not in excludes]


def get_batteries(excludes=["AC"]):
    return get_sys_class(path="/sys/class/power_supply", excludes=excludes)


def get_mounted_partitions(excludes=["/boot", "/run/media", "/nix/store"]):
    partitions = []

    with open("/proc/self/mounts") as f:
        for info in f:
            partition = info.split(" ")[1]
            if info[0] == "/" and all(x not in partition for x in excludes):
                partitions.append(partition)

    return partitions


def get_net_interfaces(excludes=["lo"]):
    return get_sys_class(path="/sys/class/net", excludes=excludes)


# Config generators
def block(name, **kwargs):
    def convert(value):
        if isinstance(value, bool):
            return str(value).lower()
        elif isinstance(value, str):
            return f'"{value}"'
        else:
            return value

    extra_config = "\n".join([f"{k} = {convert(v)}" for k, v in kwargs.items()])

    return BLOCK_TEMPLATE.format(name=name, extra_config=extra_config)


def generate_config(*blocks):
    config = BASE_TEMPLATE

    for block in blocks:
        if isinstance(block, list):
            config += "\n".join(block)
        else:
            config += block

    return config


def run_i3status_rs(config):
    with NamedTemporaryFile(suffix=".toml") as config_file:
        config_file.write(config.encode())
        config_file.flush()

        debug("Running i3status-rs with config file: ", config_file.name)

        return run(["i3status-rs", config_file.name])


def main():
    disk_block = [
        block("disk_space", path=i, info_type="available", unit="GiB")
        for i in get_mounted_partitions()
    ]
    net_block = [
        block("net", device=i, speed_up=True, speed_down=True, hide_inactive=True)
        for i in get_net_interfaces()
    ]
    battery_block = [block("battery", device=i, show="both") for i in get_batteries()]

    config = generate_config(
        block("music", max_width=0, buttons=["play", "next"]),
        block("focused_window", max_width=41),
        disk_block,
        net_block,
        block(
            "memory",
            format_mem="{MAg}GiB",
            format_swap="{SFg}GiB",
            display_type="memory",
        ),
        block("load", interval=5),
        block("temperature", format="{average}°C", collapsed=False),
        block("sound", on_click="pavucontrol"),
        battery_block,
        block("time", interval=1, format="%a %T"),
    )

    debug(config)

    run_i3status_rs(config)


if __name__ == "__main__":
    main()

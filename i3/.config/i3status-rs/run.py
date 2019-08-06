#!/usr/bin/env python3

import os, sys
from functools import partial
from subprocess import run
from tempfile import NamedTemporaryFile

BASE_TEMPLATE = f"""\
[theme]
name = "plain"

[theme.overrides]
idle_bg = "#1d1f21"
idle_fg = "#c5c8c6"
info_bg = "#81a2be"
info_fg = "#1d1f21"
good_bg = "#b5bd68"
good_fg = "#1d1f21"
warning_bg = "#f0c674"
warning_fg = "#1d1f21"
critical_bg = "#cc6666"
critical_fg = "#1d1f21"
separator_bg = "#1d1f21"
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
    return [i for i in os.listdir(path) if all(x not in i for x in excludes)]


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


def get_net_interfaces(excludes=["lo", "br"]):
    return get_sys_class(path="/sys/class/net", excludes=excludes)


# Config generators
def block(name, **kwargs):
    def convert(value):
        if isinstance(value, bool):
            return str(value).lower()
        elif isinstance(value, str):
            return '"{value}"'.format(value=value.replace('"', r"\""))
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
        block(
            "disk_space",
            path=p,
            alias="/" + "/".join([x[0] for x in p.split("/") if x]),
            info_type="available",
            unit="GiB",
        )
        for p in get_mounted_partitions()
    ]
    net_block = [
        block("net", device=n, speed_up=True, speed_down=True, hide_inactive=True)
        for n in get_net_interfaces()
    ]
    battery_block = [block("battery", device=b, show="both") for b in get_batteries()]

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
        block(
            "custom",
            command="setxkbmap -print | awk -F'+' '/xkb_symbols/ {print \"  \" $2}'",
            interval=1,
        ),
        block("sound", on_click="pavucontrol"),
        battery_block,
        block("time", interval=1, format="%a %T"),
    )

    debug(config)

    run_i3status_rs(config)


if __name__ == "__main__":
    main()

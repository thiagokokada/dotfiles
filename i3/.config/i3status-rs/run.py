#!/usr/bin/env python3

import os, sys
from functools import partial
from subprocess import run
from tempfile import NamedTemporaryFile

# Base 16 Tomorrow Night
BASE00 = "#1d1f21"
BASE01 = "#282a2e"
BASE02 = "#373b41"
BASE03 = "#969896"
BASE04 = "#b4b7b4"
BASE05 = "#c5c8c6"
BASE06 = "#e0e0e0"
BASE07 = "#ffffff"
BASE08 = "#cc6666"
BASE09 = "#de935f"
BASE0A = "#f0c674"
BASE0B = "#b5bd68"
BASE0C = "#8abeb7"
BASE0D = "#81a2be"
BASE0E = "#b294bb"
BASE0F = "#a3685a"

BASE_TEMPLATE = f"""\
[theme]
name = "plain"

[theme.overrides]
idle_bg = "{BASE00}"
idle_fg = "{BASE05}"
info_bg = "{BASE0D}"
info_fg = "{BASE00}"
good_bg = "{BASE0B}"
good_fg = "{BASE00}"
warning_bg = "{BASE0A}"
warning_fg = "{BASE00}"
critical_bg = "{BASE06}"
critical_fg = "{BASE00}"
separator_bg = "{BASE00}"
separator = " "

[icons]
name = "awesome"

[icons.overrides]
music_play = "  "
music_pause = "  "
music_next = "  "
music_prev = "  "
eco_on=" "
eco_off=" "
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


def get_backlights(excludes=[]):
    return get_sys_class(path="/sys/class/backlight", excludes=excludes)


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


def get_net_interfaces(excludes=["lo", "br", "vboxnet", "docker"]):
    return get_sys_class(path="/sys/class/net", excludes=excludes)


# Config generators
def block(name, **kwargs):
    def convert(value):
        if isinstance(value, bool):
            return str(value).lower()
        elif isinstance(value, str):
            return '"' + value.replace('"', '\\"') + '"'
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
    with NamedTemporaryFile(mode="w", suffix=".toml") as config_file:
        config_file.write(config)
        config_file.flush()

        debug("Running i3status-rs with config file: ", config_file.name)

        return run(["i3status-rs", config_file.name])


def main():
    backlight_block = [block("backlight", device=b) for b in get_backlights()]
    disk_block = [
        block(
            "disk_space",
            path=p,
            alias=" /" + "/".join([x[0] for x in p.split("/") if x]),
            info_type="available",
            unit="GiB",
        )
        for p in get_mounted_partitions()
    ]
    net_block = [
        block(
            "net",
            device=n,
            speed_up=True,
            speed_down=True,
            hide_missing=True,
            hide_inactive=True,
        )
        for n in get_net_interfaces()
    ]

    config = generate_config(
        block("focused_window", max_width=41),
        block("music", max_width=0, buttons=["play", "next"]),
        net_block,
        disk_block,
        block(
            "memory",
            format_mem="{MAg}GiB",
            format_swap="{SFg}GiB",
            display_type="memory",
        ),
        block("load"),
        block("temperature", format="{average}°C", collapsed=False),
        block(
            "toggle",
            command_state="xset q | grep -Fo 'DPMS is Enabled'",
            command_on="xset s on +dpms",
            command_off="xset s off -dpms",
            icon_on="eco_on",
            icon_off="eco_off",
            interval=5,
        ),
        backlight_block,
        block("sound", on_click="pavucontrol"),
        block(
            "battery",
            device="DisplayDevice",
            driver="upower",
            format="{percentage}% {time}",
        ),
        block("keyboard_layout", driver="kbddbus"),
        block("time", interval=1, format="%a %T"),
    )

    debug(config)

    run_i3status_rs(config)


if __name__ == "__main__":
    main()

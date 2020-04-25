#!/usr/bin/env python3

import os
import shutil
import sys
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
good_bg = "{BASE00}"
good_fg = "{BASE05}"
warning_bg = "{BASE0A}"
warning_fg = "{BASE00}"
critical_bg = "{BASE08}"
critical_fg = "{BASE00}"
separator_bg = "{BASE00}"
separator = " "

[icons]
name = "awesome"

[icons.overrides]
backlight_empty = "  "
backlight_partial1 = "  "
backlight_partial2 = "  "
backlight_partial3 = "  "
backlight_full = "  "
eco_on="  "
eco_off="  "
net_up="  "
net_down="  "
"""

BLOCK_TEMPLATE = """\
[[block]]
block = "{name}"
{extra_config}
"""


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

    return BLOCK_TEMPLATE.format(name=name, extra_config=extra_config).strip()


def flatten(l):
    if isinstance(l, list):
        return flatten(l[0]) + (flatten(l[1:]) if len(l) > 1 else [])
    else:
        return [l]


def generate_config(*blocks):
    return "\n\n".join([BASE_TEMPLATE] + flatten(list(blocks)))


def run_i3status_rs(config):
    with NamedTemporaryFile(
        mode="w", prefix="i3status-rs_", suffix=".toml"
    ) as config_file:
        config_file.write(config)
        config_file.flush()

        print(
            "Running i3status-rs with config file: ", config_file.name, file=sys.stderr
        )

        path = shutil.which("i3status-rs")
        os.execv(path, ["i3status-rs", config_file.name])


def main():
    battery_block = [
        block("battery", device=b, driver="upower", format="{percentage}% {time}")
        for b in get_batteries()
    ]
    backlight_block = [block("backlight", device=b) for b in get_backlights()]
    disk_block = [
        block(
            "disk_space",
            path=p,
            alias=" /" + "/".join([x[0] for x in p.split("/") if x]),
            info_type="used",
            unit="Percent",
            warning=75,
            alert=90,
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
        net_block,
        disk_block,
        block(
            "memory", format_mem="{Mupi}%", format_swap="{SUpi}%", display_type="memory"
        ),
        block("load"),
        block(
            "temperature",
            format="{average}°C",
            collapsed=False,
            chip="coretemp-*",
            good=20,
            idle=55,
            info=70,
            warning=80,
        ),
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
        battery_block,
        block("sound", on_click="pavucontrol"),
        block("keyboard_layout", driver="kbddbus"),
        block("time", interval=1, format="%a %T"),
    )

    if "--dry-run" in sys.argv:
        print(config, file=sys.stderr)
    else:
        run_i3status_rs(config)


if __name__ == "__main__":
    main()

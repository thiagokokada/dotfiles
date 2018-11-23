import json
from glob import glob
from pathlib import Path
from subprocess import run, PIPE

import psutil
from i3pystatus import Status, battery
from i3pystatus.updates import pacman, cower


def make_bar(percentage):
    """Modified function make_bar to substitute the original one"""
    bars = ['', '', '', '', '']
    base = 100 / (len(bars) - 1)
    index = round(percentage / base)
    return bars[index]


def get_cpu_temp_file(dir_path, filename, match):
    """Try to find which CPU file is the correct one"""
    dir = Path(dir_path)
    for p in dir.glob(filename):
        with open(p / "type") as f:
            if f.readline().rstrip() == match:
                return p / "temp"
    # failed to find a match, returns the first one
    return sorted(dir.glob(filename))[0] / "temp"


def get_mounted_block_devices(excludes = []):
    """Find all mounted devices in the system"""
    result = []
    for disk in psutil.disk_partitions():
        if disk.mountpoint not in excludes:
            result.append(disk.mountpoint)
    return result


# inject it in battery module, so it will display unicode icons instead
# of the (ugly) default bars
battery.make_bar = make_bar

# create i3pystatus instance
status = Status()

# show updates in pacman/aur
status.register(
    "updates",
    format=" {Pacman}/{Cower}",
    backends=[pacman.Pacman(), cower.Cower()],
)

# show clock
status.register(
    "clock",
    format=[" %H:%M:%S", " %a %d/%m"],
    on_leftclick="scroll_format",
)

# show current keyboard layout
status.register(
    "shell",
    command="setxkbmap -query | awk -F ': *' '/layout/ { print toupper($2) }'",
    interval=5,
    format=" {output}",
)

# show/change volume using PA
status.register(
    "pulseaudio",
    format=" {volume}%",
    format_muted=" Mute",
    on_leftclick="pavucontrol",
)

# show/control screen brightness
status.register(
    "backlight",
    format=" {percentage}%",
    format_no_backlight=" DISABLED",
)

# show/control screen brightness
status.register(
    "redshift",
    format_inhibit=["", ""],
    redshift_parameters=["-P"]
)

# show network speed
status.register(
    "network",
    format_up="[ {essid} \[{quality}%\]]  {bytes_recv}K  {bytes_sent}K",
    format_down=" {interface}",
    interface="enp3s0",
    next_if_down=True,
    on_leftclick="kitty -e sudo nethogs",
    on_upscroll=None,
    on_downscroll=None,
)

# show battery status
status.register(
    battery,
    format="{bar} {percentage:.0f}%[ {remaining}][ {status}]",
    alert_percentage=10,
    not_present_text=" ON POWER",
    status={
        "CHR": "",
        "DPL": "",
        "DIS": "",
        "FULL": "",
    },
)

# show disk available space
mounted_block_devices = get_mounted_block_devices(excludes=["/boot"])
for block_device in mounted_block_devices[::-1]:
    status.register(
        "disk",
        format=" " + block_device + " {avail:.1f}G",
        path=block_device
    )

# show available memory
status.register(
    "mem",
    format=" {avail_mem}G",
    warn_percentage=70,
    alert_percentage=90,
    divisor=1024**3,
)

# show cpu usage
status.register(
    "load",
    format=" {avg1} {avg5}",
)

# show CPU temperature
cpu_temp_file = get_cpu_temp_file("/sys/class/thermal", "thermal_zone*", "x86_pkg_temp")
status.register(
    "temp",
    file=cpu_temp_file,
    format=" {temp:.0f}°C",
)

# show CPU clock
status.register(
    "cpu_freq",
    format=" {avgg}"
)

# show current music info
player_format = '{status} [{artist} - {title} \[{song_length}\]]'
player_status = {
    'play': '',
    'pause': '',
    'stop': '',
}

status.register(
    "now_playing",
    format=player_format,
    status=player_status,
)

status.register(
    "mpd",
    format=player_format,
    status=player_status,
    hide_inactive=True,
    on_upscroll=None,
    on_downscroll=None,
)

status.run()

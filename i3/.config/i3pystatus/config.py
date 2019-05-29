from pathlib import Path

import psutil
from i3pystatus import Status


def get_cpu_temp_file(dir_path, filename, match):
    """Try to find which CPU file is the correct one"""
    dir = Path(dir_path)
    for p in dir.glob(filename):
        with open(p / "type") as f:
            if f.readline().rstrip() == match:
                return p / "temp"
    # failed to find a match, returns the first one
    return sorted(dir.glob(filename))[0] / "temp"


def get_mounted_block_devices(excludes=[]):
    """Find all mounted devices in the system"""
    result = []
    for disk in psutil.disk_partitions():
        if disk.mountpoint not in excludes:
            result.append(disk.mountpoint)
    return result


# create i3pystatus instance
status = Status()

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

status.register(
    "dpms",
    format=" ",
    format_disabled=" ",
)

# show/control screen brightness
status.register(
    "backlight",
    format=" {percentage}%",
    format_no_backlight=" ",
)

# show network speed
status.register(
    "network",
    format_up="[ {essid} \[{quality}%\]]  {bytes_recv}K  {bytes_sent}K",
    format_down=" {interface}",
    next_if_down=True,
    on_upscroll=None,
    on_downscroll=None,
)

# show battery status
status.register(
    "battery",
    format="{status}{percentage:.0f}%[ {remaining}]",
    alert_percentage=10,
    not_present_text="",
    levels={
        25: " ",
        50: " ",
        75: " ",
        90: " "
    },
    status={
        "CHR":  " ",
        "DPL":  " ",
        "FULL": " ",
    },
)

# show disk available space
mounted_block_devices = get_mounted_block_devices(excludes=["/boot", "/nix/store"])
for block_device in mounted_block_devices[::-1]:
    pretty_name = " /" + "/".join([x[0] for x in block_device.split("/") if x])
    status.register(
        "disk",
        format=pretty_name + " {avail:.1f}G",
        path=block_device,
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
cpu_temp_file = get_cpu_temp_file("/sys/class/thermal",
                                  "thermal_zone*",
                                  "x86_pkg_temp")
status.register(
    "temp",
    file=cpu_temp_file,
    format=" {temp:.0f}°C",
)

# show CPU clock
status.register(
    "cpu_freq",
    format=" {avgg}",
)

# show current music info
status.register(
    "now_playing",
    format="{status} {title}",
    status={
        "play":  "",
        "pause": "",
        "stop":  "",
    },
)

status.register(
    "window_title",
    max_width=79,
)

status.run()

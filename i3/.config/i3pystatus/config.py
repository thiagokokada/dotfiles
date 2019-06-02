from pathlib import Path
from subprocess import run

import psutil
from i3pystatus import IntervalModule, Status

COLOR_NORMAL = "#FFFFFF"
COLOR_GOOD = "#00FF00"
COLOR_WARN = "#FFFF00"
COLOR_BAD = "#FF0000"


class Xkblayout(IntervalModule):
    interval = 5
    format = " {name}",
    uppercase = True
    layouts = []
    settings = (
        ("format", "Format string"),
        ("layouts", "List of layouts"),
    )
    on_leftclick = "change_layout"

    def run(self):
        kblayout = self.kblayout()

        self.output = {
            "full_text": self.format.format(name=kblayout),
            "color": COLOR_NORMAL
        }

    def change_layout(self):
        layouts = self.layouts
        kblayout = self.kblayout()
        if kblayout in layouts:
            position = layouts.index(kblayout)
            try:
                run(["setxkbmap"] + layouts[position + 1].split(), check=True)
            except IndexError:
                run(["setxkbmap"] + layouts[0].split(), check=True)
        else:
            run(["setxkbmap"] + layouts[0].split(), check=True)

    def kblayout(self):
        result = (
            run(["setxkbmap", "-query"], capture_output=True, check=True)
            .stdout
            .decode()
            .splitlines()
        )
        kblayout = [l.split() for l in result]
        kblayout = [l[1].strip() for l in kblayout
                    if l[0].startswith(("layout", "variant"))]
        return (" ").join(kblayout)


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
    Xkblayout,
    format=" {name}",
    layouts=["us intl", "br"],
)

# show/change volume using PA
status.register(
    "pulseaudio",
    format=" {volume}%{selected}",
    format_selected=" ",
    format_muted=" Mute",
    color_unmuted=COLOR_GOOD,
    color_muted=COLOR_BAD,
    multi_colors=True,
)

status.register(
    "dpms",
    format=" ",
    format_disabled=" ",
    color=COLOR_GOOD,
    color_disabled=COLOR_BAD,
)

# show/control screen brightness
status.register(
    "backlight",
    adjust_method="light",
    format=" {percentage}%",
    format_no_backlight="",
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
    format_up="[ {essid} \[{quality}%\]]  {bytes_recv}  {bytes_sent}",
    format_down=" {interface}",
    detect_active=True,
    next_if_down=True,
    auto_units=True,
    on_leftclick="kitty iftop",
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
mounted_block_devices = get_mounted_block_devices(
    excludes=["/boot", "/nix/store"]
)
for block_device in mounted_block_devices[::-1]:
    pretty_name = " /" + "/".join(
        [x[0] for x in block_device.split("/") if x]
    )
    status.register(
        "disk",
        critical_limit=psutil.disk_usage(block_device).total / 1024**3 * 0.1,
        critical_color=COLOR_BAD,
        format=pretty_name + " {avail:.1f}G",
        on_leftclick="kitty ncdu " + block_device,
        path=block_device,
    )

# show available memory
status.register(
    "mem",
    format=" {avail_mem}G",
    warn_percentage=70,
    alert_percentage=90,
    divisor=1024**3,
    on_leftclick="kitty htop --sort-key=PERCENT_MEM",
)

# show cpu usage
status.register(
    "load",
    format=" {avg1} {avg5}",
    on_leftclick="kitty htop --sort-key=PERCENT_CPU"
)

cpu_temp_file = get_cpu_temp_file("/sys/class/thermal",
                                  "thermal_zone*",
                                  "x86_pkg_temp")
# show CPU temperature
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
    color=COLOR_GOOD,
    color_no_player=COLOR_BAD,
    status={
        "play":  "",
        "pause": "",
        "stop":  "",
    },
)

# show window title
status.register(
    "window_title",
    max_width=79,
)

status.run()

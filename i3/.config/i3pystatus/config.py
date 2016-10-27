from i3pystatus import Status, battery
from i3pystatus.updates import pacman, cower


def make_bar(percentage):
    """Modified function make_bar to substitute the original one"""
    bars = ['', '', '', '', '']
    base = 100 / (len(bars) - 1)
    index = round(percentage / base)
    return bars[index]


# Inject it in battery module, so it will display unicode icons instead
# of the (ugly) default bars
battery.make_bar = make_bar
status = Status()

# show updates in pacman/aur
status.register(
    "updates",
    format=" {Pacman}/{Cower}",
    backends=[pacman.Pacman(), cower.Cower()],
)

# show clock
status.register(
    "clock",
    format=[" %H:%M:%S", " %a %d/%m"],
    on_leftclick="scroll_format",
)

# show/change current keyboard layout
status.register(
    "xkblayout",
    format="  {name}",
    layouts=["br", "us"],
)

# show/change volume using PA
status.register(
    "pulseaudio",
    format=" {volume}%",
    format_muted=" Mute",
)

# show/control screen brightness
status.register(
    "backlight",
    format=" {percentage}%",
)

# show/control screen brightness
status.register(
    "redshift",
    format_inhibit=["", ""],
)

# show network speed
status.register(
    "network",
    format_up="[ {essid} \[{quality}%\] ] {bytes_recv}K  {bytes_sent}K",
    format_down=" {interface}",
    interface="enp3s0",
    next_if_down=True,
    on_leftclick="sakura -x nmtui",
    on_upscroll=None,
    on_downscroll=None,
)

# show battery status
status.register(
    battery,
    format="{bar} {percentage:.0f}%[ {remaining}][ {status}]",
    interval=5,
    alert_percentage=10,
    status={
        "CHR": "",
        "DPL": "",
        "DIS": "",
        "FULL": "",
    },
)

# show disk available space
status.register(
    "disk",
    format=" {avail:.1f}G",
    path="/",
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
    interval=5,
)

# show CPU temperature
status.register(
    "temp",
    format=" {temp:.0f}°C",
    file="/sys/class/thermal/thermal_zone7/temp",
    alert_temp=70,
)

# show current music info
status.register(
    "playerctl",
    format='{status} [{artist} - {title} \[{length}\]]',
    format_not_running='',
    status={
        'playing': '',
        'paused': '',
        'stopped': '',
    },
    on_leftclick="playerctl play-pause",
    on_rightclick="playerctl next",
    on_upscroll=None,
    on_downscroll=None,
)

status.run()

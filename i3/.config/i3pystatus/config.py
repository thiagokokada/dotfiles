from i3pystatus import Status
from i3pystatus.updates import pacman, cower

status = Status()

# show updates in pacman/aur
status.register(
    "updates",
    format=" {Pacman}/{Cower}",
    backends=[pacman.Pacman(), cower.Cower()]
)

# show clock
status.register(
    "clock",
    format=" %a %d-%m-%y  %H:%M:%S"
)

# show/change current keyboard layout
status.register(
    "xkblayout",
    format="  {name}",
    layouts=["br", "us"]
)

# show/change volume using PA
status.register(
    "pulseaudio",
    format=" {volume}%",
    format_muted=" Mute"
)

# show network speed
status.register(
    "network",
    format_up="{interface}  {bytes_recv}KiB/s  {bytes_sent}KiB/s",
    format_down="{interface} ",
    interface="wlp2s0"
)

# show battery status
status.register(
    "battery",
    interval=5,
    format="[{status} ]{percentage:.0f}%",
    alert=True,
    alert_percentage=15,
    status={
        "CHR": "",
        "DPL": "",
        "DIS": "",
        "FULL": "",
    }
)

# show disk available space
status.register(
    "disk",
    path="/",
    format=" {avail}GiB"
)

# show available memory
status.register(
    "mem",
    format=" {avail_mem}GiB",
    warn_percentage=70,
    alert_percentage=90,
    divisor=1024**3
)

# show cpu usage
status.register(
    "cpu_usage",
    format=" {usage:02}%"
)

# show focused window title
status.register(
    "window_title",
    max_width=79
)

status.run()

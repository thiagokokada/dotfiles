from i3pystatus import Status
from i3pystatus.network import Network, sysfs_interface_up
from i3pystatus.updates import pacman, cower


class MyNetwork(Network):
    """
    Modified Network class that automatic switch interface in case of
    the current interface is down.
    """
    on_upscroll = None
    on_downscroll = None

    def run(self):
        super().run()
        if not sysfs_interface_up(self.interface, self.unknown_up):
            self.cycle_interface()

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
    format=" %a %d/%m  %H:%M:%S",
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

status.register(
    "backlight",
    format=" {percentage}%",
    backlight="intel_backlight",
)

# show network speed
status.register(
    MyNetwork,
    format_up="{interface:.2}  {bytes_recv}K  {bytes_sent}K",
    format_down="{interface:.2} ",
    interface="enp3s0",
)

# show battery status
status.register(
    "battery",
    format="[{status} ]{percentage:.0f}% {remaining}",
    interval=5,
    alert=True,
    alert_percentage=15,
    status={
        "CHR": "",
        "DPL": "",
        "DIS": "",
        "FULL": "",
    },
)

# show disk available space
status.register(
    "disk",
    format=" {avail}G",
    path="/",
)

# show available memory
status.register(
    "mem",
    format=" {avail_mem}G",
    warn_percentage=70,
    alert_percentage=90,
    divisor=1024**3,
)

# show cpu usage
status.register(
    "load",
    format=" {avg1} {avg5}",
)

status.register(
    "temp",
    format=" {temp}°C",
    file="/sys/class/thermal/thermal_zone7/temp",
)

# show focused window title
status.register(
    "window_title",
    max_width=79,
)

status.run()

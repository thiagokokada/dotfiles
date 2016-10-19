from i3pystatus import Status, formatp
from i3pystatus.network import Network, sysfs_interface_up
from i3pystatus.spotify import Spotify
from i3pystatus.updates import pacman, cower

from gi.repository import Playerctl


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


class AnyPlayerCtl(Spotify):
    """
    Hack to allow Spotify module to be used with any player supported
    by playerctl.
    """
    player_name = None

    def run(self):
        try:
            self.player = Playerctl.Player(player_name=self.player_name)

            response = self.get_info(self.player)

            fdict = {
                'status': self.status[response['status'].lower()],
                'title': response["title"],
                'album': response.get('album', ''),
                'artist': response.get('artist', ''),
                'length': response.get('length', 0),
            }
            self.data = fdict
            self.output = {"full_text": formatp(self.format, **fdict),
                           "color": self.color}
        except:
            self.output = {"full_text": self.format_not_running,
                           "color": self.color_not_running}
            if hasattr(self, "data"):
                del self.data


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

# show CPU temperature
status.register(
    "temp",
    format=" {temp}°C",
    file="/sys/class/thermal/thermal_zone7/temp",
)

status.register(
    AnyPlayerCtl,
    format='{status} {artist} - {title}',
    format_not_running=' Not running',
    status={'playing': '', 'paused': ''},
)

status.run()

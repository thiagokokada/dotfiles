{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (steam.override ({
      withPrimus = true;
      extraPkgs = pkgs: with pkgs; [
        bumblebee
        glxinfo
      ];
    }))
    steam-run-native
  ];

  # Enable 32 bit support since most Steam games are compiled to 32-bit only.
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  environment.variables = {
    # Workaround Steam's friendlist bug
    TZ = config.time.timeZone;
    # https://github.com/Bumblebee-Project/Bumblebee/issues/971#issuecomment-410386426
    __GLVND_DISALLOW_PATCHING = "1";
  };
}

{ pkgs, config, ... }:

{
  environment.systemPackages = with pkgs; [
    discord
    piper

    # With bumblebee.
    # (steam.override ({
    #   withPrimus = true;
    #   extraPkgs = pkgs: with pkgs; [
    #     bumblebee
    #     glxinfo
    #   ];
    # }))

    # Without bumblebee.
    steam
    steam-run-native
  ];

  # Enable ratbagd (for piper).
  services.ratbagd = {
    enable = true;
  };

  hardware = {
    opengl = {
      # Enable 32 bit support since most Steam games are compiled to 32-bit only.
      driSupport32Bit = true;
    };

    pulseaudio = {
      # Enable 32 bit support since most Steam games are compiled to 32-bit only.
      support32Bit = true;
    };
  };

  environment.variables = {
    # Workaround Steam's friendlist bug.
    TZ = config.time.timeZone;
  };
}

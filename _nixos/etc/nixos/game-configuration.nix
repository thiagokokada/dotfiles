{ pkgs, config, ... }:

{
  environment.systemPackages = with pkgs; [
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

  hardware = {
    opengl = {
      # Enable 32 bit support since most Steam games are compiled to 32-bit only.
      driSupport32Bit = true;
      # Enable S3TC (S3 Texture Compression) via libtxc_dxtn.
      s3tcSupport = true;
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

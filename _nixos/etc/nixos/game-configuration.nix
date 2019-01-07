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

  hardware = {
    # Enable bumblebee to dynamic switch Intel/NVIDIA GPUs.
    bumblebee = {
      enable = true;
      pmMethod = "bbswitch";
    };

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
    # Workaround Bumblebee issue.
    # https://github.com/Bumblebee-Project/Bumblebee/issues/971#issuecomment-410386426
    __GLVND_DISALLOW_PATCHING = "1";
    # Workaround Steam's friendlist bug.
    TZ = config.time.timeZone;
  };
}

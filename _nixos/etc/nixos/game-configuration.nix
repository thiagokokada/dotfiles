{ pkgs, config, ... }:

{
  environment.systemPackages = with pkgs; [
    piper
  ];

  # Enable steam
  programs.steam.enable = true;

  # Enable ratbagd (for piper).
  services.ratbagd = {
    enable = true;
  };
}

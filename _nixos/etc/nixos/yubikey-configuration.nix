# Driver support, etc for Yubikeys
{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    libu2f-host
    yubikey-manager
    yubikey-personalization-gui
  ];

  programs.gnupg.agent.enable = true;

  services = {
    pcscd.enable = true;
    udev.packages = [
      pkgs.libu2f-host
      pkgs.yubikey-personalization
    ];
  };
}

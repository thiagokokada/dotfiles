{ pkgs ? import <nixpkgs> {} }:

(pkgs.buildFHSUserEnv {
  name = "fhs-env";
  # Packages to be installed for the main host's architecture
  # (i.e. x86_64 on x86_64 installations)
  targetPkgs = pkgs: (with pkgs; [
    alsaLib
    udev
    xorg.libX11
    xorg.libXcursor
    xorg.libXrandr
  ]);
  # Packages to be installed for all architectures supported by a host
  # (i.e. i686 and x86_64 on x86_64 installations).
  multiPkgs = pkgs: (with pkgs; [
    alsaLib
    udev
  ]);
  runScript = "bash";
}).env

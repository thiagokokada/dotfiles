{ config, lib, pkgs, ... }:

let
  mpvRefactor = fetchGit {
    url = "https://github.com/nix-community/home-manager";
    ref = "master";
    rev = "2c0e3f61da903613cc316cb992c8c07d92e1d186";
  };
in {
  imports = [ "${mpvRefactor}/modules/programs/mpv.nix" ];
  disabledModules = [ "programs/mpv.nix" ];

  programs.mpv = {
    enable = true;

    package = with pkgs;
      wrapMpv (pkgs.mpv-unwrapped.override { vapoursynthSupport = true; }) {
        extraMakeWrapperArgs = [
          "--prefix"
          "LD_LIBRARY_PATH"
          ":"
          "${vapoursynth-mvtools}/lib/vapoursynth"
        ];
      };

    config = {
      osd-font-size = 14;
      osd-level = 3;
      slang = "enUS,enGB,en,eng,ptBR,pt,por";
      alang = "ja,jpn,enUS,enGB,en,eng,ptBR,pt,por";
      profile = [ "gpu-hq" "interpolation" ];
    };

    profiles = {
      color-correction = {
        target-prim = "bt.709";
        target-trc = "bt.1886";
        gamma-auto = true;
        icc-profile-auto = true;
      };

      interpolation = {
        interpolation = true;
        tscale = "box";
        tscale-window = "quadric";
        tscale-clamp = 0.0;
        tscale-radius = 1.025;
        video-sync = "display-resample";
        blend-subtitles = "video";
      };

      hq-scale = {
        scale = "ewa_lanczossharp";
        cscale = "ewa_lanczossharp";
      };

      low-power = {
        profile = "gpu-hq";
        hwdec = "auto";
        deband = false;
        interpolation = false;
      };
    };

    bindings = let
      motion-based-interpolation = config.my.dotfilesDir
        + "/mpv/.config/mpv/filters/motion-based-interpolation.vpy";
    in {
      F1 = "seek -85";
      F2 = "seek 85";
      I = "vf toggle vapoursynth=${motion-based-interpolation}";
    };
  };
}

{ stdenv, pkgs, lib, cmake, intltool, pkgconfig, wayland-protocols, fetchFromGitHub, ... }:

stdenv.mkDerivation rec {
  version = "0.1.0";
  name = "bemenu-${version}";

  src = fetchFromGitHub {
    owner = "Cloudef";
    repo = "bemenu";
    rev = "33e540a2b04ce78f5c7ab4a60b899c67f586cc32";
    sha256 = "11h55m9dx6ai12pqij52ydjm36dvrcc856pa834njihrp626pl4w";
  };

  nativeBuildInputs = [ cmake intltool pkgconfig wayland-protocols ];

  buildInputs = with pkgs; [
    cairo
    ncurses
    pango
    pcre
    wayland
    xlibs.libX11 xlibs.libXinerama xlibs.libXft
    xorg.libXdmcp xorg.libpthreadstubs xorg.libxcb
    zlib
  ];

  meta = {
    homepage = "https://github.com/Cloudef/bemenu";
    description = "Dynamic menu library and client program inspired by dmenu with support for wayland compositors.";
    platforms = stdenv.lib.platforms.linux;
  };
}

{stdenv, fetchFromGitHub
, meson, ninja, pkgconfig
, gtk3, epoxy
, wayland, wayland-protocols
, scdoc, buildDocs ? true
}:

stdenv.mkDerivation rec {
  pname = "wdisplays";
  version = "a3d3d13a017d4a5b461311d6d477e94b36f29990";

  src = fetchFromGitHub {
    owner = "cyclopsian";
    repo = "wdisplays";
    rev = version;
    sha256 = "0d81kl7bywg60s1z61q7qa505qrqd2api1rhgjavpv1a577fspnc";
  };

  nativeBuildInputs = [ pkgconfig meson ninja ] ++ stdenv.lib.optional buildDocs scdoc;
  buildInputs = [ gtk3 epoxy wayland wayland-protocols ];
  mesonFlags = [ "-Dauto_features=enabled" ]
    ++ stdenv.lib.optional (!buildDocs) "-Dman-pages=disabled";

  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    description = "GUI display configurator for wlroots compositors";
    homepage    = "https://github.com/cyclopsian/wdisplays";
    #license     = licenses.mit; # TODO 
    platforms   = platforms.linux;
    maintainers = with maintainers; [ colemickens ];
  };
}

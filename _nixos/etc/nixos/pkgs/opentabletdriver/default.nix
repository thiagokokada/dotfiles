{ stdenv
, lib
, fetchFromGitHub
, fetchurl
, makeWrapper
, linkFarmFromDrvs
, dotnet-netcore
, dotnet-sdk
, dotnetPackages
, dpkg
, gtk3
, libX11
, libXrandr
, libappindicator
, libevdev
, libnotify
, libudev
, wrapGAppsHook
}:

stdenv.mkDerivation rec {
  pname = "OpenTabletDriver";
  version = "0.4.2";

  src = fetchFromGitHub {
    owner = "InfinityGhost";
    repo = "OpenTabletDriver";
    rev = "v${version}";
    sha256 = "048y7gjlk2yw4vh62px1d9w0va6ap1a0cndcpbirlyj9q6b8jxax";
  };

  # TODO: Ideally this should be build from InfinityGhost/OpenTabletDriver-udev instead
  udev = stdenv.mkDerivation rec {
    pname = "OpenTabletDriver-udev";
    version = "0.4.2";

    nativeBuildInputs = [ dpkg ];

    src = fetchurl {
      url = "https://github.com/InfinityGhost/OpenTabletDriver/releases/download/v${version}/OpenTabletDriver.deb";
      sha256 = "13gg0dhvjy88h9lhcrp30fjiwgb9dzjsgk1k760pi1ki71a5vz2r";
    };

    unpackCmd = ''
      dpkg-deb -x $src out
    '';

    installPhase = ''
      mkdir -p $out/lib/udev/rules.d
      cp usr/lib/udev/rules.d/*.rules $out/lib/udev/rules.d/
    '';
  };

  nativeBuildInputs = [
    dotnet-sdk
    dotnetPackages.Nuget
    makeWrapper
    wrapGAppsHook
  ];

  nugetDeps = linkFarmFromDrvs "${pname}-nuget-deps" (import ./deps.nix {
    fetchNuGet = { name, version, sha256 }: fetchurl {
      name = "nuget-${name}-${version}.nupkg";
      url = "https://www.nuget.org/api/v2/package/${name}/${version}";
      inherit sha256;
    };
  });

  runtimeDeps = [
    gtk3
    libX11
    libXrandr
    libappindicator
    libevdev
    libnotify
    libudev
  ];

  configurePhase = ''
    export HOME=$(mktemp -d)
    export DOTNET_CLI_TELEMETRY_OPTOUT=1
    export DOTNET_SKIP_FIRST_TIME_EXPERIENCE=1

    nuget sources Add -Name nixos -Source "$PWD/nixos"
    nuget init "$nugetDeps" "$PWD/nixos"

    # FIXME: https://github.com/NuGet/Home/issues/4413
    mkdir -p $HOME/.nuget/NuGet
    cp $HOME/.config/NuGet/NuGet.Config $HOME/.nuget/NuGet

    for project in OpenTabletDriver.{Console,Daemon,UX.Gtk}; do
        dotnet restore --source "$PWD/nixos" $project
    done
  '';

  buildPhase = ''
    for project in OpenTabletDriver.{Console,Daemon,UX.Gtk}; do
        dotnet build $project \
            --no-restore \
            --configuration Release \
            --framework net5
    done
  '';


  installPhase = ''
    mkdir -p $out/lib/OpenTabletDriver/
    cp -r ./OpenTabletDriver/Configurations/ $out/lib/OpenTabletDriver/
    for project in OpenTabletDriver.{Console,Daemon,UX.Gtk}; do
      dotnet publish $project \
          --no-build \
          --no-self-contained \
          --configuration Release \
          --framework net5 \
          --output $out/lib
      makeWrapper $out/lib/$project $out/bin/$project \
          "''${gappsWrapperArgs[@]}" \
          --prefix XDG_DATA_DIRS : "${gtk3}/share/gsettings-schemas/${gtk3.name}/" \
          --set DOTNET_ROOT "${dotnet-netcore}" \
          --suffix LD_LIBRARY_PATH : "${lib.makeLibraryPath runtimeDeps}"
    done
  '';

  dontWrapGApps = true;
  dontStrip = true;

  meta = with lib; {
    description = "Open source, cross-platform, user-mode tablet driver";
    homepage = "https://github.com/InfinityGhost/OpenTabletDriver";
    license = licenses.lgpl3Plus;
    maintainers = with maintainers; [ thiagokokada ];
    platforms = [ "x86_64-linux" ];
  };

  passthru.updateScript = ./update.sh;
}

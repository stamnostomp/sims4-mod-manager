{
  description = "Sims 4 Mod Manager";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      sims4-mod-manager = (pkgs.haskellPackages.callCabal2nix "Sims4-mod-manager" ./. {}).overrideAttrs (old: {
        nativeBuildInputs = (old.nativeBuildInputs or []) ++ [
          pkgs.pkg-config
          pkgs.wrapGAppsHook4
          pkgs.gobject-introspection
        ];
        buildInputs = (old.buildInputs or []) ++ [
          pkgs.gtk4
          pkgs.libadwaita
          pkgs.gsettings-desktop-schemas
          pkgs.shared-mime-info
          pkgs.hicolor-icon-theme
        ];
      });
    in {
      packages.${system} = {
        default = sims4-mod-manager;
        sims4-mod-manager = sims4-mod-manager;
      };

      apps.${system}.default = {
        type = "app";
        program = "${sims4-mod-manager}/bin/Sims4-mod-manager";
      };

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          (haskellPackages.ghcWithPackages (ps: with ps; [
            gi-adwaita
            gi-gio
            gi-gtk
            haskell-gi-base
            zip-archive
          ]))
          cabal-install
          pkg-config
          gtk4
          libadwaita
          gobject-introspection
          gsettings-desktop-schemas
          shared-mime-info
          hicolor-icon-theme
        ];
        shellHook = ''
          export XDG_DATA_DIRS="${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:${pkgs.gtk4}/share/gsettings-schemas/${pkgs.gtk4.name}:${pkgs.hicolor-icon-theme}/share:${pkgs.shared-mime-info}/share:$XDG_DATA_DIRS"
          export GIO_EXTRA_MODULES="${pkgs.glib-networking}/lib/gio/modules"
        '';
      };
    };
}

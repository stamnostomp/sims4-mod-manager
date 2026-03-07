{
  description = "Sims 4 Mod Manager";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          (haskellPackages.ghcWithPackages (ps: with ps; [
            gi-adwaita
            gi-gio
            gi-gtk
            haskell-gi-base
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

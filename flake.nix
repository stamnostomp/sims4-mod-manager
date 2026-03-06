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
        ];
      };
    };
}

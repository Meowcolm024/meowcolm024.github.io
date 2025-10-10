{
  description = "Flake for homepage";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        hPkgs = pkgs.haskell.packages.ghc984;
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            hPkgs.ghc
            hPkgs.ormolu
            hPkgs.hlint
            hPkgs.cabal-install
            hPkgs.haskell-language-server
            pkgs.stack
            pkgs.pkg-config
            pkgs.clang
            pkgs.git
            pkgs.zlib
          ];
        };
      }
    );
}

{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      utils,
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            bash
            coreutils
            git-cliff
            gnumake
            haskell.compiler.ghc98
            haskellPackages.cabal-fmt
            haskellPackages.cabal-install
            haskellPackages.fourmolu
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.stack
          ];
        };
        formatter = pkgs.nixfmt-tree; # Format this file with `nix fmt`
      }
    );
}

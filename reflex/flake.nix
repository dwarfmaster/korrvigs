{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    devenv.url = "github:cachix/devenv";
  };

  outputs = {
    nixpkgs,
    devenv,
    self,
  } @ inputs: let
    systems = ["x86_64-linux" "i686-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin"];
    forAllSystems = f:
      builtins.listToAttrs (map
        (name: {
          inherit name;
          value = f name;
        })
        systems);
  in {
    packages =
      forAllSystems
      (system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        backend = pkgs.haskellPackages.callPackage ./. {};
        default = self.packages.${system}.backend;
      });

    devShells =
      forAllSystems
      (system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        default = devenv.lib.mkShell {
          inherit inputs pkgs;
          # https://devenv.sh/reference/options/
          modules = [
            {
              languages.nix.enable = true;
              languages.haskell.enable = true;
              languages.haskell.package =
                pkgs.haskellPackages.ghcWithPackages
                (hpkgs: [
                  hpkgs.reflex
                ]);

              pre-commit.hooks = {
                alejandra.enable = true;
                deadnix.enable = true;
                cabal-fmt.enable = true;
                cabal2nix.enable = true;
                ormolu.enable = true;
              };
              pre-commit.settings = {
                alejandra.exclude = ["reflex/default.nix"];
              };
            }
          ];
        };
      });
  };
}

{
  description = "Intelligent knowledge database system";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    devenv.url = "github:cachix/devenv";
  };

  outputs = {
    nixpkgs,
    devenv,
    ...
  } @ inputs: let
    system = "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;
    };

    shell = devenv.lib.mkShell {
      inherit inputs pkgs;
      modules = [
        {
          languages.nix.enable = true;
          languages.haskell.enable = true;
          languages.haskell.package = pkgs.haskellPackages.ghcWithPackages (hpkgs: [
            hpkgs.yesod
            (pkgs.haskell.lib.dontCheck hpkgs.opaleye)
            hpkgs.pandoc
            hpkgs.lens
            hpkgs.linear
            hpkgs.base16-bytestring
            hpkgs.data-endian
            hpkgs.HUnit
            hpkgs.extra
            hpkgs.parsec3-numbers
            hpkgs.text-builder
            hpkgs.hmatrix-glpk
            hpkgs.http-conduit
            hpkgs.lens-aeson
            hpkgs.text-manipulate
            hpkgs.optparse-applicative
          ]);

          pre-commit.hooks = {
            alejandra.enable = true;
            alejandra.settings.exclude = ["default.nix"];
            deadnix.enable = true;
            cabal-fmt.enable = true;
            cabal2nix.enable = true;
            ormolu.enable = true;
          };

          packages = [
            pkgs.xdot
            pkgs.broot
            pkgs.haskellPackages.yesod-bin
            pkgs.nodejs_20
            pkgs.exiftool
            pkgs.poppler_utils
          ];
        }
      ];
    };
  in {
    devShells.${system} = {
      default = shell;
    };
  };
}

{
  description = "Intelligent knowledge database system";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    devenv.url = "github:cachix/devenv";
  };

  outputs = {
    self,
    nixpkgs,
    devenv,
  } @ inputs: let
    system = "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;
    };

    korrvigs-web = pkgs.haskellPackages.callPackage ./haskell {};

    shell = devenv.lib.mkShell {
      inherit inputs pkgs;
      modules = [
        {
          languages.nix.enable = true;
          languages.c.enable = true;
          languages.haskell.enable = true;
          languages.haskell.package = pkgs.haskellPackages.ghcWithPackages (hpkgs: [
            hpkgs.yesod
            hpkgs.opaleye
            hpkgs.pandoc
            hpkgs.dhall
            hpkgs.lens
            hpkgs.linear
          ]);

          pre-commit.hooks = {
            alejandra.enable = true;
            deadnix.enable = true;
            cabal-fmt.enable = true;
            cabal2nix.enable = true;
            ormolu.enable = true;
          };
          pre-commit.settings = {
            alejandra.exclude = ["haskell/default.nix" "new/default.nix"];
          };

          packages = [
            pkgs.socat
            pkgs.nushell
            pkgs.boost182.dev
            pkgs.souffle
            pkgs.xdot
            pkgs.perl536Packages.FileMimeInfo
            pkgs.broot
            pkgs.haskellPackages.yesod-bin
            pkgs.haskellPackages.dhall
            pkgs.nodejs_20
          ];
          env = {
            SOUFFLE_ROOT = "${pkgs.souffle}/";
          };
        }
      ];
    };
  in {
    devShells.${system} = {
      default = shell;
    };

    packages.${system} = let
      server = pkgs.callPackage ./server {boost = pkgs.boost182;};
    in {
      default = server;
      korrvigs-server = server;
      inherit korrvigs-web;
    };

    overlays.default = _: _: {
      inherit
        (self.packages.${system})
        korrvigs-server
        korrvigs-web
        ;
    };

    hmModules = {
      korrvigs = import ./nix/hm.nix;
      default = self.hmModules.korrvigs;
    };
  };
}

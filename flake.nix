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

    shell = devenv.lib.mkShell {
      inherit inputs pkgs;
      modules = [
        {
          languages.nix.enable = true;
          languages.racket.enable = true;
          languages.racket.package = pkgs.racket;

          pre-commit.hooks = {
            alejandra.enable = true;
            deadnix.enable = true;
          };
          pre-commit.settings = {
            alejandra.exclude = ["reflex/default.nix"];
          };

          packages = [
          ];
          env = {
          };
        }
      ];
    };
  in {
    devShells.${system} = {
      default = shell;
    };

    packages.${system} = let
      server = pkgs.callPackage ./racket {};
    in {
      default = server;
      korrvigs-server = server;
    };

    overlays.default = _: _: {
      inherit
        (self.packages.${system})
        korrvigs-server
        ;
    };

    hmModules = {
      korrvigs = import ./nix/hm.nix;
      default = self.hmModules.korrvigs;
    };
  };
}

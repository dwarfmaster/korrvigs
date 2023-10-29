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
          languages.go.enable = true;
          languages.nix.enable = true;
          languages.c.enable = true;

          pre-commit.hooks = {
            alejandra.enable = true;
            deadnix.enable = true;
            gofmt.enable = true;
            clang-format.enable = true;
          };
          pre-commit.settings = {
            alejandra.exclude = ["reflex/default.nix"];
          };

          packages = [
            pkgs.socat
            pkgs.gum
            pkgs.nushell
            pkgs.boost182.dev
            pkgs.souffle
            pkgs.xdot
            pkgs.perl536Packages.FileMimeInfo
            pkgs.broot
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
      tui = pkgs.callPackage ./tui {};
    in {
      default = server;
      korrvigs-server = server;
      korrvigs-tui = tui;
    };

    overlays.default = _: _: {
      inherit
        (self.packages.${system})
        korrvigs-server
        korrvigs-tui
        ;
    };

    hmModules = {
      korrvigs = import ./nix/hm.nix;
      default = self.hmModules.korrvigs;
    };
  };
}

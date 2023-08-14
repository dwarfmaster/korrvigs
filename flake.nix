{
  description = "Intelligent knowledge database system";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    devenv.url = "github:cachix/devenv";
    zig = {
      url = "github:mitchellh/zig-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    zig,
    devenv,
  } @ inputs: let
    system = "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;
      # TODO figure out where does the openssl dependency comes from
      config.permittedInsecurePackages = [
        "openssl-1.1.1u"
      ];
    };

    norg-source = pkgs.fetchFromGitHub {
      owner = "nvim-neorg";
      repo = "tree-sitter-norg";
      rev = "600874e0825a62c3822ff04e5d0efbac8d7e380c";
      sha256 = "1l200zd7xfhifaxq3i810j2bxpvm0yfgf6x2i2d83l34z32q3p4x";
    };
    norg-parser = pkgs.callPackage ./tree-sitter {tree-sitter-norg-source = norg-source;};

    shell = devenv.lib.mkShell {
      inherit inputs pkgs;
      modules = [
        {
          languages.nix.enable = true;
          languages.haskell.enable = true;
          languages.haskell.package =
            pkgs.haskellPackages.ghcWithPackages
            (hpkgs: [
              hpkgs.reflex
              hpkgs.fsnotify
              hpkgs.regex-posix
              hpkgs.uuid
              hpkgs.parsec
            ]);
          languages.zig.enable = true;
          languages.zig.package = zig.packages.${system}.master;
          languages.racket.enable = true;
          languages.racket.package = pkgs.racket;

          pre-commit.hooks = {
            alejandra.enable = true;
            deadnix.enable = true;
            cabal-fmt.enable = true;
            cabal2nix.enable = true;
            ormolu.enable = true;
            zig-fmt = {
              enable = true;
              name = "Zig Formatter";
              entry = "zig fmt";
              files = "\\.zig$";
            };
          };
          pre-commit.settings = {
            alejandra.exclude = ["reflex/default.nix"];
          };

          packages = [
            pkgs.swiProlog
            pkgs.tree-sitter
            pkgs.souffle
          ];
          env = {
            PYTHONPATH = "${pkgs.swiProlog}/lib/swipl/lib";
            PKG_CONFIG_PATH = "${pkgs.swiProlog}/share/pkgconfig:${pkgs.tree-sitter}/lib/pkgconfig";
            NORG_PARSER_SOURCE = "${norg-source}/src";
          };
        }
      ];
    };

    piper = pkgs.callPackage ./piper {};
    posix = pkgs.callPackage ./posix {};
    engine = pkgs.callPackage ./engine {};
  in {
    devShells.${system} = {
      default = shell;
    };

    packages.${system} = {
      default = engine;
      korrvigs-norg-parser = norg-parser;
      korrvigs-posix = posix;
      korrvigs = engine;
      backend = pkgs.haskellPackages.callPackage ./reflex {};
      inherit piper;
      korrvigs-server = pkgs.callPackage ./racket {};
    };

    overlays.default = _: _: {
      inherit
        (self.packages.${system})
        korrvigs
        korrvigs-posix
        korrvigs-norg-parser
        piper
        ;
    };

    hmModules = {
      korrvigs = import ./nix/hm.nix;
      default = self.hmModules.korrvigs;
    };
  };
}

{
  description = "Intelligent knowledge database system";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    devenv.url = "github:cachix/devenv";
    nixvim.url = "github:nix-community/nixvim";
  };

  outputs = {
    self,
    nixpkgs,
    devenv,
    nixvim,
  } @ inputs: let
    system = "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;
    };
    inherit (pkgs) lib;

    deps = [
      pkgs.exiftool
      pkgs.poppler_utils
      pkgs.git
    ];

    static = pkgs.callPackage ./static.nix {};

    shell = devenv.lib.mkShell {
      inherit inputs pkgs;
      modules = [
        {
          languages.nix.enable = true;
          languages.haskell.enable = true;
          languages.haskell.package = pkgs.haskellPackages.ghcWithPackages (hpkgs: [
            hpkgs.yesod
            hpkgs.yesod-static
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
            hpkgs.xdg-basedir
            hpkgs.password
            hpkgs.iconv
            hpkgs.clock
            hpkgs.isbn
            hpkgs.feed
          ]);

          git-hooks.hooks = {
            alejandra.enable = true;
            alejandra.settings.exclude = ["default.nix"];
            deadnix.enable = true;
            cabal-fmt.enable = true;
            cabal2nix.enable = true;
            ormolu.enable = true;
          };

          packages =
            [
              pkgs.haskellPackages.weeder
            ]
            ++ deps;

          env.KORRVIGS_WEB_STATIC = "${static}";
        }
      ];
    };

    korrvigs = pkgs.haskellPackages.callPackage ./default.nix {};
    korrvigs-wrapped = pkgs.writeShellScriptBin "korr" ''
      PATH=${lib.makeBinPath deps}:$PATH exec ${korrvigs}/bin/korrvigs-cli "$@"
    '';
    korrvigs-plugin = pkgs.vimUtils.buildVimPlugin {
      name = "nvim-korrvigs";
      src = ./neovim;
      doCheck = false;
      # TODO make sure checks pass
      # dependencies = with pkgs.vimPlugins; [ telescope-nvim plenary-nvim nvim-treesitter ];
    };

    nvimConfig = {config, ...}: {
      opts = {
        number = true;
        smartcase = true;
        ignorecase = true;
        autoindent = true;
        smartindent = true;
        # For ufo
        foldcolumn = "1";
        foldlevel = 99;
        foldlevelstart = 99;
        foldenable = true;
      };
      plugins.treesitter = {
        enable = true;
        nixGrammars = true;
        settings.ensure_installed = "all";
        grammarPackages = config.plugins.treesitter.package.passthru.allGrammars;
        nixvimInjections = true;
      };
      colorschemes.everforest.enable = true;
      plugins.telescope.enable = true;
      plugins.headlines.enable = true;
      plugins.nvim-ufo = {
        enable = true;
        providerSelector = ''
          function(bufnr, filetype, buftype)
            return {'treesitter', 'indent'}
          end
        '';
      };
    };
    nvim = nixvim.legacyPackages.${system}.makeNixvim {
      imports = [nvimConfig];
    };

    nvimShell = pkgs.mkShell {
      packages = [korrvigs-wrapped nvim];
    };
  in {
    devShells.${system} = {
      haskellDev = shell;
      default = self.devShells.${system}.haskellDev;
      pluginDev = nvimShell;
    };

    packages.${system} = {
      korrvigs-unwrapped = korrvigs;
      korrvigs-static = static;
      korrvigs = korrvigs-wrapped;
      nvim-korrvigs = korrvigs-plugin;
      default = self.packages.${system}.korrvigs;
      nvim-with-korrvigs = nvim;
      tmp = pkgs.haskellPackages.alfred-margaret;
      inherit pkgs;
    };

    overlays = {
      korrvigs = _: prev: {
        inherit
          (self.packages.${prev.system})
          korrvigs-unwrapped
          korrvigs-static
          korrvigs
          nvim-korrvigs
          ;
      };
      default = self.overlays.korrvigs;
    };

    nixosModules.korrvigs = import ./nixos.nix self.overlays.korrvigs;
    nixosModule = self.nixosModules.korrvigs;
  };
}

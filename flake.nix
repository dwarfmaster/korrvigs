{
  description = "Intelligent knowledge database system";

  outputs = {
    self,
    nixpkgs,
  }: let

    pkgs = import nixpkgs {system = "x86_64-linux";};

    norg-source = pkgs.fetchFromGitHub {
      owner = "nvim-neorg";
      repo = "tree-sitter-norg";
      rev = "600874e0825a62c3822ff04e5d0efbac8d7e380c";
      sha256 = "1l200zd7xfhifaxq3i810j2bxpvm0yfgf6x2i2d83l34z32q3p4x";
    };
    norg-parser = pkgs.callPackage ./tree-sitter { tree-sitter-norg-source = norg-source; };

    shell = pkgs.mkShell {
      packages = [
        pkgs.swiProlog
        pkgs.tree-sitter
      ];
      PYTHONPATH = "${pkgs.swiProlog}/lib/swipl/lib";
      PKG_CONFIG_PATH = "${pkgs.swiProlog}/share/pkgconfig:${pkgs.tree-sitter}/lib/pkgconfig";
      NORG_PARSER_SOURCE = "${norg-source}/src";
    };

    piper = pkgs.callPackage ./piper {};
  in {
    devShells."x86_64-linux" = {
      default = shell;
    };

    packages."x86_64-linux" = {
      korrvigs-norg-parser = norg-parser;
      inherit piper;
    };
  };
}

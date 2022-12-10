{
  description = "Experimenting with asciidoctor ruby API";

  outputs = {
    self,
    nixpkgs,
  }: let
    pkgs = import nixpkgs {system = "x86_64-linux";};
    gems = pkgs.bundlerEnv {
      name = "asciidoc-knowledge-database";
      gemdir = ./.;
    };
    shell = pkgs.mkShell {
      packages = [gems.wrappedRuby gems pkgs.swiProlog pkgs.python3];
      PYTHONPATH = "${pkgs.swiProlog}/lib/swipl/lib";
    };
    update = pkgs.mkShell {
      packages = [pkgs.bundix pkgs.ruby];
    };
  in {
    devShells."x86_64-linux" = {
      default = shell;
      update = update;
    };
  };
}

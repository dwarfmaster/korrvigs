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
      packages = [gems.wrappedRuby gems];
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

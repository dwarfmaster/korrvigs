{
  buildGoModule,
  lib,
}:
buildGoModule {
  pname = "korrvigs-tui";
  version = "0.1";

  src = ./.;
  vendorHash = "sha256-zy16w/eItGXn5qbsUzdPkXYBr9z4EGd0WVIKeKkurN0=";

  meta = {
    description = "A command line interface for korrvigs";
    homepage = "https://github.com/dwarfmaster/korrvigs";
    license = lib.licenses.mit;
    maintainers = [lib.maintainers.dwarfmaster];
  };
}

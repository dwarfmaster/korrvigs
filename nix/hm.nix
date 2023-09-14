{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkOption mkEnableOption types;
  cfg = config.services.korrvigs;
  basicType = types.enum ["entry" "number" "string"];
  typesFile = pkgs.writeTextFile {
    name = "types.csv";
    text =
      lib.concatStrings
      (lib.mapAttrsToList
        (pred: type: "${pred},${lib.concatStringsSep "," type}\n")
        cfg.predicates);
  };
  rulesFile = pkgs.writeTextFile {
    name = "rules.pl";
    text = lib.concatStringsSep "\n" cfg.rules;
  };
in {
  options.services.korrvigs = {
    enable = mkEnableOption "Korrvigs assistant";
    package = mkOption {
      description = "Package containing korrvigs server";
      type = types.package;
      default = pkgs.korrvigs-server;
    };

    typesFile = mkOption {
      description = "Path to csv files containing predicates";
      type = types.str;
      readOnly = true;
    };
    rulesFile = mkOption {
      description = "Path to datalog files containing rules";
      type = types.str;
      readOnly = true;
    };
    socketPath = mkOption {
      description = "Path to socket to open";
      type = types.str;
      default = "${config.xdg.stateHome}/korrvigs/server.sock";
    };
    wikiDir = mkOption {
      description = "Path of the wiki";
      type = types.str;
    };
    cacheDir = mkOption {
      description = "Directory for runtime files used by the server";
      type = types.str;
      default = "${config.xdg.stateHome}/korrvigs/cache";
    };

    predicates = mkOption {
      description = "Predicates to enable in wiki";
      type = types.attrsOf (types.listOf basicType);
      default = {};
    };
    rules = mkOption {
      description = "Rules to add";
      type = types.listOf types.str;
      default = [];
    };
  };

  config = lib.mkIf cfg.enable {
    services.korrvigs.typesFile = "${typesFile}";
    services.korrvigs.rulesFile = "${rulesFile}";

    systemd.user.services.korrvigs = {
      Unit = {
        Description = "Korrvigs assistant";
        After = ["basic.target"];
      };

      Service = {
        Type = "simple";
        Restart = "no";
        ExecStart =
          "${cfg.package}/bin/korrvigs-server"
          + " --socket ${cfg.socketPath}"
          + " --types ${cfg.typesFile}"
          + " --rules ${cfg.rulesFile}"
          + " --wiki ${cfg.wikiDir}"
          + " --cache ${cfg.cacheDir}";
        Environment = "PATH=${pkgs.souffle}/bin";
      };

      Install.WantedBy = ["default.target"];
    };
  };
}

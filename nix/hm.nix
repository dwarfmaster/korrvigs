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
    name = "types.sexp";
    text =
      "(\n"
      + (lib.concatStrings
        (lib.mapAttrsToList
          (pred: type: " ${pred} (${lib.concatStringsSep " " type})\n")
          cfg.predicates))
      + ")";
  };
  rulesFile = pkgs.writeTextFile {
    name = "rules.pl";
    text = lib.concatStringsSep "\n" cfg.rules;
  };
in {
  options.services.korrvigs = {
    enable = mkEnableOption "Korrvigs assistant";
    package = mkOption {
      description = "Package containing korrvigs racket code";
      type = types.package;
      default = pkgs.korrvigs-server;
    };
    racket = mkOption {
      description = "Which package to use for racket";
      type = types.package;
      default = pkgs.racket;
    };

    typesFile = mkOption {
      description = "Path to sexp files containing predicates";
      type = types.str;
      readOnly = true;
    };
    rulesFile = mkOption {
      description = "Path to prolog files containing rules";
      type = types.str;
      readOnly = true;
    };
    socketPath = mkOption {
      description = "Path to socket to open";
      type = types.str;
      default = "${config.xdg.stateHome}/korrvigs/server.sock";
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
        After = ["basic.target" "korrvigs-setup.service"];
        Requires = ["korrvigs-setup.service"];
      };

      Service = {
        Type = "simple";
        Restart = "on-failure";
        ExecStart = "${cfg.package}/bin/main.rktl --socket ${cfg.socketPath} --types ${cfg.typesFile} --rules ${cfg.rulesFile}";
      };

      Install.WantedBy = ["default.target"];
    };

    systemd.user.tmpfiles.rules = [
      "d ${builtins.dirOf cfg.socketPath} 0750 luc users 0 -"
    ];

    systemd.user.services.korrvigs-setup = {
      Unit = {
        Description = "Setup racket for korrvigs";
      };

      Service = {
        Type = "oneshot";
        Restart = "on-failure";
        ExecStart = "${cfg.racket}/bin/raco pkg install --deps search-auto --skip-installed megaparsack uuid";
      };
    };
  };
}

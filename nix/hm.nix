{ config, lib, pkgs, ... }:

let
  inherit (lib) mkOption mkEnableOption types;
  cfg = config.services.korrvigs;

  configFile = pkgs.writeText "config.pl" ''
    ${lib.concatMapStringsSep "\n"
        (p: "user:file_search_path(korrvigs, \"${p}\").")
        cfg.modulePaths}
    ${lib.concatMapStringsSep "\n"
        (p: "user:file_search_path(foreign, \"${p}\").")
        cfg.foreignPaths}

    data_dir("${cfg.dataDir}").
    piper("${cfg.piper}").

    ${cfg.extraConfig}
  '';
in {
  options.services.korrvigs = {
    enable = mkEnableOption "korrvigs assistant";
    package = mkOption {
      description = "Package containing korrvigs prolog files";
      type = types.package;
      default = pkgs.korrvigs;
    };
    prolog = mkOption {
      description = "Which package to use for prolog";
      type = types.package;
      default = pkgs.swiProlog;
    };

    configFile = mkOption {
      description = "Path to the generated config file";
      type = types.str;
      readOnly = true;
    };

    modulePaths = mkOption {
      description = "Paths to lookup module in";
      type = types.listOf types.str;
      default = [];
    };

    foreignPaths = mkOption {
      description = "Paths to lookup compiled c functions in";
      type = types.listOf types.str;
      default = [];
    };

    extraModules = mkOption {
      description = "List of modules to load in korrvigs instance";
      type = types.listOf (types.either types.package types.str);
      default = [];
    };

    dataDir = mkOption {
      description = "Path to the directory of wiki files";
      type = types.str;
    };

    piper = mkOption {
      description = "Path to the piper executable";
      type = types.str;
      default = "${pkgs.piper}/bin/piper";
    };

    extraConfig = mkOption {
      description = "Extra prolog code to add to the config file";
      type = types.lines;
      default = "";
    };
  };

  config = lib.mkIf cfg.enable {
    services.korrvigs = {
      configFile = "${configFile}";
      extraModules = [ pkgs.korrvigs-norg-parser pkgs.korrvigs-posix ];
      modulePaths = builtins.map (mod: "${mod}/lib/korrvigs/modules") cfg.extraModules
        ++ [ "${cfg.package}/lib/korrvigs/modules" ];
      foreignPaths = builtins.map (mod: "${mod}/lib/korrvigs/foreign") cfg.extraModules;
    };

    systemd.user.services.korrvigs = {
      Unit = {
        Description = "Korrvigs assistant";
        After = ["basic.target"];
      };

      Service = {
        Type = "simple";
        Restart = "on-failure";
        ExecStart = "${cfg.prolog}/bin/swipl -q --signals=false -l ${cfg.configFile} -f ${cfg.package}/lib/korrvigs.pl -t main";
      };

      Install.WantedBy = ["default.target"];
    };
  };
}

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

    ${lib.concatStringsSep "\n"
        (lib.mapAttrsToList (name: v: "nix_constant('${name}', \"${v}\").") cfg.constants)}
    ${if cfg.constants == [] then "nix_constants(_, _) :- fail." else ""}

    ${cfg.extraConfig}
  '';

  moduleFiles = pkgs.runCommandLocal "korrvigs-modules" {} ''
    mkdir -p $out
    ${lib.concatStrings
        (lib.mapAttrsToList (name: p: "install -m444 ${p} $out/${name}.pl\n") cfg.extraModules)}
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

    extraModulePackages = mkOption {
      description = "List of modules to load in korrvigs instance";
      type = types.listOf (types.either types.package types.str);
      default = [];
    };

    extraModules = mkOption {
      description = "List of files to modules contained in single files";
      type = types.attrsOf types.path;
      default = {};
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

    constants = mkOption {
      description = "Define nix predicate on entries";
      type = types.attrsOf types.str;
      default = { };
    };
  };

  config = lib.mkIf cfg.enable {
    services.korrvigs = {
      configFile = "${configFile}";
      extraModulePackages = [ pkgs.korrvigs-norg-parser pkgs.korrvigs-posix ];
      modulePaths = builtins.map (mod: "${mod}/lib/korrvigs/modules") cfg.extraModulePackages
        ++ [ "${cfg.package}/lib/korrvigs/modules" "${moduleFiles}" ];
      foreignPaths = builtins.map (mod: "${mod}/lib/korrvigs/foreign") cfg.extraModulePackages;
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

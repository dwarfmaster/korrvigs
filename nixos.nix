overlay: {
  config,
  pkgs,
  lib,
  ...
}: let
  inherit (lib) types mkEnableOption mkOption mkMerge mkIf;
  colorOption = d:
    mkOption {
      type = types.str;
      description = "An hexadecimal rgb color with the hash";
      default = d;
    };

  cfg = config.programs.korrvigs;
  server = cfg.server;
  psql = cfg.postgresql;
  nginx = server.nginx;
  postgre = config.services.postgresql;

  configContent =
    {
      inherit
        (cfg.theme)
        base00
        base01
        base02
        base03
        base04
        base05
        base06
        base07
        base08
        base09
        base0A
        base0B
        base0C
        base0D
        base0E
        base0F
        ;
      credentials = cfg.credentialFile;
      inherit (cfg) connectionSpec root capture;
      inherit (server) port;
      staticDir = "${pkgs.korrvigs-static.override {inherit (cfg) theme;}}";
    }
    // (
      if builtins.isNull nginx.staticDomain
      then {}
      else {staticRedirect = "https://${nginx.staticDomain}";}
    );
  pgUser = config.services.postgresql.superUser;

  dependencies = [
    pkgs.file
    pkgs.exiftool
    pkgs.poppler_utils
    pkgs.pandoc
    pkgs.imagemagick
    pkgs.ffmpeg
  ];
in {
  options.programs.korrvigs = {
    enable = mkEnableOption "korrvigs app";
    package = mkOption {
      type = types.package;
      description = "The korrvigs package";
      default = pkgs.korrvigs;
    };
    user = mkOption {
      type = types.str;
      description = "Name of the user to enable it for";
    };
    root = mkOption {
      type = types.str;
      description = "Path to the git-annexed root of korrvigs files";
    };
    capture = mkOption {
      type = types.str;
      description = "Path to the capture directory";
    };
    connectionSpec = mkOption {
      type = types.str;
      description = "Connection string to the postgreSQL database";
    };
    theme = {
      base00 = colorOption "#231e18";
      base01 = colorOption "#302b25";
      base02 = colorOption "#48413a";
      base03 = colorOption "#9d8b70";
      base04 = colorOption "#b4a490";
      base05 = colorOption "#cabcb1";
      base06 = colorOption "#d7c8bc";
      base07 = colorOption "#e4d4c8";
      base08 = colorOption "#d35c5c";
      base09 = colorOption "#ca7f32";
      base0A = colorOption "#e0ac16";
      base0B = colorOption "#b7ba53";
      base0C = colorOption "#6eb958";
      base0D = colorOption "#88a4d3";
      base0E = colorOption "#bb90e2";
      base0F = colorOption "#b49368";
    };
    postgresql = {
      enable = mkEnableOption "korrvigs postgreSQL database";
      database = mkOption {
        type = types.str;
        description = "Name of the database to use";
        default = "korrvigs";
      };
    };
    credentialFile = mkOption {
      type = types.nullOr types.str;
      description = "Path to the credential file";
      default = null;
    };

    server = {
      enable = mkEnableOption "Korrvigs server";
      port = mkOption {
        type = types.port;
        description = "Port to server korrvigs on";
        default = 3000;
      };
      nginx = {
        enable = mkEnableOption "nginx korrvigs proxy";
        passwordHash = mkOption {
          type = types.str;
          description = "Hash of the password for the user. Must be a valid htpasswd hash";
        };
        domain = mkOption {
          type = types.str;
          description = "Domain to serve korrvigs from.";
        };
        staticDomain = mkOption {
          type = types.nullOr types.str;
          description = "Domain to serve static files from.";
          default = null;
        };
      };
    };
  };

  config = mkMerge [
    {
      nixpkgs.overlays = [overlay];
    }
    (mkIf cfg.enable {
      home-manager.users.${cfg.user} = {
        xdg.configFile."korrvigs/config.json".text = builtins.toJSON configContent;
        home.packages = [cfg.package] ++ dependencies;
      };
    })

    (mkIf (cfg.enable && psql.enable) {
      services.postgresql = {
        enable = true;
        extensions = [config.services.postgresql.package.pkgs.postgis];
        ensureDatabases = [psql.database];
        ensureUsers = [
          {name = "luc";}
        ];
      };
      programs.korrvigs.connectionSpec = "dbname='${psql.database}'";
      systemd.services.korrvigs-setup-pgsql = {
        serviceConfig.Type = "oneshot";
        requiredBy = ["korrvigs.service"];
        before = ["korrvigs.service"];
        after = ["postgresql.service"];
        serviceConfig.User = "postgres";
        environment.PSQL = "psql --port=${toString postgre.settings.port}";
        path = [postgre.package];
        script = ''
          $PSQL ${pgUser} -d ${psql.database} -c 'CREATE EXTENSION IF NOT EXISTS postgis'
          $PSQL ${pgUser} -d ${psql.database} -c 'CREATE EXTENSION IF NOT EXISTS address_standardizer'
          $PSQL ${pgUser} -d ${psql.database} -f ${./doc/schema.pgsql}
          $PSQL ${pgUser} -d ${psql.database} -c 'GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO ${cfg.user}'
        '';
      };
    })

    (mkIf (cfg.enable && server.enable) {
      systemd.services.korrvigs = {
        description = "Korrvigs server";
        wantedBy = ["multi-user.target"];
        after = ["postgresql.service"];
        before = ["nginx.service"];

        serviceConfig = {
          User = cfg.user;
          Group = config.users.users.${cfg.user}.group;
          Type = "simple";
        };

        script = "${cfg.package}/bin/korr server";
        path = dependencies;
      };
    })

    (mkIf (cfg.enable && server.enable && nginx.enable) {
      services.nginx = {
        enable = true;
        virtualHosts.${nginx.domain} = {
          enableACME = true;
          forceSSL = true;
          locations."/" = {
            proxyPass = "http://localhost:${builtins.toString server.port}";
            recommendedProxySettings = true;
            basicAuthFile = pkgs.writeText "korrvigs-htpasswd" ''
              ${cfg.user}:${nginx.passwordHash}
            '';
            extraConfig = ''
              client_max_body_size 0;
            '';
          };
          locations."/public" = {
            proxyPass = "http://localhost:${builtins.toString server.port}";
            recommendedProxySettings = true;
          };
        };
      };
    })

    (mkIf (cfg.enable && server.enable && nginx.enable && !(builtins.isNull nginx.staticDomain)) {
      services.nginx.virtualHosts.${nginx.staticDomain} = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          root = pkgs.korrvigs-static;
          extraConfig = ''
            add_header Access-Control-Allow-Origin https://${nginx.domain};
          '';
        };
      };
    })
  ];
}

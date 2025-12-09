{ config, lib, pkgs, outputs, typed, ...}:

with typed;

let
  cfg = config.sshd;
  cssh = typed.ssh;
  concatLines = list: builtins.concatStringsSep "\n" list;
  prefixLines = mapper: list: concatLines (map mapper list);
  configPath = "ssh/sshd_config";
  keysFolder = "/etc/ssh";
  supportedKeysTypes = [
    "rsa"
    "ed25519"
  ];
  pathOfKeyOf = type: "${keysFolder}/ssh_host_${type}_key";
  generateKeyOf = type: ''
    ${pkgs.openssh}/bin/ssh-keygen \
      -t "${type}" \
      -f "${pathOfKeyOf type}" \
      -N ""
  '';
  generateKeyWhenNeededOf = type: ''
    if [ ! -f ${pathOfKeyOf type} ]; then
      mkdir --parents ${keysFolder}
      ${generateKeyOf type}
    fi
  '';

  sshd-start = pkgs.writeScriptBin "sshd-start" ''
    #!${pkgs.runtimeShell}

    PID=$(pgrep sshd)
    if [ -n "$PID" ]; then
      exit 0
    fi

    ${prefixLines generateKeyWhenNeededOf supportedKeysTypes}

    ${pkgs.openssh}/bin/sshd -f "/etc/ssh/sshd_config" -E "/etc/ssh/sshd.log"
  '';
in {
  options.sshd = {
    enable = mkEnable ''
      Whether to enable the OpenSSH secure shell daemon, which
      allows secure remote logins.
    '';

    ports = lib.mkOption {
      type = lib.types.listOf lib.types.port;
      default = [ 8022 ];
      description = ''
        Specifies on which ports the SSH daemon listens.
      '';
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      environment.etc = {
        "${configPath}".text = ''
          ${prefixLines (port: "Port ${toString port}") cfg.ports}

          AuthorizedKeysFile /etc/ssh/authorized_keys.d/%u
          X11Forwarding yes

          LogLevel VERBOSE
          SysLogFacility USER
          PidFile /etc/ssh/sshd.pid
        '';
      };

      environment.packages = [
        sshd-start
        pkgs.openssh
      ];

      build.activationAfter.sshd = ''
        SERVER_PID=$(${pkgs.toybox}/bin/pgrep sshd)
        if [ -z "$SERVER_PID" ]; then
          $DRY_RUN_CMD ${sshd-start}/bin/sshd-start
        fi
      '';
    }
  ]);
}

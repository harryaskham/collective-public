{ config, lib, pkgs, outputs, untyped, ...}:

with untyped.clib;

let
  cfg = config.sshd;
  cssh = untyped.ssh;
  concatLines = list: builtins.concatStringsSep "\n" list;
  prefixLines = mapper: list: concatLines (map mapper list);
  configPath = "ssh/sshd_config";
  keysFolder = "/etc/ssh";
  authorizedKeysFolder = "/etc/ssh/authorized_keys.d";
  supportedKeysTypes = [
    "rsa"
    "ed25519"
  ];
  sshd-start-bin = "sshd-start";
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
  appendAuthorizedKeysFiles = authorizedKeysFile: ''
    cat ${authorizedKeysFile} >${authorizedKeysFolder}/${config.user.userName}
  '';

  sshd-start = pkgs.writeScriptBin sshd-start-bin ''
    #!${pkgs.runtimeShell}
    ${prefixLines generateKeyWhenNeededOf supportedKeysTypes}

    if [ ! -f "${authorizedKeysFolder}/${config.user.userName}" ]; then
      mkdir --parents "${authorizedKeysFolder}"
      ${prefixLines appendAuthorizedKeysFiles cfg.authorizedKeysFiles}
      ${lib.optionalString cfg.includeCollectiveSSHKeys (prefixLines appendAuthorizedKeysFiles ["/etc/ssh/collective_keys"])}
    fi

    ${pkgs.openssh}/bin/sshd \
      -f "/etc/${configPath}"
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

    includeCollectiveSSHKeys = mkDefaultEnable "Include the clib ssh keys";

    authorizedKeysFiles = lib.mkOption {
      type = lib.types.listOf (lib.types.oneOf [ lib.types.path lib.types.str ]);
      default = [ ];
      description = ''
        Specify the rules for which files to read on the host.

        This is an advanced option.

        These are paths relative to the host root file system or home
        directories and they are subject to certain token expansion rules.
        See AuthorizedKeysFile in man sshd_config for details.
      '';
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    (lib.mkIf cfg.includeCollectiveSSHKeys {
      environment.etc."ssh/collective_keys".text = concatLines cssh.authorizedKeys;
    })

    {
      environment.etc = {
        "${configPath}".text = ''
          ${prefixLines (port: "Port ${toString port}") cfg.ports}

          AuthorizedKeysFile ${authorizedKeysFolder}/%u
          X11Forwarding yes

          LogLevel VERBOSE
        '';
      };

      environment.packages = [
        sshd-start
        pkgs.openssh
      ];

      build.activationAfter.sshd = ''
        SERVER_PID=$(${pkgs.procps}/bin/ps -a | ${pkgs.toybox}/bin/grep sshd || true)
        if [ -z "$SERVER_PID" ]; then
          $DRY_RUN_CMD ${sshd-start}/bin/${sshd-start-bin}
        fi
      '';

      session.actions.sshd = {
        checkRunning = ''
          ${pkgs.procps}/bin/ps -a | ${pkgs.toybox}/bin/grep sshd
        '';
        start = ''
          ${sshd-start}/bin/${sshd-start-bin}
        '';
      };
    }
  ]);
}

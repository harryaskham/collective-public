{ config, lib, pkgs, outputs, typed, ...}:

# Since NOD can be preempted by system restart, OOM, etc, anything started in
# build.activationAfter may no longer be running. Instead, idempotently check for and start up
# any daemons that may have been stopped.
# TODO: systemd on NOD?

with typed;
with lib;

let
  cfg = config.session;
  action = types.submodule {
    options = {
      checkRunning = mkOption {
        type = types.str;
        description = "Script that returns 0 iff action is running";
      };
      start = mkOption {
        type = types.str;
        description = "Script that triggers action to run in the background.";
      };
    };
  };
  actionScriptName = name: "nix-on-droid-session-start/check-and-run-${name}.sh";
  actionScriptPath = name: "/etc/${actionScriptName name}";
in {
  options.session = {
    enable = mkDefaultEnable "Whether to enable session management";
    actions = mkOption {
      type = types.attrsOf types.action;
      default = {};
      description = ''
        Actions to check and run on session start.
      '';
    };
  };

  config = (mkIf cfg.enable {

    agnostic.environment.etc = concatForAttrs cfg.actions (name: action: {
      ${actionScriptName name}.text = _b_ ''
        #!${pkgs.bash}/bin/bash
        
        function check_running() {
          ${_h_ action.checkRunning}
        }

        function start_action() {
          ${_h_ action.start}
        }

        function main() {
          if ! check_running; then
            start_action
          fi
        fi
      '';
    });

    shell.init = ''
      mkdir -p /tmp/nix-on-droid-session-start
      ${_h_ (_ls_ (forAttrsToList cfg.actions (name: action: _b_ ''
        nohup bash "${actionScriptPath name}" > /tmp/nix-on-droid-session-start/${name}.out 2>&1 &
      '')))}
    '';
  });
}

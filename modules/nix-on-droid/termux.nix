{ config, lib, pkgs, outputs, untyped, ...}:

# Convenience scripts for exposing actions from Nix-on-Droid to Termux.
# Enables easier composition with Tasker for one-click actions like
# starting X11 and switching between NOD, Termux and Termux-X11.
# Where commands can't be run as actions (i.e. sending commands to NOD),
# they are instead copied to the clipboard and NOD is switched to for 
# a manual paste.

with untyped.clib;

let
  cfg = config.termux;
in {
  options.termux = {
    enable = mkEnable ''
      Whether to enable extra Termux-specific integration.
    '';
    x11 = mkNestedEnable "Whether to enable x11 and associated scripts";
    sharedDir = {
      enable = mkEnable "Whether to enable shared directory";
      path = mkOption {
        type = lib.types.str;
        default = "/storage/emulated/0/shared";
        description = ''
          The directory in shared storage to output files.
        '';
      };
      copy = mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = {};
        description = ''
          Files to copy to the shared directory.
          Keyed by relative path in shared directory.
          Values are absolute paths accessible in build.activationAfter scripts.
        '';
      };
    };
  };

  config = (lib.mkIf cfg.enable (lib.mkMerge [

    (lib.mkIf cfg.sharedDir.enable {
      build.activationAfter = 
        lib.concatMapAttrs 
          (dst: src: {
            "copy-to-shared-dir__${src}__${dst}" = ''
              SRC=${toShellValue src}
              DEST="${toShellValue cfg.sharedDir}/${toShellValue dst}"
              echo "Copying $SRC to $DEST"
              mkdir -p $(dirname "$DEST")
              cp -Lr "$SRC" "$DEST"
            ''
          }) 
          cfg.sharedDir.copy;
    })

    # Fresh Termux installation using .termux defined by NOD.
    {
      environment.etc."termux/bootstrap-from-nixondroid.sh" = {
        text = ''
          # Run in fresh Termux install after Nix-on-Droid is set up to
          # install Termux themes/settings from the NOD home-manager config:
          # 
          # $ ${cfg.sharedDir}/bootstrap-from-nixondroid.sh

          rm -rf ~/.termux
          cp -Lr ${cfg.sharedDir}/.termux ~/.termux
        '';
      };

      termux.sharedDir.copy = {
        ".termux" = "/data/data/com.termux.nix/files/home/.termux";
        ".bootstrap-from-nixondroid.sh" = "/etc/termux/bootstrap-from-nixondroid.sh";
      };
    }

    (lib.mkIf cfg.x11.enable {
      environment.etc."termux-x11/x11-start-in-termux.sh" = {
        text = ''
          # Run in Termux (manually or via Tasker shortcut)
          # Starts X11 server and switches to NOD in order to launch desktop.

          echo "Acquiring wake-lock"
          termux-wake-lock

          echo "Launching nix-on-droid in 3s"
          (sleep 3; am start --user 0 -n com.termux.nix/com.termux.app.TermuxActivity) &

          echo "Starting PulseAudio server"
          LD_PRELOAD=/system/lib64/libskcodec.so pulseaudio \
            --start \
            --exit-idle-time=-1 \
            --load="module-native-protocol-tcp auth-ip-acl=127.0.0.1 auth-anonymous=1"

          echo "Starting X11 listener"
          termux-x11 :1 -listen tcp -ac -dpi 192 \
            +extension MIT-SHM \
            +extension RANDR \
            +extension GLX

          wait
        '';
      };
      termux.sharedDir.copy."termux-x11" = "/etc/termux-x11";
    })

  ]));
}

{ config, lib, pkgs, outputs, untyped, typed, ...}:

# Convenience scripts for exposing actions from Nix-on-Droid to Termux.
# Enables easier composition with Tasker for one-click actions like
# starting X11 and switching between NOD, Termux and Termux-X11.
# Where commands can't be run as actions (i.e. sending commands to NOD),
# they are instead copied to the clipboard and NOD is switched to for 
# a manual paste.

with lib;
with typed;

let
  cfg = config.termux;
  toShellValue = typed.toShellValueUnsafe;
in {
  options.termux = {
    enable = mkEnable ''
      Whether to enable extra Termux-specific integration.
    '';
    colors = mkOption {
      type = typed.colors.schemeType;
      default = typed.colors.schemes.nord;
      description = "Colors for the terminal";
    };
    settings = {
      hideExtraKeys = mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          Whether to hide extra keys.
        '';
      };
      useBlackUI = mkOption {
        type = lib.types.nullOr lib.types.bool;
        default = null;
        description = ''
          Whether to use force black UI. Null for default.
        '';
      };
      allowExternalApps = mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          Whether to allow external apps.
        '';
      };
      fullscreen = mkOption {
        type = lib.types.bool;
        default = true;
        description = ''
          Whether to allow fullscreen.
        '';
      };
      margin = {
        vertical = mkOption {
          type = lib.types.int;
          default = 8;
          description = ''
            The vertical margin.
          '';
        };
        horizontal = mkOption {
          type = lib.types.int;
          default = 8;
          description = ''
            The horizontal margin.
          '';
        };
      };
      shortcut = {
        createSession = mkOption {
          type = lib.types.str;
          default = "ctrl + alt + c";
          description = ''
            The shortcut to create a new session.
          '';
        };
        nextSession = mkOption {
          type = lib.types.str;
          default = "ctrl + alt + n";
          description = ''
            The shortcut to go to the next session.
          '';
        };
        previousSession = mkOption {
          type = lib.types.str;
          default = "ctrl + alt + p";
          description = ''
            The shortcut to go to the previous session.
          '';
        };
      };
      bellCharacter = mkOption {
        type = lib.types.str;
        default = "ignore";
        description = ''
          The character to use for the bell.
        '';
      };
    };
    x11 = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = config.bootstrap.desktop.enable;
        description = ''
          Whether to enable x11 and associated scripts.
        '';
      };
    };
    sharedDir = {
      enable = mkEnable "Whether to enable shared directory";
      path = lib.mkOption {
        type = lib.types.str;
        default = "/storage/emulated/0/shared";
        description = ''
          The directory in shared storage to output files.
        '';
      };
      copy = lib.mkOption {
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
            "copy-to-shared-dir__${src}" = ''
              SRC=${toShellValue src}
              DEST="${toShellValue cfg.sharedDir.path}/"
              echo "Copying $SRC to $DEST"
              mkdir -p "${toShellValue cfg.sharedDir.path}"
              cp -Lr "$SRC" "$DEST"
            '';
          }) 
          cfg.sharedDir.copy;
    })

    # Make settings that can be symlinked to home.
    {
      # NOD manages the .termux colors.properties and font.ttf for us.
      terminal.font =
        "${pkgs.nerd-fonts.fira-code}/share/fonts/truetype/NerdFonts/FiraCode/FiraCodeNerdFont-Regular.ttf";
      terminal.colors = with untyped.colors; forNixOnDroid config.bootstrap.colors;

      # Write our own settings out.
      environment.etc."termux/termux.properties" = {
        text = joinOptionalLines [
          (optionalString cfg.settings.hideExtraKeys "extra-keys=[]")
          (optionalString (cfg.settings.useBlackUI != null) "use-black-ui=${boolToString cfg.settings.useBlackUI}")
          "allow-external-apps=${boolToString cfg.settings.allowExternalApps}"
          "fullscreen=${boolToString cfg.settings.fullscreen}"
          "shortcut.create-session=${cfg.settings.shortcut.createSession}"
          "shortcut.next-session=${cfg.settings.shortcut.nextSession}"
          "shortcut.previous-session=${cfg.settings.shortcut.previousSession}"
          "bell-character=${cfg.settings.bellCharacter}"
          "terminal-margin-horizontal=${toString cfg.settings.margin.horizontal}"
          "terminal-margin-vertical=${toString cfg.settings.margin.vertical}"
        ];
      };
    }

    # Fresh Termux installation using .termux defined by NOD.
    {
      environment.etc."termux/bootstrap-from-nixondroid.sh" = {
        text = ''
          # Run in fresh Termux install after Nix-on-Droid is set up to
          # install Termux themes/settings from the NOD home-manager config:
          # 
          # $ ${cfg.sharedDir.path}/bootstrap-from-nixondroid.sh

          rm -rf ~/.termux
          cp -Lr ${cfg.sharedDir.path}/.termux ~/.termux
        '';
      };

      termux.sharedDir.copy = {
        # Export our NOD-managed .termux dir
        ".termux" = "/data/data/com.termux.nix/files/home/.termux";
        # Export our Termux bootstrap script installing .termux etc in fresh Termux
        ".bootstrap-from-nixondroid.sh" = "/etc/termux/bootstrap-from-nixondroid.sh";
      };
    }

    (lib.mkIf cfg.x11.enable {
      environment.etc."termux-x11/x11-start-in-termux.sh" = {
        text = ''
          # Run in Termux (manually or via Tasker shortcut)
          # Starts X11 server and switches to NOD in order to launch desktop.

          if ! [ -x "$(command -v termux-x11)" ]; then
            echo "Installing termux-x11-nightly"
            pkg i x11-repo && pkg i termux-x11-nightly
          fi

          if ! [ -x "$(command -v pulseaudio)" ]; then
            echo "Installing pulseaudio"
            pkg i pulseaudio
          fi

          echo "Acquiring wake-lock"
          termux-wake-lock

          echo "Launching nix-on-droid in 3s"
          (sleep 3; am start --user 0 -n com.termux.nix/com.termux.app.TermuxActivity) &

          echo "Starting PulseAudio server"
          WITH_LD_PRELOAD="$LD_PRELOAD"
          if [ -f /system/lib64/libskcodec.so ]; then
            echo "Using skcodec for PulseAudio"
            WITH_LD_PRELOAD="/system/lib64/libskcodec.so"
          else
            echo "No skcodec found, using default PulseAudio"
          fi

          LD_PRELOAD="$WITH_LD_PRELOAD" pulseaudio \
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

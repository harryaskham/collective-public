{ config, lib, pkgs, outputs, untyped, typed, ...}:

# Termux integration for Nix-on-Droid.
#
# Provides:
#   - Termux settings (colors, font, properties)
#   - Shared directory for copying files to regular Termux
#   - Bootstrap script for fresh Termux installs
#   - termux-exec: TCP client/daemon for running native Termux commands from proot
#   - rish: Shizuku remote shell wrappers via termux-exec
#   - Termux:Boot scripts for auto-starting services on device boot
#   - X11 scripts for Termux-X11 desktop mode

with lib;
with typed;

let
  cfg = config.termux;
  toShellValue = typed.toShellValueUnsafe;

  port = toString cfg.exec.port;
  host = cfg.exec.host;

  termux-exec-pkg = pkgs.termux-exec.override {
    inherit port host;
  };

  termux-exec-alias-pkg = pkgs.writeShellScriptBin "tx" ''
    exec "${lib.getExe termux-exec-pkg}" "$@"
  '';

  termux-exec-record-pkg = pkgs.writeShellScriptBin "tx-rec" ''
    exec "${lib.getExe termux-exec-pkg}" termux-microphone-record $@
  '';

  termux-exec-player-pkg = pkgs.writeShellScriptBin "tx-player" ''
    exec "${lib.getExe termux-exec-pkg}" termux-media-player $@
  '';

  termux-exec-play-pkg = pkgs.writeShellScriptBin "tx-play" ''
    exec "${lib.getExe termux-exec-player-pkg}" play $@
  '';

  termux-exec-pause-pkg = pkgs.writeShellScriptBin "tx-pause" ''
    exec "${lib.getExe termux-exec-player-pkg}" pause
  '';

  termux-exec-stop-pkg = pkgs.writeShellScriptBin "tx-stop" ''
    exec "${lib.getExe termux-exec-player-pkg}" stop
  '';

  # rish wrapper: sends a self-contained dex-loading command to termux-exec
  rish-pkg = pkgs.stdenvNoCC.mkDerivation {
    pname = "rish-collective";
    version = "0.4.0";
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out/bin

      cat > $out/bin/rish <<'RISH'
      #!/usr/bin/env bash
      set -euo pipefail

      RISH_PKG="''${1:?Usage: rish <package-name> [args...]}"
      shift

      [ -z "''${RISH_APPLICATION_ID:-}" ] && RISH_APPLICATION_ID="$RISH_PKG"

      # Everything inside the heredoc runs daemon-side in native Termux.
      exec @termux_exec@/bin/termux-exec bash -c "$(cat <<'DAEMON_CMD'
      set -euo pipefail

      # Positional args from caller: $1=RISH_PKG $2=RISH_APPLICATION_ID $3..=user args
      export RISH_PKG="$1" RISH_APPLICATION_ID="$2"
      shift 2

      # Find the dex (Termux-side paths only)
      DEX=""
      for candidate in \
        /data/data/com.termux/files/home/rish/rish_shizuku.dex \
        /storage/emulated/0/shared/rish/rish_shizuku.dex; do
        if [ -f "$candidate" ]; then
          DEX="$candidate"
          break
        fi
      done

      if [ -z "$DEX" ]; then
        echo "rish: cannot find rish_shizuku.dex" >&2
        echo "Place it in one of:" >&2
        echo "  /data/data/com.termux/files/home/rish/" >&2
        echo "  /storage/emulated/0/shared/rish/" >&2
        exit 1
      fi

      # Android 14+ (SDK 34+): dex must not be writable
      SDK="$(getprop ro.build.version.sdk 2>/dev/null || echo 0)"
      if [ "$SDK" -ge 34 ] && [ -w "$DEX" ]; then
        chmod 400 "$DEX" 2>/dev/null || true
        if [ -w "$DEX" ]; then
          # Can't chmod (shared storage / FAT) - copy to Termux private dir
          PRIVATE_DEX="/data/data/com.termux/files/home/rish/rish_shizuku.dex"
          mkdir -p "$(dirname "$PRIVATE_DEX")"
          cp "$DEX" "$PRIVATE_DEX"
          chmod 400 "$PRIVATE_DEX"
          DEX="$PRIVATE_DEX"
          if [ -w "$DEX" ]; then
            echo "rish: cannot make dex non-writable (required for Android 14+)" >&2
            exit 1
          fi
        fi
      fi

      exec /system/bin/app_process \
        -Djava.class.path="$DEX" \
        /system/bin \
        --nice-name=rish \
        rikka.shizuku.shell.ShizukuShellLoader "$@"
      DAEMON_CMD
      )" rish "$RISH_PKG" "$RISH_APPLICATION_ID" "$@"
      RISH

      substituteInPlace $out/bin/rish \
        --replace-warn '@termux_exec@' '${termux-exec-pkg}'
      chmod +x $out/bin/rish

      # Convenience aliases - daemon runs as com.termux, so Shizuku
      # identity must always be com.termux regardless of caller.
      for name in rish-nix rish-termux; do
        cat > "$out/bin/$name" <<ALIAS
      #!/usr/bin/env bash
      exec $out/bin/rish "com.termux" "\$@"
      ALIAS
        chmod +x "$out/bin/$name"
      done
    '';
  };
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
        description = "Whether to hide extra keys.";
      };
      useBlackUI = mkOption {
        type = lib.types.nullOr lib.types.bool;
        default = null;
        description = "Whether to use force black UI. Null for default.";
      };
      allowExternalApps = mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether to allow external apps.";
      };
      fullscreen = mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether to allow fullscreen.";
      };
      margin = {
        vertical = mkOption {
          type = lib.types.int;
          default = 8;
          description = "The vertical margin.";
        };
        horizontal = mkOption {
          type = lib.types.int;
          default = 8;
          description = "The horizontal margin.";
        };
      };
      shortcut = {
        createSession = mkOption {
          type = lib.types.str;
          default = "ctrl + alt + c";
          description = "The shortcut to create a new session.";
        };
        nextSession = mkOption {
          type = lib.types.str;
          default = "ctrl + alt + n";
          description = "The shortcut to go to the next session.";
        };
        previousSession = mkOption {
          type = lib.types.str;
          default = "ctrl + alt + p";
          description = "The shortcut to go to the previous session.";
        };
      };
      bellCharacter = mkOption {
        type = lib.types.str;
        default = "ignore";
        description = "The character to use for the bell.";
      };
    };

    # termux-exec: TCP client/daemon for running native Termux commands from proot
    exec = {
      enable = mkEnableOption "termux-exec for running native Termux commands from proot";
      port = mkOption {
        type = lib.types.port;
        default = 18356;
        description = "TCP port for the termux-command-daemon";
      };
      host = mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
        description = "Bind address for the termux-command-daemon";
      };
    };

    # rish: Shizuku remote shell wrappers
    rish = {
      enable = mkEnable "rish (Shizuku remote shell) wrappers via termux-exec";
    };

    # Termux:Boot auto-start scripts
    boot = {
      enable = mkEnableOption "Termux:Boot auto-start scripts";
      wakelock = mkOption {
        type = lib.types.bool;
        default = true;
        description = "Acquire Termux wake-lock on boot";
      };
      pulseaudio = mkOption {
        type = lib.types.bool;
        default = true;
        description = "Start PulseAudio TCP server on boot";
      };
      writeSecureSettings = mkOption {
        type = lib.types.bool;
        default = true;
        description = "Grant WRITE_SECURE_SETTINGS to Termux packages on boot";
      };
      commandDaemon = mkOption {
        type = lib.types.bool;
        default = true;
        description = "Start termux-command-daemon on boot";
      };
      wifiDebugKeepalive = mkOption {
        type = lib.types.bool;
        default = false;
        description = "Run a background loop that re-asserts adb_wifi_enabled=1 every 5 minutes (fights corp policy resets)";
      };
      wifiDebugKeepaliveInterval = mkOption {
        type = lib.types.int;
        default = 300;
        description = "Seconds between wifi debug keepalive checks";
      };
    };

    x11 = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = config.bootstrap.desktop.enable;
        description = "Whether to enable x11 and associated scripts.";
      };
    };
    sharedDir = {
      enable = mkEnable "Whether to enable shared directory";
      path = lib.mkOption {
        type = lib.types.str;
        default = "/storage/emulated/0/shared";
        description = "The directory in shared storage to output files.";
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

    # --- Shared directory activation ---
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

    # --- Terminal settings (colors, font, properties) ---
    {
      terminal.font =
        "${pkgs.nerd-fonts.fira-code}/share/fonts/truetype/NerdFonts/FiraCode/FiraCodeNerdFont-Regular.ttf";
      terminal.colors = with untyped.colors; forNixOnDroid cfg.colors;

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

    # --- Bootstrap script ---
    {
      environment.etc."termux/bootstrap-from-nixondroid.sh" = {
        text = ''
          #!/data/data/com.termux/files/usr/bin/bash
          # Run in fresh Termux install after Nix-on-Droid is set up to
          # install Termux themes/settings from the NOD home-manager config:
          #
          # $ ${cfg.sharedDir.path}/bootstrap-from-nixondroid.sh

          set -euo pipefail

          SHARED="${cfg.sharedDir.path}"

          # --- .termux settings (colors, font, properties) ---
          # Merge NOD-managed .termux into existing, preserving any
          # manual additions (e.g. boot/ scripts added by other means).
          if [ -d "$SHARED/.termux" ]; then
            mkdir -p ~/.termux
            cp -Lr "$SHARED/.termux/." ~/.termux/
            echo "Installed .termux settings from NOD"
          fi

          # --- Termux:Boot scripts ---
          if [ -d "$SHARED/termux-boot" ]; then
            mkdir -p ~/.termux/boot
            for script in "$SHARED/termux-boot/"*.sh; do
              [ -f "$script" ] || continue
              name="$(basename "$script")"
              cp -f "$script" "$HOME/.termux/boot/$name"
              chmod +x "$HOME/.termux/boot/$name"
            done
            echo "Installed boot scripts: $(ls ~/.termux/boot/)"
          fi

          echo "Bootstrap complete. Run 'termux-reload-settings' to apply."
        '';
      };

      termux.sharedDir.copy = {
        ".termux" = "/data/data/com.termux.nix/files/home/.termux";
        ".bootstrap-from-nixondroid.sh" = "/etc/termux/bootstrap-from-nixondroid.sh";
      };
    }

    # --- termux-exec: TCP daemon + client ---
    (lib.mkIf cfg.exec.enable {
      environment.packages = [ 
        termux-exec-pkg
        termux-exec-alias-pkg
        termux-exec-record-pkg
        termux-exec-player-pkg
        termux-exec-play-pkg
        termux-exec-pause-pkg
        termux-exec-stop-pkg
      ];

      environment.etc."termux-exec/termux-command-daemon.sh" = {
        text = ''
          #!/data/data/com.termux/files/usr/bin/bash
          # termux-command-daemon: TCP relay for running native Termux commands
          # from inside nix-on-droid's proot (where bionic/app_process can't run).
          #
          # Runs in regular Termux. Requires: socat (pkg install socat)
          #
          # Usage:
          #   bash termux-command-daemon.sh              # foreground
          #   nohup bash termux-command-daemon.sh &      # background
          set -euo pipefail

          PORT="''${TERMUX_CMD_PORT:-${port}}"
          HOST="''${TERMUX_CMD_HOST:-${host}}"

          # Per-connection helper: reads the command line, then execs it.
          HELPER="$(mktemp)"
          trap 'rm -f "$HELPER"' EXIT
          cat > "$HELPER" <<'INNER'
          #!/data/data/com.termux/files/usr/bin/bash
          IFS= read -r CMD_LINE 2>/dev/null || exit 1
          [ -z "$CMD_LINE" ] && exit 1
          exec bash -c "$CMD_LINE"
          INNER
          chmod +x "$HELPER"

          echo "termux-command-daemon: listening on $HOST:$PORT"
          echo "termux-command-daemon: protocol - connect, send one line (the command), then interactive I/O"

          exec socat \
            TCP-LISTEN:"$PORT",bind="$HOST",reuseaddr,fork \
            EXEC:"$HELPER",pty,setsid,ctty,stderr
        '';
      };

      termux.sharedDir.copy."termux-exec" = "/etc/termux-exec";
    })

    # --- rish: Shizuku remote shell wrappers ---
    (lib.mkIf cfg.rish.enable {
      termux.exec.enable = true;
      environment.packages = [ rish-pkg ];
    })

    # --- Termux:Boot scripts ---
    (lib.mkIf cfg.boot.enable {
      # 00-wakelock: acquire wake-lock first to prevent sleep during boot
      environment.etc."termux-boot/00-wakelock.sh" = mkIf cfg.boot.wakelock {
        text = ''
          #!/data/data/com.termux/files/usr/bin/bash
          # Acquire Termux wake-lock to prevent Android from sleeping.
          termux-wake-lock 2>/dev/null || true
        '';
      };

      # 10-write-secure-settings: grant permission to Termux packages
      environment.etc."termux-boot/10-write-secure-settings.sh" = mkIf cfg.boot.writeSecureSettings {
        text = ''
          #!/data/data/com.termux/files/usr/bin/bash
          # Grant WRITE_SECURE_SETTINGS to Termux packages (idempotent).
          # Needed for toggling WiFi debugging etc via `settings put`.
          # Requires wireless debugging to have been enabled at least once
          # so that ADB is available locally.
          for pkg in com.termux com.termux.nix; do
            if pm list packages 2>/dev/null | grep -q "$pkg"; then
              adb shell pm grant "$pkg" android.permission.WRITE_SECURE_SETTINGS 2>/dev/null || true
            fi
          done
        '';
      };

      # 20-pulseaudio: start PulseAudio TCP server
      environment.etc."termux-boot/20-pulseaudio.sh" = mkIf cfg.boot.pulseaudio {
        text = ''
          #!/data/data/com.termux/files/usr/bin/bash
          # Start PulseAudio TCP server (idempotent).
          if pgrep pulseaudio 2>/dev/null; then
            killall pulseaudio
          fi
          LD_PRELOAD="/system/lib64/libskcodec.so" pulseaudio \
            --start \
            --exit-idle-time=-1 \
            --load="module-native-protocol-tcp auth-anonymous=1"
        '';
      };

      # 30-command-daemon: start termux-command-daemon
      environment.etc."termux-boot/30-command-daemon.sh" = mkIf (cfg.boot.commandDaemon && cfg.exec.enable) {
        text = ''
          #!/data/data/com.termux/files/usr/bin/bash
          # Start termux-command-daemon (idempotent).
          DAEMON="/storage/emulated/0/shared/termux-exec/termux-command-daemon.sh"
          LOG="$HOME/.termux-command-daemon.log"

          if pgrep -f "termux-command-daemon.sh" >/dev/null 2>&1; then
            echo "$(date): termux-command-daemon already running" >> "$LOG"
            exit 0
          fi

          echo "$(date): starting termux-command-daemon" >> "$LOG"
          nohup bash "$DAEMON" >> "$LOG" 2>&1 &
        '';
      };

      # 40-wifi-debug-keepalive: periodically re-assert wifi debugging enabled
      environment.etc."termux-boot/40-wifi-debug-keepalive.sh" = mkIf cfg.boot.wifiDebugKeepalive {
        text = ''
          #!/data/data/com.termux/files/usr/bin/bash
          # WiFi Debug keepalive daemon (idempotent).
          # Re-asserts adb_wifi_enabled=1 every N seconds to fight corp policy resets.
          # Tries ADB shell first, then Shizuku/rish, then direct settings.
          # Runs in background; skips if already running.
          INTERVAL="${toString cfg.boot.wifiDebugKeepaliveInterval}"

          if pgrep -f "wifi-debug-keepalive" >/dev/null 2>&1; then
            exit 0
          fi

          exec -a wifi-debug-keepalive bash -c '
            wifi_set() {
              # ADB
              adb shell settings put global adb_wifi_enabled 1 2>/dev/null && return 0
              adb connect 127.0.0.1 2>/dev/null
              adb shell settings put global adb_wifi_enabled 1 2>/dev/null && return 0
              # Shizuku
              RISH_DEX=""
              for c in "$HOME/rish/rish_shizuku.dex" "/storage/emulated/0/shared/rish/rish_shizuku.dex"; do
                [ -f "$c" ] && RISH_DEX="$c" && break
              done
              if [ -n "$RISH_DEX" ]; then
                echo "settings put global adb_wifi_enabled 1" | \
                  /system/bin/app_process -Djava.class.path="$RISH_DEX" /system/bin \
                  --nice-name=rish rikka.shizuku.shell.ShizukuShellLoader 2>/dev/null && return 0
              fi
              # Direct
              settings put global adb_wifi_enabled 1 2>/dev/null && return 0
              return 1
            }
            while true; do
              wifi_set || true
              sleep '"$INTERVAL"'
            done
          ' &
        '';
      };

      termux.sharedDir.copy."termux-boot" = "/etc/termux-boot";
    })

    # --- X11 scripts (on-demand, not in boot) ---
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

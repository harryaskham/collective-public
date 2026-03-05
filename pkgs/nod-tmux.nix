# nod-tmux: tmux management commands for nod-term integration.
#
# Provides CLI tools for managing tmux sessions via nod-term's
# Android intent activities. Works from NOD, Termux, or any shell.
{
  pkgs,
  ...
}:

let
  nod-tmux = pkgs.writeScriptBin "nod-tmux" ''
    #!${pkgs.bash}/bin/bash
    # Tmux management via nod-term app intents
    set -euo pipefail

    ACTION="''${1:-help}"
    shift 2>/dev/null || true

    # am command — works in both NOD (via proot am) and Termux
    AM="am"
    command -v am >/dev/null 2>&1 || AM="termux-exec am"

    case "$ACTION" in
      new)
        NAME="''${1:-nt-$(date +%s | tail -c 4)}"
        echo "Creating tmux session: $NAME"
        $AM start -n com.harryaskham.nodterm/.TmuxNewSessionActivity --es name "$NAME" 2>/dev/null
        ;;
      float)
        NAME="''${1:-nt-$(date +%s | tail -c 4)}"
        echo "Creating floating tmux session: $NAME"
        $AM start -n com.harryaskham.nodterm/.TmuxFloatSessionActivity --es name "$NAME" 2>/dev/null
        ;;
      attach)
        SESSION="''${1:?Usage: nod-tmux attach <session>}"
        echo "Attaching $SESSION in docked tab"
        $AM start -n com.harryaskham.nodterm/.TmuxAttachDockActivity --es session "$SESSION" 2>/dev/null
        ;;
      float-attach)
        SESSION="''${1:?Usage: nod-tmux float-attach <session>}"
        echo "Attaching $SESSION in float"
        $AM start -n com.harryaskham.nodterm/.TmuxAttachFloatActivity --es session "$SESSION" 2>/dev/null
        ;;
      window)
        SESSION="''${1:?Usage: nod-tmux window <session> [name]}"
        WINDOW="''${2:-}"
        echo "New window in $SESSION"
        $AM start -n com.harryaskham.nodterm/.TmuxNewWindowActivity --es session "$SESSION" ''${WINDOW:+--es window "$WINDOW"} 2>/dev/null
        ;;
      list|ls)
        ${pkgs.tmux}/bin/tmux list-sessions 2>/dev/null || echo "No tmux sessions"
        ;;
      kill)
        SESSION="''${1:?Usage: nod-tmux kill <session>}"
        ${pkgs.tmux}/bin/tmux kill-session -t "$SESSION" 2>/dev/null && echo "Killed $SESSION" || echo "Session not found: $SESSION"
        ;;
      help|--help|-h|*)
        cat <<HELP
    nod-tmux — tmux management for nod-term

    Usage:
      nod-tmux new [name]              Create tmux session in docked tab
      nod-tmux float [name]            Create tmux session in float
      nod-tmux attach <session>        Attach session in docked tab
      nod-tmux float-attach <session>  Attach session in float
      nod-tmux window <session> [name] New window in session (tab group)
      nod-tmux list                    List tmux sessions
      nod-tmux kill <session>          Kill session

    Session names default to nt-XXXX if not specified.
    All commands use Android intents to nod-term app.
    HELP
        ;;
    esac
  '';

  nod-float = pkgs.writeScriptBin "nod-float" ''
    #!${pkgs.bash}/bin/bash
    # Launch a floating terminal via nod-term
    CMD="''${*:-zsh}"
    AM="am"
    command -v am >/dev/null 2>&1 || AM="termux-exec am"

    ENCODED=$(${pkgs.python3}/bin/python3 -c "import urllib.parse; print(urllib.parse.quote('''$CMD'''))")
    $AM start -a android.intent.action.VIEW \
      -d "nodterm://float?cmd=$ENCODED" \
      -n com.harryaskham.nodterm/.FloatingLaunchActivity 2>/dev/null
  '';

in {
  inherit nod-tmux nod-float;
}

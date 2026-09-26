# Packaged by the WM module with explicit runtime dependencies.
set -euo pipefail

if [[ $# != 2 || ! $2 =~ ^(-1|1)$ ]]; then
    echo "Usage: collective-workspace-step {hyprland|sway|i3} {-1|1}" >&2
    exit 2
fi
wm=$1
delta=$2
case "$wm" in
    hyprland) session=${HYPRLAND_INSTANCE_SIGNATURE:-default} ;;
    sway) session=${SWAYSOCK:-default}; ipc=swaymsg ;;
    i3) session=${I3SOCK:-${DISPLAY:-default}}; ipc=i3-msg ;;
    *) echo "Unsupported window manager: $wm" >&2; exit 2 ;;
esac

# Serialize overlapping swipes in this session: never read the same old index
# twice and silently lose one of the requested steps. Do not lock other WMs.
runtime=${XDG_RUNTIME_DIR:-/run/user/$(id -u)}
key=$(printf '%s' "$session" | cksum | cut -d ' ' -f 1)
exec 9>"$runtime/collective-workspace-step-$wm-$key.lock"
flock -w 4 9

case "$wm" in
    hyprland)
        current=$(timeout 3s hyprctl -j activeworkspace | jq -er '.id')
        ;;
    sway|i3)
        current=$(timeout 3s "$ipc" -r -t get_workspaces | jq -er 'first(.[] | select(.focused == true)) | .num')
        ;;
esac

# Named/special workspaces have no positive numeric neighbour. Fail closed;
# don't surprise the user by jumping from a special workspace to workspace 1.
if [[ ! $current =~ ^[1-9][0-9]*$ ]] || (( ${#current} > 10 || current > 2147483647 )); then
    echo "Cannot step a non-positive or out-of-range workspace number: $current" >&2
    exit 1
fi
target=$((current + delta))
if (( target < 1 || target > 2147483647 )); then
    exit 0
fi

case "$wm" in
    hyprland)
        reply=$(timeout 3s hyprctl dispatch workspace "$target")
        if [[ $reply != ok ]]; then
            printf 'Workspace dispatch failed: %s\n' "$reply" >&2
            exit 1
        fi
        ;;
    sway|i3)
        # Select/create exactly n +/- 1, including gaps and numbered names.
        # Avoid workspace_auto_back_and_forth changing the requested target.
        timeout 3s "$ipc" -r -- "workspace --no-auto-back-and-forth number $target" |
            jq -e 'type == "array" and length > 0 and all(.[]; .success == true)' >/dev/null
        ;;
esac

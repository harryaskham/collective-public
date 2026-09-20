# Sourced by the Nix-generated health-check wrapper. The Android app owns its
# private default.pa, so reconcile declared virtual sinks after it starts/restarts.
# Never unload modules, change defaults, or restart an otherwise healthy app when
# reconciliation fails. am-supervisor handles genuine connection failures.
set -eu

sinks="$("$PULSE_TIMEOUT" --kill-after=1s 5s "$PULSE_PACTL" --server="$PULSE_SERVER" list short sinks)" || exit 1
for sink in "$@"; do
  found=false
  while read -r index name rest; do
    if [ "$name" = "$sink" ]; then
      found=true
      break
    fi
  done <<< "$sinks"
  if [ "$found" = false ]; then
    if "$PULSE_TIMEOUT" --kill-after=1s 5s "$PULSE_PACTL" --server="$PULSE_SERVER" \
        load-module module-null-sink "sink_name=$sink" rate=48000 channels=2 \
        "sink_properties=device.description=$sink" >/dev/null; then
      sinks="$sinks"$'\n0\t'"$sink"
    else
      echo "PulseServer is up, but virtual sink '$sink' could not be created; retrying next check" >&2
    fi
  fi
done

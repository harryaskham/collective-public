# Omni / Flotilla control layouts

**All ten generated Code QWERTY Compact layouts** retain their existing gestures:

- **F ↙ → WM** opens `Flotilla WM`.
- **F ↗ → ⇡** uses Omni's `wm_open_terminal`, replacing the Ctrl+Alt+F macro.
  This is a semantic coordinator action, not text/key events sent to the editor.
  With Flotilla running, Omni enrolled with command access, and NOD enrolled with
  its default-window factory, it opens a NOD float even while another app is focused.
  It uses NOD's configured command. It requires the selected manager; it is **not**
  a manager-independent Android intent in Local mode.
- F's focus arrows and Fn stay in place. E ↖ remains the mounted-terminal key.

## Control deck

Import `Flotilla WM` **and** `Flotilla WM (Arrows)` with your usual compact layouts.
Named switching only finds layouts that are installed in Omni's layout list.

| Region | Action |
|---|---|
| Top 1–9 | Select workspace by its configured position |
| South swipe on 1–9 | Send the selected window to that workspace |
| Focus pad | Directional focus, through the selected manager |
| Move pad | Directional move/swap, through the selected manager |
| Resize pad | Resize using Flotilla's default Ctrl+Alt+Shift+arrow bindings |
| WS− / WS+ | Previous/next workspace, through the selected manager |
| Four live slots | Flotilla slots 1, 2, 3, 5; defaults WM, Tile, Full, Pin |
| Hide / south Min / Restore | Visibility controls, not terminal-session closure |
| Bottom ABC / Portrait / Landscape | Explicit configurable keyboard return layouts |
| Arrows / Swipes | Toggle compact swipe pads versus individual tap arrows |
| Term+ | Same semantic new-float action as F ↗ |
| Term+ ↖ / ↗ / ↓ | Mounted terminal / keyboard screen picker / keyboard dock toggle |

**Important distinction:** numeric workspace keys and Resize are shortcut macros,
not new Omni protocol events. They require a focused Flotilla-aware client such as
NOD and the standard Flotilla key bindings. They are not guaranteed to operate over
arbitrary apps, and can reach the focused app when it does not consume them. The
semantic focus/move, WS−/WS+, visibility, Term+ and live-slot keys do not need a NOD
InputConnection. Arbitrary-app-safe numeric/resize keys would require new Omni
semantic actions (or explicitly configuring Flotilla's twelve existing slots).
Do not enable Accessibility merely to try to make injected IME macros global.

Only existing workspaces can be selected. Flotilla ships three initially; 4–9 do
not create workspaces. Sending a window changes its membership, not workspace order.
BSP move swaps/repositions according to Flotilla policy. Attached keyboard terminals
remain client-owned. Slots show the **live labels from your Flotilla configuration**;
the layout does not overwrite their assignments. No Close key is added by default.

The tap-arrow variant is taller, with three columns (Focus / Move / Size) and four
direction rows, useful when gestures are inconvenient. It retains the same return
row; it is not a separate configured typing default or an automatically remembered
previous layout.

## Return destinations

```nix
services.unexpected-keyboard.wmReturnLayouts = [
  { label = "ABC"; layout = "Code QWERTY Compact (5_keys29T)"; }
  { label = "Wide"; layout = "Code QWERTY Compact (8_splitLMk2)"; }
];
```

One to four destinations are supported. Names must exactly identify imported
layouts; short labels fit best. Defaults are base compact, portrait split Mk2 and
landscape split Mk2. Existing compact variant names and indices are unchanged.

Regenerate/export with `cltv config keyboard unexpected` after making the generated
files available through your normal `cltv switch` workflow. Import/reimport the
changed compact XML and both WM XMLs in Omni. No system switch, device preferences,
integration grants or Flotilla YAML are changed by editing these sources.

## Other NOD / Omni keys: placement proposals

These are proposals, not silent additions to the typing layout:

| Existing action not previously placed here | Proposed gesture | Notes |
|---|---|---|
| Ctrl+Alt+Shift+F: move current session between docked and floating | F ↖ | Different from creating a new float; needs NOD focus. F ↖ is free. |
| Ctrl+Alt+C: new docked terminal tab | C ← | Currently defined as a macro but unbound; requires NOD's docked activity. |
| Ctrl+Alt+K: toggle keyboard from NOD | K ↑ | K's quotes/brackets remain intact; requires NOD focus. |
| Ctrl+Alt+comma: NOD Settings | C ↖ | C ↙ remains **Omni** settings; label `NOD`. Requires NOD focus. |
| `floating_other_screen`: keyboard display/pane picker | O ↖ is already keyboard move; use O ↑ | Implemented on Term+ ↗ in the WM deck; does not move a terminal. |
| `toggle_floating_docked`: keyboard float/dock | O ↓ | Implemented on Term+ ↓; different from NOD session docking. |
| `floating_disable_passthrough`: make keyboard interactive again | P ↑ | Useful counterpart to P ↘, though an untouchable keyboard needs its recovery surface. |
| Flotilla slots 4 / 6 | WM deck secondary gestures | Defaults Float / Balance; keep live slot labels if added. |
| Flotilla slots 7 / 8 / 9 | Separate optional advanced deck | Defaults Close / Hide keyboard / Show keyboard; avoid accidental Close or hiding the controls themselves. |

Already covered: new float (F ↗ and C ↘), mounted terminal (E ↖), previous/next tab
(B ↙/↘), tab switcher (Z ↘), paste (V ↗), keyboard move/resize, persistence,
centering/snapping, focus/move arrows, and manager visibility. No invented
`nod_*` key names or arbitrary intent strings are emitted.

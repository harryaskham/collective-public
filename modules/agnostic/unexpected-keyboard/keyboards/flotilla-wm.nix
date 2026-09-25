{ uklib, typed, ... }:
with typed;
with uklib;
with codes;
let
  # Technical, compact, tactile: numbered workspace rail, separated directional
  # groups, manager actions, then an always-visible return row. Inherits Omni's
  # theme. No shell text, clipboard payloads or arbitrary Android intents.
  label = text: code: (kv.k code) // { legend = text; };
  key = text: code: { c = label text code; };
  inert = text: key text "removed";
  switch = text: name: key text (wmLayoutSwitch name);
  row = height: keys: {
    inherit height;
    keys = map (k: k // { width = 10.0 / length keys; }) keys;
  };
  chord = text: keys: { c = wmShortcut text keys; };
  workspaceKeys = genList (i:
    let number = toString (i + 1);
    in (chord number [ "ctrl" "alt" number ]) // {
      # South sends the selected window instead of changing the workspace.
      s = wmShortcut ("→" + number) [ "ctrl" "alt" "shift" number ];
    }) 9;
  workspaceRow = row 0.65 workspaceKeys;
  focus = direction: _.${"wm_focus_" + direction};
  move = direction: _.${"wm_move_" + direction};
  resize = text: direction: wmShortcut text [ "ctrl" "alt" "shift" direction ];
  pad = title: code: (inert title) // {
    n = label "↑" (code "up"); s = label "↓" (code "down");
    w = label "←" (code "left"); e = label "→" (code "right");
  };
  resizePad = (inert "Resize") // {
    n = resize "↑" "up"; s = resize "↓" "down";
    w = resize "←" "left"; e = resize "→" "right";
  };
  actions = row 0.65 [
    (key "WS−" _.wm_workspace_previous)
    (key "WS+" _.wm_workspace_next)
    { c = kv.k _.flotilla_1; }
    { c = kv.k _.flotilla_2; }
    { c = kv.k _.flotilla_3; }
    { c = kv.k _.flotilla_5; }
    ((key "Hide" _.wm_hide_all) // { s = label "Min" _.wm_minimize_all; })
    (key "Restore" _.wm_restore_all)
  ];
  returns = assert length wmReturnLayouts >= 1 && length wmReturnLayouts <= 4;
    map (entry: switch entry.label entry.layout) wmReturnLayouts;
  footer = alternate: row 0.65 (returns ++ [
    (switch (if alternate == "Flotilla WM" then "Swipes" else "Arrows") alternate)
    ((key "Term+" _.wm_open_terminal) // {
      nw = label "" _.toggle_mounted_terminal;
      ne = label "Screen" _.floating_other_screen;
      s = label "Dock" _.toggle_floating_docked;
    })
  ]);
  arrowRow = direction: glyph: row 0.65 [
    (key ("Focus " + glyph) (focus direction))
    (key ("Move " + glyph) (move direction))
    { c = resize ("Size " + glyph) direction; }
  ];
  tapRows = [ workspaceRow ] ++ map (pair: arrowRow pair.direction pair.glyph) [
    { direction = "up"; glyph = "↑"; }
    { direction = "left"; glyph = "←"; }
    { direction = "down"; glyph = "↓"; }
    { direction = "right"; glyph = "→"; }
  ] ++ [ actions (footer "Flotilla WM") ];
in {
  name = "Flotilla WM";
  bottomRow = false;
  includeDefaultVariants = false;
  rows = [ workspaceRow (row 1.05 [ (pad "Focus" focus) (pad "Move" move) resizePad ])
    actions (footer "Flotilla WM (Arrows)") ];
  variants.Arrows = keyboard: keyboard // { rows = tapRows; };
}

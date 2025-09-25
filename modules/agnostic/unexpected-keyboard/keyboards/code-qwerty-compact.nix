{ uklib, typed, ... }:

with typed;
with uklib;

with codes.withAliases {
  switch_to_base = "switch_to_layout_Code_QWERTY_Compact";
  switch_to_C0Mods = "switch_to_layout_Code_QWERTY_Compact_0_C0Mods";
  switch_to_splitPG = "switch_to_layout_Code_QWERTY_Compact_1_splitPE";
  switch_to_splitPE = "switch_to_layout_Code_QWERTY_Compact_2_splitPG";
  switch_to_splitLG = "switch_to_layout_Code_QWERTY_Compact_3_splitLE";
  switch_to_splitLE = "switch_to_layout_Code_QWERTY_Compact_4_splitLG";
};

let macros = typed.recursiveMergeAttrsList [
  (mkCardinal "show_termux_sidebar" (with _; with kv; m "⌁" [(k ctrl) (k alt) (k shift) (k right)]))
];

in {
  name = "Code QWERTY Compact";
  bottomRow = false;
  includeDefaultVariants = false;  # Added back manually as part of the ordered variants below.
  variants =
    let
      addSwitchToBase = setKey 2 4 (K "⟲" se.switch_to_base c.b K);

      clearEscAdjacent = precompose [
        (setKey 0 0 (K c.q ne."1" sw."!" K)) # Clears Q
        (setKey 0 1 (K c.w ne."2" sw."@" K)) # Clears W
      ];

      clearHomeRowMods = precompose [
        (setKey 1 0 (K c.a K))
        (setKey 1 1 (K c.s K))
        (setKey 1 2 (K c.d K))
        (setKey 1 3 (K c.f K))
      ];

      clearModsAndEsc = precompose [
        clearEscAdjacent
        #clearHomeRowMods
      ];

      modCol = 
        K "⎋" c.esc nw.tab ne."`" sw."~"
        _ "✲" c.ctrl "❖" sw.meta "⌥" ne.alt "▤" nw.fn
        _ c.shift
        K;

      withModCol = colI: precompose [
        clearModsAndEsc
        (insertCol colI modCol)
      ];

      modGridL = { paddingL, ... }:
        K 1 paddingL "⎋" c.esc nw.tab ne."`" sw."~"
        _ 1 paddingL "✲" c.ctrl "❖" sw.meta "⌥" ne.alt "▤" nw.fn
        K;

      modGridR = { gap, ...}:
        let centralGap = gap - 2;
        in 
          K 1 centralGap n.up w.left e.right s.down
          _ 1 centralGap c.shift
          K;

      returnOverCursor = precompose [
        (deleteKey 2 9)
        (updateKey 2 9 (addWidth 1))
      ];

      withSplitSpace = {gap, paddingL, paddingR, ...} @ args:
        precompose [
          (updateKey 2 5 (addShift paddingR))
          (insertKey 2 5 (K gap paddingL w.cur_l  " " c.spc  e.cur_r K))
        ];

      withoutModRow = (deleteRow 3);

      # Add modifier keys to split layouts and handle padding the split if necessary
      # Use as 'mod' arguments to mkSplit
      Mods = {
        Empty = {gap, paddingL, paddingR, ...}: precompose [
          (updateKey 0 5 (addShift (gap + paddingL + paddingR)))
          (updateKey 1 5 (addShift (gap + paddingL + paddingR)))
        ];

        Col = colI: {gap, ...}: precompose [
          (updateKey 0 5 (addShift gap))
          (updateKey 1 5 (addShift gap))
          (withModCol colI)
        ];

        Grid = {gap, paddingL, paddingR, ...} @ args: precompose [
          clearModsAndEsc
          returnOverCursor
          (updateKey 0 5 (addShift paddingR))
          (updateKey 1 5 (addShift paddingR))
          (insertCol 5 (modGridL args))
          (insertCol 6 (modGridR args))
        ];
      };

      mkSplit = {gap, paddingL, paddingR, mods} @ args:
        precompose [
          addSwitchToBase
          withoutModRow
          (withSplitSpace args)
          (mods args)
          fitWidth
        ];

      # Add mods down the left side and remove duplicates on the old column-0
      # Split layout with empty middle row for landscape mode.
      C0Mods = precompose [
        addSwitchToBase
        (setKey 3 0 (K "❖" c.meta K))
        (swapKeys 3 0 3 1)  # alt-meta not meta-alt
        (updateKey 3 2 (addWidth 1)) # space fills width
        (withModCol 0)
        fitWidth
      ];

      portraitArgs = {
        gap = 2;
        paddingL = 0.2;
        paddingR = 0.2;
      };

      landscapeArgs = {
        gap = 8;
        paddingL = 0.5;
        paddingR = 0.5;
      };

      mkLandscapeSplit = mods: mkSplit (landscapeArgs // {mods = mods;});
      mkPortraitSplit = mods: mkSplit (portraitArgs // {mods = mods;});

      applyLefty = f: precompose [
        addSwitchToBase
        f 
        (Variants.lefty 2.0)
      ];

      applyRighty = f: precompose [
        addSwitchToBase
        f 
        (Variants.righty 2.0)
      ];

    in 

    # Emit the variants in the order below.
    indexPrefixedAttrs [
      { inherit C0Mods; } # Default layout with modifier keys in column 0.
      { splitPE = mkPortraitSplit Mods.Empty; } # Portrait split layout with empty centre.
      { splitPG = mkPortraitSplit Mods.Grid; } # Portrait split layout with modifier keys centre.
      { splitLE = mkLandscapeSplit Mods.Empty; } # Landscape split layout with empty centre.
      { splitLG = mkLandscapeSplit Mods.Grid; } # Landscape split layout with modifier keys centre.
      # Below: disabled layout variants.
      # { L = applyLefty id; } # Left-aligned one-handed layout.
      # { R = applyRighty id; } # Right-aligned one-handed layout.
      # { C0ModsL = applyLefty C0Mods; } # Left-aligned layout with modifier keys in column 0.
      # { C0ModsR = applyRighty C0Mods; } # Right-aligned layout with modifier keys in column 0.
      # { splitP0 = mkPortraitSplit (Mods.Col 0); } # Portrait split layout with modifier keys in column 0.
      # { splitP5 = mkPortraitSplit (Mods.Col 5); } # Portrait split layout with modifier keys in column 5 and empty centre.
      # { splitL0 = mkLandscapeSplit (Mods.Col 0); } # Landscape split layout with modifier keys in column 0.
      # { splitL5 = mkLandscapeSplit (Mods.Col 5); } # Landscape split layout with modifier keys in column 5 and empty centre.
    ];

  rows = map (row: row // {height = 0.65;}) [{
    keys =
      K nw."`"      ne."1"
              c.q
        sw."~"      "⎋" se.esc
      _
        nw."!"      ne."2"
              c.w
        sw."@"      "⌖" se.center_both
      _
                    ne."3"
              c.e
        sw."#"      "⏛" se.center_vertical
      _
        nw."£"      ne."4"
              c.r
        sw."$"      "⎅" se.center_horizontal
      _
        "↥" nw.snap_top ne."5"
              c.t
        sw."%"      "⇹" se.fill_width
      _
                    ne."6"
              c.y
        sw."^"      "⟷" se.switch_to_splitPG
      _
        "↧" nw.snap_bottom  ne."7"
              c.u
        sw."&"      "⟺" se.switch_to_splitLG
      _
                    ne."8"
              c.i
        sw."*"      "⥺" se.switch_to_C0Mods
      _
        "⛶" nw.floating_move  ne."9"
              c.o
        sw."("     se.")"
      _
        "∞" nw.toggle_persistence   ne."0"
              c.p
                                    "➟" se.floating_enable_passthrough
      K;
  }

  {
    keys =
      K
        nw.tab     ne.shift
              c.a
        "⇤" sw.snap_left  "✲" se.ctrl
      _

              c.s
                    "⌥" se.alt
      _

              c.d
                    "❖" se.meta
      _

              c.f
                    "▤" se.fn

      _
                    ne."-"
              c.g
        sw."_"
      _
                    ne."="
              c.h
        sw."+"
      _

              c.j
        sw."{"       se."}"
      _
                    ne."'"
                c.k
        sw."["       se."]"
      _
                      ne."\""
                c.l

      _
        nw.shift     ne.del
                c.bsp
        "✲" sw.ctrl  "⇥" se.snap_right
    K;
  }

  {
    keys =
      K
                        ne."|"
              c.z
        sw."\\"         "⇝" macros.se.show_termux_sidebar
      _
                      ne.cut
              c.x
        sw.bwd       se.fwd
      _
                    ne.copy
              c.c
        sw.config  #se.toggle_floating_docked 
      _
                  ne.paste
              c.v
                      se.pastePlain
      _

              c.b

      _
                    ne."?"
              c.n
        sw."/"
      _
                    ne.":"
              c.m
        sw."<"      se.">"
      _
                    ne.";"
              c."."
        sw.","
      _
                  n.up
          w.left        e.right
                  s.down
      _
        "⤡" nw.floating_resize ne.action
                c.enter

      K;
  }

  {
    keys =
      K
                  "❖" ne.meta
            "✲" c.ctrl
      _
        "ℕ" nw._123    "▤" ne.fn
              "⌥" c.alt

      _ 8

          w.cur_l  " " c.spc  e.cur_r

      K;
  }];
}

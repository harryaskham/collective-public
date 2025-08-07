{ uklib, typed, ... }:

with typed;
with uklib;

{
  QWERTY_US = 
    # https://github.com/Julow/Unexpected-Keyboard/blob/master/srcs/layouts/latn_qwerty_us.xml
    {
      # <keyboard name="QWERTY (US)" script="latin">
      name = "QWERTY (US)";
      includeDefaultVariants = false;
      rows = with codes; [
        # <row>
        #   <key c="q" ne="1" se="loc esc"/>
        #   <key c="w" nw="~" ne="2" sw="\@"/>
        #   <key c="e" nw="!" ne="3" sw="\#" se="loc €"/>
        #   <key c="r" ne="4" sw="$"/>
        #   <key c="t" ne="5" sw="%"/>
        #   <key c="y" ne="6" sw="^"/>
        #   <key c="u" ne="7" sw="&amp;"/>
        #   <key c="i" ne="8" sw="*"/>
        #   <key c="o" ne="9" sw="(" se=")"/>
        #   <key c="p" ne="0"/>
        # </row>
        {
          keys = [
            (__ [
                          ne."1"
                    c.q
                          (loc se.esc)
            ])
            (__ [
              nw."~"      ne."2"
                    c.w
              sw."@"
            ])
            (__ [
              nw."!"      ne."3"
                    c.e
              sw."#"      (loc se."€")
            ])
            (__ [
                          ne."4"
                    c.r
              sw."$"
            ])
            (__ [
                          ne."5"
                    c.t
              sw."%"
            ])
            (__ [
                          ne."6"
                    c.y
              sw."^"
            ])
            (__ [
                          ne."7"
                    c.u
              sw."&"
            ])
            (__ [
                          ne."8"
                    c.i
              sw."*"
            ])
            (__ [
                          ne."9"
                    c.o
              sw."("     se.")"
            ])
            (__ [
                          ne."0"
                    c.p

            ])
          ];
        }

        # <row>
        #   <key shift="0.5" c="a" nw="loc tab" ne="`"/>
        #   <key c="s" ne="loc §" sw="loc ß"/>
        #   <key c="d"/>
        #   <key c="f"/>
        #   <key c="g" ne="-" sw="_"/>
        #   <key c="h" ne="=" sw="+"/>
        #   <key c="j" se="}" sw="{"/>
        #   <key c="k" sw="[" se="]"/>
        #   <key c="l" ne="|" sw="\\"/>
        # </row>
        {
          keys = [
            (o 0.5 (__ [
              (loc c.tab)  ne."`"
                    c.a

            ]))
            (__ [
                          (loc (ne_ (kv.k "§")))
                    c.s
                          (loc (sw_ (kv.k "ß")))
            ])
            (__ [

                    c.d

            ])
            (__ [

                    c.f

            ])
            (__ [
                          ne."-"
                    c.g
              sw."_"
            ])
            (__ [
                          ne."="
                    c.h
              sw."+"
            ])
            (__ [

                    c.j
              sw."{"     se."}"
            ])
            (__ [

                    c.k
              sw."["     se."]"
            ])
            (__ [
                          ne."|"
                    c.l
              sw."\\"
            ])
          ];
        }

        # <row>
        #   <key width="1.5" c="shift" ne="loc capslock"/>
        #   <key c="z"/>
        #   <key c="x" ne="loc †"/>
        #   <key c="c" ne="&lt;" sw="."/>
        #   <key c="v" ne="&gt;" sw=","/>
        #   <key c="b" ne="\?" sw="/"/>
        #   <key c="n" ne=":" sw=";"/>
        #   <key c="m" ne="&quot;" sw="'"/>
        #   <key width="1.5" c="backspace" ne="delete"/>
        # </row>
        {
          height = 1.0;
          keys = [
            (wd 1.5 (__ [
                          (loc ne.caps)
                    c.shift

            ]))
            (__ [

                    c.z

            ])
            (__ [
                          (ne_ (kv.k "†"))
                    c.x

            ])
            (__ [
                          ne."<"
                    c.c
              sw."."
            ])
            (__ [
                          ne.">"
                    c.v
              sw.","
            ])
            (__ [
                          ne."?"
                    c.b
              sw."/"
            ])
            (__ [
                          ne.":"
                    c.n
              sw.";"
            ])
            (__ [
                          ne."\""
                    c.m
              sw."'"
            ])
            (wd 1.5 (__ [
                          ne.del
                    c.bsp

            ]))
          ];
        }

      ];
    };
    # </keyboard>

  Code_QWERTY = {
    name = "Code QWERTY";
    bottomRow = false;
    rows = with codes; [
      {
        keys =
          K             ne."1"
                   c.q
            sw."!"      "⎋" se.esc
          _
                        ne."2"
                   c.w
            sw."@"
          _
                        ne."3"
                   c.e
            sw."#"
          _
            nw."£"      ne."4"
                   c.r
            sw."$"
          _
                        ne."5"
                   c.t
            sw."%"
          _
                        ne."6"
                   c.y
            sw."^"
          _
                        ne."7"
                   c.u
            sw."&"
          _
                        ne."8"
                   c.i
            sw."*"
          _
                        ne."9"
                   c.o
            sw."("      se.")"
          _
            nw.del      ne."0"
                   c.p
            sw.bsp
          K;
      }

      {
        keys =
          K 1 0.5
             nw.tab       ne."`"
                   c.a
             sw."~"       "✲" se.ctrl
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
            "✲" sw.ctrl
        K;
      }

      {
        keys =
          K
                            ne."|"
                  c.z
            sw."\\"
          _
            nw.change_method   ne.cut
                  c.x
            sw.bwd       se.fwd
          _
                        ne.copy
                  c.c
            sw.config   se.emoji
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
                          ne.action
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
      }

    ];
  };

  Code_QWERTY_Compact = 
    (with codes.withAliases {
      switch_to_base = "switch_to_layout_Code_QWERTY_Compact";
      switch_to_C0Mods = "switch_to_layout_Code_QWERTY_Compact_0_C0Mods";
      switch_to_splitPG = "switch_to_layout_Code_QWERTY_Compact_1_splitPE";
      switch_to_splitPE = "switch_to_layout_Code_QWERTY_Compact_2_splitPG";
      switch_to_splitLG = "switch_to_layout_Code_QWERTY_Compact_3_splitLE";
      switch_to_splitLE = "switch_to_layout_Code_QWERTY_Compact_4_splitLG";
    };

    {
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

          modGridR = { gap, paddingL, paddingR, ...}:
            let centralGap = gap - 2 - paddingL - paddingR;
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
            gap = 12;
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
            sw."@"
          _
                        ne."3"
                  c.e
            sw."#"
          _
            nw."£"      ne."4"
                  c.r
            sw."$"
          _
                        ne."5"
                  c.t
            sw."%"      "⌨" se.toggle_floating 
          _
                        ne."6"
                  c.y
            sw."^"      "⟷" se.switch_to_splitPG
          _
                        ne."7"
                  c.u
            sw."&"      "⟺" se.switch_to_splitLG
          _
                        ne."8"
                  c.i
            sw."*"      "⥺" se.switch_to_C0Mods
          _
                      ne."9"
                  c.o
            sw."("     se.")"
          _
            "∞" nw.toggle_persistence   ne."0"
                  c.p

          K;
      }

      {
        keys =
          K
            nw.tab     ne.shift
                  c.a
                      "✲" se.ctrl
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
            "✲" sw.ctrl
        K;
      }

      {
        keys =
          K
                            ne."|"
                  c.z
            sw."\\"
          _
                         ne.cut
                  c.x
            sw.bwd       se.fwd
          _
                        ne.copy
                  c.c
            sw.config   
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
                          ne.action
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
    });
}

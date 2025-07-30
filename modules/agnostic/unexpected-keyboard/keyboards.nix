{ uklib, typed, ... }:

with typed;
with uklib;

[
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
  }
  # </keyboard>

  {
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
            sw."{"     se."}"
          _
                        ne."'"
                  c.k
            sw."["     se."]"
          _
                        ne."\""
                  c.l
            sw.enter
        K;
      }

      {
        keys =
          K
                  c.shift

          _
                       ne."|"
                  c.z
            sw."\\"    
          _

                  c.x
            sw.bwd     se.fwd
          _

                  c.c

          _

                  c.v

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
            "xⁿ" nw.sup
                  c.bsp
            "xₙ" sw.sub
          K;
      }

      {
        height = 0.95;
        keys =
          K 1.7
              "▤" nw.fn      "❖" ne.meta
                      "✲" c.ctrl
              "λ" sw.math    "ℕ" se._123
          _ 1.1
              nw.change_method   "⎘" ne.pastePlain
                         "⌥" c.alt
              sw.emoji           se.config
          _ 4.4
                       
              w.cur_l  " " c.spc  e.cur_r
                       
          _ 1.1
                       n.up
              w.left         e.right
                       s.down
          _ 1.7
                             ne.action
                       c.enter

          K;
      }

    ];
  }

  
  {
    name = "Code QWERTY Compact";
    bottomRow = false;
    includeDefaultVariants = false;  # Added back manually as part of the ordered variants below.
    variants = with codes; 
      let
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
          clearHomeRowMods
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
          let width = gap - paddingL - paddingR;
          in precompose [
            (updateKey 2 5 (addShift paddingR))
            (insertKey 2 5 (K width paddingL w.cur_l  " " c.spc  e.cur_r K))
          ];

        withoutModRow = (deleteRow 3);

        # Add modifier keys to split layouts and handle padding the split if necessary
        # Use as 'mod' arguments to mkSplit
        Mods = {
          Empty = {gap, ...}: precompose [
            (updateKey 0 5 (addShift gap))
            (updateKey 1 5 (addShift gap))
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
            withoutModRow
            (withSplitSpace args)
            (mods args)
            fitWidth
          ];

        # Add mods down the left side and remove duplicates on the old column-0
        # Split layout with empty middle row for landscape mode.
        C0Mods = precompose [
          (setKey 3 0 (K "❖" c.meta K))
          (swapKeys 3 0 3 1)  # alt-meta not meta-alt
          (updateKey 3 2 (addWidth 1)) # space fills width
          (withModCol 0)
          fitWidth
        ];

        portraitArgs = {
          gap = 2;
          paddingL = 0;
          paddingR = 0;
        };

        landscapeArgs = {
          gap = 12;
          paddingL = 0.5;
          paddingR = 0.5;
        };

        mkLandscapeSplit = mods: mkSplit (landscapeArgs // {mods = mods;});
        mkPortraitSplit = mods: mkSplit (portraitArgs // {mods = mods;});

        applyLefty = f: precompose [f (Variants.lefty 2.0)];
        applyRighty = f: precompose [f (Variants.righty 2.0)];

      in 
      # Emit the variants in order for switching back and forth
      indexPrefixedAttrs [
        { L = applyLefty id; }
        { R = applyRighty id; }
        { inherit C0Mods; }
        { C0ModsL = applyLefty C0Mods; }
        { C0ModsR = applyRighty C0Mods; }
        { splitPE = mkPortraitSplit Mods.Empty; }
        { splitPG = mkPortraitSplit Mods.Grid; }
        { splitP0 = mkPortraitSplit (Mods.Col 0); }
        { splitP5 = mkPortraitSplit (Mods.Col 5); }
        { splitLE = mkLandscapeSplit Mods.Empty; }
        { splitLG = mkLandscapeSplit Mods.Grid; }
        { splitL0 = mkLandscapeSplit (Mods.Col 0); }
        { splitL5 = mkLandscapeSplit (Mods.Col 5); }
      ];

    rows = with codes; let height = 0.65; in [{
        inherit height;
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
            sw."("     se.")"
          _
                        ne."0"
                  c.p

          K;
      }

      {
        inherit height;
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
        inherit height;
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
        inherit height;
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
  }
]

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
    variants = with codes; 
      let
        addLeftMods = precompose [
          (setKey 0 0 (K c.q ne."1" sw."!" K)) # Clears Q
          (updateKey 0 2 clearCardinal.nw) # Clears W !
          # Clear home-row mods
          (setKey 1 0 (K c.a K))
          (setKey 1 1 (K c.s K))
          (setKey 2 2 (K c.d K))
          (setKey 3 3 (K c.f K))
          # Adds LHS mods
          (insertCol 0 (
            K "⎋" c.esc nw.tab ne."`" sw."~"
            _ "✲" c.ctrl "❖" sw.meta "⌥" ne.alt "▤" nw.fn
            _ c.shift
            K))
        ];

        mkSplit = gap: spacePaddingL: spacePaddingR:
          let spaceWidth = gap - spacePaddingL - spacePaddingR;
          in precompose [
            addLeftMods
            (deleteRow 3) # Removes mods, spacebar, cursor and enter
            #(setKey 1 10 (K nw.shift ne.del c.bsp "✲" sw.ctrl K)) # Re-adds backspace
            #(setKey 2 9 (K n.up w.left e.right s.down K)) # Re-adds cursor keys
            #(setKey 2 10 (K c.enter K)) # Re-adds enter
            # Insert split
            (updateKey 0 6 (addShift gap))
            (updateKey 1 6 (addShift gap))
            # Insert spacebar into centre of split
            (updateKey 2 6 (addShift spacePaddingR))
            (insertKey 2 6 (K spaceWidth spacePaddingL w.cur_l  " " c.spc  e.cur_r K))
            # Finally fit to width to scale 20 -> 10
            fitWidth
          ];
      in {
        # Add mods down the left side and remove duplicates on the old column-0
        leftMods = precompose [
          (setKey 3 0 (K "❖" c.meta K))
          (swapKeys 3 0 3 1)  # alt-meta not meta-alt
          (updateKey 3 2 (addWidth 1))
          addLeftMods
          fitWidth
        ];

        # Split layout with empty middle row for landscape mode.
        splitPortrait = mkSplit 4 0 0;
        splitLandscape = mkSplit 10 1 1;
      };

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

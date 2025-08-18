{ uklib, typed, ... }:

with typed;
with uklib;

# https://github.com/Julow/Unexpected-Keyboard/blob/master/srcs/layouts/latn_qwerty_us.xml
{
  # <keyboard name="QWERTY (US)" script="latin">
  name = "QWERTY (US)";
  bottomRow = true;
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
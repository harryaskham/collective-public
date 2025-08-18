{ uklib, typed, ... }:

with typed;
with uklib;

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
}
{ uklib, typed, ... }:

with typed;
with uklib;
with codes;

let
  macros =
    typed.recursiveMergeAttrsList
      (mapAttrsToList mkCardinal
        (with codes._; with kv; {
          float_terminal = (m "⇡" [(k ctrl) (k alt) (k f)]);
          tmux_zoom = (m "⛶" [(k ctrl) (k a) (k z)]);
          tmux_descend = (m "↑" [(k ctrl) (k a) (k ctrl) (k t)]);
          tmux_ascend = (m "↓" [(k ctrl) (k a) (k ctrl) (k g)]);
        }));
in {
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
          "⇠" w.wm_workspace_previous  c.s  "⇢" e.wm_workspace_next
                        "⌥" se.alt
        _
          "↑" n.wm_move_up  "⛶" macros.ne.tmux_zoom
          "←" w.wm_move_left  c.d  "→" e.wm_move_right
          "↓" s.wm_move_down
                        "❖" se.meta
        _
          "↑" n.wm_focus_up  "⇡" macros.ne.float_terminal
          "←" w.wm_focus_left  c.f  "→" e.wm_focus_right
          "↓" s.wm_focus_down
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
            "Hide" n.wm_hide_all  ne.copy
                c.c
          sw.config   se.emoji
        _
            "Min" n.wm_minimize_all  ne.paste
                c.v
                        se.pastePlain
        _
          "↑" macros.n.tmux_descend
                c.b
          "↓" macros.s.tmux_ascend

        _
          "Rest" n.wm_restore_all  ne."?"
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
              "⎙" n.wm_open_terminal  ne.action
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
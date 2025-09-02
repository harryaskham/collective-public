{ lib ? import <nixpkgs/lib>,
  collective-lib ? import ./. { inherit lib; }, 
  strings ? import ./strings.nix { inherit lib collective-lib; tildePath = /home/test; pwdPath = /tmp/pwd; },
  ... }:

with strings;
with collective-lib.syntax;
with collective-lib.tests;
let ansi = collective-lib.script-utils.ansi-utils.ansi;
in suite {
  split = {
    whitespace = {
      expr = splitWhitespace " \n hell   o \n\n  \t \n wor\t\tld  \n";
      expected = [ "hell" "o" "wor" "ld" ];
    };
  };

  indent = {
    set0 = {
      expr = indent.set 0 (joinLines [
        "            line1"
        "            def fn() {"
        "              line2"
        "            }"
      ]);
      expected = joinLines [
        "line1"
        "def fn() {"
        "  line2"
        "}"
      ];
    };
    set4 = {
      expr = indent.set 4 (joinLines [
        "            line1"
        "            def fn() {"
        "              line2"
        "            }"
      ]);
      expected = joinLines [
        "    line1"
        "    def fn() {"
        "      line2"
        "    }"
      ];
    };
    by3 = {
      expr = indent.by 3 (joinLines [
        " line1"
        " def fn() {"
        "   line2"
        " }"
      ]);
      expected = joinLines [
        "    line1"
        "    def fn() {"
        "      line2"
        "    }"
      ];
    };
    get = {
      expr = indent.get (joinLines [
        "            line1"
        "            def fn() {"
        "              line2"
        "            }"
      ]);
      expected = 12;
    };
  };

  trimNewlines = {
    empty = expect.eq (trimNewlines "") "";
    single = expect.eq (trimNewlines "a") "a";
    multiple = expect.eq (trimNewlines "a\nb\nc") "a\nb\nc";
    surround = expect.eq (trimNewlines "\n\na\nb\nc\n\n") "a\nb\nc";
  };

  codeBlock = {
    embedded = {
      expr =
        let body = codeBlock ''
          embed1:
            embed2
        '';
        in codeBlock ''
          def fn() {
            ${indent.here body}
          }
        '';
      expected = trimNewlines ''
        def fn() {
          embed1:
            embed2
        }
      '';
    };
    inline = {
      expr =
        let body = codeBlock ''
          1
          2
          3
        '';
        in codeBlock ''
          X = [ ${indent.here body} ]
        '';
      expected = trimNewlines ''
        X = [ 1
              2
              3 ]
      '';
    };
    lines = expect.eq (codeBlockLines [
      ""
      "line1"
      "line2"
      "line3"
      ""
    ]) "line1\nline2\nline3";
  };

  pluralise = {
    pluralise_ = {
      test0 = expect.eq (pluralise_ 0 "test" "one" "many") "testmany";
      test1 = expect.eq (pluralise_ 1 "test" "one" "many") "testone";
      test2 = expect.eq (pluralise_ 2 "test" "one" "many") "testmany";
    };
    pluralise = {
      test0 = expect.eq (pluralise 0 "test" "many") "testmany";
      test1 = expect.eq (pluralise 1 "test" "many") "test";
      test2 = expect.eq (pluralise 2 "test" "many") "testmany";
    };
    pluralises = {
      test0 = expect.eq (pluralises 0 "test") "tests";
      test1 = expect.eq (pluralises 1 "test") "test";
      test2 = expect.eq (pluralises 2 "test") "tests";
    };
  };

  Context = {
    Safe = expect.eq (Safe "${throw "no"}") "<eval failed>";
    String = 
      let
        ctx = {attr = "value";};
        mkContextTest = CS: {
          eqSelf = expect.eq CS CS;
          #eqString = expect.eq CS "string";
          eqToString = expect.eq (toString CS) "string";
          #eqContext = expect.eq (builtins.getContext CS) "TODO";
        };
      in {
        named.attrs = mkContextTest (Context.String "name" ctx "string");
        named.empty = mkContextTest (Context.String "name" "string");
        unnamed.attrs = mkContextTest (Context.String ctx "string");
      };
  };

  stringToPath = {
    tilde = expect.eq (stringToPath "~") /home/test;
    tildeWithPath = expect.eq (stringToPath "~/a/b") /home/test/a/b;
    dot = expect.eq (stringToPath "./a/b/c") /tmp/pwd/a/b/c;
    slash = expect.eq (stringToPath "/a/b/c") /a/b/c;
  };

  pad =
    let redHello = with ansi; style [underline fg.red] "hello";
        redHelloPlus1 = with ansi; style [underline fg.red] "hello┏";
        redHelloPlus1Char = with ansi; style [underline fg.red] (Strings ["hello" ansi.boxes.heavy.kneeNW]);
        redHelloPlus2 = with ansi; style [underline fg.red] "hello┏┏";
        redHelloPlus2Chars = with ansi; style [underline fg.red] (Strings ["hello" ansi.boxes.heavy.kneeNW ansi.boxes.heavy.kneeNW]);
        redHelloPlus3 = with ansi; style [underline fg.red] "hello┏┏┏";
        redHelloPlus3Chars = with ansi; style [underline fg.red] (Strings ["hello" ansi.boxes.heavy.kneeNW ansi.boxes.heavy.kneeNW ansi.boxes.heavy.kneeNW]);
        redHelloPlus4 = with ansi; style [underline fg.red] "hello┏┏┏┏";
        redHelloPlus4Chars = with ansi; style [underline fg.red] (Strings ["hello" ansi.boxes.heavy.kneeNW ansi.boxes.heavy.kneeNW ansi.boxes.heavy.kneeNW ansi.boxes.heavy.kneeNW]);
    in {
      left = expect.eq (padString {to = 10; align = "left";} "hello") "hello     ";
      right = expect.eq (padString {to = 10; align = "right";} "hello") "     hello";
      center.exact = expect.eq (padString {to = 9; align = "center";} "hello") "  hello  ";
      center.roundUp = expect.eq (padString {to = 10; align = "center";} "hello") "  hello   ";
      center.roundDown = expect.eq (padString {to = 8; align = "center";} "hello") " hello  ";
      emptyChar = expect.eq (padString {to = 10; emptyChar = "x";} "hello") "helloxxxxx";

      # Already too wide with the codes in place.
      hello.display.stringLength.lib = expect.eq (lib.strings.stringLength redHello) 20;
      hello.plus1.display.stringLength.lib = expect.eq (lib.strings.stringLength redHelloPlus1) 23;
      # UTF8 char counts as 1
      # TODO: This failure shows thy we're under-counting lines with box chars!
      # We then add too much padding b/c we undercount these characters.
      hello.display.stringLength.utf8 = expect.eq (utf8StringLength redHello) 20;
      # Fails, 20
      hello.plus1.display.stringLength.utf8 = expect.eq (utf8StringLength redHelloPlus1) 21;
      # Fails, 20
      hello.plus1Char.display.stringLength.utf8 = expect.eq (utf8StringLength redHelloPlus1Char) 21;
      # Fails, 20
      hello.plus2.display.stringLength.utf8 = expect.eq (utf8StringLength redHelloPlus2) 22;
      # Fails, 20
      hello.plus2Chars.display.stringLength.utf8 = expect.eq (utf8StringLength redHelloPlus2Chars) 22;
      # Fails, 20
      hello.plus3.display.stringLength.utf8 = expect.eq (utf8StringLength redHelloPlus3) 23;
      # Fails, 20
      hello.plus3Char.display.stringLength.utf8 = expect.eq (utf8StringLength redHelloPlus3Chars) 23;
      # Fails, 23
      hello.plus4.display.stringLength.utf8 = expect.eq (utf8StringLength redHelloPlus4) 24;
      # Fails, 23
      hello.plus4Chars.display.stringLength.utf8 = expect.eq (utf8StringLength redHelloPlus4Chars) 24;
      # UTF8 char counts as 1, colors count as zero
      hello.display.stringLength.displayLength = expect.eq (displayLength redHello) 5;
      hello.plus1.display.stringLength.displayLength = expect.eq (displayLength redHelloPlus1) 6;
      # UTF8 char counts as 1, colors count as zero
      hello.pad.display.true =
        expect.eq (padString {to = 9; align = "center"; display = true;} redHello)
        "  ${redHello}  ";
      hello.plus1.pad.display.false =
        expect.eq (padString {to = 10; align = "center"; display = false;} redHelloPlus1)
        "${redHelloPlus1}";
    };

  joinVertical = {
    empty = expect.eq (toString (joinVertical [])) "";
    single = expect.eq (toString (joinVertical [""])) "";
    twoLines = expect.eq (toString (joinVertical [
      "line1"
      "line2"
    ])) "line1line2";
    twoBlocks =
      expect.eq 
        (toString (joinVertical [
          (_b_ ''
            line1
            longerline1
          '')
          (_b_ ''
            line2
            longerline2
            line3
          '')
        ]))
        (joinLines [
          "line1      line2      "
          "longerline1longerline2"
          "           line3      "
        ]);
    manyBlocks =
      let
        a = _b_ ''
          lineA
          longerlineA
        '';
        b = _b_ ''
          headB
          

          lineB

            footB
        '';
      in {
        noSep = expect.eq 
          (toString (joinVertical [a b a b a b]))
          (joinLines [
            "lineA      headB  lineA      headB  lineA      headB  "
            "longerlineA       longerlineA       longerlineA       "
            "                                                      "
            "           lineB             lineB             lineB  "
            "                                                      "
            "             footB             footB             footB"
          ]);
        sep = expect.eq 
          (toString (joinVerticalSep "_" [a b a b a b]))
          (joinLines [
            "lineA      _headB  _lineA      _headB  _lineA      _headB  "
            "longerlineA_       _longerlineA_       _longerlineA_       "
            "           _       _           _       _           _       "
            "           _lineB  _           _lineB  _           _lineB  "
            "           _       _           _       _           _       "
            "           _  footB_           _  footB_           _  footB"
          ]);
      };
    boxes = skip (
      with ansi;
      let
        a = box {body = "a";};
        b = box {header = "b"; body = [(box {body = "c";})];};
        x = box {body = joinVertical [a b];};
      in
        expect.eq
          (stripANSI (toString x))
          (joinLines [
            "                   "
            " ┏━━━━━━━━━━━━━━━┓ "
            " ┃               ┃ "
            " ┃ ┏━━━┓  ┏━━━┓  ┃ "
            " ┃ ┃   ┃  ┃ b ┃  ┃ "
            " ┃ ┃ a ┃  ┣━━━┫  ┃ "
            " ┃ ┃   ┃  ┃   ┃  ┃ "
            " ┃ ┃   ┃  ┃ c ┃  ┃ "
            " ┃ ┃   ┃  ┃   ┃  ┃ "
            " ┃ ┗━━━┛  ┗━━━┛  ┃ "
            " ┗━━━━━━━━━━━━━━━┛ "
          ])
    );
  };

  diffStrings = {
    empty = expect.eq (diffStrings "" "") {
      __equal = "";
      __diffType = "string";
    };
    equal = expect.eq (diffStrings "a" "a") {
      __equal = "a";
      __diffType = "string";
    };
    unequal = {
      one = expect.eq (diffStrings "a" "b")
        {
          __diffType = "string";
          __unequal = [{__unequal = {first = "a"; second = "b";}; __diffType = "segment";}];
        };
      whole = expect.eq (diffStrings "abc" "def")
        {
          __diffType = "string";
          __unequal = [{__unequal = {first = "abc"; second = "def";}; __diffType = "segment";}];
        };
      prefix = expect.eq (diffStrings "abcd" "abef")
        {
          __diffType = "string";
          __unequal = [
            {__equal = "ab"; __diffType = "segment";}
            {__unequal = {first = "cd"; second = "ef";}; __diffType = "segment";}
          ];
        };
      suffix = expect.eq (diffStrings "abcd" "efcd")
        {
          __diffType = "string";
          __unequal = [
            {__unequal = {first = "ab"; second = "ef";}; __diffType = "segment";}
            {__equal = "cd"; __diffType = "segment";}
          ];
        };
      infix.full = expect.eq (diffStrings "xcdex" "acdef")
        {
          __diffType = "string";
          __unequal = [
            {__unequal = {first = "x"; second = "a";}; __diffType = "segment";}
            {__equal = "cde"; __diffType = "segment";}
            {__unequal = {first = "x"; second = "f";}; __diffType = "segment";}
          ];
        };
      infix.linewise.true =
        expect.eq
          (diffStrings_ { linewiseStringDiff = true; } "ax\nxcde\nf\nx" "ay\nycde\nf\ny")
          {
            __diffType = "lines";
            __unequal = [
              {
                __diffType = "string";
                __unequal = [
                  {__equal = "a"; __diffType = "segment";}
                  {__unequal = {first = "x"; second = "y";}; __diffType = "segment";}
                ];
              }
              {
                __diffType = "string";
                __unequal = [
                  {__unequal = {first = "x"; second = "y";}; __diffType = "segment";}
                  {__equal = "cde"; __diffType = "segment";}
                ];
              }
              {
                __diffType = "string";
                __equal = "f";
              }
              {
                __diffType = "string";
                __unequal = [
                  {__unequal = {first = "x"; second = "y";}; __diffType = "segment";}
                ];
              }
            ];
          };
      infix.linewise.false =
        expect.eq
          (diffStrings_ { linewiseStringDiff = false; } "ax\nxcde\nf\nx" "ay\nycde\nf\ny")
          {
            __diffType = "string";
            __unequal = [
              {__equal = "a"; __diffType = "segment";}
              {__unequal = {first = "x"; second = "y";}; __diffType = "segment";}
              {__equal = "\n"; __diffType = "segment";}
              {__unequal = {first = "x"; second = "y";}; __diffType = "segment";}
              {__equal = "cde\nf\n"; __diffType = "segment";}
              {__unequal = {first = "x"; second = "y";}; __diffType = "segment";}
            ];
          };
      differentLengths = expect.eq (diffStrings "xcdex" "acdefoo")
        {
          __diffType = "string";
          __unequal = [
            {__unequal = {first = "x"; second = "a";}; __diffType = "segment";}
            {__equal = "cde"; __diffType = "segment";}
            {__unequal = {first = "x"; second = "foo";}; __diffType = "segment";}
          ];
        };
      pretty =
        expect.eq 
          (diffStrings_ { prettyStringDiff = true; linewiseStringDiff = true; } "\nabc\ndef" "\naxx\nxxf\nxyz\n")
          "\nabcxx\ndexxf\n<x>xyz\n<x>";
    };
  };

  utf8StringLength = {
    single = expect.eq [(lib.stringLength "a") (utf8StringLength "a")] [1 1];
    emoji = expect.eq [(lib.stringLength "🌍") (utf8StringLength "🌍")] [4 1];
    boxDrawing = expect.eq [(lib.stringLength "┏━━") (utf8StringLength "┏━━")] [9 3];
    boxDrawingColored =
      with ansi;
      let x = style [fg.red] "┏━━";
      in expect.eq [(lib.stringLength x) (utf8StringLength x) (displayLength x)] [22 13 3];
  };

  Strings = {
    simple = expect.eq (toString (Strings ["a" "b"])) "ab";
    Join = expect.eq (toString (Join [(Strings ["a" "b"]) (Strings ["c" "d"])])) "abcd";
    Char = expect.eq (toString (Char "a")) "a";
    Lines = expect.eq (toString (Lines ["a" "b"])) "a\nb";
    Line = expect.eq (toString (Line "a")) "a\n";
    replicate = expect.eq ((Char "a").replicate 3) "aaa";
  };
}

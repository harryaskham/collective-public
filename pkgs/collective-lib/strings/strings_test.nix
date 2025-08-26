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

  pad = {
    left = expect.eq (padString {to = 10; align = "left";} "hello") "hello     ";
    right = expect.eq (padString {to = 10; align = "right";} "hello") "     hello";
    center.exact = expect.eq (padString {to = 9; align = "center";} "hello") "  hello  ";
    center.roundUp = expect.eq (padString {to = 10; align = "center";} "hello") "  hello   ";
    center.roundDown = expect.eq (padString {to = 8; align = "center";} "hello") " hello  ";
    emptyChar = expect.eq (padString {to = 10; emptyChar = "x";} "hello") "helloxxxxx";
    ignoreANSI.true =
      let redHello = with ansi; style [underline fg.red] "hello";
      in expect.eq (padString {to = 9; align = "center"; ignoreANSI = true;} redHello) "  ${redHello}  ";
    ignoreANSI.false =
      let redHello = with ansi; style [underline fg.red] "hello";
      in expect.eq (padString {to = 9; align = "center"; ignoreANSI = false;} redHello) "${redHello}"; # Already too wide with the codes in place.
  };

  joinVertical = {
    empty = expect.eq (joinVertical []) "";
    single = expect.eq (joinVertical [""]) "";
    twoLines = expect.eq (joinVertical [
      "line1"
      "line2"
    ]) "line1line2";
    twoBlocks =
      expect.eq 
        (joinVertical [
          (_b_ ''
            line1
            longerline1
          '')
          (_b_ ''
            line2
            longerline2
            line3
          '')
        ])
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
          (joinVertical [a b a b a b])
          (joinLines [
            "lineA      headB  lineA      headB  lineA      headB  "
            "longerlineA       longerlineA       longerlineA       "
            "                                                      "
            "           lineB             lineB             lineB  "
            "                                                      "
            "             footB             footB             footB"
          ]);
        sep = expect.eq 
          (joinVerticalSep "_" [a b a b a b])
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
        b = box {header = "b"; body = box {body = "c";};};
        x = box {body = joinVertical [a b];};
      in
        expect.eq (stripANSI x)
          (joinLines [
            "lineA      headB  lineA      headB  lineA      headB  "
            "longerlineA       longerlineA       longerlineA       "
            "                                                      "
            "           lineB             lineB             lineB  "
            "                                                      "
            "             footB             footB             footB"
          ])
    );
  };

  diffStrings = {
    empty = expect.eq (diffStrings "" "") {__equal = "";};
    equal = expect.eq (diffStrings "a" "a") {__equal = "a";};
    unequal = {
      one = expect.eq (diffStrings "a" "b")
        {__unequal.__stringDiff = [{
          __unequal = {first = "a"; second = "b";};}];};
      whole = expect.eq (diffStrings "abc" "def")
        {__unequal.__stringDiff = [
          {__unequal = {first = "abc"; second = "def";};}
        ];};
      prefix = expect.eq (diffStrings "abcd" "abef")
        {__unequal.__stringDiff = [
          {__equal = "ab";}
          {__unequal = {first = "cd"; second = "ef";};}];};
      suffix = expect.eq (diffStrings "abcd" "efcd")
        {__unequal.__stringDiff = [
          {__unequal = {first = "ab"; second = "ef";};}
          {__equal = "cd";}
        ];};
      infix = expect.eq (diffStrings "xcdex" "acdef")
        {__unequal.__stringDiff = [
          {__unequal = {first = "x"; second = "a";};}
          {__equal = "cde";}
          {__unequal = {first = "x"; second = "f";};}
        ];};
      differentLengths = expect.eq (diffStrings "xcdex" "acdefoo")
        {__unequal.__stringDiff = [
          {__unequal = {first = "x"; second = "a";};}
          {__equal = "cde";}
          {__unequal = {first = "x"; second = "foo";};}
        ];};
    };
  };

  utf8StringLength = 
    let 
      a = "a";
      b = "🌍";
    in solo {
      single = expect.eq [(lib.stringLength a) (utf8StringLength a)] [1 1];
      emoji = expect.eq [(lib.stringLength b) (utf8StringLength b)] [4 1];
    };
}

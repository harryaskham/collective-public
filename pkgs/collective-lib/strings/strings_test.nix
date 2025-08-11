{ lib ? import <nixpkgs/lib>,
  collective-lib ? import ./. { inherit lib; }, 
  strings ? import ./strings.nix { inherit lib collective-lib; tildePath = /home/test; },
  ... }:

with strings;
with collective-lib.tests;

suite {
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
    dot = expect.eq (stringToPath "./a/b/c") ./a/b/c;
    slash = expect.eq (stringToPath "/a/b/c") /a/b/c;
  };
}
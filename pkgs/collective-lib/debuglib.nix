let
  # NOTE: This data is fixed in place at the top of the file such that it
  # has a predictable line location.
  testData = {
    abc = 123;
    def = 456;
    deeper = {
      ghi = 789;
    };
  };
in

{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;
with collective-lib.syntax;

let
  log = collective-lib.log;
  errors = collective-lib.errors;
  typed = collective-lib.typed;
in with typed; rec {

  # Get metadata for the given value.
  # Currently only includes pos.
  __meta = dispatch {
    set = xs: {
      pos = pos xs;
    };
  };

  # Is x a raw position from the builtin fn?
  isRawPos = x: 
    x ? column && x ? line && x ? file;

  # Get the internal metadata of the given name in the given attrs.
  pos = 
    let 
      mkPos = name: rawPos: 
        if rawPos == null then null else
        rawPos // {
          inherit name;
          __toString = self: 
            "${self.name} (${self.file}:${toString self.line}:${toString self.column})";
        };

      dispatchPos = f: dispatch.def null {
        # If given a set, get positions for all its members
        set = attrs: unsafeMapAttrs (name: _: mkPos name (f name attrs)) attrs;
        # Otherwise just the one key.
        string = name: attrs: mkPos name (f name attrs);
      };

    in {
      # Default to filename only for replicable output.
      __functor = self: self.file;

      # Returns null if the name is not present.
      # File path is absolute.
      path = dispatchPos builtins.unsafeGetAttrPos;

      # Returns null if the name is not present.
      # File is filename-only.
      file = 
        let elidePos = p: if p == null then null 
                          else p // { file = builtins.baseNameOf p.file; };
        in dispatchPos (name: attrs: elidePos (pos.path name attrs));

      deep = 
        deepMergeMapParent 
          (parent: name: v:
            let 
              maybeP =
                optionalAttrs 
                  (isAttrs parent)
                  { __meta.${name} = pos name parent; };
            in { ${name} = v; } // maybeP);
    };

  # <nix>debuglib._tests.run</nix>
  _tests = with collective-lib.tests; suite {
    pos = 
      let 
        expectedPosABC = {
          name = "abc";
          column = 5;
          line = 5;
          file = "debuglib.nix";
          __toString = expect.anyLambda;
        };
        expectedPosDEF = {
          name = "def";
          column = 5;
          line = 6;
          file = "debuglib.nix";
          __toString = expect.anyLambda;
        };
        expectedPosDeeper = {
          name = "deeper";
          column = 5;
          line = 7;
          file = "debuglib.nix";
          __toString = expect.anyLambda;
        };
        expectedPosGHI = {
          name = "ghi";
          column = 7;
          line = 8;
          file = "debuglib.nix";
          __toString = expect.anyLambda;
        };
        expectedPosAll = {
          abc = expectedPosABC;
          def = expectedPosDEF;
          deeper = expectedPosDeeper;
        };
        expectedPosDeep = {
          __meta = {
            abc = expectedPosABC;
            def = expectedPosDEF;
            deeper = expectedPosDeeper;
          };
          abc = 123;
          def = 456;
          deeper = {
            __meta = {
              ghi = expectedPosGHI;
            };
            ghi = 789;
          };
        };
      in {
        present = expect.noLambdasEq (pos "abc" testData) expectedPosABC;
        absent = expect.eq (pos "xyz" testData) null;
        nested.present = expect.noLambdasEq (pos "abc" ({ deep = testData; }).deep) expectedPosABC;
        nested.absent = expect.eq (pos "abc" { deep = testData; }) null;
        merged.present = expect.noLambdasEq (pos "abc" ({} // testData)) expectedPosABC;
        overwritten.present = expect.noLambdasEq (pos "abc" ({abc = 456; } // testData)) expectedPosABC;
        # Inherits / merge-right now looks for that field in the copied attrs, so we lose the information.
        overwritten.here = expect.True (((pos "abc" (testData // {abc = 456; })).line) > expectedPosABC.line);
        inherits.here = expect.True ((pos "abc" {inherit (testData) abc;}).line > expectedPosABC.line);
        attrs.all = expect.noLambdasEq (pos testData) expectedPosAll;
        attrs.deep = expect.noLambdasEq (pos.deep testData) expectedPosDeep;
        # Can't pull attrs out of derived set.
        attrs.overwrite.noset =
          expect.noLambdasEq
            (pos (mapAttrs (_: _: 9) testData))
            {abc = null; def = null; deeper = null;};
        # Can't pull attrs out of derived set.
        attrs.inside.lambda = 
          let f = data: pos "abc" data;
          in expect.noLambdasEq (f testData) expectedPosABC;
        toString = expect.eq (toString (pos "abc" testData)) "abc (debuglib.nix:5:5)";
      };
  };

}

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
  testSortData = {
    z = "z";
    b = "b"; a = "a";
  };
  testArgNames = {
    zArg,
    bArg, aArg
  }: {};
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

  # Does p occur before q in the source file?
  cmpPos = p: q:
    assert that (p.file == q.file) ''
      pos.cmpPos: p.file (${p.file}) != q.file (${q.file})
    '';
    p.line < q.line || (p.line == q.line && p.column < q.column);

  # Sort a list of positions by their source file location.
  sortPosList = sort cmpPos;
  sortedPos = xs: sortPosList (attrValues (pos xs));

  # Get the position of the given attrPath in the given expr.
  pathPos = attrPath: expr:
    if length attrPath == 0 then null
    else 
      let 
        go = attrPath: parent:
          if length attrPath == 1 then pos (head attrPath) parent
          else go (tail attrPath) (parent.${head attrPath});
      in go attrPath expr;

  isPos = x: x ? __isPos;
  isPosAttrs = x: isAttrs x && nonEmpty x && isPos (head (attrValues x));

  # Get the internal metadata of the given name in the given attrs.
  pos = 
    let 
      mkPos = name: rawPos: 
        if rawPos == null then null else
        rawPos // {
          inherit name;
          __isPos = true;
          __toString = self: 
            "${self.name} (${self.file}:${toString self.line}:${toString self.column})";
        };

      dispatchPos = f: dispatch.def null {
        # If given a set, get positions for all its members
        set = attrs: 
          (Unsafe.mapAttrs (name: _: mkPos name (f name attrs)) attrs);

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
        deepMapParent
          (parent: k: v:
            if isAttrs parent then pos k parent
            else v);
    };

  defaultPrintPosOpts = {
    linesBefore = 0;
    linesAfter = 0;
    printHeader = false;
    printEllipses = false;
    posSep = "\n";
    maxDepth = 5;
    emptyMsg = "<printPos_: no position information available>";
    errorMsg = "<printPos_: error printing pos>";
  };

  printPos_ = opts: p: 
    log.while "printing the positional information for a value" (
    flip errors.tryStrict (_: opts.errorMsg) (
    if p == null || (isAttrs p && all (x: x == null) (attrValues p)) then
      opts.emptyMsg

    else if opts.maxDepth <= 0 then
      "<printPos_: maxDepth reached>"

    else if !(isPos p || isPosAttrs p) then
      assert that (isAttrs p) ''
        debuglib.printPos: cannot print non-attrs pos p
      '';
      log.while "obtaining positional information for printing" (
      printPos_ opts (pos.path p)
      )

    else if isPosAttrs p then
      log.while "printing positional information for an attrset" (
      joinSep opts.posSep 
        (mapAttrsToList 
          (_: printPos_ (opts // { maxDepth = opts.maxDepth - 1; }))
          p)
      )

    else
      log.while "printing positional information for an individual position" (
      let sourceLines = splitLines (builtins.readFile p.file);
          toEnd = p.line + opts.linesAfter >= length sourceLines;
          fromStart = p.line - opts.linesBefore < 1;
          lines = 
            take 
              (1 + opts.linesBefore + opts.linesAfter) 
              (drop (p.line - opts.linesBefore) sourceLines);
          header = if opts.printHeader then "${toString p}\n---" else "";
          truncatedSource = _ls_ (
            (optionals (!fromStart && opts.printEllipses) ["..."]) ++
            (lines) ++
            (optionals (!toEnd && opts.printEllipses) ["..."])
          );
      in _ls_ [header truncatedSource]
      )
  ));

  printPos = x: 
    log.while "printing the default positinal information for a value" (
    printPos_ defaultPrintPosOpts x
    );
  printPosWith = opts: x: printPos_ (defaultPrintPosOpts // opts) x;

  # <nix>debuglib._tests.run {}</nix>
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
        expectedPosZ = {
          name = "z";
          column = 5;
          line = 12;
          file = "debuglib.nix";
          __toString = expect.anyLambda;
        };
        expectedPosB = {
          name = "b";
          column = 5;
          line = 13;
          file = "debuglib.nix";
          __toString = expect.anyLambda;
        };
        expectedPosA = {
          name = "a";
          column = 14;
          line = 13;
          file = "debuglib.nix";
          __toString = expect.anyLambda;
        };
        expectedPosZArg = {
          name = "zArg";
          column = 5;
          line = 16;
          file = "debuglib.nix";
          __toString = expect.anyLambda;
        };
        expectedPosBArg = {
          name = "bArg";
          column = 5;
          line = 17;
          file = "debuglib.nix";
          __toString = expect.anyLambda;
        };
        expectedPosAArg = {
          name = "aArg";
          column = 11;
          line = 17;
          file = "debuglib.nix";
          __toString = expect.anyLambda;
        };
        expectedPosAll = {
          abc = expectedPosABC;
          def = expectedPosDEF;
          deeper = expectedPosDeeper;
        };
        expectedPosDeep = {
          abc = expectedPosABC;
          def = expectedPosDEF;
          deeper = {
            ghi = expectedPosGHI;
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
        sortPos.nosorted = expect.noLambdasEq (attrValues (pos testSortData)) [expectedPosA expectedPosB expectedPosZ];
        sortPos.sorted = expect.noLambdasEq (sortedPos testSortData) [expectedPosZ expectedPosB expectedPosA];
        sortPos.argOrder.nosorted = 
          expect.noLambdasEq
            (attrValues (pos (builtins.functionArgs testArgNames)))
            [expectedPosAArg expectedPosBArg expectedPosZArg];
        sortPos.argOrder.sorted = 
          expect.noLambdasEq
            (sortedPos (builtins.functionArgs testArgNames))
            [expectedPosZArg expectedPosBArg expectedPosAArg];
        pathPos = expect.noLambdasEq (pathPos ["deeper" "ghi"] testData) expectedPosGHI;
      };
  };

}

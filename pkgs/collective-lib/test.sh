# (load-file "test.el")
function main() {
  NIX_FLAGS=
  REPL_FLAGS="--raw --show-trace"
  TRACE_LEVEL="0"
  ENABLE_PARTIAL_TRACE="false"
  ENABLE_VERBOSE_TRACE="false"
  PRINT="collective-lib.log.print"
  while [[ -n "$1" ]]; do
    case "$1" in
      v)
        PRINT="collective-lib.log.vprint"
        REPL_FLAGS="--raw --show-trace"
        TRACE_LEVEL=1
        shift
        ;;
      vv)
        PRINT="collective-lib.log.vprint"
        REPL_FLAGS="--raw --show-trace"
        TRACE_LEVEL=2
        ENABLE_VERBOSE_TRACE="true"
        shift
        ;;
      vvv)
        PRINT="collective-lib.log.vprint"
        REPL_FLAGS="--raw --show-trace"
        TRACE_LEVEL=3
        ENABLE_PARTIAL_TRACE="true"
        ENABLE_VERBOSE_TRACE="true"
        shift
        ;;
      vtr)
        PRINT="lib.trivial.id"
        REPL_FLAGS="--show-trace"
        TRACE_LEVEL=0
        shift
        ;;
      vvtr)
        PRINT="lib.trivial.id"
        REPL_FLAGS="--show-trace"
        TRACE_LEVEL=2
        ENABLE_PARTIAL_TRACE="true"
        ENABLE_VERBOSE_TRACE="true"
        ;;
      vvvtr)
        PRINT="lib.trivial.id"
        REPL_FLAGS="--show-trace"
        TRACE_LEVEL=3
        ENABLE_PARTIAL_TRACE="true"
        ENABLE_VERBOSE_TRACE="true"
        shift
        ;;
      vt)
        PRINT="collective-lib.log.print"
        REPL_FLAGS="--raw --show-trace"
        TRACE_LEVEL=0
        shift
        ;;
      vvt)
        PRINT="collective-lib.log.print"
        REPL_FLAGS="--raw --show-trace"
        TRACE_LEVEL=2
        ENABLE_PARTIAL_TRACE="true"
        ENABLE_VERBOSE_TRACE="true"
        shift
        ;;
      vvvt)
        REPL_FLAGS="--raw --show-trace"
        PRINT="collective-lib.log.vprint"
        TRACE_LEVEL=3
        ENABLE_PARTIAL_TRACE="true"
        ENABLE_VERBOSE_TRACE="true"
        shift
        ;;
      *)
        EXPR="$@"
        break
        ;;
    esac
  done

  FULL_EXPR=$(cat << EOF
let
  pkgs = import <nixpkgs> {};
  lib = pkgs.lib;
  collective-lib = import ~/collective/collective-public/pkgs/collective-lib {
    inherit pkgs lib;
    traceLevel = $TRACE_LEVEL;
    enablePartialTrace = $ENABLE_PARTIAL_TRACE;
    enableVerboseTrace = $ENABLE_VERBOSE_TRACE;
  };
  __ctx = _:
    collective-lib.typed
    // {
      inherit lib;
      __replPrint = $PRINT;
    };
in
  with __ctx {};
  __replPrint (${EXPR})
EOF
              )

  CMD="nix eval --option max-call-depth 1000000000 --extra-experimental-features pipe-operators --impure --expr '$FULL_EXPR' $REPL_FLAGS"

  echo "Running: ${CMD}" >&2
  bash -c "$CMD"
}

main $@

NOOP=$(cat << EOF
(load-file "test.el")

<nix>
functions.enumerate [1 2 3]
</nix>

<nix>
"test"
</nix>

<nix>
with Types.Universe.U_3;
rec {
  a = (Fields.new [{a = Int;}]).update [{b = Default Int 666;}];
  b = (Fields.new [{a = Int;}]);
  c = Fields.new {};
  d = Type;
  e = a.indexed {};
  f = (OrderedOf Int).new { a = 1; b = 2; };
  g = f.indexed {};
  h = a.getSolos {};
  i = a.instanceFields {};
  j = (Int.getFields {});
  k = (Field.new "a" Int);
  l = (lambda a.fieldSpecs) {};
  m = parseFieldSpec Int;
  n = Int;
  o = (String.new "wat").__value.value;
}.e
</nix>

<nix>
Type.name
</nix>

<nix>
_tests.run
</nix>

<nix>
isNull Nil
</nix>

<nix>
collective-lib._testsUntyped.run
</nix>

<nix>
typelib._tests.run
</nix>
<nix>
typelib._tests.debug
</nix>

<nix>
log._tests.run
</nix>
<nix>
attrsets._tests.run
</nix>
<nix>
lists
</nix>

<nix>
Nil
</nix>

<nix>
Int.new 123
</nix>

<nix>
typelib._tests.debug
</nix>

<nix>
with attrs;
with Types.Universe.U_2;
#Int.mk { __value = (BuiltinOf Int).mk { value = 123; }; }
#(BuiltinOf "int").new 123
Int.new 123
</nix>

<nix>
null
</nix>

<nix>
with log; _tests.run
</nix>

<nix>
with attrs; _tests.run
</nix>

<nix>
with functions; _tests.run
</nix>

<nix>
with log;
with attrs;
with functions;
with dispatch;
with Types.Universe.U_1;
#let B = Int.subType "B" {}; in
let B = TypeThunk; in
(B.fields B).instanceFields {}
</nix>

<nix>
with typelib; with Types.Universe.U_1; (Type.new "wat" {}).new {}
</nix>

<nix>
with Types.Universe.U_0;
rec {
  res = TypeThunk;
}.res
</nix>

<nix>
with attrs;
with functions;
with typelib.Types;
with Universe;
log.vprintD 5 rec {
  # U_0__boot = U_0.__Bootstrap;
  # U_0__args = U_0.__Bootstrap.Type__args;
  # U_0__argsGrounded = U_0.__Bootstrap.Type__argsGrounded;
  # U_0__Type__bootstrapped = log.vprintD 1 U_0.__Bootstrap.Type__bootstrapped;
  # U_0__Type__new = log.vprintD 1 U_0.__Bootstrap.Type__new;
  # U_0__Type = log.vprintD 1 U_0.Type;
  # U_1__boot = U_1.__Bootstrap;
  # U_1__args = U_1.__Bootstrap.Type__args;
  # U_1__argsGrounded = U_1.__Bootstrap.Type__argsGrounded;
  # U_1__Type__bootstrapped = log.vprintD 3 U_1.__Bootstrap.Type__bootstrapped;
  # U_1__Type__new = log.vprintD 3 U_1.__Bootstrap.Type__new;
  # U_1__Type = log.vprintD 3 U_1.Type;
  # U_1__args__fields = U_1.__Bootstrap.Type__args.fields U_1__args;
  # U_2__boot = U_2.__Bootstrap;
  # fs = U_0.parseFieldSpec (U_1.Default U_1.TypeThunk (U_1.TypeThunk.new U_1.Type));
  # f0 = with U_0; Field.new "some" Int;
  # f1 = with U_1; Field.new "some" Int;
  # fs1 = with U_1; Fields.new {some = Int;};
  # U_2__tlon = [
  #   (U_2.isTypeLikeOrNull U_1.Set)
  #   (U_2.isTypeLikeOrNull null)
  #   (U_2.isTypeLikeOrNull (U_1.Null.new null))
  #   (U_2.isTypeLikeOrNull U_1.Type)
  #   (U_2.isTypeLikeOrNull U_1.TypeThunk)
  #   #(U_2.isTypeLikeOrNull (U_1.TypeThunk.new U_1.Type))
  #   (U_2.isTypeLikeOrNull 123)
  #   (U_2.isTypeLikeOrNull (U_1.Int.new 123))
  # ];
  #U_2__args = U_2.__Bootstrap.Type__args;
  #args__fields =
  #  with U_2; with __Bootstrap;
  #  let fs = Type__args.fields Type__args; in
  #  fs.getSolos {};
  wat_args__fields =
    with U_2; with __Bootstrap;
    let fs = Type__args.fields Type__args; in
    map (x: x.getValue {}) (fs.getSolos {});
  #U_2__args__fields = U_2.__Bootstrap.Type__argsGrounded.fields U_2.__Bootstrap.Type__argsGrounded;
  #U_2__argsGrounded = U_2.__Bootstrap.Type__argsGrounded;
  # U_2__Type__bootstrapped = log.vprintD 3 U_2.__Bootstrap.Type__bootstrapped;
  # U_2__Type__new = log.vprintD 3 U_2.__Bootstrap.Type__new;
  # U_2__Type = log.vprintD 3 U_2.Type;
}
</nix>
EOF
      )

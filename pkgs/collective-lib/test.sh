# (load-file "test.el")
function main() {
  NIX_FLAGS=
  REPL_FLAGS="--raw"
  TRACE_LEVEL="0"
  ENABLE_PARTIAL_TRACE="false"
  ENABLE_VERBOSE_TRACE="false"
  PRINT="collective-lib.log.print"
  while [[ -n "$1" ]]; do
    case "$1" in
      v)
        PRINT="collective-lib.log.print"
        REPL_FLAGS="--raw"
        TRACE_LEVEL=1
        shift
        ;;
      vv)
        PRINT="collective-lib.log.vprint"
        REPL_FLAGS="--raw"
        TRACE_LEVEL=2
        ENABLE_VERBOSE_TRACE="true"
        shift
        ;;
      vvv)
        PRINT="collective-lib.log.vprint"
        REPL_FLAGS="--raw"
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
        REPL_FLAGS="--show-trace"
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
    lib // collective-lib // {
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
with types.Types;
with Universe;
map (U: with U;
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
}.i
) [
U_0
]
</nix>

<nix>
_tests.run
</nix>

<nix>
with types; _tests.run
</nix>

<nix>
with types; _tests.debug
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
with types; with Types.Universe.U_1;
#let B = Int.subType "B" {}; in
let B = TypeThunk; in
(B.fields B).instanceFields {}
</nix>

<nix>
with types; with Types.Universe.U_1; (Type.new "wat" {}).new {}
</nix>

<nix>
with functions;
with types.Types;
with Universe;
with U_0;
type (String.new ""))
</nix>

<nix>
with attrs;
with functions;
with types.Types;
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

<nix>
with types; attrNames _tests.runOne
</nix>

<nix>
123
</nix>

<nix>
with types; _tests.runOne.test-types__instantiation__U_0__Literal__instance
</nix>

<nix>
_tests.run
</nix>

<nix>
collections._tests.run
</nix>

<nix>
attrs._tests.run
</nix>

<nix>
strings._tests.run
</nix>

<nix>
with types; _tests.debug
</nix>

<nix>
with types; _tests.run
</nix>

<nix>
builtins.tryEval ((_: {}.no) x)
</nix>

<nix>
with functions;
with types;
with Types;
with Universe.U_0;
#builtinNameToBuiltinName
# isType String "wat"
isTypeLike TypeThunk
</nix>


<nix>
with functions;
with types; with Types.Universe.U_1;
with Types;
let A = Type.new "A" { fields = This: Fields.new { x = "int"; };}; in
let a = A.new 123; in
let B = A.subType "B" {}; in
let b = B.new 123; in
b
</nix>

<nix>
with types.Types;
with log.prints;
putD 1 Universe.U_0 ___
</nix>

with types.Types.Universe.U_0; (OrderedItem Int).new { a = 1; }
with types.Types.Universe.U_1; (OrderedItem Int).new { a = 1; }
with types.Types.Universe.U_2; (OrderedItem Int).new { a = 1; }

with types.Types.Universe.U_0; Set.new {a = 1;}
with types.Types.Universe.U_1; Set.new {a = 1;}

123

with types.Types.Universe.U_0; (SetOf Int).new {a = 1;}
with types.Types.Universe.U_1; (SetOf Int).new {a = 1;}
with types.Types.Universe.U_2; (SetOf Int).new {a = 1;}

with types.Types.Universe.U_0; (((SetOf Int)).new {a = 1;})
with types.Types.Universe.U_1; (((SetOf Int)).new {a = 1;})
with types.Types.Universe.U_2; (((SetOf Int)).new {a = 1;})

with types.Types.Universe.U_0; ((Sized 1 (SetOf Int)).new {a = 1;})
with types.Types.Universe.U_1; ((Sized 1 (SetOf Int)).new {a = 1;})
with types.Types.Universe.U_2; ((Sized 1 (SetOf Int)).new {a = 1;})
with types.Types.Universe.U_2; (Sized 1 (SetOf Int))

with types.Types.Universe.U_1; with log.prints; (OrderedOf Int).new [{a = 1;} {b = 2;}]

elem 1 [1 2]
with types.Types; with log.prints; with Universe.U_1; put Fields _raw ___
with types.Types; with log.prints; with Universe.U_1; put ((Fields.new [{a = String;} {b = Int;}])) _raw ___


with types.Types; with log.prints; put Universe (_depth 99) ___
with types.Types; with log.prints; with Universe.U_0.__Bootstrap; put {inherit Type__args Type__handmade Type__new;} (_depth 99) _raw ___
with types.Types; with log.prints; put Universe.U_0.__Bootstrap.Type__handmade (_depth 1) _raw ___
with types.Types; with log.prints; put Universe.U_0.__Bootstrap.Type__new (_depth 1) _raw ___
with types.Types; with log.prints; put Universe.U_0.opts (_depth 1) ___
with types.Types; printUniverse Universe.U_0

with types.Types.Universe.Type; let A = Type.new "A" { fields = Fields.new [{x = Default Int 10;}];}; in (A.new 20).modify.x (a: Int.new (a.x + 1))
with types.Types.Universe.Quasiverse; Fields.new [{a = Int;}]
with types.Types; mapAttrs (_: printUniverse) Universe
with types.Types; with log.prints; (put (Quasiverse.Field.new 0 "wat" "int") using.oneLine ___)
elemAt [1 2 3] 2

with types.Types; with log.prints; (put [1 2 3] using.oneLine ___)

with lists; _tests.run
with functions; _tests.run

with functions; (Variadic.mkList) 1 2 3 ___
with functions; with log; printWith id 123
with functions; with log; with printing; it 123 using.ignoreToString ___
with functions; with log.prints; block (here 123 using.raw ___) using.raw ___

with types.Types.Universe.HyperType; (ListOf Int).new [1]
with types.Types.Universe.HyperType; (SetOf Int).new {a=1;}
with types.Types.Universe.HyperType; U
with types.Types.Universe.HyperType; Bool.new true
with types.Types.Universe.HyperType; (Sized 1 List).new [1]

Get this working
with types.Types.Universe.Quasiverse; (Sized 1 List).new [1]
with types.Types.Universe.QuasiType; (Sized 1 List).new [1]
with types.Types.Universe.HyperType; (Sized 1 List).new [1]
with types.Types.Universe.MetaType; (Sized 1 List).new [1]

with types.Types.Universe.QuasiType; (Literal_.bind { V = 1; }).new.getLiteral
with types.Types.Universe.QuasiType; (Literal_.bind { V = 1; }).getLiteral
with types.Types.Universe.MetaType; (Literal_.bind { V = 1; }).getLiteral





with types.Types.Universe.Type; Type



with types.Types; with Universe.HyperType; (List.subType "wat" {}).Super
with types.Types; with Universe.HyperType; typeEq List List

with types.Types; with Universe.HyperType; isTypeSet List
with types.Types.Universe.HyperType; (ListOf Int).Super

with types.Types; Quasiverse.Bootstrap.args
with types.Types; mkTypeFieldsFor Quasiverse

with types.Types; mkBuiltins Quasiverse
with types.Types; attrNames Quasiverse.Type__.staticMethods

with types.Types; Quasiverse.Type
with types.Types.Quasiverse; (Type.new "wat" {fields = Fields.new [{x = null;}]; ctor = _: {x = 123;};}).new

with types.Types.Quasiverse; let T = Type.new "Type" typeArgs; in T.new "wat" {}
with types.Types; mkUniverse Quasiverse.Type "Quasiverse"

EOF
      )

function main() {
  NIX_FLAGS=
  REPL_FLAGS=
  TRACE_LEVEL="0"
  ENABLE_PARTIAL_TRACE="false"
  ENABLE_VERBOSE_TRACE="false"
  PRINT="x: x"
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
{
  a = (Fields.new [{a = null;}]).update [{b = null;}];
  b = (Fields.new [{a = null;}]);
}.b
) [
U_2
]
</nix>

<nix>
_tests.run
</nix>

<nix>
with types; _tests.run
</nix>

<nix>
with log; _tests.run
</nix>

<nix>
with types; _tests.debug
</nix>

<nix>
with attrs; _tests.run
</nix>

<nix>
with functions; _tests.run
</nix>

<nix>
with types; with Types.Universe.U_0;
let B = Int.subType "B" {}; in
B.new 6
</nix>

<nix>
with types; with Types.Universe.U_1; Type.new "wat" {}
</nix>

<nix>
with functions;
with types.Types;
with Universe;
U_0.String.new "wat"
</nix>

<nix>
with functions;
with types.Types;
with Universe;
rec {
  # U_0__args = U_0.__Bootstrap.Type__args;
  #U_0__boot = U_0.__Bootstrap;
  U_0__boot = U_0.__Bootstrap.Type__bootstrapped;
  # U_0__Type =U_0.Type.get;
  # U_0__args__fields = U_0.__Bootstrap.Type__args.fields U_0__args;
  # U_1__args = U_1.__Bootstrap.Type__args;
  # U_1__Type = U_1.__Bootstrap.Type;
  # U_2__Type = U_2.__Bootstrap.Type;
  # U_3__Type__args = U_3.__Bootstrap.Type__args;
  # U_0__Type__args__fields = U_0.__Bootstrap.Type__args.fields U_0.__Bootstrap.Type__unsafe;
  # U_1__Type__args__fields = U_1.__Bootstrap.Type__args.fields U_1.__Bootstrap.Type__unsafe;

  # U_1__Type__fields = U_1.Type.fields U_1.Type;
  # U_1__Fields = U_1.Fields;
  # U_2__Fields = U_2.Fields.new [];
  # U_1__Set = U_1.Set.new {};
  # U_2__Set = U_2.Set.new {};
  # U_1__Type__fields = U_1.Type.fields U_1.Type;
  # U_1__Type__field = (U_1.Type.fields U_1.Type).getField "name";

  # U_2__Type__args__fields = U_2.__Bootstrap.Type__args.fields U_2.__Bootstrap.Type__unsafe;
  # U_3__Type__args__fields = U_3.__Bootstrap.Type__args.fields U_3.__Bootstrap.Type__unsafe;
  # U_3__Type = U_3.__Bootstrap.Type;
  # U_2__args = U_2.__Bootstrap.Type__args;
  # U_2__Type__new = U_2.__Bootstrap.Type__new;
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
Type.fields Type
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

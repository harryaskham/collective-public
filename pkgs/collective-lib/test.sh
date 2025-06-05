function main() {
  FLAGS=
  PRINT="log.print"
  while [[ -n "$1" ]]; do
    case "$1" in
      v)
        PRINT="log.vprint"
        FLAGS="--trace-verbose"
        shift
        ;;
      vv)
        PRINT="log.vprint"
        FLAGS="--trace-verbose --show-trace"
        shift
        ;;
      vt)
        FLAGS="--show-trace"
        shift
        ;;
      *)
        EXPR="$@"
        break
        ;;
    esac
  done

  FULL_EXPR="let pkgs = import <nixpkgs> {}; in with pkgs.lib; with (import ~/collective/collective-public/pkgs/collective-lib {}); ${PRINT} (${EXPR})"

  CMD="nix eval --extra-experimental-features pipe-operators --impure --expr '$FULL_EXPR' --raw $FLAGS"

  echo "Running: ${CMD}" >&2
  bash -c "$CMD"
}

main $@

# C-x C-e on the (progn ...) form below
function noop() {
  CONFIG_EL=$(cat << EOF

(progn
  (defun forward-nix-block (&optional n)
    (interactive)
    (unless n (setq n 1))
    (let ((do-search (if (> n 0) 'search-forward 'search-backward))
          (target (if (> n 0) "</nix>" "<nix>")))
      (dotimes (_ (abs n)) (funcall do-search target))))

  (defun current-nix-block ()
    (interactive)
    (replace-regexp-in-string "<nix>" ""
      (replace-regexp-in-string "</nix>" ""
        (thing-at-point 'nix-block))))

  (defun current-nix-expr ()
    (interactive)
    (replace-regexp-in-string "\n" " "
      (current-nix-block)))

  (defun run-nix (&optional args)
    (interactive)
    (let ((nix-code (current-nix-expr)))
     (shell-command (concat "./test.sh '" args " " nix-code "'"))))

  (defun run-nix-v () (interactive) (run-nix "v"))
  (defun run-nix-vv () (interactive) (run-nix "vv"))
  (defun run-nix-vt () (interactive) (run-nix "vt"))

  (map!
   :map global-map
   :n "SPC c n" #'run-nix
   :n "SPC c v n" #'run-nix-v
   :n "SPC c v v n" #'run-nix-vv
   :n "SPC c v t n" #'run-nix-vt
  )
)

EOF
              )

  NOOP=$(cat << EOF

<nix>
with types.Types;
with Universe;
map (U: with U;
{
  a = (Fields.new [{a = null;}]).update [{b = null;}];
  b = (Fields.new [{a = null;}]);
}.b
) [
U_1
]
</nix>

<nix>
with types; _tests.run
</nix>

<nix>
with types; _tests.debug
</nix>

<nix>
with types.Types;
with log.prints;
putD 1 Universe.U_4 ___
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
}

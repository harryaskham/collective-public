/*
Sanity check:
$ nix eval -f examples/expr.nix
[ 1 2 3 ]

Parsing:
$ nix repl
nix-repl> :lf .
nix-repl> lib.x86_64-linux.parser.parse (builtins.readFile ./examples/expr.nix)
*/

let
  a = 1;
  b = let xs = rec { value = 2; other = value; };
      in xs.other;
  c = {}.value or 3;
in
  let 
    ctx = {
      inherit a;
      "${"b"}" = b;
      inherit ({"c" = c;}) c;
    };
  in
    with ctx;
    [ a b c ]

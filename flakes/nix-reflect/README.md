## nix-reflect
Pure-Nix parser, evaluator, and reflection utilities for Nix expressions.

- A full Nix parser written in pure Nix, based on `nix-parsec`.
- A complete Nix evaluator that can reduce ASTs produced by the parser.
- A reflection library that augments importable Nix code with reflective abilities by self-parsing and self-evaluation.
- Misc:
  - A small toolkit to build and transform text/AST functions (`fn`)
  - A shim Nix 'evaluator' that operates by saving expressions to the Nix store, enabling quick if inefficient string -> value conversion.

### Parsing

```nix
parser.parse "let a = rec { x = 1 + 1; y = x + 1; }; in a.x + a.y"  
```

```nix
# trace:
# let _ in _
#   │ bindings ╪ ┆0┆ <_ = _>
#   │          │ ┆ ┆   │ lhs ╪ `a`
#   │          │ ┆ ┆   │ rhs ╪ rec {_}
#   │          │ ┆ ┆   │     │   │ bindings ╪ ┆0┆ <_ = _>
#   │          │ ┆ ┆   │     │   │          │ ┆ ┆   │ lhs ╪ `x`
#   │          │ ┆ ┆   │     │   │          │ ┆ ┆   │ rhs ╪ <_ + _>
#   │          │ ┆ ┆   │     │   │          │ ┆ ┆   │     │   │ lhs ╪ ℤ 1
#   │          │ ┆ ┆   │     │   │          │ ┆ ┆   │     │   │ rhs ╪ ℤ 1
#   │          │ ┆ ┆   │     │   │          │ ┆1┆ <_ = _>
#   │          │ ┆ ┆   │     │   │          │ ┆ ┆   │ lhs ╪ `y`
#   │          │ ┆ ┆   │     │   │          │ ┆ ┆   │ rhs ╪ <_ + _>
#   │          │ ┆ ┆   │     │   │          │ ┆ ┆   │     │   │ lhs ╪ `x`
#   │          │ ┆ ┆   │     │   │          │ ┆ ┆   │     │   │ rhs ╪ ℤ 1
#   │          │ ┆ ┆   │     │   │    isRec ╪ true
#   │     body ╪ <_ + _>
#   │          │   │ lhs ╪ <_ . _>
#   │          │   │     │   │ lhs ╪ `a`
#   │          │   │     │   │ rhs ╪ <_._._>
#   │          │   │     │   │     │   │ path ╪ ┆0┆ `x`
#   │          │   │ rhs ╪ <_ . _>
#   │          │   │     │   │ lhs ╪ `a`
#   │          │   │     │   │ rhs ╪ <_._._>
#   │          │   │     │   │     │   │ path ╪ ┆0┆ `y`
{
  __args = { ... };
  __isAST = true;
  __src = "let a = rec { x = 1 + 1; y = x + 1; }; in a.x + a.y";
  __type = { ... };
  bindings = [ ... ];
  body = { ... };
  nodeType = "letIn";
  ...
}
```

### Evaluating

```nix
eval "let a = rec { x = 1 + 1; y = x + 1; }; in a.x + a.y"
# => Right 5
```

### Lambdas as values with `fn`
As long as the function starts as a string and is evaluated into a lambda, we can treat it
as a single representation for a serializable function.

```nix
let
  f = eval.fn "x: x + 1";  # build a callable function from text
in f 41  # => 42
```

Transform the underlying text or AST and re-use:
```nix
let
  f = eval.fn "a: b: 3 * a + b";
  g = f.mapText (t: "z: ${t} + z");
in g 5 3 1  # => 15
```

### Transform an AST and re-evaluate
```nix
let
  ast = parser.parse "1 + 2";
  transformed = ast.mapNode (n: n // { op = "-"; lhs = n.rhs; rhs = n.lhs; });
in (eval transformed).right  # => -1
```

# WIP

- [ ] laziness; proven out with thunks for e.g. list values, not yet used for attrvalues. Need own builtin types Attrs that can have its attributes inspected without forcing it.

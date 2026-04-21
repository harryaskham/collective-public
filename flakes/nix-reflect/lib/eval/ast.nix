{ lib, collective-lib, nix-reflect, ... }:

let
  inherit (nix-reflect) parser debuglib eval;
  inherit (parser) AST N isAST parse printBoxed;
  inherit (eval.monad) Thunk isThunk Eval;  # Explicit inclusion to avoid shadow fail against typed.functions.Thunk
  inherit (Eval) do;
  inherit (collective-lib.typed.script-utils.ansi-utils) ansi;
in 
with collective-lib.typed;
with eval.monad;
rec {
  # Module callable as eval.ast
  __functor = self: self.evalAST;

  strictness = strict: if strict then "strictly" else "lazily";

  /*
  Parse the expression in the Eval monad and drop the state from the result.

  Exposed as eval.eval.ast (and eval.eval) in default.nix for use as just "eval"
 
  runAST :: (string | AST) -> Either EvalError {a :: a, s :: EvalState} */
  runAST = runAST_ false;
  runAST' = runAST_ true;
  runAST_ = strict: expr:
    maybePrintStackTrace (
    (do
      (whileV 1 {_ = _b_ ''
        ${strictness strict} evaluating expression or AST node:
        
        ${_p_ expr}
      '';})
      (evalM strict expr))
    .run (EvalState.mempty {})
    );

  /*
  evalAST :: (string | AST) -> Either EvalError a */
  evalAST = evalAST_ false;
  evalAST' = evalAST_ true;
  evalAST_ = strict: expr:
    maybePrintStackTrace (
    (do
      (whileV 1 {_ = _b_ ''
        ${strictness strict} evaluating expression or AST node:
        
        ${_p_ expr}
      '';})
      (evalM strict expr))
    .run_ (EvalState.mempty {})
    );

  maybePrintStackTrace = r: r.case {
    Left = e: 
      (log.v 1).show ("\n" + (with ansi; box {
        header = style [fg.red bold] "Evaluation Error";
        body = _b_ ''
          ${e}
          ${e.__printStackTrace {}}
        '';
        margin = zeros;
      })) r;
    Right = _: r;
  };

  /*
  evalM :: (string | AST) -> bool -> Eval a */
  evalM = strict: expr:
    let parsed = parse expr;
    in with (log.v 1).call "evalM" strict expr ___;
    {_, ...}: _.do
      (whileV 2 {_ = (_b_ ''
        ${strictness strict} evaluating parsed AST node:

        ${printBoxed parsed}
      '');})
      {result = if strict then toNixM parsed else toNixLazyM parsed;}
      {state = get;}
      ({result, state, _}: _.do
        (whileV 1 {_ = (_b_ ''
          returning evaluation result:

          ${_ph_ result}
        '');})
        (whileV 4 {_ = (_b_ ''
          with final state:
          ${printBoxed (removeBuiltins state)}
        '');})
        (whileV 5 {_ = (_b_ ''
          with final stack:
          ${with ansi; box {
            header = style [bold] "Stack";
            body = _p_ state.stack;
          }}
        '');})
        (pure result));

  sig = toNixSignature;
  toNixSignature = x:
    log.while "building dispatch signature for value:\n${_p_ x}" (
    if isEvalError x then "EvalError"
    else if isMonadOf Eval x then "Eval"
    else if isAST x then "AST"
    else if isThunk x then "Thunk"
    else if isEvaluatedLambda x then "EvaluatedLambda"
    else if isBinOp x then "BinOp"
    else if x ? __functor then "Functor"
    else if builtins.isFunction x then "Function"
    else if isList x then "List"
    else if isAttrs x then "Attrs"
    else "Value"
    );

  toNix = x: ((Eval.do (toNixM x)).run_ {}).case {
    Left = throw;
    Right = id;
  };

  toNixM = x: {_, ...}:
    let signature = toNixSignature x;
    in _.do
      (whileV 2 {_ = "Converting to Nix value: ${signature} (${lib.typeOf x})\n  ${_p_ x}";})
      ({_, ...}: switch signature {
        EvalError = _.liftEither x;
        Eval = throw "toNixM: Eval value ${_ph_ x} cannot be converted to Nix";
        AST = _.do
          {value = forceEvalNodeM x;}
          ({value, _}: _.bind (toNixM value));
        Thunk = _.do
          {forced = force x;}
          ({forced, _}: _.bind (toNixM forced));
        EvaluatedLambda = _.pure x.asLambda;
        BinOp = _.do
          {result = x.run;}
          ({result, _}: _.bind (toNixM result));
        Functor = _.do
          {f = toNixM x.__functor;}
          ({f, _}: _.pure (x // { __functor = self: arg: f self arg; }));
        #Function = _.pure (arg: toNix (x arg));
        Function = _.pure x;
        List = _.traverse toNixM x;
        Attrs = _.traverseAttrs (_: toNixM) x;
        Value = _.pure x;
      });

  toNixLazy = x:
    if isEvalError x then throw (_p_ x)
    else if isEvaluatedLambda x then x.asLambda
    else if isBinOp x then toNix x # TODO: Lazy enough?
    else if builtins.isFunction x then arg: toNixLazy (x arg)
    else if x ? __functor then x // { __functor = toNixLazy x.__functor; }
    else x;

  # TODO: Need to strictly convert deeply nested functions to not return thunks
  toNixLazyM = x: {_, ...}:
    let signature = toNixSignature x;
    in _.do
      (whileV 2 {_ = "Converting to lazy Nix value: ${signature} (${lib.typeOf x})\n  ${_p_ x}";})
      ({_, ...}: switch signature {
        EvalError = _.liftEither x;
        Eval = _.do
          {value = x;}
          ({value, _}: _.bind (toNixLazyM value));
        AST = _.do
          {value = evalNodeM x;}
          ({value, _}: _.bind (toNixLazyM value));
        Thunk = _.do
          {forced = force x;}
          ({forced, _}: _.bind (toNixLazyM forced));
        EvaluatedLambda = _.pure x.asLambda;
        BinOp = _.do
          {result = x.run;}
          ({result, _}: _.bind (toNixLazyM result));
        Functor = _.do
          {f = toNixLazyM x.__functor;}
          ({f, _}: _.pure (x // { __functor = f; }));
        #Function = pure (arg: toNixLazy (x arg));
        Function = _.pure x;
        List = _.pure x;
        Attrs = _.pure x;
        Value = _.pure x;
      });

  /* Main monadic eval entrypoint.
  Evaluates a node down to its WHNF value.
  evalNodeM :: AST -> Eval a */
  evalNodeM = node:
    with (log.v 3).call "evalNodeM" (toString node) ___;
    ({_, ...}: _.do
      (guard (is AST node || isThunk node || isEvaluatedLambda node || is EvalError node || isBinOp node) (RuntimeError ''
        evalNodeM: Expected AST node, Thunk, EvaluatedLambda, or EvalError, got ${lib.typeOf node}
      ''))
      ({_, ...}: 
        if is EvalError node then _.do
          (whileV 3 {_ = "lifting an EvalError";})
          (liftEither node)
        else if isThunk node then _.do
          (whileV 3 {_ = "not evaluating a Thunk in evalNodeM";})
          (pure node)
        else if isEvaluatedLambda node then _.do
          (whileV 3 {_ = "not evaluating an EvaluatedLambda in evalNodeM";})
          (pure node)
        else if isBinOp node then _.do
          #if node.op == "." then _.do
          #  (whileV 3 {_ = "not evaluating a '.' BinOp in evalNodeM";})
          #  (pure node)
          #else _.do
            (whileV 3 {_ = "running a BinOp in evalNodeM";})
            (node.run)
        else _.do
          (whileV 1 {_ = "evaluating AST node:\n\n${toString node}";})
          (pushStack node)
          {res = switch node.nodeType {
            int = evalLiteral node;
            float = evalLiteral node;
            string = evalLiteral node;
            stringPieces = evalStringPieces node;
            interpolation = evalInterpolation node;
            path = evalPath node;
            anglePath = evalAnglePath node;
            list = evalList node;
            attrs = evalAttrs node;
            attrPath = evalAttrPath node;
            identifier = evalIdentifier node;
            binaryOp = evalBinaryOp node;
            unaryOp = evalUnaryOp node;
            conditional = evalConditional node;
            lambda = evalLambda node;
            application = evalApplication node;
            letIn = evalLetIn node;
            assignment = evalAssignment node;
            "with" = evalWith node;
            "assert" = evalAssert node;
            "abort" = evalAbort node;
            "throw" = evalThrow node;
            "import" = evalImport node;
            "inherit" = evalInherit node;
          };}
          (popStack)
          ({_, res}: _.do
            (whileV 1 {_ = _b_ ''
              returning evaluation result for AST node:
              
              ${node}
              -> 
              ${_p_ res}
            '';})
            (pure (return res)))));

  deeplyEvalNodeM = node: {_, ...}:
    _.do
      (whileV 2 {_ = "deeply evaluating AST node: ${printBoxed node}";})
      (forceDeeplyM (evalNodeM node));

  forceEvalNodeM = node: {_, ...}:
    _.do
      (whileV 2 {_ = "evaluating AST node to WHNF: ${printBoxed node}";})
      {result = forceM (evalNodeM node);}
      ({result, _}: if isBinOp result then _.bind result.run else _.pure result);

  forceM = m: {_, ...}:
    _.do
      (whileV 5 {_ = "forcing a monadic value";})
      {unforced = m;}
      ({_, unforced}: _.bind (force unforced));

  forceDeeplyM = m: {_, ...}:
    _.do
      (whileV 2 {_ = "deeply forcing a monadic value";})
      {unforced = m;}
      ({_, unforced}: _.bind (forceDeeply unforced));

  forceDeeply = x: {_, ...}: _.do
    (while {_ = "forcing deeply";})
    ({_, ...}:
      if isEvaluatedLambda x then _.do
        (while {_ = "forcing deeply: returning an EvaluatedLambda";})
        (pure x)
      else if isBinOp x then _.do
        (while {_ = "forcing deeply: running a BinOp";})
        (x.run)
      else if isEvalError x then _.do
        (while {_ = "forcing deeply: returning an EvalError";})
        (pure x)
      else if isAST x then _.do
        (while {_ = "forcing deeply: returning an AST node";})
        (pure x)
      else if isThunk x then _.do
        (while {_ = "forcing deeply: forcing a Thunk";})
        {forced = force x;}
        ({forced, _}: _.do
          (while {_ = "forcing deeply: descending into thunk contents (${getT forced}))";})
          (forceDeeply forced))
      else if isList x then _.do
        (while {_ = "forcing deeply: descending into a list";})
        (traverse forceDeeply x)
      else if isAttrs x then _.do
        (while {_ = "forcing deeply: descending into an attrset";})
        (traverseAttrs (_: forceDeeply) x)
      else _.do
        (while {_ = "forcing deeply: returning a non-monadic value of type ${lib.typeOf x}";})
        (pure x));

  # Evaluate a literal value (int, float, string, etc.)
  # evalLiteral :: AST -> Eval a
  evalLiteral = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'literal' node";})
      (pure node.value);

  # Evaluate a name (identifier or string)
  # If identifier, get the name itself, not evaluate it.
  # This can then be used on LHS of assignments.
  # Could also be a string or an interpolation.
  # Always used in places where we need to reference the name, so must
  # force its computation at least to WHNF.
  identifierName = node: {_, ...}:
    _.do
      (whileV 4 {_ = "evaluating a name";})
      {name = {_, ...}:
        if isString node then _.do
          (while {_ = "evaluating a pure string name '${node}'";})
          (pure node)
        else if node.nodeType == "identifier" then _.do
          (while {_ = "evaluating identifier name '${node.name}'";})
          (pure node.name)
        else _.do
          (while {_ = "evaluating a name from a '${node.nodeType}' node";})
          (forceEvalNodeM node);}
      ({_, name}: _.do
        (guard (lib.isString name) (RuntimeError ''
          Expected string identifier name, got ${lib.typeOf name}
        ''))
        (pure name));

  # Evaluate an identifier lookup
  # evalIdentifier :: Scope -> AST -> Eval a
  evalIdentifier = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'identifier' node";})
      {scope = getScope;}
      ({_, scope}: _.do
        (guard (hasAttr node.name scope || hasAttr node.name scope.__internal__.withScope) (UnknownIdentifierError ''
          Undefined identifier '${node.name}' in current scope:
          ${_pd_ 1 scope}
        ''))
        (if scope ? ${node.name}
         then evalScopeValue appendScope node.name scope.${node.name}
         else evalScopeValue appendWithScope node.name scope.__internal__.withScope.${node.name}));

  # Evaluate a value retrieved from the scope.
  # Handles:
  # - AST Nodes: lazily stored scope-free AST nodes to be evaluated in the current scope
  # - Thunk Nodes: stored by any non-strict recursive context i.e. let bindings, rec-attrs (temporarily), with blocks
  # - Monadic values: any monadic action, bound to current scope
  # - Standard Nix values
  evalScopeValue = appendScopeF: name: value: {_, ...}:
    _.do
      (while {_ = "evaluating value from scope";})
      ({_, ...}: 
        if isThunk value then _.do
          (while {_ = "evaluating Thunk from scope";})
          # Ensure the forced value replaces the thunk for all future computations
          # TODO: Cleaner with a separate thunk store
          {forced = force value;}
          ({_, forced}: _.do
            (appendScopeF { ${name} = forced; })
            (pure forced))

        else if isAST value then _.do
          (while {_ = "evaluating AST node from scope";})
          # Evaluate once to WHNF, capturing the state but not deeply evaluating
          {evaluated = evalNodeM value;}
          ({_, evaluated}: _.do
            (appendScopeF { ${name} = evaluated; })
            (pure evaluated))

          # Any monadic state should be bound to
          else if isMonadOf Eval value then _.do
            (while {_ = "evaluating Eval value from scope";})
            ({_, ...}: _.bind value)

          # Or just return strict values from the store.
          else _.do
            (while {_ = "evaluating ${lib.typeOf value} from scope";})
            (pure value));

  # Evaluate a list of AST nodes
  # evalList :: AST -> Eval [a]
  evalList = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'list' node";})
      (traverse Thunk node.elements);

  # Evaluate an assignment (name-value pair)
  # evalAssignment :: AST -> Eval [{name, value}]  
  evalAssignment = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'assignment' node";})
      {name = identifierName node.lhs;}
      {value = Thunk node.rhs;}
      ({_, name, value}: _.pure [{ inherit name value; }]);

  # Evaluate an inherit expression, possibly with a source
  # evalInherit :: AST -> Eval [{name = value}]
  evalInherit = node: {_, ...}:
    _.do
      (whileV 5 {_ = "evaluating 'inherit' node";})
      (traverse
        (identifierOrString: {_, ...}: 
          # Homogenize to an identifier node.
          let identifier = if isAST identifierOrString then identifierOrString else N.identifier identifierOrString;
          in _.do
            (guard (isString identifierOrString || identifierOrString.nodeType == "identifier") (TypeError ''
              Expected string or identifier in inherit RHS, got ${lib.typeOf identifierOrString}
            ''))
            {name = identifierName identifier;}
            ({name, _, ...}:
              if node.from == null
              then _.do
                # Ensure we return a thunk for consistency
                # even though this identifier cannot syntactically come from the same
                # set since it would be a duplicate name.
                {value = Thunk identifier;}
                ({value, _}: _.pure { inherit name value; })
              else _.do
                # Desugar the access to a binary 'from.name' operation
                # which we can thunk with the current scope, and which will evaluate with
                # other rec attrs in scope.
                # This creates a thunk that will resolve to a BinOp, so its scope can be addBefore'd
                # before the BinOp is created
                {value = Thunk (N.binaryOp node.from "." (N.attrPath [identifier]));}
                ({value, _}: _.pure { inherit name value; })))
          node.attrs);

  # Evaluate a list of inheritance or assignment expressions
  # evalBindingList :: AST -> Eval set
  evalBindingList = bindings: {_, ...}:
    _.do
      (while {_ = "evaluating 'bindings' node-list";})
      # Evaluate bindings down to a [[{name = Thunk value}]]
      {attrsLists = traverse evalNodeM bindings;}
      # Merge bindings down to a [{name = Thunk value}]
      ({attrsLists, _, ...}:
        let attrsList = concatLists attrsLists;
        in _.do
          (guard (all (attr: (hasAttr "name" attr) && (hasAttr "value" attr)) attrsList) (RuntimeError ''
            Recursive binding list evaluation produced invalid name/value pairs:
              bindings: ${_ph_ bindings}
              attrsList: ${_ph_ attrsList}
          ''))
          (guard (all isThunk (map (x: x.value) attrsList)) (RuntimeError ''
            Recursive binding list evaluation produced non-thunk values:
              bindings: ${_ph_ bindings}
              attrsList: ${_ph_ attrsList}
            ''))
          # Finally return in {name = Thunk value} format.
          (pure (listToAttrs attrsList)));

  # Evaluate an attribute set
  # evalAttrs :: AST -> Eval AttrSet
  evalAttrs = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'attrs' node";})
      (if node.isRec 
      then evalRecBindingList node.bindings
      else evalBindingList node.bindings);

  # Evaluate a list of recursive bindings where each can access all others.
  evalRecBindingList = bindings: {_, ...}:
    _.do
      (while {_ = "evaluating recursive binding list";})
      {attrs = evalBindingList bindings;}
      ({attrs, _}:
        _.traverseAttrs
          (name: thunk: {_, ...}: _.bind (thunk.unsafeAddBefore ({_, ...}: _.do
            (while {_ = "resolving a Thunk member of a recursive binding list";})
            (appendScope attrs))))
          attrs);

  guardOneBinaryOp = op: compatibleTypeSets: l: r: {_, ...}:
    _.do
      (whileV 3 {_ = "checking compatible types to ${op} op: ${_l_ compatibleTypeSets}";})
      (guard 
        (any id 
          (map 
            (typeSet: elem (lib.typeOf l) typeSet && elem (lib.typeOf r) typeSet)
            compatibleTypeSets))
        (TypeError ''Incorrect types for binary operator ${op}:

          ${_ph_ l} and ${_ph_ r}
          
          (expected one of ${_l_ compatibleTypeSets})
        ''));

  guardBinaryOp = l: op: r: {_, ...}:
    _.do
      (while {_ = "guarding argument types to ${op} op";})
      (guard (elem (sig l) ["Value" "List" "Attrs" "Functor" "Function"]) (TypeError ''
        ${op}: got non-value left operand of type ${sig l}:
          ${_ph_ l}
      ''))
      (guard (elem (sig r) ["Value" "List" "Attrs" "Functor" "Function"]) (TypeError ''
        ${op}: got non-value right operand of type ${sig r}:
          ${_ph_ r}
        ''))
      (switch op {
        "+" = guardOneBinaryOp "+" [["int" "float"] ["string" "path"]] l r;
        "-" = guardOneBinaryOp "-" [["int" "float"]] l r;
        "*" = guardOneBinaryOp "*" [["int" "float"]] l r;
        "/" = guardOneBinaryOp "/" [["int" "float"]] l r;
        "++" = guardOneBinaryOp "++" [["list"]] l r;
        "//" = guardOneBinaryOp "//" [["set"]] l r;
        "==" = guardOneBinaryOp "==" [builtinNames] l r;
        "!=" = guardOneBinaryOp "!=" [builtinNames] l r;
        "<" = guardOneBinaryOp "<" [["int" "float"] ["string"] ["path"] ["list"]] l r;
        ">" = guardOneBinaryOp ">" [["int" "float"] ["string"] ["path"] ["list"]] l r;
        "<=" = guardOneBinaryOp "<=" [["int" "float"] ["string"] ["path"] ["list"]] l r;
        ">=" = guardOneBinaryOp ">=" [["int" "float"] ["string"] ["path"] ["list"]] l r;
      });

  runBinaryOp = l: op: r: {_, ...}:
    _.do
      (while {_ = "running binary operation ${op}";})
      (guardBinaryOp l op r)
      ({_, ...}: _.pure (switch op {                                                                                                                                                                                           
        "+" = l + r;                                                                                                                                                                                                      
        "-" = l - r;                                                                                                                                                                                                      
        "*" = l * r;                                                                                                                                                                                                      
        "/" = l / r;                                                                                                                                                                                                      
        "++" = l ++ r;                                                                                                                                                                                                    
        "//" = l // r;                                                                                                                                                                                                    
        "==" = l == r;                                                                                                                                                                                                    
        "!=" = l != r;                                                                                                                                                                                                    
        "<" = l < r;
        ">" = l > r;
        "<=" = l <= r;
        ">=" = l >= r;
      }));

  runBinaryOpThunks = l: op: r: {_, ...}:
    _.do
      (while {_ = "running binary operation ${op} with thunks";})
      {binOp = BinOp' l op r;}
      ({binOp, _}: _.bind (binOp.run));

  runBinaryOpListwise = ls: op: rs: {_, ...}:
    let 
      lSnoc = maybeSnoc ls;
      rSnoc = maybeSnoc rs;
    in _.do
      (whileV 4 {_ = "running listwise binary operation ${op} (sizes ${toString (size ls)}, ${toString (size rs)})";})
      ( if lSnoc == null && rSnoc == null then switch op {
          "==" = pure true;
          "!=" = pure false;
          "<" = pure false;
          ">" = pure false;
          "<=" = pure true;
          ">=" = pure true;
        }
        else if lSnoc == null then switch op {
          "==" = pure false;
          "!=" = pure true;
          "<" = pure true;
          ">" = pure false;
          "<=" = pure true;
          ">=" = pure false;
        }
        else if rSnoc == null then switch op {
          "==" = pure false;
          "!=" = pure true;
          "<" = pure false;
          ">" = pure true;
          "<=" = pure false;
          ">=" = pure true;
        }
        else {_, ...}: _.do
          (whileV 4 {_ = "recursing listwise binary operation ${op} (${sig lSnoc.head} ${op} ${sig rSnoc.head})";})
          {eq = runBinaryOpThunks lSnoc.head "==" rSnoc.head;}
          ({eq, _}: _.do
            (if op == "==" && !eq
              then pure false
            else if op == "!=" && eq
              then pure false
            else if (op == "==" && eq) || (op == "!=" && !eq) || (op != "==" && op != "!=" && eq)
              # Can't decide based on this element, so recurse into tail.
              then runBinaryOpListwise lSnoc.tail op rSnoc.tail
            else 
              # For >, >=, <, <=, we just need to compare the first unequal entry.
              runBinaryOpThunks lSnoc.head op rSnoc.head)));

  # Only needs to support "==" and "!="; compares lists of sorted [name value].
  runBinaryOpSetwise = ls: op: rs: {_, ...}:
    _.do
      (whileV 4 {_ = "recursing setwise binary operation (${sig ls} ${op} ${sig rs})";})
      ({_, ...}:
        let
          sizeLs = size ls;
          sizeRs = size rs;
          sortedLs = sorted (attrNames ls);
          sortedRs = sorted (attrNames rs);
        in
        if (sizeLs != sizeRs) then switch op {
          "==" = _.do
            (whileV 4 {_ = "unequal set sizes in == check on sets: ${sizeLs} != ${sizeRs}";})
            (pure false);
          "!=" = _.do
            (whileV 4 {_ = "unequal set sizes in != check on sets: ${sizeLs} != ${sizeRs}";})
            (pure true);
        }
        else 
          if sortedLs != sortedRs then switch op {
          "==" = _.do
            (whileV 4 {_ = "unequal set keys in == check on sets ${_p_ sortedLs} != ${_p_ sortedRs}";})
            (pure false);
          "!=" = _.do
            (whileV 4 {_ = "unequal set keys in != check on sets ${_p_ sortedLs} != ${_p_ sortedRs}";})
            (pure true);
        }
        else _.do
          (whileV 4 {_ = "converting setwise binary operation ${op} to listwise";})
          (runBinaryOpListwise (sortedAttrValues ls) op (sortedAttrValues rs)));

  knownBinaryOps = [
    "+"
    "-"
    "*"
    "/"
    "++"
    "//"
    "=="
    "!="
    "<"
    ">"
    "<="
    ">="
    "&&"
    "||"
    "."
    "or"
  ];

  listwiseBinaryOps = [
    "<"
    ">"
    "<="
    ">="
    "=="
    "!="
  ];

  setwiseBinaryOps = [
    "=="
    "!="
  ];

  evalBooleanAndOperation = l: rhsThunk: {_, ...}:
    if !l then _.pure false
    else _.do
      (while {_ = "evaluating && RHS";})
      {r = forceEvalNodeM rhsThunk;}
      ({r, _}: _.do
        (guard (lib.isBool r) (TypeError (_b_ ''
          &&: got non-bool right operand of type ${typeOf r}:
            ${_ph_ r}
        '')))
        (pure r));

  evalBooleanOrOperation = l: rhsThunk: {_, ...}:
    if l then _.pure true
    else _.do
      (while {_ = "evaluating || RHS";})
      {r = forceEvalNodeM rhsThunk;}
      ({r, _}: _.do
        (guard (lib.isBool r) (TypeError (_b_ ''
          ||: got non-bool right operand of type ${typeOf r}:
            ${_ph_ r}
        '')))
        (pure r));

  evalBinaryOp = node: {_, ...}: _.do
    (while {_ = "evaluating 'binaryOp' node";})
    (BinOp node);

  BinOp = node: {_, ...}: _.do
    (while {_ = "constructing '${node.op}' BinOp";})
    {lhs = Thunk node.lhs;}
    {rhs = Thunk node.rhs;}
    ({lhs, rhs, _}: _.bind (BinOp' lhs node.op rhs));

  # TODO: Needs string coercion for full compatibitility i.e. {__toString=self: "a";}+"b" == "ab"
  BinOp' = lhs: op: rhs: {_, ...}: _.do
    (while {_ = "constructing '${op}' BinOp' from thunks";})
    (guard (elem op knownBinaryOps) (RuntimeError ''
      Unsupported binary operator: ${op}
    ''))
    (pure (fix (self: {
      __isBinOp = true;
      getLHS = {}: lhs;
      getRHS = {}: rhs;
      getOp = {}: op;
      catchable = false;
      __toString = self: "BinOp<${lhs.nodeType} ${op} ${rhs.nodeType}>";

      run = {_, ...}: _.do
        (while {_ = "running '${op}' BinOp";})
        (unless (elem op ["==" "!="]) ({_, ...}: _.do
          (guard (!isEvaluatedLambda lhs) (TypeError ''
            Cannot run binary operation '${op}' on lambda LHS
          ''))
          (guard (!isEvaluatedLambda rhs) (TypeError ''
            Cannot run binary operation '${op}' on lambda RHS
          ''))))
        ({_, ...}:
          # No op should end up being run with lambda arguments
          if op == "==" && lhs.nodeType == "lambda" || rhs.nodeType == "lambda" then _.pure false
          else if op == "!=" && lhs.nodeType == "lambda" || rhs.nodeType == "lambda" then _.pure true

          # Because or takes precedence, this can only ever be a raw access without or, so this
          # shouldn't be catchable.
          else if op == "." then _.bind (evalAttributeAccess self)

          # Only a catchable error returned directly from the attribute access chain can be caught
          else if op == "or" then _.do
            (while {_ = "evaluating 'or' BinOp";})
            {binOp = forceWith evalNodeM lhs;}
            ({binOp, _, ...}: 
              (_.bind (evalAttributeAccess (binOp // { catchable = true; }))
              ).catch (e: {_, ...}: _.do
                (while {_ = "Handling missing attribute in 'or' node";})
                (guard (CatchableMissingAttributeError.check e) e)
                (pure rhs)))

          else if op == "&&" || op == "||" then _.do
            (while {_ = "evaluating possibly short-circuiting boolean binary operation";})
            {l = forceEvalNodeM lhs;}
            ({_, l, ...}: _.do
              (guard (lib.isBool l) (TypeError (_b_ ''
                &&: got non-bool left operand of type ${typeOf l}:
                  ${_ph_ l}
              '')))
              ( if op == "&&" then evalBooleanAndOperation l rhs
                else if op == "||" then evalBooleanOrOperation l rhs
                else throws (RuntimeError ''
                  Invalid boolean binary operation: ${op}
                '')))

          # All other binary operators without short-circuiting.
          else _.do
            (while {_ = "evaluating binary operation '${op}'";})
            {l = forceEvalNodeM lhs;}
            {r = forceEvalNodeM rhs;}
            ({l, r, _}: _.do
              (guardBinaryOp l op r)
              ( if sig l == "List" && sig r == "List" && elem op listwiseBinaryOps
                then runBinaryOpListwise l op r
                else if sig l == "Attrs" && sig r == "Attrs" && elem op setwiseBinaryOps
                then runBinaryOpSetwise l op r
                else runBinaryOp l op r )));
    })));

  isBinOp = x: x ? __isBinOp;

  guardAttrsForAccess = attrs:
    guard (lib.isAttrs attrs) (TypeError ''
      Cannot select attribute from non-attrset of type ${lib.typeOf attrs}:
        ${_ph_ attrs}
    '');

  # obj.a.b.c - traverse the attribute path
  # traversePath :: bool -> (AST | Thunk) -> Node -> Eval a
  traversePath = catchable: attrs_: path: {_, ...}:
    _.do
      {attrs = force attrs_;}
      {pathSnoc = pure (maybeSnoc path);}
      ({attrs, pathSnoc, _}:
        # Empty path implies the end of a chain (may not be attrs, so don't guard yet)
        if pathSnoc == null then _.pure attrs
        else _.do
          (guardAttrsForAccess attrs)
          {k = identifierName pathSnoc.head;}
          ({k, _}: _.do
            # The CatchableMissingAttributeError will propagate along a chain which the "or" operator
            # can catch. If it terminates without being caught, 
            (guard 
              (hasAttr k attrs)
              (if catchable 
               then CatchableMissingAttributeError k
               else MissingAttributeError k))
            (traversePath catchable attrs.${k} pathSnoc.tail)));

  # Evaluate an attrPath in the source code down to its list of components.
  # evalAttrPath = AST -> Eval [AST]
  evalAttrPath = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'attrPath' node";})
      (pure (node.path));

  # Evaluate attribute access (dot operator)
  # Raises a CatchableMissingAttributeError if the attribute is missing which can be caught
  # by the 'or' operator.
  # evalAttributeAccess :: BinOp -> Eval a
  evalAttributeAccess = binOp: {_, ...}:
    _.do
      (while {_ = "evaluating 'attribute access' node";})
      (guard (isBinOp binOp && (binOp.getOp {}) == ".") (TypeError ''
        Expected attribute access BinOp; got '${getT binOp}'
      ''))
      {attrs = forceEvalNodeM (binOp.getLHS {});}
      {path = forceEvalNodeM (binOp.getRHS {});}
      ({attrs, path, _}: _.bind (traversePath binOp.catchable attrs path));

  # evalUnaryOp :: AST -> Eval a
  evalUnaryOp = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'unary' node";})
      {operand = forceEvalNodeM node.operand;}
      ({_, operand}: _.pure (switch node.op {
        "!" = (!operand);
        "-" = (-operand);
      }));

  # evalConditional :: AST -> Eval a
  evalConditional = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'conditional' node";})
      {cond = forceEvalNodeM node.cond;}
      ({_, cond}: _.do
        (guard (lib.isBool cond) (TypeError ''
          if: got non-bool condition (type ${lib.typeOf cond}, sig ${sig cond})
        ''))
        (if cond
         then Thunk node."then"
         else Thunk node."else"));

  # Get the name of a parameter from its AST node.
  # param.name is an identifier.
  getParamName = param: param.name.name;

  # Return any extra scope bound by passing in the arg to the param.
  # Receives the defining scope for the evaluation of any default values that depend upon other scope.
  # addLambdaParamsToScope :: a -> AST -> Eval Scope
  addLambdaParamsToScope = arg: param: {_, ...}:
    _.do
      (while {_ = "evaluating 'lambda' parameters";})
      ({_, ...}: switch param.nodeType {
        # Simple param is just a name, so just bind it to the arg with the calling scope _.
        simpleParam = _.do
          {newScope = pure { ${getParamName param} = arg; };}
          ({newScope, _}: _.do
            (while {_ = "adding new scope from lambda params:\n${_p_ newScope}";})
            (appendScope newScope)
            (pure newScope));

        # Attrset param is a set of names, so bind each to the arg, with defaults handled.
        attrSetParam =
          let
            required = requiredParamAttrs param;
            default = defaultParamAttrs param;
            requiredPartitioned = partition (p: hasAttr (getParamName p) arg) required;
            requiredSupplied = requiredPartitioned.right;
            requiredUnsupplied = requiredPartitioned.wrong;
            defaultPartitioned = partition (p: hasAttr (getParamName p) arg) default;
            defaultSupplied = defaultPartitioned.right;
            defaultUnsupplied = defaultPartitioned.wrong;
            suppliedUnknown = removeAttrs arg (map getParamName param.attrs);
          in _.do
            (guard (lib.isAttrs arg) (TypeError ''
              Expected attrset argument, got ${lib.typeOf arg}:
                ${_ph_ arg}
            ''))
            (guard (empty requiredUnsupplied) (RuntimeError ''
              Missing required parameters in attrset lambda: ${joinSep ", " requiredUnsupplied}:
                ${_ph_ param}
            ''))
            (guard (param.ellipsis || empty suppliedUnknown) (RuntimeError ''
              Unknown parameters in non-ellipsis attrset lambda: ${joinSep ", " (attrNames suppliedUnknown)}:
                ${_ph_ param}
            ''))
            # First add the concrete supplied values to the scope so that default resolution sees them.
            (while {_ = "adding new scope from explicitly supplied lambda params:\n${_p_ arg}";})
            (appendScope arg)
            # Then evaluate a recursive binding list of only those default values that are not already supplied.
            # Reuses the rec binding mechanism by creating shim AST nodes. These will now be evaluated with the
            # supplied values in scope and so mutual recursion should work.
            {defaults =
              let bindings = forAttrsToList defaultUnsupplied (k: v: N.assignment (N.identifier k) v);
              in evalRecBindingList bindings;}
            # Add to scope and return the merged scope as an attrset.
            ({defaults, _}: _.do
              (while {_ = "adding new scope from default lambda params:\n${_p_ defaults}";})
              (appendScope defaults)
              (pure (arg // defaults)));
      });

  defaultParamAttrs = param: 
    filter (paramAttr: paramAttr.nodeType == "defaultParam") param.attrs;
  requiredParamAttrs = param: 
    filter (paramAttr: paramAttr.nodeType != "defaultParam") param.attrs;

  guardAttrSetParam = arg: param: {_, ...}:
    let
      requiredPartitioned = partition (p: hasAttr (getParamName p) arg) (requiredParamAttrs param);
      requiredSupplied = requiredPartitioned.right;
      requiredUnsupplied = requiredPartitioned.wrong;
      defaultPartitioned = partition (p: hasAttr (getParamName p) arg) (defaultParamAttrs param);
      defaultSupplied = defaultPartitioned.right;
      defaultUnsupplied = defaultPartitioned.wrong;
      suppliedUnknown = removeAttrs arg (map getParamName param.attrs);
    in _.do
      (guard (lib.isAttrs arg) (TypeError ''
        Expected attrset argument, got ${lib.typeOf arg}:
          ${_ph_ arg}
      ''))
      (guard (empty requiredUnsupplied) (RuntimeError ''
        Missing required parameters in attrset lambda: ${joinSep ", " requiredUnsupplied}:
          ${_ph_ param}
      ''))
      (guard (param.ellipsis || empty suppliedUnknown) (RuntimeError ''
        Unknown parameters in non-ellipsis attrset lambda: ${joinSep ", " (attrNames suppliedUnknown)}:
          ${_ph_ param}
      ''));

  # Carrier for lambdas created entirely inside the Eval monad, as opposed to e.g.
  # lambdas created by the use of the builtins embedded in the scope.
  # In these cases we can continue evaluating inside the Eval monad, lazily
  # binding the parameters to the arguments passed in and evaluating the body
  # inside the monad.
  #
  # For feature parity with Nix runtime typechecking/functor calling, if these are passed
  # in to native code (imports, builtins, lib, etc) we convert them to regular Nix lambdas first.
  #
  # We get deep conversion via the use of run_:
  # e.g. if we have Eval (a: b: a + b) then asLambda gives native a: (b: a + b).run ==
  # a: (b: a + b) (the latter EL is converted via asLambda in run_) so this maybe be okay.
  isEvaluatedLambda = x: x ? __isEvaluatedLambda;

  evalLambdaParam = param: arg:
    switch param.nodeType {
      simpleParam = evalSimpleParam param arg;
      attrSetParam = evalAttrSetParam param arg;
    };

  # evalSimpleParam :: AST -> Thunk -> Eval Unit
  evalSimpleParam = param: argThunk: {_, ...}:
    _.do
      (while {_ = "evaluating 'simpleParam' node";})
      (appendScope { ${getParamName param} = argThunk; });

  # evalAttrSetParam :: AST -> Thunk -> Eval Unit
  evalAttrSetParam = param: argThunk: {_, ...}:
    _.do
      (while {_ = "evaluating 'attrSetParam' node";})
      {arg = force argThunk;}
      ({arg, _}: _.do
        (guardAttrSetParam arg param)
        # Add arg to scope so default Thunks contain it
        (appendScope arg)
        # Add ASTs to the scope to break mutual recursion
        (traverse
          (p: {_, ...}:
            let name = getParamName p;
            in _.when (!(arg ? ${name})) (appendScope { ${name} = p.default; }))
          (defaultParamAttrs param))
        # Thunkify the defaults for the return value
        (traverse
          (p: {_, ...}:
            let name = getParamName p;
            in _.when (!(arg ? ${name})) ({_, ...}: _.do
              {thunk = Thunk p.default;}
              ({thunk, _}: _.appendScope { ${name} = thunk; })))
          (defaultParamAttrs param)));

  # Evaluate a lambda expression down to a Thunk.
  # The only time this needs forcing is during application, where we evaluate
  # the argument in the calling context, and any default arguments in the thunk's context.
  # evalLambda :: AST -> Eval EvaluatedLambda
  evalLambda = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'lambda' node without argument to EvaluatedLambda";})
      ({_}: _.do
        # Take a thunk of the body which will have arguments bound via addBefore
        # upon application.
        {body = Thunk node.body;}
        ({body, _}: _.pure (lib.fix (self: {
          __isEvaluatedLambda = true;
          __toString = self: "EvaluatedLambda<${node.param}: ...>";
          asLambda = arg: toNix (Eval.do (self.applyM arg));
          applyM = argThunk: {_, ...}: _.do
            (while {_ = "applying lambda";})
            # Thunkify the argument in the context of the caller.
            # Then return a new body thunk with the argument in scope.
            # This will only be forced if the lambda accepts attrset parameters.
            (body.forkWithBefore (evalLambdaParam node.param argThunk));
        }))));

  isApplicable = x: builtins.isFunction x || x ? __functor || isEvaluatedLambda x;

  guardApplicable = func: {_, ...}:
    _.guard (isApplicable func) (TypeError ''
      Attempted to apply non-applicable value of type ${lib.typeOf func}:
        ${_ph_ func}
    '');

  # Apply an already-evaluated function to two already-evaluated arguments.
  # apply1 :: (EvaluatedLambda | function) -> a -> b -> Eval c
  apply2 = func_: l: r: {_, ...}: _.do
    {func = forceEvalNodeM func_;}
    ({func, _}: _.do
      (guardApplicable func)
      {func' = apply1 func l;}
      ({func', _}: _.bind (apply1 func' r)));

  # Apply an already-evaluated function to an argument
  # apply1 :: (EvaluatedLambda | function) -> AST -> Eval b
  apply1 = func: argNode: {_, ...}:
    _.do
      (while {_ = "applying evaluated function to unevaluated argument";})
      {argThunk = Thunk argNode;}
      ({argThunk, _, ...}: _.do
        ( if isEvaluatedLambda func then func.applyM argThunk
          # First apply __functor self monadically, then again with the argument.
          else 
            if func ? __functor then (apply2 func.__functor func argThunk)
            # Otherwise, just apply the function to the argument.
            # This should only trigger for pure Nix functions i.e. builtins from the core language / lib
            else {_, ...}: _.do
              (while {_ = "applying function as a Nix lambda to a Nix value";})
              {nixArg = toNixM argThunk;}
              ({nixArg, _}: _.pure (func nixArg))));

  # Apply an unevaluated function to an unevaluated argument.
  # apply1Node :: AST -> AST -> Eval a
  apply1Node = func_: argNode: {_, ...}:
    _.do
      (while {_ = _b_ ''
        applying function to argument:
          (( ${_ph_ func_} )
           ( ${_ph_ argNode} ))
        '';
      })
      ({_, ...}: _.do
        {func = if isEvaluatedLambda func_ || builtins.isFunction func_                                                                                                                                                     
                then pure func_                                                                                                                                                                                             
                else forceEvalNodeM func_;}    
        ({func, _}: _.do
          (guardApplicable func)
          (apply1 func argNode)));

  # Evaluate function application.
  # evalApplication :: AST -> Eval a
  evalApplication = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'application' node";})
      (foldM apply1Node node.func node.args);

  # Evaluate a let expression
  # evalLetIn :: AST -> Eval a
  evalLetIn = node: {_, ...}: 
    _.do
      (while {_ = "evaluating 'letIn' node";})
      {letScope = evalRecBindingList node.bindings;}
      ({letScope, _}: _.saveScope ({_, ...}: _.do
        (appendScope letScope)
        (Thunk node.body)));

  # Evaluate a with expression
  # evalWith :: AST -> Eval a
  evalWith = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'with' node";})
      # Env as an attrset in WHNF
      {env = forceEvalNodeM node.env;}
      ({_, env}: _.saveScope ({_, ...}: _.do
        # A non-attrs with body is valid as long as we don't try to access any attributes
        # that are not in the scope.
        # e.g. with []; 1 == 1, let a = 1; in with []; a == 1
        # TODO: However `with {a=1;}; with []; a` is an error?
        (when (lib.isAttrs env) (appendWithScope env))
        (forceEvalNodeM node.body)));

  # Evaluate an assert expression
  # evalAssert :: AST -> Eval a
  evalAssert = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'assert' node";})
      {cond = forceEvalNodeM node.cond;}
      ({_, cond}: _.guard (lib.isBool cond) (TypeError ''
        assert: got non-bool condition of type ${typeOf cond}:
          ${_ph_ cond}
      ''))
      ({_, cond}: _.guard cond (AssertError ''
        assert: condition failed:
          ${_ph_ cond}
      ''))
      (evalNodeM node.body);

  # Evaluate an abort expression
  # evalAbort :: Scope -> AST -> Eval a
  evalAbort = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'abort' node";})
      {msg = evalNodeM node.msg;}
      ({_, msg}: _.guard (lib.isString msg) (TypeError ''
        abort: got non-string message of type ${typeOf msg}:
          ${_ph_ msg}
      ''))
      ({_, msg}: _.throws (Abort msg));

  # Evaluate a throw expression
  # evalThrow :: Scope -> AST -> Eval a
  evalThrow = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'throw' node";})
      {msg = evalNodeM node.msg; }
      ({_, msg}: _.do
        (guard (lib.isString msg) (TypeError ''
          throw: got non-string message of type ${typeOf msg}:
            ${_ph_ msg}
        ''))
        (throws (Throw msg)));

  # Evaluate an import expression
  # evalImport :: Scope -> AST -> Eval a
  evalImport = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'import' node";})
      {path = evalNodeM node.path;}
      ({_, path}: _.guard (lib.isString path || lib.isPath path) (TypeError ''
        import: got non-string or path message of type ${typeOf path}:
          ${_ph_ path}
      ''))
      (pure (import path));

  coercibleToString = x:
    lib.isString x
    || lib.isPath x
    || x ? __toString
    || x ? outPath;

  coerceToString = x_: {_, ...}: _.do
    ("while coercing to string")
    {x = force x_;}
    ({x, _}: 
      let e = TypeError ''
        Cannot coerce value of type ${lib.typeOf x} to string:
          ${_ph_ x}
      '';
      in _.do
        (guard (coercibleToString x) e)
        (switch.def.on lib.typeOf (throws e) x {
          string = pure x;
          path = pure (toString x);
          set = x:
            if x ? __toString then apply1 x.__toString x
            else if x ? __outPath then force x.__outPath
            else throws e;
        }));

  evalInterpolation = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'interpolation' node";})
      {value = forceEvalNodeM node.body;}
      ({value, _}: _.bind (coerceToString value));

  # TODO: This needs to operate as a closure
  evalStringPieces = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'stringPieces' node";})
      {pieces = traverse evalNodeM node.pieces;}
      ({_, pieces}:
        let s = join pieces;
        in if node.indented
           then _.pure (setIndent 0 s)
           else _.pure s);

  # TODO: Fix parser so that Path is just a wrapper round stringPieces to enable interpolation in the path
  evalPath = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'path' node";})
      {state = get;}
      ({state, _}: _.pure (stringToPath_ state.scope.PWD state.scope.HOME node.value));

  evalAnglePath = node: {_, ...}:
    _.do
      (while {_ = "evaluating 'anglePath' node";})
      {scope = getScope;}
      ({_, scope}:
        let path = splitSep "/" node.value;
            name = maybeHead path;
            rest = maybeTail path;
            restPath = joinSep "/" (def [] rest);
        in _.do
          ({_, ...}: _.guard (hasAttr "NIX_PATH" scope) (NixPathError ''
            No NIX_PATH found in scope when resolving ${node.value}.
          ''))
          ({_, ...}: _.guard (hasAttr name scope.NIX_PATH) (NixPathError ''
            ${name} not found in NIX_PATH when resolving ${node.value}.
          ''))
          (pure (scope.NIX_PATH.${name} + "/${restPath}")));

  # Helper to test round-trip property: eval (parse x) == x
  testRoundTripLazy = testRoundTripWith evalAST collective-lib.tests.expect.noLambdasEq;
  testRoundTrip = testRoundTripWith evalAST' collective-lib.tests.expect.noLambdasEq;
  testRoundTripWith = evalASTFn: expectation: expr: expected: {
    # Just test that parsing succeeds and the result evaluates to expected
    roundTrip = 
      let result = evalASTFn expr;
      in expectation result ((Either EvalError (getT expected)).Right expected);
  };

  testRoundTripBoth = expr: expectedLazy: expectedStrict: {
    lazy = testRoundTripLazy expr expectedLazy;
    strict = testRoundTrip expr expectedStrict;
  };
  testRoundTripSame = expr: expected: testRoundTripBoth expr expected expected;

  _tests = with tests; suite {

    evalAST = {
      _00_smoke = {
        _00_int = testRoundTripSame "1" 1;
        _01_float = testRoundTripSame "1.0" 1.0;
        _02_string = testRoundTripSame ''"hello"'' "hello";
        _03_indentString = testRoundTripSame "''hello''" "hello";
        _04_true = testRoundTripSame "true" true;
        _05_false = testRoundTripSame "false" false;
        _06_null = testRoundTripSame "null" null;
        _07_list.empty = testRoundTripSame "[]" [];
        _07_list.full = 
          testRoundTripBoth "[1 2 3]"
            [(CODE 0 "int") (CODE 1 "int") (CODE 2 "int")]
            [1 2 3];
        _08_attrSet.empty = testRoundTripSame "{}" {};
        _08_attrSet.full = testRoundTripBoth "{a = 1;}" {a = CODE 0 "int";} {a = 1;};
        _09_attrPath = testRoundTripSame "{a = 1;}.a" 1;
        _10_attrPathOr = testRoundTripSame "{a = 1;}.b or 2" 2;
        _11_inheritsConst = testRoundTripBoth "{ inherit ({a = 1;}) a; }" {a = CODE 0 "binaryOp";} {a = 1;};
        _12_recAttrSetNoRecursion = testRoundTripBoth "rec { a = 1; }" {a = CODE 0 "int";} {a = 1;};
        _13_recAttrSetRecursion._00_define =
          testRoundTripBoth "rec { a = 1; b = a; }"
            {a = CODE 0 "int"; b = CODE 1 "identifier";}
            {a = 1; b = 1;};
        _13_recAttrSetRecursion._01_access =
          testRoundTripSame "(rec { a = 1; b = a; }).b" 1;
        _13_recAttrSetRecursion._02_defineBackwards =
          testRoundTripBoth "rec { a = b; b = 1; }"
            {a = CODE 0 "identifier"; b = CODE 1 "int";}
            {a = 1; b = 1;};
        _13_recAttrSetRecursion._03_accessBackwards =
          testRoundTripSame "(rec { a = b; b = 1; }).a" 1;
        _14_recAttrSetNested =
          testRoundTripBoth "rec { a = 1; b = { c = a; }; }"
            { a = CODE 0 "int"; b = CODE 1 "attrs"; }
            {a = 1; b = {c = 1;};};
        _15_recAttrSetNestedRec =
         testRoundTripBoth "rec { a = 1; b = rec { c = a; }; }"
           { a = CODE 0 "int"; b = CODE 1 "attrs"; }
           {a = 1; b = {c = 1;};};
        _16_letIn = testRoundTripSame "let a = 1; in a" 1;
        _17_letInNested =
         testRoundTripBoth "let a = 1; in let b = a + 1; in [a b]"
           [(CODE 4 "identifier") (CODE 5 "identifier")]
           [1 2];
        _18_withs =
         testRoundTripSame "with {a = 1;}; a" 1;
        _19_withsNested =
         testRoundTripBoth "with {a = 1;}; with {b = a + 1;}; [a b]"
           [(CODE 2 "identifier") (CODE 3 "identifier")]
           [1 2];
        _20_lambda._00_define = testRoundTripSame "(a: a)" expect.anyLambda;
        _20_lambda._01_apply = testRoundTripSame "(a: a) 1" 1;
        _21_lambdaCurry = testRoundTripSame "(a: b: a + b) 1 2" 3;
        _22_lambdaClosure = testRoundTripSame "let a = 1; f = b: a + b; in let a = 100; in a + f 2" 103;
        _23_lambdaAttrs = testRoundTripSame "({a}: a) { a = 1; }" 1;
        #_24_lambdaDefaults = testRoundTripSame "({a ? 1, b ? 2}: a + b) {}" 3;
        #_25_lambdaRecDefaults.overrideBoth = testRoundTripSame "({a ? 1, b ? a + 1}: a + b) { a = 2; b = 3; }" 5;
        #_25_lambdaRecDefaults.overrideA = testRoundTripSame "({a ? 1, b ? a + 1}: a + b) { a = 2; }" 5;
        #_25_lambdaRecDefaults.overrideB = testRoundTripSame "({a ? 1, b ? a + 1}: a + b) { b = 3; }" 4;
        #_25_lambdaRecDefaults.overrideNone = testRoundTripSame "({a ? 1, b ? a + 1}: a + b) {}" 3;
      };

      _01_allFeatures =
        skip (
        let 
          expr = ''
            let f = { a ? 1, b, ... }:
                  let 
                    data = { 
                      aa = a;
                      inherit b;
                    }; 
                  in with data; aa + b; 
            in f {b = 4;}
          '';
        in testRoundTrip expr 5
        );

      # Basic literals
      _02_literals = {
        integers = {
          positive = testRoundTrip "42" 42;
          negative = testRoundTrip "-42" (-42);
          zero = testRoundTrip "0" 0;
          large = testRoundTrip "999999999" 999999999;
          veryLarge = testRoundTrip "9223372036854775807" 9223372036854775807;
        };
        floats = {
          positive = testRoundTrip "3.14" 3.14;
          negative = testRoundTrip "-3.14" (-3.14);
          zero = testRoundTrip "0.0" 0.0;
          scientific = testRoundTrip "1.23e10" 1.23e10;
          scientificNegative = testRoundTrip "1.23e-10" 1.23e-10;
          fractional = testRoundTrip ".5" 0.5;
          trailingZero = testRoundTrip "1.0" 1.0;
        };
        strings = {
          simple = testRoundTrip ''"hello"'' "hello";
          empty = testRoundTrip ''""'' "";
          withSpaces = testRoundTrip ''"hello world"'' "hello world";
          withEscapes = testRoundTrip ''"hello\nworld"'' "hello\nworld";
          withQuotes = testRoundTrip ''"say \"hello\""'' "say \"hello\"";
          withBackslash = testRoundTrip ''"path\\to\\file"'' "path\\to\\file";
          withTab = testRoundTrip ''"hello\tworld"'' "hello\tworld";
          unicode = testRoundTrip ''"λ hello 世界"'' "λ hello 世界";
          multiline = testRoundTrip "''hello\nworld''" "hello\nworld";
          indentedString = testRoundTrip "''  hello\n  world''" "hello\nworld";
          emptyIndented = testRoundTrip "''''" "";
          singleChar = testRoundTrip ''"a"'' "a";
          numbers = testRoundTrip ''"123"'' "123";
          specialChars = testRoundTrip ''"!@#$%^&*()"'' "!@#$%^&*()";
        };
        booleans = {
          true = testRoundTrip "true" true;
          false = testRoundTrip "false" false;
        };
        nullValue = testRoundTrip "null" null;
        paths = {
          relative = testRoundTrip "./test" /tmp/pwd/test;
          absolute = testRoundTrip "/tmp/test" /tmp/test;
          home = testRoundTrip "~/test" /tmp/home/test;
          nested = testRoundTrip "./a/b/c" /tmp/pwd/a/b/c;
        };
      };

      # Collections
      _03_lists = {
        nested._0 = testRoundTripBoth "[]" [] [];
        nested._1 = testRoundTripBoth "[1]" [(CODE 0 "int")] [1];
        nested._2 = testRoundTripBoth "[[1]]" [(CODE 0 "list")] [[1]];
        nested._3 = testRoundTripBoth "[[[1]]]" [(CODE 0 "list")] [[[1]]];
        alternatingTypes = testRoundTrip ''[1 "a" 2 "b"]'' [1 "a" 2 "b"];
        emptyStrings = testRoundTrip ''["" "hello" ""]'' ["" "hello" ""];
        expressions = testRoundTrip "[(1 + 1) (2 * 3)]" [2 6];
        largeList = testRoundTrip "[1 2 3 4 5 6 7 8 9 10]" [1 2 3 4 5 6 7 8 9 10];
        largeNumbers = testRoundTrip "[999999999 (-999999999)]" [999999999 (-999999999)];
        mixed = testRoundTrip ''[1 "hello" true]'' [1 "hello" true];
        mixedNested = testRoundTrip ''[1 [2 "three"] [true [null]]]'' [1 [2 "three"] [true [null]]];
        numbers = testRoundTrip "[1 2 3]" [1 2 3];
        single = testRoundTrip "[1]" [1];
        singletonNested = testRoundTrip "[[1]]" [[1]];
        whitespace = testRoundTrip "[ 1 2 3 ]" [1 2 3];
        withAttributes = testRoundTrip "[{a = 1;} {b = 2;}]" [{a = 1;} {b = 2;}];
        withBooleans = testRoundTrip "[true false true]" [true false true];
        withFloats = testRoundTrip "[1.1 2.2 3.3]" [1.1 2.2 3.3];
        withNulls = testRoundTrip "[1 null 3]" [1 null 3];
        withStrings = testRoundTrip ''["a" "b" "c"]'' ["a" "b" "c"];
      };

      _04_attrs = {
        empty = testRoundTrip "{}" {};
        simple = testRoundTrip "{ a = 1; b = 2; }" { a = 1; b = 2; };
        nested = testRoundTrip "{ x = { y = 42; }; }" { x = { y = 42; }; };
        recursive = {
          complex = testRoundTrip "rec { a = 1; b = a + 1; c = b + a; }" { a = 1; b = 2; c = 3; };
          mutualRecursion = testRoundTrip "rec { a = b + 1; b = 5; }" { a = 6; b = 5; };
          nested = testRoundTrip "rec { x = { y = z; }; z = 42; }" { x = { y = 42; }; z = 42; };
          selfReference = testRoundTrip "rec { a = { b = 1; c = a.b; }; }" {a = { b = 1; c = 1; };};
          #selfReferenceFn = testRoundTrip "(rec { f = x: if x == 0 then 1 else x * f (x - 1); }).f 3" 6;
          simple = testRoundTrip "rec { a = 1; b = a; }" { a = 1; b = 1; };
        };
        inheritance = {
          simple = testRoundTrip "let x = 1; in { inherit x; }" { x = 1; };
          multiple = testRoundTrip "let x = 1; y = 2; in { inherit x y; }" { x = 1; y = 2; };
          fromAttrSet = testRoundTrip "{ inherit ({a = 1; b = 2;}) a b; }" { a = 1; b = 2; };
          mixed = testRoundTrip "let x = 3; in { inherit x; y = 4; }" { x = 3; y = 4; };
          nested = testRoundTrip "let attrs = {a = 1;}; in { inherit (attrs) a; }" { a = 1; };
        };
        expressions = {
          arithmetic = testRoundTrip "{ a = 1 + 2; b = 3 * 4; }" { a = 3; b = 12; };
          conditionals = testRoundTrip "{ a = if true then 1 else 2; }" { a = 1; };
          lists = testRoundTrip "{ a = [1 2]; b = []; }" { a = [1 2]; b = []; };
        };
        specialNames = {
          keywords = testRoundTrip ''{ "if" = 1; "then" = 2; "else" = 3; }'' { "if" = 1; "then" = 2; "else" = 3; };
          numbers = testRoundTrip ''{ "123" = "numeric"; }'' { "123" = "numeric"; };
          spaces = testRoundTrip ''{ "hello world" = 1; }'' { "hello world" = 1; };
          symbols = testRoundTrip ''{ "@#$" = "symbols"; }'' { "@#$" = "symbols"; };
        };
        whitespace = {
          spaces = testRoundTrip "{ a = 1 ; b = 2 ; }" { a = 1; b = 2; };
          newlines = testRoundTrip "{\n  a = 1;\n  b = 2;\n}" { a = 1; b = 2; };
          tabs = testRoundTrip "{\ta = 1;\tb = 2;}" { a = 1; b = 2; };
        };
        deepNesting = {
          threeLevels = testRoundTrip "{ a = { b = { c = 42; }; }; }" { a = { b = { c = 42; }; }; };
          fourLevels = testRoundTrip "{ a = { b = { c = { d = 1; }; }; }; }" { a = { b = { c = { d = 1; }; }; }; };
          mixed = testRoundTrip "{ a = { b = [1 2]; c = { d = true; }; }; }" { a = { b = [1 2]; c = { d = true; }; }; };
        };
        shadowing = {
          letShadowsGlobal = testRoundTrip "let a = 1; in { a = 2; result = a; }" { a = 2; result = 1; };
          innerShadowsOuter = testRoundTrip "{ a = 1; inner = { a = 2; }; }" { a = 1; inner = { a = 2; }; };
        };
        withNull = {
          nullValue = testRoundTrip "{ a = null; }" { a = null; };
          mixedWithNull = testRoundTrip "{ a = 1; b = null; c = true; }" { a = 1; b = null; c = true; };
        };
        allTypes = testRoundTrip ''{ 
          int = 42; 
          float = 3.14; 
          string = "hello"; 
          bool = true; 
          null = null; 
          list = [1 2]; 
          attrs = { nested = true; }; 
        }'' { 
          int = 42; 
          float = 3.14; 
          string = "hello"; 
          bool = true; 
          null = null; 
          list = [1 2]; 
          attrs = { nested = true; }; 
        };
      };

      # Binary operations
      _05_arithmetic = {
        addition = {
          simple = testRoundTrip "1 + 2" 3;
          floats = testRoundTrip "1.5 + 2.5" 4.0;
          negative = testRoundTrip "(-1) + (-2)" (-3);
          zero = testRoundTrip "0 + 5" 5;
          floatInt = testRoundTrip "1.5 + 2" 3.5;
          intFloat = testRoundTrip "2 + 1.5" 3.5;
          large = testRoundTrip "999999999 + 1" 1000000000;
          stringConcat = testRoundTrip ''"hello" + " world"'' "hello world";
          pathConcat = testRoundTrip "./hello + /world" /tmp/pwd/hello/world;
          mixed = testRoundTrip "1 + 2 + 3" 6;
          precedence = testRoundTrip "1 + 2 * 3" 7;
        };
        subtraction = {
          simple = testRoundTrip "10 - 3" 7;
          floats = testRoundTrip "5.5 - 2.5" 3.0;
          negative = testRoundTrip "(-5) - (-3)" (-2);
          zero = testRoundTrip "5 - 0" 5;
          floatInt = testRoundTrip "5.5 - 2" 3.5;
          intFloat = testRoundTrip "5 - 2.5" 2.5;
          toNegative = testRoundTrip "3 - 5" (-2);
          chain = testRoundTrip "10 - 3 - 2" 5;
        };
        multiplication = {
          simple = testRoundTrip "3 * 4" 12;
          floats = testRoundTrip "2.5 * 3.0" 7.5;
          negative = testRoundTrip "(-3) * 4" (-12);
          zero = testRoundTrip "5 * 0" 0;
          one = testRoundTrip "7 * 1" 7;
          floatInt = testRoundTrip "2.5 * 4" 10.0;
          intFloat = testRoundTrip "4 * 2.5" 10.0;
          large = testRoundTrip "999 * 1000" 999000;
          chain = testRoundTrip "2 * 3 * 4" 24;
          precedence = testRoundTrip "2 + 3 * 4" 14;
        };
        division = {
          simple = testRoundTrip "8 / 2" 4;
          floats = testRoundTrip "7.5 / 2.5" 3.0;
          intToFloat = testRoundTrip "7 / 2" 3;
          negative = testRoundTrip "(-8) / 2" (-4);
          negativeNumerator = testRoundTrip "8 / (-2)" (-4);
          floatInt = testRoundTrip "7.5 / 3" 2.5;
          intFloat = testRoundTrip "8 / 2.0" 4.0;
          fractional = testRoundTrip "1 / 3" 0;
          chain = testRoundTrip "24 / 4 / 2" 3;
          precedence = testRoundTrip "8 / 2 + 1" 5;
        };
        listOps = {
          concat = testRoundTrip "[1 2] ++ [3 4]" [1 2 3 4];
          empty = testRoundTrip "[] ++ [1 2]" [1 2];
          emptyRight = testRoundTrip "[1 2] ++ []" [1 2];
          nested = testRoundTrip "[[1]] ++ [[2]]" [[1] [2]];
          mixed = testRoundTrip ''[1 "a"] ++ [true null]'' [1 "a" true null];
          chain = testRoundTrip "[1] ++ [2] ++ [3]" [1 2 3];
        };
        attrOps = {
          merge = testRoundTrip "{a = 1;} // {b = 2;}" {a = 1; b = 2;};
          override = testRoundTrip "{a = 1; b = 2;} // {b = 3;}" {a = 1; b = 3;};
          empty = testRoundTrip "{} // {a = 1;}" {a = 1;};
          emptyRight = testRoundTrip "{a = 1;} // {}" {a = 1;};
          nested = testRoundTrip "{a = {x = 1;};} // {b = {y = 2;};}" {a = {x = 1;}; b = {y = 2;};};
          chain = testRoundTrip "{a = 1;} // {b = 2;} // {c = 3;}" {a = 1; b = 2; c = 3;};
          complex = testRoundTrip "{a = 1; b = 2;} // {b = 3; c = 4;}" {a = 1; b = 3; c = 4;};
        };
        associativity = {
          leftAssoc = testRoundTrip "10 - 5 - 2" 3;
          rightAssoc = testRoundTrip "2 + 3 + 4" 9;
          mixed = testRoundTrip "2 * 3 + 4" 10;
          parentheses = testRoundTrip "2 * (3 + 4)" 14;
        };
        edgeCases = {
          largeNumbers = testRoundTrip "9007199254740991 + 1" 9007199254740992;
          verySmallFloat = testRoundTrip "0.000001 * 1000000" 1.0;
          zeroOperations = testRoundTrip "0 + 0 - 0 * 5" 0;
          negativeZero = testRoundTrip "-0" 0;
        };
      };

      _06_logical = {
        and = {
          trueFalse = testRoundTrip "true && false" false;
          trueTrue = testRoundTrip "true && true" true;
          falseFalse = testRoundTrip "false && false" false;
          falseTrue = testRoundTrip "false && true" false;
          shortCircuit = testRoundTrip "false && (abort \"should not evaluate\")" false;
          chain = testRoundTrip "true && true && false" false;
          allTrue = testRoundTrip "true && true && true" true;
          precedence = testRoundTrip "true || false && false" true;
          parentheses = testRoundTrip "(true || false) && false" false;
          nonBoolFirst = expectEvalError TypeError "1 && true";
          nonBoolSecond = expectEvalError TypeError "true && 1";
          bothNonBool = expectEvalError TypeError "1 && 2";
        };
        or = {
          trueFalse = testRoundTrip "true || false" true;
          trueTrue = testRoundTrip "true || true" true;
          falseFalse = testRoundTrip "false || false" false;
          falseTrue = testRoundTrip "false || true" true;
          shortCircuit = testRoundTrip "true || (abort \"should not evaluate\")" true;
          chain = testRoundTrip "false || false || true" true;
          allFalse = testRoundTrip "false || false || false" false;
          precedence = testRoundTrip "false && true || true" true;
          parentheses = testRoundTrip "false && (true || true)" false;
          nonBoolFirst = expectEvalError TypeError "1 || true";
          nonBoolSecond = expectEvalError TypeError "false || 1";
          bothNonBool = expectEvalError TypeError "1 || 2";
        };
        mixed = {
          andOr = testRoundTrip "true && false || true" true;
          orAnd = testRoundTrip "false || true && false" false;
          complex = testRoundTrip "(true || false) && (false || true)" true;
          nested = testRoundTrip "true && (false || true)" true;
          doubleNegative = testRoundTrip "!(!true)" true;
          withComparison = testRoundTrip "(1 == 1) && (2 > 1)" true;
          withArithmetic = testRoundTrip "(1 + 1 == 2) || false" true;
        };
        edgeCases = {
          multipleParens = testRoundTrip "((true))" true;
          deepNesting = testRoundTrip "true && (false || (true && true))" true;
          allOperators = testRoundTrip "true && true || false && false" true;
          precedenceTest = testRoundTrip "true || false && false || true" true;
        };
      };

      _07_comparison = {
        _00_equality = {
          _00_boolEqual = testRoundTrip "true == true" true;
          _01_boolNotEqual = testRoundTrip "true == false" false;
          _02_emptyListEqual = testRoundTrip "[] == []" true;
          _03_floatEqual = testRoundTrip "1.0 == 1.0" true;
          _04_floatIntEqual = testRoundTrip "1.0 == 1" true;
          _05_floatNotEqual = testRoundTrip "1.0 == 2.0" false;
          _06_intEqual = testRoundTrip "1 == 1" true;
          _07_intFloatEqual = testRoundTrip "1 == 1.0" true;
          _08_intNotEqual = testRoundTrip "1 == 2" false;
          _10_nullEqual = testRoundTrip "null == null" true;
          _11_nullNotEqual = testRoundTrip "null == 1" false;
          _09_parentheses = testRoundTrip "(1 == 1)" true;
          _10_stringEqual = testRoundTrip ''"hello" == "hello"'' true;
          _12_stringNotEqual = testRoundTrip ''"hello" == "world"'' false;
          _13_listEqual = testRoundTrip "[1 2] == [1 2]" true;
          _14_listNotEqual = testRoundTrip "[1 2] == [2 1]" false;
          _14_attrEqual = testRoundTrip "{a = 1;} == {a = 1;}" true;
          _15_attrNotEqual = testRoundTrip "{a = 1;} == {a = 2;}" false;
          _16_emptyAttrEqual = testRoundTrip "{} == {}" true;
          _17_lambda = testRoundTrip "(a: a) == (b: b)" false;
          _18_lambdaSelf = testRoundTrip "let f = (a: a); in f == f" false;
        };
        _01_inequality = {
          _00_intNotEqual = testRoundTrip "1 != 2" true;
          _01_intEqual = testRoundTrip "1 != 1" false;
          _02_floatNotEqual = testRoundTrip "1.0 != 2.0" true;
          _03_floatEqual = testRoundTrip "1.0 != 1.0" false;
          _04_stringNotEqual = testRoundTrip ''"hello" != "world"'' true;
          _05_stringEqual = testRoundTrip ''"hello" != "hello"'' false;
          _07_boolNotEqual = testRoundTrip "true != false" true;
          _08_boolEqual = testRoundTrip "true != true" false;
          _09_nullNotEqual = testRoundTrip "null != 1" true;
          _10_nullEqual = testRoundTrip "null != null" false;
        };
        _02_ordering = {
          _00_intLess = testRoundTrip "1 < 2" true;
          _01_intNotLess = testRoundTrip "2 < 1" false;
          _02_intLessEqual = testRoundTrip "1 <= 1" true;
          _03_intLessEqualFalse = testRoundTrip "2 <= 1" false;
          _04_intGreater = testRoundTrip "3 > 2" true;
          _05_intNotGreater = testRoundTrip "2 > 3" false;
          _06_intGreaterEqual = testRoundTrip "2 >= 2" true;
          _07_intGreaterEqualFalse = testRoundTrip "1 >= 2" false;
          _08_floatLess = testRoundTrip "1.5 < 2.5" true;
          _09_floatGreater = testRoundTrip "2.5 > 1.5" true;
          _10_intFloatLess = testRoundTrip "1 < 1.5" true;
          _11_floatIntGreater = testRoundTrip "1.5 > 1" true;
          _12_stringLess = testRoundTrip ''"a" < "b"'' true;
          _13_stringGreater = testRoundTrip ''"b" > "a"'' true;
          _14_stringEqual = testRoundTrip ''"a" <= "a"'' true;
        };
        _03_edgeCases = {
          _00_zero = testRoundTrip "0 == 0" true;
          _01_negativeZero = testRoundTrip "(-0) == 0" true;
          _02_negativeComparison = testRoundTrip "(-1) < 0" true;
          _03_largeNumbers = testRoundTrip "999999999 == 999999999" true;
          _04_veryLargeNumbers = testRoundTrip "9223372036854775807 == 9223372036854775807" true;
          _05_floatPrecision = testRoundTrip "0.1 + 0.2 == 0.3" false;
          _06_emptyString = testRoundTrip ''"" == ""'' true;
          _07_emptyVsNull = testRoundTrip ''"" == null'' false;
          _08_boolVsInt = testRoundTrip "true == 1" false;
          _09_boolVsString = testRoundTrip ''true == "true"'' false;
          #_10_pointerEqualityTrickDoesNotWork = testRoundTrip ''
          #  let pointerEqual = a: b: [ a ] == [ b ];
          #      f = a: a;
          #      g = a: a;
          #  in [(pointerEqual f f) (pointerEqual f g)]
          #'' [false false];
        };
        _04_nested = {
          _00_listOrdering.one = testRoundTrip "[1] < [2]" true;
          _00_listOrdering.two = testRoundTrip "[1 2] < [2 1]" true;
          _00_listOrdering.twoRev = testRoundTrip "[1 1] < [1 2]" true;
          _00_listOrdering.twoRevGT = testRoundTrip "[1 1] > [1 2]" false;
          _01_attrOrdering = expectEvalError TypeError "{a = 1;} < {a = 2;}";
          _02_nestedAttrs = testRoundTrip "{a = {b = 1;};} == {a = {b = 1;};}" true;
          _03_nestedLists = testRoundTrip "[[1 2]] == [[1 2]]" true;
          _04_mixedNested = testRoundTrip ''[{a = 1;}] == [{a = 1;}]'' true;
        };
        _05_chains = {
          _00_comparison = testRoundTrip "1 < 2 && 2 < 3" true;
          _01_mixed = testRoundTrip "1 == 1 && 2 != 3" true;
          _02_precedence = testRoundTrip "1 + 1 == 2" true;
          _03_complex = testRoundTrip "(1 + 2) == 3 && (4 - 1) == 3" true;
        };
      };

      # Unary operations
      _08_unary = {
        not = {
          notFalse = testRoundTrip "!false" true;
          notTrue = testRoundTrip "!true" false;
          doubleNot = testRoundTrip "!!true" true;
          tripleNot = testRoundTrip "!!!false" true;
          withParens = testRoundTrip "!(true)" false;
          withComparison = testRoundTrip "!(1 == 2)" true;
          withLogical = testRoundTrip "!(true && false)" true;
          complex = testRoundTrip "!(1 < 2 && 3 > 4)" true;
          precedence = testRoundTrip "!true || false" false;
          precedenceWithParens = testRoundTrip "!(true || false)" false;
        };
        negation = {
          negativeInt = testRoundTrip "-42" (-42);
          negativeFloat = testRoundTrip "-3.14" (-3.14);
          negativeZero = testRoundTrip "-0" 0;
          doubleNegative = testRoundTrip "-(-5)" 5;
          tripleNegative = testRoundTrip "-(-(-3))" (-3);
          withParens = testRoundTrip "-(42)" (-42);
          withArithmetic = testRoundTrip "-(1 + 2)" (-3);
          withComparison = testRoundTrip "-(1)" (-1);
          largeNumber = testRoundTrip "-999999999" (-999999999);
          floatPrecision = testRoundTrip "-0.000001" (-0.000001);
          scientificNotation = testRoundTrip "-1.23e10" (-1.23e10);
        };
        combined = {
          notAndNegation = testRoundTrip "!(-1 == -1)" false;
          negationAndComparison = testRoundTrip "-(1) == (-1)" true;
          complexNesting = testRoundTrip "!(-(1) == 1)" true;
          precedence = testRoundTrip "-1 + 2" 1;
          precedenceWithParens = testRoundTrip "-(1 + 2)" (-3);
          multipleUnary = testRoundTrip "!(!true) && -(-1) > 0" true;
        };
        edgeCases = {
          notWithArithmetic = testRoundTrip "!(1 + 1 == 3)" true;
          negativeInList = testRoundTrip "[(-1) (-2) (-3)]" [(-1) (-2) (-3)];
          negativeInAttrs = testRoundTrip "{a = (-1); b = (-2);}" {a = (-1); b = (-2);};
          notInConditional = testRoundTrip "if !false then 1 else 2" 1;
          negativeInConditional = testRoundTrip "if (-1) < 0 then 1 else 2" 1;
        };
      };

      # Conditionals
      _09_conditionals = {
        simple = {
          trueCondition = testRoundTrip "if true then 1 else 2" 1;
          falseCondition = testRoundTrip "if false then 1 else 2" 2;
          withParens = testRoundTrip "if (true) then 1 else 2" 1;
          withComparison = testRoundTrip "if 1 == 1 then 42 else 0" 42;
          withLogical = testRoundTrip "if true && false then 1 else 2" 2;
          withArithmetic = testRoundTrip "if 1 + 1 == 2 then 100 else 0" 100;
        };
        nested = {
          elseIf = testRoundTrip "if false then 1 else if true then 2 else 3" 2;
          deepNesting = testRoundTrip "if false then 1 else if false then 2 else if true then 3 else 4" 3;
          allFalse = testRoundTrip "if false then 1 else if false then 2 else 3" 3;
          nestedInThen = testRoundTrip "if true then (if true then 1 else 2) else 3" 1;
          nestedInElse = testRoundTrip "if false then 1 else (if true then 2 else 3)" 2;
          bothNested = testRoundTrip "if true then (if false then 1 else 2) else (if true then 3 else 4)" 2;
        };
        expressions = {
          stringResult = testRoundTrip ''if true then "yes" else "no"'' "yes";
          listResult = testRoundTrip "if true then [1 2] else [3 4]" [1 2];
          attrResult = testRoundTrip "if true then {a = 1;} else {b = 2;}" {a = 1;};
          nullResult = testRoundTrip "if false then 1 else null" null;
          #functionResult = testRoundTrip "if true then (x: x + 1) else (x: x - 1)" (x: x + 1);
        };
        complexConditions = {
          multipleComparisons = testRoundTrip "if 1 < 2 && 3 > 2 then 1 else 0" 1;
          negation = testRoundTrip "if !(1 == 2) then 1 else 0" 1;
          orCondition = testRoundTrip "if false || true then 1 else 0" 1;
          precedence = testRoundTrip "if true && false || true then 1 else 0" 1;
          withParens = testRoundTrip "if (true && false) || true then 1 else 0" 1;
        };
        typeVariations = {
          intConditions = expectEvalError TypeError "if 1 then 42 else 0";
          zeroCondition = expectEvalError TypeError "if 0 then 1 else 2";
          stringCondition = expectEvalError TypeError ''if "hello" then 1 else 2'';
          emptyStringCondition = expectEvalError TypeError ''if "" then 1 else 2'';
          listCondition = expectEvalError TypeError "if [1] then 1 else 2";
          emptyListCondition = expectEvalError TypeError "if [] then 1 else 2";
          attrCondition = expectEvalError TypeError "if {a = 1;} then 1 else 2";
          emptyAttrCondition = expectEvalError TypeError "if {} then 1 else 2";
          nullCondition = expectEvalError TypeError "if null then 1 else 2";
        };
        contextual = {
          inLet = testRoundTrip "let x = true; in if x then 1 else 2" 1;
          inAttr = testRoundTrip "{result = if true then 1 else 2;}" {result = 1;};
          inList = testRoundTrip "[(if true then 1 else 2)]" [1];
          inFunction = testRoundTrip "(x: if x then 1 else 2) true" 1;
          inArithmetic = testRoundTrip "(if true then 1 else 2) + 3" 4;
          asComparison = testRoundTrip "(if true then 1 else 2) == 1" true;
        };
        edgeCases = {
          sameTypeResults = testRoundTrip "if true then 1 else 1" 1;
          differentTypes = testRoundTrip ''if true then 1 else "hello"'' 1;
          complexExpressions = testRoundTrip "if (let x = 1; in x == 1) then (1 + 2) else (3 * 4)" 3;
          # Skip - self-recursion
          # recursiveCondition = testRoundTrip "let f = x: if x == 0 then 1 else x * f (x - 1); in f 3" 6;
          withScope = testRoundTrip "let a = 1; in if true then a + 1 else a - 1" 2;
        };
      };

      # Let expressions
      _10_letExpressions = {
        simple = {
          basic = testRoundTrip "let x = 1; in x" 1;
          multipleBindings = testRoundTrip "let a = 1; b = 2; in a + b" 3;
          withString = testRoundTrip ''let msg = "hello"; in msg'' "hello";
          withList = testRoundTrip "let xs = [1 2 3]; in xs" [1 2 3];
          withAttrs = testRoundTrip "let obj = {a = 1;}; in obj" {a = 1;};
          withFunction = testRoundTrip "let f = x: x + 1; in f 5" 6;
          withFunctions = testRoundTrip "let a = 1; f = x: x + a; in f 5" 6;
        };
        nested = skip { # deep overflows
          basic = testRoundTrip "let x = 1; y = let z = 2; in z + 1; in x + y" 4;
          deep = testRoundTrip "let a = let b = let c = 1; in c + 1; in b + 1; in a + 1" 4;
          dependent = testRoundTrip "let x = 1; y = let z = x + 1; in z * 2; in y" 4;
          independent = testRoundTrip "let x = (let y = 1; in y); z = (let w = 2; in w); in x + z" 3;
        };
        recursive = {
          _00_simple = testRoundTrip "let x = y; y = 1; in x" 1;
          _01_mutual = testRoundTrip "let a = b + 1; b = 5; in a" 6;
          _02_complex = testRoundTrip "let a = b + c; b = 2; c = 3; in a" 5;
          _03_factorial._00_let =
            let expr = i: "let f = x: if x <= 1 then 1 else x * f (x - 1); in f ${toString i}";
            in {
              _0 = testRoundTripSame (expr 0) 1;
              _1 = testRoundTripSame (expr 1) 1;
              _2 = testRoundTripSame (expr 2) 2;
              _3 = testRoundTripSame (expr 3) 6;
              _4 = testRoundTripSame (expr 4) 24;
            };
          _03_factorial._01_rec = 
            let expr = i: "rec { f = x: if x <= 1 then 1 else x * f (x - 1); x = f ${toString i}; }.x";
            in {
              _0 = testRoundTrip (expr 0) 1;
              _1 = testRoundTrip (expr 1) 1;
              #_2 = testRoundTrip (expr 2) 2;
              #_3 = testRoundTrip (expr 3) 6;
              #_4 = testRoundTrip (expr 4) 24;
            };
          _03_factorial._02_with = 
            let expr = i: "with rec { f = x: if x <= 1 then 1 else x * f (x - 1); }; f ${toString i}";
            in {
              _0 = testRoundTrip (expr 0) 1;
              _1 = testRoundTrip (expr 1) 1;
              _2 = testRoundTrip (expr 2) 2;
              _3 = testRoundTrip (expr 3) 6;
              _4 = testRoundTrip (expr 4) 24;
            };
          _04_fibonacci._00_let =
            let expr = i: "let fib = n: if n <= 1 then n else fib (n - 1) + fib (n - 2); in fib ${toString i}";
            in {
              _0 = testRoundTrip (expr 0) 0;
              _1 = testRoundTrip (expr 1) 1;
              _2 = testRoundTrip (expr 2) 1;
              _3 = testRoundTrip (expr 3) 2;
              _4 = testRoundTrip (expr 4) 3;
            };
          # TODO: Fixing this requires laziness; when fib2 calls into memo, memo is fully eval'd,
          # including fib2. We should instead have 'attrs' evaluate to an object that can be
          # accessed lazily i.e. most evaluations should return thunks.
          _05_fibonacci._01_memo =
            skip (
            let expr = ''
              let k = i: "_" + (builtins.toString i);
                  fibm = i: builtins.getAttr (k i) memo;
                  fib = i: fibm (i - 1) + fibm (i - 2);
                  memo = {
                    _0 = 1;
                    _1 = 1;
                    _2 = fib 2;
                    #_3 = fib 3;
                    #_4 = fib 4;
                    #_5 = fib 5;
                    #_6 = fib 6;
                    #_7 = fib 7;
                  };
              in lib.attrValues memo
            '';
            in (testRoundTrip expr [0 1 1 2 3 5 8 13])
            );
        };
        _06_shadowing = {
          innerShadowsOuter = testRoundTrip "let x = 1; in let x = 2; in x" 2;
          accessOuter = testRoundTrip "let x = 1; y = x + 1; in let x = 3; in y" 2;
          multipleLevels = testRoundTrip "let x = 1; in let x = 2; in let x = 3; in x" 3;
          partialShadowing = testRoundTrip "let x = 1; y = 2; in let x = 3; z = x + y; in z" 5;
        };
        _07_expressions = {
          arithmetic = testRoundTrip "let x = 1 + 2; y = x * 3; in y" 9;
          conditionals = testRoundTrip "let result = if true then 1 else 2; in result" 1;
          withScope = testRoundTrip "let x = 1; result = if x == 1 then x + 1 else x - 1; in result" 2;
          functionApplication = testRoundTrip "let f = x: x * 2; result = f 5; in result" 10;
          listOperations = testRoundTrip "let xs = [1 2]; ys = [3 4]; result = xs ++ ys; in result" [1 2 3 4];
          attrOperations = testRoundTrip "let a = {x = 1;}; b = {y = 2;}; result = a // b; in result" {x = 1; y = 2;};
        };
        _08_edgeCases = {
          complexBody = testRoundTrip "let x = 1; in {a = x; b = x + 1; c = [x (x + 1)];}" {a = 1; b = 2; c = [1 2];};
          emptyLet = testRoundTrip "let in 42" 42;
          functionInLet = testRoundTrip "let f = (x: y: x + y); in f 1 2" 3;
          letInFunction = testRoundTrip "(x: let y = x + 1; in y * 2) 3" 8;
          #nestedAccess = testRoundTrip "let outer = 1; in let inner = outer + 1; final = inner + outer; in final" 3;
          unusedBinding = testRoundTrip "let x = 1; y = 2; in x" 1;
        };
      };

      # Functions
      _11_functions = {
        _00_smoke = {
          _00_identity = testRoundTrip "let f = x: x; in f 42" 42;
          _01_const = testRoundTrip "let f = x: y: x; in f 1 2" 1;
          _02_returnBinOp = testRoundTrip "let f = x: x + 1; in f 1" 2;
          _03_recursive._0 = testRoundTrip "let f = x: if x == 0 then 1 else x * f (x - 1); in f 0" 1;
          _03_recursive._1 = testRoundTrip "let f = x: if x == 0 then 1 else x * f (x - 1); in f 1" 1;
        };

        # Edge case tests for function parameters and applications
        _01_simpleParams = {
          identity = testRoundTrip "(x: x) 42" 42;
          arithmetic = testRoundTrip "(x: x + 1) 5" 6;
          multiple = testRoundTrip "(x: y: x + y) 3 4" 7;
          nested = testRoundTrip "(x: (y: x + y)) 1 2" 3;
          currying = testRoundTrip "let f = x: y: z: x + y + z; in f 1 2 3" 6;
        };
        
        _02_attrParams = {
          _00_simple = testRoundTrip "({a}: a) {a = 42;}" 42;
          _01_multiple = testRoundTrip "({a, b}: a + b) {a = 1; b = 2;}" 3;
          _02_withDefaults = testRoundTrip "({a ? 1, b}: a + b) {b = 2;}" 3;
          _03_ellipsis = testRoundTrip "({a, ...}: a) {a = 1; b = 2;}" 1;
          _04_defaultOverride = testRoundTrip "({a ? 1}: a) {a = 2;}" 2;
          _05_dependentFirstOverride = (testRoundTrip "({a ? 1, b ? a + 1}: a + b) {a = 2;}" 3);
          _06_dependentSecondOverride = (testRoundTrip "({a ? 1, b ? a + 1}: a + b) {b = 2;}" 3);
          _07_dependentTwoDefaults = (testRoundTrip "({a ? 1, b ? a + 1}: a + b) {}" 3);
          _08_mixedParams = testRoundTrip "({a, b ? 10}: a + b) {a = 5;}" 15;
        };

        _03_evaluatedLambdas = skip {
          _00_apply = {
            _00_applyEvaluatedLambda = testRoundTrip "(x: x + 2) 40" 42;
            _01_applyEvaluatedLambdaNested = testRoundTrip "(x: y: x + y) 40 2" 42;
            _02_applyNixLambda = testRoundTrip "(x: builtins.add) {} 40 2" 42;
          };
          _01_return = {
            _00_returnEvaluatedLambda =
              let result = evalAST "(x: x + 2)";
              in expect.eq ((result.fmap (f: f 40)).right or null) 42;
            _01_returnEvaluatedLambdaNestedMixedApplication =
              let result = evalAST' "(x: y: x + y) 40";
              in expect.eq ((result.fmap (f: f 2)).right or null) 42;
            _02_returnNixLambda =
              let result = evalAST "(x: builtins.add) {}";
              in expect.eq (result.fmap (f: f 40 2)).right 42;
          };
        };

        _04_scopeAndClosure = {
          closure = testRoundTrip "let x = 1; f = y: x + y; in f 2" 3;
          nestedClosure = testRoundTrip "let x = 1; in (let y = 2; f = z: x + y + z; in f 3)" 6;
          shadowParameter = testRoundTrip "let x = 1; f = x: x + 1; in f 5" 6;
          recursiveReference = testRoundTrip "let f = x: if x == 0 then 1 else x * f (x - 1); in f 3" 6;
          argsOverWiths = testRoundTrip "(arg: with { arg = 1; }; arg) 2" 2;
        };
        
        _05_partialApplication = {
          curried = testRoundTrip "let add = x: y: x + y; add5 = add 5; in add5 3" 8;
          complex = testRoundTrip "let f = a: b: c: a + b * c; g = f 1; h = g 2; in h 3" 7;
          withAttrs = testRoundTrip "let f = {a}: b: a + b; g = f {a = 10;}; in g 5" 15;
        };
        
        _06_errorCases = {
          missingParam = expectEvalError RuntimeError "({a, b}: a + b) {a = 1;}";
          unknownParam = expectEvalError RuntimeError "({a}: a) {a = 1; b = 2;}";
          wrongType = expectEvalError TypeError "({a}: a) 42";
          nestedApplication = testRoundTrip "((x: y: x + y) 1) 2" 3;
        };

        _07_functors = skip {
          _00_isFunctionBuiltins = testRoundTrip "builtins.isFunction { __functor = self: x: x + 1; }" false;
          _01_isFunctionLib = testRoundTrip "lib.isFunction { __functor = self: x: x + 1; }" true;
          _02_isAttrs = testRoundTrip "builtins.isAttrs { __functor = self: x: x + 1; }" true;
          _03_callable = testRoundTrip "{ __functor = self: x: x + 1; } 1" 2;
          _04_returnCallable =
            let result = nix-reflect.eval.strict "{ __functor = self: x: x + 1; }";
            in expect.eq (result.right 1) 2;
        };
      };

      # Attribute access
      _12_attrAccess = {
        simple = {
          basic = testRoundTrip "{ a = 42; }.a" 42;
          letIn = testRoundTrip "let xs = { a = 42; }; in xs.a" 42;
          nested = testRoundTrip "{ a = { b = 42; }; }.a.b" 42;
          deepNesting = testRoundTrip "{ a = { b = { c = { d = 42; }; }; }; }.a.b.c.d" 42;
          multipleAttrs = testRoundTrip "{ a = 1; b = 2; }.a + { a = 1; b = 2; }.b" 3;
        };
        
        withDefaults = {
          exists = testRoundTrip "{ a = 42; }.a or 0" 42;
          missing = testRoundTrip "{ a = 42; }.b or 0" 0;
          nestedExists = testRoundTrip "{ a = { b = 42; }; }.a.b or 0" 42;
          nestedMissing = testRoundTrip "{ a = { b = 42; }; }.a.c or 0" 0;
          chainedDefaults = testRoundTrip "{ a = 1; }.b or { c = 2; }.c or 3" 2;
          complexDefault = testRoundTrip "{ a = 1; }.b or (1 + 2)" 3;
          nullDefault = testRoundTrip "{ a = 1; }.b or null" null;
          stringDefault = testRoundTrip ''{ a = 1; }.b or "default"'' "default";
        };
        
        dynamicAccess = {
          stringKey = testRoundTrip ''{ "hello world" = 42; }."hello world"'' 42;
          numberKey = testRoundTrip ''{ "123" = 42; }."123"'' 42;
          specialChars = testRoundTrip ''{ "@#$" = 42; }."@#$"'' 42;
          computed = testRoundTrip ''let key = "a"; attrs = { a = 42; }; in attrs.a'' 42;
          expression = testRoundTrip ''let attr = if true then "a" else "b"; in { a = 42; }.a'' 42;
        };
        
        errorCases = {
          missingAttr = expectEvalError MissingAttributeError "{ a = 1; }.b";
          missingAttrDownstream = expectEvalError MissingAttributeError "let b = {}.a; in b.c or 2";
          missingNested = expectEvalError MissingAttributeError "{ a = { b = 1; }; }.a.c";
          accessNonAttr = expectEvalError TypeError "42.a";
          accessNull = expectEvalError TypeError "null.a";
          accessString = expectEvalError TypeError ''"hello".a'';
          accessList = expectEvalError TypeError "[1 2 3].a";
          deepMissing = expectEvalError MissingAttributeError "{ a = { b = {}; }; }.a.b.c";
          deepMissingDoesntPropagate0 = testRoundTrip "{a = {b = {};};}.a.b.c or 4" 4;
          deepMissingDoesntPropagate1 = expectEvalError MissingAttributeError "{a = {b = {};};}.a.b.c";
          deepMissingDoesntPropagate2 = expectEvalError MissingAttributeError "{a = {b = {}.c;}; }.a.b or 4";
          deepMissingDoesntPropagate3 = expectEvalError MissingAttributeError "{a = {b = {}.c;};}.a or 4";
          missingInPathDoesntPropagate = expectEvalError MissingAttributeError "{}.\${{}.a} or 2";
          missingInKeyDoesntPropagate = expectEvalError MissingAttributeError "{\${{}.a} = 1;}.a or 2";
        };
        
        expressions = {
          arithmetic = testRoundTrip "{ a = 1; b = 2; }.a + { a = 1; b = 2; }.b" 3;
          comparison = testRoundTrip "{ a = 1; }.a == 1" true;
          conditional = testRoundTrip "if { test = true; }.test then 1 else 2" 1;
          function = testRoundTrip "{ f = x: x + 1; }.f 5" 6;
          list = testRoundTrip "{ items = [1 2 3]; }.items" [1 2 3];
          nestedFunction = testRoundTrip "{ outer = { inner = x: x * 2; }; }.outer.inner 5" 10;
        };
        
        specialCases = {
          emptyAttr = testRoundTrip "{}.a or 42" 42;
          nullValue = testRoundTrip "{ a = null; }.a" null;
          boolValue = testRoundTrip "{ flag = true; }.flag" true;
          listValue = testRoundTrip "{ items = []; }.items" [];
          attrValue = testRoundTrip "{ nested = {}; }.nested" {};
          functionValue = testRoundTrip "{ id = x: x; }.id 42" 42;
        };
        
        chaining = {
          simple = testRoundTrip "{ a = { b = { c = 42; }; }; }.a.b.c" 42;
          withDefaults = testRoundTrip "{ a = { b = 1; }; }.a.c or { a = { b = 1; }; }.a.b" 1;
          mixed = testRoundTrip "{ a = { b = 2; }; }.a.b + { x = { y = 3; }; }.x.y" 5;
          conditional = testRoundTrip "{ test = { flag = true; }; }.test.flag && true" true;
        };
        
        contextual = {
          inLet = testRoundTrip "let obj = { a = 1; }; in obj.a" 1;
          inFunction = testRoundTrip "(obj: obj.value) { value = 42; }" 42;
          inList = testRoundTrip "[{ a = 1; }.a { b = 2; }.b]" [1 2];
          inAttr = testRoundTrip "{ result = { a = 1; }.a; }" { result = 1; };
          recursive = testRoundTrip "rec { a = b.value; b = { value = 42; }; }.a" 42;
        };
        
        edgeCases = {
          selfReference = testRoundTrip "let obj = { self = obj; value = 42; }; in obj.self.value" 42;
          multipleChains = testRoundTrip "{ a = { x = 1; }; b = { y = 2; }; }.a.x + { a = { x = 1; }; b = { y = 2; }; }.b.y" 3;
          withArithmetic = testRoundTrip "{ value = 10; }.value / 2" 5;
          withLogical = testRoundTrip "{ flag = true; }.flag && false" false;
          complexNesting = testRoundTrip "{ a = { b = { c = { d = { e = 42; }; }; }; }; }.a.b.c.d.e" 42;
        };
      };

      # Assert expressions - testing proper Nix semantics
      _13_assertExpressions = {
        basic = {
          assertTrue = testRoundTrip "assert true; 42" 42;
          assertFalse = expectEvalError AssertError "assert false; 42";
          withParens = testRoundTrip "assert (true); 42" 42;
          withExpression = testRoundTrip "assert (1 == 1); 42" 42;
        };
        
        typeErrors = {
          assertString = expectEvalError TypeError ''assert "error message"; 42'';
          assertInteger = expectEvalError TypeError "assert 1; 42";
          assertZero = expectEvalError TypeError "assert 0; 42";
          assertNull = expectEvalError TypeError "assert null; 42";
          assertList = expectEvalError TypeError "assert []; 42";
          assertAttrs = expectEvalError TypeError "assert {}; 42";
          assertFunction = expectEvalError TypeError "assert (x: x); 42";
        };
        
        complexConditions = {
          arithmetic = testRoundTrip "assert (1 + 1 == 2); 42" 42;
          comparison = testRoundTrip "assert (5 > 3); 42" 42;
          logical = testRoundTrip "assert (true && true); 42" 42;
          negation = testRoundTrip "assert (!false); 42" 42;
          nested = testRoundTrip "assert ((1 < 2) && (3 > 2)); 42" 42;
          withOr = testRoundTrip "assert (false || true); 42" 42;
          complex = testRoundTrip "assert (1 + 2 == 3 && 4 * 2 == 8); 42" 42;
        };
        
        failingConditions = {
          falseComparison = expectEvalError AssertError "assert (1 == 2); 42";
          failingArithmetic = expectEvalError AssertError "assert (1 + 1 == 3); 42";
          failingLogical = expectEvalError AssertError "assert (true && false); 42";
          failingNegation = expectEvalError AssertError "assert (!true); 42";
          complexFailing = expectEvalError AssertError "assert (1 > 2 || 3 < 2); 42";
        };
        
        nested = {
          simpleNested = testRoundTrip "assert true; assert true; 42" 42;
          chainedTrue = testRoundTrip "assert (1 == 1); assert (2 == 2); 42" 42;
          firstFails = expectEvalError AssertError "assert false; assert true; 42";
          secondFails = expectEvalError AssertError "assert true; assert false; 42";
          deepNesting = testRoundTrip "assert true; (assert true; (assert true; 42))" 42;
        };
        
        contextual = {
          inLet = testRoundTrip "let x = true; in assert x; 42" 42;
          letFails = expectEvalError AssertError "let x = false; in assert x; 42";
          inFunction = testRoundTrip "(x: assert x; 42) true" 42;
          functionFails = expectEvalError AssertError "(x: assert x; 42) false";
          inConditional = testRoundTrip "if true then (assert true; 42) else 0" 42;
          conditionalFails = expectEvalError AssertError "if true then (assert false; 42) else 0";
          inList = testRoundTrip "[(assert true; 42)]" [42];
          inAttrs = testRoundTrip "{ value = assert true; 42; }" { value = 42; };
        };
        
        withScope = {
          localVariable = testRoundTrip "let valid = true; in assert valid; 42" 42;
          computation = testRoundTrip "let x = 1; y = 2; in assert (x + y == 3); 42" 42;
          functionCall = testRoundTrip "let f = x: x > 0; in assert (f 5); 42" 42;
          nestedScope = testRoundTrip "let outer = true; in let inner = outer; in assert inner; 42" 42;
          shadowing = testRoundTrip "let x = true; in let x = false; in assert (!x); 42" 42;
        };
        
        edgeCases = {
          emptyBody = testRoundTrip "assert true; null" null;
          complexBody = testRoundTrip "assert true; { a = 1; b = [2 3]; }" { a = 1; b = [2 3]; };
          recursiveAssert = testRoundTrip "let f = x: if x == 0 then (assert true; 1) else (assert (x > 0); x * f (x - 1)); in f 3" 6;
          assertInRecursion = testRoundTrip "rec { value = assert (other == 42); other; other = 42; }.value" 42;
          multipleReturns = testRoundTrip "if true then (assert true; 1) else (assert true; 2)" 1;
        };
        
        propagation = {
          throughArithmetic = expectEvalError AssertError "1 + (assert false; 2)";
          throughComparison = expectEvalError AssertError "(assert false; 1) == 1";
          throughLogical = expectEvalError AssertError "true && (assert false; true)";
          throughList = expectEvalError AssertError "[1 (assert false; 2) 3]";
          throughAttrs = expectEvalError AssertError "{ a = 1; b = assert false; 2; }";
          throughFunction = expectEvalError AssertError "(x: x + 1) (assert false; 5)";
        };
      };

      # Abort expressions - testing our custom abort handling
      _14_abortExpressions = {
        basic = {
          simpleString = expectEvalError Abort ''abort "custom abort message"'';
          emptyString = expectEvalError Abort ''abort ""'';
          withEscapes = expectEvalError Abort ''abort "hello\nworld"'';
          withUnicode = expectEvalError Abort ''abort "λ error"'';
        };
        
        computed = {
          stringConcat = expectEvalError Abort ''abort ("a " + "msg")'';
          withArithmetic = expectEvalError Abort ''abort ("error " + builtins.toString 42)'';
          conditional = expectEvalError Abort ''abort (if true then "yes" else "no")'';
          fromVariable = expectEvalError Abort ''let msg = "error"; in abort msg'';
          complex = expectEvalError Abort ''abort ("prefix: " + (builtins.toString (1 + 2)))'';
        };
        
        typeErrors = {
          nonString = expectEvalError TypeError "abort 42";
          nullMessage = expectEvalError TypeError "abort null";
          boolMessage = expectEvalError TypeError "abort true";
          listMessage = expectEvalError TypeError "abort []";
          attrMessage = expectEvalError TypeError "abort {}";
          functionMessage = expectEvalError TypeError "abort (x: x)";
        };
        
        propagation = {
          throughArithmetic = expectEvalError Abort ''1 + (abort "msg")'';
          throughComparison = expectEvalError Abort ''(abort "msg") == 1'';
          throughLogical = expectEvalError Abort ''true && (abort "msg")'';
          throughList = expectEvalError Abort ''[1 (abort "msg") 3]'';
          throughAttrs = expectEvalError Abort ''{ a = 1; b = abort "msg"; }'';
          throughFunction = expectEvalError Abort ''(x: x + 1) (abort "msg")'';
          leftOperand = expectEvalError Abort ''(abort "left") + 1'';
          rightOperand = expectEvalError Abort ''1 + (abort "right")'';
        };
        
        contextual = {
          inLet = expectEvalError Abort ''let x = abort "error"; in x'';
          inConditional = expectEvalError Abort ''if true then (abort "error") else 42'';
          inElse = expectEvalError Abort ''if false then 42 else (abort "error")'';
          inCondition = expectEvalError Abort ''if (abort "error") then 1 else 2'';
          inFunction = expectEvalError Abort ''(x: abort "error") 42'';
          inApplication = expectEvalError Abort ''((abort "error") x) 42'';
          inRecursive = expectEvalError Abort ''rec { a = abort "error"; b = a; }'';
        };
        
        nested = {
          doubleAbort = expectEvalError Abort ''abort (abort "inner")'';
          abortInCondition = expectEvalError Abort ''abort (if (abort "condition") then "yes" else "no")'';
          abortInArithmetic = expectEvalError Abort ''abort (1 + (abort "inner"))'';
        };
        
        shortCircuiting = {
          logicalAndFalse = testRoundTrip ''false && (abort "should not reach")'' false;
          logicalOrTrue = testRoundTrip ''true || (abort "should not reach")'' true;
          logicalAndAbort = expectEvalError Abort ''(abort "error") && false'';
          logicalOrAbort = expectEvalError Abort ''(abort "error") || true'';
          conditionalTrue = testRoundTrip ''if true then 42 else (abort "should not reach")'' 42;
          conditionalFalse = testRoundTrip ''if false then (abort "should not reach") else 42'' 42;
        };
        
        edgeCases = {
          veryLongMessage = expectEvalError Abort ''abort ("very " + "long " + "error " + "message " + "here")'';
          emptyMessage = expectEvalError Abort ''abort ""'';
          messageWithNewlines = expectEvalError Abort ''abort "line1\nline2\nline3"'';
          messageWithTabs = expectEvalError Abort ''abort "tab\there"'';
          unicodeMessage = expectEvalError Abort ''abort "λ λ λ"'';
          numberInMessage = expectEvalError Abort ''abort ("error code: " + builtins.toString 404)'';
        };
        
        interaction = {
          beforeAssert = expectEvalError Abort ''(abort "first") && (assert false; true)'';
          afterAssert = expectEvalError AssertError ''(assert false; true) && (abort "second")'';
          withThrow = expectEvalError Abort ''let x = abort "abort"; y = throw "throw"; in x'';
          abortOverThrow = expectEvalError Abort ''(abort "abort") + (throw "throw")'';
        };
      };

      # With expressions - testing proper scope precedence
      _15_withExpressions = {
        basic = {
          simple = testRoundTrip "with { a = 1; }; a" 1;
          multipleAttrs = testRoundTrip "with { a = 1; b = 2; }; a + b" 3;
          emptyWith = testRoundTrip "with {}; 42" 42;
          withString = testRoundTrip ''with { msg = "hello"; }; msg'' "hello";
          withList = testRoundTrip "with { items = [1 2 3]; }; items" [1 2 3];
          withFunction = testRoundTrip "with { f = x: x + 1; }; f 5" 6;
        };
        
        shadowing = {
          lexicalShadows = testRoundTrip "let x = 1; in with { x = 2; }; x" 1;
          multipleLevels = testRoundTrip "let x = 1; in with { x = 2; }; let x = 3; in x" 3;
          partialShadowing = testRoundTrip "let a = 10; b = 20; in with { a = 1; c = 3; }; a + b + c" 33;
          withShadowsGlobal = testRoundTrip "with { y = 2; }; y" 2;
          nestedLexical = testRoundTrip "let x = 1; in let x = 2; in with { x = 3; }; x" 2;
          innerShadowing = testRoundTrip "with { a = 1; }; let a = 2; in a" 2;
        };
        
        nested = {
          simple = testRoundTrip "with { a = 1; }; with { b = 2; }; a + b" 3;
          overlapping = testRoundTrip "with { a = 1; b = 2; }; with { b = 3; c = 4; }; a + b + c" 7;
          deep = testRoundTrip "with { a = 1; }; with { b = 2; }; with { c = 3; }; a + b + c" 6;
          access = testRoundTrip "with { outer = { inner = 42; }; }; with outer; inner" 42;
          independent = testRoundTrip "with { x = 1; }; (with { y = 2; }; y) + x" 3;
        };
        
        fallback = {
          basic = testRoundTrip "with { y = 2; }; y" 2;
          withLet = testRoundTrip "let x = 1; in with { y = 2; }; x + y" 3;
          notFound = expectEvalError UnknownIdentifierError "with { a = 1; }; b";
          nestedFallback = testRoundTrip "with { a = { b = 1; }; }; a.b" 1;
          deepAccess = testRoundTrip "with { data = { values = [1 2 3]; }; }; data.values" [1 2 3];
        };
        
        typeErrors = {
          nonAttrSet = expectEvalError UnknownIdentifierError "with 42; x";
          nullWith = expectEvalError UnknownIdentifierError "with null; x";
          stringWith = expectEvalError UnknownIdentifierError ''with "hello"; x'';
          functionWith = expectEvalError UnknownIdentifierError "with (x: x); x";
        };
        
        expressions = {
          arithmetic = testRoundTrip "with { x = 5; y = 3; }; x + y" 8;
          attrOps = testRoundTrip "with { a = { x = 1; }; b = { y = 2; }; }; a // b" { x = 1; y = 2; };
          comparison = testRoundTrip "with { a = 1; b = 2; }; a < b" true;
          conditional = testRoundTrip "with { flag = true; }; if flag then 1 else 2" 1;
          function = testRoundTrip "with { double = x: x * 2; }; double 5" 10;
          listOps = testRoundTrip "with { xs = [1 2]; ys = [3 4]; }; xs ++ ys" [1 2 3 4];
        };
        
        scoping = {
          recursive = testRoundTrip "with rec { a = 1; b = a + 1; }; b" 2;
          mutualRecursion = testRoundTrip "with rec { a = b + 1; b = 5; }; a" 6;
          crossReference = testRoundTrip "with {y = 5;}; with { x = y + 1; }; x" 6;
        };
        
        contextual = {
          inLet = testRoundTrip "let result = with { a = 1; }; a; in result" 1;
          inFunction = testRoundTrip "(env: with env; value) { value = 42; }" 42;
          inList = testRoundTrip "with { x = 1; y = 2; }; [x y]" [1 2];
          inAttrs = testRoundTrip "with { val = 42; }; { result = val; }" { result = 42; };
          inConditional = testRoundTrip "with { test = true; }; if test then 1 else 2" 1;
          inApplication = testRoundTrip "with { f = x: x + 1; arg = 5; }; f arg" 6;
        };
        
        edgeCases = {
          emptyAttrs = testRoundTrip "with {}; 42" 42;
          withSelf = testRoundTrip "let env = { a = 1; }; in with env; a" 1;
          complexNesting = testRoundTrip "with { a = with { x = 1; }; x + 1; }; a" 2;
          nullValues = testRoundTrip "with { a = null; }; a" null;
          boolValues = testRoundTrip "with { flag = false; }; flag" false;
          listValues = testRoundTrip "with { items = []; }; items" [];
          attrValues = testRoundTrip "with { nested = {}; }; nested" {};
        };
        
        interaction = {
          withLet = testRoundTrip "with { a = 1; }; let b = 2; in a + b" 3;
          withAssert = testRoundTrip "with { valid = true; }; assert valid; 42" 42;
          withAssertFail = expectEvalError AssertError "with { valid = false; }; assert valid; 42";
          withAbort = expectEvalError Abort ''with { msg = "error"; }; abort msg'';
          withConditional = testRoundTrip "with { test = true; val = 42; }; if test then val else 0" 42;
        };

        withList = {
          # List needs to be evaluated as with-scope to check for x presence
          direct = expectEvalError UnknownIdentifierError "with [1 2 3]; x";
          # Okay; no usage of identifiers so list never evaluated as with-scope
          unforced = testRoundTrip "with {a=1;}; with []; 1" 1;
          # Okay; a is found so never need to evaluate the list as a with-scope
          # Matches Nix semantics
          unforcedFlipped = testRoundTrip "with []; with {a=1;}; a" 1;
          # Needs to evaluate the list as with-context to check for b presence
          searchedMissing = expectEvalError UnknownIdentifierError "with {a=1;}; with []; b";
        };

        withListShouldFail = skip {
          # Fails in Nix, passes here.
          # TODO: Need to maintain a stack of with-contexts
          innerWithListFails = expectEvalError UnknownIdentifierError "with {a=1;}; with []; a";
        };
      };

      # Import expressions - basic import tests
      _16_importExpressions = {
        # Import self test (simplified)
        # importSelf = testRoundTrip "let path = ./default.nix; in true" true;
        #importNixpkgs = testRoundTrip "(import <nixpkgs> {}).lib.isBool true" true;
        #importNixpkgsLib = testRoundTrip "(import <nixpkgs/lib>).isBool true" true;
        #importNixpkgsLibVersion =
        #  expect.eq 
        #    ((nix-reflect.eval "(import <nixpkgs/lib>).version").right)
        #    ("");
        paths = {
          nixpkgs = testRoundTrip "<nixpkgs>" <nixpkgs>;
          nixpkgsLib = testRoundTrip "<nixpkgs/lib>" <nixpkgs/lib>;
        };
      };

      # String interpolation tests
      _17_stringInterpolation = {
        basic = {
          simple = testRoundTrip ''"hello ''${toString 42}"'' "hello 42";
          multiple = testRoundTrip ''"''${toString 1} + ''${toString 2} = ''${toString 3}"'' "1 + 2 = 3";
          withSpaces = testRoundTrip ''"value: ''${ toString 42 }"'' "value: 42";
          emptyString = testRoundTrip ''"''${toString 0}"'' "0";
          onlyInterpolation = testRoundTrip ''''${toString 42}'' "42";
          quotesInside = testRoundTrip ''"value: ''${toString "quoted"}"'' "value: quoted";
          newlinesInside = testRoundTrip ''"text: ''${toString "line1\nline2"}"'' "text: line1\nline2";
          backslashInside = testRoundTrip ''"path: ''${toString "a\\b"}"'' "path: a\\b";
        };
        
        expressions = {
          arithmetic = testRoundTrip ''"result: ''${toString (1 + 2)}"'' "result: 3";
          comparison = testRoundTrip ''"test: ''${toString (1 == 1)}"'' "test: true";
          conditional = testRoundTrip ''"value: ''${toString (if true then 42 else 0)}"'' "value: 42";
          functionCall = testRoundTrip ''"doubled: ''${toString ((x: x * 2) 5)}"'' "doubled: 10";
          listAccess = testRoundTrip ''"first: ''${toString (builtins.head [1 2 3])}"'' "first: 1";
          attrAccess = testRoundTrip ''"value: ''${toString {a = 42;}.a}"'' "value: 42";
        };
        
        nested = {
          simple = testRoundTrip ''"outer ''${let x = "inner"; in x} end"'' "outer inner end";
          withInterpolation = testRoundTrip ''"a ''${"b ''${toString 1} c"} d"'' "a b 1 c d";
          deepNesting = testRoundTrip ''"''${toString (1 + (let x = 2; in x + 3))}"'' "6";
          multiLevel = testRoundTrip ''"''${"level1 ''${"level2"}"}"'' "level1 level2";
        };
        
        types = {
          strings = testRoundTrip ''"hello ''${"world"}"'' "hello world";
          integers = testRoundTrip ''"number: ''${toString 42}"'' "number: 42";
          floats = testRoundTrip ''"pi: ''${toString 3.14}"'' "pi: 3.14";
          booleans = testRoundTrip ''"flag: ''${toString true}"'' "flag: true";
          nullValue = testRoundTrip ''"null: ''${toString null}"'' "null: null";
          lists = testRoundTrip ''"list: ''${toString [1 2 3]}"'' "list: [ 1 2 3 ]";
        };
        
        escapingNormal = {
          dollarEscape = testRoundTrip ''"literal \''${not interpolated}"'' "literal \${not interpolated}";
          quoteEscape = testRoundTrip ''"literal \"quotes\""'' ''literal "quotes"'';
        };

        escapingIndent = {
          dollarEscape = testRoundTrip "''literal ''\${not interpolated}''" "literal \${not interpolated}";
          quoteEscape = testRoundTrip "''literal '''quotes'''''" "''literal ''quotes''";
        };
        
        contextual = {
          inLet = testRoundTrip ''let msg = "hello"; in "''${msg} world"'' "hello world";
          inFunction = testRoundTrip ''(name: "Hello ''${name}!") "Alice"'' "Hello Alice!";
          inList = testRoundTrip ''["''${toString 1}" "''${toString 2}"]'' ["1" "2"];
          inAttrs = testRoundTrip ''{msg = "value: ''${toString 42}";}.msg'' "value: 42";
          inConditional = testRoundTrip ''if true then "yes: ''${toString 1}" else "no"'' "yes: 1";
        };
        
        withScope = {
          localVariable = testRoundTrip ''let x = 42; in "value: ''${toString x}"'' "value: 42";
          computation = testRoundTrip ''let x = 1; y = 2; in "''${toString x} + ''${toString y} = ''${toString (x + y)}"'' "1 + 2 = 3";
          function = testRoundTrip ''let f = x: x * 2; in "doubled: ''${toString (f 5)}"'' "doubled: 10";
          recursive = testRoundTrip ''rec { msg = "value: ''${toString value}"; value = 42; }.msg'' "value: 42";
        };
        
        edgeCases = {
          emptyInterpolation = testRoundTrip ''"''${toString null}"'' "null";
          multipleEmpty = testRoundTrip ''"''${toString null}''${toString null}"'' "nullnull";
          onlyInterpolations = testRoundTrip ''''${toString 1}''${toString 2}'' "12";
          longString = testRoundTrip ''"very long string with ''${toString 42} in the middle"'' "very long string with 42 in the middle";
          manyInterpolations = testRoundTrip ''"''${toString 1}''${toString 2}''${toString 3}''${toString 4}"'' "1234";
          withIndentation = testRoundTrip "''value: \${toString 42}''" "value: 42";
        };
      };

      # Operator precedence and associativity tests
      _18_operatorPrecedence = {
        arithmetic = {
          additionMultiplication = testRoundTrip "1 + 2 * 3" 7;
          multiplicationAddition = testRoundTrip "2 * 3 + 1" 7;
          divisionAddition = testRoundTrip "8 / 2 + 1" 5;
          additionDivision = testRoundTrip "1 + 8 / 2" 5;
          subtraction = testRoundTrip "10 - 3 - 2" 5;
          division = testRoundTrip "24 / 4 / 2" 3;
          mixed = testRoundTrip "1 + 2 * 3 - 4 / 2" 5;
          parentheses = testRoundTrip "(1 + 2) * (3 - 1)" 6;
          deepNesting = testRoundTrip "((1 + 2) * 3) + ((4 - 1) * 2)" 15;
        };
        
        comparison = {
          arithmeticComparison = testRoundTrip "1 + 2 == 3" true;
          comparisonArithmetic = testRoundTrip "3 == 1 + 2" true;
          multiple = testRoundTrip "1 < 2 && 3 > 2" true;
          nested = testRoundTrip "(1 + 1) < (2 + 2)" true;
          withNegation = testRoundTrip "!(1 > 2)" true;
          chained = testRoundTrip "(1 < 2) && (2 < 3)" true;
        };
        
        logical = {
          andOr = testRoundTrip "true && false || true" true;
          orAnd = testRoundTrip "false || true && false" false;
          notAnd = testRoundTrip "!true && false" false;
          notOr = testRoundTrip "!false || false" true;
          parentheses = testRoundTrip "(true && false) || true" true;
          complex = testRoundTrip "true && (false || true) && true" true;
          withComparison = testRoundTrip "1 == 1 && 2 != 3" true;
          mixedTypes = testRoundTrip "(1 < 2) && (3 > 2) || false" true;
        };
        
        unary = {
          negationArithmetic = testRoundTrip "-1 + 2" 1;
          arithmeticNegation = testRoundTrip "1 + (-2)" (-1);
          notComparison = testRoundTrip "!(1 == 2)" true;
          doubleNegation = testRoundTrip "--1" 1;
          notNot = testRoundTrip "!!true" true;
          mixed = testRoundTrip "!false && (-1) < 0" true;
          parentheses = testRoundTrip "-(1 + 2)" (-3);
          complex = testRoundTrip "!(-1 > 0)" true;
        };
        
        attributeAccess = {
          beforeArithmetic = testRoundTrip "{a = 2;}.a * 3" 6;
          afterArithmetic = testRoundTrip "2 * {a = 3;}.a" 6;
          withComparison = testRoundTrip "{a = 1;}.a == 1" true;
          nested = testRoundTrip "{a = {b = 2;};}.a.b + 1" 3;
          withFunction = testRoundTrip "{f = x: x + 1;}.f 5" 6;
          chained = testRoundTrip "{a = {b = {c = 42;};};}.a.b.c" 42;
          withOr = testRoundTrip "{a = 1;}.b or 2" 2;
          complexOr = testRoundTrip "{a = 1;}.b or {c = 2;}.c" 2;
        };
        
        application = {
          beforeArithmetic = testRoundTrip "(x: x + 1) 2 * 3" 9;
          afterArithmetic = testRoundTrip "2 * (x: x + 1) 3" 8;
          nested = testRoundTrip "(x: y: x + y) 1 2 + 3" 6;
          withComparison = testRoundTrip "(x: x > 0) 5 && true" true;
          complex = testRoundTrip "(f: x: f (f x)) (y: y + 1) 0" 2;
        };
        
        listOperations = {
          concat = testRoundTrip "[1] ++ [2] ++ [3]" [1 2 3];
          concatArithmetic = testRoundTrip "[(1 + 1)] ++ [(2 * 2)]" [2 4];
          concatComparison = testRoundTrip "[(1 == 1)] ++ [(2 != 3)]" [true true];
          nested = testRoundTrip "[([1] ++ [2])] ++ [[3]]" [[1 2] [3]];
        };
        
        attrOperations = {
          merge = testRoundTrip "{a = 1;} // {b = 2;} // {c = 3;}" {a = 1; b = 2; c = 3;};
          mergeArithmetic = testRoundTrip "{a = 1 + 1;} // {b = 2 * 2;}" {a = 2; b = 4;};
          mergeComparison = testRoundTrip "{a = 1 == 1;} // {b = 2 != 3;}" {a = true; b = true;};
          precedence = expectEvalError TypeError "{a = 1;} // {b = 2;}.b == 2";
        };
        
        comprehensive = {
          everything = testRoundTrip "!false && 1 + 2 * 3 == 7 || [1] ++ [2] == [1 2]" true;
          withAttrs = testRoundTrip "let x = {a = 1; b = 2;}; in x.a + x.b * 2 == 5" true;
          withFunctions = testRoundTrip "(x: y: x + y * 2) 1 3 == 7" true;
          complex = testRoundTrip "let f = x: x * 2; attrs = {val = 3;}; in f attrs.val + 1 == 7" true;
          deepNesting = testRoundTrip "((1 + 2) * (3 + 4)) == ((2 * 3) + (4 * 5) - 3)" false;
        };
      };

      # Complex expressions demonstrating code transformations
      _19_transformations = let
        # Example: transform "1 + 2" to "2 + 1" (commutativity)
        original = parse "1 + 2";
        transformed = original.mapNode (node: with node; { 
          op = "-";
          lhs = rhs;
          rhs = lhs;
        });
      in {
        original = testRoundTrip original 3;
        transformed = testRoundTrip transformed 1;
      };

      # Self-eval test (skipped due to slow parsing of large file)
      _20_selfParsing = skip {
        parseParserFile =
          expect.True ((evalAST (builtins.readFile ./ast.nix)) ? right);
      };

      _20_caching = {
        putGetMany =
          let xsNode = parse "{x = 1;}";
              getX = xs: {_, ...}: _.do
                {xs' = force xs;}
                ({xs', _}: _.pure xs'.x);
          in expectRun {
            actual =
              Eval.do
                {xs = evalNodeM xsNode;}
                ({xs, _}: _.traverse getX [xs xs xs]);
            expected = [1 1 1];
            expectedThunkCache = ThunkCache {
              thunks = {
                "0" = CODE 0 "attrs";
                "1" = CODE 1 "int";
              };
              values = {
                "0" = { x = CODE 1 "int"; };
                "1" = 1;
              };
              misses = 2;
              hits = 4;
              nextId = 2;
            };
          };
      };

      _21_scopeLeak = {
        simpleLambdaArg = expectEvalError UnknownIdentifierError "[((a: a) 1) a]";
        #attrsLambdaArg.default = expectEvalError UnknownIdentifierError "[(({a ? 1, b}: a + b) {b = 2;}) a]";
        #attrsLambdaArg.required = expectEvalError UnknownIdentifierError "[(({a ? 1, b}: a + b) {b = 2;}) b]";
        containedLet = expectEvalError UnknownIdentifierError "[(let x = 1; in x) x]";
        containedWith = expectEvalError UnknownIdentifierError "[(with {x=1;}; x) x]";
        recAttrs = expectEvalError UnknownIdentifierError "(rec { a = 1; b = a; }).b + a";
      };


    };

  };
}

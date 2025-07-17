# evalAST Implementation - Round-Trip AST Evaluation

This document demonstrates the implementation of `evalAST` function in `parser/default.nix` that enables round-trip conversion between Nix expressions and their AST representations.

## Overview

The `evalAST` function allows you to:
1. Parse Nix code into an AST: `parseAST "1 + 2"`
2. Evaluate AST back to Nix values: `evalAST (ast.int 1)` → `1`
3. Achieve round-trip property: `evalAST (parseAST x) == x`

## Key Features

### Basic Literals
```nix
# These expressions demonstrate the round-trip property:
evalAST (ast.int 42) == 42
evalAST (ast.string "hello") == "hello"
evalAST (ast.bool true) == true
evalAST (ast.null) == null
evalAST (ast.float 3.14) == 3.14
```

### Collections
```nix
# Lists
evalAST (ast.list [ast.int 1, ast.int 2, ast.int 3]) == [1 2 3]
evalAST (parseAST "[1 2 3]") == [1 2 3]

# Attribute sets
evalAST (ast.attrs [
  (ast.assignment (ast.identifier "a") (ast.int 1))
  (ast.assignment (ast.identifier "b") (ast.int 2))
] false) == { a = 1; b = 2; }

evalAST (parseAST "{ a = 1; b = 2; }") == { a = 1; b = 2; }
```

### Binary Operations
```nix
# Arithmetic
evalAST (ast.binaryOp "+" (ast.int 1) (ast.int 2)) == 3
evalAST (parseAST "1 + 2") == 3

# Logical
evalAST (ast.binaryOp "&&" (ast.bool true) (ast.bool false)) == false
evalAST (parseAST "true && false") == false

# Comparison
evalAST (ast.binaryOp "==" (ast.int 1) (ast.int 1)) == true
evalAST (parseAST "1 == 1") == true
```

### Control Flow
```nix
# Conditionals
evalAST (ast.conditional (ast.bool true) (ast.int 1) (ast.int 2)) == 1
evalAST (parseAST "if true then 1 else 2") == 1

# Let expressions
evalAST (ast.letIn 
  [(ast.assignment (ast.identifier "x") (ast.int 42))]
  (ast.identifier "x")) == 42
evalAST (parseAST "let x = 42; in x") == 42
```

### Functions
```nix
# Lambda creation and application
let f = evalAST (ast.lambda (ast.simpleParam "x") (ast.identifier "x"));
in f 42 == 42

# Parse and evaluate function application
evalAST (parseAST "let f = x: x; in f 42") == 42
```

## Code Transformation Examples

The round-trip capability enables powerful code transformations:

### Commutativity Transformation
```nix
# Original: "1 + 2"
let original = parseAST "1 + 2";
    # Transform to "2 + 1" 
    transformed = ast.binaryOp "+" original.root.right original.root.left;
in evalAST original == evalAST transformed  # Both equal 3
```

### AST Manipulation
```nix
# Create AST directly
let directAST = ast.binaryOp "*" (ast.int 6) (ast.int 7);
    parsedAST = parseAST "6 * 7";
in evalAST directAST == evalAST parsedAST == 42
```

### Expression Optimization
```nix
# Replace "0 + x" with "x"
optimizeAddZero = expr:
  if expr.nodeType == "binaryOp" && expr.op == "+" 
     && expr.left.nodeType == "int" && expr.left.value == 0
  then expr.right
  else expr;

# Apply optimization and verify equivalence
let original = parseAST "0 + 42";
    optimized = optimizeAddZero original.root;
in evalAST original == evalAST optimized == 42
```

## Implementation Details

### Supported AST Node Types
- **Literals**: `int`, `float`, `string`, `indentString`, `path`, `bool`, `null`
- **Collections**: `list`, `attrs` (including recursive)
- **Operations**: `binaryOp`, `unaryOp`
- **Control Flow**: `conditional`, `letIn`
- **Functions**: `lambda`, `application`
- **Access**: `select` (attribute access)

### Scoping and Context
The implementation handles:
- Variable binding in `let` expressions
- Function parameter scoping
- Recursive attribute sets using `lib.fix`
- Default values in attribute access

### Limitations
- Complex attribute paths require simplification
- Identifier evaluation requires scope context
- Some advanced Nix features (with, assert, inherit) have simplified implementations

## Testing

The implementation includes comprehensive tests covering:
- Round-trip property for all supported constructs
- AST manipulation and transformation
- Edge cases and error conditions

```nix
# Example test
testRoundTrip = expr: expected: {
  ast = expect.eq (parseAST expr).root.nodeType (expected.nodeType or "unknown");
  roundTrip = expect.eq (evalAST (parseAST expr)) expected;
};

# Usage
testRoundTrip "1 + 2" 3  # Verifies parsing and evaluation
```

## Use Cases

This implementation enables:
1. **Code Analysis**: Parse code, analyze AST structure, make decisions
2. **Code Generation**: Create AST programmatically, convert to executable code
3. **Code Transformation**: Parse → Transform → Evaluate for optimizations
4. **Metaprogramming**: Write Nix code that manipulates Nix code
5. **Development Tools**: Formatters, linters, refactoring tools

The round-trip property `evalAST (parseAST x) == x` ensures that transformations preserve semantic meaning while allowing syntactic modifications.
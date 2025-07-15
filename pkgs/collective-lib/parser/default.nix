{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, collective-lib ? import ./. { inherit lib; },
  nix-parsec,
  ...
}:

# TODO
# - complete parser:
#   - floats paths nulls bools lists
#   - lambdas
#   - default arguments
#   - .fieldaccess
#   - let/in
#   - inherits
#   - assert, abort, other builtins
# -
# - retain position information

let
  eval = collective-lib.eval;
  typed = collective-lib.typed;
  parsec = nix-parsec.parsec;
  lexer = nix-parsec.lexer;
in 
  with typed;
rec {

  node = nodeType: args: {
    nodeType = nodeType;
  } // args;

  ast = {
    assignment = lhsRhs:
      node "assignment" {
        identifier = lhsRhs.lhs;
        value = lhsRhs.rhs;
      };

    attrs = assignments:
      node "attrs" {
        inherit assignments;
      };

    identifier = name:
      node "identifier" {
        inherit name;
      };

    indentString = s:
      node "indentString" {
        inherit s;
      };

    normalString = s:
      node "normalString" {
        inherit s;
      };

    int = i:
      node "int" {
        inherit i;
      };
  };

  p = with parsec; rec {
    ws = matching "[ \t\n]+";
    lineComment = lexer.skipLineComment "#";
    blockComment = lexer.skipBlockComment "/*" "*/";
    spaces = lexer.space ws lineComment blockComment;
    spaced = x: skipThen spaces (thenSkip x spaces);
    lex = lexer.lexeme spaces;
    sym = lexer.symbol spaces;

    semi = spaced (sym ";");

    infix2 = lhs: opString: rhs:
      bind (thenSkip lhs (spaced (sym opString)))
      (lhs: bind rhs 
      (rhs: pure { inherit lhs rhs; }));

    identifier = fmap (matches: ast.identifier (head matches))(matching ''[a-zA-Z_][a-zA-Z0-9_\-\.]*'');

    assignment =
      let lhs = choice [
        identifier
        anyString
        interpolation
      ];
      in spaced (fmap ast.assignment (infix2 lhs "=" expr));

    assignments = many (thenSkip assignment semi);

    attrs = spaced (fmap ast.attrs (between (sym "{") (sym "}") assignments));

    int = fmap ast.int lexer.decimal;
    signedInt = fmap ast.int (lexer.signed spaces lexer.decimal);
    normalString = fmap ast.normalString lexer.stringLit;
    # TODO: indent strings require harder stopping logic
    indentString = fmap ast.indentString (between (string "''") (string "''") (fmap join (many (anyCharBut "'"))));
    interpolation = between (string "\${") (string "}") expr;
    anyString = choice [
      normalString
      indentString
    ];
    value = choice [
      int
      signedInt
      anyString
    ];


    expr = spaced (choice [
      value
      attrs
    ]);
  };

  parse = s: parsec.runParser p.expr s;

  read = rec {

    fileFromAttrPath = attrPath: file: args:
      let expr = import file args;
          pos = typed.pathPos attrPath expr;
      in fileFrom pos.line pos.column file;

    fileFrom = line: column: path:
      let src = builtins.readFile path;
          srcLines = typed.splitLines src;
          srcLinesFrom = lib.drop (line - 1) srcLines;
          firstLine = lib.head srcLinesFrom;
          firstLineFrom = builtins.substring (column - 1) (size firstLine) firstLine;
          srcLinesFromColumn = [firstLineFrom] ++ (lib.tail srcLinesFrom);
          srcFrom = typed.joinLines srcLinesFromColumn;
      in srcFrom;
  };

  # <nix>parser._tests.run {}</nix>
  _tests = with typed.tests; suite {
    parser = 
      with parsec; 
      let expectSuccess = p: s: v: expect.eq (runParser p s) { type = "success"; value = v; };
          expectError = p: s: expect.eq (runParser p s).type "error";
      in {
        space = {
          lineComment = expectSuccess
            p.spaces "# a comment" null;
          blockComment = expectSuccess
            p.spaces ''/* a 
            comment
            */'' null;
          spaces = expectSuccess
            p.spaces "   \t \n" null;
        };

        identifier = {
          normal = expectSuccess p.identifier "a" (ast.identifier "a");
          startUnderscore = expectSuccess p.identifier "_a" (ast.identifier "_a");
          numbersLater = expectSuccess p.identifier "_a0" (ast.identifier "_a0");
          startNumber = expectError p.identifier "1a";
        };

        value = {
          int = expectSuccess p.value "1" (ast.int 1);
          normalString = expectSuccess p.value ''"a"'' (ast.normalString "a");
          indentString = expectSuccess p.value "''a''" (ast.indentString "a");
        };

        attrs = {
          empty = expectSuccess p.attrs "{}" (ast.attrs []);
          spacedEmpty = expectSuccess p.attrs "  \t \n {  \t\n }  \t \n" (ast.attrs []);
          surroundSpacedEmpty = expectSuccess p.attrs "  \t \n {  \t\n }  \t \n" (ast.attrs []);
          singleAssignment = 
            expectSuccess p.attrs "{ a = 1; }"
            (ast.attrs [ (ast.assignment { lhs = ast.identifier "a"; rhs = ast.int 1; }) ]);
          singleAssignmentWithComment =
            expectSuccess p.attrs ''
            {
              a = 1;  #a comment
            }
            ''
            (ast.attrs [ (ast.assignment { lhs = ast.identifier "a"; rhs = ast.int 1; }) ]);
        };
      };

    read = {
      fileFromAttrPath =
        expect.eq 
          (read.fileFromAttrPath  [ "__testData" "deeper" "anExpr" ] ./default.nix { inherit pkgs lib collective-lib nix-parsec; })
          ((_b_ ''
anExpr = (((with {a = 1;}; assert true; x:
      y:
      const ( ( (
      z: let
        d = 3;
        e = 4;  # a comment
        in x + y + a +
           z + d + (if !!!(e > 0)
           then length [1 2 3]
           else length '''''${toString 123}"ok
           "{]}[()]())))[[[''')))))) 1 2 3);
    };
  };

}
'') + "\n");
    };
  };

  # DO NOT MOVE
  __testData = {
    deeper = {
      anExpr = (((with {a = 1;}; assert true; x:
      y:
      const ( ( (
      z: let
        d = 3;
        e = 4;  # a comment
        in x + y + a +
           z + d + (if !!!(e > 0)
           then length [1 2 3]
           else length ''${toString 123}"ok
           "{]}[()]())))[[['')))))) 1 2 3);
    };
  };

}

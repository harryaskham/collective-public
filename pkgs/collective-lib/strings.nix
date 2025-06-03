{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with lib.strings;
with cutils.lists;
with cutils.functions;

# String formatting and indentation utilities.
# Allows for multi-line indentation in indented strings where
# otherwise if
# x = ''
# some
#   string
# '';
# y = ''
#   def f() {
#     ${x}
#   }
# '';
#
# then
#
# y == ''
#   def f() {
#     some
# string
#   }
# ''
#
# instead:
#
# z = indent.block ''
#   def f() {
#     ${indent.here x}
#   }
# '';
# z == ''def f() {
#     some
#       string
#   }'';
let
  log = cutils.log;
in rec {
  # Join a list of strings into a string with a separator.
  # Just shorthand for concatStringsSep.
  joinSep = concatStringsSep;

  # Join a list of strings with no separator.
  join = joinSep "";

  # Split a string to a list of strings by a separator.
  # Just shorthand for splitString.
  splitSep = splitString;

  # Split a string by spaces
  splitWords = splitSep " ";

  # Split a string into lines.
  splitLines = splitSep "\n";

  # Split a string according to any whitespace character.
  # e.g. "hell   o \n\n  \t \n wor\t\tld" -> [ "hell" "o" "wor" "ld"]
  splitWhitespace = s:
    let s_ = replaceStrings [ "\n" "\t" "\r" ] [ " " " " " " ] s;
        words = splitWords s_;
    in nonEmpties words;

  # Join a list of lines into a string.
  joinLines = joinSep "\n";

  # Join a list of words into a string.
  joinWords = joinSep " ";

  lineCount = s: length (splitLines s);
  wordCount = s: length (splitWhitespace s);
  isWhitespace = s: wordCount s == 0;

  # Retain only the non-null/emptystrings in a list.
  nonEmpties = filter (x: x != null && x != "");

  # Join a list of strings into a string with given separator, discarding null / empty strings.
  joinOptionalsSep = sep: xs:
    let nonEmpty = filter (x: x != null && x != "") xs;
    in joinSep sep nonEmpty;

  # Join a list of strings into a string, ignoring null / empty lines, with no separator.
  joinOptionals = joinOptionalsSep "\n";

  # Join a list of lines into a string, ignoring null / empty lines.
  joinOptionalLines = joinOptionalsSep "\n";

  # Join a list of words into a string, ignoring null / empty words.
  joinOptionalWords = joinOptionalsSep " ";

  # Join a list of statements into a semicolon-space separated string.
  joinStatements = joinOptionalsSep "; ";

  # Join a list of statements into a semicolon-escapednewline separated string.
  joinStatementLines = joinOptionalsSep "; \\\n";

  # Add a prefix to a string.
  addPrefix = prefix: s: prefix + s;

  # A string of n spaces.
  spaces = n: replicate n " ";

  # Map a function over the lines in a string.
  mapLines = f: s: map f (splitLines s);

  # Map a function over the lines in a string and concat the result
  concatMapLines = f: s: joinLines (mapLines f s);

  # Left-fold a function over the lines in a string.
  foldlLines = f: initial: s: foldl f initial (splitLines s);

  # Right-fold a function over the lines in a string.
  foldrLines = f: initial: s: foldr f initial (splitLines s);

  # Get the first regex match or a default value
  firstMatchOr = def: regex: s:
    let res = match regex s;
     in if (res == null) then def else (head res);

  # Get the first regex match or null if none.
  maybeFirstMatch = firstMatchOr null;

  # Get the number of regex matches.
  numMatches = regex: s:
    let res = match regex s;
     in if (res == null) then 0 else length res;

  # Regex match as a bool function
  # Iff we have any matches, even if no capture groups, return true
  matches = regex: s: match regex s != null;

  # Get the space indentation length of a block as its minimum line indentation
  getIndent =
    let regex = ''^([ ]*).*$'';
        getLineIndent = line: stringLength (firstMatchOr "" regex line);
    in s: minimum (mapLines getLineIndent s);

  # Indent a string by n spaces, or dedent if n is negative
  indentBy = n:
    let prefix = spaces n;
    in concatMapLines (
      if (n < 0) then removePrefix (spaces (-1 * n))
      else if (n > 0) then addPrefix (spaces n)
      else id);

  # Indent or dedent a multi-line string s.t. its leftmost line aligns with char n
  setIndent = n: s: indentBy (n - getIndent s) s;

  # Indent a multi-line string s.t. it starts with n tabs
  setTabIndent = n: s:
    concatMapLines
      (l: ''${replicate n "\t"}l'')
      (setIndent 0 s);

  # Remove the surrounding newlines from a string
  trimNewlines =
    let chars = "\r\n";
        regex = "[${chars}]*(.*[^${chars}])[${chars}]*";
    in s: firstMatchOr "" regex s;

  # Remove the surrounding newlines from a list of lines
  trimNewlinesList = lines: splitLines (trimNewlines (joinLines lines));

  # Format a block trimmed and zero-indented for later re-indentation
  # Handles any markers left in the text by embedded to-indent blocks.
  codeBlock = s: indent.set 0 (indent.markers.handle (trimNewlines s));

  # Format a list of lines as a code block, trimmed and zero-indented for later re-indentation
  # Trims newlines from the list before handling indentation on the combined block
  # to avoid failing to trim lines that now contain only an indent.
  # Handles any markers left in the text by embedded to-indent blocks.
  codeBlockLines = codeBlockLinesSep "\n";
  codeBlockLinesSep = sep: lines: setIndent 0 (indent.markers.handle (joinOptionalsSep sep (trimNewlinesList lines)));

  # Combine a list of code blocks with a separator
  codeBlocksSep = sep: blocks: joinOptionalsSep sep (map codeBlock blocks);

  # Combine a list of code blocks
  codeBlocks = codeBlocksSep "\n\n";

  # Format a block trimmed and zero-indented for later re-indentation.
  # If the block is non-empty, includes the given header block at the same indentation.
  codeBlockHeader = header: s:
    codeBlock
      (maybePrefixString
        "${trimNewlines header}\n"
        (trimNewlines s));

  # Format a list of lines as a code block, trimmed and zero-indented for later re-indentation.
  # If the block is non-empty, includes the given header block at the same indentation.
  codeBlockLinesHeader = headerLines: lines:
    codeBlockLines
      (maybePrefixList
        (trimNewlinesList headerLines)
        (trimNewlinesList lines));

  # Indent an inlined multiline string to match its indented point
  # The first line is already indented so we can use this to find the required indentation
  # However we can't know it within this call, so we need a two-pass approach
  # This pass adds a special marker string to each line:
  #
  # s = codeBlock ''
  #   line1
  #   myFn() {
  #     line2
  #   }
  # '';
  #
  # t = ''
  #   someFn() {
  #     ${indent.here s}
  #   };
  # ''
  #
  # tCode = codeBlock t;
  #
  # Here 's' is first indented to 0 by use of codeBlock.
  # We need 'line1' to receive no extra indent, as it will appear at the point of interpolation
  # 'myFn()' and all subsequent lines will require an indentation of 4
  # Before the application of 'codeBlock', 't' becomes:
  #
  # t == ''
  #   someFn() {
  #     __CUTILS_START_INDENT__
  # line1
  # myFn() {
  #   line2
  # }
  # __CUTILS_END_INDENT__
  #   }
  # '';
  #
  # codeBlock first calls handleIndentMarkers which passes through the string and
  # upon seeing __CUTILS_START_INDENT__ will set indentation to the indentation of
  # the start marker until the __CUTILS_END_INDENT_ marker is reached, removing
  # both markers. The indentation added will match whatever occurs on the line of
  # the embed, whether this is spaces, tabs or a mixture.
  #
  # Finally this gives:
  #
  # tCode == ''someFn() {
  #   line1
  #   myFn() {
  #     line2
  #   }
  # }'';
  indent = rec {
    # Interface to indent that throws its result
    throws = mapAttrs (k: f: Variadic.compose (x: throw x) f) indent;
    # Shorthand for printing in blocks
    print = log.print;
    print' = log.print';
    print_ = log.print_;
    vprint = log.vprint;
    markers = {
      start = ''__CUTILS_START_INDENT__'';
      end = ''__CUTILS_END_INDENT__'';
      regex = {
        start = ''^(.*)${markers.start}(.*)$'';
        end = ''^(.*)${markers.end}(.*)$'';
        both = ''^(.*)${markers.start}(.*)${markers.end}(.*)$'';
      };
      handle = s:
        let
          # Initial linefold state
          init = { lines = []; indentFn = id; };

          # Function to handle each line in the fold
          handleLine = (acc @ {lines, indentFn}: line:
            let startMatch = match markers.regex.start line;
                isStart = startMatch != null;
                endMatch = match markers.regex.end line;
                isEnd = endMatch != null;
                bothMatch = match markers.regex.both line;
                isBoth = bothMatch != null;
            in
              # If we have both markers on a line we don't need to update state but do need
              # to take the middle and add the prefix/suffix.
              if isBoth then
                let
                  prefix = elemAt bothMatch 0;
                  line_ = elemAt bothMatch 1;
                  suffix = elemAt bothMatch 2;
                in acc // {lines = acc.lines ++ ["${prefix}${line_}${suffix}"];}
              else if isStart then
                # Retain any content either side of the marker, which is already in the right place.
                # The LHS of the marker is used to get the indentation level for the block.
                let
                  prefix = elemAt startMatch 0;
                  line_ = elemAt startMatch 1;
                in acc // {
                  indentFn = indent.by (stringLength prefix);
                  lines = acc.lines ++ ["${prefix}${line_}"];
                }
              else if isEnd then
                # Keep any content that occurred after the marker.
                # Indent this line, but reset indentation thereafter.
                let
                  line_ = elemAt endMatch 0;
                  suffix = elemAt endMatch 1;
                in
                  acc // {
                    indentFn = id;
                    lines = acc.lines ++ [(indentFn "${line_}${suffix}")];
                  }
              else
                # Otherwise just indent the line according to current marker
                acc // { lines = acc.lines ++ [(indentFn line)]; });

        in joinLines (trimNewlinesList (foldlLines handleLine init s).lines);
    };
    here = s: "${markers.start}${indent.set 0 s}${markers.end}";

    # Consistent API
    get = getIndent;
    set = setIndent;
    tab = {
      set = setTabIndent;
    };
    by = indentBy;
    block = codeBlock;
    blocks = codeBlocks;
    blocksSep = codeBlocksSep;
    lines = codeBlockLines;
    linesSep = codeBlockLinesSep;
    linesHeader = codeBlockLinesHeader;

    # Indented block affecting only the tail, with an f that operates on a block.
    tail = s: f:
      let snoc = maybeSnoc (splitLines s);
      in if snoc == null then ""
         else if snoc.tail == null then snoc.head
         else lines [snoc.head (f snoc.tail)];

    # Indented block affecting only the tail, with an f that operates per-line.
    tailLines = ls: f:
      let snoc = maybeSnoc ls;
      in if snoc == null then ""
         else if snoc.tail == null then snoc.head
         else lines ([snoc.head] ++ (map f snoc.tail));
  };

  # Adds a prefix to a string if the string is not empty
  maybePrefixString = prefix: s: if s == "" then "" else prefix + s;

  # Adds a prefix list to a list if the list is not empty
  maybePrefixList = prefix: list: if (list == []) then [] else prefix ++ list;

  # Adds a line to a list if the list is not empty
  maybePrefixLines = prefix: maybePrefixList [prefix];

  # Escape doublequotes in a string and wrap in doublequotes
  # Like strings.escapeShellArg but allowing for $VAR interpolation
  shellQuote = arg:
    let
      string = toString arg;
      mustQuote = match "[[:alnum:],._+:@%/-]+" string == null;
    in
      if mustQuote
      then ''"${replaceStrings [ ''"'' ] [ ''\"'' ] string}"''
      else string;

  # Convert a value to a shell value
  # Handles each type ("int", "bool", "string", "path", "null", "set", "list", "lambda", "float")
  #
  # strings without special chars are left unchanged:
  # toShellValue "some_safe-string" == "some_safe-$string"
  # toShellValue "some_$VAR" == ''\"some_$VAR\""
  #
  # strings are quoted with double quotes if they contain special characters:
  # toShellValue ''"quote"'' == ''"\"quote\""''
  # toShellValue "'safe quote'" == "'safe quote'"
  #
  # bool goes to string literal, not 0/1:
  # toShellValue true == "true"
  # toShellValue false == "false"
  #
  # lists go to array literals:
  # toShellValue [1 "xxx" "$YYY" true] == "(1 xxx \"$YYY\" true)"
  #
  # Numbers use toString:
  # toShellValue 1 == "1"
  # toShellValue 123.3 == "123.300000"
  toShellValue = arg:
    let convert = {
      int = toString;
      bool = b: if b then "true" else "false";
      string = shellQuote;
      path = shellQuote;
      null = "";
      set = throw "Cannot convert set to shell value";
      list = xs: ''(${joinSep " " (map toShellValue xs)})'';
      lambda = throw "Cannot convert lambda to shell value";
      float = toString;
    };
    in convert.${typeOf arg} arg;

  # nix eval --impure --expr '(import ./cutils/strings.nix {})._tests'
  _tests =
    cutils.tests.suite {
      strings = {
        split = {
          whitespace = {
            expr = splitWhitespace " \n hell   o \n\n  \t \n wor\t\tld  \n";
            expected = [ "hell" "o" "wor" "ld" ];
          };
        };

        indent = {
          set0 = {
            expr = indent.set 0 (joinLines [
              "            line1"
              "            def fn() {"
              "              line2"
              "            }"
            ]);
            expected = joinLines [
              "line1"
              "def fn() {"
              "  line2"
              "}"
            ];
          };
          set4 = {
            expr = indent.set 4 (joinLines [
              "            line1"
              "            def fn() {"
              "              line2"
              "            }"
            ]);
            expected = joinLines [
              "    line1"
              "    def fn() {"
              "      line2"
              "    }"
            ];
          };
          get = {
            expr = indent.get (joinLines [
              "            line1"
              "            def fn() {"
              "              line2"
              "            }"
            ]);
            expected = 12;
          };
        };

        codeBlock = {
          embedded = {
            expr =
              let body = codeBlock ''
                embed1:
                  embed2
              '';
              in codeBlock ''
                def fn() {
                  ${indent.here body}
                }
              '';
            expected = trimNewlines ''
              def fn() {
                embed1:
                  embed2
              }
            '';
          };
          inline = {
            expr =
              let body = codeBlock ''
                1
                2
                3
              '';
              in codeBlock ''
                X = [ ${indent.here body} ]
              '';
            expected = trimNewlines ''
              X = [ 1
                    2
                    3 ]
            '';
          };
        };
      };
    };
}

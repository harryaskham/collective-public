{ lib, collective-lib,
  overrideToShellValue ? null,
  ... }:

let
  toShellValue =
    if overrideToShellValue != null
    then overrideToShellValue
    else collective-lib.typelib.toShellValueUnsafe;
in

with collective-lib.collections;
with collective-lib.dispatchlib;
with collective-lib.syntax;
with lib;
with lib.strings;
with collective-lib.lists;
with collective-lib.strings;
with collective-lib.functions;

let
  log = collective-lib.log;
  typed = collective-lib.typed;
in rec {
  # Utilities for echoing styled text
  ansi = rec {
    fg = rec {
      black = "30";
      red = "31";
      green = "32";
      yellow = "33";
      blue = "34";
      magenta = "35";
      cyan = "36";
      white = "37";
      default = "39";
      brightblack = "90";
      brightred = "91";
      brightgreen = "92";
      brightyellow = "93";
      brightblue = "94";
      brightmagenta = "95";
      brightcyan = "96";
      brightwhite = "97";
      grey = brightblack;
    };
    bg = rec {
      black = "40";
      red = "41";
      green = "42";
      yellow = "43";
      blue = "44";
      magenta = "45";
      cyan = "46";
      white = "47";
      default = "49";
      brightblack = "100";
      brightred = "101";
      brightgreen = "102";
      brightyellow = "103";
      brightblue = "104";
      brightmagenta = "105";
      brightcyan = "106";
      brightwhite = "107";
      grey = brightblack;
    };
    bold = "1";
    faint = "2";
    italic = "3";
    underline = "4";
    reset = "0";
    code = styles: if (styles == []) then "" else ''\e[${concatStringsSep ";" styles}m'';
    end = code [reset reset];
    style = styles: text:
      if styles == [] then text
      else (Strings_ {w = width text;} [(code styles) text end]);
    style_ = styles: text:
      if styles == [] then text
      else (Strings_ {w = width text;} [(code styles) text]);
    style' = styles: text: (Strings_ {w = width text;} [end (style styles text)]);

    escapeANSI = s: 
      if isString s then
        replaceStrings ["\\e["] ["\\\\e["] (toString s)
      else if isStrings s then
        toString (s.mapPieces escapeANSI)
      else throw "Invalid argument to escapeANSI: ${typeOf s} (expected String or Strings)";

    stripANSI = text:
      if isString text then
        let 
          c = ''[^\\]*(\\e\[[0-9;]*[0-9]m).*'';
          r = match c (toString text);
        in 
          if r == null then text
          else stripANSI (replaceStrings (drop 0 r) [""] (toString text))
      else if isStrings text then
        toString (text.mapPieces stripANSI)
      else throw "Invalid argument to stripANSI: ${typeOf text} (expected String or Strings)";

    boxes = {
      single = {
        vLine = Char "│";
        hLine = Char "─";
        hMidLine = Char "─";
        kneeSW = Char "└";
        kneeSE = Char "┘";
        kneeNW = Char "┌";
        kneeNE = Char "┐";
        teeL = Char "├";
        teeR = Char "┤";
      };
      heavy = {
        vLine = Char "┃";
        hLine = Char "━";
        hMidLine = Char "━";
        kneeSW = Char "┗";
        kneeSE = Char "┛";
        kneeNW = Char "┏";
        kneeNE = Char "┓";
        teeL = Char "┣";
        teeR = Char "┫";
      };
      double = {
        vLine = Char "║";
        hLine = Char "═";
        hMidLine = Char "─";
        kneeSW = Char "╚";
        kneeSE = Char "╝";
        kneeNW = Char "╔";
        kneeNE = Char "╗";
        teeL = Char "╠";
        teeR = Char "╣";
      };
    };

    zeros = {
      top = 0;
      bottom = 0;
      left = 0;
      right = 0;
    };

    ones = {
      top = 1;
      bottom = 1;
      left = 1;
      right = 1;
    };

    box = { 
      styles ? [fg.brightwhite],
      borderStyles ? [fg.brightblack],
      header ? null,
      border ? boxes.heavy,
      showBorder ? true,
      showDivider ? true,
      padding ? {
        top = 1;
        bottom = if showBorder then 1 else 0;
        left = 1;
        right = 1;
      },
      margin ? {
        top = 1;
        bottom = 0;
        left = 1;
        right = 1;
      },
      # Body can be string or list of HasWidth
      body ? "",
      align ? "left"
    }: 
      let
        headerBlock = flattenToStrings1 header;
        bodyBlock = flattenToStrings1 body;
        mkOuterBlock = s: Lines (s.mapLines mkLine);
        outerHeaderBlock =
          if header == null
          then Strings []
          else Lines 
            ([(mkOuterBlock headerBlock)]
            ++ (optionals showDivider [midBorder]));
        outerBodyBlock = mkOuterBlock bodyBlock;

        # Explicit width calculations needed for chars that are >1 unicode codepoint wide.
        # This trusts that body and header correctly report their widths and break if not:
        # - Will over-pad or under-pad, moving the right border out of place.
        contentWidth = maximum [ (if header == null then 0 else width headerBlock) (width bodyBlock) ];
        innerWidth = contentWidth + padding.left + padding.right;
        borderedWidth = innerWidth + 2;
        outerWidth = borderedWidth + margin.left + margin.right;

        vBorder = style' (styles ++ borderStyles) (if showBorder then border.vLine else " ");
        hBorder = hLine: kneeW: kneeE:
          style' (styles ++ borderStyles) (Join ([kneeW] ++ (lib.lists.replicate innerWidth hLine) ++ [kneeE]));
        hBorderTop = 
          if showBorder then hBorder border.hLine border.kneeNW border.kneeNE
          else hBorder " " " " " ";
        hBorderMid =
          if showBorder then hBorder border.hMidLine border.teeL border.teeR
          else hBorder " " " " " ";
        hBorderBottom =
          if showBorder then hBorder border.hLine border.kneeSW border.kneeSE
          else hBorder " " " " " ";
        lineLeft = Join [leftMargin vBorder leftPadding];
        lineRight = Join [rightPadding vBorder rightMargin];

        #mkLine = s: Strings_ {w = outerWidth;} [
        mkLine = s: Strings_ {w = outerWidth;} (Join [
          lineLeft
          (Strings_ {w = contentWidth;} [(style' styles (pad { to = contentWidth; utf8 = true; inherit align; asStrings = true; } s))])
          lineRight
        ]);

        leftMargin = spaces margin.left;
        rightMargin = spaces margin.right;

        topMargin = Lines (lib.lists.replicate margin.top (spaces outerWidth));
        bottomMargin = Lines (lib.lists.replicate margin.bottom (spaces outerWidth));

        topBorder = Join [leftMargin hBorderTop rightMargin];
        midBorder = Join [leftMargin hBorderMid rightMargin];
        bottomBorder = Join [leftMargin hBorderBottom rightMargin];

        topPadding = Lines (lib.lists.replicate padding.top (mkLine ""));
        bottomPadding = Lines (lib.lists.replicate padding.bottom (mkLine ""));
        leftPadding = style' styles (spaces padding.left);
        rightPadding = style' styles (spaces padding.right);

        debugBoxes = true;

        boxStrings = 
        # Nix doesn't handle unicode codepoints or ANSI, so we include the logical width here.
        # Enables nesting by providing a 'body' as a list of blocks / boxes.
          let lines_ = [
                topMargin
                topBorder
                outerHeaderBlock
                topPadding
                outerBodyBlock
                bottomPadding
                bottomBorder
              ] ++ (optionals (margin.bottom > 0) [bottomMargin]);
          in NonEmptyLines lines_;
      in 
        Strings_ {w = outerWidth;} (Lines [
          boxStrings
          (optionalStrings debugBoxes (NonEmptyLines [
            (typed.replicate outerWidth ".")
            "^ ${toString outerWidth} dots"

            (Lines ["topMargin: " (topMargin.debug {})])
            (Lines ["topBorder: " (topBorder.debug { v = 1; })])
            (Lines ["outerHeaderBlock: " (outerHeaderBlock.debug {})])
            (Lines ["topPadding: " (topPadding.debug {})])
            (Lines ["outerBodyBlock: " (outerBodyBlock.debug {})])
            (Lines ["bottomPadding: " (bottomPadding.debug {})])
            (Lines ["bottomBorder: " (bottomBorder.debug {})])
            # (Join ["bottomMargin: " (bottomMargin.debug {})])
            #(Join ["boxStrings: " (boxStrings.debug {})])


            "widths:"
            (Join ["topMargin: " (toString (width topMargin))])
            (Join ["topBorder: " (toString (width topBorder))])
            (optionalStrings (header != null) (Join ["outerHeaderBlock: " (toString (width outerHeaderBlock))]))
            (Join ["topPadding: " (toString (width topPadding))])
            (Join ["outerBodyBlock: " (toString (width outerBodyBlock))])
            (Join ["bottomPadding: " (toString (width bottomPadding))])
            (Join ["bottomBorder: " (toString (width bottomBorder))])
            #(Join ["bottomMargin: " (toString (width bottomMargin))])

            (Join ["content (computed): " (toString contentWidth)])
            (Join ["box (computed): " (toString (width boxStrings))])

            (Join ["final outerWidth: " (toString outerWidth)])
          ]))
        ]);

    optionalStrings = cond: ss: if cond then ss else Strings [];

    echoWith = arg:
      let heredoc = arg.heredoc or true;
          quoteText = arg.quoteText or false;
          flags = arg.flags or [];
          styles = arg.styles or [];
          __text = arg.text or "";
          text = if quoteText
            then toShellValue __text
            else __text;
          nesting = arg.nesting or 0;
          eof = if nesting > 0 then "EOF${toString nesting}" else "EOF";
          suffix = arg.suffix or "";
          echoCmd =
            if (flags == [])
            then "echo"
            else "echo ${joinSep " " flags}";
      in if heredoc
         then codeBlock ''
${echoCmd} "$(cat << ${eof}
${style styles text}
${eof}
)"${suffix}
         ''
         else "${echoCmd} ${style styles text}${suffix}";

    # echoVariadic echoParam... [styles]... "text"
    echoVariadic = params: arg:
      let
        isEchoParam = isFunction arg;
        isStyles = isList arg;
        isText = isString arg;
      in # Allow passing without needing the chain of ({} |> param)
         if (isFunction params) then echoVariadic (params {}) arg
         # When we hit text, conclude and issue the params
         else if isText then echoWith (params // { text = arg; })
         # Otherwise set styles
         else if isStyles then echoVariadic (params // { styles = arg; })
         # Or apply a parameter function
         else if isEchoParam then echoVariadic (arg params)
         else throw (joinLines [
           "Invalid argument type to echoVariadic: ${typeOf arg}"
           "(expected a string, a list of styles, or an attribute set echoParam)"
         ]);

    # Either
    # echo "test"
    # or
    # echo [styles] "test"
    # or
    # echo param1 param2 ... [styles] "test"
    echoing = rec {
      flags = flagList: params: params // { flags = (params.flags or []) ++ flagList; };
      flag = flagStr: (flags [flagStr]);
      noFlag = flagStr: params: params // { flags = remove flagStr (params.flags or []); };
      withQuotes = params: params // { quoteText = true; };
      noHeredoc = params: params // { heredoc = false; };
      stderr = params: params // { suffix = " >&2"; };
      formatAnsi = flag "-e";
      noFormatAnsi = noFlag "-e";
      echo = with echoing; echoVariadic formatAnsi;
    };
    echo = echoing.echo;
    echo-n = with echoing; echo (flag "-n");

    atoms = {
      h1 = { style = [bold fg.cyan]; };
      h2 = { style = [bold fg.magenta]; };
      li-h1 = { style = [bold fg.brightwhite]; };
      name = { style = [bold fg.cyan]; };
      number = { style = [bold fg.yellow]; };
      cmdName = { style = [bold fg.green]; };
      cmd = { style = [italic]; };
      nixExpr = { style = [bg.grey]; };
      file = { style = [italic fg.blue]; };
      path = { style = [italic fg.cyan]; };
      address = { style = [underline fg.blue]; };
      port = { style = [underline fg.yellow]; };
      requiredOpt = { style = [fg.brightwhite]; };
      optionalOpt = { style = [fg.white]; };
      mode = { style = [fg.magenta]; };
      action = { style = [bold fg.brightblue]; };
      actionGreen = { style = [bold fg.green]; };
      actionRed = { style = [bold fg.red]; };
      testName = { style = [bold fg.white]; };
    };

    # Shorthand for making atoms e.g. ${ansi.atom.cmd "cmdname"}
    atom = mapAttrs (_: atomAttrs: style atomAttrs.style) atoms // {
      addressPort = address: port: "${atom.address address}${style [underline] ":"}${atom.port (toString port)}";
    };
  };

  _tests = with typed.tests; suite {
    styles = with ansi; 
      let 
        expectANSI = actualWith: exWithout: exWith: expect.eq (rec {
          escaped = escapeANSI actualWith;
          stripped = stripANSI actualWith;
          strippedEscaped = escapeANSI (stripANSI actualWith);
        }) {
          escaped = exWith;
          stripped = exWithout;
          strippedEscaped = exWithout;
        };
      in
        {
          _00_simple =
            expectANSI
              (style [fg.red] "test")
              "test"
              ''\\e[31mtest\\e[0;0m'';
          _01_backToBack =
            expectANSI
              "${(style [fg.red] "red")}${style [fg.blue] "blue"}"
              "redblue"
              ''\\e[31mred\\e[0;0m\\e[34mblue\\e[0;0m'';
          _02_nested =
            expectANSI
              (_b_ ''
                outer
                ${style [bg.black] (_b_ ''
                  inner
                  ${style_ [fg.blue] "nested"}
                '')}
              '')
              (_b_ ''
                outer
                inner
                nested
              '')
              (_b_ ''
                outer
                \\e[40minner
                \\e[34mnested\\e[0;0m
              '');
          _03_strip = {
            none = expect.eq (stripANSI "a") "a";
            unescaped.flat = 
              expect.eq (stripANSI ''\e[31mred\e[0;0m\e[34mblue\e[0;0m'') "redblue";
            unescaped.nested = 
              expect.eq (stripANSI ''\e[31mred\e[1m(redbold)\e[0;0m'') "red(redbold)";
            escaped = 
              let x = ''\\e[31mred\\e[0;0m\\e[34mblue\\e[0;0m'';
              in expect.eq (stripANSI x) x;
            simpleBox =
              expect.eq 
                (stripANSI (toString (box {body = "a";})))
                (joinLines [
                  "       "
                  " ┏━━━┓ "
                  " ┃   ┃ "
                  " ┃ a ┃ "
                  " ┃   ┃ "
                  " ┗━━━┛ "
                ]);

            header =
              expect.eq 
                (stripANSI (toString (box {header = "b"; body = "a";})))
                (joinLines [
                  "       "
                  " ┏━━━┓ "
                  " ┃ b ┃ "
                  " ┣━━━┫ "
                  " ┃   ┃ "
                  " ┃ a ┃ "
                  " ┃   ┃ "
                  " ┗━━━┛ "
                ]);

            styledHeader =
              expect.eq 
                (stripANSI (toString (box {header = style [fg.red] "head"; body = "body";})))
                (joinLines [
                  "          "
                  " ┏━━━━━━┓ "
                  " ┃ head ┃ "
                  " ┣━━━━━━┫ "
                  " ┃      ┃ "
                  " ┃ body ┃ "
                  " ┃      ┃ "
                  " ┗━━━━━━┛ "
                ]);

            styledBody =
              expect.eq 
                (stripANSI (toString (box {header = "head"; body = style [fg.blue] "body";})))
                (joinLines [
                  "          "
                  " ┏━━━━━━┓ "
                  " ┃ head ┃ "
                  " ┣━━━━━━┫ "
                  " ┃      ┃ "
                  " ┃ body ┃ "
                  " ┃      ┃ "
                  " ┗━━━━━━┛ "
                ]);

            nested =
              let inner = withColor: box {
                    header = if withColor then style [fg.red] "inner" else "inner";
                    body = if withColor then style [fg.blue] "body" else "body";
                  };
                  outer = withColor: box {
                    header = if withColor then style [fg.green] "outer" else "outer";
                    body = [ "Inner box:" (inner withColor) ];
                  };
              in {
                withoutColor = expect.eq
                  (stripANSI (toString (outer false)))
                  (joinLines [
                    "                  "
                    " ┏━━━━━━━━━━━━━━┓ "
                    " ┃ outer        ┃ "
                    " ┣━━━━━━━━━━━━━━┫ "
                    " ┃              ┃ "
                    " ┃ Inner box:   ┃ "
                    " ┃              ┃ "
                    " ┃  ┏━━━━━━━┓   ┃ "
                    " ┃  ┃ inner ┃   ┃ "
                    " ┃  ┣━━━━━━━┫   ┃ "
                    " ┃  ┃       ┃   ┃ "
                    " ┃  ┃ body  ┃   ┃ "
                    " ┃  ┃       ┃   ┃ "
                    " ┃  ┗━━━━━━━┛   ┃ "
                    " ┃              ┃ "
                    " ┗━━━━━━━━━━━━━━┛ "
                  ]);
                withColor = expect.eq
                  (stripANSI (toString (outer true)))
                  (joinLines [
                    "                  "
                    " ┏━━━━━━━━━━━━━━┓ "
                    " ┃ outer        ┃ "
                    " ┣━━━━━━━━━━━━━━┫ "
                    " ┃              ┃ "
                    " ┃ Inner box:   ┃ "
                    " ┃              ┃ "
                    " ┃  ┏━━━━━━━┓   ┃ "
                    " ┃  ┃ inner ┃   ┃ "
                    " ┃  ┣━━━━━━━┫   ┃ "
                    " ┃  ┃       ┃   ┃ "
                    " ┃  ┃ body  ┃   ┃ "
                    " ┃  ┃       ┃   ┃ "
                    " ┃  ┗━━━━━━━┛   ┃ "
                    " ┃              ┃ "
                    " ┗━━━━━━━━━━━━━━┛ "
                  ]);
              };
          };
        };
  };
}

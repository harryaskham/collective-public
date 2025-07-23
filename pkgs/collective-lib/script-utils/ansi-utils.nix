{ lib, collective-lib,
  overrideToShellValue ? null,
  ... }:

let
  toShellValue =
    if overrideToShellValue != null
    then overrideToShellValue
    else collective-lib.typelib.toShellValueUnsafe;
in

with lib;
with lib.strings;
with collective-lib.strings;
with collective-lib.functions;

let
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
    end = code [reset];
    style = styles: text:
      if styles == [] then text
      else ''${code styles}${text}${end}'';

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
      name = { style = [bold fg.cyan]; };
      cmdName = { style = [bold fg.green]; };
      cmd = { style = [italic]; };
      nixExpr = { style = [bg.grey]; };
      requiredOpt = { style = [fg.brightwhite]; };
      optionalOpt = { style = [fg.white]; };
    };

    # Shorthand for making atoms e.g. ${ansi.atom.cmd "cmdname"}
    atom = mapAttrs (_: atomAttrs: style atomAttrs.style) atoms;
  };
}

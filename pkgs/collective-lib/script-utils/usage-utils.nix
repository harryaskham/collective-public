{ lib, collective-lib, ansi-utils, log-utils,
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
with ansi-utils;
with log-utils;

let
  atom = ansi.atom;
  typed = collective-lib.typed;
in rec {
  # Print an option description
  optUsageString = optName: opt:
    let flags = if opt.isSwitch
                then opt.allFlags
                else map (f: ''${f} ${optName}'') (opt.allFlags);
    in with ansi;
      joinLines [
        (join [
          "  "
          (style [bold fg.blue] (concatStringsSep " | " flags))
          (optionalString (opt.isRequired)
            (style [italic fg.brightcyan] " (required)"))
          (optionalString (!(opt.isSwitch) && (opt.params ? default))
            (style [italic fg.yellow] " (default: ${opt.defaultShellValue})"))
          (optionalString (!(opt.isSwitch) && (opt.params ? defaultRaw))
            (style [italic fg.yellow] " (default: ${escapeShellArg opt.params.defaultRaw})"))
          (optionalString (opt.isSwitch && opt.isInverted)
            (style [italic fg.yellow] " (default: true)"))
          (optionalString (opt.isSwitch && !(opt.isInverted))
            (style [italic fg.yellow] " (default: false)"))
        ])
        (setIndent 4 (
          if opt ? description then (trimNewlines opt.description)
          else if (opt.isSwitch && opt.isInverted) then "Sets ${optName} to false"
          else if (opt.isSwitch) then "Sets ${optName} to true"
          else "Sets ${optName} to the value provided"))
      ];

  # Get an opt in the form of a command line usage part
  # e.g. "--required-arg XYZ", "[-o XYZ | --optional-arg XYZ]", --switch
  getOptCommandUsage = optName: opt:
    let flags = opt.allFlagsWithName;
        flagsString = concatStringsSep " | " flags;
     in if opt.isRequired then last flags else "[${flagsString}]";

  # Build a block making the 'usage' function available
  usageBlock = args: opts:
    let exposedOpts = filterAttrs (_: opt: !opt.isHiddenInUsage) opts;
        requiredOpts = filterAttrs (_: opt: opt.isRequired) exposedOpts;
        optionalOpts = filterAttrs (_: opt: !opt.isRequired) exposedOpts;
    in codeBlockHeader "### Print command usage" ''
USAGE_COMMAND=$(${
  with ansi;
  echo-n (joinWords [
    (atom.h1 "Usage")
    (toShellValue args.name)
    (atom.requiredOpt (joinWords (mapAttrsToList getOptCommandUsage requiredOpts)))
    (atom.optionalOpt (joinWords (mapAttrsToList getOptCommandUsage optionalOpts)))
  ])
})

USAGE_SUMMARY=$(${
  with ansi;
  echo-n (joinOptionalWords [
    (atom.cmdName args.name)
    (optionalString (args ? summary) (style [italic] (trimNewlines args.summary)))
  ])
})

${optionalString (args ? usage)
''USAGE_BODY=$(cat << EOF
${codeBlock args.usage}
EOF
)''}

USAGE_FULLTEXT=$(${
  with ansi;
  joinStatementLines ([
    (echo "$USAGE_SUMMARY")
    (echo "\n$USAGE_COMMAND")
    (optionalString (args ? usage) (echo "\n$USAGE_BODY"))
  ] ++ (
    optionals
      (exposedOpts != {})
      ([(echo (atom.h2 "\nOptions"))]
       ++ (mapAttrsToList
             (optName: opt: echo (optUsageString optName opt))
             exposedOpts))))
})

function usage() {
  ${with ansi.echoing; echo stderr "$USAGE_FULLTEXT"}
}
'';

}

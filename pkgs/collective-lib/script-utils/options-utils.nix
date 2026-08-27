{ pkgs, lib, collective-lib, script-utils, ... }:

with lib;
with lib.strings;
with collective-lib.strings;
with script-utils.ansi-utils;
with script-utils.log-utils;

let
  log = script-utils.log-utils.log.shell;
in rec {
  defaultOpts = args: {
    PRINT_SUMMARY = {
      long = "print-summary";
      description = "Print a summary of the command";
      switch = true;
      assign = false;
      hideInUsage = true;
      hideInOptions = true;
      body = ''
        ${ansi.echo "$USAGE_SUMMARY"}
        exit 0
      '';
    };
    HELP = {
      short = "h";
      long = "help";
      description = "Show the help text for this command";
      switch = true;
      assign = false;
      hideInUsage = true;
      hideInOptions = true;
      body = ''
        usage
        exit 0
      '';
    };
    SHOULD_SOURCE_SUBCOMMAND = {
      long = "should-source-subcommand";
      description = "If true, the script should be sourced if run as a subcommand";
      switch = true;
      assign = false;
      hideInUsage = true;
      hideInOptions = true;
      body = ''
        echo ${if (args.sourceSubcommand or false) then "true" else "false"}
        exit 0
      '';
    };
    __COLLECTIVE_DEBUG = {
      long = "debug";
      description = "Enable debugging output";
      switch = true;
      export = true;
      hideInUsage = true;
      hideInOptions = true;
    };
  };

  # Build an option along with its derived data.
  mkOpt = name: params: rec {
    inherit name params;
    hasShort = params ? short;
    hasLong = params ? long;
    assignOpt = params.assign or true;
    isRequired = params.required or false;
    isSwitch = params.switch or false;
    isInverted = params.invert or false;
    hasDefault = (params ? default) || (params ? defaultRaw) || isSwitch;
    isHiddenInUsage = params.hideInUsage or false;
    isHiddenInOptions = params.hideInOptions or false;
    shouldSourceSubcommand = params.sourceSubcommand or false;
    isExported = params.export or false;
    exportPrefix = if isExported then "export " else "";
    allFlags = 
      (optionals hasShort ["-${params.short}"])
      ++ (optionals hasLong ["--${params.long}"]);
    allFlagsWithName =
      if isSwitch then allFlags 
      else map (f: ''${f} ${name}'') allFlags;
    shouldSetDefault = hasDefault && assignOpt;
    optSuffix = if isSwitch then "" else ":";  # All non-switches require values
    shortSpec = "${params.short}${optSuffix}";
    longSpec = "${params.long}${optSuffix}";
    defaultShellValue =
      if isSwitch then if isInverted then "true" else "false"
      else if (params ? default) then escapeShellArg params.default
      else if (params ? defaultRaw) then params.defaultRaw
      else throw "Cannot infer default value for option ${name}";
    errors =
      let errorStrings = (
            (optionals (!hasShort && !hasLong) ["Option has neither long nor short flag name"])
            ++ (optionals (isSwitch && isRequired) ["Switch cannot be required"])
            ++ (optionals (!isSwitch && isInverted) ["Non-switch option cannot be inverted"])
            ++ (optionals ((params ? default) && (params ? defaultRaw)) ["Option cannot have both default and defaultRaw"])
          );
      in optionalAttrs (length errorStrings > 0) {name = errorStrings;};

    # Set an option value inside a getopt case
    assignOptValue =
      let shellValue =
            if isSwitch then if isInverted then "false" else "true"
            else ''"''${2}"'';
      in ''${exportPrefix}${name}=${shellValue}'';

    # Build a case match regex for the given opt
    caseRegex = "${concatStringsSep "|" allFlags})";

    # Build a case block body for the given opt
    handleCaseBlockBody = codeBlockLines [
      (optionalString assignOpt assignOptValue)
      (if isSwitch then "shift" else "shift 2")
      (optionalString (params ? body) (params.body))
    ];

    # Build a case block body for the given opt
    handleCaseBlock = codeBlockLines [
      caseRegex
      handleCaseBlockBody
      ";;"
    ];

    # Build a block checking if a required option is set
    checkRequiredBlock = optionalString isRequired (codeBlock ''
      if [[ -z ${"$" + name} ]]; then
        ${log.fatalWithUsage ''${name} is required''}
      fi
    '');

    # Build a block for setting the default value of an option
    setDefaultBlock = optionalString shouldSetDefault (codeBlock ''
${exportPrefix}${name}=${defaultShellValue}
    '');
  };

  # Construct the augmented options from the given option params
  mkOpts = params: validateOpts (mapAttrs mkOpt params);

  mkOptsWithDefaults = args:
    let opts = (optionalAttrs (args ? opts) args.opts) // (defaultOpts args);
    in mkOpts opts;

  validateOpts = opts:
    let
      optErrors = concatMapAttrs (_: opt: opt.errors) opts;
      optErrorStrings = mapAttrsToList (name: errorString: "${name}: ${errorString}") optErrors;
    in
      if (optErrors == {})
      then opts
      else throw (joinLines (["Invalid Options:"] ++ optErrorStrings));

  # Build the getopts command for parsing options for a script
  getoptCommand = args: opts:
    let shortSpecs = concatMapStrings (opt: opt.shortSpec) (filter (opt: opt.hasShort) (attrValues opts));
        longSpecs = concatStringsSep "," (map (opt: opt.longSpec) (filter (opt: opt.hasLong) (attrValues opts)));
    in ''${pkgs.getopt}/bin/getopt -n ${args.name} -o ${shortSpecs} --long ${longSpecs} -- "''${@}"'';

  # Build a block setting default options
  setDefaultOptsBlock = opts:
    codeBlockLinesHeader
      ["### Set default option values"]
      (mapAttrsToList (_: opt: opt.setDefaultBlock) opts);

  # Build a block checking required options are set
  checkRequiredOptsBlock = opts:
    codeBlockLinesHeader
      ["### Validate that required options are set"]
      (mapAttrsToList (_: opt: opt.checkRequiredBlock) opts);

  # Build a block handling the options inside a case
  handleOptCasesBlock = opts:
    codeBlockLines (mapAttrsToList (_: opt: opt.handleCaseBlock) opts);

  # Build an option case for the passthrough parser. GNU getopt rejects unknown
  # options before we can preserve them, so allowUnrecognisedOptions uses this
  # parser instead. It supports the normal separated form plus --long=value and
  # -sVALUE for value options.
  handlePassthroughOptCaseBlock = opt:
    let
      missingValueCheck = optionalString (!opt.isSwitch) ''
        if (( $# < 2 )); then
          ${log.fatalWithUsage "Option ${concatStringsSep " or " opt.allFlags} requires a value"}
        fi
      '';
      exactCase = codeBlockLines [
        opt.caseRegex
        missingValueCheck
        opt.handleCaseBlockBody
        ";;"
      ];
      longEqualsCase = optionalString (opt.hasLong && !opt.isSwitch) (codeBlockLines [
        "--${opt.params.long}=*)"
        ''set -- "--${opt.params.long}" "''${1#*=}" "''${@:2}"''
        opt.handleCaseBlockBody
        ";;"
      ]);
      shortAttachedCase = optionalString (opt.hasShort && !opt.isSwitch) (codeBlockLines [
        "-${opt.params.short}?*)"
        ''set -- "-${opt.params.short}" "''${1#-${opt.params.short}}" "''${@:2}"''
        opt.handleCaseBlockBody
        ";;"
      ]);
    in codeBlockLines [exactCase longEqualsCase shortAttachedCase];

  handlePassthroughOptCasesBlock = opts:
    codeBlockLines (mapAttrsToList (_: handlePassthroughOptCaseBlock) opts);

  # Build a block for handling the options
  handleOptsBlock = args: opts:
    codeBlockHeader "### Handle options" ''
while true; do
  case "''${1}" in
${handleOptCasesBlock opts}
-- )
shift
break
;;
*) ${
    if (args.allowUnrecognisedOptions or false)
    then log.debug "Ignoring unrecognised arg ${args.name}"
    else log.fatalWithUsage ''Unrecognised option: ''${1}''} ;;
  esac
done'';

  # Build a block for parsing the options
  parseOptsBlock = args: opts:
    codeBlockHeader "### Parse options" ''
      OPTS=$(${getoptCommand args opts})
      case "$?" in
        0) ;;
        1) ${
          if (args.allowUnrecognisedOptions or false)
          then log.debug "Ignoring unrecognised arg ${args.name}"
          else log.fatalWithUsage "Invalid options provided to ${args.name}"} ;;
        2) ${log.fatal "Bad parameters to getopt"} ;;
        3) ${log.fatal "Internal error in getopt"} ;;
        *) ${log.fatal "Unknown error in getopt"} ;;
      esac
      eval set -- "''${OPTS}"
    '';

  # Parse known wrapper options while preserving every unknown/positional
  # argument byte-for-byte for a wrapped command. Callers should still use `--`
  # to prevent a downstream flag that matches a wrapper flag from being consumed.
  passthroughOptsBlock = args: opts: codeBlocks [
    (setDefaultOptsBlock opts)
    (codeBlockHeader "### Parse options and preserve passthrough arguments" ''
PASSTHROUGH_ARGS=()
while (( $# > 0 )); do
  case "$1" in
${handlePassthroughOptCasesBlock opts}
--)
shift
PASSTHROUGH_ARGS+=("$@")
break
;;
*)
PASSTHROUGH_ARGS+=("$1")
shift
;;
  esac
done
set -- "''${PASSTHROUGH_ARGS[@]}"
'')
    (checkRequiredOptsBlock opts)
  ];

  # Build the combined options handling block. Unknown options cannot be made
  # safe by ignoring getopt's failure: that loses or rewrites argv. Use the
  # dedicated parser whenever passthrough was requested.
  optsBlock = args: opts:
    if (args.allowUnrecognisedOptions or false)
    then passthroughOptsBlock args opts
    else codeBlocks [
      (parseOptsBlock args opts)
      (setDefaultOptsBlock opts)
      (handleOptsBlock args opts)
      (checkRequiredOptsBlock opts)
    ];

}

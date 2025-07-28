{ config, lib, pkgs, collective-lib, ansi-utils, log-utils, script-types, ... }:

with lib;
with lib.strings;
with collective-lib.strings;
with ansi-utils;
with log-utils;
with script-types;

let
  log = log-utils.log.shell;
in rec {
  # Build a script package with a name and a summary
  mkCommandRoot = name: summary: {
    inherit summary;
    type = bashScript;
    usage = ''
      $(subcommands-of --prefix ${name} --long --delimiter "-" \
        | xargs -I{} bash -c "{} --print-summary" \
        | indent -n2 \
        | sed -e 's/${name}-//')
    '';
    preamble = ''
      if [[ "''${1}" =~ ^-.+$ ]]; then
        ${log.debug ''No command detected, parsing flags as normal''}
        COMMAND=""
      else
        COMMAND="''${1}"
        ${log.debug ''Command detected: $COMMAND''}
        shift
        COMMAND_ARGS="''${@}"

        # getopt now sees no options, all will be passed to the command
        set -- --

        ${log.debug ''Command arguments: ''${COMMAND_ARGS}''}
        ${log.debug ''New flags: $@''}
      fi
    '';
    main = ''
      if [[ -z "$COMMAND" ]]; then
        ${log.exit.usage}
      fi

      CMD="${name}-''${COMMAND}"
      if which "''${CMD}" 1>/dev/null 2>/dev/null; then
        ${log.debug ''Command found as: ''${CMD}''}
        if $("''${CMD}" --should-source-subcommand); then
          ${log.debug ''Sourcing command: ''${COMMAND}''}
          source "''${CMD}" ''${COMMAND_ARGS}
        else
          ${log.debug ''Running command: ''${COMMAND}''}
          "''${CMD}" ''${COMMAND_ARGS}
        fi
      else
        ${log.fatalWithUsage ''Unknown command: ''${COMMAND}''}
      fi
    '';
  };

  # Make a non-leaf node of a command tree
  mkCommandTreeNode = name: summary: {
    "${name}" = mkCommandRoot name summary;
    "${name}-help" = {
      type = bashScript;
      summary = "Print the help message for the ${with ansi; style [bold fg.green] name} command.";
      main = "${name} --help";
    };
  };

  # Script type Builder for a command tree
  # A tree looks like:
  # command = {
  #   type = commandTree;
  #   summary = "Root of command"; # Summary to appear in helptext
  #   subcommands = {
  #     subcommand = {
  #       type = bashScript;
  #       ...
  #     };
  #     subtree = {
  #       type = commandTree;
  #       ...
  #     };
  #   };
  # };
  commandTree = {
    id = "commandTree";
    builder = args:
      let
        flattenNode = prefix: name: __node:
          let node = __node // {name = "${prefix}${name}";};
          in if (scriptTypeEquals commandTree node.type)
             then (mkCommandTreeNode node.name node.summary)
                  // (concatMapAttrs (flattenNode "${node.name}-") node.subcommands)
             else { ${node.name} = node; };
      in mkScriptPackage {
        inherit (args) name;
        type = collection;
        scripts = flattenNode "" args.name args;
      };
  };

  aliases = {
    bat = path: "bat -P --style=plain --theme=Nord $(${path})";
  };

}

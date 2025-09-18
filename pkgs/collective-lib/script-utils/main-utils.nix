{ lib, collective-lib, script-utils, ... }:

with lib;
with lib.strings;
with collective-lib.strings;
with script-utils.ansi-utils;
with script-utils.log-utils;

let
  log = script-utils.log-utils.log.shell;
in rec {
  maybeRunMainBlock = args:
    let runMain = args.runMain or true;
        mainWrappedDisabled = codeBlock ''
          function __main-wrapped() {
            ${log.debug "Main function running disabled"}
          }
        '';
        mainWrappedEnabled = codeBlock ''
          function __main-wrapped() {
            if [[ $(type -t main) == function ]]; then
              main "$@"
            else
              ${log.fatal "main() not defined"}
            fi
          }
        '';
        mainWrapped = if runMain then mainWrappedEnabled else mainWrappedDisabled;
        runMainWrapped = codeBlock ''
          __main-wrapped "$@"
          if [[ $? -ne 0 ]]; then
            ${log.fatal "main() failed"}
          fi
          ${log.debug "main() completed successfully"}
        '';
    in codeBlocks [
      mainWrapped
      runMainWrapped
    ];
}

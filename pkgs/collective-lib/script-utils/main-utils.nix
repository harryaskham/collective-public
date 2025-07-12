{ config, lib, pkgs, collective-lib, ansi-utils, log-utils, ... }:

with lib;
with lib.strings;
with collective-lib.strings;
with ansi-utils;
with log-utils;

let
  log = log-utils.log.shell;
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
              ${log.debug "Running main()"}
              main $@
            else
              ${log.fatal "main() not defined"}
            fi
          }
        '';
        mainWrapped = if runMain then mainWrappedEnabled else mainWrappedDisabled;
        runMainWrapped = codeBlock ''
          __MAIN_RT=$(__main-wrapped $@)
          if [[ $? -ne 0 ]]; then
            ${log.fatal "main() failed"}
          fi
          ${log.debug "main() completed successfully"}
          if [[ -n "$__MAIN_RT" ]]; then
            ${log.debug "main() returned: $__MAIN_RT"}
            ${ansi.echo "$__MAIN_RT"}
          fi
        '';
    in codeBlocks [
      mainWrapped
      runMainWrapped
    ];
}

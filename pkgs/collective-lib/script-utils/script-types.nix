{ config, lib, pkgs, collective-lib, ansi-utils, log-utils, usage-utils, options-utils, main-utils, ... }:

with lib;
with lib.strings;
with collective-lib.strings;
with ansi-utils;
with log-utils;
with usage-utils;
with options-utils;
with main-utils;

rec {
  # Build a script attrset into the set of derivations it produces.
  mkScriptPackage = script:
    script.type.builder script;

  # Build a script attrset into the set of derivations it produces.
  # Sets the script name given its key in an attrset.
  mkScriptPackageNamed = name: script:
    mkScriptPackage (script // { inherit name; });

  # Build a script attrset into the set of derivations it produces.
  # Returns the script named at the root.
  # For collections or command trees, can be used as a default package.
  mkDefaultScriptPackage = script:
    (mkScriptPackage script).${script.name};

  # A flat collection of scripts as { scriptName = script; ... }
  collection = {
    id = "collection";
    builder = args: {
      ${args.name} = pkgs.symlinkJoin {
        inherit (args) name;
        paths =
          attrValues
            (concatMapAttrs (name: script: mkScriptPackageNamed name script)
             args.scripts);
      };
    };
  };

  # Create a Bash script with given name, opts, help text, and body.
  #
  # Args are defined as:
  #
  # {
  #   type = bashScript;                 # A builder fn from this args attrSet to a derivation
  #   summary = "Runs some script";      # Summary to appear in helptext
  #   usage = "somescript -a ARG_A -b";  # Usage text to appear in helptext
  #   opts = { ... };                    # Defined below
  #   sourceSubcommand = false;          # If true, the script will be sourced when run as a subcommand
  #   preamble = ""                      # Script body to appear before opt parsing (optional)
  #   body = ''                          # Script body (optional; all logic can be in opt bodies)
  #     ...
  #   '';
  #   runMain = true;                    # If true, will run main() if it is defined in body or if main is set.
  #                                      # and fail if not defined. (default: true)
  #   or
  #   main = ''                          # Provide the main() function body for simple scripts
  #     ...
  #   '';
  # };
  #
  # Options are defined as:
  #
  # {
  #   ARG_NAME = {             # Option name; will be available as $ARG_NAME unless assign=false
  #     short = "a";           # Short flag name (one of short or long must be provided)
  #     long = "arg-name";     # Long flag name (one of short or long must be provided)
  #     description = "...";   # Description of the option (optional)
  #     required = false;      # If true, must be provided (default: false, switch-incompatible)
  #     default = 123;         # Escaped default value (optional)
  #     defaultRaw = "$HOME";  # Unescaped default value (optional)
  #     switch = false;        # If true, sets ARG_NAME=true when present (default: false)
  #     invert = false;        # If true, switch will default to true unless provided (default: false)
  #     assign = true;         # If true, will be assigned to $ARG_NAME (default: true)
  #     hideInUsage = true;    # If true, will not be shown in usage (default: false)
  #     hideInOptions = true;  # If true, will not be shown in help options (default: false)
  #   };
  #   ...
  # }
  mkBashScriptBody = argsPre:
    let
      validateBashScriptArgs = args:
        let errors = joinOptionalLines [
              (optionalString ((args ? body) && (args ? main))
                "Cannot specify both body and main function")
            ];
         in
          if errors != ""
          then throw "Invalid bash script args: ${errors}"
          else args;
      args = validateBashScriptArgs argsPre;
      # The actual validated options attrs for the script
      opts = mkOptsWithDefaults args;

      # The body of the pre-script before options are set.
      scriptPreamble =
        codeBlockHeader "### Script Preamble"
        (optionalString (args ? preamble) (codeBlock args.preamble));

      # The body of the script after options are set.
      # If all the logic occurs in the option bodies (i.e. for different switches)
      # this can be empty.
      scriptBody =
        codeBlockHeader "### Script Body"
        (if args ? body then codeBlock args.body
         else if args ? main then codeBlockLines ["function main() {" args.main "}"]
         else "");

      # Build the final completed script
      fullScript = codeBlocks [
        logBlock
        scriptPreamble
        (usageBlock args opts)
        (optsBlock args opts)
        scriptBody
        (maybeRunMainBlock args)
      ];

    in fullScript;

  # Check equality of script types by comparing their IDs.
  scriptTypeEquals = a: b: a.id == b.id;

  # Builder for a Bash script.
  bashScript = {
    id = "bashScript";
    builder = args: {
      ${args.name} = pkgs.writeShellScriptBin args.name (mkBashScriptBody args);
    };
  };

  # Builder-factory for a raw script file wrapping the given shell name
  rawScript = shellName: {
    id = "rawScript-${shellName}";
    builder = args: {
      ${args.name} = pkgs.writeTextFile {
        name = args.name;
        text = joinLines [
          "#!/usr/bin/env ${shellName}"
          (mkBashScriptBody args)
        ];
        executable = true;
        destination = "/bin/${args.name}";
      };
    };
  };

  # Builder for a Zsh script.
  # Compatible with bashScript, but interprets args.body as Zsh and runs nested.
  zshScript = {
    id = "zshScript";
    builder = args:
      let
        zshArgs = {
          type = rawScript "zsh";
          name = "zshwrapper-" + args.name;
        };
        zshScript = mkScriptPackage zshArgs;
      in mkScriptPackage {
          inherit (args) name;
          type = rawScript "bash";
          body = ''zsh "${zshScript}/bin/${zshArgs.name}" "$@"'';
        };
  };

  # Builder for a Python script with given name, python dependency function (ps: deps), and body.
  #
  # Example:
  # myscript = {
  #   type = pythonScript;
  #   deps = ps: [ps.numpy];
  #   body = ''
  #     import numpy as np
  #     ...
  #   '';
  # };
  pythonScript = {
    id = "pythonScript";
    builder = args:
      let libraries =
            if args ? deps
            then args.deps pkgs.python3Packages
            else [];
      in {
        ${args.name} =
          pkgs.writers.writePython3Bin
            args.name { inherit libraries; } args.body;
      };
  };

  # Create a Python Fire script with given name, python dependency function (ps: deps),
  # and body. Body must define a function with the name of the script (args.name) which is
  # then exposed as a CLI via Fire.
  #
  # Example:
  # myscript = {
  #   type = fireScript;
  #   body = ''
  #     def myscript(a):
  #       echo f"Hello {a}!"
  #   '';
  # };
  fireScript = {
    id = "fireScript";
    builder = args: mkScriptPackage {
      inherit (args) name;
      type = pythonScript;
      deps = ps: [ps.fire] ++ (optionals (args ? deps) (args.deps ps));
      body = ''
        import fire


        ${args.body}

        if __name__ == "__main__":
            fire.Fire(${args.name})
      '';
    };
  };
}

{ pkgs, collective-lib }:

let
  inherit (pkgs) lib;
  inherit (collective-lib) typed untyped;
  inherit (typed.tests) expect suite;

  # Capture the exact text passed by the real system module to writeText while
  # leaving every other package dependency unchanged.
  testPkgs = pkgs // {
    writeText = name: text: builtins.toFile name text;
    # Keep rendered config context-free so builtins.toFile can capture it; the
    # test only inspects the command string and never executes this fake path.
    coreutils = builtins.toFile "test-coreutils" "";
  };

  evalHome = usage:
    (lib.evalModules {
      specialArgs = { inherit typed; };
      modules = [
        ../home-manager/supervisord.nix
        usage
      ];
    }).config;

  systemOptionStubs = { lib, ... }: {
    options = {
      assertions = lib.mkOption {
        type = lib.types.listOf lib.types.attrs;
        default = [];
      };
      environment.packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [];
      };
      build.activationAfter = lib.mkOption {
        type = lib.types.attrsOf lib.types.lines;
        default = {};
      };
      shell.init = lib.mkOption {
        type = lib.types.lines;
        default = "";
      };
    };
  };

  evalSystem = usage:
    (lib.evalModules {
      specialArgs = { pkgs = testPkgs; inherit typed untyped; };
      modules = [
        systemOptionStubs
        ../nix-on-droid/supervisord.nix
        usage
      ];
    }).config;

  homeDefault = evalHome ({ ... }: {
    supervisord.programs.example.command = "/bin/true";
  });
  homeExplicit = evalHome ({ ... }: {
    supervisord.programs.example = {
      command = "/bin/true";
      stopwaitsecs = 45;
    };
  });

  systemDefault = evalSystem ({ ... }: {
    supervisord = {
      enable = true;
      programs.example.command = "/bin/true";
    };
  });
  systemExplicit = evalSystem ({ ... }: {
    supervisord = {
      enable = true;
      programs.example = {
        command = "/bin/true";
        stopwaitsecs = 45;
      };
    };
  });
  systemLogDirs = evalSystem ({ ... }: {
    supervisord = {
      enable = true;
      programs.logs = {
        command = "/bin/true";
        directory = "/home/u/.work";
        stdout_logfile = "/home/u/.app/app.log";
        stderr_logfile = "/home/u/.app/app.err";
      };
      programs.special = {
        command = "/bin/true";
        stdout_logfile = "/dev/null";
        stderr_logfile = "%(here)s/x.log";
      };
    };
  });
  systemConditional = evalSystem ({ ... }: {
    supervisord = {
      enable = true;
      programs.always.command = "/bin/true";
      programs.cond = {
        command = "/bin/true";
        stdout_logfile = "/home/u/.caco/caco.log";
        requiresPath = "/home/u/.caco";
        requiresHint = "Run the boot script.";
      };
    };
  });
  systemRelativeRequires = evalSystem ({ ... }: {
    supervisord = {
      enable = true;
      programs.rel = {
        command = "/bin/true";
        requiresPath = "relative/path";
      };
    };
  });
  homeConditional = evalHome ({ ... }: {
    supervisord.programs.cond = {
      command = "/bin/true";
      requiresPath = "/home/u/.caco";
    };
  });
  systemReservedCollision = evalSystem ({ ... }: {
    supervisord = {
      enable = true;
      programs.collective-exec-canary.command = "/bin/false";
    };
  });

  renderedConfig = evaluated: let
    contextPaths = builtins.attrNames (builtins.getContext evaluated.build.activationAfter.supervisord);
    configPaths = lib.filter (lib.hasSuffix "-supervisord.conf") contextPaths;
  in
    builtins.readFile (lib.head configPaths);

  startPackage = evaluated:
    lib.findFirst (p: (p.name or "") == "supervisord-start") null evaluated.environment.packages;
  startScript = evaluated: (startPackage evaluated).text;
  innerScript = evaluated: (startPackage evaluated).supervisord-start-inner.text;
  conditionalConf = evaluated: name: let
    contextPaths = builtins.attrNames (builtins.getContext (innerScript evaluated));
  in
    builtins.readFile (lib.head (lib.filter (lib.hasSuffix "-supervisord-program-${name}.conf") contextPaths));
in {
  home-manager = suite {
    defaultsToNull = expect.eq homeDefault.supervisord.programs.example.stopwaitsecs null;
    acceptsInteger = expect.eq homeExplicit.supervisord.programs.example.stopwaitsecs 45;
    acceptsRequiresPath = expect.eq homeConditional.supervisord.programs.cond.requiresPath "/home/u/.caco";
  };

  nix-on-droid = suite {
    preservesSupervisorDefault =
      expect.eq (lib.hasInfix "stopwaitsecs=" (renderedConfig systemDefault)) false;
    rendersExplicitValue =
      expect.eq (lib.hasInfix "stopwaitsecs=45" (renderedConfig systemExplicit)) true;
    rendersExecCanary =
      expect.eq (lib.hasInfix "[program:collective-exec-canary]" (renderedConfig systemDefault)) true;
    execCanaryIsOnDemand =
      expect.eq (lib.hasInfix "[program:collective-exec-canary]\ncommand=" (renderedConfig systemDefault)
        && lib.hasInfix "autostart=false\nautorestart=false\nstartsecs=0" (renderedConfig systemDefault)) true;
    createsProgramDirsBeforeStart =
      expect.eq (lib.hasInfix "mkdir -p /home/u/.app /home/u/.work" (startScript systemLogDirs)) true;
    skipsNonPathLogTargets =
      expect.eq (lib.any (s: lib.hasInfix s (startScript systemLogDirs)) ["mkdir -p /dev" "%(here)s" " /dev "]) false;
    noProgramDirsWithoutLogs =
      expect.eq (lib.length (lib.filter (lib.hasPrefix "mkdir") (lib.splitString "\n" (startScript systemDefault)))) 1;
    conditionalProgramLeftOutOfMainConfig =
      expect.eq (lib.hasInfix "[program:cond]" (renderedConfig systemConditional)) false;
    unconditionalProgramStaysInMainConfig =
      expect.eq (lib.hasInfix "[program:always]" (renderedConfig systemConditional)) true;
    mainConfigIncludesConfDir =
      expect.eq (lib.hasInfix "[include]\nfiles = /tmp/run/supervisord/conf.d/*.conf" (renderedConfig systemConditional)) true;
    noIncludeWithoutConditionalPrograms =
      expect.eq (lib.hasInfix "[include]" (renderedConfig systemDefault)) false;
    conditionalProgramHasOwnConfig =
      expect.eq (lib.hasInfix "[program:cond]\ncommand=/bin/true" (conditionalConf systemConditional "cond")
        && lib.hasInfix "stdout_logfile=/home/u/.caco/caco.log" (conditionalConf systemConditional "cond")) true;
    linksConditionalProgramOnlyWhenPathExists =
      expect.eq (lib.hasInfix "if [ -e /home/u/.caco ]; then" (innerScript systemConditional)
        && lib.hasInfix "/cond.conf" (innerScript systemConditional)) true;
    warnsWithHintWhenPathMissing =
      expect.eq (lib.hasInfix "warning: not starting cond: /home/u/.caco does not exist. Run the boot script." (innerScript systemConditional)) true;
    leavesConditionalDirsToBootstrap =
      expect.eq (lib.hasInfix "mkdir -p /home/u/.caco" (startScript systemConditional)) false;
    keptInstanceRereadsConfDir =
      expect.eq (lib.hasInfix "supervisorctl reread" (innerScript systemConditional)
        && lib.hasInfix "supervisorctl update" (innerScript systemConditional)) true;
    requiresAbsolutePath = expect.eq
      (lib.any (assertion:
        !assertion.assertion && lib.hasInfix "requiresPath must be absolute: rel" assertion.message
      ) systemRelativeRequires.assertions)
      true;
    reservesExecCanaryName = expect.eq
      (lib.any (assertion:
        !assertion.assertion
        && lib.hasInfix "reserved for the proot exec canary" assertion.message
      ) systemReservedCollision.assertions)
      true;
  };
}

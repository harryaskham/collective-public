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
in {
  home-manager = suite {
    defaultsToNull = expect.eq homeDefault.supervisord.programs.example.stopwaitsecs null;
    acceptsInteger = expect.eq homeExplicit.supervisord.programs.example.stopwaitsecs 45;
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
    reservesExecCanaryName = expect.eq
      (lib.any (assertion:
        !assertion.assertion
        && lib.hasInfix "reserved for the proot exec canary" assertion.message
      ) systemReservedCollision.assertions)
      true;
  };
}

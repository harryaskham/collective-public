{ 
  pkgs,
  lib ? pkgs.lib,
  inputs ? {},
  ...
}:

lib.fix (self:
  let
    collective-public = inputs.collective-public or {
      lib = import ../../../pkgs/collective-lib {
        inherit pkgs lib;
        inputs.nix-reflect.lib.${pkgs.system} = self;
      };
    };
    collective-lib = collective-public.lib.${pkgs.system}.withTraceOpts {
      traceLevel =
        let l = builtins.getEnv "CLTV_TRACE_LEVEL";
        in if l == "" then 3 else lib.toInt l;
    };
    nix-parsec = inputs.nix-parsec or {
      # nix eval --raw .#inputs.nix-parsec.outPath 2>/dev/null
      lib = import /nix/store/nlawm43dvjgaz5q9bj45vwk6a3rfddbn-source;
    };
  in
    collective-lib.tests.withMergedSuites (rec {
      parser = import ./parser {
        inherit lib collective-lib nix-parsec;
        nix-reflect = self;
      };

      eval = import ./eval {
        inherit lib collective-lib parser;
        nix-reflect = self;
      };

      debuglib = import ./debuglib.nix {
        inherit lib collective-lib;
        nix-reflect = self;
      };
    }) // {
      # Reexpose collective-lib.typed for script convenience
      inherit (collective-lib) typed;
    }
)

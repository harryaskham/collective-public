{
  lib,
  typed,
  ...
}:
# Home-manager facing supervisord declarations for nix-on-droid.
# These declarations are rolled up by the nix-on-droid system module into the
# single system-level supervisord instance so user-scoped modules can declare
# long-running services without depending on a separate home-level process
# manager.
with typed;
with lib; let
  programOpts = import ../supervisord-program-options.nix { inherit lib typed; };
in {
  options.supervisord = {
    programs = mkOption {
      type = types.attrsOf (types.submodule programOpts);
      default = {};
      description = ''
        User-declared supervisord programs. On nix-on-droid these are merged
        into the single system-level supervisord instance so home-manager
        modules can contribute managed services alongside system modules.
      '';
    };
  };
}

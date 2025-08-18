{ uklib, typed, ... }:

{
  QWERTY_US = import ./qwerty-us.nix { inherit uklib typed; };
  Code_QWERTY = import ./code-qwerty.nix { inherit uklib typed; };
  Code_QWERTY_Compact = import ./code-qwerty-compact.nix { inherit uklib typed; };
}

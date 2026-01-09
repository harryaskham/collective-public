{ lib ? import <nixpkgs/lib>, collective-lib ? import ./. { inherit lib; }, ... }:

let
  typed = collective-lib.typed;
in rec {
  # Datatype for defining enums.
  # Stringifies by default to an identifier string, while thunk-evals to the original value.
  # This enables attr-switch statements to work with string interpolation.
  # A value of {} is interpreted as a unit value, and the enum name is used as the value.
  Enum = name: lib.mapAttrs (id: value: {
    inherit id;
    value = if value == {} then id else value;
    functor = self: {}: self.value;
    switch = opts: opts.${id} or opts._ or (throw ''Enum "${name}" does not have member "${id}"'');
    __toString = self: "${name}.${id}${lib.optionalString (value != {}) "(${self.value})"}";
  });

  # Module utils
  ConfigType = Enum "ConfigType" {
    System = {};
    Home = {};
  };

  Architecture = Enum "Architecture" {
    X86 = "x86_64-linux";
    Aarch64 = "aarch64-linux";
    Darwin = "aarch64-darwin";
  };

  SystemType = Enum "SystemType" {
    NixOS = "nixos";
    NixOnDroid = "nix-on-droid";
    NixDarwin = "nix-darwin";
  };

  System = {
    NixOS.X86 = {
      architecture = Architecture.X86;
      systemType = SystemType.NixOS;
    };
    NixOS.Aarch64 = {
      architecture = Architecture.Aarch64;
      systemType = SystemType.NixOS;
    };
    NixOnDroid = {
      architecture = Architecture.Aarch64;
      systemType = SystemType.NixOnDroid;
    };
    NixDarwin = {
      architecture = Architecture.Darwin;
      systemType = SystemType.NixDarwin;
    };
  };

  NamedSystem = systemName: system: systemExtra: {
    inherit systemName system systemExtra;
  };

  # <nix> data._tests.run {} </nix>
  _tests = with typed.tests; suite {
    SystemType = {
      id = expect.eq SystemType.NixOS.id "NixOS";
      value = expect.eq SystemType.NixOS.value "nixos";
      toString = expect.eq (toString SystemType.NixOS) "SystemType.NixOS(nixos)";
      switch = expect.True
        (SystemType.NixOS.switch {
          NixOS = true;
          NixOnDroid = false;
          NixDarwin = false;
        });
    };

    units =
      let E = Enum "E" {
        A = {};
        B = {};
      };
      in {
        toString = expect.eq (toString E.A) "E.A";
        id.A = expect.eq E.A.id "A";
        id.B = expect.eq E.B.id "B";
        value.A = expect.eq E.A.value "A";
        value.B = expect.eq E.B.value "B";
        switch.A = expect.True (E.A.switch { A = true; B = false; });
        switch.B = expect.False (E.B.switch { A = true; B = false; });
        switch.missingA = expect.error (E.A.switch { B = false; });
        switch.missingB = expect.False (E.B.switch { B = false; });
      };
  };

}

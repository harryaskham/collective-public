{ lib ? import <nixpkgs/lib>, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;
with collective-lib;

# Misc library fns
rec {
  # Options factories
  Opt = {
    def = mkDefaultOption;

    enum = {
      __functor = self: mkEnum;
      def = mkEnumDef;
    };

    enable = {
      __functor = self: mkEnable;
      default = mkDefaultEnable;
      nested = {
        __functor = self: mkNestedEnable;
        default = mkNestedDefaultEnable;
      };
    };

    mk = 
      let 
        bind = self:
          if self ? __class__ then
            self // (forAttrs self.__class__.methods (_: method: method self))
          else
            self;
        makeBinding = Variadic.compose bind;
        setters = {
          set = forAttrs self.__class__.fields (field: _: value: bind (self // { ${field} = value; } ));
          modify = forAttrs self.__class__.fields (field: _: f: self.set.${field} (f self.${field}));
        };
        class = name: fields: methods: 
          lib.fix (cls: {
            inherit name fields;
            methods = mapAttrs (_: makeBinding) methods;
            new = values:
              lib.fix (self: bind (mergeAttrsList [
                {__class__ = cls;}
                fields
                values
                setters
              ]));
          });
      in class "Opt.mk" { opt = {}; } {
        def = self: default: self.modify.opt (opt: opt // { inherit default; });
        of = self: type: self.modify.opt (opt: opt // { inherit type; });
        desc = self: description: self.modify.opt (opt: opt // { inherit description; });
        done = self: {}: self.opt;
      };

    propagate = {
      __functor = self: self.option;

      # Propagate an option block from a module verbatim
      option = getModule: getOption: { pkgs, outputs, ... } @ args:
        let ms =  outputs.modules.${pkgs.system};
            m = pkgs.callPackage (getModule ms) args;
        in getOption m.options;

      # Make a default-enabled version of the propagated option block
      enabled = getModule: getOption: args:
        let opt = Opt.propagate getModule getOption args;
        in opt // {
          enable = opt.enable // { default = true; };
        };
    };
  };

  # Legacy options factories
  mkEnum = (values: desc: mkOption {
    type = types.enum values;
    description = desc;
  });

  mkEnumDef = (def: values: desc: mkEnum values desc // { default = def; });

  mkEnable = mkEnableOption;

  mkDefaultEnable = (desc: mkOption {
    type = types.bool;
    default = true;
    description = desc;
  });

  mkNestedEnable = (desc: {
    enable = mkEnableOption desc;
  });

  mkNestedDefaultEnable = (desc: {
    enable = mkDefaultEnable desc;
  });

  mkDefaultOption = (type: default: description: mkOption {
    inherit type default description;
  });

  mkDefaultNull = type: mkDefaultOption (types.nullOr type) null;

  # Misc
  pow = lib.fix (
    self: base: power:
      if power != 0
      then base * (self base (power - 1))
      else 1
    );
  trunc = d: x: let e = pow 10 d; in x - ((x * e - round (x * e)) / e);
  round = x: 
    let f = builtins.floor (x + 0.0);
        d = x - f;
    in if d >= 0.5 then f + 1 else f;
  abs = x: if x < 0 then -x else x;

  _tests = with typed.tests; suite {
    round = {
      low = expect.eq (round 1.2345) 1;
      high = expect.eq (round 1.8345) 2;
      neg = expect.eq (round (-1.8345)) (-2);
      int = expect.eq (round 1) 1;
    };
    trunc = {
      low = expect.eq (trunc 1 1.2345) 1.2;
      high = expect.eq (trunc 2 1.8345) 1.83;
      neg = expect.eq (trunc 2 (-1.8345)) (-1.83);
      int = expect.eq (trunc 2 1) 1;
      zero = expect.eq (trunc 0 1.2345) 1;
    };
    abs = {
      pos = expect.eq (abs 1) 1;
      neg = expect.eq (abs (-1)) 1;
      zero = expect.eq (abs 0) 0;
    };
  };
}

{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib, cutils ? import ./. { inherit lib; }, ... }:

with lib;
with lib.strings;
with cutils.attrs;
with cutils.functions;
with cutils.errors;
with cutils.strings;

# Simply inheritance-based typesystem for use outside of the module system.
#
# Objects are represented as attrsets with "Type" and "Super" keys, holding the
# type and supertype definitions respectively.
#
# All types ultimately inherit from Types.Type, which inherits from itself.
#
# Fields are validated and typechecked upon object creation and again on updates,
# such that once an object of type T is constructed it remains valid under T.check.
#
# Builtin types are wrapped as Builtin.<type> such that when a field is assigned
# a builtin type it is implicitly converted to its corresponding Builtin. The
# raw builtin value can be accessed via the 'value' field.
#
# These are different types to the underlying builtins:
# e.g. String.check "abc" == false
#      String.check (String.new "abc") == true
#      String.check (wrap "abc") == true
#      isString (String.new "abc") == false
#      isString (String.new "abc").value == true
#
# Person = mkType "Person" {
#   fields = [
#     (Field (Of String) "name")
#     (Field (Of (Optional String)) "nickname")
#     (Field (Of Int) "age")
#   ];
#   methods = {
#     hello = this: msg: "Hello, ${this.show}! ${msg}";
#     show = this: verbose: joinOptionalsSep ", "
#       [this.get.name]
#       ++ optionals verbose [
#         (optionalString this.has.nickname) "AKA ${this.get.nickname}")
#         "${toString this.get.age}y old";
#     ];
#   }
# };
#
# OK:   alice = Person.new {name = "Alice"; age = 22;};
# OK:   bob = Person.new {name = "Robert"; nickname = "Bob"; age = 40;};
# FAIL: ethan = Person.new {nickname = "Bob"; age = 40;} -> Throws error for missing name field
# FAIL: jake = Person.new {name = "Jake"; age = "unknown";} -> Throws error for invalid age type
#
# alice.get.name = "Alice";
# bob.get.nickname = "Bob";
# alice.get.nickname = null;
# alice.hello "Binding works as expected" = "Hello, Alice, 22y old! Binding works as expected"
# bob.show = "Robert (Bob), 40y old! Cool."

let
  log = cutils.log;
in rec {
  over = name: aToDict: a: (aToDict a).${typeOf a} or throw "${name}: ${typeOf a}";
  Functor = {
    fmap = f: over "fmap" (a: {
      null = null;
      list = map f a;
      set = mapAttrs (_: v: f v) a;
      function = compose f a;
    });
  };
  inherit (Functor) fmap;

  assertType = Type: x:
    unless
      (Type.name == actual)
      throw "Expected ${expectedName} but got ${typeOf actual}: ${actual}";

  assertBuiltinType = typeName: x:
    unless
      (typeName == typeOf x)
      throw "Expected builtin ${typeName} but got ${typeOf x}: ${x}";

  Builtin =
    with Types;
    let mkBuiltin = name: builtinType: isType:
          mkType name {
            fields = [
              (Field (Of builtinType) "value")
            ];
            static = {
              __isValueType = This: true;
              __builtinTypeName = This: builtinType;
              __checkBuiltin = This: isType;
            };
            # For a builtin we already wrapped the value for "New"
            # so we can unpack the value here
            ctor = x: { inherit (x) value; };
            checkValue = that: isType that.value;
          };
    in rec {
      Null = mkBuiltin "Null" "null" isNull;
      Int = mkBuiltin "Int" "int" isInt;
      Float = mkBuiltin "Float" "float" isFloat;
      String = mkBuiltin "String" "string" isString;
      Path = mkBuiltin "Path" "path" isPath;
      Bool = mkBuiltin "Bool" "bool" isBool;
      List = mkBuiltin "List" "list" isList;
      Set = mkBuiltin "Set" "set" (x: isAttrs x && !(x ? Type));
      Lambda = mkBuiltin "Lambda" "lambda" isFunction;

      # Get the builtin type name corresponding to the given Builtin T.
      # Fails if T is not a Builtin.
      toBuiltinTypeName = T:
        T.__builtinTypeName or
          (throw "Invalid T argument for Builtin.toBuiltinTypeName: ${log.print T}");

      # Get the Builtin type corresponding to the given builtin type.
      # Returns null if builtinType is not a builtin type string.
      maybeFromT = builtinType:
        {
          null = Null;
          int = Int;
          float = Float;
          string = String;
          path = Path;
          bool = Bool;
          list = List;
          set = Set;
          lambda = Lambda;
        }.${builtinType} or null;

      # Get the Builtin type corresponding to the given builtin type.
      # Throws if builtinType is not a builtin type string.
      FromT = builtinType:
        let T = maybeFromT builtinType;
        in if T == null then (throw "Invalid T argument for Builtin.FromT: ${log.print builtinType}")
        else T;

      From = x:
        let T = ({
              null = Null;
              int = Int;
              float = Float;
              string = String;
              path = Path;
              bool = Bool;
              list = List;
              set = assert !(x ? Type); Set;
              lambda = Lambda;
            }."${typeOf x}" or (throw "Invalid type for Builtin.From: ${typeName x}"));
        in T.mk { value = x; };  # mk not new here so new can use From
    };

  Types = rec {
    # Whether or not x is a builtin type
    isBuiltinValue = x: !(isAttrs x && x ? Type);

    # Whether or not name is one of the builtin type names.
    isBuiltinTypeName = name: Builtin.maybeFromT name != null;

    # Wrap a builtin type
    wrap = Builtin.From;

    # Unwrap a value type
    unwrap = x: x.value;

    # Wrap a builtin type or leave a Type value unchanged
    maybeWrap = x: if isBuiltinValue x then wrap x else x;

    # Unwrap a value type or leave a non-value Type value unchanged
    maybeUnwrap = x: if isValueType x then unwrap x else x;

    # Cast between builtin and Builtin
    cast = T: x:
      # noop if same type
      if hasType T x then x
      # maybe downcast a Builtin
      else if isTyped x
        then if (!isBuiltinTypeName T) then throw "Cannot cast value of type ${typeName x} to non-builtin type ${print T}"
        else let TWrapped = Builtin.FromT T;
             in if !(typeEq x.Type TWrapped) then "Cannot cast value of type ${typeName x} to type ${print T} via wrapping as ${TWrapped.name}"
        else unwrap x
      # maybe upcast a builtin
      else
        let xTWrapped = Builtin.FromT (typeOf x);
        in if !(typeEq xTWrapped T) then "Cannot cast value of type ${typeName x} to type ${print T} via wrapping as ${xTWrapped.name}"
        else wrap x;

    # The root type of all types, including itself.
    Type = mkType "Type" {};

    # Is the given type equivalent to Type?
    isRootType = T: T.name or "" == Type.name;

    # Check if a given argument is a Type.
    isType = hasType Types.Type;

    # Get the name of a type whether builtin or user-defined.
    typeName = x:
      if isBuiltinValue x then typeOf x else x.Type.name;

    # Check two types for equality, supporting both Types and builtin type-name strings.
    typeEq = T: U:
      if isType T && isType U then T.name == U.name
      else let
        T_ = maybeFromT T;
        U_ = maybeFromT U;
      in
        if T_ == null then throw "typeEq: ${print T} is not a Type or builtin type"
        else if U_ == null then throw "typeEq: ${print U} is not a Type or builtin type"
        else typeEq T_ U_;

    # Check a string or custom type against a value.
    hasType = T: x:
      if isBuiltinValue x
        then
          if isString T
          then typeOf x == T
          else throw "Cannot check type of builtin value against non-string type ${T.name}"
        else
          if isString T
            then false
            else T.check x;

    # Construct a type from spec attrs:
    # {
    #   name = "TypeName";
    #   Super = SuperType (default: Type);
    #   fields = [ (Field (Of Type) ... "fieldname") ... ]
    #   ctor = some: args: {field attrs to be passed to mk}
    #   allowUnknownFields = false (default)
    #   checkType = that: <any additional type checks> (optional)
    #   checkValue = that: <any value checks> (optional)
    #   overrideCheck = that: <completely take over check> (optional)
    # }
    mkType = name: spec:
      let This = rec {
        inherit name;
        Super = spec.Super or Types.Type;
        Type = Types.Type;
        ctor = spec.ctor or null;

        __toString = this: name;
        __allowUnknownFields = spec.allowUnknownFields or false;
        __allFields =
          if isRootType This then {}
          else Super.__allFields // This.__fields;
        __allMethods =
          if isRootType This then This.__methods or {}
          else Super.__allMethods // This.__methods or {};
        __allStatic =
          if isRootType This then This.__static or {}
          else Super.__allStatic // This.__static or {};

        __fields = keyByName (must "list" (spec.fields or []));
        __methods = must "set" (spec.methods or {});
        __static = must "set" (spec.static or {});

        checkType = that:
          doChecks [
            {
              cond = This.name == typeName that;
              msg = "Type name check failed for ${This.name} (got ${typeName that})";
            }
            {
              cond = if spec ? checkType then spec.checkType that else true;
              msg = "spec.checkType failed: ${typeName that} is not of type ${This.name}";
            }
          ];

        checkValue = that:
          doChecks [
            {
              cond = if spec ? checkValue then spec.checkValue that else true;
              msg = "spec.checkValue failed: ${toString that} is not a value of type ${This.name}";
            }
          ];

        check =
          if spec ? overrideCheck
          then spec.overrideCheck
          else that: checkType that && checkValue that;

        mk = mkInstance This;
        new = Types.new This;

        # Variadically accept arguments until the ctor is fully applied
      };
      in
        # Bind static at top-level finally.
        This // (mapAttrs (name: staticMember: staticMember This) This.__allStatic);

    # Variadic constructor for a type that wraps its arguments.
    # ctor should be a function of e.g.
    # arg1: arg2: arg3: {
    #   field1 = arg1;
    #   field2 = arg2;
    #   field3 = arg3;
    # }
    # where arg1/2/3 can be assumed to be of the correct type (and wrapped if builtin).
    new = T:
      if (T.ctor == null)
        then throw "${typeName T}: No constructor provided for new"
        else
          let go = ctorOrArg:
                if isFunction ctorOrArg
                then let ctor = ctorOrArg; in arg: go (ctor (maybeWrap arg))
                else let arg = ctorOrArg; in T.mk arg;
          in go T.ctor;

    # Construct an instance from a This type and attrset.
    # arg = { fieldName = value, ... } for all fields.
    # Fields with defaults can be omitted.
    mkInstance = This: arg:
      let
        # Construct the parent instance for merging with the new instance.
        # Temporarily enable unknown fields for constructing super to avoid failing
        # on This-defined fields.
        # Any still-unknown fields provided will be caught by checking the expected
        # fields of the entire inheritance chain after 'this' is created.
        super = mkInstance (This.Super // { __allowUnknownFields = true; }) arg;

        # Set the Type field on the instance.
        setType = this: this // { Type = This; };

        # Set the super value such that instance methods can access this.super
        # when overriding superclass methods, as well as any field values on the
        # superclass that may behave differently (i.e. different defaults).
        # this.super essentially acts as though we instantiated Super directly
        # with the subset of the This fields that are not overridden.
        setSuper = this: this // { inherit super; };

        setByName = this: fieldName: value:
          let fieldExists = this.Type.__allFields ? ${fieldName};
              field = this.Type.__allFields.${fieldName};
          in assert (doChecks [
              { cond = fieldExists;
                msg = "Setting unknown field ${fieldName} on ${field.fieldType.name}"; }
              { cond = !fieldExists || hasType field.fieldType value;
                msg = "Invalid type for ${typeName this}.${fieldName}: Wanted ${field.fieldType}, got ${typeName value}"; }
            ]);
            this // { ${fieldName} = wrap value; };

        # Set all fields on the instance.
        # Occurs before methods are set, so cannot use the this.set interface directly.
        setFields = this:
          foldl'
            (this: field:
              let hasValue = arg ? ${field.name} || field ? default;
                  value = arg.${field.name} or field.default;
              in if hasValue
                 then setByName this field.name value
                 else this
            )
            this
            (attrValues This.__fields); # Not __allFields as these will be set in Super init

        # Methods to include on all instances.
        defaultMethods = rec {
          # Enable setting by string fieldname
          # e.g. this.setByName "myInt" 123 -> this'
          #      this.setByName "myInt" "123" -> throws Type error
          inherit setByName;

          # Field setting interface
          # e.g. this.set.someInt 123 -> this'
          #      this.set.someInt "123" -> throws Type error
          set = this: mapAttrs (fieldName: _: x: setByName this fieldName x) this.Type.__allFields;

          # Field modification interface
          # e.g. this.modify.someInt (x: x+1) -> this'
          #      this.modify.someInt toString -> throws Type error
          modify = this: mapAttrs (fieldName: x: f: this.set.${fieldName} (f x));

          # String representation of the instance.
          # Since this is available as a nullary this.__toString function, it passes
          # the builtins.toString (x ? __toString) check and will be used as the
          # default string repr for printing.
          __toString =
            let
            in this: indent.block ''
              (${this.Type.name} {
                ${indent.here
                    (joinLines
                      (map
                        (fieldName:
                          let fieldValue = this.${fieldName};
                          in "${fieldName} = ${log.print fieldValue};")
                        (attrNames this.Type.__allFields)))}
              })
            '';
        };

        # Copy over already-bound static methods to the instance before binding
        # instance methods.
        setStaticMethods = this:
          let
            boundStaticMethods = mapAttrs (name: staticMethod: staticMethod This) This.__allStatic;
          in this // boundStaticMethods;

        # Bind defined methods to 'this' in the first argument
        # Must be called last in initialization so that bound methods see the
        # fully initialized instance.
        finallySetMethods = this:
          let
            this_ = this // boundMethods;
            methods = defaultMethods // This.__allMethods;
            # Bind methods recursively to the final this_ that has all methods set such
            # that they can call one another, otherwise in a left-fold earlier-defined
            # methods would not be able to call later-defined methods.
            boundMethods = concatMapAttrs (name: method: method this_) methods;
           in this_;

        # Construct the This instance.
        this =
          {} |> setType
             |> setSuper
             |> setStaticMethods
             |> setFields
             |> finallySetMethods;

        checks =
          let
            # Get any supplied fields not present in this or any supertype.
            unknownFields = removeAttrs arg (attrNames This.__allFields);

            # Get any supplied fields not present in this or any supertype.
            missingFields = removeAttrs This.__allFields (attrNames this);
          in [
            {
              cond = This.__allowUnknownFields || unknownFields == {};
              msg = "${typeName this}: Unknown fields in mkInstance call: ${joinSep ", " (attrNames unknownFields)}";
            }
            {
              cond = missingFields == {};
              msg = "${typeName this}: Missing fields in mkInstance call: ${joinSep ", " (attrNames missingFields)}";
            }
          ];
      in
        # Base case of Type.new == {} avoiding recursively constructing Super beyond the root Type.
        if isRootType This then {}
        # Otherwise validate and return the new instance.
        else assert (doChecks checks); this;

    Any = mkType "Any" {
      overrideCheck = that: true;
    };

    Union = Ts: mkType "Union[${joinSep ", " (sort (map (T: T.name) Ts))}]" {
      overrideCheck = that: any (T: T.check that) Ts;
    };

    NullOr = T: mkType "NullOr" {
      fields = [(Field (Of (Union [Null T])) "value")];
      static = {__isValueType = This: true;};
    };

    # Construct a new type that wraps another in its value field.
    # MyInt = NewType "MyInt" Int;
    # x = MyInt.from 123;
    # x.value == 123;
    NewType = name: T: mkType name {
      fields = [(Field (Of T) "value")];
      static = {
        __isValueType = This: true;
      };
      ctor = value: { inherit value; };
    };

    TypeClass = mkType "TypeClass" [
      (Field (Of String) "name")
      (Field (Of Set) (_.Default {}) "methods")
      (Field (Of Bool) (_.Default true) "embedWithPrefix")
    ];

    ToString = TypeClass.mk {
      name = "ToString";
      embedWithPrefix = false;
      methods = {
        toString = builtins.toString;
      };
    };

    withTypeClass = TC:
      let tcAttrs =
            concatMapAttrs
              (methodName: method:
                let embedName = joinOptionals [
                      (optionalString TC.embedWithPrefix "__${TC.name}")
                      "__${methodName}"
                    ];
                in { ${embedName} = method; })
              TC.methods;
      in T // tcAttrs;

    # Inline assertion for runtime type assertion.
    must = T: x:
      if hasType T x then x
      else throw "Expected type ${T} (got ${typeName x})";

    mkEnum = enumName: itemNames:
      let
        indexes = range 0 (length itemNames - 1);
        EnumName = mkType enumName {
          fields = [
            (Field (Of Int) "index")
            (Field (Of String) "name")
          ];
          ctor = index: name: { inherit index name; };
          static = {
            # Enum members live on the Enum type itself.
            __items = This: zipListsWith EnumName.new indexes names;
            __indexToItem = This: keyByF (item: item.index) __items;
            __nameToItem = This: keyByName __items;
            fromIndex = This: index: This.__indexToItem.${index} or throw "Invalid index in enum ${enumName}: ${toString index}";
            fromName = This: name: This.__nameToItem.${name} or throw "Invalid name in enum ${enumName}: ${name}";
          };
        };
      in EnumName;

    # Variadic field constructor and argument tags.
    Field = mkVariadic {
      # Stop when we reach the field name
      isTerminal = isString;
      # Field name gets inserted as 'name'
      handle = params: arg:
        if isString arg
        then params // { name = arg; }
        else params // arg;
    };
    # Argument tag setting the type of a field.
    Of = T: { fieldType = T; };

    # Argument tag setting the default value of a field.
    Default = value: { default = value; };
  };

  # nix eval --impure --expr '(import ./cutils/types.nix {})._tests'
  _tests =
    with Types;
    with Builtin;
    with cutils.tests;
    let
      mkBuiltinTest = T: name: rawValue: {
        expr =
          let x = T.new rawValue;
          in {
            name = typeName x;
            value = if name == "Lambda" then null else x.value;
            newIsBuiltin = isBuiltinValue x;
            rawIsBuiltin = isBuiltinValue x.value;
          };
        expected = {
          inherit name;
          value = if name == "Lambda" then null else rawValue;
          newIsBuiltin = false;
          rawIsBuiltin = true;
        };
      };

      MyString = NewType "MyString" String;

      MyType = mkType "MyType" {
        fields = [
          (Field (Of String) "myField")
        ];
        ctor = myField: { inherit myField; };
        methods = {
          helloMyField = this: extra:
            "Hello, ${this.myField.value}${extra}";
        };
      };

      MyType2 = mkType "MyType2" {
        fields = [
          (Field (Of String) "stringField")
          (Field (Of Int) "intField")
          (Field (Of Int) (Default (wrap 666)) "defaultIntField")
        ];
      };

    in cutils.tests.suite {
      types = {
        Null = mkBuiltinTest Null "Null" null;
        Int = mkBuiltinTest Int "Int" 123;
        Float = mkBuiltinTest Float "Float" 12.3;
        String = mkBuiltinTest String "String" "Hello, world!";
        Path = mkBuiltinTest Path "Path" ./.;
        Bool = mkBuiltinTest Bool "Bool" true;
        List = mkBuiltinTest List "List" [1 2 3];
        Set = mkBuiltinTest Set "Set" {a = 1; b = 2; c = 3;};
        Lambda = mkBuiltinTest Lambda "Lambda" (a: b: 123);

        MyString_mk = {
          expr = (MyString.mk { value = String.mk {value = "hello";}; }).value.value;
          expected = "hello";
        };

        MyString_new = {
          expr = (MyString.new (String.new "hello")).value.value;
          expected = "hello";
        };

        MyString_new_raw = {
          expr = (MyString.new "hello").value.value;
          expected = "hello";
        };

        MyType_mk = {
          expr = (MyType.mk { myField = wrap "World"; }).myField.value;
          expected = "World";
        };

        MyType_new = {
          expr = (MyType.new (wrap "World")).myField.value;
          expected = "World";
        };

        MyType_call = {
          expr =
            let this = MyType.new (wrap "World");
             in this.helloMyField "!";
          expected = "Hello, World!";
        };

        MyType_set = {
          expr =
            let this = MyType.new (wrap "");
            in [
              (this.helloMyField "!")
              ((this.set.myField (wrap "World")).helloMyField "!")
              ((this.setByName "myField" (wrap "World")).helloMyField "!")
            ];
          expected = [ "Hello, !" "Hello, World!" "Hello, World!"];
        };

        MyType2_mk_overrideDefault = {
          expr =
            let this = MyType2.mk {
                  stringField = wrap "hi";
                  intField = wrap 123;
                  defaultIntField = wrap 7;
                };
            in [this.stringField.value this.intField.value this.defaultIntField.value];
          expected = ["hi" 123 7];
        };

        MyType2_mk_missingRequired = {
          expr =
            let this = MyType2.mk {
                  intField = wrap 123;
                  defaultIntField = wrap 7;
                };
            in builtins.tryEval this;
          expected = expect.failure;
        };

        MyType2_mk_missingDefault = {
          expr =
            let this = MyType2.mk {
                  intField = wrap 123;
                  stringField = wrap "hi";
                };
            in this.defaultIntField.value;
          expected = 666;
        };

        MyType2_mk_wrongType = {
          expr =
            let this = MyType2.mk {
                  intField = wrap 123;
                  stringField = wrap true;
                  defaultIntField = wrap 7;
                };
            in builtins.tryEval this;
          expected = expect.failure;
        };
      };

        # TODO:
        # Inheritance
        # Type classes
        # Unions
        # Enums
        # Optionals
    };

}

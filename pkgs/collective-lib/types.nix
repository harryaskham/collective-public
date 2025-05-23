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
#   fields = {
#     name = String;
#     nickname = NullOr String;
#     age = Int;
#   };
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

  assertBuiltinType = builtinTypeName: x:
    unless
      (builtinTypeName == typeOf x)
      throw "Expected builtin ${builtinTypeName} but got ${typeOf x}: ${x}";

  Builtin =
    with Types;
    let mkBuiltin = name: builtinType: isType:
          mkType name {
            fields = {
              value = builtinType;
            };
            staticMethods = {
              __builtinTypeName = This: builtinType;
              __checkBuiltin = This: isType;
            };
            # For a builtin we already wrapped the value for "New"
            # so we can unpack the value here
            ctor = value: { inherit value; };
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
        if (!isString builtinType) then null
        else {
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
    # All builtins are attrs. The short-circuit stops recursive inspection of Type.
    isBuiltinValue = x: !(isAttrs x) || (x ? Type);

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

    # Cast between builtin and Builtin types.
    # Returns an error set if the cast is not possible.
    mkCastError = msg: { castError = msg; };
    isCastError = x: x ? castError;
    cast = T: x:
      # noop if same type
      if hasType T x then x
      # maybe downcast a Builtin
      else if isTyped x
        then if (!isBuiltinTypeName T) then mkCastError "Cannot cast value of type ${typeName x} to non-builtin type ${log.print T}"
        else let TWrapped = Builtin.FromT T;
             in if !(typeEq x.Type TWrapped) then mkCastError "Cannot cast value of type ${typeName x} to type ${log.print T} via wrapping as ${TWrapped.name}"
        else unwrap x
      # maybe upcast a builtin
      else
        let xTWrapped = Builtin.FromT (typeOf x);
        in if !(typeEq xTWrapped T) then mkCastError "Cannot cast value of type ${typeName x} to type ${log.print T} via wrapping as ${xTWrapped.name}"
        else wrap x;

    # The root type of all types, including itself.
    Type = mkType "Type" {
      staticMethods = {
        # __toString needs to take a this parameter for toString compliance.
        # Therefore we have Type.staticMethods.__toString which ends up on instances as (instance.__toString instance) which is correct.
        __toString = This: this:
          if typeEq This this then "(${This.name})"
          else "(${This.name} ${log.print (this.get or {})})";
      };
    };

    # Is the given type equivalent to Type?
    isRootType = T: T.name or "" == "Type";

    # Check if a given argument is a custom Type.
    isTyped = x: isAttrs x && x ? Type;

    # Check if a given argument is a Type.
    isType = T: isTyped T && (isRootType T || (T ? Super && isType T.Super));

    # Get the name of a type whether builtin or Builtin.
    typeName = x:
      if isBuiltinValue x then typeOf x else x.Type.name;

    # Check two types for equality, supporting both Types and builtin type-name strings.
    typeEq = T: U:
      if isType T && !isType U then false
      else if !isType T && isType U then false
      else if isType T && isType U then T.name == U.name
      else let
        T_ = Builtin.maybeFromT T;
        U_ = Builtin.maybeFromT U;
      in
        if T_ == null then throw "typeEq: ${log.print T} is not a Type or builtin type"
        else if U_ == null then throw "typeEq: ${log.print U} is not a Type or builtin type"
        else typeEq T_ U_;

    # Check a string or custom type against a value.
    hasType = T: x:
      if isString T then typeOf x == T
      else T.check x;

    # Construct a type from spec attrs:
    # {
    #   name = "TypeName";
    #   Super = SuperType (default: Type);
    #   fields = {
    #     fieldname = Type;
    #     defaultfieldname = Default Type 123;
    #     ...
    #   }
    #   ctor = some: args: {field attrs to be passed to mk}
    #   allowUnknownFields = false (default)
    #   checkType = that: <any additional type checks> (optional)
    #   checkValue = that: <any value checks> (optional)
    #   overrideCheck = that: <completely take over check> (optional)
    # }
    mkType = name: spec:
      # TODO: This should be implemented in terms of Type.new where each type is just
      # an instance of Type.
      let 
        This = rec {
        inherit name;
        Super = if isRootType This then null else spec.Super or Types.Type;
        Type = Types.Type;

        __ctor = spec.ctor or null;
        __fields = must "set" (mapAttrs mkField (spec.fields or {}));
        __methods = must "set" (spec.methods or {});
        __staticMethods = must "set" (spec.staticMethods or {});
        __allowUnknownFields = spec.allowUnknownFields or false;

        __allFields =
          __fields
          // (if isRootType This then {} else Super.__allFields);
        __allMethods =
          __methods
          // (if isRootType This then {} else Super.__allMethods);
        __allStaticMethods =
          __staticMethods
          // (if isRootType This then {} else Super.__allStaticMethods); 

        checkType = that:
          doChecksNoAssert [
            {
              cond = isTyped that && This.name == that.Type.name;
              msg = "Type name check failed for ${This.name} (got ${typeName that})";
            }
            {
              cond = if spec ? checkType then spec.checkType that else true;
              msg = "spec.checkType failed: ${typeName that} is not of type ${This.name}";
            }
          ];

        checkValue = that:
          doChecksNoAssert [
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
        # Bind staticMethods at top-level finally.
        # TODO: Replace with Type using mkInstance
        This // (mapAttrs (name: staticMethod: staticMethod This) This.__allStaticMethods);

    # Variadic constructor for a type that wraps its arguments.
    # ctor should be a function of e.g.
    # arg1: arg2: arg3: {
    #   field1 = arg1;
    #   field2 = arg2;
    #   field3 = arg3;
    # }
    # where arg1/2/3 can be assumed to be of the correct type (and wrapped if builtin).
    new = T:
      if (T.__ctor == null)
        then throw "${typeName T}: No constructor provided for new"
        else
          let go = ctorOrArg:
                if isFunction ctorOrArg
                then let ctor = ctorOrArg; in arg: go (ctor arg)
                else let arg = ctorOrArg; in T.mk arg;
          in go T.__ctor;

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
        super = 
          if isRootType This then {}
          else mkInstance (This.Super // { __allowUnknownFields = true; }) arg;

        # Set the Type field on the instance.
        setType = this: this // { Type = This; };

        # Set the super value such that instance methods can access this.super
        # when overriding superclass methods, as well as any field values on the
        # superclass that may behave differently (i.e. different defaults).
        # this.super essentially acts as though we instantiated Super directly
        # with the subset of the This fields that are not overridden.
        setSuper = this:
          super // this // { 
            # Inherit super as this.super
            inherit super;
          };

        # Accessors to include on all instances.
        # Since modifying here requires a new instance with updated accessors, and we don't
        # want to have to call as e.g. (this.fn this arg1 arg2) we need to set the accessors
        # again here in 'set' bound to the new instance.
        setAccessors = this: this // rec {
          # Field getting interface
          # Fields are also accessible at this.fieldName but alongside methods and static methods
          # This set will be constructable at this point and should not incur infinite recursion when
          # accessed in __toString below
          # e.g. this.someInt -> 123
          #      this.get.someInt -> 123
          #      this.notAField -> throws error
          #      this.get.notAField -> throws error
          #      this.get.notAField or default -> default
          get = mapAttrs (fieldName: _: this.${fieldName}) This.__allFields;

          # Field checking interface.
          # e.g. this.has.someInt -> true
          #      this.has ? someInt -> true
          #      this.has.notAField -> throws error
          #      this.has ? notAField -> false
          #      this.has.notAField or default -> default
          has = mapAttrs (_: _: true) This.__allFields;

          # Field setting interface
          # We need to setAccessors again here to update the logical binding for future updates
          # e.g. this.set.someInt 123 -> this'
          #      this.set.someInt "123" -> throws Type error
          set = 
            let
              setByName = this: fieldName: uncastValue:
                let field = This.__allFields.${fieldName} or null;
                    castValue = cast field.fieldType uncastValue;
                in assert (predChecks [
                    { pred = (field: field != null);
                      msg = joinLines [
                        "Setting unknown field: ${This.name}.${fieldName}"
                        "Known fields: ${joinSep ", " (attrNames This.__allFields)}"
                      ];
                    }
                    { pred = (field: !(isCastError castValue));
                      msg = "Error casting field assignment for ${This.name}.${fieldName}: ${castValue.castError}"; }
                  ] field);
                  reinitThis (this // { 
                    ${fieldName} = castValue;
                  });
            in 
              mapAttrs 
                (fieldName: _: x: setByName this fieldName x)
                This.__allFields;

          # Field modification interface
          # e.g. this.modify.someInt (x: x+1) -> this'
          #      this.modify.someInt toString -> throws Type error
          modify = mapAttrs (fieldName: x: f: set.${fieldName} (f x));

          # Method calling interface
          # We can't bind recursively in general but for .call we bind to this_ so that
          # get/has/set/modify/call all work.
          # Nested this.call should work as each subsequent nest adds another layer of call
          # e.g. this.call.someMethod arg1 arg2 arg3 -> return value
          call = mapAttrs (_: method: method (reinitThis this)) This.__allMethods;
        };

        # Set all fields on the instance
        # Occurs before methods are set, so cannot use the this.set interface directly.
        setFields = this:
          foldl'
            (this: field:
              let hasValue = arg ? ${field.name} || field ? default;
                  value = arg.${field.name} or field.default;
              in if hasValue
                then this.set.${field.name} value
                else this
            )
            this
            (attrValues This.__fields); # Not __allFields as these will be set in Super init


        # Copy over already-bound static methods to the instance before binding
        # instance methods.
        setStaticMethods = this:
          let boundStaticMethods = 
                mapAttrs 
                  (name: staticMethod: staticMethod This)
                  This.__allStaticMethods;
          in this // boundStaticMethods;

        # Construct the This instance.
        initThis = x:
          x
            |> setType
            |> setSuper
            |> setStaticMethods
            |> setAccessors
            |> setFields;

        # Reconstruct the This instance after change.
        reinitThis = setAccessors;

        this = initThis {};

        checks =
          let
            # Get any supplied fields not present in this or any supertype.
            allFieldNames = attrNames This.__allFields;
            unknownFieldNames = attrNames (removeAttrs arg allFieldNames);

            # Get any fields not populated in this or any supertype.
            populatedFieldNames = filter (name: this ? ${name}) allFieldNames;
            missingFieldNames = attrNames (removeAttrs This.__allFields populatedFieldNames);
          in [
            {
              cond = This.__allowUnknownFields || unknownFieldNames == [];
              msg = "${This.name}: Unknown fields in mkInstance call: ${joinSep ", " unknownFieldNames}";
            }
            {
              cond = missingFieldNames == [];
              msg = ''
                ${This.name}: Missing fields in mkInstance call: ${joinSep ", " missingFieldNames}
                arg: ${log.print arg}
                Super: ${log.print This.Super}
                super: ${log.print super}
                This: ${log.printAttrs This}
                this: ${log.print this}
              '';
            }
          ];
      
      in assert (doChecks checks); this;

    Any = mkType "Any" {
      overrideCheck = that: true;
    };

    Union = Ts: mkType "Union[${joinSep ", " (sort (map (T: T.name) Ts))}]" {
      overrideCheck = that: any (T: T.check that) Ts;
    };

    NullOr = T: mkType "NullOr" {
      fields = {
        value = Union [Null T];
      };
      ctor = value: { inherit value; };
    };

    # Construct a new type that wraps another in its value field.
    # MyInt = NewType "MyInt" Int;
    # x = MyInt.new 123;
    # x.value == 123;
    # x.get.value == 123;
    NewType = name: T: mkType name {
      fields = {
        value = T;
      };
      ctor = value: { inherit value; };
    };

    # Inline assertion for runtime type assertion.
    must = T: x:
      if hasType T x then x
      else throw "Expected type ${T} (got ${typeName x})";

    mkEnum = enumName: itemNames:
      let
        indexes = range 0 (length itemNames - 1);
        EnumName = mkType enumName {
          fields = {
            index = Int;
            name = String;
          };
          ctor = index: name: { inherit index name; };
          staticMethods = {
            # Enum members live on the Enum type itself.
            __items = This: zipListsWith EnumName.new indexes names;
            __indexToItem = This: keyByF (item: item.index) __items;
            __nameToItem = This: keyByName __items;
            fromIndex = This: index: This.__indexToItem.${index} or throw "Invalid index in enum ${enumName}: ${toString index}";
            fromName = This: name: This.__nameToItem.${name} or throw "Invalid name in enum ${enumName}: ${name}";
          };
        };
      in EnumName;

    Default = T: def: { Type = T; Default = def; };
    mkField = name: spec:
      if spec ? Default
        then { name = name; fieldType = spec.Type; default = spec.Default; }
        else { name = name; fieldType = spec; };
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
        fields = {
          myField = String;
        };
        ctor = myField: { inherit myField; };
        methods = {
          helloMyField = this: extra:
            "Hello, ${this.myField.value}${extra}";
        };
      };

      MyType2 = mkType "MyType2" {
        fields = {
          stringField = String;
          intField = Int;
          defaultIntField = Default Int 666;
        };
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
             in this.call.helloMyField "!";
          expected = "Hello, World!";
        };

        MyType_set = {
          expr =
            let this = MyType.new (wrap "");
            in [
              (this.call.helloMyField "!")
              ((this.set.myField (wrap "World")).call.helloMyField "!")
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
        # Maybe / ADTs
  };

}

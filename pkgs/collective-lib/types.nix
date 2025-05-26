{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib,
  cutils ? import ./. { inherit lib; }, ... }:

with lib;
with lib.strings;
with cutils.clib;
with cutils.attrs;
with cutils.functions;
with cutils.errors;
with cutils.strings;

# TODO:
# - SetOf
# - LambdaOf
# - OrderedOf
# - mkSubType
# - Tests for:
#   - Inheritance
#   - Nested calls / chained sets see the correct bound state
#   - ListOf
#   - Type classes
#   - Unions
#   - Enums
#   - Maybe / ADTs
# - Try https://bolt.tvix.dev/

# Simple inheritance-based typesystem for use outside of the module system.
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
# Person = Type.new "Person" {
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

  Types = rec {

    Builtin = rec {

      mkBuiltin =
        name: builtinType: isType: methods: staticMethods_:
          Type.new name {
            fields.value = builtinType;

            staticMethods = {
              __builtinTypeName = This: builtinType;
              __checkBuiltin = This: isType;
              cast = This: x: cast This x;
              is = This: x: !(isCastError This.cast x);
            } // staticMethods_;

            methods = {
              mapBuiltin = this: this.modify.value;
            } // methods;

            checkValue = that: isType that.value;
          };

      Null = mkBuiltin "Null" "null" isNull {} {};

      Int = mkBuiltin "Int" "int" isInt {} {};

      Float = mkBuiltin "Float" "float" isFloat {} {};

      String = mkBuiltin "String" "string" isString {
        size = this: stringLength this.value;
      } {};

      Path = mkBuiltin "Path" "path" isPath {
        size = this: stringLength (toString this.value);
      } {};

      Bool = mkBuiltin "Bool" "bool" isBool {} {};

      List = mkBuiltin "List" "list" isList {
        fmap = this: f: this.modify.value (map f);
        size = this: length this.value;
        append = this: x: this.modify.value (xs: xs ++ [x]);
      } {};

      Set = mkBuiltin "Set" "set" (x: isAttrs x && !(x ? Type)) {
        fmap = this: f: this.modify.value (mapAttrs (_: f));
        size = this: length this.attrNames;
        attrNames = this: attrNames this.value;
        attrValues = this: attrValues this.value;
        # e.g. this.modifyAt.name (x: x+1)
        getAt = this: this.value;
        modifyAt = this: mapAttrs (_: value: f: f value) this.value;
        setAt = this: mapAttrs (_: modifyHere: x: modifyHere (const x)) this.modifyAt;
        setAtName = this: name: value:
          if this.setAt ? ${name}
          then this.setAt.${name} value
          else this.modify.value (xs: xs // {${name} = value;});
      };

      Lambda = mkBuiltin "Lambda" "lambda" isFunction {
        fmap = this: f: this.modify.value (compose f);
      } {};

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

    # Whether or not x is a builtin type
    # All builtins are attrs. The short-circuit stops recursive inspection of Type.
    isBuiltinValue = x:
      if isAttrs x then !(x ? Type)
      else true;

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
      # Noop if same type
      # Note that this does not call T.check, so if the value has been manipulated
      # to be invalid, it will not throw an error.
      if hasType T x then x
      # maybe downcast a Builtin
      else if isTyped x
      then if (!isBuiltinTypeName T) then mkCastError (indent.block ''
          Cannot cast value of type ${typeName x} to ${log.print T}:
            ${indent.here (log.print x)}
        '')
        else let TWrapped = Builtin.FromT T;
             in if !(typeEq x.Type TWrapped) then mkCastError "Cannot cast value of type ${typeName x} to type ${typeName T} via wrapping as ${typeName}"
        else unwrap x
      # maybe upcast a builtin
      else
        let xTWrapped = Builtin.FromT (typeOf x);
        in if !(typeEq xTWrapped T) then mkCastError "Cannot cast value of type ${typeName x} to type ${typeName T} via wrapping as ${typeName xTWrapped}"
        else wrap x;

    # Merge the static data of a type, preferring children over parents.
    mergeSuper = getFn: This:
      let xs = getFn This;
      in if !(This ? Super) || This.Super == null || typeEq (This.Super {}) This
         then xs
         else (mergeSuper getFn (This.Super {})) // xs;

    # A constraint on a type variable.
    Constraint = Type.new "Constraint" {
      fields.constraintType = Type;
      methods = {
        # Whether a given type variable binding satisfies the constraint.
        # If the constraint is unbound, we treat as satisfied, but instantiating the unbound type
        # will throw an error.
        satisfiedBy = this: That:
          typeEq Unbound That
          || That.isInstance this.constraintType;
      };
    };

    # Modify an argument set to inherit from a supertype.
    # Unless ctor is set explicitly in the child, this will also inherit the supertype's constructor.
    inheritFrom = Super: args: {
      inherit Super;
      inherit (Super) ctor;
    } // args;

    # The way a Type is represented when embededd as a value inside a Type.
    # Made thunk-lazy to avoid recursion errors in bootstrapping Type.
    # TODO: Typed lambda
    TypeThunk = "lambda";
    # The way a Fields instance is represented when embedded as a value inside a Type.
    # A lambda from This->Fields
    FieldsThunk = "lambda";

    # Bootstrap the definition of type by calling unbound-new on Prototype.
    # Prototype is manually constructed without any access to static methods or fields.
    # MetaType is instantiated as an instance of Prototype, inheriting its methods and fields.
    # Type is instantiated as an instance of MetaType, which then overrides its fields to correctly typed versions
    MetaType = mkInstance (Prototype // {
      name = "MetaType";
    });

    Type = MetaType.new "Type" (Prototype // {
      fields = Fields.new {
        # The name of the type.
        name = String;
        # The type of the instance as a thunk.
        Type = (Default TypeThunk).new Types.Type;
        # The supertype of the type.
        Super = (Default (NullOr TypeThunk)).new Types.Type;
        # The type parameters of the type.
        tvars = (Default (NullOr (SetOf Constraint))).new null;
        # The type parameter bindings of the type.
        tvarBindings = (Default (NullOr (SetOf TypeThunk))).new null;
        # The constructor function creating the fields of the type as a set to pass to mk.
        ctor = (Default Ctor).new Ctors.Fields;
        # A set of ordered fields to make available as this.___ and this.get.___, this.set.___, etc
        fields = (Default FieldsThunk).new (_: Fields.new {});
        # A set of methods from this to make available as this.___
        methods = (Default (SetOf Lambda)).new {};
        # A set of methods from This to make available as This.___ and this.___
        staticMethods = (Default (SetOf Lambda)).new {};
        # Perform additional checks on the value of the type when comparing.
        checkValue = (Default (NullOr Lambda)).new null;
        # If set, ignore all other checks and use this check function only.
        overrideCheck = (Default (NullOr Lambda)) null;
      };
    });

    protoValue = x: { value = x; };
    protoDefaultValue = x: { defaultValue = x; };
    protoFields = {
      indexed = {
        # name = { fieldStatic = false; index = 0; fieldName = "name"; fieldType = String;};
        # Type = { fieldStatic = false; index = 1; fieldName = "Type"; fieldType = TypeThunk; fieldDefault = protoDefaultValue Types.Type;};
        # Super = { fieldStatic = false; index = 2; fieldName = "Super"; fieldType = NullOr TypeThunk; fieldDefault = protoDefaultValue Types.Type;};
        # tvars = { fieldStatic = false; index = 3; fieldName = "tvars"; fieldType = NullOr (SetOf Constraint); fieldDefault = null;};
        # tvarBindings = { fieldStatic = false; index = 4; fieldName = "tvarBindings"; fieldType = NullOr (SetOf TypeThunk); fieldDefault = null;};
        # ctor = { fieldStatic = false; index = 5; fieldName = "ctor"; fieldType = Ctor; fieldDefault = protoDefaultValue Ctors.Fields;};
        # fields = { fieldStatic = false; index = 6; fieldName = "fields"; fieldType = Fields; fieldDefault = protoDefaultValue (Fields.new {}); };
        # methods = { fieldStatic = false; index = 7; fieldName = "methods"; fieldType = SetOf Lambda; fieldDefault = protoDefaultValue {};};
        # staticMethods = { fieldStatic = false; index = 8; fieldName = "staticMethods"; fieldType = SetOf Lambda; fieldDefault = protoDefaultValue {};};
        # checkValue = { fieldStatic = false; index = 9; fieldName = "checkValue"; fieldType = NullOr Lambda; fieldDefault = null;};
        # overrideCheck = { fieldStatic = false; index = 10; fieldName = "overrideCheck"; fieldType = NullOr Lambda; fieldDefault = null;};
        name = { fieldStatic = false; index = 0; fieldName = "name"; fieldType = null; fieldDefault = null;};
        Type = { fieldStatic = false; index = 1; fieldName = "Type"; fieldType = null; fieldDefault = null;};
        Super = { fieldStatic = false; index = 2; fieldName = "Super"; fieldType = null; fieldDefault = null;};
        tvars = { fieldStatic = false; index = 3; fieldName = "tvars"; fieldType = null; fieldDefault = null;};
        tvarBindings = { fieldStatic = false; index = 4; fieldName = "tvarBindings"; fieldType = null; fieldDefault = null;};
        ctor = { fieldStatic = false; index = 5; fieldName = "ctor"; fieldType = null; fieldDefault = null;};
        fields = { fieldStatic = false; index = 6; fieldName = "fields"; fieldType = null; fieldDefault = null;};
        methods = { fieldStatic = false; index = 7; fieldName = "methods"; fieldType = null; fieldDefault = null;};
        staticMethods = { fieldStatic = false; index = 8; fieldName = "staticMethods"; fieldType = null; fieldDefault = null;};
        checkValue = { fieldStatic = false; index = 9; fieldName = "checkValue"; fieldType = null; fieldDefault = null;};
        overrideCheck = { fieldStatic = false; index = 10; fieldName = "overrideCheck"; fieldType = null; fieldDefault = null;};
      };
    };
    # The Prototype of all Types.
    # Construct the minimal base type manually in order to enable instantiating Type.
    Prototype =
      let This = rec {
        name = "Type";
        Type = Prototype;
        Super = null;
        tvars = null;
        tvarBindings = null;
        ctor = This: name: arg: arg // { inherit name; };
        # fields = This: Fields.new {
        #   # The name of the type.
        #   name = String;
        #   # The type of the instance as a thunk.
        #   Type = (Default TypeThunk).new Types.Type;
        #   # The supertype of the type.
        #   Super = (Default (NullOr TypeThunk)).new Types.Type;
        #   # The type parameters of the type.
        #   tvars = (Default (NullOr (SetOf Constraint))).new null;
        #   # The type parameter bindings of the type.
        #   tvarBindings = (Default (NullOr (SetOf TypeThunk))).new null;
        #   # The constructor function creating the fields of the type as a set to pass to mk.
        #   ctor = (Default Ctor).new Ctors.Fields;
        #   # A set of ordered fields to make available as this.___ and this.get.___, this.set.___, etc
        #   fields = (Default FieldsThunk).new (_: Fields.new {});
        #   # A set of methods from this to make available as this.___
        #   methods = (Default (SetOf Lambda)).new {};
        #   # A set of methods from This to make available as This.___ and this.___
        #   staticMethods = (Default (SetOf Lambda)).new {};
        #   # Perform additional checks on the value of the type when comparing.
        #   checkValue = (Default (NullOr Lambda)).new null;
        #   # If set, ignore all other checks and use this check function only.
        #   overrideCheck = (Default (NullOr Lambda)) null;
        # };
        fields = protoFields;
        staticMethods = {};
        methods = {
          # Is this instance an instance of type That, or inherits from it?
          isInstance = this: That:
            typeEq this.Type That
            || (This.Super != null && This.Super.isInstance That);

          # Use the bound name with its ordered param assignments to determine type identity
          # and equality.
          id = This: This.boundName;

          # Get the full templated name of the type.
          boundName = This:
            if This.tvars == null || This.tvars.size == 0 then This.name
            else
              let
                printBinding = tvarName:
                  let T = This.tvarBindings.getAt.${tvarName};
                  in if typeEq Unbound T
                    then tvarName
                    else "${tvarName} = ${typeBoundName T}";
                printBindings = joinSep ", " (map printBinding This.tvars.attrNames);
              in "${This.name}<${printBindings}>";

          # Construct the type resulting from applying the given bindings to the parameterized This type.
          bind = This: tvarBindingsAttrs:
            let
              tvarBindingsList =
                mapAttrsToList
                  (tvarName: T: {inherit tvarName T;})
                  tvarBindingsAttrs;

              bindOne = tvarName: T:
                let
                  throwBindError = msg: joinLines [
                    "bind: Error binding ${tvarName} <- ${typeName T}"
                    msg
                  ];
                  constraint =
                    This.tvars.${tvarName} or
                      (throwBindError "Type ${This.name} does not have type variable ${tvarName}");
                in
                  if This.tvarBindings.getAt ? tvarName
                    then throwBindError "Type ${This.boundName} already has type variable ${tvarName} bound to ${typeName This.tvarBindings.getAt.${tvarName}}"
                  else if !(constraint.satisfiedBy T)
                    then throwBindError "Type ${typeName T} does not satisfy constraint: ${log.print constraint}"
                  else This.modify.tvarBindings (bs: bs.setAt.${tvarName} T);

            in foldl' bindOne This tvarBindingsList;

          # Create a new instance of the type by providing field values
          mk = This: args: mkInstance (args // {
            name = This.name;
            Type = This;
          });

          # Create a new instance of the type by calling This's constructor
          # When This == Type, creates a new Type.
          new = This: Variadic.compose This.mk (This.ctor This);

          # Create a new template of a type accepting type parameters.
          # The parameters can be accessed via the _ argument surrounding the type specification.
          # Returns a function from {bindings} to a new bound type.
          # e.g.
          # MyInt = Int.subtype "MyInt" {};
          # MyTemplate = Type.template "MyTemplate" { T = Type, U = Int; } (_: {
          #   fields = [{ t = _.T;} {u = _.U;}];
          # };
          # then
          # typeOf MyTemplate == lambda
          # (MyTemplate { T = String; U = Int }).new "abc" 123 == MyTemplate<String, Int> {t="abc"; u=123;}
          # (MyTemplate { T = String; U = MyInt }).new "abc" (MyInt.new 123) == MyTemplate<String, MyInt> {t="abc"; u=MyInt 123;}
          # (MyTemplate { T = String; U = Bool }) -> throws binding error, Bool not valid for U's Int constraint
          # MyStringInt = MyTemplate { T = String }) -> MyStringInt == MyTemplate<String, U = Int>, the partially bound template
          # MyStringInt.bind {U = MyInt} -> MyTemplate<String, MyInt>
          template =
            This: name: tvars_: bindingsToArg_:
            let
              tvars = mapAttrs (_: Constraint.new) tvars_;
              bindingsToArg = bindings: (bindingsToArg_ bindings) // {
                inherit tvars;
                tvarBindings = (SetOf Types.Type).new bindings; # TODO: cast
              };
              unboundBindings = mapAttrs (_: _: Unbound) tvars;
              unboundArg = bindingsToArg unboundBindings;
              T = This.new name unboundArg;
            in T.bind;

          # Create a new subtype inheriting from This
          # TODO: Compose checkValue / overrideCheck
          subtype = This: name: args:
            Type.new name (inheritFrom This args);

          # Does this type inherit from That?
          inheritsFrom = This: That:
            typeEq This That
            || (This.Super != null && This.Super.inheritsFrom That);

          check = This: that:
            let
              runChecks = doChecksNoAssert [
                {
                  cond = typeEq This that.Type;
                  msg = "Type check failed for ${This.boundName} (got ${That.boundName})";
                }
                {
                  cond = This.checkValue == null || This.checkValue that;
                  msg = "checkValue failed: ${log.print that} is not a value of type ${This.boundName}";
                }
              ];
            in if This.overrideCheck.value != null
              then This.overrideCheck.value that
              else runChecks;

          # __toString needs to take a this parameter for toString compliance.
          # Therefore we have Type.staticMethods.__toString which ends up on instances as (instance.__toString instance) which is correct.
          # This will work to print both types as T.__toString T and instances as t.__toString t
          __toString = This: this:
            if typeEq This this then This.boundName
            else "${This.boundName} ${log.print (this.get or {})}";
        };
        checkValue = null;
        overrideCheck = null;
      };
      in This;

    Void = Type.new "Void" {
      staticMethods.mk = error "Void.mk: Void has no inhabitants";
    };

    # A type specific to unbound type variables
    Unbound = Void.subtype "Unbound" {};

    # Is the given type equivalent to Type?
    isRootType = typeEq Type;

    # Check if a given argument is a custom Type.
    isTyped = x: isAttrs x && x ? Type;

    # Check if a given argument is a Type.
    isType = T: isTyped T && (isRootType T || (T ? Super && isType T.Super));

    # Gets the type of a value, whether it is a Type or a builtin type.
    getRawType = x:
      if isTyped x then x.Type
      else typeOf x;

    # Gets the type of a value, whether it is a Type or a builtin type.
    getType = x:
      if isTyped x then x.Type
      else Builtin.FromT (typeOf x);

    # Get the name of a type whether builtin or Type.
    typeName = x:
      if isBuiltinValue x then typeOf x else x.Type.name;

    # Get the bound name of a type whether builtin or Type.
    typeBoundName = x:
      if isBuiltinValue x then typeOf x else x.Type.boundName;

    # Check two types for equality, supporting both Types and builtin type-name strings.
    typeEq = T: U:
      if isType T && !isType U then false
      else if !isType T && isType U then false
      else if isType T && isType U then T.id == U.id
      else let
        T_ = Builtin.maybeFromT T;
        U_ = Builtin.maybeFromT U;
      in
        if T_ == null then throw "typeEq: ${log.print T} is not a Type or builtin type"
        else if U_ == null then throw "typeEq: ${log.print U} is not a Type or builtin type"
        else typeEq T_ U_;

    # Check a string or custom type against a value.
    hasType = T: x: typeEq T (getRawType x);

    # A Ctors.CtorName is of form:
    # Ctor.new (This: args...: <field-value attrs to pass to mk)
    Ctor = Type.new "Ctor" {
      fields = {
        __Ctor = Lambda;
      };
    };

    # Constructor presets
    Ctors = rec {
      Nullary = Ctor.new (This: {});

      # Default constructor for regular non-NewType/Builtin/Alias types.
      # Accept fields in order of their index attribute.
      # If This has a supertype, its constructor is variadically precomposed with This.new
      # so that if we have:
      #
      # Super = {fields = {a = String};};
      # This = {
      #   inherit Super;
      #   fields = {b = Int; c = Bool;};
      # };
      #
      # Then:
      #
      # This.new "a" 2 true == This { a = "a"; b = 2; c = true; }
      Fields =
        Ctor.new
          (This:
            let fields =
                  filterAttrs (_: f: !f.fieldStatic) This.fields.indexed;  # Here 'indexed' is manually set on the Prototype
                # Do manually to avoid a new call
                sortedFieldNames = attrNames (sortOn (f: f.index) (mapAttrsToList (_: f: {inherit (f) fieldName index;})));
            in
              if size fields == 0
               then if This.Super == null then {}
                    else This.Super.ctor
              else
                let thisCtor = Variadic.mkOrdered This.fields.attrNames;
                in
                  if This.Super == null
                  then thisCtor
                  else fjoin mergeAttrs thisCtor This.Super.ctor);
    };

    combinedFields = This:
      if This.Super == null
      then This.fields.indexed
      else mergeSuper (T: T.fields.indexed) This;

    combinedMethods = This:
      if This.Super == null
      then This.methods
      else mergeSuper (T: T.methods) This;

    combinedStaticMethods = This:
      if This.Super == null
      then This.staticMethods
      else mergeSuper (T: T.staticMethods) This;

    staticFields = This:
      filterAttrs (_: f: f.fieldStatic) (combinedFields This);

    instanceFields = This:
      filterAttrs (_: f: !f.fieldStatic) (combinedFields This);

    # Construct the parent instance for merging with the new instance.
    # Any still-unknown fields provided will be caught by checking the expected
    # fields of the entire inheritance chain after 'this' is created.
    mkSuperInstance = This: args:
      if !(args ? Super) || args.Super == null then {}
      else
        let superFields = mergeSuper (T: T.fields.getAllFields) Super;
            superArgs = filter (name: _: superFields ? name) args;
        in mkInstance This.Super superArgs;

    setType = This: this: this // { Type = This;};

    # Set the super value such that instance methods can access this.super
    # when overriding superclass methods, as well as any field values on the
    # superclass that may behave differently (i.e. different defaults).
    # this.super essentially acts as though we instantiated Super directly
    # with the subset of the This fields that are not overridden.
    setSuper = This: args: this:
      let super = mkSuperInstance This args;
      in super // this // {
        # Inherit super as this.super
        inherit super;
      };

    # Accessors to include on all instances.
    # Since modifying here requires a new instance with updated accessors, and we don't
    # want to have to call as e.g. (this.fn this arg1 arg2) we need to set the accessors
    # again here in 'set' bound to the new instance.
    setAccessors = This: this: this // rec {
      # Field getting interface
      # Fields are also accessible at this.fieldName but alongside methods and static methods
      # This set will be constructable at this point and should not incur infinite recursion when
      # accessed in __toString below
      # e.g. this.someInt -> 123
      #      this.get.someInt -> 123
      #      this.notAField -> throws error
      #      this.get.notAField -> throws error
      #      this.get.notAField or default -> default
      # Defined with optionalAttrs for the first pass to define .set, where .get will be empty
      get =
        concatMapAttrs
          (fieldName: _: optionalAttrs (this ? ${fieldName}) { ${fieldName} = this.${fieldName}; })
          (combinedFields This);

      # Field checking interface.
      # e.g. this.has.someInt -> true
      #      this.has ? someInt -> true
      #      this.has.notAField -> throws error
      #      this.has ? notAField -> false
      #      this.has.notAField or default -> default
      has = mapAttrs (_: _: true) (combinedFields This);

      # Field setting interface
      # We need to setAccessors again here to update the logical binding for future updates
      # e.g. this.set.someInt 123 -> this'
      #      this.set.someInt "123" -> throws Type error
      set =
        let
          setByName = this: fieldName: uncastValue:
            let field = (combinedFields This).${fieldName} or null;
                castValue =
                  if field.fieldType != null
                    then cast field.fieldType uncastValue
                    else uncastValue;
            in assert (predChecks [
                { pred = (field: field != null);
                  msg = joinLines [
                    "Setting unknown field: ${This.name}.${fieldName}"
                    "Known fields: ${joinSep ", " (attrNames (combinedFields This))}"
                  ];
                }
                { pred = (field: !(isCastError castValue));
                  msg = indent.block ''
                    Error casting field assignment:
                      ${This.boundName}.${fieldName} = ${log.print uncastValue}
                      ${indent.here castValue.castError}
                  '';
                }
                # TODO: This check could be O(n) for container types, updating attrsets goes O(1) to O(n)
                # Need item-wise checks
                { pred = field: field.fieldType == null || field.fieldType.check castValue;
                  msg = indent.block ''
                    Cast value did not pass typecheck:
                      ${This.boundName}.${fieldName} = ${log.print uncastValue}
                      Cast value of ${log.print castValue} is not a valid instance of ${field.fieldType.boundName}.
                  '';
                }
              ] field);
              reinitThis This (this // {
                ${fieldName} = castValue;
              });
        in
          mapAttrs
            (fieldName: _: x: setByName this fieldName x)
            (combinedFields This);

      # Field modification interface
      # e.g. this.modify.someInt (x: x+1) -> this'
      #      this.modify.someInt toString -> throws Type error
      modify = mapAttrs (fieldName: x: f: set.${fieldName} (f x)) (combinedFields This);

      # Method calling interface
      # Nested calls should work as each is bound to this_, the version after the methods are set.
      # e.g. this.call.someMethod arg1 arg2 arg3 -> return value
      call = mapAttrs (_: method: method this) (combinedMethods This);
    };

    # Set all fields on the instance
    # Occurs before methods are set, so cannot use the this.set interface directly.
    setFields = This: args: this:
      foldl'
        (this: field:
          let hasValue = args ? ${field.fieldName} || (field.fieldDefault or null) != null;
              value = args.${field.fieldName} or field.fieldDefault.defaultValue;
          in if hasValue
            then this.set.${field.fieldName} value
            else this
        )
        this
        (attrValues (instanceFields This)); # Not combinedFields as these will be set in Super init

    # Copy over already-bound static methods to the instance before binding
    # instance methods.
    setStaticMethods = This: this:
      let boundStaticMethods =
            mapAttrs
              (name: staticMethod: staticMethod This)
              (combinedStaticMethods This);
      in this // boundStaticMethods;

    # Copy over already-bound static methods to the instance before binding
    # instance methods.
    setMethods = This: this:
      let this_ = this // (mapAttrs (_: method: method this_) (combinedMethods This));
      in this_;

    # Construct the This instance.
    initThis = This: this: args:
      this
        |> (setType This)
        |> (setSuper This args)
        |> (setAccessors This)
        |> (setStaticMethods This)
        |> (setFields This args)
        |> (setMethods This);

    # Reconstruct the This instance after change.
    reinitThis = This: this:
      this
        |> (setAccessors This)
        |> (setStaticMethods This)
        |> (setMethods This);

    # Build a new instance from a single dict of field values.
    # Construct an instance from a This type and attrset of field-to-value assignments.
    # arg = { fieldName = value, ... } for all fields.
    # Fields with defaults can be omitted.
    # Build a new instance from a single dict of field values.
    mkInstance = args:
      let
        This = args.Type;

        # Construct the final instance
        this = initThis This {} args;

        checks =
          let
            # Get any supplied non-static fields not present in this or any supertype.
            allFieldNames = attrNames (mergeSuper (T: T.fields.indexed) This);
            unknownFieldNames = attrNames (removeAttrs args allFieldNames);

            # Get any fields not populated in this or any supertype.
            populatedFieldNames = filter (name: this ? ${name}) allFieldNames;
            requiredFields = filterAttrs (_: field: field.fieldDefault != null) This.fields.indexed;
            missingFieldNames = attrNames (removeAttrs requiredFields populatedFieldNames);
          in [
            {
              cond = unknownFieldNames == [];
              msg = "${This.boundName}: Unknown fields in mkInstance call: ${joinSep ", " unknownFieldNames}";
            }
            {
              cond = missingFieldNames == [];
              msg = ''
                ${This.boundName}: Missing fields in mkInstance call: ${joinSep ", " missingFieldNames}
                args: ${log.print args}
                Super: ${log.print This.Super}
                super: ${log.print super}
                This: ${log.printAttrs This}
                this: ${log.print this}
              '';
            }
          ];

      in assert (doChecks checks); this;

    Any = Type.new "Any" {
      overrideCheck = that: true;
    };

    Union = Type.template "Union" {Ts = ListOf Type;} (_: {
      overrideCheck = that: any (T: T.check that) _.Ts;
    });

    NullOr = T: Union {Ts = [Null T];};

    # Inline assertion for runtime type assertion.
    must = T: x:
      if hasType T x then x
      else throw "Expected type ${T} (got ${typeName x})";

    # A = Type.new "A" {};
    # B = Type.new "B" { Super = A; };
    #
    # isSuperTypeOf A B == true
    # isSuperTypeOf B A == false
    # isSuperTypeOf A A == false
    # isSuperTypeOf Type A == true
    # isSuperTypeOf Type B == true
    isSuperTypeOf = Super: T:
      if T == null then false
      else typeEq Super T.Super || isSuperTypeOf Super T.Super;
    isSubTypeOf = flip isSuperTypeOf;

    ListOf = Type.template "ListOf" {T = Type;} (_: {
      checkValue = that: all (x: hasType _.T x) that.value;
    });

    SetOf = Type.template "SetOf" {T = Type;} (_: {
      checkValue = that: all (x: hasType _.T x) that.attrValues;
    });

    # A type that enforces a size on the value. If n is null, any size is allowed, but .size must be present.
    Sized = n:
      let Sized_ = Type.template "Sized" {N = Literal n; T = Has "size";} (_:
            inheritFrom _.T {
              checkValue = that:
                Super.checkValue that
                && typeEq Any _.N || that.size == _.N.literal;
            });
      in Sized_ {N = Literal n;};

    # An attribute set with attributes zero-indexed by their definition order.
    # xs = Ordered.new [ {c = 1;} {b = 2;} {a = 3;} ];
    # xs.value == { a = 1; b = 2; c = 3; } (arbitrary order)
    # xs.names == [ "c" "b" "a" ] (in order of definition)
    # xs.values == [ 1 2 3 ] (in order of definition)
    OrderedOf = Type.template "OrderedOf" {T = Type;} (_:
      let Item = Sized 1 (SetOf _.T);
      in inheritFrom (ListOf Item) (rec {
        methods = {
          # The unordered merged attribute set
          unindexed = this:
            mergeAttrsList
              (this.zipWith
                (_: name: value: {name = value;}));

          # The merged attribute set with an additional 'index' field indicating
          # its place in the order.
          indexed = this:
            mergeAttrsList
              (this.zipWith
                (index: name: value: {
                  name = value // { inherit index; };
                }));

          # The ordered attribute names.
          attrNames = this: (this.fmap (x: head x.attrNames)).value;

          # The ordered attribute values.
          attrValues = this: (this.fmap (x: head x.attrValues)).value;

          # A set from name to index.
          indexes = this: mapAttrs (name: xs: xs.index) this.indexed;

          # Zip over the index, name and value of the ordered fields.
          zipWith = this: f:
            zipListsWith
              ({index, name}: value: f index name value)
              (enumerate this.attrNames)
              this.attrValues;

          # Modify the value at the given key via this.modifyAt.name f, preserving order
          modifyAt = this:
            mapAttrs
              (name: _: f:
                this.fmap
                  (item:
                    let maybeModifyHere = item.modifyAt.${name} or (const item);
                    in maybeModifyHere f))
              this.unindexed;

          # Set the value at the given key via this.setAt.name value, preserving order
          # Key must exist in the Ordered (cannot create new keys).
          setAt = this: mapAttrs (name: modifyHere: x: modifyHere (const x)) this.modifyAt;

          # Set the value at the given key via this.setAtName "name" value, preserving order
          # If the key does not exist, it will be added at the end of the order.
          setAtName = this: name: value:
            if this.setAt ? ${name}
            then this.setAt.name value
            else this.append (Item.new {${name} = value;});

          # Get the value with the given key via this.getAt.name
          getAt = this: this.unindexed;

          # Get the value with the given key and index set via this.getIndexedAt.name
          getIndexedAt = this: this.indexed;
        };

        checkValue = that:
          Super.checkValue that
          && assertMsg
              (size that.attrNames == size that.unindexed)
              "Duplicate keys in OrderedOf: ${joinSep ", " that.attrNames}";
      }));

    # An Ordered that takes any value type
    Ordered = (OrderedOf Any).subtype "Ordered" {};

    # Base type for enums.
    Enum = Type.new "Enum" {};

    # Create an enum type from a list of item names.
    # MyEnum = mkEnum "MyEnum" [ "Item1" "Item2" "Item3" ]
    # MyEnum.names == [ "Item1" "Item2" "Item3" ]
    # MyEnum.fromName "Item1" == 0
    # MyEnum.fromIndex 0 == "Item1"
    mkEnum = enumName: itemNames:
      let
        Item = Enum.subtype enumName {
          fields = [
            {index = Int;}
            {name = String;}
          ];
          staticMethods = {
            # Enum members live on the Enum type itself.
            __items = This: zipListsWith EnumName.new (range 0 (length itemNames - 1)) names;
            __indexToItem = This: keyByF (item: item.index) __items;
            __nameToItem = This: keyByName __items;
            fromIndex = This: index: This.__indexToItem.${index} or throw "Invalid index in enum ${enumName}: ${toString index}";
            fromName = This: name: This.__nameToItem.${name} or throw "Invalid name in enum ${enumName}: ${name}";
          };
        };
      in Item;

    # A type inhabited by only one value.
    Literal = Type.template "Literal" {Value = Any;} (_: rec {
      ctor = Ctor.Nullary;
      staticMethods = {
        literal = This: _.Value;
      };
    });

    # A type inhabited by literals of any of the given list of values
    Literals = values:
      let literalTypes = map Literal values;
      in Union literalTypes;

    Default = Type.template "Default" {T = Type;} (_: {
      fields.defaultValue = _.T;
    });

    Static = Type.template "Static" {T = Type;} (_: {});

    # Either:
    # Field.new "myField" Int
    # Field.new "myField" (Static Int)
    # Field.new "myField" (Default 123) -> type int
    # Field.new "myField" (Default (Int.new 123)) -> type Int
    # Field.new "myField" (Static (Default 123)) -> type (Static int)
    Field = T: Type.new "Field" [T] {
      fields = [
        {fieldName = String;}
        {fieldType = Default (NullOr T) null;}
        {fieldStatic = Default Bool false;}
        {fieldDefault = Default (NullOr (Default T)) null;}
        {index = Default Int 0;}
      ];
      # ctor in the order of Ordered.zipWith iteration
      ctor = fieldName: T:
        let
          init = U:
            if U.tvars.size == 0
            then {inherit fieldName; fieldType = U;}
            else {
              # TODO: Pattern matching
              Static = init U.tvars.T // {fieldStatic = true;};
              Default = init U.tvars.T // {fieldDefault = U.defaultValue;};
            }.${typeName U}
              or (throw "Invalid Field argument of type ${typeName T}: ${log.print T}");
        in init T;
    };

    Fields = Type.new "Fields" {
      Super = OrderedOf Field;

      ctor = fields:
        let init = fields:
              {
                # Treat a raw fields set as an arbitrary-order collection of fields.
                set = init (mapAttrsToList (name: field: { inherit name field; }) fields);
                Set = init fields.value;

                # Treat a list/List as though shorthand for Ordered.
                list = Ordered.new (map (field: Field.new field.name field.fieldType) fields);
                List = init fields.value;
              }.${(fields.Type or {name = typeOf fields;}).name}
                or (throw "Invalid Fields argument (${typeOf fields}): ${log.print fields}");
        in init fields;
    };

  };

  # nix eval --impure --expr '(import ./cutils/types.nix {})._tests'
  _tests =
    with Types;
    with Types.Builtin;
    with cutils.tests;
    let
      mkBuiltinTest = T: name: rawValue: {
        expr =
          let x = T.new rawValue;
          in {
            name = x.Type.name;
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

      MyString = mkType "MyString" {
        fields = { value = String; };
      };

      MyType = Type.new "MyType" {
        fields = {
          myField = String;
        };
        methods = {
          helloMyField = this: extra:
            "Hello, ${this.myField.value}${extra}";
        };
      };

      MyType2 = Type.new "MyType2" {
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

        RootType = {
          expr = Type.name;
          expected = "Type";
        };

        isRootType = {
          Type = {
            expr = isRootType Type;
            expected = true;
          };
          bool = {
            expr = isRootType "bool";
            expected = false;
          };
          Bool = {
            expr = isRootType Bool;
            expected = false;
          };
        };

        isBuiltinValue = {
          Type = {
            expr = isBuiltinValue Type;
            expected = false;
          };
          int = {
            expr = isBuiltinValue 123;
            expected = true;
          };
          set = {
            expr = isBuiltinValue {abc="xyz";};
            expected = true;
          };
          Bool = {
            expr = isBuiltinValue (Bool.new true);
            expected = false;
          };
        };

        MyString = {
          mk = {
            typed = {
              expr = (MyString.mk { value = String.mk {value = "hello";}; }).value.value;
              expected = "hello";
            };

            raw = {
              expr = (MyString.mk { value = "hello"; }).value.value;
              expected = "hello";
            };
          };

          new = {
            typed = {
              expr = (MyString.new (String.new "hello")).value.value;
              expected = "hello";
            };

            raw = {
              expr = (MyString.new "hello").value;
              expected = String.new "hello";
              compare = Compare.Fields;
            };
          };
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
          expected = [ "Hello, !" "Hello, World!" ];
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

        cast =
          let
            ComparableBuiltinTypes = removeAttrs BuiltinTypes [ "Lambda" ];
            testX = {
              Null = null;
              Int = 123;
              Float = 12.3;
              String = "abc";
              Path = ./.;
              Bool = true;
              List = [1 2 3];
              Set = { a = 1; b = 2; c = 3; };
            };
            mkToTypedBuiltinTest = T: x: {
              expr = cast T x;
              expected = T.new x;
              compare = Compare.Fields;
            };
          in {
            toTypedBuiltin = mapAttrs (name: T: mkToTypedBuiltinTest T testX.${name}) ComparableBuiltinTypes;
          };

        inheritance =
          let
            A = Type.new "A" { fields = { a = String; }; };
            B = Type.new "B" { Super = A; fields = { b = Int; }; };
          in {

            newA = {
              expr = A.new "a";
              expected = A.mk { a = "a"; };
              compare = Compare.Fields;
            };

            isSuperTypeOf = {
              parentChild = expect.True (isSuperTypeOf A B);
              childParent = expect.False (isSuperTypeOf B A);
              parentParent = expect.False (isSuperTypeOf A A);
              childChild = expect.False (isSuperTypeOf B B);
              typeParent = expect.True (isSuperTypeOf Type A);
              typeChild = expect.True (isSuperTypeOf Type B);
              typeType = expect.False (isSuperTypeOf Type Type);
            };

            isSubTypeOf = {
              parentChild = expect.False (isSubTypeOf A B);
              childParent = expect.True (isSubTypeOf B A);
              parentParent = expect.False (isSubTypeOf A A);
              childChild = expect.False (isSubTypeOf B B);
              typeParent = expect.False (isSubTypeOf Type A);
              typeChild = expect.False (isSubTypeOf Type B);
              typeType = expect.False (isSubTypeOf Type Type);
            };

            hasThisFields = {
              expr = B.fields;
              expected = {
                b = {
                  index = 0;
                  fieldType = Int;
                  name = "b";
                };
              };
            };

            hasSuperFields = {
              expr = B.__allFields;
              expected = {
                a = {
                  index = 0;
                  fieldType = String;
                  name = "a";
                };
                b = {
                  index = 0;
                  fieldType = Int;
                  name = "b";
                };
              };
            };

            fieldsCompose = {
              expr = B.new "a" 2;
              expected = B.mk { a = "a"; b = 2; };
              compare = Compare.Fields;
            };

        };

      };
  };

}

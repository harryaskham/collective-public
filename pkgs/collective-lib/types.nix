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
# - newSubType
# - Tests for:
#   - Inheritance
#   - Nested calls / chained sets see the correct bound state
#   - ListOf
#   - Type classes
#   - Unions
#   - Enums
#   - Maybe / ADTs
# - Try https://bolt.tvix.dev/

# Inheritance-based typesystem for use outside of the module system.
#
# Objects are represented as attrsets with "Type" and "Super" keys, holding the
# type and supertype definitions respectively.
#
# The type system is bootstrapped from:
# - a minimal manually constructed HyperType, definining the fields of a type
# - a subtype of HyperType, MetaType, which is also an instance of HyperType, adding methods
# - a subtype of MetaType, ProtoType, which is also an instance of MetaType, whose instantiation binds the methods
# - Type, a subtype and instance of ProtoType, which is the base type for all subsequent types in this system.
#
# Fields are validated and typechecked upon object creation and again on updates,
# such that once an object of type T is constructed it remains valid under T.check.
#
# Builtin types are wrapped as e.g. String, Bool, ... such that when a field is assigned
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

    # Whether or not x is a builtin type
    # All builtins are attrs. The short-circuit stops recursive inspection of Type.
    isBuiltinValue = x:
      if isAttrs x then !(x ? Type)
      else true;

    # Whether or not name is one of the lowercase builtin type names.
    isBuiltinTypeName = name: Builtin.maybeFromT name != null;

    # Wrap a builtin type
    wrap = Builtin.From;

    # Unwrap a value type
    unwrap = x: x.value;

    # Wrap a builtin type or leave a Type value unchanged
    maybeWrap = x: if isBuiltinValue x then wrap x else x;

    # Unwrap a value type or leave a non-value Type value unchanged
    maybeUnwrap = x: if isValueType x then unwrap x else x;

    # Cast between types.
    # Returns a set with a 'castError' attribute if the cast is not possible
    # or a set with a 'castSuccess' attribute containined the cast value.
    # Also includes a 'msgs' attribute with a list of messages recording
    # cast attempts.
    mkCastError = msg: { castError = msg; };
    mkCastSuccess = value: msg: { castSuccess = value; castSuccessMsg = msg; };
    isCastError = x: x ? castError;
    isCastSuccess = x: x ? castSuccess;
    cast = T: x:
      if !(isType T || isBuiltinTypeName T)
      then mkCastError "Invalid target type provided for cast: T = ${log.print T}"
      else let
        printT = T: T.__TypeId or T;

        TName = printT T;
        xTName = typeBoundName x;

        xFields = assert isTyped x; filterAttrs (_: f: !f.fieldStatic) x.Type.fields.indexed;
        TFields = assert isType T; filterAttrs (_: f: !f.fieldStatic) T.fields.indexed;

        xIsUnary = isTyped x && size xFields == 1;
        TIsUnary = isType T && size TFields == 1;

        xUnaryField = assert xIsUnary; head (attrValues xFields);
        TUnaryField = assert TIsUnary; head (attrValues TFields);

        xUnaryFieldName = xUnaryField.fieldName;
        TUnaryFieldName = TUnaryField.fieldName;

        xUnaryFieldT = xUnaryField.fieldType;
        TUnaryFieldT = TUnaryField.fieldType;

        xUnaryFieldTName = printT xUnaryFieldT;
        TUnaryFieldTName = printT TUnaryFieldT;

        TFieldNames = sortOn (f: f.index) (mapAttrsToList (fieldName: _: fieldName) TFields);
        xFieldNames = sortOn (f: f.index) (mapAttrsToList (fieldName: _: fieldName) xFields);

        # A list of casts to attempt in order.
        # The first cast satisfying 'when == true && isCastSuccess result' will be returned.
        # If no cast satisfies, then a cast error set is returned with the collated errors.
        casts = [
          # No-op cast if x is already an instance of T.
          # This does not call T.check, so if the value has been manipulated
          # to be invalid through modification outside of the type-checked x.set and x.modify
          # interfaces, this will not be caught here.
          {
            name = "Identity";
            when = hasType T x;
            orMsg = "Not an identity cast: ${xTName} -> ${TName}";
            result = mkCastSuccess x "";
            failMsg = _: null;
            successMsg = _: "Identity cast succeeded: ${xTName} -> ${TName}";
          }

          # Downcasting for unary x types via a nested cast
          # This includes Builtins and other value types.
          # Careful not to use other attrs of 'fields' here besides 'indexed' to
          # be bootstrap-compatible.
          {
            name = "Downcast from Unary";
            when = xIsUnary;
            orMsg = "Cannot downcast from an instance of non-unary type: ${xTName} -> ${TName}";
            result = cast T x.${xUnaryField.fieldName};
            failMsg = castErrorMsg: indent.block ''
              Downcast failed from unary field ${xTName}.${xUnaryFieldName}: ${xUnaryFieldTName} -> ${TName}

              ${indent.here "Downcast error: ${castErrorMsg}"}
            '';
            successMsg = castSuccessMsg: ''
              Downcast succeeded from unary field ${xTName}.${xUnaryFieldName}: ${xUnaryFieldTName} -> ${TName}";

              ${indent.here "Upcast success: ${castSuccessMsg}"}
            '';
          }

          # Upcasting for unary x types via a nested cast
          # This includes Builtins and other value types.
          # Careful not to use other attrs of 'fields' here besides 'indexed' to
          # be bootstrap-compatible.
          {
            name = "Upcast to Unary";
            when = TIsUnary;
            orMsg = "Cannot upcast to a non-unary type: ${xTName} -> ${TName}";
            result =
              let xCast = cast TUnaryFieldT x;
              in if isCastError xCast then xCast
              else mkCastSuccess (T.mk { ${TUnaryFieldName} = xCast.castSuccess; }) xCast.castSuccessMsg;
            failMsg = castErrorMsg: indent.block ''
              Upcast failed to unary field ${TName}.${TUnaryFieldName}: ${xTName} -> ${TUnaryFieldTName})

              ${indent.here "Upcast error: ${castErrorMsg}"}
            '';
            successMsg = castSuccessMsg: indent.block ''
              Upcast succeeded to unary field ${TName}.${TUnaryFieldName}: ${xTName} -> ${TUnaryFieldTName})

              ${indent.here "Upcast success: ${castSuccessMsg}"}
            '';
          }

          # Sidecasting types via a nested cast
          # Careful not to use other attrs of 'fields' here besides 'indexed' to
          # be bootstrap-compatible.
          {
            name = "Sidecast Fields";
            when = isType T && isTyped x && length xFieldNames == length TFieldNames;
            orMsg = ''
              Cannot sidecast unless from a typed instance to a Type with the same field count: ${xTName} -> ${TName}
                Source fields: ${joinSep ", " xFieldNames}
                Target fields: ${joinSep ", " TFieldNames}
              '';
            result =
              let castTArgs = zipListsWith (xFieldName: TFieldName: { ${TFieldName} = cast T.fields.indexed.${TFieldName}.fieldType x.${xFieldName}; }) xFieldNames TFieldNames;
                  castErrors = filterAttrs (_: isCastError) castTArgs;
                  castArgs = mapAttrs (_: castResult: castResult.castSuccess) castTArgs;
                  castErrorMsgs = joinLines (mapAttrsToList (name: castResult: "${name}: ${castResult.castError}") castTArgs);
                  castSuccessMsgs = joinLines (mapAttrsToLIst (name: castResult: "${name}: ${castResult.castSuccessMsg}") castTArgs);
              in if size castErrors > 0
                 then mkCastError castErrorMsgs
                 else mkCastSuccess (T.mk castArgs) castSuccessMsgs;
            failMsg = castErrorMsg: indent.block ''
              Sidecast failed: ${xTName} -> ${TName}

              Field cast errors:
                ${indent.here castErrorMsg}
            '';
            successMsg = castSuccessMsg: indent.block ''
              Sidecast succeeded: ${xTName} -> ${TName}

              Field casts successes:
                ${indent.here castSuccessMsg}
            '';
          }
        ];

        getOrMsg = cast: "${cast.name}: ${cast.orMsg}";
        getFailMsg = cast: assert isCastError cast.result; "${cast.name}: ${cast.failMsg cast.result.castError}";
        getSuccessMsg = cast: assert isCastSuccess cast.result; "${cast.name}: ${cast.successMsg cast.result.castSuccessMsg}";

        tryCasts = msgs: casts:
          let
            cast = head casts;
            casts' = tail casts;
          in
            # If we exhausted all casts, terminate with a combined castError
            if casts == []
              then (
                mkCastError ''
                  Cast failed: ${xTName} -> ${TName}

                  ${xTName} instance:
                    ${indent.here (log.print x)}

                  Attempted casts:
                    ${indent.here (joinLines msgs)}
                '')

            # Skip non-matching casts with a note message
            else if !cast.when
              then let msgs' = msgs ++ [(getOrMsg cast)]; in tryCasts msgs' casts'

            # Record nested cast errors
            else if isCastError cast.result
              then let msgs' = msgs ++ [(getFailMsg cast)]; in tryCasts msgs' casts'

            # Cast succeeded
            else
              assert isCastSuccess cast.result;
              let
                msgs' = msgs ++ [(getSuccessMsg cast)];
              in
                mkCastSuccess cast.result.castSuccess ''
                  Cast succeeded: ${xTName} -> ${TName}

                  ${xTName} instance:
                    ${indent.here (log.print x)}

                  Attempted casts:
                    ${indent.here (joinLines msgs')}
                '';

      in tryCasts [] casts;

    # Fold over a chain of inheritance, calling a function of (acc: This: acc') on each
    # type and returning the final acc.
    # Takes an initial value for the accumulator and the current type This.
    # Terminates when Super is null or when reaching a type where This == Super.
    #
    # Akin to a left fold, with children being processed before parents:
    # (f
    #   ...
    #     (f
    #       (f
    #         (f init This)
    #        This.Super)
    #      This.Super.Super)
    #   ...
    #  This.Super.Super... ... .Super)
    foldUpward = f: acc: This:
      let
        acc' = f acc This;
        This' = This.Super or null;
      in
        if nully This then acc
        else if nully This' || typeEq This This' then acc'
        else foldUpward f acc' This';

    # Merge an attribute of This's inheritance chain, with This attributes overriding those of Super.
    # Does not deeply merge so should be used to pick out a single flat mergable like 'methods' such that
    # this models This inheriting Super's methods, but applying any overrides to them by redefining.
    #
    # e.g. mergeSuper (This: This.methods) This ==
    #        This.Super.Super...methods // ... // This.Super.methods // This.methods;
    mergeSuper = f: foldUpward (acc: This: acc // f This) {};

    # Modify an argument set to inherit from a supertype.
    # Unless ctor is set explicitly in the child, this will also inherit the supertype's constructor.
    inheritFrom = Super: args:
      if !(isType Super) then throw "inheritFrom: Super must be a Type, got ${log.print Super}"
      else {
        inherit Super;
        inherit (Super) Type ctor;
      } // args;

    typeFields = Universe: with Universe; [
      # The name of the type.
      {name = String;}
      # The type of the instance as a thunk.
      {Type = Default Type Type;}
      # The supertype of the type.
      {Super = Default (NullOr Type) null;}
      # The type parameters of the type.
      {tvars = Default "set" {};}
      # The type parameter bindings of the type.
      {tvarBindings = Default "set" {};}
      # The constructor function creating the fields of the type as a set to pass to mk.
      {ctor = Default "lambda" Ctors.Fields;}
      # A set of ordered fields to make available as this.___ and this.get.___, this.set.___, etc
      {fields = Default Fields (Fields.new []);}
      # A set of methods from this to make available as this.___
      {methods = Default "set" {};}
      # A set of methods from This to make available as This.___ and this.___
      {staticMethods = Default "set" {};}
      # Perform additional checks on the value of the type when comparing.
      {checkValue = Default (NullOr Lambda) null;}
      # If set, ignore all other checks and use this check function only.
      {overrideCheck = Default (NullOr Lambda) null;}
    ];

    # Execute a function of the type level in that level, returning the value
    # of the expression.
    withTypeLevel = Level: f: f Level;

    # Map a function over the supplied type levels, producing a set keyed
    # by Type name. Can be used to construct the same type in multiple different
    # Type universes.
    withTypeLevels = Levels: f:
      mergeAttrsList (map (Level: { ${Level.__TypeId} = f Level; }) Levels);

    # Build the 'methods' field for one of the given types or type precursors.
    mkTypeMethods = Universe: This: with Universe;
      {
      # Is this instance an instance of type That, or inherits from it?
      isInstance = this: That:
        typeEq this.Type That
        || (This.Super != null && This.Super.isInstance That);

      # Get the full templated name of the type.
      boundName = This:
        if This.tvars == null then This.name
        else if This.tvars == {} then This.name
        else
          let
            printBinding = tvarName:
              let T = This.tvarBindings.${tvarName} or Void;
              in if typeEq Void T
                then tvarName
                else "${tvarName} = ${typeBoundName T}";
            printBindings = joinSep ", " (map printBinding (attrNames This.tvars));
          in "${This.name}<${printBindings}>";

      # Construct the type resulting from applying the given bindings to the parameterized This type.
      bind = This: tvarBindingsAttrs:
        let
          tvarBindingsList =
            mapAttrsToList
              (tvarName: T: {inherit tvarName T;})
              tvarBindingsAttrs;

          bindOne = This: {tvarName, T}:
            let
              throwBindError = msg: throw (joinLines [
                "bind: Error binding ${tvarName} <- ${typeName T}"
                msg
              ]);
              constraint =
                This.tvars.${tvarName} or
                  (throwBindError "Type ${This.name} does not have type variable ${tvarName}");
            in
              if This.tvarBindings == null
                then throwBindError "Type ${This.__TypeId} does not have bound or unbound type variable bindings"
              else if This.tvarBindings ? tvarName
                then throwBindError "Type ${This.__TypeId} already has type variable ${tvarName} bound to ${typeName This.tvarBindings.${tvarName}}"
              else if !(constraint.satisfiedBy T)
                then throwBindError "Type ${typeName T} does not satisfy constraint: ${log.print constraint}"
              else This.modify.tvarBindings (bs: bs // {${tvarName} = T;});

        in foldl' bindOne This tvarBindingsList;

      # Does this type inherit from That?
      inheritsFrom = This: That:
        typeEq This That
        || (This.Super != null && This.Super.inheritsFrom That);

      check = This: that:
        let
          runChecks = doChecksNoAssert [
            {
              cond = typeEq This that.Type;
              msg = "Type check failed for ${This.__TypeId} (got ${That.__TypeId})";
            }
            {
              cond = This.checkValue == null || This.checkValue that;
              msg = "checkValue failed: ${log.print that} is not a value of type ${This.__TypeId}";
            }
          ];
        in if This.overrideCheck.value != null
          then This.overrideCheck.value that
          else runChecks;
    };

    universeIndependentBaseMethods = {
      # Use the bound name with its ordered param assignments to determine type identity
      # and equality.
      # For bootstrap types this may not be bound yet, falling back to the name.
      __TypeId = This: This.boundName or This.name;

      # Create a new instance of the type by providing field values
      mk = mkInstance;

      # Create a new instance of the type by calling This's constructor
      # When This == Type, creates a new Type.
      new = newInstance;

      # Create a new subType inheriting from This
      # TODO: Compose checkValue / overrideCheck if they appear in args
      subType = newSubType;

      # __toString needs to take a this parameter for toString compliance.
      # Therefore we have Type.staticMethods.__toString which ends up on instances as (instance.__toString instance) which is correct.
      # This will work to print both types as T.__toString T and instances as t.__toString t
      __toString = This: this:
        if isType this then (assert This.__TypeId == this.__TypeId; This.__TypeId)
        else "${This.__TypeId or "<no __TypeId>"} ${log.print (this.get or {})}";
    };

    universeDependentBaseMethods = Universe: {
      # Create a new template of a type accepting type parameters.
      # The parameters can be accessed via the _ argument surrounding the type specification.
      # Returns a function from {bindings} to a new bound type.
      template = Universe.newTemplate;

      # Create a new template inheriting from This
      subTemplate = Universe.newSubTemplate;

      # Create a new template inheriting from a function of This plus any type variables.
      # TODO: Generic inheritance and tvar/tvarBinding merging.
      subTemplateOf = Universe.newSubTemplateOf;
    };

    baseMethods = Universe:
      universeIndependentBaseMethods
      // (universeDependentBaseMethods Universe);

    # Field default values using the given Universe.
    # Before we can use the current universe's Default type.
    # Required to avoid needing all types in the Universe to specify their defaults.
    fieldDefaults = Universe:
      with Universe; {
        Super = null;
        tvars = {};
        tvarBindings = {};
        fields = Fields.new {};
        ctor = Ctors.Fields;
        methods = {};
        staticMethods = {};
        checkValue = {value = null;};
        overrideCheck = {value = null;};
      };

    # Manually construct an untyped indexed field.
    # Simulated having an instantiated Fields instance.
    universeField = Universe: index: fieldName:
      let
        defaults = fieldDefaults Universe;
      in {
        ${fieldName} = {
          inherit fieldName index;
          fieldStatic = false;
          fieldType = null;
          fieldDefault = { defaultValue = defaults.${fieldName} or null; };
        };
      };

    # Simulate the IndexedOf
    # Manually construct the untyped indexed fields by transforming the typed
    # field specifications of Type, such that HyperType can be instantiated to
    # bootstrap via HyperType -> MetaType -> ProtoType -> Type.
    universeFields = Universe:
      mergeAttrsList
        (imap0
          (index: field: universeField Universe index (head (attrNames field)))
          (typeFields Universe));

    # Simulate the raw set of lambdas required by Methods
    universeMethods = Universe:
      (baseMethods Universe)
        // (mkTypeMethods Universe Universe.Type);

    # Simulate the raw set of lambdas required by Methods with
    # type methods dependent on the Quasiverse for Ctor, Fields.new, etc
    quasiverseMethods =
      (baseMethods Quasiverse)
        // (mkTypeMethods Quasiverse HyperType);

    # Produce the Type-precursor arguments using elements from a given SuperUniverse.
    mkTypeArgs = Universe: mkMethods: name:
      with Universe; {
        # Set up the Type behaviour
        inherit name;
        ctor = Ctors.TypeCtor;
        fields = Fields.new (universeFields Universe);
        methods = mkMethods Universe;

        # Defaults matching those of a properly constructed type.
        staticMethods = {};
        tvars = {};
        tvarBindings = {};
        checkValue = { value = null; };
        overrideCheck = { value = null; };
      };

    Bootstrap = rec {
      # HyperType is constructed such that it is both:
      #
      # - a valid type input to mkInstance for the creation of MetaType
      # - a minimal subset of the output from mkInstance HyperType, manually created.
      #
      # This allows us to ground the infinite recurse we'd otherwise get by needing to actually create HyperType as an instance
      # of itself or of a higher type.
      #
      # - HyperType.Type is manually set to be self-referential, as it would be if HyperType was created via 'mkInstance HyperType'
      # - Method HyperType.__TypeId is manually set, rather than relying on binding staticMethods.boundName, which in turn relies on the template machinery.
      # - HyperType.fields is a manually constructed set mirroring the ordered interface to Fields, exposing a manually precomputed Fields.indexed result for field inspection.
      # - HyperType.methods are restricted only to a Universe-independent set:
      #   - These independent methods are callable when bound on MetaType.
      #   - Further method construction requires reference to the Universe
      #     - These are added in MetaType and bound in ProtoType.
      # - HyperType.get/set/modify/call accessors are missing owing to having not itself gone through mkInstance (this happens in MetaType creation).
      # - Other defaults are manually set.
      HyperType =
        let
          # HyperType acts as its own SuperUniverse.
          HyperType__args = (mkTypeArgs Quasiverse (_: quasiverseMethods) "HyperType") // {
            # Break recursion at this root level via a quasi-type Type object sufficient to call mkInstance.
            Type = { name = "HyperType"; };
          };

          # As subtype, supertype and instance of itself.
          HyperType__fixedPoint =
            let toQuasiType = T: T // { __TypeId = T.name; };
                toFixedPointType = T: newInstance (toQuasiType T) (T.name) T;
            in toFixedPointType HyperType__args;

          # HyperType instances still come from the HyperType__precursor, meaning you can
          # follow instance.Type.Type.Type back to { name = "HyperType" }. We take one last
          # subtype here.
          HyperType__selfHosted = newInstance HyperType__fixedPoint "HyperType" HyperType__args;
       in
         HyperType__selfHosted;

      # MetaType is the instantiated HyperType, which is both an instance of
      # HyperType and also inherits from HyperType.
      MetaType =
        HyperType.new "MetaType"
          (mkTypeArgs Universe.HyperType universeMethods "MetaType");

      # The ProtoType of all Types.
      # Constructed as per Type, but without using any of the machinery of Type
      # Can be used to construct e.g. Fields for use in Type
      ProtoType = newSubType MetaType "ProtoType" {
        methods =
          (baseMethods Universe.MetaType)
          // (mkTypeMethods Universe.MetaType ProtoType);
      };

      # Finally construct the base Type to use from here on out.
      # Override 'fields' to contain the actual field specifiers.
      Type =
        with Universe.ProtoType;
        ProtoType.subType "Type" {
          fields = Fields.new typeFields;
          methods =
            (baseMethods Universe.ProtoType)
            // (mkTypeMethods Universe.ProtoType MetaType);
        };
    };

    inherit (Bootstrap) HyperType MetaType ProtoType Type;

    # Check if a given argument is a custom Type.
    isTyped = x: isAttrs x && x ? Type;

    # Check if a given argument is a Type.
    isType = T: T ? __TypeId;

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
      if isBuiltinValue x then typeOf x else x.Type.__TypeId;

    # Check two types for equality, supporting both Types and builtin type-name strings.
    typeEq = T: U:
      if isType T && !isType U then false
      else if !isType T && isType U then false
      else if isType T && isType U then T.__TypeId == U.__TypeId
      else if !(isBuiltinTypeName T) then throw "typeEq: ${log.print T} is not a Type or builtin type"
      else if !(isBuiltinTypeName U) then throw "typeEq: ${log.print U} is not a Type or builtin type"
      else T == U;

    # Check a string or custom type against a value.
    hasType = T: x: typeEq T (getRawType x);

    nully = x:
      x == null
      || (x ? value && x.value == null);

    combinedFields = This:
      if nully (This.Super or null)
      then This.fields.indexed or {}
      else mergeSuper (T: T.fields.indexed or {}) This;

    # Get all methods as Lambdas
    combinedMethods = This:
      if nully (This.Super or null)
      then This.methods
      else mergeSuper (T: T.methods) This;

    # Get all static methods as Lambdas
    combinedStaticMethods = This:
      if nully (This.Super or null)
      then This.staticMethods
      else mergeSuper (T: T.staticMethods) This;

    staticFields = This:
      filterAttrs (_: f: f.fieldStatic or false) (combinedFields This);

    instanceFields = This:
      filterAttrs (_: f: !(f.fieldStatic or false)) (combinedFields This);

    # Construct the parent instance for merging with the new instance.
    # Any still-unknown fields provided will be caught by checking the expected
    # fields of the entire inheritance chain after 'this' is created.
    mkSuperInstance = This: args:
      if nully (This.Super or null) then {}
      else
        let superFields = mergeSuper (T: T.fields.indexed) This.Super;
            superArgs = filterAttrs (name: _: superFields ? name) args;
        in mkInstance This.Super superArgs;

    setType = This: this: this // { Type = This;};

    # Set the super value such that instance methods can access this.super
    # when overriding superclass methods, as well as any field values on the
    # superclass that may behave differently (i.e. different defaults).
    # this.super essentially acts as though we instantiated Super directly
    # with the subset of the This fields that are not overridden.
    # If This == Super then terminate with super = this.
    setSuper = This: args: this:
      if ((This.Super or null) == null) then this // { super = null; }
      else if typeEq This This.Super then this // { super = this; }
      else
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
                      ${This.__TypeId}.${fieldName} = ${log.print uncastValue}
                      ${indent.here castValue.castError}
                  '';
                }
                # TODO: This check could be O(n) for container types, updating attrsets goes O(1) to O(n)
                # Need item-wise checks
                { pred = field: field.fieldType == null || field.fieldType.check castValue;
                  msg = indent.block ''
                    Cast value did not pass typecheck:
                      ${This.__TypeId}.${fieldName} = ${log.print uncastValue}
                      Cast value of ${log.print castValue} is not a valid instance of ${field.fieldType.__TypeId}.
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
    # If a field is already set on 'this', leave it as-is (i.e. Type)
    setFields = This: args: this:
      foldl'
        (this: field:
          let hasValue = args ? ${field.fieldName} || (field.fieldDefault or null) != null;
              value = args.${field.fieldName} or field.fieldDefault.defaultValue;
          in if this ? ${field.fieldName} then this
            else if hasValue then this.set.${field.fieldName} value
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

    # Reconstruct the This instance after change.
    reinitThis = This: this:
      this
        |> (setAccessors This)
        |> (setStaticMethods This)
        |> (setMethods This);

    # Create a new instance of a type by calling its constructor.
    # The constructor's output arguments are then passed into mkInstance.
    # For types, the constructor just merges a name parameter with an arguments
    # parameter and delegates to mkInstance.
    newInstance = This:
      Variadic.compose (mkInstance This) (This.ctor This);

    # For a given type, create a new type of the same Type with the given name and args, inheriting any
    # unspecified fields, ctor, and non-overridden methods.
    newSubType = This: name: args:
      if !(isType This) then throw "Cannot subtype non-Type or Type-precursor: ${log.print This}"
      else newInstance This name (inheritFrom This args);

    # Build a new instance from a single dict of field values.
    # Construct an instance from a This type and attrset of field-to-value assignments.
    # arg = { fieldName = value, ... } for all fields.
    # Fields with defaults can be omitted.
    # Build a new instance from a single dict of field values.
    mkInstance = This: args:
      let
        # Construct the final instance
        this =
          {}
            |> (setType This)
            |> (setSuper This args)
            |> (setAccessors This)
            |> (setStaticMethods This)
            |> (setFields This args)
            |> (setMethods This);

        checks =
          let
            # Get any supplied non-static fields not present in this or any supertype.
            allFieldNames = attrNames (mergeSuper (T: T.fields.indexed or {}) This);
            unknownFieldNames = attrNames (removeAttrs args allFieldNames);

            # Get any fields not populated in this or any supertype.
            populatedFieldNames = filter (name: this ? ${name}) allFieldNames;
            requiredFields = filterAttrs (_: field: (field.fieldDefault or null) != null) (This.fields.indexed or {});
            missingFieldNames = attrNames (removeAttrs requiredFields populatedFieldNames);
          in [
            {
              cond = unknownFieldNames == [];
              msg = "${This.__TypeId}: Unknown fields in mkInstance call: ${joinSep ", " unknownFieldNames}";
            }
            {
              cond = missingFieldNames == [];
              msg = ''
                ${This.__TypeId}: Missing fields in mkInstance call: ${joinSep ", " missingFieldNames}
                args: ${log.print args}
                Super: ${log.print This.Super}
                super: ${log.print super}
                This: ${log.printAttrs This}
                this: ${log.print this}
              '';
            }
          ];

      in assert (doChecks checks); this;

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

    Ctors = {
      # The ctor for a Type, whose Type.new "name" { ... } should create a new type.
      TypeCtor = _: name: arg: arg // { inherit name; };

      # Explicit nullary constructor s.t. X.new == X.mk {}
      Nullary = This: {};

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
      Fields = This:
        let
          # Here 'indexed' is manually set on the ProtoType
          fields = filterAttrs (_: f: !f.fieldStatic) This.fields.indexed;
          # Do manually to avoid a new call
          sortedFieldNames = map (f: f.fieldName) (sortOn (f: f.index) (mapAttrsToList (_: f: {inherit (f) fieldName index;})));
        in
          if size fields == 0
          then if This.Super == null then {}
              else
                # Dig into Super to apply the wrapped Ctor
                This.Super.ctor This
          else
            let thisCtor = Variadic.mkOrdered sortedFieldNames;
            in
              if This.Super == null
              then thisCtor
              else fjoin mergeAttrs thisCtor This.Super.ctor;
    };

    mkBuiltins = Universe: with Universe; rec {
      mkBuiltin = name:
        let
          builtinTypeName = toLower name;

          builtinCheckValue = {
            # Additional check on sets s.t. we don't accept a typed value when expecting
            # a raw set.
            Set = that: !(that ? Type);
          };

          withSize =
            let sizeFn = cutils.functions.size;
            in methods: methods // { size = this: sizeFn this.value; };

          builtinMethods = {
            Null = {};
            Int = {};
            Float = {};
            String = withSize {};
            Path = withSize {};
            Bool = {};
            List = withSize {
              fmap = this: f: this.modify.value (map f);
              append = this: x: this.modify.value (xs: xs ++ [x]);
            };
            Set = withSize {
              fmap = this: f: this.modify.value (mapAttrs (_: f));
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
            Lambda = {
              fmap = this: f: this.modify.value (compose f);
            };
          };

        in {
          ${name} = Universe.Type.new name {
            ctor = _: value: { inherit value; };
            fields = Fields.new [
              { value = builtinTypeName; }
            ];
            methods = builtinMethods.${name};
            checkValue = builtinCheckValue.${name} or null;
          };
        };

      BuiltinNames = [ "Null" "Int" "Float" "String" "Path" "Bool" "List" "Set" "Lambda" ];
      BuiltinTypes = mergeAttrsList (map mkBuiltin BuiltinNames);
      inherit (BuiltinTypes) Null Int Float String Path Bool List Set Lambda;
    };

    # The barest minimum universe to bootstrap the type system.
    Quasiverse = rec {

      Type = HyperType;

      quasiField = fieldName: _: {
        inherit fieldName;
        index = 0;
        fieldStatic = false;
        fieldType = null;
        fieldDefault =
          let def = {
            Super = null;
            tvars = {};
            tvarBindings = {};
            fields = Fields.new {};
            ctor = Ctors.Fields;
            methods = {};
            staticMethods = {};
            checkValue = {value = null;};
            overrideCheck = {value = null;};
          }.${fieldName} or null;
          in if def == null then null else { defaultValue = def; };
      };

      # The barest minimum s.t. Fields.new can be called without many features.
      Fields = {
        new = xs: {
          indexed = {
            set = mapAttrs quasiField xs;
            list = mapAttrs quasiField (mergeAttrsList xs);
          }.${typeOf xs};
        };
      };

      SetOf = _: {new = x: {value = x;};};
      ListOf = T: {new = x: {value = x;}; tvars = { T = Type; }; tvarBindings = { inherit T; };};
      Constraint = {new = x: {value = x; satisfiedBy = _: true;};};


      inherit (Universe.HyperType) newTemplate newSubTemplate newSubTemplateOf Void;
      inherit (mkBuiltins Quasiverse) BuiltinNames BuiltinTypes;
      inherit (BuiltinTypes) Null Int Float String Path Bool List Set Lambda;

      withUniverse = f: f Quasiverse;
      withSuperUniverse = withUniverse;
    };

    # Create a related set of types in each of .Type and .ProtoType universes.
    Universe = {
      HyperType = withTypeLevel HyperType mkUniverse;
      MetaType = withTypeLevel MetaType mkUniverse;
      ProtoType = withTypeLevel ProtoType mkUniverse;
      Type = withTypeLevel Type mkUniverse;
    };

    mkUniverse = (Type:
      let ThisUniverse = rec {

        # Inherit Type as defined to be the root of the universe
        inherit Type;

        withUniverse = f: f ThisUniverse;

        withSuperUniverse = f:
          withUniverse (U:
            let SU = superUniverse U;
            in f SU);

        # Get the super universe of a given universe U.
        # If at the root universe, return Quasiverse for bootstrapping.
        superUniverse = U:
          if U.Type.__TypeId == "HyperType" then Quasiverse
          else
            Universe.${U.Type.Super.__TypeId} or
            (throw "superUniverse: Universe ${U.Type.__TypeId} does not have a superuniverse.");

        # Construct a temporary parallel type universe to avoid recursion.
        withParallelUniverse = f:
          withSuperUniverse (SU:
            let
              SuperType = SU.Type;
              ParallelType =
                SuperType.subType "ParallelType"
                  (mkTypeArgs SU universeMethods "ParallelType");
              PU = withTypeLevel ParallelType mkUniverse;
            in
              f PU
          );

        inherit (withSuperUniverse mkBuiltins) Null Int Float String Path Bool List Set Lambda;

        # Wrap up some builtin constructors.
        # TODO: Builtin to a base type for all builtins.
        Builtin = {
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
        ### End Builtin value-types.

        # Create a new template of a type accepting type parameters.
        # The parameters can be accessed via the _ argument surrounding the type specification.
        # Returns a function from {bindings} to a new bound type.
        #
        # Due to dependency on Constraint / SetOf, exists within the Universe.
        #
        # e.g.
        # MyInt = Int.subType "MyInt" {};
        # MyTemplate = Type.template "MyTemplate" { T = Type, U = Int; } (_: {
        #   fields = Fields.new [{ t = _.T;} {u = _.U;}];
        # };
        # then
        # typeOf MyTemplate == lambda
        # (MyTemplate { T = String; U = Int }).new "abc" 123 == MyTemplate<String, Int> {t="abc"; u=123;}
        # (MyTemplate { T = String; U = MyInt }).new "abc" (MyInt.new 123) == MyTemplate<String, MyInt> {t="abc"; u=MyInt 123;}
        # (MyTemplate { T = String; U = Bool }) -> throws binding error, Bool not valid for U's Int constraint
        # MyStringInt = MyTemplate { T = String }) -> MyStringInt == MyTemplate<String, U = Int>, the partially bound template
        # MyStringInt.bind {U = MyInt} -> MyTemplate<String, MyInt>
        #
        # If bindingsToSuper is not null:
        # In this Universe, create a new template whose eventual bound type inherits from a function of its type variables.
        # For example:
        # ListOfSetOf = newSubTemplateOf (_: ListOf (SetOf T)) "ListOfSetOf" { T = Type; } (_: { ... });
        # (ListOfSetOf {T = Int}).new [{x = 123;}]; typechecks
        newSubTemplateOf =
          This: bindingsToSuper: name: tvars_: bindingsToArgs_:
          let
            # Convert the given tvars into a SetOf Constraints
            # Avoid circularity by ascending universe.
            tvars = withSuperUniverse (SU:
              (mapAttrs (_: SU.Constraint.new) tvars_)
            );

            # Convert the given (_: {...}) type template definition into one that
            # explicitly extends args with tvars and tvarBindings
            bindingsToArgs = bindings:
              let args = bindingsToArgs_ bindings;
              in args // {
                # Set the tvars to exactly those given.
                inherit tvars;
              };

            # Create initial total set of bindings that leave all tvars unbound.
            unboundBindings = mapAttrs (_: _: Void) tvars;

            # Construct a full type against the unbound bindings
            T = newSubType This name (bindingsToArgs unboundBindings);

          in
            # If bindingsToSuper is not null, we can't instantiate the type until we get
            # bindings.
            if bindingsToSuper == null
            then
              # MyTemplate = Type.template "name" { T = Type; } (_: { ... });
              # MyInt = MyTemplate.bind { T = Int; }
              # MyUnbound = MyTemplate;
              (T.bind unboundBindings).bind
            else
              # Otherwise require we are fully bound before inheriting.
              bindings:
                let T = newSubType (bindingsToSuper bindings) name (bindingsToArgs bindings);
                in T.bind (unboundBindings // bindings);

        # Create a new template with no inheritance.
        newTemplate = This: name: tvars: bindingsToArgs:
          newSubTemplateOf This null name tvars bindingsToArgs;

        # For a given This type, create a new template whose eventual bound type subtypes This.
        newSubTemplate = This: newSubTemplateOf This (_: This);

        # Unit Type
        Unit = Type.new "Unit" {
          ctor = Ctors.Nullary;
        };

        # Uninhabited type
        Void = Type.new "Void" {
          ctor = _: throw "Void: ctor";
        };

        # A constraint on a type variable.
        Constraint = Type.new "Constraint" {
          fields.constraintType = Type;
          methods = {
            # Whether a given type variable binding satisfies the constraint.
            # If the constraint is unbound, we treat as satisfied, but instantiating the unbound type
            # will throw an error.
            satisfiedBy = this: That:
              typeEq Void That
              || That.isInstance this.constraintType;
          };
        };

        # A type satisfied by all values.
        Any = Type.new "Any" {
          overrideCheck = that: true;
        };

        # A type satisfied by any value of the given list of types.
        Union = Ts:
          let
            Union_ = Type.template "Union" {Ts = Type;} (_: {
              overrideCheck = that: any (T: T.check that) _.Ts;
            });
          in
            Union_ {inherit Ts;};

        # A value or T or Null.
        NullOr = T: Union [Null T];

        # Subtype of List that enforces homogeneity of element types.
        ListOf_ = List.subTemplate "ListOf" {T = Type;} (_: {
          checkValue = that: all (x: hasType _.T x) that.value;
        });
        ListOf = T: ListOf_ { inherit T; };

        # Subtype of Set that enforces homogeneity of value types.
        SetOf = T:
          let
            SetOf_ = Set.subTemplate "SetOf" {T = Type;} (_: {
              checkValue = that: all (x: hasType _.T x) that.attrValues;
            });
          in
            SetOf_ { inherit T; };

        # A type that enforces a size on the value.
        Sized_ = Type.subTemplateOf (_: _.T) "Sized" {N = Type; T = Type;} (_: {
          checkValue = that:
            Super.checkValue that
            && that.size == _.N.literal;
        });
        Sized = n: T:
          let N = Literal n;
          in Sized_ { inherit N T; };

        # An attribute set with attributes zero-indexed by their definition order.
        # xs = Ordered.new [ {c = 1;} {b = 2;} {a = 3;} ];
        # xs.value == { a = 1; b = 2; c = 3; } (arbitrary order)
        # xs.names == [ "c" "b" "a" ] (in order of definition)
        # xs.values == [ 1 2 3 ] (in order of definition)
        OrderedOf_ =
          let
            Item = T: Sized 1 (SetOf T);
          in
            Type.subTemplateOf (_: ListOf (Item _.T)) "OrderedOf" {T = Type;} (_: {
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

                # Modify the value at the given key via this.modifyAt.name f, preserving order and type.
                modifyAt = this:
                  mapAttrs
                    (name: _: f:
                      this.fmap
                        (item:
                          let maybeModifyHere = item.modifyAt.${name} or (const item);
                          in maybeModifyHere f))
                    this.unindexed;

                # Set the value at the given key via this.setAt.name value, preserving order and type.
                # Key must exist in the Ordered (cannot create new keys).
                setAt = this: mapAttrs (name: modifyHere: x: modifyHere (const x)) this.modifyAt;

                # Set the value at the given key via this.setAtName "name" value, preserving order and type.
                # If the key does not exist, it will be added at the end of the order.
                setAtName = this: name: value:
                  if this.setAt ? ${name}
                  then this.setAt.name value
                  else this.append (Item.new {${name} = value;});

                # Get the value with the given key via this.getAt.name
                getAt = this: this.indexed;

                # Get the value with the given key and index set via this.getIndexedAt.name
                getUnindexedAt = this: this.unindexed;
              };

              checkValue = that:
                Super.checkValue that
                && assertMsg
                    (size that.attrNames == size that.unindexed)
                    "Duplicate keys in OrderedOf: ${joinSep ", " that.attrNames}";
            });
        OrderedOf = T: OrderedOf_ { inherit T; };

        # An Ordered that takes any value type
        Ordered = OrderedOf Any;

        # Base type for enums.
        Enum = Type.new "Enum" {};

        # Create an enum type from a list of item names.
        # MyEnum = mkEnum "MyEnum" [ "Item1" "Item2" "Item3" ]
        # MyEnum.names == [ "Item1" "Item2" "Item3" ]
        # MyEnum.fromName "Item1" == 0
        # MyEnum.fromIndex 0 == "Item1"
        mkEnum = enumName: itemNames:
          let Item = Enum.subType enumName {
                fields = Fields.new [
                  {index = Int;}
                  {name = String;}
                ];
                staticMethods = {
                  # Enum members live on the Enum type itself.
                  __items = This: zipListsWith Item.new (range 0 (length itemNames - 1)) names;
                  __indexToItem = This: keyByF (item: item.index) __items;
                  __nameToItem = This: keyByName __items;
                  fromIndex = This: index: This.__indexToItem.${index} or throw "Invalid index in enum ${enumName}: ${toString index}";
                  fromName = This: name: This.__nameToItem.${name} or throw "Invalid name in enum ${enumName}: ${name}";
                };
              };
          in Item;

        # A type inhabited by only one value.
        Literal = V:
          let
            Literal_ = Type.template "Literal" {V = Any;} (_: rec {
              ctor = Ctors.Nullary;
              staticMethods = {
                literal = This: _.V;
              };
            });
          in
            Literal_ { inherit V; };

        # A type inhabited by literals of any of the given list of values
        Literals = Vs: Union (map Literal values);

        # A type indicating a default value.
        Default = T: value:
          let
            Default_ = Type.template "Default" {T = Type; V = Literal T;} (_: {
              staticMethods.defaultType = _.T;
              staticMethods.defaultValue = _.V.literal;
            });
            V = Literal value;
          in
            Default_ { inherit T V; };

        # Newtype wrapper
        Static = T:
          let
            Static_ = Type.template "Static" {T = Type;} (_: {
              staticMethods.staticType = _.T;
            });
          in
            Static_ {inherit T;};

        # Either:
        # (FieldOf Int).new "myField"
        # (FieldOf (Static Int)).new "myField" -> FieldOf<Static<Int>>.fieldType == Int
        # (FieldOf (Default Int 123)).new "myField" -> FieldOf<Default<Int, 123>>.fieldType == Int
        # (FieldOf (Static (Default Int 123))).new "myField" -> FieldOf<Static<Default<Int, 123>>>.fieldType == Int
        FieldOf = T:
          let
            # TODO: Type Family candidate
            # Untag T to its root field value type.
            # e.g. untagged Static<Default<Int, 123>> == {fieldStatic = true, fieldDefault = 123; fieldType = Int; }
            #      untagged Static<Int> -> {fieldStatic = true; fieldType = Int; }
            #      untagged Default<Int, 123> -> {fieldDefault = 123; fieldType = Int; }
            #      untagged Int -> { fieldType = Int; }
            untagged = FieldType:
              {
                # Unwrap Static types.
                # If not a Static<T>, defaultType is not of type T
                Static =
                  (untagged FieldType.staticType)
                  // {fieldStatic = true;};
                # Unwrap Default types.
                # If defaultType is not of type T
                Default =
                  (untagged FieldType.defaultType)
                  // {fieldDefault = FieldType.defaultValue;};
              }.${ # typeName here to match all Default/Static, not Default<Int> etc
                  typeName FieldType
                }
                # When reaching a non-Static/Default, treat as the type.
                # Defaults here are overridden above when Static/Default are encountered.
                or {
                  fieldType = FieldType;
                  fieldStatic = false;
                  fieldDefault = null;
                };

            FieldOf_ = Type.template "FieldOf" { T = Type; } (_: {
              fields = Fields.new [
                {fieldName = String;}
                {fieldType = (untagged _.T).fieldType;}
                {fieldStatic = Bool;}
                {fieldDefault = NullOr (untagged _.T).fieldType;}
                {index = Int;}
              ];
              # ctor adds support for Static/Default type wrappers.
              # Constructed as (FieldOf T).new "fieldName"
              ctor = This: fieldName:
                {
                  inherit fieldName;
                  index = 0;
                  fieldStatic = false;
                  fieldDefault = null;
                } // (untagged _.T);
            });
          in
            FieldOf_ { inherit T; };



        # Fields is an OrderedOf that first converts any RHS values into Field types.
        # TODO: FieldOf Int must correctly type-match to FieldOf Type
        Fields = (OrderedOf (FieldOf Type)).subType {
          ctor = This: fieldListOrSet:
            let
              mkField = fieldName: T:
                # Produce a valid (Sized 1 (SetOf (FieldOf Type)))
                # ctor must return args for the supertype's mk; in this case,
                # ultimately a ListOf (...), which constructs with {value = list}.
                (Sized 1 (SetOf (FieldOf Type))).new {
                  ${fieldName} = (FieldOf T).new fieldName;
                };

              init = fieldListOrSet: {
                # Raw set converted to raw list of field singletons.
                set = let fieldList = mapAttrsToList mkField fieldListOrSet;
                      in { value = fieldList; };

                # Typed Set unwrapped to raw set and handled above
                Set = init fieldListOrSet.value;
                SetOf = init fieldListOrSet.value;

                # Raw list of field singletons conver
                # [ { fieldName: fieldType; } ... ] assignments
                # -> [ Sized 1 (SetOf (FieldOf Type)) { fieldName: (FieldOf fieldType).new(Field Any)
                list = let fieldList = map (f: let soloField = mapAttrsToList mkField f;
                                              in if length soloField != 1 then throw "Non-singleton field in Fields list: ${log.print soloField}"
                                                  else (head (attrValues soloField))) fieldListOrSet;
                      in { value = fieldList; };

                # Typed List unwrapped to raw list and handled above
                List = init fieldListOrSet.value;
                ListOf = init fieldListOrSet.value;
              }.${
                # Match against base type name, or otherwise builtin type.
                (fieldListOrSet.Type or {name = typeOf fieldListOrSet;}).name
                }
                or (throw "Invalid Fields.new argument (${typeOf fieldListOrSet}): ${log.print fieldListOrSet}");
            in init fieldListOrSet;
        };

      };
    in
      ThisUniverse
    );

  };

  # nix eval --impure --expr '(import ./cutils/types.nix {})._tests'
  _tests =
    with Types;
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

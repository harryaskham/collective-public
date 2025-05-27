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

    mkBuiltin =
      name: builtinType: isT: methods:
        Type.new name {
          fields = { value = builtinType; };
          inherit methods;
          staticMethods = {
            __builtinTypeName = This: builtinType;
            is = This: x: !(isCastError cast This x);
          };
          checkValue = that: isT that.value;
        };
    mkBuiltin_ = name: builtinType: isT:
      mkBuiltin name builtinType isT {};


    # Start Builtin value-types.

    Null = mkBuiltin_ "Null" "null" isNull;

    Int = mkBuiltin_ "Int" "int" isInt;

    Float = mkBuiltin_ "Float" "float" isFloat;

    String = mkBuiltin "String" "string" isString {
      size = this: stringLength this.value;
    };

    Path = mkBuiltin "Path" "path" isPath {
      size = this: stringLength (toString this.value);
    };

    Bool = mkBuiltin_ "Bool" "bool" isBool;

    List = mkBuiltin "List" "list" isList {
      fmap = this: f: this.modify.value (map f);
      size = this: length this.value;
      append = this: x: this.modify.value (xs: xs ++ [x]);
    };

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
    };

    ### End Builtin value-types.


    # Get the builtin type name corresponding to the given Builtin T.
    # Fails if T is not a Builtin.
    toBuiltinTypeName = T:
      T.__builtinTypeName or
        (throw "Invalid T argument for toBuiltinTypeName: ${log.print T}");

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
        if This == null then acc
        else if This' == null || typeEq This This' then acc'
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
    inheritFrom = Super: args: {
      inherit Super;
      inherit (Super) Type ctor;
    } // args;

    typeFields = Universe: with Universe; [
      # The name of the type.
      {name = String;}
      # The type of the instance as a thunk.
      {Type = (Default Type).new Type;}
      # The supertype of the type.
      {Super = (Default (NullOr Type)).new null;}
      # The type parameters of the type.
      {tvars = (Default (NullOr (SetOf Constraint))).new null;}
      # The type parameter bindings of the type.
      {tvarBindings = (Default (NullOr (SetOf Type))).new null;}
      # The constructor function creating the fields of the type as a set to pass to mk.
      {ctor = (Default Ctor).new Ctors.Fields;}
      # A set of ordered fields to make available as this.___ and this.get.___, this.set.___, etc
      {fields = (Default Fields).new (_: Fields.new {});}
      # A set of methods from this to make available as this.___
      {methods = (Default (SetOf Lambda)).new {};}
      # A set of methods from This to make available as This.___ and this.___
      {staticMethods = (Default (SetOf Lambda)).new {};}
      # Perform additional checks on the value of the type when comparing.
      {checkValue = (Default (NullOr Lambda)).new null;}
      # If set, ignore all other checks and use this check function only.
      {overrideCheck = (Default (NullOr Lambda)) null;}
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
                then throwBindError "Type ${This.__TypeId} already has type variable ${tvarName} bound to ${typeName This.tvarBindings.getAt.${tvarName}}"
              else if !(constraint.satisfiedBy T)
                then throwBindError "Type ${typeName T} does not satisfy constraint: ${log.print constraint}"
              else This.modify.tvarBindings (bs: bs.setAt.${tvarName} T);

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
    };

    baseMethods = Universe:
      universeIndependentBaseMethods
      // (universeDependentBaseMethods Universe);

    # Default values for field instantiation in HyperType, before we can use ProtoType.Fields
    # or ProtoType.Default.
    hyperFieldDefaults = {
      Super = null;
      fields = {indexed = {};};
      ctor = _: {};
      methods = {};
      staticMethods = {};
    };

    # Manually construct an untyped indexed field.
    hyperField = index: fieldName:
      {
        ${fieldName} = {
          inherit fieldName index;
          fieldStatic = false;
          fieldType = null;
          fieldDefault = { defaultValue = hyperFieldDefaults.${fieldName} or null; };
        };
      };

    # Manually construct the untyped indexed fields by transforming the typed
    # field specifications of Type, such that HyperType can be instantiated to
    # bootstrap via HyperType -> MetaType -> ProtoType -> Type.
    hyperFields = {
      indexed =
        mergeAttrsList
          (imap0
            (index: field: hyperField index (head (attrNames field)))
            (typeFields Universe.HyperType));
    };

    # HyperType is constructed such that it is both:
    # - a valid type input to mkInstance for the creation of MetaType
    # - a valid output from mkInstance HyperType, manually created.
    # This allows us to ground the infinite recurse we'd otherwise get by needing to actually create HyperType as an instance
    # of itself or a higher type.
    #
    # - HyperType.Type is manually set to be self-referential, as it would be if HyperType was created via 'mkInstance HyperType'
    # - Super is manually set to be null.
    # - HyperType.methods are restricted only to a Universe-independent set. Further method construction requires reference to the Universe. These are added in MetaType and bound in ProtoType.
    # - HyperType.get/set/modify/call accessors are missing owing to having not itself gone through mkInstance (this happens in MetaType creation).
    # - Other defaults are manually set.
    HyperType = {
      __TypeId = "HyperType";
      name = "HyperType";
      Type = HyperType;
      Super = null;
      tvars = null;
      tvarBindings = null;
      fields = hyperFields;
      ctor = _: name: arg: arg // { inherit name; };
      methods = universeIndependentBaseMethods;
      staticMethods = {};
      checkValue = null;
      overrideCheck = null;
    };

    # MetaType is the instantiated HyperType, which is both an instance of
    # HyperType and also inherits from HyperType.
    # __TypeId must be manually set as the boundName method is not in place at this point.
    MetaType = newSubType HyperType "MetaType" {
      methods =
        (baseMethods Universe.MetaType)
        // (mkTypeMethods Universe.HyperType MetaType);
    };

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
      if (This.Super or null) == null then {}
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
    # unspecified fields and non-overridden methods.
    # Unless ctor is specified, the ctor is also inherited.
    newSubType = This: name: args:
      if !(isType This) then throw "Cannot subtype non-Type or Type-precursor: ${log.print This}"
      else newInstance This.Type name (inheritFrom This args);

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
            allFieldNames = attrNames (mergeSuper (T: T.fields.indexed) This);
            unknownFieldNames = attrNames (removeAttrs args allFieldNames);

            # Get any fields not populated in this or any supertype.
            populatedFieldNames = filter (name: this ? ${name}) allFieldNames;
            requiredFields = filterAttrs (_: field: field.fieldDefault != null) This.fields.indexed;
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

    # Create a related set of types in each of .Type and .ProtoType universes.
    Universe = {
      HyperType = withTypeLevel HyperType mkUniverse;
      MetaType = withTypeLevel MetaType mkUniverse;
      ProtoType = withTypeLevel ProtoType mkUniverse;
      Type = withTypeLevel Type mkUniverse;
    };

    mkUniverse = (Type: rec {

      # Create a new template of a type accepting type parameters.
      # The parameters can be accessed via the _ argument surrounding the type specification.
      # Returns a function from {bindings} to a new bound type.
      #
      # Due to dependency on Constraint / SetOf, exists within the ProtoType Universe.
      #
      # e.g.
      # MyInt = Int.subType "MyInt" {};
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
      newTemplate =
        This: name: tvars_: bindingsToArg_:
        let
          tvars = mapAttrs (_: Constraint.new) tvars_;
          bindingsToArg = bindings: (bindingsToArg_ bindings) // {
            inherit tvars;
            tvarBindings = bindings;
          };
          unboundBindings = mapAttrs (_: _: Unbound) tvars;
          unboundArg = bindingsToArg unboundBindings;
          T = This.new name unboundArg;
        in T.bind;

      Void = Type.new "Void" {
        ctor = _: throw "Void.new: Void has no inhabitants";
      };

      # A type specific to unbound type variables
      Unbound = Void.subType "Unbound" {};

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

      # A type satisfied by all values.
      Any = Type.new "Any" {
        overrideCheck = that: true;
      };

      # A type satisfied by any value of the given list of types.
      Union = Type.template "Union" {Ts = ListOf Type;} (_: {
        overrideCheck = that: any (T: T.check that) _.Ts.value;
      });

      # A value or T or Null.
      NullOr = T: Union {Ts = [Null T];};

      # A Ctors.CtorName is of form:
      # Ctor.new (This: args...: <field-value attrs to pass to mk>)
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
              let
                # Here 'indexed' is manually set on the ProtoType
                fields = filterAttrs (_: f: !f.fieldStatic) This.fields.indexed;
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

      # Subtype of List that enforces homogeneity of element types.
      ListOf = Type.template "ListOf" {T = Type;} (_: {
        checkValue = that: all (x: hasType _.T x) that.value;
      });

      # Subtype of Set that enforces homogeneity of value types.
      SetOf = newTemplate Type "SetOf" {T = Type;} (_: {
        checkValue = that: all (x: hasType _.T x) that.attrValues;
      });

      # A type that enforces a size on the value.
      Sized = n:
        let Sized_ = Type.template "Sized" {N = Literal n; T = Has "size";} (_:
              inheritFrom _.T {
                checkValue = that:
                  Super.checkValue that
                  && that.size == _.N.literal;
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
      Ordered = (OrderedOf Any).subType "Ordered" {};

      # Base type for enums.
      Enum = Type.new "Enum" {};

      # Create an enum type from a list of item names.
      # MyEnum = mkEnum "MyEnum" [ "Item1" "Item2" "Item3" ]
      # MyEnum.names == [ "Item1" "Item2" "Item3" ]
      # MyEnum.fromName "Item1" == 0
      # MyEnum.fromIndex 0 == "Item1"
      mkEnum = enumName: itemNames:
        let Item = Enum.subType enumName {
              fields = [
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
      Literal = Type.template "Literal" {Value = Any;} (_: rec {
        ctor = Ctor.Nullary;
        staticMethods = {
          literal = This: _.Value;
        };
      });

      # A type inhabited by literals of any of the given list of values
      Literals = values: Union (map Literal values);

      # A type indicating a default value.
      Default = Type.template "Default" {T = Type;} (_: {
        fields.defaultValue = _.T;
      });

      # Newtype wrapper
      Static = Type.template "Static" {T = Type;} (_: {});

      # Either:
      # Field.new "myField" Int
      # Field.new "myField" (Static Int)
      # Field.new "myField" (Default 123) -> type int
      # Field.new "myField" (Default (Int.new 123)) -> type Int
      # Field.new "myField" (Static (Default 123)) -> type (Static int)
      Field = Type.template "Field" {T = Type;} {
        fields = [
          {fieldName = String;}
          {fieldType = Default (NullOr T) null;}
          {fieldStatic = Default Bool false;}
          {fieldDefault = Default (NullOr (Default T)) null;}
          {index = Default Int 0;}
        ];
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

    });

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

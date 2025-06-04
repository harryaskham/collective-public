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
#      String.check "abc" == false
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
    builtinNames = [ "null" "int" "float" "string" "path" "bool" "list" "set" "lambda" ];
    builtinNameCheck_ = mergeAttrsList (map (name: { ${name} = true; }) builtinNames);
    # Whether or not name is one of the lowercase builtin type names.
    builtinNameCheck = name: isString name && (builtinNameCheck_.${name} or false);

    BuiltinNames = [ "Null" "Int" "Float" "String" "Path" "Bool" "List" "Set" "Lambda" ];
    BuiltinNameCheck_ = mergeAttrsList (map (name: { ${name} = true; }) BuiltinNames);
    # Whether or not name is one of the uppercase Builtin type names.
    BuiltinNameCheck = name: isString name && (BuiltinNameCheck_.${name} or false);

    # Whether or not x is a lowercase builtin type
    # All builtins are attrs. The short-circuit stops recursive inspection of Type.
    builtinValueCheck = x:
      if isAttrs x then !(x ? Type)
      else true;

    # Whether or not x is an uppercase Builtin type
    # All builtins are attrs. The short-circuit stops recursive inspection of Type.
    BuiltinValueCheck = x:
      (x ? Type) && (BuiltinNameCheck (x.Type {}).name or "");

    # Cast between types.
    # Returns a set with a 'castError' attribute if the cast is not possible
    # or a set with a 'castSuccess' attribute containined the cast value.
    # Also includes a 'msgs' attribute with a list of messages recording
    # cast attempts.
    mkCastError = msg: { castError = msg; };
    mkCastSuccess = value: msg: { castSuccess = value; castSuccessMsg = msg; };
    isCastError = x: x ? castError;
    isCastSuccess = x: x ? castSuccess;
    castErrorOr = xOrError: f:
      if isCastError xOrError then xOrError else f xOrError;

    cast = T: x:
      if T == null
        then mkCastError "Cannot cast to null: T = ${log.print T}, x = ${log.print x}"
      else if !(isTypeSet T || builtinNameCheck T)
        then mkCastError "Invalid target type provided for cast: T = ${log.print T}, x = ${log.print x}"
      else let
        printT = T: log.print T;

        TName = printT T;
        xTName = getTypeBoundName x;

        xFields = if isTyped x then filterAttrs (_: f: !f.fieldStatic) (x.Type {}).fields.indexed else
          mkCastError ''
            Cannot get fields from untyped uncast value: T = ${log.print T}, x = ${log.print x}
            '';
        TFields = if isTypeSet T then filterAttrs (_: f: !f.fieldStatic) T.fields.indexed else mkCastError ''
            Cannot get fields from non-Type target type: T = ${log.print T}, x = ${log.print x}
            '';

        xIsUnary = !(isCastError xFields) && size xFields == 1;
        TIsUnary = !(isCastError TFields) && size TFields == 1;

        xUnaryField = castErrorOr xFields (fs: head (attrValues fs));
        TUnaryField = castErrorOr TFields (fs: head (attrValues fs));

        xUnaryFieldName = castErrorOr xUnaryField (field: field.fieldName);
        TUnaryFieldName = castErrorOr TUnaryField (field: field.fieldName);

        xUnaryFieldT = castErrorOr xUnaryField (field: field.fieldType);
        TUnaryFieldT = castErrorOr TUnaryField (field: field.fieldType);

        xUnaryFieldTName = castErrorOr xUnaryFieldT printT;
        TUnaryFieldTName = castErrorOr TUnaryFieldT printT;

        xFieldNames = castErrorOr xFields (fs: sortOn (f: f.index) (mapAttrsToList (fieldName: _: fieldName) fs));
        TFieldNames = castErrorOr TFields (fs: sortOn (f: f.index) (mapAttrsToList (fieldName: _: fieldName) fs));

        castStr = "${xTName} -> ${TName}";

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
            orMsg = indent.block ''
              Not an identity cast:
                ${indent.here castStr}
            '';
            result = mkCastSuccess x "";
            failMsg = _: null;
            successMsg = _: "Identity cast succeeded: ${xTName} -> ${TName}";
          }

          # {
          #   name = "Coerce";
          #   when = (T.checkValue or (const false)) x;
          #   orMsg = indent.block ''
          #     No value-check or not directly checkValue-coercible:
          #       ${indent.here castStr}
          #   '';
          #   result = mkCastSuccess (T.mk x.get) "";
          #   failMsg = _: null;
          #   successMsg = _: "Coercion cast succeeded: ${xTName} -> ${TName}";
          # }

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
            when = !(isCastError xFieldNames) && !(isCastError TFieldNames)
                   && (length xFieldNames == length TFieldNames);
            orMsg = ''
              Cannot sidecast unless from a typed instance to a Type with the same field count: ${xTName} -> ${TName}
                Source fields: ${log.print xFieldNames}
                Target fields: ${log.print TFieldNames}
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
        if isNull This then acc
        else if isNull This' || typeEq This This' then acc'
        else foldUpward f acc' This';

    # Merge an attribute of This's inheritance chain, with This attributes overriding those of Super.
    # Does not deeply merge so should be used to pick out a single flat mergable like 'methods' such that
    # this models This inheriting Super's methods, but applying any overrides to them by redefining.
    #
    # e.g. mergeSuper (This: This.methods) This ==
    #        This.Super.Super...methods // ... // This.Super.methods // This.methods;
    mergeSuper = f: foldUpward (acc: This: let xs = f This |> def {}; in acc // xs) {};

    # Modify an argument set to inherit from a supertype.
    # If a ctor is set in args, it is used, otherwise the Super's ctor is inherited.
    # Fields and methods are also inherited, with any new ones overriding any previously defined.
    inheritFrom = Super: ctorArgs:
      if !(isTypeSet Super) then throw "inheritFrom: Super must be a Type, got ${log.print Super}"
      else
        ctorArgs // {
          inherit Super;  # Not a thunk here; Type ctor converts it.
          ctor = ctorArgs.ctor or Super.ctor;
          fields = Super.fields.update (ctorArgs.fields or {});
          methods = Super.methods // (ctorArgs.methods or {});
          staticMethods = Super.staticMethods // (ctorArgs.staticMethods or {});
        };

    # Construct the fields for a universe using the types of the universe above.
    mkTypeFieldListFor = opts: U:
      let
        SU = U._SU.get {};
        maybeTyped = T: if opts.enableTypeChecking then T else null;
      in
        [
          # The name of the type.
          {name = maybeTyped (SU.String);}
          # The type of the instance as a thunk.
          # Defaults to this universe's U.Type, as types in this universe will be
          # created using U.Type and this field's default dictates their Type.
          {Type = maybeTyped (SU.Default "lambda" (_: U.Type));}
          # The supertype of the type.
          {Super = maybeTyped (SU.Default (SU.NullOr "lambda") null);}
          # The type parameters of the type.
          {tvars = maybeTyped (SU.Default "set" {});}
          # The type parameter bindings of the type.
          {tvarBindings = maybeTyped (SU.Default "set" {});}
          # The constructor function creating the fields of the type as a set to pass to mk.
          {ctor = maybeTyped (SU.Default "lambda" Ctors.Fields);}
          # A set of ordered fields to make available as this.___ and this.get.___, this.set.___, etc
          {fields = maybeTyped (SU.Default SU.Fields (SU.Fields.new []));}
          # A set of methods from this to make available as this.___
          {methods = maybeTyped (SU.Default "set" {});}
          # A set of methods from This to make available as This.___ and this.___
          {staticMethods = maybeTyped (SU.Default "set" {});}
          # Perform additional checks on the value of the type when comparing.
          {checkValue = maybeTyped (SU.Default (SU.NullOr "lambda") null);}
          # If set, ignore all other checks and use this check function only.
          {overrideCheck = maybeTyped (SU.Default (SU.NullOr "lambda") null);}
        ];

    mkTypeFieldsFor = opts: U:
      let
        SU = U._SU.get {};
        fieldList = mkTypeFieldListFor opts U;
      in SU.Fields.new fieldList;

    # Map a function over the supplied type levels, producing a set keyed
    # by Type name. Can be used to construct the same type in multiple different
    # Type universes.
    withTypeLevels = Levels: f:
      mergeAttrsList (map (Level: { ${Level.__TypeId} = f Level; }) Levels);

    typeMethodsFor = Universe: {
      # Is this instance an instance of type That, or inherits from it?
      isInstance = This: That:
        typeEq (This.Type {}) That
        || (This.Super != null && (This.Super {}).isInstance That);

      # Get the full templated name of the type.
      boundName = This:
        if This.tvars == {} then This.name
        else
          let
            printBinding = tvarName:
              let C = This.tvars.${tvarName} or (
                    throw "No type variable ${tvarName} on ${This.name}");
                  T = This.tvarBindings.${tvarName} or Void;
              in
                # Unbound
                if typeEq Universe.Void T
                  then tvarName

                # Bound to a type
                else if isTyped T
                  then T.boundName

                # Bound to a literal or builtin
                else
                  with log.prints; put T _line ___;
            printBindings = joinSep ", " (map printBinding (attrNames This.tvars));
          in "${This.name}<${printBindings}>";

      # Construct the type resulting from applying the given bindings to the parameterized This type.
      # TODO: TypeVar object plus bootstraps
      bind = This: tvarBindingsAttrs:
        let
          tvarBindingsList =
            mapAttrsToList
              (tvarName: T: {inherit tvarName T;})
              tvarBindingsAttrs;

          bindOne = This: {tvarName, T}:
            let
              throwBindError = msg: throw (indent.block ''
                Bind error:
                  ${indent.here "${This.name}.${tvarName} <- ${log.print T}"}
                  Constraints: ${indent.here "${log.print C.value} (${if TSatC then "" else "not "}satisfied)"}
                  Existing binding: ${log.print B}
                  ${indent.here msg}
                '');
              C = This.tvars.${tvarName} or null;
              B = This.tvarBindings.${tvarName} or Void;
              TSatC = C.satisfiedBy T;
            in
              if C == null
                then throwBindError "Type ${This.name} does not have type variable ${tvarName}"

              else if This.tvarBindings == null
                then throwBindError "Type ${This.__TypeId} does not have bound or unbound type variable bindings"

              else if !(typeEq B Void)
                then throwBindError "Type ${This.__TypeId} already has type variable ${tvarName} bound to ${log.print B}"

              else if !(C.satisfiedBy T)
                then throwBindError "Binding ${log.print T} does not satisfy constraint: ${log.print C}"

              else This.modify.tvarBindings (bs: bs // {${tvarName} = T;});

        in foldl' bindOne This tvarBindingsList;

      # Does this type inherit from That?
      inheritsFrom = This: That:
        typeEq This That
        || (This.Super != null && (This.Super {}).inheritsFrom That);

      check = This: that:
        let
          runChecks = doChecksNoAssert [
            {
              cond = hasType This that;
              msg = "Type check failed for ${This.__TypeId} (got ${That.__TypeId})";
            }
            {
              cond = This.checkValue == null || This.checkValue that;
              msg = "checkValue failed: ${log.print that} is not a value of type ${This.__TypeId}";
            }
          ];
        in if This.overrideCheck != null
          then This.overrideCheck that
          else runChecks;

      # Use the bound name with its ordered param assignments to determine type identity
      # and equality.
      # For bootstrap types this may not be bound yet, falling back to the name.
      __TypeId = This: This.boundName or This.name;

      # Create a new instance of the type by providing field values
      mk = mkInstance;

      # Create a new instance of the type by calling This's constructor
      # When This == Type, creates a new Type.
      new = This:
        with log.vtrace.methodCall This "new" ___;
        return (newInstance This);

      # Create a new subType inheriting from This
      # TODO: Compose checkValue / overrideCheck if they appear in args
      subType = Universe.newSubType;

      # For a given This type, create a new template whose eventual bound type subtypes This.
      subTemplate = This: Universe.newTemplate_ This This;

      # Create a new template of a type accepting type parameters.
      # The parameters can be accessed via the _ argument surrounding the type specification.
      # Returns a function from {bindings} to a new bound type.
      template = Universe.newTemplate;

      # Create a new template inheriting from a function of This plus any type variables.
      # TODO: Generic inheritance and tvar/tvarBinding merging.
      subTemplateOf =
        This: bindingsToSuper: name: tvars_: bindingsToArgs_:
        Universe.newTemplate_ This bindingsToSuper name tvars_ bindingsToArgs_;

      # __toString needs to take a this parameter for toString compliance.
      # Therefore we have Type.staticMethods.__toString which ends up on instances as (instance.__toString instance) which is correct.
      # This will work to print both types as T.__toString T and instances as t.__toString t
      __toString = This: this:
        if isTypeSet this && isString this.__TypeId then
          this.__TypeId
        else
          with log.prints; put this _raw ___;

      __print = this: maxDepth:
        let truncated = deepMapWith (depth: x: if depth >= maxDepth then "..." else x) this;
        in log.print_ (log.mkPrintArgs // { ignoreToString = true; }) truncated;
    };

    mkTypeArgsFor = opts: U:
      rec {
        name = opts.typeName;
        Super = null;
        # The defaults here are only required for universes with typechecking disabled.
        # Otherwise they are set per the Default values in mkTypeFieldListFor.
        ctor = This: name: args: {
          inherit name;
          Super = if args ? Super then (_: args.Super) else null;
          ctor = args.ctor or Ctors.Fields;
          fields = args.fields or (U.Fields.new []);
          methods = args.methods or {};
          staticMethods = args.staticMethods or {};
          tvars = args.tvars or {};
          tvarBindings = args.tvarBindings or {};
          checkValue = args.checkValue or null;
          overrideCheck = args.overrideCheck or null;
        };
        fields = mkTypeFieldsFor opts U;
        methods = typeMethodsFor U;
        staticMethods = {};
        tvars = {};
        tvarBindings = {};
        checkValue = null;
        overrideCheck = null;
      };

    # Check if a given argument is a custom Type.
    isTyped = x: isAttrs x && x ? Type;

    # Check if a given argument is a Type.
    # 'isType' collides with lib.isType.
    isTypeSet = T: T ? __TypeId;

    # Gets the type of a value, whether it is a Type or a builtin type.
    getRawType = x:
      if isTyped x then x.Type {}
      else typeOf x;

    # Gets the type of a value, whether it is a Type or a builtin type.
    getType = x:
      if isTyped x then x.Type {}
      else Builtin.FromT (typeOf x);

    # Get the name of a type whether builtin or Type.
    getTypeName = x:
      if isTyped x
        then (x.Type {}).name or (throw ''
          Type is missing name in getTypeName:
          ${indent.here (log.print (x.Type {}))}
        '')
        else typeOf x;

    # Get the bound name of a type whether builtin or Type.
    getTypeBoundName = x:
      if isTyped x
        then (x.Type {}).boundName or throw ''
          Type is missing boundName in getTypeBoundName:
          ${indent.here (log.print (x.Type {}))}
        ''
        else typeOf x;

    # Check two types for equality, supporting both Types and builtin type-name strings.
    typeEq = T: U:
      if isTypeSet T && !isTypeSet U then false
      else if !isTypeSet T && isTypeSet U then false
      else if isTypeSet T && isTypeSet U then T.__TypeId == U.__TypeId
      else if !(builtinNameCheck T) then false # throw "typeEq ${log.print T} ${log.print U}: ${log.print T} is not a Type or builtin type"
      else if !(builtinNameCheck U) then false # throw "typeEq ${log.print T} ${log.print U}: ${log.print U} is not a Type or builtin type"
      else T == U;

    # Check a string or custom type against a value.
    hasType = T: x: typeEq T (getRawType x);

    # TODO: Unused
    staticFields = This: filterAttrs (_: f: f.fieldStatic or false) (This.fields.indexed or {});
    instanceFields = This: filterAttrs (_: f: !(f.fieldStatic or false)) (This.fields.indexed or {});

    setType = This: this: this // { Type = _: This;};

    # Set the value of a field on an instance 'this' of type 'This'
    #
    # This is the only mechanism through which an instance should have a field updated or
    # modified.
    # TODO:
    # - Enforce this via having fields be read-only via thunk such that updates are meaningless
    # - Fields-as-thunks would also mean we can lazily compose field updates and only force them
    #   as needed (could also only type-check on access enabling invalid intermediate field
    #   values that can never be seen)
    #
    # Handles:
    #
    # - Casting the assigned value to the field type if necessary and possible
    #
    # - Typechecking the assigned value
    #
    # - Typechecking 'this' after assignment, for those cases where the update has
    #   broken a runtime checkValue invariant.
    #
    #   For example:
    #
    #     SortedListOf = T: (ListOf T).subType {
    #       checkValue = that: that.value == sort that.value;
    #       methods = { add = this: x: this.modify.value (xs: sort [x] ++ [xs]); };
    #     }
    #     SortedInts = SortedListOf Int;
    #     SortedInts.new [1 3 2] -> Type error
    #     xs = SortedInts.new [1 2 3] -> ok
    #
    #     Invalid values are always possible to construct manually:
    #     invalid = xs // { value = xs.value ++ [0]; }
    #
    #     Now we have the following, which will not be continuously checked as checkValue is
    #     only called on construction:
    #     invalid.value == [1 2 3 0]
    #
    #     Instead:
    #     valid = xs.add 0 -> ok; valid.value == [0 1 2 3]
    #     xs.add "no" -> Type error via (ListOf T).check
    #     xs.modify.value reverseList -> Type error via SortedListOf.checkValue
    #     xs.set.value [10 20 -30] -> Type error via SortedListOf.checkValue
    #     valid = xs.set.value [10 20 30] -> ok
    setByName = This: this: fieldName: uncastValue:
      let field = (This.fields.indexed or {}).${fieldName} or null;
          castRequired = 
            field.fieldType != null
            && !((isTypeSet field.fieldType && field.fieldType.check uncastValue)
                 || (hasType uncastValue field.fieldType));
          castValue =
            if castRequired
              then cast field.fieldType uncastValue
              else mkCastSuccess uncastValue "id";
      in assert (predChecks [
          { pred = (field: field != null);
            msg = joinLines [
              "Setting unknown field: ${This.name}.${fieldName}"
              "Known fields: ${joinSep ", " (attrNames (This.fields.indexed or {}))}"
            ];
          }
          { pred = field: (!castRequired) || isCastSuccess castValue;
            msg = indent.block ''
              Error casting field assignment:
                ${This.__TypeId}.${fieldName} = ${log.print uncastValue}
                ${indent.here castValue.castError}
            '';
          }
          # TODO: This check could be O(n) for container types, updating attrsets goes O(1) to O(n)
          # Need item-wise checks
          { pred = field: (!castRequired)
                          || (isTypeSet field.fieldType && field.fieldType.check castValue.castSuccess)
                          || (typeOf castValue.castSuccess == field.fieldType);
            msg = indent.block ''
              Cast value did not pass typecheck:
                ${This.__TypeId}.${fieldName} = ${log.print uncastValue}
                Cast value of ${log.print (castValue.castSuccess or null)} is not a valid instance of ${log.print field.fieldType}.
            '';
          }
        ] field);
        # We need to reinit here to update the logical binding for future updates
        reinitThis This (this // {
          ${fieldName} = castValue.castSuccess;
        });

    # Accessors to include on all instances.
    # Since modifying here requires a new instance with updated accessors, and we don't
    # want to have to call as e.g. (this.fn this arg1 arg2) we need to set the accessors
    # again here in 'set' bound to the new instance.
    setAccessors = This: this:
      let this_ =
        this // rec {
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
            (This.fields.indexed or {});

        # Field checking interface.
        # e.g. this.has.someInt -> true
        #      this.has ? someInt -> true
        #      this.has.notAField -> throws error
        #      this.has ? notAField -> false
        #      this.has.notAField or default -> default
        has = mapAttrs (_: _: true) (This.fields.indexed or {});

        # Field setting interface
        # e.g. this.set.someInt 123 -> this'
        #      this.set.someInt "123" -> throws Type error
        set =
          mapAttrs
            (fieldName: _: x: setByName This this fieldName x)
            (This.fields.indexed or {});

        # Field modification interface
        # e.g. this.modify.someInt (x: x+1) -> this'
        #      this.modify.someInt toString -> throws Type error
        modify =
          mapAttrs
            (fieldName: _: f: set.${fieldName} (f this.${fieldName}))
            (This.fields.indexed or {});
      };
    in this_;

    # Set all fields on the instance
    # Occurs before methods are set, so cannot use the this.set interface directly.
    setFields = This: args: this:
      foldl'
        (this: field:
          let shouldSetValue = args ? ${field.fieldName} || (field.fieldDefault or null) != null;
              value = args.${field.fieldName} or field.fieldDefault.defaultValue;
          in
            if !(isAttrs field) || !(isString field.fieldName) then with indent; throws.block ''
              Invalid field encountered in setFields:

                This = ${here (print This)}

                field = ${here (print field)}
            ''
            else if this ? ${field.fieldName} && shouldSetValue then throw ''
              Field ${field.fieldName} already set to ${log.print this.${field.fieldName}}
              (when setting to ${log.print args.${field.fieldName}})
            ''
            else if shouldSetValue then setByName This this field.fieldName value
            else this
        )
        this
        (attrValues (instanceFields This));

    # Get all the static methods for a given type.
    boundStaticMethods = This:
      mapAttrs
        (name: staticMethod: staticMethod This)
        (This.staticMethods or {});

    # Bind the methods of a This instance this, given a reference to the bound
    # instance as this_.
    boundMethods = This: this_:
      mapAttrs (_: method: method this_) (This.methods or {});

    # Bind this instances' Type's static methods to the instance.
    setTypeStaticMethods = This: this:
      this // boundStaticMethods This;

    # Bind this instances' own static methods to the instance.
    setInstanceStaticMethods = this:
      this // boundStaticMethods this;

    # Copy over bound methods to the instance.
    setMethods = This: this:
      let this_ = this // boundMethods This this_;
      in this_;

    # Initialise a type from its This type, its partial this set, and any args.
    initThis = This: this: args:
      this
        |> (setType This)
        |> (setAccessors This)
        |> (setFields This args)
        |> (setTypeStaticMethods This)
        |> setInstanceStaticMethods
        |> (setMethods This);

    # Reconstruct the This instance after change.
    reinitThis = This: this:
      this
        |> (setAccessors This)
        |> (setMethods This);

    # Create a new instance of a type by calling its constructor.
    # The constructor's output arguments are then passed into mkInstance.
    # For types, the constructor just merges a name parameter with an arguments
    # parameter and delegates to mkInstance.
    newInstance = This:
      with log.vtrace.call "newInstance" { inherit This; } ___;

      let mkViaCtor =
            traceComposeVariadic "mkViaCtor"
              "(mkInstance This)" "(This.ctor This)"
              (mkInstance This)   (This.ctor This);
      in traceVariadic mkViaCtor;

    # Build a new instance from a single dict of field values.
    # Construct an instance from a This type and attrset of field-to-value assignments.
    # arg = { fieldName = value, ... } for all fields.
    # Fields with defaults can be omitted.
    # Build a new instance from a single dict of field values.
    mkInstance = This: args:
      with log.vtrace.call "mkInstance" This args ___;
      let
        # Construct 'this' as an instance of 'This'.
        this = {} |> initThis This args;

        # Check the validity of the constructed instance.
        checks =
          let
            # Get any supplied non-static fields not present in this or any supertype.
            # TODO: Remove mergeSuper ref
            # allFieldNames = attrNames (mergeSuper (T: T.fields.indexed or {}) This);
            allFieldNames = attrNames (This.fields.indexed or {});
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
                This: ${log.printAttrs This}
                this: ${log.print this}
              '';
            }
          ];
      in
        assert (doChecks checks);
        return this;

    # Inline assertion for runtime type assertion.
    must = T: x:
      if hasType T x then x
      else throw "Expected type ${T} (got ${getTypeName x})";

    # A = Type.new "A" {};
    # B = Type.new "B" { Super = A; };
    #
    # isSuperTypeOf A B == true
    # isSuperTypeOf B A == false
    # isSuperTypeOf A A == false
    # isSuperTypeOf Type A == true
    # isSuperTypeOf Type B == true
    isSuperTypeOf = Super: T:
      if T == null || (T.Super or null) == null then false
      else typeEq Super (T.Super {}) || isSuperTypeOf Super (T.Super {});

    isSubTypeOf = flip isSuperTypeOf;

    Ctors = {

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
        with log.vtrace.call "Ctors.Fields" { inherit This; } ___;
        let
          # Sort non-static fields manually to avoid a new call to Ordered
          fields = assign "fields" (filterAttrs (_: f: !f.fieldStatic) This.fields.indexed);
          sortedFields = assign "sortedFields" (sortOn (f: f.index) (attrValues fields));
          sortedFieldNames = assign "sortedFieldNames" (map (f: f.fieldName) sortedFields);
        in return (
          if size fields == 0 then {}
          else Variadic.mkOrdered sortedFieldNames
        );
    };

    mkBuiltin = U: name:
      let
        hasSize = { String = true; Path = true; List = true; Set = true; }.${name} or false;
        withSize = methods: if hasSize then methods // { size = this: cutils.functions.size this.value; } else methods;
      in
        U.Type.new name {
          fields = U.Fields.new [{ value = toLower name; }];
          methods = withSize ({
            List = {
              fmap = this: f: this.modify.value (map f);
              append = this: x: this.modify.value (xs: xs ++ [x]);
            };
            Set = {
              fmap = this: f: this.modify.value (mapAttrs (_: f));
              attrNames = this: attrNames this.value;
              attrValues = this: attrValues this.value;
              # e.g. this.modifyAt.name (x: x+1)
              getAt = this: this.value;
              modifyAt = this: mapAttrs (k: v: f: this.modify.value (xs: xs // { ${k} = f v; })) this.value;
              setAt = this: mapAttrs (k: _: x: this.modify.value (xs: xs // { ${k} = x; })) this.value;
              setAtName = this: name: value:
                if this.setAt ? ${name}
                then this.setAt.${name} value
                else this.modify.value (xs: xs // {${name} = value;});
            };
            Lambda = {
              fmap = this: f: this.modify.value (compose f);
            };
          }.${name} or {});
          checkValue = that: {
            # Additional check on sets s.t. we don't accept a typed value when expecting
            # a raw set.
            Set = !(that ? Type);
          }.${name} or true;
        };

    mkBuiltins = U:
      mergeAttrsList (map (name: { ${name} = mkBuiltin U name; }) BuiltinNames);

    # e.g. parseFieldSpec Static<Default<Int, 123>> -> {fieldStatic = true, fieldDefault = 123; fieldType = Int; }
    #      parseFieldSpec Static<Int> -> {fieldStatic = true; fieldType = Int; }
    #      parseFieldSpec Default<Int, 123> -> {fieldDefault = 123; fieldType = Int; }
    #      parseFieldSpec Int -> { fieldType = Int; }
    parseFieldSpec = Spec:
      with log.vtrace.call "parseFieldSpec" Spec ___;
      return ({
        # Unwrap Static types.
        # If not a Static<T>, defaultType is not of type T
        Static =
          (parseFieldSpec Spec.staticType)
          // {fieldStatic = true;};
        # Unwrap Default types.
        # If defaultType is not of type T
        Default =
          (parseFieldSpec Spec.defaultType)
          // {fieldDefault = Spec.defaultValue;};
      }.${ # getTypeName here to match all Default/Static, not Default<Int> etc
          getTypeName Spec
        }
        # When reaching a non-Static/Default, treat as the type.
        # Defaults here are overridden above when Static/Default are encountered.
        or {
          fieldType = Spec;
          fieldStatic = false;
          fieldDefault = null;
        });

    # Universe-independent types that only depend on Type.
    mkTrivialTypes = Type: rec {
      # Unit Type
      Unit = Type.new "Unit" {ctor = Ctors.Nullary;};
      unit = Unit.new;

      # Uninhabited type
      Void = Type.new "Void" {ctor = _: _: throw "Void: ctor";};
    };

    mkTemplating = U: rec {
      # For a given type, create a new type of the same Type with the given name and args, inheriting any
      # unspecified fields, ctor, and non-overridden methods.
      newSubType = This: name: args:
        with log.vtrace.call "newSubType" { inherit This name args; } ___;

        return (
          if !(isTypeSet This) then throw "Cannot subtype non-Type or Type-precursor: ${log.print This}"
          else U.Type.new name (inheritFrom This args)
        );

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
      newTemplate_ =
        This: bindingsToSuperOrSuper: name: tvars_: bindingsToArgs_:
        let
          # Convert the given tvars into a SetOf Constraints
          tvars = mapAttrs (_: U.Constraint.new) tvars_;

          # Convert the given (_: {...}) type template definition into one that
          # explicitly extends args with tvars and tvarBindings
          voidBindings = mapAttrs (_: _: U.Void) tvars;
          bindingsToArgs = tvarBindings:
            let args = bindingsToArgs_ tvarBindings;
            in args // {
              # Set the tvars to exactly those given.
              inherit tvars tvarBindings;
            };
          voidArgs = bindingsToArgs voidBindings;
          isFullyBound = T: size (filterAttrs (_: T: typeEq Void T) T.tvars) == 0;
        in
          # No supertype; just create a new type with unbound args.
          # 'This' goes unused here. MyString.template is not meaningful vs Type.template.
          if bindingsToSuperOrSuper == null
            then
              { bind = bindings:
                  # TODO: constraints skipped here
                  let args = bindingsToArgs bindings;
                  in U.Type.new name args;
              }

          # Rigid supertype; just create a new subtemplate.
          else if isTypeSet bindingsToSuperOrSuper
            then
              { bind = bindings:
                  let args = bindingsToArgs bindings;
                      # TODO: constraints skipped here
                      Super = bindingsToSuperOrSuper;
                  in Super.subType name args;
              }

          else
            assert assertMsg (isFunction bindingsToSuperOrSuper) ''
              newTemplate_: bindingsToSuperOrSuper must be a Type or function returning a Type, got:
              ${log.print bindingsToSuperOrSuper}
            '';

            # We can't instantiate the type until we get bindings, since we inherit from
            # the bindings in the general case.
            # For now we just return a bind thunk that requires full-boundedness
            # before instantiating.
            # TODO: A Template type with static bind method
            { bind = bindings:
                let Super = bindingsToSuperOrSuper bindings;
                    # TODO: constraints skipped here
                    args = bindingsToArgs bindings;
                    T = Super.subType name args;
                  in T;
            };

      # Create a new template with no inheritance.
      newTemplate = This: name: tvars: bindingsToArgs:
        newTemplate_ This null name tvars bindingsToArgs;
    };

    # Universe of related types, enabling building the Type type and its constituent types in
    # terms of types in the superuniverse.
    #
    # If U_0 = Quasiverse then:
    #
    #   U_0:
    #     - Type checking disabled by options.
    #     - U_0.Type bootstrapped from U_0 constituents.
    #     - U_0 constituents built from U_0.Type and U_0 constituents.
    #     - Type checking unsupported in construction of U_0.Type.
    #     - Type-checking unsupported in construction of U_0 constituent types.
    #     - Type-checking unsupported in instantiation of U_0 constituent types.
    #     - U_0.Type behaves correctly to create new types.
    #       Created types are not type-checked.
    #       U_0.Type is not itself type-checked.
    #     - U_0 constituents behave according to minimally implemented shims, mimicking
    #       the behaviour of the U_1+ constituents but without any checks, type enforcement
    #       or casting.
    #
    #   U_1:
    #     - Type checking disabled by options.
    #     - U_1.Type built from U_0 constituents.
    #     - U_1 constituents built from U_1.Type and mixture of U_0 and U_1 constituent types,
    #       preferring U_1 but falling back to U_0 to break circularity.
    #     - Type-checking unsupported in construction of U_1.Type.
    #     - Type-checking unsupported in construction of U_1 constituent types that have U_0 dependencies.
    #     - Type-checking supported but disabled in construction of U_1 constituent types that have no U_0 dependencies.
    #     - Type-checking unsupported in instantiation of U_0 constituent types.
    #     - Type-checking unsupported in instantiation of U_1 constituent types.
    #     - U_1.Type behaves correctly to create new types.
    #       U_1.Type is not itself type-checked.
    #       Created types are not type-checked.
    #     - U_1 constituent types behave correctly to create new constituents
    #       U_1 constituent types are not themselves type-checked
    #       Created constituents are not type-checked.
    #
    #   U_2:
    #     - Type checking enabled by options.
    #     - U_2.Type built from U_1 constituents.
    #     - U_2 constituents built from U_2.Type and mixture of U_1 and U_2 constituent types,
    #       preferring U_2 but falling back to U_1 to break circularity.
    #     - Type-checking supported and performed in construction of U_2.Type.
    #     - Type-checking supported and performed in construction of U_2 constituent types.
    #     - Type-checking unsupported in instantiation of U_1 constituent types.
    #     - Type-checking supported and performed in instantiation of U_2 constituent types.
    #     - U_2.Type behaves correctly to create new types.
    #       U_2.Type is not itself type-checked.
    #       Created types are type-checked.
    #     - U_2 constituent types behave correctly to create new constituents
    #       U_2 constituent types are type-checked.
    #       Created constituents are not type-checked.
    #
    #   U_3:
    #     - Type checking enabled by options.
    #     - U_3.Type built from U_2 constituents.
    #     - U_3 constituents built from U_3.Type and mixture of U_2 and U_3 constituents,
    #       preferring U_3 but falling back to U_2 to break circularity.
    #     - Type-checking supported and performed in construction of U_3.Type.
    #     - Type-checking supported and performed in construction of U_3 constituents.
    #     - Type-checking supported and performed in instantiation of U_2 constituent types.
    #     - Type-checking supported and performed in instantiation of U_3 constituent types.
    #     - U_3.Type behaves correctly to create new types.
    #       U_3.Type is partially type-checked (its U_2 constituent instances are not).
    #       Created types are type-checked.
    #     - U_3 constituent types behave correctly to create new constituents
    #       U_3 constituent types are partially type-checked (their U_2 constituent instances are not)
    #       Created constituents are type-checked.
    #
    #   U_4:
    #     - Type checking enabled by options.
    #     - U_4.Type built from U_3 constituents.
    #     - U_4 constituents built from U_4.Type and mixture of U_3 and U_4 constituents,
    #       preferring U_4 but falling back to U_3 to break circularity.
    #     - Type-checking supported and performed in construction of U_4.Type.
    #     - Type-checking supported and performed in construction of U_4 constituents.
    #     - Type-checking supported and performed in instantiation of U_3 constituent types.
    #     - Type-checking supported and performed in instantiation of U_4 constituent types.
    #     - U_4.Type behaves correctly to create new types.
    #       U_4.Type is type-checked (including its U_3 constituent instances).
    #       Created types are type-checked.
    #     - U_4 constituent types behave correctly to create new constituents
    #       U_4 constituent types are type-checked (including their U_3 constituent instances)
    #       Created constituents are type-checked.
    #
    # Universe.U_0 == Quasiverse
    # Universe.U_1 == mkSubUniverse (Universe 0)
    # ...
    Universe =
      let
        # Generate a list of n universes, starting with Quasiverse.
        # The first universe is Quasiverse, the rest are generated from the previous one.
        genUniverses = maxLevel:
          let go = level: SU:
                if level > maxLevel then {}
                else let U = mkSubUniverse SU;
                    in { ${SU.opts.name} = SU; }
                        // go (level + 1) U;
          in go 0 Quasiverse;
      in
        genUniverses 4;

    #  We can then expose U_4 as a self-consistent base typesystem comprised of:
    #    - The U_4 constituent types, instances of U_4.Type
    #    - The U_4.Type, itself already grounded by setting U_4.Type.Type to U_4.Type
    #
    # This type system then has the following properties:
    #   - All types in TS have type TS.Type, including TS.Type
    #   - TS is fixed under subuniverse creation: deriving a subuniverse from TS gives
    #     exactly TS, modulo lambda equality
    # The final type system is then U_4 with any U_*, U, SU or opts references removed.
    TS = removeAttrs Universe.U_4 [
      "opts"
      "_U"
      "_SU"
    ];

    # Cause a Type to have itself as its own Type, eliding any information about
    # either bootstrap pseudo-types or the Type of its superuniverse.
    groundType = Type:
      let Type__grounded = Type // { Type = _: Type__grounded; };
      in Type__grounded;

    # Cause a Type to have itself as its own Type, eliding any information about
    # either bootstrap pseudo-types or the Type of its superuniverse.
    # Also asserts that recreating the grounded Type using itself, via Type.new,
    # creates an identical Type (modulo lambda equality).
    groundTypeAndAssertFixed = opts: Type__args: Type:
      with log.vtrace.call "groundTypeAndAssertFixed" { inherit Type; } ___;
      let Type__grounded = assign "Type__grounded" (groundType Type);
      in
        assert assertTypeFixedUnderNew Type__grounded opts.typeName Type__args;
        return Type__grounded;

    withCommonUniverse = opts: SU: U:
      mergeAttrsList [
        U
        {inherit opts;}
        (mkUniverseReferences opts U SU)
        (mkBuiltins SU)
        (mkTemplating SU)
        (mkTrivialTypes U.Type)
      ];
    withCommonUniverseSelf = opts: U: withCommonUniverse opts U U;

    mkUniverseReference = opts: tag: U:
      {
        __toString = _: "<${tag}-universe ref: ${opts.name}>";
        get = _: U;
      };

    mkUniverseReferences = opts: U: SU: {
      _U = mkUniverseReference opts "self" U;
      _SU = mkUniverseReference opts "super" SU;
    };

    printUniverse = U:
      with log.prints; put U using.raw (using.maxDepth 3) ___;

    assertFixedUnderF = fLabel: xLabel: f: x:
      with log.vtrace.call "assertFixedUnderF" { inherit fLabel xLabel f x; } ___;
      with cutils.tests.Compare;
      let fx = f x;
          x_NL = NoLambdas x;
          fx_NL = NoLambdas fx;
      in
        with intermediate "x" x;
        with intermediate "fx" fx;
        with intermediate "x_NL" x_NL;
        with intermediate "fx_NL" fx_NL;
        assertMsg (x_NL == fx_NL) (with log.prints; ''
          ${xLabel} is not fixed under ${fLabel}:

            ${xLabel} (original):
            ${here x using.raw ___}

            ${xLabel} (under ${fLabel}):
            ${here fx using.raw ___}

          Comparing lambda-free:
            ${xLabel} (lambda-free, original):
            ${here x_NL using.raw ___}

            ${xLabel} (lambda-free, under ${fLabel}):
            ${here fx_NL using.raw ___}
        '');

    assertTypeFixedUnderNew = T: typeName: typeArgs:
      assertFixedUnderF "new" "Type" (T: T.new typeName typeArgs) T;

    # Lazily cascading options that disable typechecking at levels U_0 and U_1.
    # Options for the next universe can be produced via opts.descend {}.
    UniverseOpts_0 =
      let mkOpts = level: enableTypeChecking: enableTypeCheckingInSubUniverse: {
            inherit level enableTypeChecking enableTypeCheckingInSubUniverse;
            name = "U_${toString level}";
            typeName = "Type";
            descend = _: mkOpts (level + 1) enableTypeCheckingInSubUniverse true;
          };
      in mkOpts 0 false false;

    # The barest minimum universe to bootstrap the type system.
    # Constructs a bootstrapped Quasitype from a hand-build GroundType instance, which has no
    # typed fields.
    Quasiverse =
      with log.vtrace.attrs "Quasiverse" ___;
      with msg "Constructing Quasiverse";

      let
        # Quasiverse is its own self- and super-universe, enabled by containing no circular dependencies
        # in quasitype construction.
        U = Quasiverse;
        SU = Quasiverse;

        # Take on the intial options for a root universe.
        opts = UniverseOpts_0;

      in withCommonUniverseSelf opts (rec {

        # Bootstrap the Type type.
        __Bootstrap = rec {
          # Create simple untyped arguments using the pseuedotypes below for containers,
          # fields, literals, etc.
          Type__args = assign "__Bootstrap.Type__args" (mkTypeArgsFor opts U);

          # Bootstrap a handmade type by treating typeArgs as though it was already created
          # via Type.new.
          Type__handmade = assign "__Bootstrap.Type__handmade" Type__args;

          # Convert the handmade type into a bona-fide type by instantiating it as though
          # 'Type__handmade.new "QuasiType" typeArgs' was called.
          # Must be newInstance rather than mkInstance here, as we need to push typeArgs through
          # PseudoType.ctor.
          Type__bootstrapped =
            assign "__Bootstrap.Type__bootstrapped"
              (newInstance Type__handmade opts.typeName Type__args);

          # Construct Type through an actual application of the .new constructor.
          # This is valid besides Type__new.(Type {}).(Type {}).Type {} == Type__args, and after that,
          # Type__args ? Type == false.
          # We fix this in the next stage.
          Type__new =
            assign "__Bootstrap.Type__new"
              (Type__bootstrapped.new opts.typeName Type__args);

          # Finally, ground this Type by setting Type.Type to return itself, eliding any information
          # about the bootstrap types.
          # This should now have reached a fixed point in terms of further bootstrapping by Type.new,
          # modulo lambda equality on created Type instances.
          # An assertion checks that Type is fixed under further bootstrapping.
          Type =
            assign "__Bootstrap.Type"
              (groundTypeAndAssertFixed opts Type__args Type__new);
        };

        # Expose only the final fixed Type.
        Type = assign "Type" __Bootstrap.Type;

        # Shim out all types used in the construction of Type s.t. Type can be created
        # with U == SU == Quasiverse.
        Field = {
          inherit Type;
          new = index: fieldName: fieldSpec: parseFieldSpec fieldSpec // {
            inherit index fieldName fieldSpec;
            # Disable typechecking in base case but retain static / default indicators.
            fieldType = null;
          };
        };

        Fields =
          let
            # Make a single indexed field from a {name = spec} entry.
            mkIndexed = index: nameSpec:
              assert size nameSpec == 1;
              let fieldName = head (attrNames nameSpec);
                  fieldSpec = head (attrValues nameSpec);
              in { ${fieldName} = U.Field.new index fieldName fieldSpec; };

            # Convert incoming set arguments to list form.
            toNameToSpecList = nameToSpec: {
              set = mapAttrsToList (k: v: {${k} = v;}) nameToSpec;
              list = nameToSpec;
            }.${typeOf nameToSpec};
          in {
            inherit Type;
            new = nameToSpec_:
              let nameToSpec = toNameToSpecList nameToSpec_;
              in rec {
                # Store the original arguments in [{...}] form for easy extension.
                __nameToSpec = nameToSpec;
                indexed = mergeAttrsList (imap0 mkIndexed __nameToSpec);
                update = newNameToSpec_:
                  let newNameToSpec = toNameToSpecList newNameToSpec_;
                  in Fields.new (__nameToSpec ++ newNameToSpec);
              };
          };

        SetOf = T: {new = U.Set.new;};
        ListOf = T: {new = U.List.new; tvars = { T = U.Type; }; tvarBindings = { inherit T; };};
        OrderedOf = T: {new = (U.ListOf (U.Sized 1 (U.SetOf T))).new;};
        Constraint = {new = x: {value = x; satisfiedBy = _: true;};};
        Static = T: { tvarBindings = { inherit T; }; };
        Default = T: V: { tvarBindings = { inherit T; V = { tvarBindings = { inherit V; }; }; }; };
        NullOr = T: { new = id; };
        Literal = V: {new = { value = V; }; tvarBindings = { inherit V; }; getLiteral = V; };
        Sized = _: T: {new = T.new;};
      });

    # Create a new universe descending from SU.
    mkSubUniverse = SU:
      let opts = SU.opts.descend {}; in
      with log.vtrace.call "mkSubUniverse" opts "<SU>" ___;
      with msg "Constructing ${opts.name} universe";
      let U = withCommonUniverse opts SU rec {

        # Construct the Type type in terms of the SU, set Type.Type to Type, and ensure it
        # is fixed under further bootstrapping.
        __Bootstrap = rec {
          Type__args = assign "__Bootstrap.Type__args" (mkTypeArgsFor opts SU);
          Type__new = assign "__Bootstrap.Type__new" (SU.Type.new opts.typeName Type__args);
          Type = assign "__Bootstrap.Type" (groundTypeAndAssertFixed opts Type__args Type__new);
        };
        Type = assign "Type" __Bootstrap.Type;

        # Wrap up some builtin constructors.
        # TODO: Builtin to a base type for all builtins.
        # TODO: Move to common
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
                }."${typeOf x}" or (throw "Invalid type for Builtin.From: ${getTypeName x}"));
            in T.mk { value = x; };  # mk not new here so new can use From
        };

        # A constraint on a type variable.
        Constraint = Type.new "Constraint" {
          fields = SU.Fields.new [
            { constraintType = Type; }
          ];
          methods = {
            # Whether a given type variable binding satisfies the constraint.
            # If the constraint is unbound, we treat as satisfied, but instantiating the unbound type
            # will throw an error.
            satisfiedBy = this: That:
              typeEq U.Void That
              || That.isInstance this.constraintType
              || this.constraintType.check That;
          };
        };

        # Subtype of List that enforces homogeneity of element types.
        ListOf_ = U.List.subTemplate "ListOf" {T = Type;} (_: {
          fields = U.List.fields;
          checkValue = that: all (x: hasType _.T x) that.value;
        });
        ListOf = T: U.ListOf_.bind { inherit T; };

        # Subtype of Set that enforces homogeneity of value types.
        SetOf_ = U.Set.subTemplate "SetOf" {T = Type;} (_: {
          fields = U.Set.fields;
          checkValue = that: all (x: hasType _.T x) that.attrValues;
        });
        SetOf = T: U.SetOf_.bind { inherit T; };

        # A type that enforces a size on the value.
        Sized_ = Type.subTemplateOf (_: _.T) "Sized" {N = U.Literal_; T = Type;} (_: {
          fields = _.T.fields;
          checkValue = that:
            (Super.checkValue or (const true)) that
            && that.size == _.N.literal;
        });
        Sized = n: T:
          let N = U.Literal n;
          in U.Sized_.bind { inherit N T; };

        # A type satisfied by any value of the given list of types.
        Union_ = Type.template "Union" {Ts = U.ListOf Type;} (_: {
          overrideCheck = that: any (T: T.check that) _.Ts.value;
        });
        Union = Ts: U.Union_.bind {inherit Ts;};

        # A value or T or Null.
        NullOr = T: U.Union ["null" U.Null T];

        # An attribute set with attributes zero-indexed by their definition order.
        # xs = Ordered.new [ {c = 1;} {b = 2;} {a = 3;} ];
        # xs.value == { a = 1; b = 2; c = 3; } (arbitrary order)
        # xs.names == [ "c" "b" "a" ] (in order of definition)
        # xs.values == [ 1 2 3 ] (in order of definition)
        OrderedItem = T: U.Sized 1 (U.SetOf T);
        OrderedOf_ = Type.subTemplateOf (_: U.ListOf (U.OrderedItem _.T)) "OrderedOf" {T = Type;} (_: {
          methods = {
            # The unordered merged attribute set
            unindexed = this: mergeAttrsList (this.imap (_: k: v: { ${k} = v; }));

            # The merged attribute set with an additional 'index' field indicating
            # its place in the order.
            indexed = this:
              mergeAttrsList
                (this.imap (i: k: v:
                  # TODO: 'Has' constraint
                  assert assertMsg v.has.index "OrderedItem: index field must be present";
                  {${k} = v.set.index i; }));

            # The ordered attribute names.
            attrNames = this: this.imap (_: k: _: k);

            # The ordered attribute values.
            attrValues = this: this.imap (_: _: v: v);

            # A set from name to index.
            indexes = this: mapAttrs (name: xs: xs.index) this.indexed;

            # Map over the underlying ListOf (Sized 1 (SetOf T)) with an index.
            # This -> (int -> string -> T -> a) -> [a]
            imap = this: f:
              imap0
                (index: item:
                  let kvs = mapAttrsToList (k: v: {inherit k v;}) item.value;
                  in
                    assert assertMsg (size kvs == 1) "OrderedItem: Sized 1 SetOf with size != 1 (${log.print kv})";
                    let kv = head kvs; in f index kv.k kv.v)
                  this.value;

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
              else this.append (U.OrderedItem.new {${name} = value;});

            # Get the value with the given key via this.getAt.name
            getAt = this: this.indexed;

            # Get the value with the given key and index set via this.getIndexedAt.name
            getUnindexedAt = this: this.unindexed;

            # Update by inserting the given items sequentially at the end of the order.
            # If any already appear, they update the item in its original order.
            update = this: items:
              foldl' (this: item:
                let
                  name = head (attrNames xs);
                in
                  assert item.size == 1;
                  this.setAtName (head (item.attrNames)) (head (item.attrValues)))
                this
                items;
          };

          checkValue = that:
            Super.checkValue that
            && assertMsg
                (size that.attrNames == size that.unindexed)
                "Duplicate keys in OrderedOf: ${joinSep ", " that.attrNames}";
        });
        OrderedOf = T: U.OrderedOf_.bind { inherit T; };

        # Base type for enums.
        Enum = Type.new "Enum" {};

        # Create an enum type from a list of item names.
        # MyEnum = mkEnum "MyEnum" [ "Item1" "Item2" "Item3" ]
        # MyEnum.names == [ "Item1" "Item2" "Item3" ]
        # MyEnum.fromName "Item1" == 0
        # MyEnum.fromIndex 0 == "Item1"
        mkEnum = enumName: itemNames:
          let Item = U.Enum.subType enumName {
                fields = U.Fields.new [
                  {i = Int;}
                  {name = String;}
                ];
                staticMethods = {
                  # Enum members live on the Enum type itself.
                  __items = This: zipListsWith Item.new (range 0 (length itemNames - 1)) names;
                  __indexToItem = This: keyByF (item: toString item.i) __items;
                  __nameToItem = This: keyByName __items;
                  fromIndex = This: i: This.__indexToItem.${toString i} or throw "Invalid index in enum ${enumName}: ${toString i}";
                  fromName = This: name: This.__nameToItem.${name} or throw "Invalid name in enum ${enumName}: ${name}";
                };
              };
          in Item;

        # A type inhabited by only one value.
        Literal_ = Type.template "Literal" {V = Type;} (_: rec {
          ctor = Ctors.Nullary;
          staticMethods = {
            getLiteral = This: _.V;
          };
        });
        Literal = V: U.Literal_.bind { inherit V; };
        literal = v: (U.Literal v).new;

        # A type inhabited by literals of any of the given list of values
        Literals = Vs: U.Union (map Literal values);

        # A type indicating a default value.
        Default_ = Type.template "Default" {T = Type; V = Type;} (_: {
          staticMethods.defaultType = This: This.tvarBindings.T;
          staticMethods.defaultValue = This: This.tvarBindings.V.tvarBindings.V;
        });
        Default = T: v:
          let V = SU.Literal v;
          in U.Default_.bind { inherit T V; };

        # Newtype wrapper
        Static_ = Type.template "Static" {T = Type;} (_: {
          staticMethods.staticType = _.T;
        });
        Static = T: U.Static_.bind {inherit T;};

        # TODO: Type Family candidate
        # Untag T to its root field value type.
        # e.g. Untagged Static<Default<Int, 123>> -> Int
        #      Untagged Static<Int> -> Int
        #      Untagged Default<Int, 123> -> Int
        #      Untagged Int -> Int
        Untagged = Spec: (parseFieldSpec Spec).fieldType;

        # Either:
        # (Field.new index "myField" Int
        # (Field.new index "myField" (Static Int)) -> Field<Static<Int>>.fieldType == Int
        # (Field.new index "myField" (Default Int 123)) -> Field<Default<Int, 123>>.fieldType == Int
        # (Field.new index "myField" (Static (Default Int 123))) -> Field<Static<Default<Int, 123>>>.fieldType == Int
        Field = Type.new "Field" {
          fields = SU.Fields.new [
            {index = "int";}
            {fieldName = U.String;}
            {fieldSpec = U.NullOr Type;}
          ];
          methods = {
            parsedT = this: parseFieldSpec this.fieldSpec;
            fieldType = this: this.parsedT.fieldType;
            fieldStatic = this: this.parsedT.fieldStatic;
            fieldDefault = this: this.parsedT.fieldDefault;
          };
        };

        # Fields is an OrderedOf that first converts any RHS values into Field types.
        Fields = (U.OrderedOf U.Field).subType "Fields" {
          # Fields.new { field = FieldType; ... }
          # Fields.new { field = Default FieldType defaultValue; ... }
          # Fields.new { field = Static FieldType; ... }
          # Fields.new [ { field = FieldType; ... } ... ]
          # Fields.new [ { field = Default FieldType defaultValue; ... } ... ]
          # Fields.new [ { field = Static FieldType; ... } ... ]
          ctor = This: fieldListOrSet:
            let
              mkFieldItem = fieldName: T:
                # Produce a valid (Sized 1 (SetOf Field))
                # ctor must return args for the supertype's mk; in this case,
                # ultimately a ListOf (...), which constructs with {value = list}.
                (U.OrderedItem U.Field).new {${fieldName} = U.Field.new 0 fieldName T;};

              mkFieldList = fieldListOrSet: {
                # Raw set converted to raw list of field singletons.
                # Arbitrarily ordered.
                set =
                  mkFieldList
                    (mapAttrsToList (k: v: { ${k} = v; }) fieldListOrSet);

                # Raw list of field singletons converted to Ordered
                # [ { fieldName: fieldType; } ... ] assignments
                # -> [ Sized 1 (SetOf Field) { fieldName = Field.new 0 fieldName fieldType; } ...]
                list =
                  map
                    (set1Field:
                      if size set1Field != 1 then throw "Non-singleton field in Fields list: ${log.print set1Field}"
                      else head (mapAttrsToList mkFieldItem set1Field))
                    fieldListOrSet;
              }.${typeOf fieldListOrSet}
                or (throw (indent.block ''
                      Invalid Fields.new argument (${typeOf fieldListOrSet}):
                      ${indent.here (log.print fieldListOrSet)}
                    ''));
            in {
              value = mkFieldList fieldListOrSet;
            };
        };
      };
      in U;
  };

  # nix eval --impure --expr '(import ./cutils/types.nix {})._tests'
  _tests =
    with Types;
    with cutils.tests;
    let
      testInUniverse = test: U: test U;
      testInUniverses = Us: test: mapAttrs (_: testInUniverse test) Us;
      allUniverses = Universe;
      untypedUniverses = {inherit (allUniverses) Quasiverse QuasiType HyperType;};
      typedUniverses = {inherit (allUniverses) MetaType ProtoType Type;};
      precursorUniverses = {inherit (allUniverses) Quasiverse QuasiType HyperType MetaType ProtoType;};
      finalUniverses = {inherit (allUniverses) Type;};

      precursorUniverseTests = testInUniverses precursorUniverses (U: with U; {
        instantiate = {
          Literal = expect.equal (Literal 123).getLiteral 123;
        };

        field = {
          untyped = {
            expr = let f = Field.new 0 "name" null;
                    in [f.fieldName f.fieldType f.fieldStatic f.fieldDefault f.index];
            expected = ["name" null false null 0];
          };
        };
      });

      typedUniverseTests = testInUniverses typedUniverses (U: with U;
        let
          A = Type.new "A" {
            fields = Fields.new [
              { a = "int"; }
              { b = Int; }
              { c = Default Int 5; }
              { d = Default Int (Int.new 10); }
            ];
          };
        in {
          Int = {
            expr = Int.new 123;
            expected = 123;
          };
          new = {
            expr = (A.new 1 2 3 4).get;
            expected = {a = 1; b = 2; c = 3; d = 4;};
          };
          mk = {
            expr = (A.mk {a = 1; b = 2;}).get;
            expected = {a = 1; b = 2; c = 5; d = 10;};
          };
          wrongType = expect.error (A.new "no" 2 3 4).get;
        });

      untypedUniverseTests = testInUniverses typedUniverses (U: {
      });

      finalUniverseTests = testInUniverses finalUniverses (U: {
      });

      allUniverseTests = testInUniverses allUniverses (U: with U;
        let
          mkBuiltinTest = T: name: rawValue: {
            expr =
              let x = T.new rawValue;
              in {
                name = (x.Type {}).name;
                value = if name == "Lambda" then null else x.value;
                newIsBuiltin = builtinValueCheck x;
                rawIsBuiltin = builtinValueCheck x.value;
              };
            expected = {
              inherit name;
              value = if name == "Lambda" then null else rawValue;
              newIsBuiltin = false;
              rawIsBuiltin = true;
            };
          };

          MyType = Type.new "MyType" {
            fields = Fields.new [{ myField = String; }];
            methods = {
              helloMyField = this: extra:
                "Hello, ${this.myField.value}${extra}";
            };
          };

          MyType2 = Type.new "MyType2" {
            fields = Fields.new [
              { stringField = String; }
              { intField = Int; }
              { defaultIntField = Default Int 666; }
              { staticIntField = Static Int; }
              { staticDefaultIntField = Static (Default Int 666); }
            ];
          };

        in {
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

          builtinValueCheck = {
            Type = {
              expr = builtinValueCheck Type;
              expected = false;
            };
            int = {
              expr = builtinValueCheck 123;
              expected = true;
            };
            set = {
              expr = builtinValueCheck {abc="xyz";};
              expected = true;
            };
            Bool = {
              expr = builtinValueCheck (Bool.new true);
              expected = false;
            };
          };

          MyString = let MyString = Type.new "MyString" {
                           fields = Fields.new [{ value = String; }];
                         };
                         WrapString = String.subType "WrapString" {};
                     in {
            MyString = {
              mkFromString = expect.eq (MyString.mk { value = String.new "hello"; }).value.value "hello";
              mkFromstring = expect.eq (MyString.mk { value = "hello"; }).value.value "hello";
              newFromString = expect.eq (MyString.new (String.new "hello")).value.value "hello";
              newFromstring = expect.eq (MyString.new "hello").value.value "hello";
              eqString = expect.eqOn Compare.Fields (MyString.new "hello").value (String.new "hello");
            };

            WrapString = {
              mkFromString = expect.eq (WrapString.mk { value = String.new "hello"; }).value "hello";
              mkFromstring = expect.eq (WrapString.mk { value = "hello"; }).value "hello";
              newFromString = expect.eq (WrapString.new (String.new "hello")).value "hello";
              newFromstring = expect.eq (WrapString.new "hello").value "hello";
              eqString = expect.eqOn Compare.Fields (WrapString.new "hello") (String.new "hello");
            };
          };

          MyType_mk = {
            expr = (MyType.mk { myField = "World"; }).myField.value;
            expected = "World";
          };

          MyType_new = {
            expr = (MyType.new "World").myField.value;
            expected = "World";
          };

          MyType_call = {
            expr =
              let this = MyType.new "World";
                in this.helloMyField "!";
            expected = "Hello, World!";
          };

          MyType_set = {
            expr =
              let this = MyType.new "";
              in [
                (this.helloMyField "!")
                ((this.set.myField "World").helloMyField "!")
              ];
            expected = [ "Hello, !" "Hello, World!" ];
          };

          MyType2_mk_overrideDefault = {
            expr =
              let this = MyType2.mk {
                    stringField = "hi";
                    intField = 123;
                    defaultIntField = 7;
                  };
              in [this.stringField.value this.intField.value this.defaultIntField.value];
            expected = ["hi" 123 7];
          };

          MyType2_mk_missingRequired = expect.error (
            let this = MyType2.mk {
                  intField = 123;
                  defaultIntField = 7;
                };
              in builtins.tryEval this
          );

          MyType2_mk_missingDefault = {
            expr =
              let this = MyType2.mk {
                    intField = 123;
                    stringField = "hi";
                  };
              in this.defaultIntField.value;
            expected = 666;
          };

          MyType2_mk_wrongType = expect.error (
              let this = MyType2.mk {
                    intField = 123;
                    stringField = true;
                    defaultIntField = 7;
                  };
              in builtins.tryEval this
          );

          cast =
            let
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
              toTypedBuiltin = mapAttrs (name: v: mkToTypedBuiltinTest U.${name} v) testX;
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
        });

    in cutils.tests.suite {
      types = {
        inherit
          precursorUniverseTests
          untypedUniverseTests
          typedUniverseTests
          allUniverseTests
          finalUniverseTests;
      };
    };


}

{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib,
  cutils ? import ./. { inherit lib; }, ... }:

with lib;
with lib.strings;
with cutils.collections;
with cutils.clib;
with cutils.attrs;
with cutils.dispatch;
with cutils.functions;
with cutils.errors;
with cutils.lists;
with cutils.strings;

# TODO:
# - Universe via merge
#   - Build global type repository
#   - Meyhods loook up in there
#   - Laziness lets us recurse
#
# - TypeClasses using mkMerge
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
#   fields = This: {
#     name = String;
#     nickname = NullOr String;
#     age = Int;
#   };
#   methods = {
#     hello = this: msg: "Hello, ${this.show}! ${msg}";
#     show = this: verbose: joinOptionalsSep ", "
#       [this.name]
#       ++ optionals verbose [
#         (optionalString this.has.nickname) "AKA ${this.nickname}")
#         "${toString this.age}y old";
#     ];
#   }
# };
#
# OK:   alice = Person.new {name = "Alice"; age = 22;};
# OK:   bob = Person.new {name = "Robert"; nickname = "Bob"; age = 40;};
# FAIL: ethan = Person.new {nickname = "Bob"; age = 40;} -> Throws error for missing name field
# FAIL: jake = Person.new {name = "Jake"; age = "unknown";} -> Throws error for invalid age type
#
# alice.name = "Alice";
# bob.nickname = "Bob";
# alice.nickname = null;
# alice.hello "Binding works as expected" = "Hello, Alice, 22y old! Binding works as expected"
# bob.show = "Robert (Bob), 40y old! Cool."

let
  log = cutils.log;
in rec {

  Types = rec {
    UnsafeTypeThunk = name: T:
      (NamedThunk "UnsafeTypeThunk[${name}]" T) // {
        __isTypeThunk = true;
      };
    TypeThunk = T:
      assert assertMsg (isNull T || isTypeLike T) (indent.block ''
        TypeThunk must be null or a Type:
          ${indent.here (log.print T)}
      '');
      UnsafeTypeThunk (getTypeName T) T;
    isTypeThunk = x: x.__isTypeThunk or false;

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
    BuiltinValueCheck = x: x ? Type && BuiltinNameCheck (x.Type.do (T: T.name));

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
      if isCastError xOrError
      then xOrError
      else f xOrError;

    cast_ = T: x:
      let
        result = cast T x;
      in
        assert assertMsg (isCastSuccess result) (indent.block ''
          Unsafe cast_ failed:
            ${indent.here result.castError}
        '');
        result.castSuccess;

    cast = T: x:
      with log.vtrace.call "cast" T x ___;

      if T == null then
        return (mkCastError "Cannot cast to null: T = ${log.print T}, x = ${log.print x}")

      else if !(isTypeLike T) then
        return (mkCastError (indent.block ''
          Invalid target type provided for cast:

            T = ${indent.here (log.print T)}

            x = ${indent.here (log.print x)}
        ''))

      else with lets rec {

        printT = T: log.print T;

        TName = printT T;
        xTName = getTypeBoundName x;

        xFields = (
          if isTyped x then x.Type.do (T: (T.fields T).instanceFields) else
          mkCastError ''
            Cannot get fields from untyped uncast value: T = ${log.print T}, x = ${log.print x}
          '');
        TFields = (
          if isTypeSet T then (T.fields T).instanceFields else
          mkCastError ''
            Cannot get fields from non-Type target type: T = ${log.print T}, x = ${log.print x}
          '');

        xIsUnary = castErrorOr xFields (fields: size fields == 1) == true;
        TIsUnary = castErrorOr TFields (fields: size fields == 1) == true;

        xUnaryField = castErrorOr xFields maybeHead;
        TUnaryField = castErrorOr TFields maybeHead;

        xUnaryFieldName = castErrorOr xUnaryField (field:
          if field == null then mkCastError ''Unary field is not a solo: ${log.print field}''
          else soloName field);
        TUnaryFieldName = castErrorOr TUnaryField (field:
          if field == null then mkCastError ''Unary field is not a solo: ${log.print field}''
          else soloName field);

        xUnaryFieldT = castErrorOr xUnaryField (field:
          if field == null then mkCastError ''Unary field is not a solo: ${log.print field}''
          else (soloValue field).fieldType or null);
        TUnaryFieldT = castErrorOr TUnaryField (field:
          if field == null then mkCastError ''Unary field is not a solo: ${log.print field}''
          else (soloValue field).fieldType or null);

        xUnaryFieldTName = castErrorOr xUnaryFieldT printT;
        TUnaryFieldTName = castErrorOr TUnaryFieldT printT;

        xFieldNames = castErrorOr xFields (map soloName);
        TFieldNames = castErrorOr TFields (map soloName);

        castStr = "${xTName} -> ${TName}";

        # A list of casts to attempt in order.
        # The first cast satisfying 'when == true && isCastSuccess result' will be returned.
        # If no cast satisfies, then a cast error set is returned with the collated errors.
        casts =
          let mkCast = cast: if cast.when then cast else cast // {
                result = mkCastError "'when' condition not satisfied";
              };
          in map mkCast [
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
              successMsg = _: ''
                Identity cast succeeded: ${castStr}
              '';
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
              orMsg = indent.block ''
                Cannot downcast from an instance of non-unary type:
                  ${xTName}
              '';
              result = castErrorOr xUnaryFieldName (fieldName: cast T x.${fieldName});
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
                else
                  mkCastSuccess
                    (T.mk { ${TUnaryFieldName} = xCast.castSuccess; })
                    xCast.castSuccessMsg;
              failMsg = castErrorMsg: indent.block ''
                Upcast failed to unary field ${TName}.${TUnaryFieldName}:
                  ${xTName} -> ${TUnaryFieldTName}

                Upcast error:
                  ${indent.here castErrorMsg}
              '';
              successMsg = castSuccessMsg: indent.block ''
                Upcast succeeded to unary field ${TName}.${TUnaryFieldName}:
                  ${xTName} -> ${TUnaryFieldTName})

                Upcast success:
                  ${indent.here castSuccessMsg}
              '';
            }

            # Sidecasting types via a nested cast
            # Careful not to use other attrs of 'fields' here besides 'indexed' to
            # be bootstrap-compatible.
            {
              name = "Sidecast Fields";
              when = !(isCastError xFieldNames) && !(isCastError TFieldNames)
                    && (length xFieldNames == length TFieldNames);
              orMsg = indent.block ''
                Cannot sidecast unless from a typed instance to a Type with the same field count:
                  ${xTName} -> ${TName}

                Source fields:
                  ${indent.here (log.print xFieldNames)}

                Target fields:
                  ${indent.here (log.print TFieldNames)}
                '';
              result =
                let castTArgs =
                      zipListsWith
                        (xFieldName: TFieldName: {
                          ${TFieldName} =
                            cast
                              (T.fields.getField TFieldName).fieldType
                              x.${xFieldName};
                        })
                        xFieldNames
                        TFieldNames;
                    castErrors = filterAttrs (_: isCastError) castTArgs;
                    castArgs =
                      mapAttrs (_: castResult: castResult.castSuccess) castTArgs;
                    castErrorMsgs =
                      indent.blocks
                        (mapAttrsToList
                          (name: castResult: "${name}: ${castResult.castError}")
                          castTArgs);
                    castSuccessMsgs =
                      indent.blocks
                        (mapAttrsToLIst
                          (name: castResult: "${name}: ${castResult.castSuccessMsg}")
                          castTArgs);
                in if size castErrors > 0
                  then mkCastError castErrorMsgs
                  else mkCastSuccess (T.mk castArgs) castSuccessMsgs;
              failMsg = castErrorMsg: indent.block ''
                Sidecast failed:
                  ${xTName} -> ${TName}

                Field cast errors:
                  ${indent.here castErrorMsg}
              '';
              successMsg = castSuccessMsg: indent.block ''
                Sidecast succeeded:
                  ${xTName} -> ${TName}

                Field casts successes:
                  ${indent.here castSuccessMsg}
              '';
            }
          ];
      };

      let
        getOrMsg = castResult:
          assert !castResult.when;
          indent.block ''
            ${castResult.name}:
              ${indent.here castResult.orMsg}
          '';

        getFailMsg = castResult:
          assert isCastError castResult.result;
          indent.block ''
            ${castResult.name}:
              ${indent.here (castResult.failMsg castResult.result.castError)}
          '';

        getSuccessMsg = castResult:
          assert isCastSuccess castResult.result;
          indent.block ''
            ${castResult.name}:
              ${indent.here (castResult.successMsg castResult.result.castSuccessMsg)}
          '';

        tryCasts = msgs: casts:
          let
            castResult = head casts;
            casts' = tail casts;
          in
            # If we exhausted all casts, terminate with a combined castError
            if casts == []
              then (
                mkCastError (indent.block ''
                  Cast failed: ${xTName} -> ${TName}

                  ${xTName} instance:
                    ${indent.here (log.print x)}

                  Attempted casts:
                    ${indent.here (indent.blocks msgs)}

                  Log State:
                    ${indent.here (log.print __logState)}
                ''))

            # Skip non-matching casts with a note message
            else if !castResult.when
              then let msgs' = msgs ++ [(getOrMsg castResult)]; in tryCasts msgs' casts'

            # Record nested cast errors
            else if isCastError castResult.result
              then let msgs' = msgs ++ [(getFailMsg castResult)]; in tryCasts msgs' casts'

            # Cast succeeded
            else if isCastSuccess castResult.result
              then
                let msgs' = msgs ++ [(getSuccessMsg castResult)];
                in
                  mkCastSuccess castResult.result.castSuccess (indent.block ''
                    Cast succeeded: ${xTName} -> ${TName}

                    ${xTName} instance:
                      ${indent.here (log.print x)}

                    Attempted casts:
                      ${indent.here (joinLines msgs')}
                  '')
            else
              throw (indent.block ''
                Cast result is neither castSuccess nor castError (malformed 'casts = [ ... ]' entry?):
                  ${indent.here (log.print castResult)}
              '');


        in return (tryCasts [] casts);

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
    mergeSuper = f: foldUpward (acc: This: let xs = def (f This) {}; in acc // xs) {};

    setFieldTypeToNull = opts: SU: spec:
      if !opts.retainTypeFieldSpec then null
      else
        if spec ? defaultType then
          SU.Default (setFieldTypeToNull opts SU spec.defaultType) (spec.defaultValue)
        else if spec ? staticType then
          SU.Static (setFieldTypeToNull opts SU spec.staticType)
        else
          null;

    maybeNulled = opts: SU: T:
      if opts.enableTypeChecking then T
      else setFieldTypeToNull opts SU T;

    # Construct the common fields for a universe using the types of the universe above.
    withCommonFieldSpecs = opts: SU: fieldSpecs:
      # concatSolos to ensure we don't duplicate Type
      concatSolos
        # The type of the instance as a thunk.
        # Set in mkInstance args_->args to the This TypeThunk.
        [{Type = maybeNulled opts SU "set";}]
        (solos fieldSpecs);

    # Construct the fields for a universe using the types of the universe above.
    # The 'Type' field is added in Fields.new in both shim and real implementations.
    mkTypeFieldListFor = opts: _: SU: [
      # Indicates that this is a type. Should never be set manually.
      {__isTypeSet = maybeNulled opts SU (SU.Default "bool" true);}
      # The supertype of the type.
      {Super = maybeNulled opts SU (SU.Default "set" (TypeThunk null));}
      # The name of the type.
      {name = maybeNulled opts SU (SU.String);}
      # The type parameters of the type.
      {tvars = maybeNulled opts SU (SU.Default "set" {});}
      # The type parameter bindings of the type.
      {tvarBindings = maybeNulled opts SU (SU.Default "set" {});}
      # The constructor function creating the fields of the type as a set to pass to mk.
      {ctor = maybeNulled opts SU (SU.Default SU.Ctor SU.Ctors.CtorFields);}
      # A set of ordered fields to make available as this.___ and this.has.___, this.set.___, etc
      {fields = maybeNulled opts SU (SU.Default "lambda" (This: SU.Fields.new []));}
      # A set of methods from this to make available as this.___
      {methods = maybeNulled opts SU (SU.Default "set" {});}
      # A set of methods from This to make available as This.___ and this.___
      {staticMethods = maybeNulled opts SU (SU.Default "set" {});}
      # Perform additional checks on the value of the type when comparing.
      {checkValue = maybeNulled opts SU (SU.Default (SU.NullOr "lambda") null);}
      # If set, ignore all other checks and use this check function only.
      {overrideCheck = maybeNulled opts SU (SU.Default (SU.NullOr "lambda") null);}
    ];

    mkTypeFieldsFor = opts: U: SU:
      let fieldList = mkTypeFieldListFor opts U SU;
      in SU.Fields.new fieldList;

    # Map a function over the supplied type levels, producing a set keyed
    # by Type name. Can be used to construct the same type in multiple different
    # Type universes.
    withTypeLevels = Levels: f:
      mergeAttrsList (map (Level: { ${Level.__TypeId} = f Level; }) Levels);

    typeMethodsFor = U: SU: {
      # Is this instance an instance of type That.
      isInstance = This: that: isTyped that && that.Type.do (That: That.eq This);

      # A = Type.new "A" {};
      # B = Type.new "B" { Super = A; };
      #
      # isSuperTypeOf A B == true
      # isSuperTypeOf B A == false
      # isSuperTypeOf A A == false
      # isSuperTypeOf Type A == true
      # isSuperTypeOf Type B == true
      isSuperTypeOf = This: That:
        if That.Super.do isNull then false
        else That.Super.do (typeEq This)
             || That.Super.do (ThatSuper: This.isSuperTypeOf ThatSuper);

      isSubTypeOf = This: That: That.isSuperTypeOf This;


      # Get the full templated name of the type.
      boundName = This:
        with log.vtrace.call "boundName" "unsafe:This" ___;
        return (
          if This.tvars == {} then This.name
          else
            let
              printBinding = tvarName:
                let C = This.tvars.${tvarName} or (
                      throw "No type variable ${tvarName} on ${This.name}");
                    T = This.tvarBindings.${tvarName} or Void;
                in
                  # Unbound
                  if typeEq U.Void T
                    then tvarName

                  # Bound to a type
                  else if isTypeSet T
                    then T.boundName

                  # Bound to a literal or builtin
                  else
                    with log.prints; put T _line ___;
              printBindings = joinSep ", " (map printBinding (attrNames This.tvars));
            in "${This.name}<${printBindings}>"
        );

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

      # Is That the same type as This?
      eq = This: That: typeEq This That;

      # Does this type inherit from That?
      inheritsFrom = This: That:
        isTypeSet That
        && That.eq This
        || (!(This.Super.do isNull)
            && This.Super.do (Super: Super.inheritsFrom That));

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
      subType = U.newSubType;

      # For a given This type, create a new template whose eventual bound type subtypes This.
      subTemplate = This: U.newTemplate_ This This;

      # Create a new template of a type accepting type parameters.
      # The parameters can be accessed via the _ argument surrounding the type specification.
      # Returns a function from {bindings} to a new bound type.
      template = U.newTemplate;

      # Create a new template inheriting from a function of This plus any type variables.
      # TODO: Generic inheritance and tvar/tvarBinding merging.
      subTemplateOf =
        This: bindingsToSuper: name: tvars_: bindingsToArgs_:
        U.newTemplate_ This bindingsToSuper name tvars_ bindingsToArgs_;

      __show = This: this:
        if isTypeSet this && isString this.__TypeId then
          this.__TypeId
        else
          this;
    };

    mkTypeArgsFor = opts: U: SU: {
      __isTypeSet = true;
      name = opts.typeName;
      Super = TypeThunk null;
      ctor = SU.Ctors.CtorType;
      fields = This: mkTypeFieldsFor opts U SU;
      methods = typeMethodsFor U SU;
      staticMethods = {};
      tvars = {};
      tvarBindings = {};
      checkValue = null;
      overrideCheck = null;
    };

    # Check if a given argument is a custom Type.
    checkTyped = x:
      assert assertMsg (isAttrs x) (indent.block ''
        checkTyped: x is not an attrset.
          x = ${indent.here (log.print x)}
        '');
      assert assertMsg (x ? Type) (indent.block ''
        checkTyped: x has no Type field.
          x = ${indent.here (log.print x)}
        '');
      assert assertMsg (isTypeThunk x.Type) (indent.block ''
        checkTyped: x.Type is not a TypeThunk
          x = ${indent.here (log.print x)}
        '');
      x;
    isTyped = x: tryBool (checkTyped x);

    # Check if a given argument is a Type.
    # 'isType' collides with lib.isType.
    isTypeSet = T: T.__isTypeSet or false;

    # Check if a given argument is a Type or a builtin type.
    isTypeLike = T:
      isTypeSet T || builtinNameCheck T;

    # Gets the type of a value, whether it is a Type or a builtin type.
    getRawType = x:
      if isTyped x then resolve x.Type
      else typeOf x;

    # Gets the type of a value, whether it is a Type or a builtin type.
    getType = x:
      if isTyped x then resolve x.Type
      else Builtin.FromT (typeOf x);

    # Get the name of a type whether builtin or Type.
    getTypeName = x:
      if isTyped x
        then x.Type.do (T: T.name or (throw ''
          Type is missing name in getTypeName:
          ${indent.here (log.print (resolve x.Type))}
        ''))
        else typeOf x;

    # Get the bound name of a type whether builtin or Type.
    getTypeBoundName = x:
      if isTyped x
      then x.Type.do (T: T.boundName or (throw ''
          Type is missing boundName in getTypeBoundName:
          ${indent.here (log.print (resolve x.Type))}
        ''))
        else typeOf x;

    # Check two types for equality, supporting both Types and builtin type-name strings.
    typeEq = T: U:
      # Unpack type thunks
      if isTypeThunk T then typeEq (resolve T) U
      else if isTypeThunk U then typeEq T (resolve U)
      # Otherwise compare like with like
      else if isTypeSet T && !isTypeSet U then false
      else if !isTypeSet T && isTypeSet U then false
      else if isTypeSet T && isTypeSet U then T.__TypeId == U.__TypeId
      else if !(builtinNameCheck T) then false # throw "typeEq ${log.print T} ${log.print U}: ${log.print T} is not a Type or builtin type"
      else if !(builtinNameCheck U) then false # throw "typeEq ${log.print T} ${log.print U}: ${log.print U} is not a Type or builtin type"
      else T == U;

    # Check a string or custom type against a value.
    hasType = T: x: typeEq T (getRawType x);

    # Gets the final value that should be set on the given field after casting and typechecks.
    # Returns only the value, not the full { name = value }
    #
    # Handles:
    #
    # - Casting the assigned value to the field type if necessary and possible
    #
    # - Typechecking the assigned value
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
    mkFieldAssignmentValue = This: fieldName: uncastValue:
      with log.vtrace.call "mkFieldAssignmentValue" This fieldName uncastValue ___;

      with lets rec {
        fields = This.fields This;
        field = fields.getField fieldName;
        castRequired =
          # Field has a type
          field.fieldType != null
          && (
            # The value provided does not already have this type
            !(hasType field.fieldType uncastValue)
            # nor does it pass a check on the field type
            && !(isTypeSet field.fieldType && field.fieldType.check uncastValue)
          );
        value =
          if castRequired
            then cast field.fieldType uncastValue
            else mkCastSuccess uncastValue "id";
      };

      assert (predChecks [
        { pred = (field: field != null);
          msg = joinLines [
            "Setting unknown field: ${This.name}.${fieldName}"
            "Known fields: ${joinSep ", " (map soloName fields.instanceFields)}"
          ];
        }
        { pred = field: (!castRequired) || isCastSuccess value;
          msg = indent.block ''
            Error casting field assignment:

              ${This.__TypeId or "This"}.${fieldName} =
                ${indent.here (log.print uncastValue)}

              ${indent.here value.castError}
          '';
        }
        # TODO: This check could be O(n) for container types, updating attrsets goes O(1) to O(n)
        # Need item-wise checks
        { pred = field: (!castRequired)
                        || (isTypeSet field.fieldType && field.fieldType.check value.castSuccess)
                        || (typeOf value.castSuccess == field.fieldType);
          msg = indent.block ''
            Cast value did not pass typecheck:
              ${This.__TypeId}.${fieldName} = ${log.print uncastValue}
              Cast value of ${log.print (value.castSuccess or null)} is not a valid instance of ${log.print field.fieldType}.
          '';
        }
      ] field
    );

    return value.castSuccess;

    # Accessors to include on all instances.
    # Since modifying here requires a new instance with updated accessors, and we don't
    # want to have to call as e.g. (this.fn this arg1 arg2) we need to set the accessors
    # again here in 'set' bound to the new instance.
    mkAccessors = This: this: this_:
      let
        fields = This.fields This;
        accessors = rec {
          # Field checking interface.
          # e.g. this.has.someInt -> true
          #      this.has ? someInt -> true
          #      this.has.notAField -> throws error
          #      this.has ? notAField -> false
          #      this.has.notAField or default -> default
          has = mergeAttrsList (mapSolos (_: _: true) fields.instanceFields);

          # Field getting interface
          # e.g. this.get.someInt -> 123
          get = mergeAttrsList (mapSolos (fieldName: _: this.${fieldName}) fields.instanceFields);

          # Field setting interface
          # e.g. this.set.someInt 123 -> this'
          #      this.set.someInt "123" -> throws Type error
          set =
            mergeAttrsList
              (mapSolos
                (fieldName: _:
                  # Reinit here inside the thunk to avoid constant reinit while setting by name
                  # during construction.
                  x: let assignment = { ${fieldName} = mkFieldAssignmentValue This fieldName x; };
                         this__ = this_ // assignment;
                     in bindThis This this__ this__)
                fields.instanceFields);

          # Field modification interface
          # e.g. this.modify.someInt (x: x+1) -> this'
          #      this.modify.someInt toString -> throws Type error
          modify =
            mergeAttrsList
              (mapSolos
                (fieldName: _:
                  f: set.${fieldName} (f this.${fieldName}))
                fields.instanceFields);
        };
    in
      accessors;

    # Make all field assignment solos on the instance
    # This is the only mechanism through which an instance should have a field updated or
    # modified.
    # TODO:
    # - Enforce this via having fields be read-only via thunk such that updates are meaningless
    # - Fields-as-thunks would also mean we can lazily compose field updates and only force them
    #   as needed (could also only type-check on access enabling invalid intermediate field
    #   values that can never be seen)
    mkFieldAssignments = This: args:
      let
        fields = This.fields This;
      in
        concatMapSolos
          (fieldName: field:
            with log.vtrace.call "mkFieldAssignments.mapSolos" This args fieldName field ___;
            with lets rec {
              hasValidValue = (args ? ${fieldName}) || field.hasDefault;
              argsValue = args.${fieldName} or (_: throw "No args.${fieldName} provided");
              value = args.${fieldName} or (
                if isThunk field.fieldDefault then resolve field.fieldDefault
                else _: throw "Invalid non-Thunk fieldDefault for ${fieldName}: ${log.print field.fieldDefault}");
            };
            return (
              if !((resolve field.Type).__TypeId == "Field") then with indent; throws.block ''
                Invalid field encountered in setFields:

                  This = ${here (print This)}

                  field = ${here (print field)}
              ''
              # else if (this ? ${field.fieldName}) && (args ? ${field.fieldName}) then throw ''
              #   Field ${field.fieldName} already set to ${log.print this.${field.fieldName}}
              #   (when setting to ${log.print args.${field.fieldName}})
              # ''
              else if !hasValidValue then (with indent; throws.block ''
                Field ${fieldName} is not set in args and has no default value:
                  This = ${here (print This)}
                  args = ${here (print args)}
              '')
              else mkFieldAssignmentValue This fieldName value
            )
          )
          fields.instanceFieldsWithType;

    # Get all the static methods for a given type.
    # Any supplied in args will be bound here in Type.new as well as being made
    # available on instances when This.new is called
    boundStaticMethods = This: this: this_:
      let
        staticMethodsType = mapAttrs (name: staticMethod: staticMethod This) (This.staticMethods or {});
        staticMethodsInstance = mapAttrs (name: staticMethod: staticMethod this_) (this.staticMethods or {});
      in
        staticMethodsType // staticMethodsInstance;

    # Bind the methods of a This instance this, given a reference to the bound
    # instance as this_.
    boundMethods = This: this:
      mapAttrs
        (methodName: method: method this)
        (This.methods or {});

    # Initialise a type from its This type, its partial this set, and any args.
    mkthis = This: args:
      with log.vtrace.call "mkthis" This args ___;
      with lets rec {
        this = mkFieldAssignments This args;
        bindings = bindThis This this this_;
        this_ = mergeAttrsList [
          this
          bindings
        ];
      };
      this_;

    # Bind members that refer to this on construction and after any change.
    bindThis = This: this: this_:
      let
        accessors = mkAccessors This this this_;
        staticMethods = boundStaticMethods This this this_;
        methods = boundMethods This this_;
      in
        mergeAttrsList [
          accessors
          staticMethods
          methods
        ];

    # Create a new instance of a type by calling its constructor.
    # The constructor's output arguments are then passed into mkInstance.
    # For types, the constructor just merges a name parameter with an arguments
    # parameter and delegates to mkInstance.
    newInstance = This:
      with log.vtrace.call "newInstance" This ___;
      with check "This.ctor present" (This ? ctor) "This.ctor is not set";
      with check "This.tvars present" (This ? tvars) "This.tvars is not set";

      let mkViaCtor =
            traceComposeVariadic "mkViaCtor"
              "(mkInstance This)" "(This.ctor.bind This)"
               (mkInstance This)   (This.ctor.bind This);
      in traceVariadic mkViaCtor;

    # Build a new instance from a single dict of field values.
    # Construct an instance from a This type and attrset of field-to-value assignments.
    # arg = { fieldName = value, ... } for all fields.
    # Fields with defaults can be omitted.
    # Build a new instance from a single dict of field values.
    mkInstance = This: args_:
      with log.vtrace.call "mkInstance" This args_ ___;
      let
        # All instances have a common Type field.
        args = args_ // { Type = TypeThunk This; };

        # Construct 'this' as an instance of 'This'.
        this = mkthis This args;

        # Check the validity of the constructed instance.
        checks =
          let
            fields = This.fields This;
            suppliedFieldNames = attrNames args;
            fieldNames = map soloName fields.instanceFieldsWithType;
            populatedFieldNames = filter (name: this ? ${name}) fieldNames;
            requiredFieldNames = map soloName fields.requiredFields;

            # Get any supplied non-static fields not present in this or any supertype.
            unknownFieldNames = assign "unknownFieldNames" (
              subtractLists fieldNames populatedFieldNames);

            # Get any fields not populated in this or any supertype.
            missingFieldNames = assign "missingFieldNames" (
              subtractLists populatedFieldNames requiredFieldNames);
          in [
            {
              cond = unknownFieldNames == [];
              msg = "${log.print This}: Unknown fields in mkInstance call: ${joinSep ", " unknownFieldNames}";
            }
            {
              cond = missingFieldNames == [];
              msg = ''
                ${log.print This}: Missing fields in mkInstance call: ${joinSep ", " missingFieldNames}
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

    mkBuiltin = U: name:
      let
        hasSize = { String = true; Path = true; List = true; Set = true; }.${name} or false;
        withSize = methods:
          if hasSize
          then let sizeFn = this: size this.value;
               in methods // { size = sizeFn; }
          else methods;
      in
        U.Type.new name {
          fields = This: U.Fields.new [{ value = toLower name; }];
          methods = withSize ({
            List = {
              fmap = this: f: this.modify.value (map f);
              append = this: x: this.modify.value (xs: xs ++ [x]);
            };
            Set = {
              fmap = this: f: this.modify.value (mapAttrs (_: f));
              names = this: attrNames this.value;
              values = this: attrValues this.value;
              # e.g. this.modifyAt.name (x: x+1)
              getAt = this: this.value;
              modifyAt = this: mapAttrs (k: v: f: this.modify.value (xs: xs // { ${k} = f v; })) this.value;
              setAt = this: mapAttrs (k: _: x: this.modify.value (xs: xs // { ${k} = x; })) this.value;
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

    mkCtors = U: SU: with U; rec {
      # Only used in universe below; Ctors all created in terms of SU.Type
      Ctor = Type.new "Ctor" {
        fields = This: SU.Fields.new [
          { name = U.String; }
          { ctor = "lambda"; }
        ];
        methods = {
          bind = this: This: this.ctor This;
        };
      };

      Ctors = {
        # Explicit nullary constructor s.t. X.new == X.mk {}
        CtorNullary = SU.Ctor.new "CtorNullary" (This: {});

        # Default constructor for regular non-NewType/Builtin/Alias types.
        CtorFields = SU.Ctor.new "CtorFields" (This:
          with log.vtrace.call "CtorFields.ctor" { inherit This; } ___;
          let
            fields = assign "fields" (This.fields This);
            sortedFieldNames = assign "sortedFieldNames" (map soloName fields.instanceFields);
          in return (
            if size fields.instanceFields == 0 then {}
            else Variadic.mkOrdered sortedFieldNames
          )
        );

        # The constructor for Type in U and its precursors / descendent Type types
        # in subuniverses of U.
        # TODO: Defaults should not need restating in U_2+
        CtorType = SU.Ctor.new "CtorType" (This: name: args: {
          __isTypeSet = true;
          inherit name;
          Super = setThunkName "Super[Type.ctor]" (args.Super or (TypeThunk null));
          ctor = args.ctor or Ctors.CtorFields;
          fields = args.fields or (This: SU.Fields.new []);
          methods = args.methods or {};
          staticMethods = args.staticMethods or {};
          tvars = args.tvars or {};
          tvarBindings = args.tvarBindings or {};
          checkValue = args.checkValue or null;
          overrideCheck = args.overrideCheck or null;
        });
      };
    };

    mkBuiltins = U:
      mergeAttrsList (map (name: { ${name} = mkBuiltin U name; }) BuiltinNames) // {

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
            in if T == null then (throw ''
              Invalid T argument for Builtin.FromT:
              ${with log.prints; here builtinType ___}
            '')
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
                }."${typeOf x}" or (throw ''
                  Invalid type for Builtin.From:
                  ${with log.prints; here (getTypeName x) ___}
                ''));
            in T.mk { value = x; };  # mk not new here so new can use From

          getBuiltin = T: {
            Null = "null";
            Int = "int";
            Float = "float";
            String = "string";
            Path = "path";
            Bool = "bool";
            List = "list";
            Set = "set";
            Lambda = "lambda";
          }."${T.name}" or (throw ''
            Invalid type for Builtin.getBuiltin:
            ${with log.prints; here T ___}
          '');
        };
    };

    # e.g. parseFieldSpec Static<Default<Int, 123>> -> {fieldStatic = true, fieldDefault = 123; fieldType = Int; }
    #      parseFieldSpec Static<Int> -> {fieldStatic = true; fieldType = Int; }
    #      parseFieldSpec Default<Int, 123> -> {fieldDefault = 123; fieldType = Int; }
    #      parseFieldSpec Int -> { fieldType = Int; }
    parseFieldSpec = spec:
      # Unwrap Static types.
      # Duck-typed to support bootstrap.
      if spec ? staticType
        then (parseFieldSpec spec.staticType) // {
          fieldStatic = true;
        }

      # Unwrap Default types.
      # Duck-typed to support bootstrap.
      else if (spec ? defaultType) && (spec ? defaultValue)
        then (parseFieldSpec spec.defaultType) // {
          fieldDefault = NamedThunk "fieldDefault" spec.defaultValue;
        }

      # Return unwrapped types.
      else
        with check "isTypeLike spec" (spec == null || isTypeLike spec)
              "Got a non-type spec in parseFieldSpec: ${log.print spec}";

        {
          fieldType = spec;
          fieldStatic = false;
          fieldDefault = null;
        };

    # Universe-independent types that only depend on Type.
    mkTrivialTypes = U: SU: rec {
      # Unit Type
      Unit = U.Type.new "Unit" {
        ctor = U.Ctors.CtorNullary;
      };
      unit = Unit.new;

      # Uninhabited type
      Void = U.Type.new "Void" {
        ctor = SU.Ctor.new "CtorVoid" (_: thunk (throw "Void: ctor"));
      };

      # Any type
      # Used in Type fields (via Literal binding) so must be SU.Type.
      Any = SU.Type.new "Any" {overrideCheck = _: true;};
    };

    mkTemplating = U: rec {

      # Modify an argument set to inherit from a supertype.
      # If a ctor is set in args, it is used, otherwise the Super's ctor is inherited.
      # Fields and methods are also inherited, with any new ones overriding any previously defined.
      inheritFrom = SuperThunk: ctorArgs:
        with log.vtrace.call "inheritFrom" SuperThunk ctorArgs ___;
        with check "isThunk SuperThunk" (isThunk SuperThunk) "inheritFrom: SuperThunk must be a Thunk, got ${log.print SuperThunk}";
        with check "!(SuperThunk.do isNull)" (!(SuperThunk.do isNull)) "inheritFrom: Super must not be null, ${log.print (resolve SuperThunk)}";

        return (ctorArgs // {
          # Store the Thunk directly.
          Super = SuperThunk;

          # Override or inherit the ctor
          ctor = ctorArgs.ctor or (SuperThunk.do (T: T.ctor));

          # Merge fields into a new This:...
          fields = assign "fields" (This:
            let superFields = SuperThunk.do (Super: Super.fields This);
            in if ctorArgs ? fields
               then let ctorFields = assign "ctorFields" (ctorArgs.fields This);
                        ctorFieldSolos = assign "ctorFieldSolos" ctorFields.getSolos;
                    in superFields.update ctorFieldSolos
               else superFields);

          # Merge methods
          methods = SuperThunk.do (T: T.methods // (ctorArgs.methods or {}));

          # Merge static methods
          staticMethods = SuperThunk.do (T: T.staticMethods // (ctorArgs.staticMethods or {}));
        });

      # For a given type, create a new type of the same Type with the given name and args, inheriting any
      # unspecified fields, ctor, and non-overridden methods.
      newSubType = This: name: args:
        with log.vtrace.call "newSubType" { inherit This name args; } ___;

        return (
          if !(isTypeSet This) then throw "Cannot subtype non-Type or Type-precursor: ${log.print This}"
          else U.Type.new name (inheritFrom (TypeThunk This) args)
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
      #   fields = This: Fields.new [{ t = _.T;} {u = _.U;}];
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
      let Type__grounded = Type // { Type = TypeThunk Type__grounded; };
      in Type__grounded;

    # Cause a Type to have itself as its own Type, eliding any information about
    # either bootstrap pseudo-types or the Type of its superuniverse.
    # Also asserts that recreating the grounded Type using itself, via Type.new,
    # creates an identical Type (modulo lambda equality).
    groundTypeAndAssertFixed = opts: Type__args: Type:
      let Type__grounded = groundType Type;
      in assert assertTypeFixedUnderNew Type__grounded opts.typeName Type__args;
        Type__grounded;

    withCommonUniverse = opts: SU: U:
      foldl'
        (U: f: f U SU // U)
        U
        [
          (_: _: {inherit opts;})
          (U: SU: mkUniverseReferences opts U SU)
          (_: SU: mkBuiltins SU)
          (U: SU: mkCtors U SU)
          (_: SU: mkTemplating SU)
          (U: SU: mkTrivialTypes U SU)
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
      with cutils.tests.Compare;
      let
        fx = f x;
        x_NL = NoLambdas x;
        fx_NL = NoLambdas fx;
        printDepth = 10;
        assertion = assertMsg (x_NL == fx_NL) (indent.block ''
          ${xLabel} is not fixed under ${fLabel}:

            ${xLabel} (original):
            ${indent.here (log.vprintD printDepth x)}

            ${xLabel} (under ${fLabel}):
            ${indent.here (log.vprintD printDepth fx)}

          Comparing lambda-free:
            ${xLabel} (lambda-free, original):
            ${indent.here (log.vprintD printDepth x_NL)}

            ${xLabel} (lambda-free, under ${fLabel}):
            ${indent.here (log.vprintD printDepth fx_NL)}

          Diff:
            ${indent.here (log.vprintD printDepth (diff x_NL fx_NL))}
        '');
      in assertion;

    assertTypeFixedUnderNew = T: typeName: typeArgs:
      assertFixedUnderF
        "new"
        "Type"
        (T: T.new typeName typeArgs)
        T;

    # Lazily cascading options that disable typechecking at levels U_0 and U_1.
    # Options for the next universe can be produced via opts.descend {}.
    mkUniverseOpts = level:
      let
        # During the bootstrap, do not enable typechecking.
        # All fields have type 'null' and do not make use of builtin types
        # or Default/Static, which are not yet defined.
        optEnableTypeChecking = level: level >= 2;
        # During the bootstrap, do not enable Default/Static structure.
        # When this is retained, if type checking is enabled, the entire field type
        # is kept e.g. Static (Default Int 123)
        # If type checking is disabled in this level, the Static/Default structure is retained
        # but the type is nulled out e.g. Static (Default null 123)
        optRetainTypeFieldSpec = level: level >= 2;
        # We only expect Type to be fixed under Type.new "Type" when it contains and produces
        # no shim elements of the Quasiverse.
        # U_0 is entirely shim elements and a Type made of shims.
        # U_1 has a Type made from U_0's Type.new and is made of shim elements.
        # U_2 has a Type made from U_1's Type.new and is made of untyped U_1 elements.
        # U_3 has a Type made from U_2's Type.new and is made of typed U_2 elements.
        # Type in U3+ should be fixed under .new, made entirely of final typed elements and producing
        # types themselves made of final typed elements.
        optCheckTypeFixedUnderNew = level: level >= 3;
      in {
        inherit level;
        name = "U_${toString level}";
        typeName = "Type";
        enableTypeChecking = optEnableTypeChecking level;
        retainTypeFieldSpec = optRetainTypeFieldSpec level;
        checkTypeFixedUnderNew = optCheckTypeFixedUnderNew level;
        descend = _: mkUniverseOpts (level + 1);
      };
    UniverseOpts_0 = mkUniverseOpts 0;

    # Create shim types appearing as instances of Type.
    mkTypeShim = name: attrs:
      attrs // {
        __isTypeSet = true;
        __TypeId = name;
        inherit name;
        boundName = name;
        tvars = {};
        check = _: true;
        __toString = _: "TypeShim<${name}>";
      };

    mkInstanceShim = shimT: attrs:
      attrs // {
        Type = UnsafeTypeThunk "InstanceShim[${shimT.name}]" shimT;
      };

    mkBootstrappedType = opts: U: SU: rec {
      # Create simple untyped arguments using the pseuedotypes below for containers,
      # fields, literals, etc.
      Type__args = mkTypeArgsFor opts U SU;

      # Bootstrap a handmade type by treating typeArgs as though it was already created
      # via Type.new. This requires setting Type manually, which we can do by grounding it via
      # an unsafe shim.
      Type__handmade =
        mkInstanceShim
          Type__handmade
          (mkTypeShim
            "Type"
            (Type__args // {
              Type = UnsafeTypeThunk "Type__args" Type__args;
            })
          );

      # Convert the handmade type into a bona-fide type by instantiating it as though
      # 'Type__handmade.new "QuasiType" typeArgs' was called.
      # Must be newInstance rather than mkInstance here, as we need to push typeArgs through
      # PseudoType.ctor.
      Type__bootstrapped = newInstance Type__handmade opts.typeName Type__args;

      # Construct Type through an actual application of the .new constructor.
      # This is valid besides Type__new.(Type {}).(Type {}).Type {} == Type__args, and after that,
      # Type__args ? Type == false.
      # We fix this in the next stage.
      Type__new = Type__bootstrapped.new opts.typeName Type__args;

      # Finally, ground this Type by setting Type.Type to return itself, eliding any information
      # about the bootstrap types.
      Type =
        # In U_3 and beyond, this should now have reached a fixed point in terms
        # of further bootstrapping by Type.new, modulo lambda equality on created Type instances.
        # If enabled in the opts, an assertion checks that Type is fixed under further bootstrapping.
        if opts.checkTypeFixedUnderNew
          then groundTypeAndAssertFixed opts Type__args Type__new
          else groundType Type__new;
    };

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

      in
        withCommonUniverseSelf opts (
          rec {
            __Bootstrap = mkBootstrappedType opts U SU;

            # Expose the final Type.
            Type = __Bootstrap.Type;

            Ctor = mkTypeShim "Ctor" {
              new = name: ctor: mkInstanceShim Ctor {
                name = { value = name; };
                inherit ctor;
                bind = This: ctor This;
              };
            };

            # Shim out all types used in the construction of Type s.t. Type can be created
            # with U == SU == Quasiverse.
            Field = mkTypeShim "Field" {
              new = fieldName: fieldSpec:
                let
                  get = {
                    fieldName = { value = fieldName; };
                    inherit fieldSpec;
                  };
                  parsedSpec = parseFieldSpec fieldSpec;
                in mkInstanceShim Field (get // parsedSpec // {
                  inherit get;
                  getSolo = { ${fieldName} = get; };
                  hasDefault = parsedSpec.fieldDefault != null;
                });
            };

            # TODO: One implementation
            Fields =
              mkTypeShim "Fields" {
                new = fieldSpecs_:
                  let
                    # Include Type field on all instances.
                    fieldSpecs = withCommonFieldSpecs opts SU fieldSpecs_;
                    soloFields = mapSolos U.Field.new fieldSpecs;
                  in mkInstanceShim Fields (rec {
                    getSolos = soloFields;
                    indexed = mergeAttrsList (cutils.attrs.indexed soloFields);
                    update = newFieldSpecs:
                      Fields.new (concatSolos (solos fieldSpecs) (solos newFieldSpecs));
                    getField = name: indexed.${name}.value;
                    getFieldsWhere = pred: filterSolos pred getSolos;
                    instanceFields =
                      getFieldsWhere (fieldName: field:
                        fieldName != "Type"
                        && (field == null
                            || !field.fieldStatic));
                    instanceFieldsWithType =
                      getFieldsWhere (fieldName: field:
                        field == null
                        || !field.fieldStatic);
                    requiredFields = getFieldsWhere (_: field:
                      field == null
                      || (!field.fieldStatic
                          && !field.hasDefault));
                  });
              };

            SetOf = T: mkTypeShim "SetOf" { new = U.Set.new; };
            ListOf = T: mkTypeShim "ListOf" { new = U.List.new; };
            Constraint = mkTypeShim "Constraint" { new = x: {value = x; satisfiedBy = _: true;};};
            Static = T: mkTypeShim "Static" { staticType = T; };
            Default = T: V: mkTypeShim "Default" { defaultType = T; defaultValue = V; };
            NullOr = T: mkTypeShim "NullOr" { new = id; };
            Literal = V: mkTypeShim "Literal" { getLiteral = V; new = { getLiteral = V; }; };
            Sized = n: T: mkTypeShim "Sized" { new = x: { getSized  = x; }; };
            Any = mkTypeShim "Any" {};
          }
        );

    # Create a new universe descending from SU.
    mkSubUniverse = SU:
      let opts = SU.opts.descend {}; in
      with log.vtrace.call "mkSubUniverse" opts "<SU>" ___;
      with msg "Constructing ${opts.name} universe";
      let U = withCommonUniverse opts SU rec {

        # Construct the Type type in terms of the SU, then in terms of itself.
        __Bootstrap = rec {
          Type__args = mkTypeArgsFor opts U SU;
          Type__new = SU.Type.new opts.typeName Type__args;
          Type =
            if opts.checkTypeFixedUnderNew
              then groundTypeAndAssertFixed opts Type__args Type__new
              else groundType Type__new;
        };
        #__Bootstrap = mkBootstrappedType opts U SU;
        Type = __Bootstrap.Type;

        # A constraint on a type variable.
        Constraint = Type.new "Constraint" {
          fields = This: SU.Fields.new [
            { constraintType = U.Type; }
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
        ListOf_ = U.List.subTemplate "ListOf" {T = SU.Type;} (_: {
          checkValue = that: all (x: hasType _.T x) that.value;
        });
        ListOf = T: U.ListOf_.bind { inherit T; };

        # Subtype of Set that enforces homogeneity of value types.
        SetOf_ = U.Set.subTemplate "SetOf" {T = Type;} (_: {
          checkValue = that: all (x: hasType _.T x) that.values;
        });
        SetOf = T: U.SetOf_.bind { inherit T; };

        # A type that enforces a size on the value.
        Sized_ = Type.template "Sized" {N = Type; T = Type;} (_: {
          fields = This: SU.Fields.new [{ getSized = _.T; }];
          checkValue = that:
            (_.T.checkValue or (const true)) that
            && that.size == _.N.getLiteral;
        });
        Sized = n: T:
          let N = U.Literal n;
          in U.Sized_.bind { inherit N T; };

        # A type satisfied by any value of the given list of types.
        Union_ = Type.template "Union" {Ts = Type;} (_: {
          overrideCheck = that: any (T: hasType T that) _.Ts.getLiteral;
        });
        Union = Tlist:
          let Ts = U.Literal Tlist;
          in U.Union_.bind {inherit Ts;};

        # A value or T or Null.
        NullOr = T: U.Union ["null" U.Null T];

        # An attribute set with attributes zero-indexed by their definition order.
        # xs = Ordered.new [ {c = 1;} {b = 2;} {a = 3;} ];
        # xs.value == { a = 1; b = 2; c = 3; } (arbitrary order)
        # xs.names == [ "c" "b" "a" ] (in order of definition)
        # xs.values == [ 1 2 3 ] (in order of definition)
        OrderedItem_ = Type.template "OrderedItem" { T = Type; } (_: {
          # TODO: Could be a unary cast, or inherit from Sized 1 (SetOf T)
          ctor = SU.Ctor.new "CtorOrderedItem" (This: x: {
            value =
              let setOfX = ((SU.SetOf _.T).new x);
                  sizedSetOfX = (SU.Sized 1 (SU.SetOf _.T)).new setOfX;
              in sizedSetOfX;
          });

          fields = This: SU.Fields.new [{
            value = SU.Sized 1 (SU.SetOf _.T);
          }];

          methods = {
            getSolo = this: this.value.getSized.value;
            getName = this: soloName this.getSolo;
            getValue = this: soloValue this.getSolo;
          };
        });
        OrderedItem = T: OrderedItem_.bind { inherit T; };

        OrderedOf_ = Type.subTemplateOf (_: U.ListOf (U.OrderedItem _.T)) "OrderedOf" {T = Type;} (_: {
          # Pass OrderedItems to the underlying ListOf
          ctor = SU.Ctor.new "CtorOrderedOf" (This: xs: {
            value = map (x: (U.OrderedItem _.T).new x) (solos xs);
          });

          methods = {
            # Get the solo attr list in order.
            getSolos = this: this.mapItems (item: item.getSolo);

            # The unordered merged attribute set
            unindexed = this: mergeAttrsList this.getSolos;

            # The merged attribute set with an additional 'index' field indicating
            # its place in the order.
            indexed = this: mergeAttrsList (indexed this.getSolos);

            # The ordered attribute names.
            names = this: this.mapItems (item: item.getName);

            # The ordered attribute values.
            values = this: this.mapItems (item: item.getValue);

            # A set from name to index.
            indexes = this: this.imapItems (i: item: { ${item.getName} = i; });

            # Map over the underlying [OrderedItem T]
            # f :: (OrderedItem T -> a) -> [a]
            mapItems = this: f: map f this.value;

            # Map over the underlying [OrderedItem T] with index.
            # f :: (int -> OrderedItem T -> a) -> [a]
            imapItems = this: f: imap0 f this.value;

            # Map over the underlying [solo T] in order
            # f :: (string -> T -> a) -> [a]
            mapSolos = this: f: this.mapItems (item: f item.getName item.getValue);

            # Map over the underlying indexed [solo T]
            # f :: (int -> string -> T -> a) -> [a]
            imapSolos = this: f: this.imapItems (index: item: f index item.getName item.getValue);

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

            # Update by inserting the given items sequentially at the end of the order.
            # If any already appear, they update the item in its original order.
            update = this: items_:
              let items = solos items;
              in Fields.new (concatSolos this.getSolos items);
          };

          checkValue = that:
            Super.checkValue that
            && assertMsg
                (size that.names == size that.unindexed)
                "Duplicate keys in OrderedOf: ${joinSep ", " that.names}";
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
                fields = This: SU.Fields.new [
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
        Literal_ = Type.template "Literal" {V = U.Any;} (_: rec {
          ctor = SU.Ctors.CtorNullary;
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
          staticMethods.defaultType = This: _.T;
          staticMethods.defaultValue = This: _.V.getLiteral;
          overrideCheck = that: _.T == null || _.T.check that;
        });
        Default = T: v:
          let V = U.Literal v;
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
        # (Field.new "myField" Int
        # (Field.new "myField" (Static Int)) -> Field<Static<Int>>.fieldType == Int
        # (Field.new "myField" (Default Int 123)) -> Field<Default<Int, 123>>.fieldType == Int
        # (Field.new "myField" (Static (Default Int 123))) -> Field<Static<Default<Int, 123>>>.fieldType == Int
        Field = Type.new "Field" {
          fields = This: SU.Fields.new [
            {fieldName = U.String;}
            {fieldSpec = SU.NullOr Type;}
          ];
          methods = {
            parsedT = this: parseFieldSpec this.fieldSpec;
            fieldType = this: this.parsedT.fieldType;
            fieldStatic = this: this.parsedT.fieldStatic;
            hasDefault = this: this.parsedT.fieldDefault != null;
            fieldDefault = this: this.parsedT.fieldDefault;
          };
        };

        # Fields is an OrderedOf that first converts any RHS values into Field types.
        Fields = (U.OrderedOf U.Field).subType "Fields" {
          # Fields ctor just converts the incoming list or set of field specs into
          # the list of Field that OrderedOf Field expects.
          #
          # Fields.new { field = FieldType; ... }
          # Fields.new { field = Default FieldType defaultValue; ... }
          # Fields.new { field = Static FieldType; ... }
          # Fields.new [ { field = FieldType; ... } ... ]
          # Fields.new [ { field = Default FieldType defaultValue; ... } ... ]
          # Fields.new [ { field = Static FieldType; ... } ... ]
          ctor = SU.Ctor.new "CtorFields" (This: fieldSpecs_:
            let
              # Include Type field on all instances.
              fieldSpecs = withCommonFieldSpecs opts SU fieldSpecs_;
              soloFields = mapSolos U.Field.new fieldSpecs;
              # Call the OrderedOf constructor.
              # TODO: Shorthand for calling super().
              superCtor = This.Super.do (T: T.ctor);
              args = superCtor.bind This soloFields;
            in
              args
          );
          methods = {
            getIndexedField = this: name: this.indexed.${name};
            getField = this: name: (this.getIndexedField name).value;
            getFieldsWhere = this: pred: filterSolos pred this.getSolos;
            instanceFields = this: this.getFieldsWhere (fieldName: field:
              fieldName != "Type"
              && (field == null
                  || !field.fieldStatic));
            instanceFieldsWithType = this: this.getFieldsWhere (fieldName: field:
              field == null
              || !field.fieldStatic);
            requiredFields = this: this.getFieldsWhere (_: field:
              field == null
              || (!field.fieldStatic
                  && !field.hasDefault));
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
      allUniverses = Universe // { inherit TS; };
      untypedUniverses = {inherit (allUniverses) U_0 U_1;};
      typedUniverses = {inherit (allUniverses) U_2 U_3 U_4; inherit TS; };

      TestTypes = U: with U; {
        MyType = Type.new "MyType" {
          fields = This: Fields.new [{ myField = String; }];
          methods.helloMyField = this: extra: "Hello, ${this.myField.value}${extra}";
        };

        MyType2 = Type.new "MyType2" {
          fields = This: Fields.new [
            { stringField = String; }
            { intField = Int; }
            { defaultIntField = Default Int 666; }
            { staticIntField = Static Int; }
            { staticDefaultIntField = Static (Default Int 666); }
          ];
        };

        MyString = Type.new "MyString" {
          fields = This: Fields.new [{ value = String; }];
        };

        WrapString = String.subType "WrapString" {};
      };


      smokeTests = U: with U; let SU = resolve U._SU.get; in {

        Bootstrap = with __Bootstrap; {
          Type__args = {
            ctor.defaults =
              let expected = Type__args.ctor.bind Type__args "A" {};
              in expect.printEq
                expected
                {
                  __isTypeSet = true;
                  Super = expected.Super;  # TODO: Cheat due to named thunk comparison
                  checkValue = null;
                  ctor = U.Ctors.CtorFields;
                  fields = This: SU.Fields.new [];
                  methods = {};
                  name = "A";
                  overrideCheck = null;
                  staticMethods = {};
                  tvarBindings = {};
                  tvars = {};
                };
          };
        };

        Type = {
          new = {
            id = expect.eq (Type.new "A" {}).__TypeId "A";
            name = expect.eq (Type.new "A" {}).name "A";
            boundName = expect.eq (Type.new "A" {}).boundName "A";
          };
        };

      };

      instantiationTests = U: with U; with TestTypes U; {
        Type = expect.equal (Type.new "SomeType" {}).__TypeId "SomeType";

        Literal = {
          static = expect.equal (Literal 123).getLiteral 123;
          instance = expect.equal (Literal 123).new.getLiteral 123;
        };

        Field = {
          expr = let f = Field.new "name" null;
                 in [f.fieldName.value f.fieldType f.fieldStatic f.fieldDefault];
          expected = ["name" null false null];
        };

        MyString_nocast = {
          mkFromString = expect.eq (MyString.mk { value = String.new "hello"; }).value.value "hello";
          newFromString = expect.eq (MyString.new (String.new "hello")).value.value "hello";
        };

        WrapString_nocast = {
          mkFromString = expect.eq (WrapString.mk { value = "hello"; }).value "hello";
          newFromString = expect.eq (WrapString.new "hello").value "hello";
        };

        MyType_mk_nocast = {
          expr = (MyType.mk { myField = String.new "World"; }).myField.value;
          expected = "World";
        };

        MyType_new_nocast = {
          expr = (MyType.new (String.new "World")).myField.value;
          expected = "World";
        };
      };

      builtinTests = U: with U; {
        mk =
          let
            mkBuiltinTest = T: name: rawValue: {
              expr =
                let x = T.new rawValue;
                in {
                  name = x.Type.do (T: T.name);
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
        };

      untypedTests = U: with U;
        let
          fields = Fields.new [
            { a = null; }
            { b = null; }
            { c = null; }
          ];
          fieldsFromSet = Fields.new {
            c = null;
            b = null;
            a = null;
          };
        in {
          fromListEqFromSet =
            expect.eqOn
              (this: mapSolos (_: field: field.get) this.getSolos)
              fields
              fieldsFromSet;

          getSolos =
            expect.eq
              (soloNames fields.getSolos)
              [ "Type" "a" "b" "c" ];
        };

      castTests = U: with U;
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
          mkToTypedBuiltinTest = T: x: expect.fieldsEq (cast_ T x) (T.new x);
          mkToUntypedBuiltinTest = T: x: {
            expr = cast_ (Builtin.getBuiltin T) (T.new x);
            expected = x;
            compare = Compare.Fields;
          };
        in {
          to = {
            typedBuiltin = mapAttrs (name: v: mkToTypedBuiltinTest U.${name} v) testX;
            untypedBuiltin = mapAttrs (name: v: mkToUntypedBuiltinTest U.${name} v) testX;
          };
        };

      typeCheckingTests = U: with U;
        let
          A = Type.new "A" {
            fields = This: Fields.new [
              { a = "int"; }
              { b = Int; }
              { c = Default Int 5; }
              { d = Default Int (Int.new 10); }
            ];
          };
        in {
          Int = expect.eq (Int.new 123) 123;
          A.new = expect.eq (A.new 1 2 3 4).get {a = 1; b = 2; c = 3; d = 4;};
          A.mk = expect.eq (A.mk {a = 1; b = 2;}).get {a = 1; b = 2; c = 5; d = 10;};
          A.wrongType = expect.error (A.new "no" 2 3 4).get;

          castInMk = with TestTypes U; {
            MyString = {
              mkFromstring = expect.eq (MyString.mk { value = "hello"; }).value.value "hello";
              newFromstring = expect.eq (MyString.new "hello").value.value "hello";
              eqString = expect.eqOn Compare.Fields (MyString.new "hello").value (String.new "hello");
            };

            WrapString = {
              mkFromstring = expect.eq (WrapString.mk { value = "hello"; }).value "hello";
              newFromstring = expect.eq (WrapString.new "hello").value "hello";
              eqString = expect.eqOn Compare.Fields (WrapString.new "hello") (String.new "hello");
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
          };
        };

      typeFunctionalityTests = U: with U; with TestTypes U; {

        checks = {
          RootType = {
            expr = Type.name;
            expected = "Type";
          };
        };

        methodCalls = {
          MyType_call = {
            expr =
              let this = MyType.new (String.new "World");
                in this.helloMyField "!";
            expected = "Hello, World!";
          };
        };

        setField = {
          MyType_set_nocast = {
            expr =
              let this = MyType.new (String.new "");
              in [
                (this.helloMyField "!")
                ((this.set.myField "World").helloMyField "!")
              ];
            expected = [ "Hello, !" "Hello, World!" ];
          };
        };

        modifyField = {
          MyType_modify = {
            expr =
              let this = MyType.new (String.new "Hello");
              in (this.modify.myField (x: String.new "${x.value}, World!"));
            expected = MyType.new (String.new "Hello, World!");
            compare = Compare.Fields;
          };
        };

        inheritance =
          let
            A = Type.new "A" {
              fields = This: Fields.new {
                a = String;
              };
            };
            B = A.subType "B" {
              ctor = Ctor.new "CtorB" (This: a: b: {
                inherit a b;
              });
              fields = This: Fields.new {
                b = Int;
              };
            };
          in {

            newA = {
              expr = A.new "a";
              expected = A.mk { a = "a"; };
              compare = Compare.Fields;
            };

            isSuperTypeOf = {
              parentChild = expect.True (A.isSuperTypeOf B);
              childParent = expect.False (B.isSuperTypeOf A);
              parentParent = expect.False (A.isSuperTypeOf A);
              childChild = expect.False (B.isSuperTypeOf B);
              typeParent = expect.False (Type.isSuperTypeOf A);
              typeChild = expect.False (Type.isSuperTypeOf B);
              typeType = expect.False (Type.isSuperTypeOf Type);
            };

            isSubTypeOf = {
              parentChild = expect.False (A.isSubTypeOf B);
              childParent = expect.True (B.isSubTypeOf A);
              parentParent = expect.False (A.isSubTypeOf A);
              childChild = expect.False (B.isSubTypeOf B);
              typeParent = expect.False (Type.isSubTypeOf A);
              typeChild = expect.False (Type.isSubTypeOf B);
              typeType = expect.False (Type.isSubTypeOf Type);
            };

        };
      };

    in
      cutils.tests.suite {
        types = with Universe; {

          peripheral = {
            fixing = {
              intUnderId = expect.asserts.ok (assertFixedUnderF "f" "x" id 123);
              intUnderPlus1 = expect.asserts.fail (assertFixedUnderF "f" "x" (x: x+1) 123);
              intUnderPlus0 = expect.asserts.ok (assertFixedUnderF "f" "x" (x: x+0) 123);
              Type =
                let
                  FakeType = name: {
                    inherit name;
                    new = name: args: FakeType name;
                  };
                in {
                  fixed = expect.asserts.ok (assertTypeFixedUnderNew (FakeType "FakeType") "FakeType" {});
                  # unfixed = expect.asserts.fail (assertTypeFixedUnderNew (FakeType "FakeType") "NextFakeType" {});
                  unfixed = expect.error (assert assertTypeFixedUnderNew (FakeType "FakeType") "NextFakeType" {}; true);
                };
            };
          };

          smoke = testInUniverses {
            inherit
              U_0
              U_1
              # U_2
              ;
          } smokeTests;

          typeFunctionality = testInUniverses {
            inherit
              U_0
              U_1
              # U_2
              ;
          } typeFunctionalityTests;

          instantiation = testInUniverses {
            inherit
              U_0
              U_1
              # U_2
              ;
          } instantiationTests;

          builtin = testInUniverses {
            inherit
              U_0
              U_1
              # U_2
              ;
          } builtinTests;

          cast = testInUniverses {
            inherit
              U_0
              U_1
              # U_2
              ;
          } castTests;

          untyped = testInUniverses {
            inherit
              U_0
              # U_1
              ;
          } untypedTests;

          typeChecking = testInUniverses {
            inherit
              # U_0
              # U_1
              # U_2
              ;
          } typeCheckingTests;

        };
      };

}

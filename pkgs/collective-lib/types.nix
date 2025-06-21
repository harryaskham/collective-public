{ pkgs ? import <nixpkgs> {}, lib ? pkgs.lib,
  cutils ? import ./. { inherit lib; }, ... }:

with lib;
with lib.strings;
with cutils.collections;
with cutils.clib;
with cutils.attrs;
with cutils.dispatch;
with cutils.functions;
with cutils.lists;
with cutils.strings;

# TODO:
# - TypeClasses using mkMerge - i.e. instances form a fixed-point class dictionary
# - Enums
# - Maybe / ADTs
# - Tvars/TvarBindings need to be lazy as well

# Typesystem for use outside of the module system.
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
  errors = cutils.errors;
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
    BuiltinValueCheck = x: x ? Type && BuiltinNameCheck (x.Type.__do (T: T.getName {}));

    str = x: if typeOf x == "string" then x
      else if isTyped x && ((resolve x.Type).getName {}) == "String" then x.value
      else throw (indent.block ''
          Cannot convert to string:
            ${log.print x}
        '');

    maybeStr = x: errors.try (str x) (_: x);

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

    cast = T_: x:
      with (log.v 4).call "cast" T_ x ___;
      let T = maybeStr T_; in

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
          if isTyped x then x.Type.__do (T:
            let fields = T.fields or (throw ''TFields: T.fields does not exist: ${log.print T}'');
            in (fields T).instanceFields {})
          else
            mkCastError ''
              Cannot get fields from untyped uncast value: T = ${log.print T}, x = ${log.print x}
            '');
        TFields = (
          if isTypeSet T then
            let fields = T.fields or (throw ''TFields: T.fields does not exist: ${log.print T}'');
            in (fields T).instanceFields {}
          else
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
          else maybeResolve ((soloValue field).fieldType or null));
        TUnaryFieldT = castErrorOr TUnaryField (field:
          if field == null then mkCastError ''Unary field is not a solo: ${log.print field}''
          else maybeResolve ((soloValue field).fieldType or null));

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

            # No-op cast if x is already an instance of a Union.
            {
              name = "Union";
              when = isTypeSet T && T.getName {} == "Union";
              orMsg = indent.block ''
                Not a Union type:
                  ${indent.here castStr}
              '';
              result =
                let go = errors: Ts:
                      if Ts == [] then mkCastError (joinLines errors)
                      else
                        let T = head Ts;
                            Ts' = tail Ts;
                            c = cast T x;
                        in if isCastSuccess c then c
                           else go (errors ++ [c.castError]) Ts';
                in go [] ((resolve T.tvarBindings).Ts.getLiteral {});
              failMsg = id;
              successMsg = _: ''
                Union cast succeeded: ${castStr}
              '';
            }

            # {
            #   name = "Coerce";
            #   when = (T.checkValue or (const false)) x;
            #   orMsg = indent.block ''
            #     No value-check or not directly checkValue-coercible:
            #       ${indent.here castStr}
            #   '';
            #   result = mkCastSuccess (T.mk (mapAttrs (_: resolve) x.get)) "";
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
                if !(T ? mk) then mkCastError (indent.block ''
                  ${TName} does not have a 'mk' method
                '')
                else
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
                              (((T.fields T).getField TFieldName).fieldType {})
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
                    ${indent.here (log.print (resolve __logState))}
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

    setFieldTypeToNull = opts: SU: spec:
      if !opts.retainTypeFieldSpec then null
      else
        if spec ? defaultType then
          SU.Default (setFieldTypeToNull opts SU (spec.defaultType {})) (spec.defaultValue {})
        else if spec ? staticType then
          SU.Static (setFieldTypeToNull opts SU (spec.staticType {}))
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
        [{Type = maybeNulled opts SU (SU.ThunkOf SU.Type);}]
        (solos fieldSpecs);

    # Construct the fields for a universe using the types of the universe above.
    # The 'Type' field is added in Fields.new in both shim and real implementations.
    mkTypeFieldListFor = U: SU: [
      # Indicates that this is a type. Should never be set manually.
      {__isTypeSet = maybeNulled U.opts SU (SU.Default "bool" true);}
      # The supertype of the type.
      {Super = maybeNulled U.opts SU (SU.Default "set" (TypeThunk null));}
      # The name of the type.
      {name = maybeNulled U.opts SU (SU.String);}
      # The type parameters of the type.
      {tvars = maybeNulled U.opts SU (SU.Default "set" (LazyAttrs {}));}
      # The type parameter bindings of the type.
      {tvarBindings = maybeNulled U.opts SU (SU.Default "set" (LazyAttrs {}));}
      # The constructor function creating the fields of the type as a set to pass to mk.
      {ctor = maybeNulled U.opts SU (SU.Default U.Ctor U.Ctors.CtorFields);}
      # A set of ordered fields to make available as this.___ and this.has.___, this.set.___, etc
      {fields = maybeNulled U.opts SU (SU.Default "lambda" (This: SU.Fields.new []));}
      # A set of methods from this to make available as this.___
      {methods = maybeNulled U.opts SU (SU.Default "set" (LazyAttrs {}));}
      # A set of methods from This to make available as This.___ and this.___
      {staticMethods = maybeNulled U.opts SU (SU.Default "set" (LazyAttrs {}));}
      # Perform additional checks on the value of the type when comparing.
      {checkValue = maybeNulled U.opts SU (SU.Default (SU.NullOr "lambda") null);}
      # If set, ignore all other checks and use this check function only.
      {overrideCheck = maybeNulled U.opts SU (SU.Default (SU.NullOr "lambda") null);}
    ];

    mkTypeFieldsFor = U: SU:
      let fieldList = mkTypeFieldListFor U SU;
      in SU.Fields.new fieldList;

    # We need to be very careful here to only access U from sites that are
    # called by a fully bound and constructed U.Type (i.e. during U.Type.newTemplate)
    # so that we do not attempt to access types that will themselves attempt to
    # force U.Type so they can be constructed.
    # See mkTemplating - essentially anywhere we produce a new Type here, we need
    # to do so from U.* - i.e. we need to use U.newSubType instead of SU.newSubType
    # so that ((U.Type.new "Parent" {}).subType "Child" {}).Type == U.Type and not SU.Type,
    # which would mean every subtype created in a chain would ascend a universe until
    # it reached the Quasiverse.
    typeMethodsFor = U: SU: {
      # Get the resolved name of the type.
      getName = This: _: This.name.value;

      # Is this instance an instance of type That.
      isInstance = This: that: isTyped that && that.Type.__do (That: That.eq This);

      # A = Type.new "A" {};
      # B = Type.new "B" { Super = A; };
      #
      # isSuperTypeOf A B == true
      # isSuperTypeOf B A == false
      # isSuperTypeOf A A == false
      # isSuperTypeOf Type A == true
      # isSuperTypeOf Type B == true
      isSuperTypeOf = This: That:
        if That.Super.__do isNull then false
        else That.Super.__do (typeEq This)
             || That.Super.__do (ThatSuper: This.isSuperTypeOf ThatSuper);
      isSubTypeOf = This: That: That.isSuperTypeOf This;

      # Get the full templated name of the type.
      # Usually would be safe to use U.Void, except for the U.Void.getBoundName binding,
      # so uses SU.Void.
      getBoundName = This: thunk (
        with (log.v 4).methodCall This "getBoundName" ___;
        return (
          if (resolve This.tvars) == {} then (This.getName {})
          else
            let
              printBinding = tvarName:
                let C = (resolve This.tvars).${tvarName} or (
                      throw "No type variable ${tvarName} on ${This.getName {}}");
                    T = (resolve This.tvarBindings).${tvarName} or SU.Void;
                in
                  # Unbound
                  if typeEq SU.Void T
                    then tvarName

                  # Bound to a type
                  else if isTypeSet T
                  then T.getBoundName {}

                  # Bound to a literal or builtin
                  else
                    with log.prints; put T _line ___;
              printBindings = joinSep ", " (map printBinding (This.tvars.__attrNames {}));
            in "${This.getName {}}<${printBindings}>"
        ));

      # Construct the type resulting from applying the given bindings to the parameterized This type.
      # Bind is only exposed after U.Type is constructed on instances of U.Type.
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
                  ${indent.here "${This.getName {}}.${tvarName} <- ${log.print T}"}
                  Constraints: ${indent.here "${log.print C.value} (${if TSatC then "" else "not "}satisfied)"}
                  Existing binding: ${log.print B}
                  ${indent.here msg}
                '');
              C = (resolve This.tvars).${tvarName} or null;
              B = (resolve This.tvarBindings).${tvarName} or SU.Void;
              TSatC = C.satisfiedBy T;
            in
              if C == null
              then throwBindError "Type ${This.getName {}} does not have type variable ${tvarName}"

              else if (resolve This.tvarBindings) == null
                then throwBindError "Type ${This.__TypeId {}} does not have bound or unbound type variable bindings"

              else if !(typeEq B SU.Void)
                then throwBindError "Type ${This.__TypeId {}} already has type variable ${tvarName} bound to ${log.print B}"

              else if !(C.satisfiedBy T)
                then throwBindError "Binding ${log.print T} does not satisfy constraint: ${log.print C}"

              else This.modify.tvarBindings (bs: bs.__fmap (bs: bs // {${tvarName} = T;}));

        in foldl' bindOne This tvarBindingsList;

      # Is That the same type as This?
      eq = This: That: typeEq This That;

      # Does this type inherit from That?
      inheritsFrom = This: That:
        isTypeSet That
        && That.eq This
        || (!(This.Super.__do isNull)
            && This.Super.__do (Super: Super.inheritsFrom That));

      check = This: that:
        let
          runChecks = errors.tryBool (errors.checks [
            {
              cond = hasType This that;
              msg = "Type check failed for ${This.__TypeId {}} (got ${getTypeName that})";
            }
            {
              cond = This.checkValue == null || This.checkValue that;
              msg = "checkValue failed: ${log.print that} is not a value of type ${This.__TypeId {}}";
            }
          ]);
        in if This.overrideCheck != null
          then This.overrideCheck that
          else runChecks;

      # Use the bound name with its ordered param assignments to determine type identity
      # and equality.
      # For bootstrap types this may not be bound yet, falling back to the name.
      __TypeId = This: _: resolve (This.getBoundName or This.getName);

      # Create a new instance of the type by providing at least all required field values.
      mk = This: mkInstance SU U This;

      # Create a new instance of the type by calling This's constructor
      # When This == Type, creates a new Type.
      # Consumes its first argument to avoid nullaries
      new = This: newInstance SU U This;

      # Create a new subType inheriting from This
      # TODO: Compose checkValue / overrideCheck if they appear in args
      subType = This: name: args:
        SU.newSubType This name args;

      # For a given This type, create a new template whose eventual bound type subtypes This.
      subTemplate = This: name: tvars_: bindingsToArgs_:
        SU.__newTemplate This This name tvars_ bindingsToArgs_;

      # Create a new template of a type accepting type parameters.
      # The parameters can be accessed via the _ argument surrounding the type specification.
      # Returns a function from {bindings} to a new bound type.
      # Only makes sense to do from Type.newTemplate
      # TODO: hide method on non-Type types.
      template =
        This: name: tvars: bindingsToArgs:
        assert assertMsg ((This.getName {}) == "Type") (indent.block ''
          template: This is not Type:
            ${indent.here (log.print This)}
        '');
        SU.newTemplate This name tvars bindingsToArgs;

      # Create a new template inheriting from a function of This plus any type variables.
      # TODO: Generic inheritance and tvar/tvarBinding merging.
      subTemplateOf =
        This: bindingsToSuper: name: tvars_: bindingsToArgs_:
        SU.__newTemplate This bindingsToSuper name tvars_ bindingsToArgs_;

      # Prefer show over toString here so that the methods will still print.
      __show = This: this:
        if isTypeSet this && isString (this.__TypeId {}) then
          (this.__TypeId {})
        else
          this;
    };

    # Type consists only of members of SU.
    # Descendence into subU takes places on U.Type.new, which references SU.* components
    # to call mkInstance using subU.Type__args, which are made up of U.* components,
    # producing a subU.Type made only of U components, and so on, such that U.Type
    # is always composed of SU.Type components for all U, SU.
    # The exception is templating, or types producing types - these refer to U only
    # in the method body, after Type is already constructed and bound.
    mkTypeArgsFor = U: SU: {
      __isTypeSet = true;
      name = SU.String.new (U.opts.typeName);
      Super = TypeThunk null;
      ctor = U.Ctors.CtorType;
      fields = This: mkTypeFieldsFor U SU;
      # We have these are both methods and staticMethods on Type s.t. we have
      # them present in bootstrap and in Type.new instances, which get these as
      # methods via Ctors.CtorType.
      methods = maybeNamedLazyAttrs "mkTypeArgsFor.methods" (typeMethodsFor U SU);
      staticMethods = maybeNamedLazyAttrs "mkTypeArgsFor.staticMethods" (typeMethodsFor U SU);
      tvars = LazyAttrs {};
      tvarBindings = LazyAttrs {};
      checkValue = null;
      overrideCheck = null;
    };

    # Check if a given argument is a custom Type.
    checkTyped = x:
      with (log.v 4).call "checkTyped" x ___;
      assert checks [
        {
          name = "isAttrs x";
          cond = isAttrs x;
          msg = "checkTyped: x is not an attrset";
        }
        {
          name = "x ? Type";
          cond = x ? Type;
          msg = "checkTyped: x has no Type field";
        }
        {
          name = "isTypeThunk x.Type";
          cond = (isAttrs x && x ? Type && (x.__TypeId {}) == "ThunkOf Type");
          msg = "checkTyped: x.Type is not a TypeThunk";
        }
      ];
      return true;

    isTyped = x: errors.tryBool (checkTyped x);

    # Check if a given argument is a Type.
    # 'isType' collides with lib.isType.
    isTypeSet = T: T.__isTypeSet or false;

    # Check if a given argument is a Type or a builtin type.
    isTypeLike = T:
      isTypeSet T || builtinNameCheck (T.value or T);

    # Gets the type of a value, whether it is a Type or a builtin type.
    getRawType = x:
      if isTyped x then resolve x.Type
      else typeOf x;

    # Gets the type of a value, whether it is a Type or a builtin type.
    getType = x:
      if isTyped x then resolve x.Type
      else Builtin.FromT (typeOf x);

    # Get the name of a type whether builtin or Type.
    # If a Type, just "Typed" to avoid recursion triggering.
    getTypeNameSafe = x:
      if x == null then "null"
      else if isTyped x then "Typed"
      else typeOf x;

    # Get the name of a type whether builtin or Type.
    getTypeName = x:
      if x == null
        then "null"
      else if isTyped x
        then x.Type.__do (T:
          (T.name or (throw ''
            Type is missing name in getTypeName:
            ${indent.here (log.print (resolve x.Type))}
          '')).value)
        else
          typeOf x;

    # Get the bound name of a type whether builtin or Type.
    getTypeBoundName = x:
      if isTyped x
      then x.Type.__do (T:
        (T.getBoundName or (throw ''
          Type is missing name in getTypeName:
          ${indent.here (log.print (resolve x.Type))}
        '')) {})
      else typeOf x;

    # Check two types for equality, supporting both Types and builtin type-name strings.
    typeEq = T: U:
      # Unpack type thunks
      if T ? __resolve then typeEq (resolve T) U
      else if U ? __resolve then typeEq T (resolve U)
      # Otherwise compare like with like
      else if isTypeSet T && !isTypeSet U then false
      else if !isTypeSet T && isTypeSet U then false
      else if isTypeSet T && isTypeSet U then (T.__TypeId {}) == (U.__TypeId {})
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
      with (log.v 3).call "mkFieldAssignmentValue" {inherit This fieldName uncastValue;} ___;

      with lets rec {
        fields = This.fields This;
        field = fields.getField fieldName;
        castRequired =
          # Field has a type
          (field.fieldType {}) != null
          && (
            # The value provided does not already have this type
            !(hasType (field.fieldType {}) uncastValue)
            # nor does it pass a check on the field type
            && !(isTypeSet (field.fieldType {}) && (field.fieldType {}).check uncastValue)
          );
        value =
          if castRequired
            then cast (field.fieldType {}) uncastValue
            else mkCastSuccess uncastValue "id";
      };

      assert (errors.predChecks field [
        { pred = (field: field != null);
          msg = joinLines [
            "Setting unknown field: ${This.name}.${fieldName}"
            "Known fields: ${joinSep ", " (map soloName (fields.instanceFields {}))}"
          ];
        }
        { pred = field: (!castRequired) || isCastSuccess value;
          msg = indent.block ''
            Error casting field assignment:

              ${if This ? __TypeId then This.__TypeId {} else "This"}.${fieldName} =
                ${indent.here (log.print uncastValue)}

              ${indent.here value.castError}
          '';
        }
        # TODO: This check could be O(n) for container types, updating attrsets goes O(1) to O(n)
        # Need item-wise checks
        { pred = field: (!castRequired)
                        || (isTypeSet (field.fieldType {}) && (field.fieldType {}).check value.castSuccess)
                        || (typeOf value.castSuccess == (field.fieldType {}));
          msg = indent.block ''
            Cast value did not pass typecheck:
              ${if This ? __TypeId then This.__TypeId {} else "This"}.${fieldName} = ${log.print uncastValue}
              Cast value of ${log.print (value.castSuccess or null)} is not a valid instance of ${log.print (field.fieldType {})}.
          '';
        }
      ]
    );
    return value.castSuccess;

    # Make a single assignment solo valid for merging into an instance by extracting
    # its value from args and casting if necessary, or setting a default if not present.
    # Unsupplied required fields will throw an error.
    #
    # This is the only mechanism through which an instance should have a field updated or
    # modified.
    # TODO:
    # - Enforce this via having fields be read-only via thunk such that updates are meaningless
    # - Fields-as-thunks would also mean we can lazily compose field updates and only force them
    #   as needed (could also only type-check on access enabling invalid intermediate field
    #   values that can never be seen)
    mkFieldAssignmentSoloFromUncastValue = This: fieldName: field: uncastValue:
      with (log.v 3).call "mkFieldAssignmentSoloFromUncastValue" {inherit This fieldName field uncastValue; } ___;
      with lets rec {
        castValue =
          if !((resolve field.Type).__TypeId {} == "Field") then with indent; throws.block ''
            Invalid field encountered in setFields:

              This = ${here (print This)}

              field = ${here (print field)}
          ''
          else mkFieldAssignmentValue This fieldName uncastValue;
      };
      return { ${fieldName} = castValue; };

    mkFieldAssignmentSoloFromArgs = This: args: fieldName: field:
      with (log.v 3).call "mkFieldAssignmentSoloFromArgs" {inherit This args fieldName field;} ___;
      with lets rec {
        defaultValue = resolve (field.fieldDefault or (with indent; throws.block ''
          Field ${fieldName} is not set in args and has no default value:
            This = ${here (print This)}
            args = ${here (print args)}
        ''));
        uncastValue = args.${fieldName} or (errors.try (resolve defaultValue) (_: throw ''
          Invalid non-Thunk fieldDefault for ${fieldName}: ${log.print defaultValue}''
        ));
      };
      return (
        mkFieldAssignmentSoloFromUncastValue
          This fieldName field uncastValue);

    # Make all field assignments on the instance
    mkFieldAssignments = This: args:
      with (log.v 2).call "mkFieldAssignments" This args ___;
      let fields = This.fields This; in
      return
        (concatMapSolos
          (fieldName: field: soloValue (mkFieldAssignmentSoloFromArgs This args fieldName field))  # soloValue as this produces {name = {name = value;};}
          (fields.instanceFieldsWithType {}));

    # Accessors to include on all instances.
    # Where these need binding, they are bound similarly to methods s.t. methods can call
    # accessors.
    mkAccessors = This:
      let
        fields = This.fields This;
      in rec {
          # Field checking interface.
          # e.g. this.has.someInt -> true
          #      this.has ? someInt -> true
          #      this.has.notAField -> throws error
          #      this.has ? notAField -> false
          #      this.has.notAField or default -> default
          has = mergeAttrsList (mapSolos (_: _: _: true) (fields.instanceFields {}));

          # Field getting interface
          # e.g. this.get.someInt {} -> 123
          get = mergeAttrsList (mapSolos (fieldName: _: this: thunk this.${fieldName}) (fields.instanceFields {}));

          # Field setting interface
          # e.g. this.set.someInt 123 -> this'
          #      this.set.someInt "123" -> throws Type error
          set =
            mergeAttrsList
              (mapSolos
                (fieldName: field:
                  this:
                    # Reinit here inside the thunk to avoid constant reinit while setting by name
                    # during construction.
                    uncastValue:
                      let assignment = mkFieldAssignmentSoloFromUncastValue This fieldName field uncastValue;
                      in bindThis This (this // assignment))
                (fields.instanceFields {}));

          # Field modification interface
          # e.g. this.modify.someInt (x: x+1) -> this'
          #      this.modify.someInt toString -> throws Type error
          modify =
            mergeAttrsList
              (mapSolos
                (fieldName: _:
                  this: f:
                    this.set.${fieldName} (f this.${fieldName}))
                (fields.instanceFields {}));
        };

    checkNoNullaryBindings = check: This: bindings:
      let
        nullaryBindings = filterAttrs (_: binding: !(isFunction binding)) bindings;
      in
      check
        "No nullary bindings"
        (empty nullaryBindings)
        (indent.block (''
          Nullary bindings encountered when binding 'this' of ${log.print This}:
            ${indent.here (log.vprintD 2 nullaryBindings)}
          ''));

    mkStaticMethodBindings = This: staticMethods: this_:
      with (log.v 3).call "mkStaticMethodBindings" This staticMethods "this_" ___;
      let
        bindings =
          mapAttrs
            (methodName: staticMethod: staticMethod this_)
            (maybeResolve staticMethods);
      in
        assert checkNoNullaryBindings check This bindings;
        return bindings;

    mkMethodBindings = This: this_:
      with (log.v 3).call "mkMethodBindings" This "this_" ___;
      let
         bindings =
           mapAttrs
             (methodName: method: method this_)
             (maybeResolve (This.methods or {}));
      in
        assert checkNoNullaryBindings check This bindings;
        with safety true;
        return bindings;

    mkAccessorBindings = This: this_:
      with (log.v 3).call "mkAccessorBindings" This "this_" ___;
      let
        bindings =
          mapAttrs
            (_: mapAttrs (_: accessor: accessor this_))
            (mkAccessors This);
      in
        # No nullary check on accessors.
        return bindings;

    # Initialise a type from its This type, its partial this set, and any args.
    mkthis = This: args:
      with (log.v 2).call "mkthis" This args ___;
      let
        this = mkFieldAssignments This args;
      in
        # with safety true;
        # return (bindThis This this);
        bindThis This this;

    # Bind members that refer to this on construction and after any change.
    bindThis = This: this:
      let
        # If we are creating a Type, we need to bind its static methods too
        # When creating Type via Type.new: binds 'new' to the new Type
        # When creating Bool via Type.new: binds 'new' to Bool
        # When creating bool via Bool.new: this.Type.staticMethods == Bool.staticMethods == {}
        staticMethodInstanceBindings = mkStaticMethodBindings This (resolve this.Type).staticMethods this_;
        # When creating Type via Type.new: binds 'new' to the new Type
        # When creating Bool via Type.new: this.staticMethods == Bool.staticMethods == {}
        # When creating bool via Bool.new: this ? staticMethods == false
        staticMethodBindings =
          if this ? staticMethods
            then mkStaticMethodBindings This this.staticMethods this_
            else {};
        accessorBindings = mkAccessorBindings This this_;
        methodBindings = mkMethodBindings This this_;
        this_ = mergeAttrsList [
          this
          staticMethodInstanceBindings
          staticMethodBindings
          accessorBindings
          methodBindings
        ];
      in
        this_;

    # Create a new instance of a type by calling its constructor.
    # The constructor's output arguments are then passed into mkInstance.
    # For types, the constructor just merges a name parameter with an arguments
    # parameter and delegates to mkInstance.
    newInstance = SU: U: This:
      with (log.v 2).call "newInstance" SU U "unsafe:This" ___;
      # let boundCtor = This.ctor.bind This; in
      # if isFunction boundCtor then Variadic.compose (mkInstance This) boundCtor
      # else mkInstance This boundCtor;
      # assert
      #   check
      #     "(This.ctor.bind This) is non-nullary"
      #     (isFunction boundCtor)
      #     (indent.block ''Nullary ctor encountered when creating new instance of ${log.print This}'');
      # let mkViaCtor = traceComposeVariadic "(mkInstance This)" "(arg: This.ctor.bind This arg)"
      #                                       (mkInstance This)   (arg: This.ctor.bind This arg); in
      # mkViaCtor;
      # assert (isFunction boundCtor);
      Variadic.compose (mkInstance This) (arg: This.ctor.bind This arg);

    # Build a new instance from a single dict of field values.
    # Construct an instance from a This type and attrset of field-to-value assignments.
    # arg = { fieldName = value, ... } for all fields.
    # Fields with defaults can be omitted.
    # Build a new instance from a single dict of field values.
    mkInstance = SU: U: This: args_:
      with (log.v 2).call "mkInstance" SU U This args_ ___;
      let
        # All instances have a common Type field.
        args = args_ // { Type = (SU.ThunkOf Type).new This; };

        # Construct 'this' as an instance of 'This'.
        this = assign "this" (mkthis This args);

        # Check the validity of the constructed instance.
        validityChecks =
          let
            fields = This.fields This;

            suppliedFieldNames =
              assign "suppliedFieldNames" (
                attrNames args);

            fieldNames =
              assign "fieldNames" (
                map soloName (fields.instanceFieldsWithType {}));

            populatedFieldNames =
              assign "populatedFieldNames" (
                filter (name: this ? ${name}) fieldNames);

            requiredFieldNames =
              assign "requiredFieldNames" (
                map soloName (fields.requiredFields {}));

            # Get any supplied non-static fields not present in this or any supertype.
            unknownFieldNames = assign "unknownFieldNames" (
              subtractLists fieldNames populatedFieldNames);

            # Get any fields not populated in this or any supertype.
            missingFieldNames = assign "missingFieldNames" (
              subtractLists populatedFieldNames requiredFieldNames);

            staticMethodNames =
              assign "staticMethodNames" (
                This.staticMethods.__attrNames {});

            populatedStaticMethodNames =
              assign "populatedStaticMethodNames" (
                filter (name: this ? ${name}) (This.staticMethods.__attrNames {}));

            missingStaticMethodNames = assign "missingStaticMethodNames" (
              subtractLists populatedStaticMethodNames staticMethodNames);
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
            {
              cond = missingStaticMethodNames == [];
              msg = ''
                ${log.print This}: Missing staticMethods in mkInstance call: ${joinSep ", " missingStaticMethodNames}
                args: ${log.print args}
                This: ${log.printAttrs This}
                this: ${log.print this}
              '';
            }
          ];
      in
        assert checks validityChecks;
        return this;

    # Inline assertion for runtime type assertion.
    must = T: x:
      if hasType T x then x
      else throw "Expected type ${T} (got ${getTypeName x})";

    MkBuiltinFns = {
      TypeShims = U: SU: name:
        let
          BuiltinTypeShim = mkTypeShim name {
            new = x: mkInstanceShim U SU BuiltinTypeShim { value = x; };
            mk = args: mkInstanceShim U SU BuiltinTypeShim args;
            fields = _: U.Fields.new { value = (toLower name); };
          };
        in
          BuiltinTypeShim;
      Types = U: SU: name:
        let
          hasSize = { String = true; Path = true; List = true; Set = true; }.${name} or false;
          hasToString = { String = true; }.${name} or false;
          withToString = methods:
            if hasToString
            then methods // {
              __toString = this: self: indent.block ''${name}:"${indent.here (toString self.value)}"'';
            }
            else methods;
          withSize = methods:
            if hasSize
            then let sizeFn = this: _: size this.value;
                in methods // { size = sizeFn; }
            else methods;
        in
          U.Type.new name {
            fields = This: SU.Fields.new [{ value = toLower name; }];
            methods = withToString (withSize ({
              List = {
                fmap = this: f: this.modify.value (map f);
                append = this: x: this.modify.value (xs: xs ++ [x]);
              };
              Set = {
                fmap = this: f: this.modify.value (mapAttrs (_: f));
                names = this: _: attrNames this.value;
                values = this: _: attrValues this.value;
                # e.g. this.modifyAt.name (x: x+1)
                getAt = this: _: this.value;
                modifyAt = this: _: mapAttrs (k: v: f: this.modify.value (xs: xs // { ${k} = f v; })) this.value;
                setAt = this: _: mapAttrs (k: _: x: this.modify.value (xs: xs // { ${k} = x; })) this.value;
              };
              Lambda = {
                fmap = this: f: this.modify.value (compose f);
              };
            }.${name} or {}));
            checkValue = that: {
              # Additional check on sets s.t. we don't accept a typed value when expecting
              # a raw set.
              Set = !(that ? Type);
            }.${name} or true;
          };
    };

    # Constructed such that for a universe U, all types should only need to access U.Ctors.
    mkCtors = U: SU: with U; rec {
      Ctor =
        if opts.level == 0
        then mkTypeShim "Ctor" {
          new = name: ctor: mkInstanceShim U SU Ctor {
            name = { value = name; };
            inherit ctor;
            bind = This: ctor This;
          };
        }
        else SU.Type.new "Ctor" {
          fields = This: SU.Fields.new [
            { name = SU.String; }
            { ctor = "lambda"; }
          ];
          methods = {
            bind = this: This: this.ctor This;
          };
        };

      Ctors = rec {
        None = Ctor.new "None" (This: _: throw ''Ctors.None evoked'');

        # Explicit nullary constructor s.t. X.new == X.mk {}
        # Still needs a thunk arg otherwise it will evaluate
        CtorNullary = Ctor.new "CtorNullary" (This: _: {});

        # Default constructor for regular non-NewType/Builtin/Alias types.
        CtorFields = Ctor.new "CtorFields" (This:
          with (log.v 3).call "CtorFields.ctor" { inherit This; } ___;
          let
            fields = assign "fields" (This.fields This);
            sortedFieldNames = assign "sortedFieldNames" (map soloName (fields.instanceFields {}));
          in
          assert check
            "Nonempty fields Ctors.CtorFields"
            (nonEmpty (fields.instanceFields {}))
            ''At least one required field must be set for CtorFields to be used'';
          return (Variadic.mkOrdered sortedFieldNames)
        );

        # The constructor for Type in U and its precursors / descendent Type types
        # in subuniverses of U.
        # TODO: Defaults should not need restating in U_2+
        CtorType = Ctor.new "CtorType" (This: name: args: {
          __isTypeSet = true;
          name = SU.String.new name;
          Super = setThunkName "Super[Type.ctor]" (args.Super or (TypeThunk null));
          # Whatever ctor is given, for universes that don't have access to Field's defaults,
          # we need to ensure the end result contains values for all Type args.
          ctor = args.ctor or CtorFields;
          fields =
            let fields_ = args.fields or (This: SU.Fields.new []);
            in assert assertMsg (isFunction fields_) (indent.block ''
                 Non-function Fields provided:
                   fields = ${indent.here (log.print fields_)}
                   This = ${indent.here (log.print This)}
               '');
               fields_;
          methods = maybeLazyAttrs (args.methods or {});
          staticMethods = maybeLazyAttrs (args.staticMethods or {});
          tvars = maybeLazyAttrs (args.tvars or {});
          tvarBindings = maybeLazyAttrs (args.tvarBindings or {});
          checkValue = args.checkValue or null;
          overrideCheck = args.overrideCheck or null;
        });
      };
    };

    # Construct the Builtin type wrappers for universe U using the given mkBuiltin function.
    mkBuiltins = U: SU:
      let BuiltinTypes =
            mergeAttrsList
              (map (name: { ${name} = U.opts.mkBuiltin U SU name; }) BuiltinNames);
      in BuiltinTypes // {
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
            let T = (with BuiltinTypes; {
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
      with (log.v 4).call "parseFieldSpec" spec ___;

      # Unwrap Static types.
      # Duck-typed to support bootstrap.
      if spec ? staticType
      then (parseFieldSpec (spec.staticType {})) // {
        fieldStatic = true;
      }

      # Unwrap Default types.
      # Duck-typed to support bootstrap.
      else if (spec ? defaultType) && (spec ? defaultValue)
      then (parseFieldSpec (spec.defaultType {})) // {
        fieldDefault = NamedThunk "fieldDefault" (spec.defaultValue {});
      }

      # Return unwrapped types.
      else
        assert check "isTypeLike spec" (spec == null || isTypeLike spec)
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
      unit = Unit.new {};

      # Uninhabited type
      Void = U.Type.new "Void" {
        ctor = U.Ctor.new "CtorVoid" (_: thunk (throw "Void: ctor"));
      };

      # Any type
      # Used in Type fields (via Literal binding) so must be SU.Type.
      Any = U.Type.new "Any" {overrideCheck = _: true;};
    };

    # Construct templating functions for a given universe.
    # For Universe U, This == U.Type when these are bound during bootstrap, and for e.g.
    # components, This == U.Fields when they are constructed via U.Type.new.
    #
    # For __newTemplate and variants, a constraint is that we haven't bound methods on
    # our TypeShims such that they can be subtyped, and so we always use U.Thing.subType,
    # U.Thing.subTemplate
    #
    # The special case here is newSubType/newSubTemplate - U.Fields is a U.Type, not a SU.Type, and so
    # when we call U.Fields.subType, we want this to defer to U.Type.newSubType, not
    # SU.Type.newSubType. The bound methods of U.Type are the only place where we can therefore refer to other parts
    # of U (lazily, after Type is properly built), as e.g. U.newSubType.
    mkTemplating = U: SU: rec {

      # Modify an argument set to inherit from a Super type, given as a TypeThunk.
      # Inheritance is performed as follows:
      # - Fields:
      #   - Any fields present on Super and not redefined in args.fields are inherited.
      #   - Any fields present on Super and redefined in args.fields are overridden.
      #     These fields retain their original positions in the field list, if fields were
      #     specified as a list of solos.
      #   - Any fields only present in args.fields are appended to the end of the combined field list.
      # - Methods / Static Methods:
      #   - Any methods present on Super and not redefined in args.methods are inherited.
      #     These methods do not inherit any Super binding and are bound instead to this/This (methods/staticMethods)
      #   - Any methods present on Super and redefined in args.methods are overridden and bound to this/This.
      #   - Any methods only present in args.methods are merged into the method attrset.
      # - Ctor:
      #   - If args.ctor is set, it is used as the ctor, overriding any ctor in Super.
      #   - If args.ctor is unset, then the Super.ctor is inherited.
      #     If args adds any new fields, or changes the types of existing fields, it is likely that
      #     this inherited ctor will not produce valid argument sets for consumption by mkInstance.
      #     One can manually call the super ctor when constructing a new ctor by:
      #     (This.Super.do (T: T.ctor.bind This)) args
      inheritFrom = SuperThunk: ctorArgs:
        with (log.v 3).call "inheritFrom" SuperThunk ctorArgs ___;
        assert check "isThunk SuperThunk" (isThunk SuperThunk) "inheritFrom: SuperThunk must be a Thunk, got ${log.print SuperThunk}";
        assert check "!(SuperThunk.__do isNull)" (!(SuperThunk.__do isNull)) "inheritFrom: Super must not be null, ${log.print (resolve SuperThunk)}";

        return (ctorArgs // {
          # Store Super as a Thunk directly.
          Super = SuperThunk;

          # Override or inherit the ctor
          ctor = ctorArgs.ctor or (SuperThunk.__do (T: T.ctor));

          # Merge fields inside the Super thunk into a new This: ... form
          # The Fields.update method handles the solo-list duplicate merge.
          fields = assign "fields" (This:
            let superFields = SuperThunk.__do (Super: Super.fields This);
            in if ctorArgs ? fields
               then let ctorFields = assign "ctorFields" (ctorArgs.fields This);
                        ctorFieldSolos = assign "ctorFieldSolos" (ctorFields.getSolos {});
                    in superFields.update ctorFieldSolos
               else superFields);

          # Merge methods with those inside the Super thunk.
          methods =
            let
              ctorMethods = maybeResolve (ctorArgs.methods or {});
              superMethods = SuperThunk.__do (Super: resolve Super.methods);
            in LazyAttrs (superMethods // ctorMethods);

          # Merge static methods with those inside the Super thunk.
          staticMethods =
            let
              ctorStaticMethods = maybeResolve (ctorArgs.staticMethods or {});
              superStaticMethods = SuperThunk.__do (Super: resolve Super.staticMethods);
            in LazyAttrs (superStaticMethods // ctorStaticMethods);
        });

      # For a given type, create a new type in the same universe.
      # Inheritance is performed as per inheritFrom.
      newSubType = This: name: args:
        with (log.v 2).call "newSubType" { inherit This name args; } ___;
        if !(isTypeSet This)
          then throw (indent.block ''
            Cannot subtype non-Type or Type-precursor:
              ${log.print This}
          '')
          else
            return (U.Type.new name (inheritFrom (TypeThunk This) args));

      # Create a new template of a type accepting type parameters.
      # The parameters can be accessed via the _ argument surrounding the type specification.
      # Returns a function from {bindings} to a new bound type.
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
      __newTemplate =
        This: bindingsToSuperOrSuper: name: tvars_: bindingsToArgs_:
        let
          # Convert the given tvars into a SetOf Constraints
          # This can use U.Constraint, because we have U.Type
          # U.Constraint uses SU.Fields, which subtype an (SU.OrderedOf_ SU.Field) bound template.
          # That bound template uses SU.Constraint.
          tvars = mapAttrs (_: T: SU.Constraint.new (TypeThunk T)) tvars_;

          # Convert the given (_: {...}) type template definition into one that
          # explicitly extends args with tvars and tvarBindings
          bindingsToArgs = tvarBindings:
            let args = bindingsToArgs_ tvarBindings;
            in args // {
              # Set the tvars to exactly those given.
              # Thunk'd in the ctor
              inherit tvars tvarBindings;
            };
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
              __newTemplate: bindingsToSuperOrSuper must be a Type or function returning a Type, got:
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
        __newTemplate This null name tvars bindingsToArgs;
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
      let Type__grounded = Type // { Type = (ThunkOf Type).new Type__grounded; };
      in Type__grounded;

    # Cause a Type to have itself as its own Type, eliding any information about
    # either bootstrap pseudo-types or the Type of its superuniverse.
    # Also asserts that recreating the grounded Type using itself, via Type.new,
    # creates an identical Type (modulo lambda equality).
    groundTypeAndAssertFixed = opts: Type__args: Type:
      let Type__grounded = groundType Type;
      in assert assertTypeFixedUnderNew Type__grounded opts.typeName Type__args;
        Type__grounded;

    # Add the common core to the U universe.
    # U_ must have 'opts' already set.
    withCommonUniverse = SU: U_:
      let
        opts = U.opts;
        U = foldl'
          (U: f: U // f U SU)
          U_
          [
            (_: _: {
              __toString = _: "<Universe: ${opts.name}>";
            })
            (U: SU: mkUniverseReferences opts U SU)
            (U: SU: mkCtors U SU)
            (U: SU: mkBuiltins U SU)
            (U: SU: mkTemplating U SU)
            (U: SU: mkTrivialTypes U SU)
          ];
      in
        U;
    withCommonUniverseSelf = U_:
      let U = withCommonUniverse U U_;
      in U;

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
    # Options for the next universe can be produced via 'resolve opts.descend'.
    mkUniverseOpts = level: rec {
      inherit level;
      name = "U_${toString level}";
      # Type is named identically at all levels.
      typeName = "Type";
      # TODO: not needed with shims being good 
      # During the bootstrap, do not enable typechecking.
      # All fields have type 'null' and do not make use of builtin types
      # or Default/Static, which are not yet defined.
      enableTypeChecking = level >= 0;
      # During the bootstrap, do not enable Default/Static structure.
      # When this is retained, if type checking is enabled, the entire field type
      # is kept e.g. Static (Default Int 123)
      # If type checking is disabled in this level, the Static/Default structure is retained
      # but the type is nulled out e.g. Static (Default null 123)
      retainTypeFieldSpec = level >= 0;
      # We only expect Type to be fixed under Type.new "Type" when it contains and produces
      # no shim elements of the Quasiverse.
      # U_0 is entirely shim elements and a Type made of shims.
      # U_1 has a Type made from U_0's Type.new and is made of shim elements.
      # U_2 has a Type made from U_1's Type.new and is made of untyped U_1 elements.
      # U_3 has a Type made from U_2's Type.new and is made of typed U_2 elements.
      # Type in U3+ should be fixed under .new, made entirely of final typed elements and producing
      # types themselves made of final typed elements.
      checkTypeFixedUnderNew = level >= 3;
      # Only construct Builtin shims in the Quasiverse; otherwise real Builtin types.
      mkBuiltin = if level == 0 then MkBuiltinFns.TypeShims else MkBuiltinFns.Types;
      # A thunk returning the options for the next-lower subuniverse.
      descend = NamedThunk "${name}.descend" (mkUniverseOpts (level + 1));
    };

    # Create shim types appearing as instances of Type.
    mkTypeShim = name: attrs:
      attrs // {
        # Fields
        __isTypeSet = true;
        name = { value = name; };
        tvars = LazyAttrs {};
        check = _: true;
        # Methods
        __TypeId = thunk name;
        getBoundName = thunk name;
        getName = thunk name;
        __toString = _: "TypeShim<${name}>";
      };

    mkInstanceShim = U: SU: shimT: attrs:
      attrs // {
        Type = (SU.ThunkOf SU.Type).new shimT;
      };

    mkBootstrappedType = U: SU:
      with (log.v 3).call "mkBootstrappedType" U SU ___;
      let
        opts = U.opts;
      in rec {
        # Create the arguments to mkInstance for creating a Type.
        Type__args = assign "Type__args" (
          mkTypeArgsFor U SU);

        # Treat the arguments as an instantiated Type, despite the lack of bound methods.
        # This is sufficient for mkInstance. We only need Type to be set in addition,
        # which we can achieve via groundTypeUnsafe (unsafe since we need a TypeThunk and
        # Type__args is not itself a Type).
        Type__argsGrounded = assign "Type__argsGrounded" (
          groundType Type__args);

        # We can then bootstrap a new Type by running mkInstance with This as Type__argsGrounded
        # and args as Type__args.
        Type__bootstrapped = assign "Type__bootstrapped" (
          mkInstance Type__argsGrounded Type__args
        );

        # Construct Type through an actual application of the .mk constructor.
        # This is valid besides Type__mk.(Type {}).(Type {}).Type {} == Type__args, and after that,
        # Type__args ? Type == false.
        # We fix this in the next stage.
        Type__new =
          assert checks [{ name = "Type__bootstrapped has mk";
                         cond = Type__bootstrapped ? mk;
                         msg = "Type__bootstrapped must have a mk function";
                       }];
          assign "Type__new" (
            Type__bootstrapped.mk Type__args
          );

        # Finally, ground this Type by setting Type.Type to return itself, eliding any information
        # about the bootstrap types.
        Type =
          assert checks [{ name = "Type__new has new";
                         cond = Type__new ? new;
                         msg = "Type__new must have a new function";
                       }];
          assign "Type" (
            # In U_3 and beyond, this should now have reached a fixed point in terms
            # of further bootstrapping by Type.new, modulo lambda equality on created Type instances.
            # If enabled in the opts, an assertion checks that Type is fixed under further bootstrapping.
            if opts.checkTypeFixedUnderNew
              then groundTypeAndAssertFixed opts Type__args Type__new
              else groundType Type__new
          );
      };

    # The barest minimum universe to bootstrap the type system.
    # Constructs a bootstrapped Quasitype from a hand-build GroundType instance, which has no
    # typed fields.
    Quasiverse =
      with (log.v 4).attrs "Quasiverse" ___;
      with msg "Constructing Quasiverse";

      # Quasiverse is its own self- and super-universe, enabled by containing no circular dependencies
      # in quasitype construction.
      let U = Quasiverse;
          SU = Quasiverse;
      in withCommonUniverseSelf rec {
        # Take on the intial options for a root universe.
        # All other opts are generated from here via 'resolve opts.descend'
        opts = mkUniverseOpts 0;

        __Bootstrap = mkBootstrappedType U SU;

        # Expose the final Type.
        Type = __Bootstrap.Type;

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
            in mkInstanceShim U SU Field (get // {
              inherit get;
              inherit fieldName;
              fieldDefault = _: parsedSpec.fieldDefault;
              fieldType = _: parsedSpec.fieldType;
              fieldStatic = _: parsedSpec.fieldStatic;
              getSolo = _: { ${fieldName} = get; };
              hasDefault = _: parsedSpec.fieldDefault != null;
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
              in mkInstanceShim U SU Fields (rec {
                getSolos = _: soloFields;
                indexed = mergeAttrsList (cutils.attrs.indexed soloFields);
                update = newFieldSpecs:
                  Fields.new (concatSolos (solos fieldSpecs) (solos newFieldSpecs));
                getField = name: indexed.${name}.value;
                getFieldsWhere = pred: filterSolos pred (getSolos {});
                instanceFields = _:
                  getFieldsWhere (fieldName: field:
                    fieldName != "Type"
                    && (field == null
                        || !(field.fieldStatic {})));
                instanceFieldsWithType = _:
                  getFieldsWhere (fieldName: field:
                    field == null
                    || !(field.fieldStatic {}));
                requiredFields = _:
                  getFieldsWhere (_: field:
                    field == null
                    || (!(field.fieldStatic {})
                        && !(field.hasDefault {})));
              });
          };

        # Cannot use mkInstanceShim
        ThunkOf = T: mkTypeShim "ThunkOf" {
          new = x: {
            Type = { __resolve = U.Type; };
            value = NamedThunk "ThunkOfShim" x;
            __resolve = _: resolve value;
          };
        };
        SetOf = T: mkTypeShim "SetOf" { new = U.Set.new; };
        ListOf = T: mkTypeShim "ListOf" { new = U.List.new; };
        Constraint = mkTypeShim "Constraint" { new = x: {value = _: x; satisfiedBy = _: true;};};
        Static = T: mkTypeShim "Static" { staticType = _: T; };
        Default = T: V: mkTypeShim "Default" { defaultType = _: T; defaultValue = _: V; };
        NullOr = T: mkTypeShim "NullOr" { new = id; };
        Literal = V: mkTypeShim "Literal" { getLiteral = _: V; new = _: { getLiteral = _: V; }; };
        Sized = n: T: mkTypeShim "Sized" {
          new = x: {
            getSized = T.new x;
          };
        };
        Any = mkTypeShim "Any" {};
        OrderedItem = T: mkTypeShim "OrderedItem" {
          new = x: mkInstanceShim U SU (OrderedItem T) (rec {
            value = (Sized 1 (SetOf T)).new x;
            getSolo = _: value.getSized.value;
            getName = _: soloName (getSolo {});
            getValue = _: soloValue (getSolo {});
          });
        };
      };

    # Create a new universe descending from SU.
    mkSubUniverse = SU:
      let opts = resolve SU.opts.descend; in
      with (log.v 4).call "mkSubUniverse" opts "<SU>" ___;
      with msg "Constructing ${opts.name} universe";
      let U = withCommonUniverse SU rec {
        # Store the opts as U.opts
        inherit opts;

        # Construct the Type type in terms of the SU, then in terms of itself.
        #__Bootstrap = rec {
        #  Type__args = mkTypeArgsFor U SU;
        #  Type__new = SU.Type.new opts.typeName Type__args;
        #  Type =
        #    if opts.checkTypeFixedUnderNew
        #      then groundTypeAndAssertFixed opts Type__args Type__new
        #      else groundType Type__new;
        #};
        __Bootstrap = mkBootstrappedType U SU;
        Type = __Bootstrap.Type;

        # A constraint on a type variable.
        Constraint = U.Type.new "Constraint" {
          fields = This: SU.Fields.new [
            { constraintType = "set"; }  # TypeThunk
          ];
          methods = {
            # Whether a given type variable binding satisfies the constraint.
            # If the constraint is unbound, we treat as satisfied, but instantiating the unbound type
            # will throw an error.
            satisfiedBy = this: That:
              typeEq U.Void That
              || That.isInstance (resolve this.constraintType)
              || (resolve this.constraintType).check That;
          };
        };

        ThunkOf_ = U.Lambda.subTemplate "ThunkOf" {T = SU.Type;} (_: {
          ctor = U.Ctor.new "CtorThunkOf" (This: x: {
            value = NamedThunk (log.print This) x;
          });
          methods = {
            __resolve = this: _: resolve this.value;
          };
          checkValue = that: isTypeSet that && hasType _.T (that.resolve {});
        });
        ThunkOf = T: U.ThunkOf_.bind { inherit T; };

        # Subtype of List that enforces homogeneity of element types.
        ListOf_ = U.List.subTemplate "ListOf" {T = SU.Type;} (_: {
          checkValue = that: all (x: hasType _.T x) that.value;
        });
        ListOf = T: U.ListOf_.bind { inherit T; };

        # Subtype of Set that enforces homogeneity of value types.
        SetOf_ = U.Set.subTemplate "SetOf" {T = Type;} (_: {
          checkValue = that: all (x: hasType _.T x) (that.values {});
        });
        SetOf = T: U.SetOf_.bind { inherit T; };

        # A type that enforces a size on the value.
        Sized_ = Type.template "Sized" {N = Type; T = Type;} (_: {
          fields = This: SU.Fields.new [{ getSized = _.T; }];
          checkValue = that:
            (_.T.checkValue or (const true)) that
            && (that.size {}) == _.N.getLiteral {};
        });
        Sized = n: T:
          let N = U.Literal n;
          in U.Sized_.bind { inherit N T; };

        # A type satisfied by any value of the given list of types.
        Union_ = Type.template "Union" {Ts = Type;} (_: {
          ctor = U.Ctors.None;
          overrideCheck = that: any (T: hasType T that) (_.Ts.getLiteral {});
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
          ctor = U.Ctor.new "CtorOrderedItem" (This: x: {
            value =
              let setOfX = ((SU.SetOf _.T).new x);
                  sizedSetOfX = (SU.Sized 1 (SU.SetOf _.T)).new setOfX;
              in sizedSetOfX;
          });

          fields = This: SU.Fields.new [{
            value = SU.Sized 1 (SU.SetOf _.T);
          }];

          methods = {
            getSolo = this: _: this.value.getSized.value;
            getName = this: _: soloName (this.getSolo {});
            getValue = this: _: soloValue (this.getSolo {});
          };
        });
        OrderedItem = T: OrderedItem_.bind { inherit T; };

        OrderedOf_ = Type.subTemplateOf (_: U.ListOf (U.OrderedItem _.T)) "OrderedOf" {T = Type;} (_: {
          # Pass OrderedItems to the underlying ListOf
          ctor = U.Ctor.new "CtorOrderedOf" (This: xs: {
            value = map (x: (SU.OrderedItem _.T).new x) (solos xs);
          });

          methods = {
            # Get the solo attr list in order.
            getSolos = this: _: this.mapItems (item: item.getSolo {});

            # The unordered merged attribute set
            unindexed = this: _: mergeAttrsList (this.getSolos {});

            # The merged attribute set with an additional 'index' field indicating
            # its place in the order.
            indexed = this: _: mergeAttrsList (indexed (this.getSolos {}));

            # The ordered attribute names.
            names = this: _: this.mapItems (item: item.getName {});

            # The ordered attribute values.
            values = this: _: this.mapItems (item: item.getValue {});

            # A set from name to index.
            indexes = this: _: this.imapItems (i: item: { ${item.getName {}} = i; });

            # Map over the underlying [OrderedItem T]
            # f :: (OrderedItem T -> a) -> [a]
            mapItems = this: f: map f this.value;

            # Map over the underlying [OrderedItem T] with index.
            # f :: (int -> OrderedItem T -> a) -> [a]
            imapItems = this: f: imap0 f this.value;

            # Map over the underlying [solo T] in order
            # f :: (string -> T -> a) -> [a]
            mapSolos = this: f: this.mapItems (item: f (item.getName {}) (item.getValue {}));

            # Map over the underlying indexed [solo T]
            # f :: (int -> string -> T -> a) -> [a]
            imapSolos = this: f: this.imapItems (index: item: f index (item.getName {}) (item.getValue {}));

            # Modify the value at the given key via this.modifyAt {}.name f, preserving order and type.
            modifyAt = this: _:
              mapAttrs
                (name: _: f:
                  this.fmap
                    (item:
                      let maybeModifyHere = (item.modifyAt {}).${name} or (const item);
                      in maybeModifyHere f))
                (this.unindexed {});

            # Set the value at the given key via this.setAt {}.name value, preserving order and type.
            # Key must exist in the Ordered (cannot create new keys).
            setAt = this: _: mapAttrs (name: modifyHere: x: modifyHere (const x)) (this.modifyAt {});

            # Update by inserting the given items sequentially at the end of the order.
            # If any already appear, they update the item in its original order.
            update = this: items_:
              let items = solos items;
              in Fields.new (concatSolos (this.getSolos {}) items);
          };

          checkValue = that:
            Super.checkValue that
            && assertMsg
                (size (that.names {}) == size (that.unindexed {}))
                "Duplicate keys in OrderedOf: ${joinSep ", " (that.names {})}";
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
                  {i = U.Int;}
                  {name = U.String;}
                ];
                staticMethods = {
                  # Enum members live on the Enum type itself.
                  __items = This: _: zipListsWith Item.new (range 0 (length itemNames - 1)) names;
                  __indexToItem = This: _: keyByF (item: toString item.i) (This.__items {});
                  __nameToItem = This: _: keyByName (This.__items {});
                  fromIndex = This: i: (This.__indexToItem {}).${toString i} or throw "Invalid index in enum ${enumName}: ${toString i}";
                  fromName = This: name: (This.__nameToItem {}).${name} or throw "Invalid name in enum ${enumName}: ${name}";
                };
              };
          in Item;

        # A type inhabited by only one value.
        Literal_ = Type.template "Literal" {V = U.Any;} (_: rec {
          ctor = U.Ctors.CtorNullary;
          staticMethods = {
            getLiteral = This: thunk _.V;
          };
        });
        Literal = V: U.Literal_.bind { inherit V; };
        literal = v: (U.Literal v).new {};

        # A type inhabited by literals of any of the given list of values
        Literals = Vs: U.Union (map Literal values);

        # A type indicating a default value.
        Default_ = Type.template "Default" {T = Type; V = Type;} (_: {
          ctor = U.Ctors.None;
          staticMethods.defaultType = This: thunk _.T;
          staticMethods.defaultValue = This: thunk (_.V.getLiteral {});
          overrideCheck = that: _.T == null || _.T.check that;
        });
        Default = T: v:
          let V = U.Literal v;
          in U.Default_.bind { inherit T V; };

        # Newtype wrapper
        Static_ = Type.template "Static" {T = Type;} (_: {
          ctor = U.Ctors.None;
          staticMethods.staticType = This: thunk _.T;
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
            {fieldName = SU.String;}
            {fieldSpec = U.Union [ U.Null Type U.String ];}
          ];
          methods = {
            parsedT = this: _: parseFieldSpec this.fieldSpec;
            fieldType = this: _: (this.parsedT {}).fieldType;
            fieldStatic = this: _: (this.parsedT {}).fieldStatic;
            hasDefault = this: _: (this.fieldDefault {}) != null;
            fieldDefault = this: _: (this.parsedT {}).fieldDefault;
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
          ctor = U.Ctor.new "CtorFields" (This: fieldSpecs_:
            let
              # Include Type field on all instances.
              fieldSpecs = withCommonFieldSpecs opts SU fieldSpecs_;
              soloFields = mapSolos U.Field.new fieldSpecs;
              # Call the OrderedOf constructor to construct this subType.
              args = This.Super.__do (Super: Super.ctor.bind This soloFields);
            in
              args
          );
          methods = {
            getIndexedField = this: name: (this.indexed {}).${name};
            getField = this: name: (this.getIndexedField name).value;
            getFieldsWhere = this: pred: filterSolos pred (this.getSolos {});
            instanceFields = this: _: this.getFieldsWhere (fieldName: field:
              fieldName != "Type"
              && (field == null
                  || !(field.fieldStatic {})));
            instanceFieldsWithType = this: _: this.getFieldsWhere (fieldName: field:
              field == null
              || !(field.fieldStatic {}));
            requiredFields = this: _: this.getFieldsWhere (_: field:
              field == null
              || (!(field.fieldStatic {})
                  && !(field.hasDefault {})));
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

      TestTypes = U: with U; {
        MyType =
          assert Type ? new;
          assert Fields ? new;
          Type.new "MyType" {
            fields = This: Fields.new [{ myField = String; }];
            methods = {
              helloMyField = this: extra: "Hello, ${this.myField.value}${extra}";
            };
          };

        MyType2 =
          assert Type ? new;
          assert Fields ? new;
          Type.new "MyType2" {
            fields = This: Fields.new [
              { stringField = String; }
              { intField = Int; }
              { defaultIntField = Default Int 666; }
              { staticIntField = Static Int; }
              { staticDefaultIntField = Static (Default Int 666); }
            ];
          };

        MyString =
          assert Type ? new;
          assert Fields ? new;
          Type.new "MyString" {
            fields = This: Fields.new [{ value = String; }];
          };

        Mystring =
          assert Type ? new;
          assert Fields ? new;
          Type.new "Mystring" {
            fields = This: Fields.new [{ value = "string"; }];
          };

        WrapString =
          assert String ? subType;
          String.subType "WrapString" {};
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
                  methods = expected.methods; # TODO: Cheat due to named thunk comparison
                  staticMethods = expected.staticMethods; # TODO: Cheat due to named thunk comparison
                  name = "A";
                  overrideCheck = null;
                  tvars = LazyAttrs {};
                  tvarBindings = LazyAttrs {};
                };
          };
        };

        Type =
          assert Type ? new;
          let A = Type.new "A" {}; in
          assert A ? Type;
          assert A ? __TypeId;
          assert (resolve A.Type) ? __TypeId;
          assert A ? name;
          assert A ? getBoundName;
          {
            Type = expect.stringEq ((resolve A.Type).__TypeId {}) "Type";
            id = expect.stringEq (A.__TypeId {}) "A";
            name = expect.stringEq A.name "A";
            getBoundName = expect.stringEq (A.getBoundName {}) "A";
          };

      };

      instantiationTests = U: with U; with TestTypes U; {
        Type = expect.eq
          (assert Type ? new;
           let SomeType = Type.new "SomeType" {}; in
           assert SomeType ? __TypeId;
           SomeType.__TypeId {})
          "SomeType";

        Literal = {
          static =
            expect.eq
              (let L = Literal 123; in
               assert L ? getLiteral;
               L.getLiteral {})
              123;
          instance =
            expect.eq
              (let L = Literal 123; in
               assert L ? new;
               (L.new {}).getLiteral {})
              123;
        };

        Field =
          assert Field ? new;
          let f = Field.new "name" null; in
          {
            name = expect.stringEq f.fieldName "name";
            fieldType = expect.eq (f.fieldType {}) null;
            fieldStatic = expect.False (f.fieldStatic {});
            fieldDefault = expect.eq (f.fieldDefault {}) null;
          };

        Fields =
          assert Fields ? new;
          let
            testFields = fields: args: {
              getSolos =
                expect.eqOn
                  (mapSolos (_: v: { inherit (v) fieldName; }))
                  (fields.getSolos {})
                  args.expectedSolos;
              # indexed = mergeAttrsList (cutils.attrs.indexed soloFields);
              # update = newFieldSpecs:
              #   Fields.new (concatSolos (solos fieldSpecs) (solos newFieldSpecs));
              # getField = name: indexed.${name}.value;
              # getFieldsWhere = pred: filterSolos pred (getSolos {});
              # instanceFields = _:
              #   getFieldsWhere (fieldName: field:
              #     fieldName != "Type"
              #     && (field == null
              #         || !(field.fieldStatic {})));
              # instanceFieldsWithType = _:
              #   getFieldsWhere (fieldName: field:
              #     field == null
              #     || !(field.fieldStatic {}));
              # requiredFields = _:
              #   getFieldsWhere (_: field:
              #     field == null
              #     || (!(field.fieldStatic {})
              #         && !(field.hasDefault {})));
            };
            expected = rec {
              TypeField = Field.new "Type" "set";
              aField = Field.new "a" null;
              bField = Field.new "b" "int";
              cField = Field.new "c" (Default Int 3);
              dField = Field.new "d" (Static Int);
              expectedSolos = [{Type = TypeField;} {a = aField;} { b = bField;} {c = cField;} {d = dField;}];
            };
          in
          {
            fromSolos =
              let fields = Fields.new [{a = null;} {b = "int";} {c = Default Int 3;} {d = Static Int;}];
              in testFields fields expected;

            fromSet =
              solo
                (let fields = Fields.new [{a = null; b = "int"; c = Default Int 3; d = Static Int;}];
                 in testFields fields expected);
          };

        Mystring = {
          mkFromString =
            expect.stringEq
              (assert MyString ? mk;
               assert String ? new;
               (MyString.mk { value = "hello"; }).value)
              "hello";
          newFromString =
            expect.stringEq
              (assert MyString ? new;
               assert String ? new;
                (MyString.new "hello").value)
              "hello";
        };

        MyType_mk_nocast =
          assert MyType ? mk;
          expect.stringEq
            (MyType.mk { myField = String.new "World"; }).myField
            "World";

        MyType_new_nocast = 
          assert MyType ? new;
          expect.stringEq
            (MyType.new (String.new "World")).myField
            "World";
      };

      builtinTests = U: with U; {
        mk =
          let
            mkBuiltinTest = T: name: rawValue:
              assert T ? new;
              let x = T.new rawValue;
              in {
                name = expect.stringEq (x.Type.__do (T: T.name.value)) name;
                value = expect.eq
                  (if name == "Lambda" then null else x.value)
                  (if name == "Lambda" then null else rawValue);
                newIsBuiltin =
                  expect.False (builtinValueCheck x);
                rawIsBuiltin =
                  expect.True (builtinValueCheck x.value);
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
            Bool =
              expect.False (
                assert Bool ? new;
                builtinValueCheck (Bool.new true)
              );
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
              (this: soloNames (this.getSolos {}))
              fields fieldsFromSet;

          getSolos =
            expect.eq
              (soloNames (fields.getSolos {}))
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

          mkToTypedBuiltinTest = T: x:
            expect.valueEq
              (cast_ T x)
              (assert T ? new;
               T.new x);

          mkToUntypedBuiltinTest = T: x:
            expect.valueEq
              (Builtin.From x)
              (assert T ? new;
               T.new x);
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
          Int = {
            valid = expect.eq (Int.new 123).value 123;
            invalid = expect.error (Int.new "123");
          };

          A = {
            valid =
              let x = A.new 1 2 3 4;
              in
                expect.eq
                  [x.a x.b.value x.c.value x.d.value]
                  [1 2 3 4];
            wrongType = {
              a = expect.error (A.new "1" 2 3 4);
              b = expect.error (A.new 1 "2" 3 4);
              c = expect.error (A.new 1 2 "3" 4);
              d = expect.error (A.new 1 2 3 "4");
            };
          };

          castInMk = with TestTypes U; {
            MyString = {
              mkFromstring = expect.eq (MyString.mk { value = "hello"; }).value.value "hello";
              newFromstring = expect.eq (MyString.new "hello").value.value "hello";
              eqString = expect.valueEq (MyString.new "hello").value (String.new "hello");
            };

            WrapString = {
              mkFromstring = expect.eq (WrapString.mk { value = "hello"; }).value "hello";
              newFromstring = expect.eq (WrapString.new "hello").value "hello";
              eqString = expect.valueEq (WrapString.new "hello") (String.new "hello");
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

      inheritanceTests = U: with U; with TestTypes U;
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

        newA = expect.lazyEqOn Compare.Fields (_: A.new "a") (_: A.mk { a = "a"; });

        isSuperTypeOf = {
          parentChild = expect.lazyTrue (_: A.isSuperTypeOf B);
          childParent = expect.lazyFalse (_: B.isSuperTypeOf A);
          parentParent = expect.lazyFalse (_: A.isSuperTypeOf A);
          childChild = expect.lazyFalse (_: B.isSuperTypeOf B);
          typeParent = expect.lazyFalse (_: Type.isSuperTypeOf A);
          typeChild = expect.lazyFalse (_: Type.isSuperTypeOf B);
          typeType = expect.lazyFalse (_: Type.isSuperTypeOf Type);
        };

        isSubTypeOf = {
          parentChild = expect.lazyFalse (_: A.isSubTypeOf B);
          childParent = expect.lazyTrue (_: B.isSubTypeOf A);
          parentParent = expect.lazyFalse (_: A.isSubTypeOf A);
          childChild = expect.lazyFalse (_: B.isSubTypeOf B);
          typeParent = expect.lazyFalse (_: Type.isSubTypeOf A);
          typeChild = expect.lazyFalse (_: Type.isSubTypeOf B);
          typeType = expect.lazyFalse (_: Type.isSubTypeOf Type);
        };
        
        WrapString_nocast = {
          mkFromString =
            expect.eq
              (assert WrapString ? mk;
               (WrapString.mk { value = "hello"; }).value)
              "hello";
          newFromString =
            expect.eq
              (assert WrapString ? new;
               (WrapString.new "hello").value)
              "hello";
        };

      };

      typeFunctionalityTests = U: with U; with TestTypes U; {

        checks = {
          RootType = expect.stringEq Type.name "Type";
        };

        methodCalls = {
          MyType_methods = {
            expr = MyType.methods.__attrNames {};
            expected = [ "helloMyField" ];
          };
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
                ((this.set.myField (String.new "World")).helloMyField "!")
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
            compare = this: this.myField.value;
          };
        };

      };

    in
      cutils.tests.suite {
        types = with Universe; {

          peripheral =
            #solo
              {
                checks = {
                  isTyped = {
                    string = expect.False (isTyped "hello");
                  };
                };
                fixing = {
                  intUnderId = expect.asserts.ok (assertFixedUnderF "f" "x" id 123);
                  intUnderPlus1 = expect.asserts.fail (assertFixedUnderF "f" "x" (x: x+1) 123);
                  intUnderPlus0 = expect.asserts.ok (assertFixedUnderF "f" "x" (x: x+0) 123);
                  Type =
                    let
                      FakeType = name: {
                        name = { value = name; };
                        new = name: args: FakeType name;
                      };
                    in {
                      fixed = expect.asserts.ok (assertTypeFixedUnderNew (FakeType "FakeType") "FakeType" {});
                      unfixed = expect.asserts.fail (assertTypeFixedUnderNew (FakeType "FakeType") "NextFakeType" {});
                    };
                };
              };

          smoke =
            solo
              (testInUniverses {
                inherit
                  U_0
                  U_1
                  U_2
                  ;
              } smokeTests);

          typeFunctionality =
            #solo
              (testInUniverses {
                inherit
                  U_0
                  U_1
                  #U_2
                  ;
              } typeFunctionalityTests);

          typeFunctionalityBroken =
            #solo
              (testInUniverses {
                inherit
                  #U_2
                  ;
              } typeFunctionalityTests);

          inheritance =
            #solo
              (testInUniverses {
                inherit
                  U_1
                  #U_2
                  ;
              } inheritanceTests);

          inheritanceBroken =
            #solo
              (testInUniverses {
                inherit
                  #U_2
                  ;
              } inheritanceTests);

          instantiation =
            #solo
              (testInUniverses {
                inherit
                  U_0
                  U_1
                  #U_2
                  ;
              } instantiationTests);

          instantiationBroken =
            #solo
              (testInUniverses {
                inherit
                #U_2
                ;
              } instantiationTests);

          builtin =
            #solo
              (testInUniverses {
                inherit
                  U_0
                  U_1
                  U_2
                  ;
              } builtinTests);

          cast =
            #solo
              (testInUniverses {
                inherit
                  U_1  # U_1+ due to reliance upon cast
                  U_2
                  ;
              } castTests);

          untyped =
            #solo
              (testInUniverses {
                inherit
                  U_0
                  U_1
                  ;
              } untypedTests);

          typeChecking =
            #solo
              (testInUniverses {
                inherit
                  U_1  # U_1+ due to reliance upon cast
                  # U_2
                  ;
              } typeCheckingTests);

          typeCheckingBroken =
            #solo
              (testInUniverses {
                inherit
                  #U_2
                  ;
              } typeCheckingTests);

        };
      };

}

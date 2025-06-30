{ pkgs ? import <nixpkgs> {},
  collective-lib ? import ./. { inherit lib; },
  cutils ? collective-lib,
  lib ? pkgs.lib,
  ... }:

with lib.strings;
with cutils.collections;
with cutils.clib;
with cutils.attrsets;
with cutils.functions;
with cutils.lists;
with cutils.strings;

# TODO:
# - TypeClasses using mkMerge - i.e. instances form a fixed-point class dictionary
# - Enums
# - Maybe / ADTs
#
# - rename to __Type
# - thunkof subtype istypethunk
# - replace builtin usage with BuiltinOf
# - shims can start to go with the lib overrides

# Typesystem for use outside of the module system.
#
# Objects are represented as attrsets with "__Type" and "__Super" keys, holding the
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

  Types =
    with lib;  # lib == untyped default pkgs.lib throughout Types
    rec {

    # Nix library overrides to take Types into account.
    mkTypelib = SU: rec {

      ### constants

      True = SU.Bool.new true;
      False = SU.Bool.new false;
      Nil = SU.Null.new null;

      ### naming workarounds

      unsafeNames = [ "__Type" "__toString" "__show" "__functor" ];

      elideName = name: if elem name unsafeNames then "ELIDED-${name}" else name;
      unelideName = replaceStrings ["ELIDED-"] [""];

      elideNames = cutils.dispatchlib.deepMapNamesCond (k: k != "__Type") elideName;

      unelideNames = cutils.dispatchlib.deepMapNamesCond (k: k != "__Type") unelideName;

      withElidedNames = xs: f:
        let xsElided = elideNames xs;
            fXs = f xsElided;
        in unelideNames fXs;

      ### unwrap

      unwrapperName = builtinName:
        if builtinName == "null" then "nil" else builtinName;

      BuiltinNameToUnwrapperSolo = 
        mapAttrs 
          (BuiltinName: builtinName: { 
            ${unwrapperName builtinName} = dispatch.type.id {
              ${builtinName} = id;
              ${BuiltinName} = x: x.getValue {};
            };
          })
        BuiltinNameTobuiltinName;

      BuiltinNameToUnwrapper = mapAttrs (_: soloValue) BuiltinNameToUnwrapperSolo;

      unwrappers = mergeAttrsList (attrValues BuiltinNameToUnwrapperSolo);

      inherit (unwrappers) nil bool string int float path list set lambda;

      ### type utilities

      # Check if a given argument is a Type in the Type system.
      # 'isType' collides with lib.isType.
      isTypeSet = T: bool (T.__isTypeSet or false);

      # Check if a given argument is a Type or a builtin type.
      isTypeLike = T: isTypeSet T || isbuiltinName T;

      # Check if a given argument is a Type or a builtin type or null.
      isTypeLikeOrNull = T: isNull T || isTypeLike T;

      # Check if a given argument is a TypeThunk
      isTypeThunk = T: bool (T.__isTypeThunk or false);

      # Check if a given argument is a custom Type.
      # Throws if not, otherwise true.
      checkTyped = x:
        with (log.v 4).call "checkTyped" x ___;
        assert checks [
          {
            name = "isAttrs x";
            cond = isAttrs x;
            msg = "checkTyped: x is not an attrset";
          }
          {
            name = "x ? __Type";
            cond = x ? __Type;
            msg = "checkTyped: x has no __Type field";
          }
        ];
        return true;

      # Check if a given argument is a custom Type using the checks of checkTyped.
      # True iff it is.
      # Does not actually tryBool to avoid strictly forcing x.
      isTyped = x: x ? __Type;

      # Check two types for equality, supporting both Types and builtin type-name strings.
      typeEq = A: B:
        #assert assertMsg (isTypeLike A) (indent.block ''
        #  typeEq: Invalid A provided:

        #    A = ${indent.here (log.print A)}
        #      = ${indent.here (log.vprintD 5 A)}
        #  '');
        #assert assertMsg (isTypeLike B) (indent.block ''
        #  typeEq: Invalid B provided:

        #    B = ${indent.here (log.print B)}
        #      = ${indent.here (log.vprintD 5 B)}
        #  '');
        bool (
          (lib.isString A && lib.isString B && A == B)
          || (isTypeSet A && isTypeSet B && A.__TypeId {} == B.__TypeId {})
        );

      # Override isType s.t. it operates per the module system as:
      # isType "string" {_type = "string"} == true
      #
      # but also as:
      # isType "string" "my string" == true
      #
      # and as:
      # isType String (String.new "my string") == true
      #
      # Where:
      # isType "set" (String.new "my string") == false
      # so that a typed value cannot be treated as a raw set.
      #
      # T must be a fully applied type. 'isType Default' will fail.
      isType = T: x:
        assert assertMsg (isTypeLike T) (indent.block ''
          isType: Invalid T provided (expected builtin type string or Type):

            T = ${indent.here (log.print T)}
              = ${indent.here (log.vprintD 5 T)}
          '');
        # Check x for _type
        bool (lib.isType T x || typeEq T (typeOf x));

      # Override typeOf s.t. on a raw builtin it operates normally, but on a typed value,
      # returns the resolved type.
      # typeOf true == "bool"
      # typeOf (Bool.new true) == Bool
      typeOf = x:
        # Extra check for resolvable to avoid accidentally trying to resolve the type of __Type
        # in a solo of {__Type = TypeThunk; }
        if x ? __Type # && !(length (attrNames x) == 1)
        then resolve x.__Type
        else lib.typeOf x;

      # Get the type as a string ID. For builtins, operates as typeOf, and for others returns
      # the string form e.g. "Union<[Int Float]>"
      # typeIdOf true == "bool"
      # typeIdOf (Bool.new true) == "Bool"
      typeIdOf = x:
        let T = typeOf x;
        in if lib.isString T then T
          else if !(T ? __TypeId) then throw (indent.block ''
            typeIdOf: Type instance has no __TypeId field:
              x = ${indent.here (log.vprintD 5 x)}
              T = ${indent.here (log.vprintD 5 T)}
          '')
          else T.__TypeId {};

      # Get the type name as a string ID. For builtins, operates as typeOf, and for others returns
      # a raw string.
      typeNameOf = x:
        let T = typeOf x;
        in if lib.isString T then T
          else if !(T ? name) then throw (indent.block ''
            typeNameOf: Type instance has no name field:
              x = ${indent.here (log.vprintD 5 x)}
              T = ${indent.here (log.vprintD 5 T)}
          '')
          else T.name;

      # Get the type name as a string ID. For builtins, operates as typeOf, and for others returns
      # a raw string.
      typeBoundNameOf = x:
        let T = typeOf x;
        in if lib.isString T then T
          else T.getBoundName {};

      ### dispatch

      # Eventually exposed as collective-lib.typelib.dispatch and merged into collective-lib.dispatch.dispatch
      dispatch = {
        type = {
          # Dispatch on __TypeId
          # Matches full bound name string
          id = cutils.dispatchlib.dispatch.on typeIdOf;

          # Dispatch on type name
          # Matches only the type name, not its bindings
          # i.e. can match any Union, not just Union<[Int Float]>
          name = cutils.dispatchlib.dispatch.on typeNameOf;
        };
      };

      ### builtin / Builtin

      # List of all Builtin names i.e. [ "Null" ... ]
      BuiltinNames = [ "Null" "Int" "Float" "String" "Path" "Bool" "List" "Set" "Lambda" ];

      # Mapping i.e. Null to null
      BuiltinNameTobuiltinName =
        mergeAttrsList (map (BName: { ${BName} = toLower BName; }) BuiltinNames);

      # List of all builtin names i.e. [ "null" ... ]
      builtinNames = map toLower BuiltinNames;

      # Whether or not name is one of the lowercase builtin type names i.e. "string"
      isbuiltinName = name: elem name builtinNames;

      # Whether or not name is one of the uppercase Builtin type names i.e. "String"
      isBuiltinName = name: elem name BuiltinNames;

      # Whether or not x is a lowercase builtin type
      # Using typelib.typeOf to avoid set.__Type confusion.
      isbuiltinValue = x: isbuiltinName (typeIdOf x);

      # Whether or not x is a lowercase builtin type
      # Using typelib.typeOf to avoid set.__Type confusion.
      isBuiltinValue = x: isbuiltinName (typeIdOf x);

      ### getValue

      getValue = x:
        if x ? getValue then x.getValue {} else throw (indent.block ''
          Attempted getValue on non-Builtin instance:
            ${indent.here (log.print x)}
          '');
      getValueOr = def: x: errors.try (getValue x) (_: def);
      getValueOrNull = getValueOr null;

      ### null

      isNull = x: x == null || isType SU.Null x;

      ### bool

      isBool = x:
        lib.isBool x
        || (isType SU.Bool x && lib.isBool (getValueOrNull x));

      boolToString = x: lib.boolToString (bool x);

      ### int

      isInt = x:
        lib.isInt x
        || (isType SU.Int x && lib.isInt (getValueOrNull x));

      ### float

      isFloat = x:
        lib.isFloat x
        || (isType SU.Float x && lib.isFloat (getValueOrNull x));

      ### string

      isString = x:
        lib.isString x
        || (isType SU.String x && lib.isString (getValueOrNull x));

      hasToString = x:
        x ? __toString || x ? outPath || elem (lib.typeOf x) [
          "string" "path" "list" "int" "float" "bool" "null"
        ];

      withoutToString = x: removeAttrs x ["__toString" "outPath"];

      withoutShow = x: removeAttrs x ["__show"];

      withoutStringConversions = x: withoutToString (withoutShow x);

      withoutFunctor = x: removeAttrs x ["__functor"];

      withoutUnsafeStringConversionAttrs = x: withoutFunctor (withoutStringConversions x);

      ### path

      isPath = x:
        lib.isPath x
        || (isType SU.Path x && lib.isPath (getValueOrNull x));

      ### list

      isList = x:
        lib.isList x
        || (isType SU.List x && lib.isList (getValueOrNull x));

      ### attrs

      # Use isUntypedAttrs to detect attrs that are not intended as being a Type instance.
      isAttrs = x:
        lib.isAttrs x
        || (isType SU.Set x && lib.isAttrs (getValueOrNull x));

      # isAttrs where Types and instances do not qualify as attrsets.
      isUntypedAttrs = x: !(isTyped x) && lib.isAttrs x;

      ### function

      # Enables Lambda to be identified as a function.
      # Note that every Type will show up as a function due to being a functor.
      isFunction = x:
        lib.isFunction x
        || (isType SU.Lambda x && lib.isFunction (getValueOrNull x));

      # Important to be able to identify functors in particular, because we terminate
      # variadic calls when we have emitted a non-function. This enables "new" to variadically
      # return an instance of Type i.e. Int, which otherwise would pass the isFunction check
      # and fail to terminate the variadic call.
      isFunctor = x: lib.isAttrs x && x ? __functor;
      isFunctionNotFunctor = x: lib.isFunction x && !isFunctor x;

      # Call a Lambda.
      call = lambda;
    };

    mkCast = SU: U: rec {
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
        with (log.v 3).call "cast" T x ___;
        assert checks [
          { name = "isTypeLikeOrNull T";
            cond = U.isTypeLikeOrNull T;
            msg = "cast: T is not a Type or builtin type: ${log.print T}"; }
        ];

        if T == null then
          return (mkCastError "Cannot cast to null: T = ${log.print T}, x = ${log.print x}")

        else if !(U.isTypeLike T) then
          return (mkCastError (indent.block ''
            Invalid target type provided for cast:

              T = ${indent.here (log.print T)}

              x = ${indent.here (log.print x)}
          ''))

        else with lets rec {

          printT = T: log.print T;

          TName = printT T;
          xTName = U.typeBoundNameOf x;

          xFields = (
            if U.isTyped x then thunkDo x.__Type (T:
              if !(T ? getFields) then throw (indent.block ''
                xFields: x.__Type.getFields does not exist:
                  x = ${indent.here (log.print x)}
                  x.__Type {} = ${indent.here (log.print T)}
              '')
              else (T.getFields {}).instanceFields {})
            else
              mkCastError ''
                Cannot get fields from untyped uncast value: T = ${log.print T}, x = ${log.print x}
              '');
          TFields = (
            if U.isTypeSet T then
              if !(T ? getFields) then throw ''TFields: T.getFields does not exist: ${log.print T}''
              else (T.getFields {}).instanceFields {}
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
            else ((soloValue field).fieldType or (const null)) {});
          TUnaryFieldT = castErrorOr TUnaryField (field:
            if field == null then mkCastError ''Unary field is not a solo: ${log.print field}''
            else ((soloValue field).fieldType or (const null)) {});

          xUnaryFieldTName = castErrorOr xUnaryFieldT printT;
          TUnaryFieldTName = castErrorOr TUnaryFieldT printT;

          xFieldNames = castErrorOr xFields (map (s: (soloValue s).fieldName {}));
          TFieldNames = castErrorOr TFields (map (s: (soloValue s).fieldName {}));

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
                when = U.isType T x;
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
                when = U.isTypeSet T && T.getName {} == "Union";
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
                result = castErrorOr xUnaryFieldName (fieldName:
                  if !(x ? ${fieldName})
                  then mkCastError "x does not have detected unary field x.${fieldName} set"
                  else cast T x.${fieldName});
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
    };

    # "string" -> null
    # Default<string, 123> -> Default<null, 123>
    # Static<Default<string, 123>> -> Static<Default<null, 123>>
    setFieldTypeToNull = SU: 
      # Either match a type by name, or use the (possibly string) type-spec verbatim.
      dispatchDefOn (spec: (spec.getName or (const "")) {}) (const null) {
        Default = D: SU.Default (setFieldTypeToNull SU (D.defaultType {})) (D.defaultValue {});
        Static = S: SU.Static (setFieldTypeToNull SU (S.staticType {}));
      };

    maybeNulled = opts: SU: spec:
      if opts.enableTypeChecking then spec
      else if !opts.retainTypeFieldSpec then null
      else setFieldTypeToNull SU spec;

    # Construct the common fields for a universe using the types of the universe above.
    withCommonFieldSpecs = opts: SU: fieldSpecs_:
      # The type of the instance as a thunk.
      # Set in mkInstance args_->args to the This TypeThunk.
      let fieldSpecs = solos fieldSpecs_;
          typeField = mkSolo "__Type" (maybeNulled opts SU SU.TypeThunk); in
      consSolo typeField fieldSpecs;

    # Construct the fields for a universe using the types of the universe above.
    # The 'Type' field is added in Fields.new in both shim and real implementations.
    mkTypeFieldSolosFor = SU: U: [
      # Indicates that this is a type. Should never be set manually.
      {__isTypeSet = maybeNulled U.opts SU (SU.Default SU.Bool (SU.Bool.new true));}
      # The supertype of the type.
      {__Super = maybeNulled U.opts SU (SU.Default (SU.NullOr SU.TypeThunk) SU.Nil);}
      # The name of the type.
      {name = maybeNulled U.opts SU (SU.String);}
      # The type parameters of the type.
      {tvars = maybeNulled U.opts SU (SU.Default "set" (LazyAttrs {}));}
      # The type parameter bindings of the type.
      {tvarBindings = maybeNulled U.opts SU (SU.Default "set" (LazyAttrs {}));}
      # The constructor function creating the fields of the type as a set to pass to mk.
      {ctor = maybeNulled U.opts SU (SU.Default SU.Ctor U.Ctors.CtorDefault);}
      # A set of ordered fields to make available as this.___ and this.has.___, this.set.___, etc
      {fields = maybeNulled U.opts SU (SU.Default SU.Lambda (SU.Lambda.new (This: SU.Fields.new [])));}
      # A set of methods from this to make available as this.___
      {methods = maybeNulled U.opts SU (SU.Default "set" (LazyAttrs {}));}
      # A set of methods from This to make available as This.___ and this.___
      {staticMethods = maybeNulled U.opts SU (SU.Default "set" (LazyAttrs {}));}
      # Perform additional checks on the value of the type when comparing.
      {checkValue = maybeNulled U.opts SU (SU.Default (SU.NullOr SU.Lambda) SU.Nil);}
      # If set, ignore all other checks and use this check function only.
      {overrideCheck = maybeNulled U.opts SU (SU.Default (SU.NullOr SU.Lambda) SU.Nil);}
    ];

    # We need to be very careful here to only access U from sites that are
    # called by a fully bound and constructed U.Type (i.e. during U.Type.newTemplate)
    # so that we do not attempt to access types that will themselves attempt to
    # force U.Type so they can be constructed.
    # See mkTemplating - essentially anywhere we produce a new Type here, we need
    # to do so from U.* - i.e. we need to use U.newSubType instead of SU.newSubType
    # so that ((U.Type.new "Parent" {}).subType "Child" {}).Type == U.Type and not SU.Type,
    # which would mean every subtype created in a chain would ascend a universe until
    # it reached the Quasiverse.
    typeMethodsFor = SU: U: {
      # Get the resolved name of the type.
      getName = This: _: This.name.getValue {};

      # Get the resolved fields of the type.
      getFields = This: _: (U.lambda (This.fields)) This;

      # Is this instance an instance of type That.
      isInstance = This: that: U.isTyped that && thunkDo that.__Type (That: That.eq This);

      # A = Type.new "A" {};
      # B = Type.new "B" { __Super = A; };
      #
      # isSuperTypeOf A B == true
      # isSuperTypeOf B A == false
      # isSuperTypeOf A A == false
      # isSuperTypeOf Type A == true
      # isSuperTypeOf Type B == true
      isSuperTypeOf = This: That:
        if U.isNull That.__Super then false
        else thunkDo That.__Super (U.typeEq This)
            || thunkDo That.__Super (ThatSuper: This.isSuperTypeOf ThatSuper);
      isSubTypeOf = This: That: That.isSuperTypeOf This;

      # Get the full templated name of the type.
      getBoundName = This: thunk (
        with (log.v 4).methodCall This "getBoundName" ___;
        let tvars = resolve This.tvars;
            tvarBindings = resolve This.tvarBindings;
        in
        return (
          if tvars == {} then This.getName {}
          else
            let
              printBinding = tvarName:
                let C = tvars.${tvarName} or (
                      throw "No type variable ${tvarName} on ${This.getName {}}");
                    T = tvarBindings.${tvarName} or SU.Void;
                in
                  # Unbound
                  if U.typeEq SU.Void T
                    then tvarName

                  # Bound to a type
                  else if U.isTypeSet T
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
                  Constraints: ${indent.here "${log.print C} (${if TSatC then "" else "not "}satisfied)"}
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

              else if !(U.typeEq B SU.Void)
                then throwBindError "Type ${This.__TypeId {}} already has type variable ${tvarName} bound to ${log.print B}"

              else if !(C.satisfiedBy T)
                then throwBindError "Binding ${log.print T} does not satisfy constraint: ${log.print C}"

              else This.modify.tvarBindings (bs: thunkFmap bs (bs: bs // {${tvarName} = T;}));

        in foldl' bindOne This tvarBindingsList;

      # Is That the same type as This?
      eq = This: That: U.typeEq This That;

      # Does this type inherit from That?
      inheritsFrom = This: That:
        U.isTypeSet That
        && That.eq This
        || (!(U.isNull This.__Super)
            && thunkDo This.__Super (Super: Super.inheritsFrom That));

      check = This: that:
        let
          runChecks = errors.tryBool (errors.checks [
            {
              cond = U.isType This that;
              msg = "Type check failed for ${This.__TypeId {}} (got ${log.print that})";
            }
            {
              cond = U.isNull This.checkValue || U.call This.checkValue that;
              msg = "checkValue failed: ${log.print that} is not a value of type ${This.__TypeId {}}";
            }
          ]);
        in if !(U.isNull This.overrideCheck)
          then U.call This.overrideCheck that
          else runChecks;

      # Use the bound name with its ordered param assignments to determine type identity
      # and equality.
      # For bootstrap types this may not be bound yet, falling back to the name.
      __TypeId = This: _: U.call (This.getBoundName or This.getName) {};

      # Create a new instance of the type by providing at least all required field values.
      mk = This: U.mkInstance This;

      # Create a new instance of the type by calling This's constructor
      # When This == Type, creates a new Type.
      # Consumes its first argument to avoid nullaries
      new = This: U.newInstance This;

      # Create a new subType inheriting from This
      # TODO: Compose checkValue / overrideCheck if they appear in args
      subType = This: name: args:
        U.newSubType This name args;

      # For a given This type, create a new template whose eventual bound type subtypes This.
      subTemplate = This: name: tvars_: bindingsToArgs_:
        U.__newTemplate This This name tvars_ bindingsToArgs_;

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
        U.newTemplate This name tvars bindingsToArgs;

      # Create a new template inheriting from a function of This plus any type variables.
      # TODO: Generic inheritance and tvar/tvarBinding merging.
      subTemplateOf =
        This: bindingsToSuper: name: tvars_: bindingsToArgs_:
        U.__newTemplate This bindingsToSuper name tvars_ bindingsToArgs_;

      ### __implements: Converted to e.g. __toString upon binding

      # Print Type objects as just their name.
      __implements__toString = This: self: This.__TypeId {};

      # Enable creation of Types simply by calling the constructor.
      # When this method is bound, turns the Type instance itself into a callable functor.
      # Consumes one argument to avoid internal __functor machinery going infinite upon binding.
      __implements__functor = This: self: arg: This.new arg;
    };

    # Type consists only of members of SU.
    # Descendence into subU takes places on U.Type.new, which references SU.* components
    # to call mkInstance using subU.Type__args, which are made up of U.* components,
    # producing a subU.Type made only of U components, and so on, such that U.Type
    # is always composed of SU.Type components for all U, SU.
    # The exception is templating, or types producing types - these refer to U only
    # in the method body, after Type is already constructed and bound.
    mkTypeArgsFor = SU: U: {
      # Set manually to enable mkInstance rather than newInstance
      __Type = SU.TypeThunk.new U.Type;
      __isTypeSet = true;
      name = SU.String.new (U.opts.typeName);
      __Super = SU.Nil;
      ctor = U.Ctors.CtorType;
      fields = This: SU.Fields.new (mkTypeFieldSolosFor SU U);
      # We have these are both methods and staticMethods on Type s.t. we have
      # them present in bootstrap and in Type.new instances, which get these as
      # methods via Ctors.CtorType.
      methods = maybeNamedLazyAttrs "mkTypeArgsFor.methods" (typeMethodsFor SU U);
      staticMethods = maybeNamedLazyAttrs "mkTypeArgsFor.staticMethods" (typeMethodsFor SU U);
      tvars = LazyAttrs {};
      tvarBindings = LazyAttrs {};
      checkValue = SU.Nil;
      overrideCheck = SU.Nil;
    };

    mkInstantiation = SU: U: rec {
      # e.g. parseFieldSpec Static<Default<Int, 123>> -> {fieldStatic = true, fieldDefault = 123; fieldType = Int; }
      #      parseFieldSpec Static<Int> -> {fieldStatic = true; fieldType = Int; }
      #      parseFieldSpec Default<Int, 123> -> {fieldDefault = 123; fieldType = Int; }
      #      parseFieldSpec Int -> { fieldType = Int; }
      parseFieldSpec = spec:
        with (log.safe.v 3).call "parseFieldSpec" { inherit spec; level = U.opts.level; } ___;

        # Unwrap Static types.
        # Duck-typed to support bootstrap.
        if spec ? staticType
        then (parseFieldSpec (spec.staticType {})) // {
          fieldStatic = true;
        }

        # Unwrap Default types.
        # Duck-typed to support bootstrap.
        else if (spec ? defaultType) && (spec ? defaultValue)
        then 
          (parseFieldSpec (spec.defaultType {})) // {
          fieldDefault = spec.defaultValue {};
          hasDefault = true;
        }

        # Return unwrapped types.
        else
          assert check
            "SU.isTypeLikeOrNull spec"
            (U.isTypeLikeOrNull spec)
            (indent.blocks [
              ''
                Got a non-type spec in parseFieldSpec:
                  ${indent.here (log.print spec)}
              ''
              (optionalString (U.isTypeThunk spec) ''
                Spec is a TypeThunk, attempting resolution:
                  ${indent.here (log.print (errors.try (resolve spec) (_: "Resolution failed")))}
              '')
            ]);
          {
            fieldType = spec;
            fieldStatic = false;
            fieldDefault = throw (indent.block ''
              parseFieldSpec: Attempted downstream access of default value for non-defaultable field:
                ${indent.here (log.print spec)}
            '');
            hasDefault = false;
          };

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
      mkFieldAssignmentValue = This: field: uncastValue:
        with (log.v 3).call "mkFieldAssignmentValue" {inherit This field uncastValue;} ___;

        with lets rec {
          fields = U.call This.fields This;
          fieldName = field.fieldName {};
          fieldType = field.fieldType {};
          castRequired =
            # Field has a type
            !(U.isNull fieldType)
            && (
              # The value provided does not already have this type
              !(U.isType fieldType uncastValue)
              # nor does it pass a check on the field type
              && !(U.isTypeSet fieldType && fieldType.check uncastValue));
          value =
            if castRequired
              then U.cast fieldType uncastValue
              else U.mkCastSuccess uncastValue "id";
        };

        assert (errors.predChecks field [
          { pred = (field: !(U.isNull field));
            msg = joinLines [
              "Setting unknown field: ${This.name}.${fieldName}"
              "Known fields: ${joinSep ", " (map (s: (soloValue s).fieldName {}) (fields.instanceFields {}))}"
            ];
          }
          { pred = field: (!castRequired) || U.isCastSuccess value;
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
                          || (U.isTypeSet fieldType && fieldType.check value.castSuccess)
                          || (typeOf value.castSuccess == fieldType);
            msg = indent.block ''
              Cast value did not pass typecheck:
                ${if This ? __TypeId then This.__TypeId {} else "This"}.${fieldName} = ${log.print uncastValue}
                Cast value of ${log.print (value.castSuccess or null)} is not a valid instance of ${log.print fieldType}.
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
      mkFieldAssignmentSoloFromUncastValue = This: field: uncastValue:
        with (log.v 2).call "mkFieldAssignmentSoloFromUncastValue" {inherit This field uncastValue; } ___;
        with lets rec {
          castValue =
            if !((resolve field.__Type).__TypeId {} == "Field") then with indent; throws.block ''
              Invalid field encountered in setFields:

                This = ${here (print This)}

                field = ${here (print field)}
            ''
            else mkFieldAssignmentValue This field uncastValue;
        };
        return { ${field.fieldName {}} = castValue; };

      mkFieldAssignmentSoloFromArgs = This: args: field:
        with (log.v 2).call "mkFieldAssignmentSoloFromArgs" {inherit This args field;} ___;
        with lets rec {
          getDefaultValue = field.fieldDefault or (with indent; throws.block ''
            Field ${field.fieldName {}} is not set in args and has no default value:
              This = ${here (print This)}
              args = ${here (print args)}
          '');
          uncastValue = args.${field.fieldName {}} or getDefaultValue;
        };
        return (
          mkFieldAssignmentSoloFromUncastValue
            This field uncastValue);

      # Make all field assignments on the instance
      mkFieldAssignments = This: args:
        with (log.v 2).call "mkFieldAssignments" This args ___;
        let fields = This.fields This; in
        return
          (mergeSolos
            (concatMapSolos
              (_: field: mkFieldAssignmentSoloFromArgs This args field)
              (fields.instanceFieldsWithType {})));

      # Accessors to include on all instances.
      # Where these need binding, they are bound similarly to methods s.t. methods can call
      # accessors.
      mkAccessors = This:
        let
          fields = SU.call This.fields This;
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
            get = mergeAttrsList (mapSolos (_: field: this: thunk this.${field.fieldName {}}) (fields.instanceFields {}));

            # Field setting interface
            # e.g. this.set.someInt 123 -> this'
            #      this.set.someInt "123" -> throws Type error
            set =
              mergeAttrsList
                (mapSolos
                  (_: field:
                    this:
                      # Reinit here inside the thunk to avoid constant reinit while setting by name
                      # during construction.
                      uncastValue:
                        let assignment = mkFieldAssignmentSoloFromUncastValue This field uncastValue;
                        in bindThis This (this // assignment))
                  (fields.instanceFields {}));

            # Field modification interface
            # e.g. this.modify.someInt (x: x+1) -> this'
            #      this.modify.someInt toString -> throws Type error
            modify =
              mergeAttrsList
                (mapSolos
                  (_: field:
                    this: f:
                      this.set.${field.fieldName {}} (f this.${field.fieldName {}}))
                  (fields.instanceFields {}));
          };

      checkNoNullaryBindings = check: This: bindings:
        let
          nullaryBindings = filterAttrs (_: binding: !(U.isFunctionNotFunctor binding)) bindings;
        in
        check
          "No nullary bindings"
          (empty nullaryBindings)
          (indent.block (''
            Nullary bindings encountered when binding 'this' of ${log.print This}:
              ${indent.here (log.vprintD 2 nullaryBindings)}
            ''));

      mkStaticMethodBindings = This: staticMethods: this_:
        with (log.v 3).call "mkStaticMethodBindings" This staticMethods "unsafe:this_" ___;
        let
          bindings =
            mapAttrs
              (methodName: staticMethod: staticMethod this_)
              (maybeResolve (This.staticMethods or {}));
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

      # Convert the given special method name to its intended name.
      # e.g. __implements__toString -> __toString
      elideAttrName = replaceStrings ["__implements"] [""];
      elideInstanceAttrNames = concatMapAttrs (name: value: { ${elideAttrName name} = value; });

      # Bind members that refer to this on construction and after any change.
      bindThis = This: this:
        let
          # If we are creating a Type, we need to bind its static methods too
          # When creating Type via Type.new: binds 'new' to the new Type
          # When creating Bool via Type.new: binds 'new' to Bool
          # When creating bool via Bool.new: this.__Type.staticMethods == Bool.staticMethods == {}
          staticMethodInstanceBindings = 
            mkStaticMethodBindings This ((resolve this.__Type).staticMethods or {}) this_;
          # When creating Type via Type.new: binds 'new' to the new Type
          # When creating Bool via Type.new: this.staticMethods == Bool.staticMethods == {}
          # When creating bool via Bool.new: this ? staticMethods == false
          staticMethodBindings =
            mkStaticMethodBindings This (this.staticMethods or {}) this_;

          accessorBindings = mkAccessorBindings This this_;
          methodBindings = mkMethodBindings This this_;
          this_ = 
            # We can only elide the __implements prefix after this merge, otherwise
            # mergeAttrsList fails when it encounters a perceived functor (i.e. staticMethodBindings
            # or methodBindings containing a bound __functor method).
            elideInstanceAttrNames
              (mergeAttrsList [
                this
                staticMethodInstanceBindings
                staticMethodBindings
                accessorBindings
                methodBindings
              ]);
        in
          this_;

      # Create a new instance of a type by calling its constructor.
      # The constructor's output arguments are then passed into mkInstance.
      # For types, the constructor just merges a name parameter with an arguments
      # parameter and delegates to mkInstance.
      newInstance = This:
        with (log.v 2).call "newInstance" "unsafe:This" ___;
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
        let
          # All instances have a common __Type field.
          # If this is specified explicitly by the ctor as args.__Type, use this value instead
          # This should be overridden / specified with caution - this is not typechecked to ensure
          # it is a valid TypeThunk, or that it refers to This.
          maybeSetType = args: args // {
            __Type = args.__Type or (SU.TypeThunk.new This);
          };
        in
          Variadic.composeFunctorsAreAttrs
            (args: mkInstance This (maybeSetType args))
            (arg: This.ctor.bind This arg);

      # Build a new instance from a single dict of field values.
      # Construct an instance from a This type and attrset of field-to-value assignments.
      # arg = { fieldName = value, ... } for all fields.
      # Fields with defaults can be omitted.
      # Build a new instance from a single dict of field values.
      mkInstance = This: args_:
        with (log.safe.v 2).call "mkInstance" { inherit This args_; level = U.opts.level; }___;
        let
          args = args_ // {
            # Ensure that args.__Type is set to the This type.
            # This is used to ensure that the type of the instance is correct.
            # If set by ctor and constructed by new, the .ctor __Type takes precedence.
            # Otherwise if unset by ctor and constructed by new, the .new __Type takes precedence
            # Otherwise if constructed by mkInstance and unspecified in args_ (i.e. using .mk directly) we set here.
            __Type = args_.__Type or (SU.TypeThunk.new This);
          };

          # Construct 'this' as an instance of 'This'.
          this = assign "this" (mkthis This args);

          # Check the validity of the constructed instance.
          validityChecks =
            let
              fields = (U.lambda This.fields) {};

              suppliedFieldNames =
                assign "suppliedFieldNames" (
                  attrNames args);

              fieldNames =
                assign "fieldNames" (
                  soloValues 
                  (mapSolos (_: field: field.fieldName {}) (fields.instanceFieldsWithType {})));

              populatedFieldNames =
                assign "populatedFieldNames" (
                  filter (name: this ? ${name}) fieldNames);

              requiredFieldNames =
                assign "requiredFieldNames" (
                  map (s: (soloValue s).fieldName {}) (fields.requiredFields {}));

              # Get any supplied non-static fields not present in this or any supertype.
              unknownFieldNames = assign "unknownFieldNames" (
                subtractLists fieldNames populatedFieldNames);

              # Get any fields not populated in this or any supertype.
              missingFieldNames = assign "missingFieldNames" (
                subtractLists populatedFieldNames requiredFieldNames);

              staticMethodNames =
                assign "staticMethodNames" (
                  map elideAttrName (This.staticMethods.__attrNames {}));

              populatedStaticMethodNames =
                assign "populatedStaticMethodNames" (
                  filter (name: this ? ${name}) staticMethodNames);

              missingStaticMethodNames = assign "missingStaticMethodNames" (
                subtractLists populatedStaticMethodNames staticMethodNames);
            in [
              {
                name = "unknownFieldNames == []";
                cond = unknownFieldNames == [];
                msg = "${log.print This}: Unknown fields in mkInstance call: ${joinSep ", " unknownFieldNames}";
              }
              {
                name = "missingFieldNames == []";
                cond = missingFieldNames == [];
                msg = ''
                  ${log.print This}: Missing fields in mkInstance call: ${joinSep ", " missingFieldNames}
                  args: ${log.print args}
                  This: ${log.printAttrs This}
                  this: ${log.print this}
                '';
              }
              {
                name = "missingStaticMethodNames == []";
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
    };

    mkCtors = SU: U: rec {
      Ctors = rec {
        None = SU.Ctor.new "None" (This: _: throw ''Ctors.None evoked'');

        # Explicit nullary constructor s.t. X.new == X.mk {}
        # Still needs a thunk arg otherwise it will evaluate
        CtorNullary = SU.Ctor.new "CtorNullary" (This: _: {});

        # Default constructor for regular non-NewType/Builtin/Alias types.
        # Accepts required field values in order of Fields definition
        CtorDefault = SU.Ctor.new "CtorDefault" (This:
          let
            fields = U.call This.fields This;
            sortedFieldNames = 
              soloValues 
                (mapSolos (_: field: field.fieldName {}) (fields.instanceFields {}));
          in
          with (log.v 3).call "CtorDefault.ctor" { inherit This; } ___;
          if (nonEmpty (fields.instanceFields {}))
          then return (Variadic.mkOrdered sortedFieldNames)
          else return (_: {}));

        # The constructor for Type in U and its precursors / descendent Type types
        # in subuniverses of U.
        # TODO: Defaults should not need restating in U_2+
        CtorType = SU.Ctor.new "CtorType" (This: name: args: {
          __isTypeSet = true;
          name = SU.String.new name;
          __Super = args.__Super or SU.Nil;
          # Whatever ctor is given, for universes that don't have access to Field's defaults,
          # we need to ensure the end result contains values for all Type args.
          ctor = args.ctor or CtorDefault;
          fields =
            let fields_ = args.fields or (This: SU.Fields.new []);
            in assert assertMsg (U.isFunction fields_) (indent.block ''
                Non-function Fields provided:
                  fields = ${indent.here (log.print fields_)}
                  This = ${indent.here (log.print This)}
              '');
              fields_;
          # Merge any provided methods with the given defaults. Any specified in args.methods
          # will be overridden.
          methods =
            thunkFmap
              (maybeLazyAttrs (args.methods or {}))
              (methods: {
                # Otherwise the raw set with ctor, without __show etc
                __implements__toString = this: self: indent.block ''
                  ${errors.try (U.typeIdOf this) (_: "<typeIdOf failed>")} (
                    ${indent.here (log.print (U.withoutUnsafeStringConversionAttrs this))} )
                '';
              } // methods);
          staticMethods = maybeLazyAttrs (args.staticMethods or {});
          tvars = maybeLazyAttrs (args.tvars or {});
          tvarBindings = maybeLazyAttrs (args.tvarBindings or {});
          checkValue = args.checkValue or null;
          overrideCheck = args.overrideCheck or null;
        });
      };
    };

    mkBuiltinUtils = SU: U: rec {
      # Wrap up some builtin constructors.
      # TODO: Builtin to a base type for all builtins.
      # TODO: Move to common
      Builtin = {
        # Get the Builtin type corresponding to the given builtin type.
        # Returns null if builtinType is not a builtin type string.
        maybeFromT = builtinType:
          if (!isString builtinType) then null
          else {
            null = U.Null;
            int = U.Int;
            float = U.Float;
            string = U.String;
            path = U.Path;
            bool = U.Bool;
            list = U.List;
            set = U.Set;
            lambda = U.Lambda;
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
          let 
            builtinT = lib.typeOf x;
            T = {
                int = U.Int;
                float = U.Float;
                string = U.String;
                path = U.Path;
                bool = U.Bool;
                list = U.List;
                set = assert !(x ? __Type); U.Set;
                lambda = U.Lambda;
              }."${typeOf x}" or (throw ''
                Invalid type for Builtin.From:
                ${with log.prints; here (U.typeNameOf x) ___}
              '');
          in 
            if builtinT == "null" then U.Nil
            else T.new x;

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
    #
    # Builtins do not use these, so we are safe to access e.g. U.Lambda
    mkTemplating = SU: U: rec {

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
      inheritFrom = Super: ctorArgs:
        with (log.v 3).call "inheritFrom" Super ctorArgs ___;
        assert check "U.isTypeSet Super"
          (U.isTypeSet Super)
          "inheritFrom: Super must be a Type, got ${log.print Super}";

        return (ctorArgs // {
          # Store Super as a Thunk directly.
          # If __Super is already set on ctorArgs, defer to this.
          # This should be overridden with caution; this enables TypeThunk's __Super
          # to be a shim, such that TypeThunk does not contain field values of type TypeThunk.
          # Uses SU.TypeThunk, because TypeThunk relies on inheritFrom of (ThunkOf Type).
          __Super = ctorArgs.__Super or (SU.TypeThunk.new Super);

          # Override or inherit the ctor
          # TODO: Merge, or make super() available
          ctor = ctorArgs.ctor or Super.ctor;

          # Merge fields inside Super into a new This: ... form
          # The Fields.update method handles the solo-list duplicate merge.
          fields = assign "fields" (
            U.Lambda (This:
              let superFields = Super.getFields {};
              in
                if ctorArgs ? fields
                then 
                  let ctorFields = ctorArgs.fields This;
                      ctorFieldSpecs = ctorFields.fieldSpecs {};
                  in superFields.update ctorFieldSpecs
                else superFields)
          );

          # Merge methods with those inside the Super thunk.
          methods =
            let
              ctorMethods = maybeResolve (ctorArgs.methods or {});
              superMethods = resolve Super.methods;
            in LazyAttrs (superMethods // ctorMethods);

          # Merge static methods with those inside the Super thunk.
          staticMethods =
            let
              ctorStaticMethods = maybeResolve (ctorArgs.staticMethods or {});
              superStaticMethods = resolve Super.staticMethods;
            in LazyAttrs (superStaticMethods // ctorStaticMethods);
        });

      # For a given type, create a new type in the same universe.
      # Inheritance is performed as per inheritFrom.
      # Only ever used after U.Type is built, so we can safely access U.Type.new.
      newSubType = This: name: args:
        with (log.v 2).call "newSubType" { inherit This name args; } ___;
        assert check "U.isTypeSet This"
          (U.isTypeSet This)
          (indent.block ''
            Cannot subtype non-Type or Type-precursor:
              ${log.print This}
          '');
        with safety true;
        return (U.Type.new name (inheritFrom This args));

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
          tvars = mapAttrs (_: T: SU.Constraint.new (SU.TypeThunk.new T)) tvars_;

          # Convert the given (_: {...}) type template definition into one that
          # explicitly extends args with tvars and tvarBindings
          bindingsToArgs = tvarBindings:
            let args = bindingsToArgs_ tvarBindings;
            in args // {
              # Set the tvars to exactly those given.
              tvars = LazyAttrs tvars;
              tvarBindings = LazyAttrs tvarBindings;
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
          else if U.isTypeSet bindingsToSuperOrSuper
            then
              { bind = bindings:
                  let args = bindingsToArgs bindings;
                      # TODO: constraints skipped here
                      Super = bindingsToSuperOrSuper;
                  in Super.subType name args;
              }

          else
            assert assertMsg (U.isFunction bindingsToSuperOrSuper) ''
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
    # 
    # TODO: Update TS to best current working universe
    # U_4 is perfect but the constant rebuilds are slow.
    # U_1 is functional but contains shims in the inner workings.
    TSUniverse = Universe.U_1;
    TS = removeAttrs TSUniverse [
      "opts"
      "_U"
      "_SU"
    ];

    # Cause a Type to have itself as its own Type, eliding any information about
    # either bootstrap pseudo-types or the Type of its superuniverse.
    groundType = SU: U: Type:
      let Type__grounded = Type // {__Type = SU.TypeThunk.new Type__grounded;};
      in Type__grounded;

    # Cause a Type to have itself as its own Type, eliding any information about
    # either bootstrap pseudo-types or the Type of its superuniverse.
    # Also asserts that recreating the grounded Type using itself, via Type.new,
    # creates an identical Type (modulo lambda equality).
    groundTypeAndAssertFixed = SU: U: opts: Type__args: Type:
      let Type__grounded = groundType SU U Type;
      in assert assertTypeFixedUnderNew Type__grounded opts.typeName Type__args;
        Type__grounded;

    # Add the common core to the U universe.
    # U_ must have 'opts' already set.
    withCommonUniverse = SU: U_:
      let
        opts = U.opts;
        U = foldl'
          (U: f: U // f SU U)
          U_
          [ (SU: U: {
              # Ensure the lib for a universe is accessible by reference.
              Typelib = mkTypelib SU;

              # Tersely print universes.
              __toString = _: "<Universe: ${opts.name}>";
            })
            (SU: U: U.Typelib) # Embed the contents of created typelib.
            (SU: U: mkBuiltinUtils SU U)
            (SU: U: mkCast SU U)
            (SU: U: mkInstantiation SU U)
            (SU: U: mkUniverseReferences opts SU U)
            (SU: U: mkCtors SU U)
            (SU: U: mkTemplating SU U)
          ];
      in
        U;
    withCommonUniverseSelf = U_:
      let U = withCommonUniverse U U_;
      in U;

    mkUniverseReference = opts: tag: U:
      NamedThunk "<${tag}-universe ref: ${opts.name}>" U;

    mkUniverseReferences = opts: SU: U: {
      _U = mkUniverseReference opts "self" U;
      _SU = SU;
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
      # Must be at least 1, such that there is opportunity for level 1 to make use of level 0
      # shim types (e.g. in mkCtors where enableTypeChecking determines whether U_0.Ctor is a shim or not)
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
      checkTypeFixedUnderNew = level >= 5;
      # A thunk returning the options for the next-lower subuniverse.
      descend = NamedThunk "${name}.descend" (mkUniverseOpts (level + 1));
    };

    mkBootstrappedType = SU: U:
      with (log.v 3).call "mkBootstrappedType" SU U ___;
      let
        opts = U.opts;
      in rec {
        # Create the arguments to mkInstance for creating a Type.
        Type__args = assign "Type__args" (
          mkTypeArgsFor SU U);

        # Treat the arguments as an instantiated Type, despite the lack of bound methods.
        # This is sufficient for mkInstance. We only need Type to be set in addition,
        # which we can achieve via groundTypeUnsafe (unsafe since we need a TypeThunk and
        # Type__args is not itself a Type).
        Type__argsGrounded = assign "Type__argsGrounded" (
          groundType SU U Type__args);

        # We can then bootstrap a new Type by running mkInstance with This as Type__argsGrounded
        # and args as Type__args.
        Type__bootstrapped = assign "Type__bootstrapped" (
          U.mkInstance Type__argsGrounded Type__argsGrounded
        );

        # Construct Type through an actual application of the .mk constructor.
        # This is valid besides Type__mk.(Type {}).(Type {}).Type {} == Type__args, and after that,
        # Type__args ? __Type == false.
        # We fix this in the next stage.
        Type__new =
          assert checks [{ name = "Type__bootstrapped has mk";
                        cond = Type__bootstrapped ? mk;
                        msg = indent.block ''
                          Type__bootstrapped must have a mk function:
                            ${indent.here (log.print Type__bootstrapped)}
                          '';
                      }];
          assign "Type__new" (
            Type__bootstrapped.mk Type__argsGrounded
          );

        # Finally, ground this Type by setting Type.__Type to return itself, eliding any information
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
              then groundTypeAndAssertFixed SU U opts Type__args Type__new
              else groundType SU U Type__new
          );
      };

    # The barest minimum universe to bootstrap the type system.
    # Constructs a bootstrapped Quasitype from a hand-build GroundType instance, which has no
    # typed fields.
    Quasiverse =
      with (log.v 4).attrs "Quasiverse" ___;
      with msg "Constructing Quasiverse";

      let
        # Quasiverse is its own self- and super-universe, enabled by containing no circular dependencies
        # in quasitype construction.
        U = Quasiverse;
        SU = Quasiverse;

      in withCommonUniverseSelf rec {
        # Take on the intial options for a root universe.
        # All other opts are generated from here via 'resolve opts.descend'
        opts = mkUniverseOpts 0;

        __Bootstrap = mkBootstrappedType SU U;

        # Expose the final Type.
        Type = __Bootstrap.Type;

        # Simplest possible value shim standing in for String, Int, etc.
        # Does not have unresolved recursive shims.
        mkSimpleValueShim = typeName: value: {
          __Type = { 
            __resolve = self: {
              __isTypeSet = true;
              __TypeId = _: typeName;
              getBoundName = _: typeName;
              getName = _: typeName;
              name = typeName;
              getFields = _: [{value = BuiltinOf (toLower typeName);}];
            };
            __isTypeThunk = true;
          };
          getValue = _: value;
          __value.value = value;
        } // optionalAttrs (typeName == "Lambda") {
          __functor = self: arg: value arg;
        };

        # Create a builtin value shim without using any other machinery.
        # For top-level values used in creating TypeShim, from which Bool/Lambda etc shims are
        # created, so cannot themselves be used.
        mkValueShim = typeName: value: {
          __Type = { 
            __resolve = self: {
              __isTypeSet = mkSimpleValueShim "Bool" true;
              __TypeId = _: typeName;
              getBoundName = _: typeName;
              getName = _: typeName;
              name = typeName;
              getFields = _: [{value = BuiltinOf (toLower typeName);}];
            };
            # Simple shim to terminate recursion. Cannot go deeper than e.g.
            # value.__Type.__isTypeThunk.__Type.<__isTypeThunk
            __isTypeThunk = mkSimpleValueShim "Bool" true;
          };
          __isTypeSet = mkSimpleValueShim "Bool" true;
          getValue = _: value;
          __value.value = value;
        } // optionalAttrs (typeName == "Lambda") {
          __functor = self: arg: value arg;
        };

        # Create a shim for a field value.
        #mkFieldShim = 
        #  fieldName: fieldSpec:
        #    mkInstanceShim Field {
        #      rawFieldName = mkSimpleValueShim "String" fieldName;
        #      fieldSpec = {
        #        __resolve = self: fieldSpec;
        #        __isTypeThunk = mkSimpleValueShim "Bool" true;
        #      };
        #    } 
        #    {
        #      fieldName = _: fieldName;
        #      fieldType = _: fieldSpec;
        #      fieldStatic = _: false;
        #      fieldDefault = _: throw "mkFieldShim: fieldDefault used";
        #      hasDefault = _: false;
        #    };

        # Create a shim for the fields lambda of a type.
        #mkFieldsShim = fieldSpecs_: mkValueShim "Lambda" (_:
        #  mkInstanceShim Fields {
        #    fieldSpecs = withCommonFieldSpecs opts SU fieldSpecs_;
        #  } rec { 
        #    instanceFieldsWithType = _: mapSolos mkFieldShim fieldSpecs;
        #    instanceFields = _: mapSolos mkFieldShim fieldSpecs_;
        #  });

        # Create shim types appearing as instances of Type.
        mkTypeShim = name: attrs:
          let 
            get = {
              __isTypeSet = mkValueShim "Bool" true;
              __Super = mkValueShim "Null" null;
              name = mkValueShim "String" name;
              tvars = LazyAttrs {};
              tvarBindings = LazyAttrs {};
              ctor = { bind = _: attrs.new or throw "Shim type '${name}' has no 'new' fn"; };
              # Field/Fields must be extra-shimmed to avoid recursion when shimming Fields itself.
              # fields = mkFieldsShim (attrs.fields or []);
              # fields = Lambda (_: Fields.new (attrs.fields or []));
              fields = mkValueShim "Lambda" (_: rec { 
                fieldSpecs = withCommonFieldSpecs opts SU (attrs.fields or []);
                instanceFields = _: 
                  filter 
                    (s: !(elem (soloName s) ["__Type"]))
                    (mapSolos Field.new fieldSpecs);
              });
              methods = LazyAttrs {};
              staticMethods = LazyAttrs {};
              checkValue = mkValueShim "Null" null;
              overrideCheck = mkValueShim "Null" null;
            };
          T = mkInstanceShim TypeShim get (attrs // {
            __TypeId = _: name;
            getBoundName = _: name;
            getName = _: name;
            # Fields must be extra-shimmed to avoid recursion when shimming Fields itself.
            getFields = _: get.fields {};
            __toString = _: "TypeShim<${name}>";
            __show = _: "TypeShim<${name}>";
            check = _: true;
            # Ensure shims also operate as functors.
            __functor = self: arg: if T ? new then T.new arg else throw "__functor called on Type shim without .new: ${log.print T}";
          });
        in T // {
          fields = _: T.getFields {};
        };

        # Create shim types appearing as instances of ShimT.
        mkInstanceShim = ShimT: strictGet: attrs: {
          __Type = { __resolve = self: ShimT;
                     __isTypeThunk = mkValueShim "Bool" true;};
          inherit strictGet;
          get = mapAttrs (_: thunk) strictGet;
        } // attrs // strictGet;

        # A shimmed version of Type.
        # Any args are provided as attrs to mkTypeShim.
        TypeShim = mkTypeShim "Type" {
          new = _: throw "TypeShim.new used";
        };

        # A shimmed version of the inner builtin value.
        BuiltinOf = T: mkTypeShim "BuiltinOf<${T}>" rec {
          getName = _: "BuiltinOf";
          fields = [{value = T;}];
          new = x:
            let get = { value = x; }; in
            mkInstanceShim (BuiltinOf T) get {};
          mk = args: new args.value;  # To enable cast to work
        };

        # Create a shim for a builtin type.
        # Simulates casting in that e.g. Int.new 123 will convert 123 to a BuiltinOf Int shim.
        # and Int.new (Int.new 123) will also first unwrap x and do the same.
        mkBuiltinTypeShim = name:
          let
            builtinName = toLower name;
            BuiltinTypeShim = mkTypeShim name rec {
              fields = [{__value = BuiltinOf builtinName;}];
              new = x_:
                let unwrapper = x:
                      # We encounter solos like {__Type=...} which are not intended as being a Type instance.
                      if U.isUntypedAttrs x then x
                      else errors.try (U.BuiltinNameToUnwrapper.${name} x) (_: throw (indent.block ''
                        TypeShim<${name}>.new: Could not unwrap given value to ${builtinName}
                          Value: ${indent.here (log.print x_)}
                      ''));
                    x = unwrapper x_;
                    get = { __value = (BuiltinOf builtinName).new x; }; in
                mkInstanceShim BuiltinTypeShim get (rec {
                  getValue = _: x;
                  __toString = _: if name == "String" then x else log.print x;
                } // optionalAttrs (name == "Lambda") {
                  __functor = self: arg: x arg;
                });
              mk = args: new args.__value.value;  # To enable cast to work
            };
          in
            BuiltinTypeShim;

        BuiltinTypeShims = mapAttrs (BuiltinName: _: mkBuiltinTypeShim BuiltinName) U.BuiltinNameTobuiltinName;

        inherit (BuiltinTypeShims) Null Int Float String Path Bool List Set Lambda;

        Ctor = mkTypeShim "Ctor" {
          fields = [{name = String; ctor = Lambda;}];
          new = name: ctorFn:
            let get = {
              name = String.new name;
              ctor = Lambda.new ctorFn;
            }; in
            mkInstanceShim Ctor get {
              bind = This: ctorFn This;
            };
        };

        # Shim out all types used in the construction of Type s.t. Type can be created
        # with U == SU == Quasiverse.
        OrderedItem = T: mkTypeShim "OrderedItem" {
          fields = [{value = Sized 1 (SetOf T);}];
          new = x:
            let setOf = (SetOf T).new x;
                sized = (Sized 1 (SetOf T)).new setOf;
                get = { value = sized; }; in
            mkInstanceShim (OrderedItem T) get (rec {
              getName = _: soloName x;
              getValue = _: soloValue x;
              getSolo = _: get.value.getSized.getValue {};
            });
        };

        OrderedOf = T: mkTypeShim "OrderedOf<${toString T}>" {
          fields = [{value = ListOf (OrderedItem T);}];
          new = xs:
            let listOf = (ListOf (OrderedItem T)).new (map (OrderedItem T).new (solos xs));
                get = listOf.strictGet; in
            listOf // mkInstanceShim (OrderedOf T) get rec {
              getValue = _: get.__value.value;
              mapItems = f: map f (getValue {});
              getSolos = _: mapItems (item: item.getSolo {});
              indexed = _: mergeAttrsList (cutils.attrsets.indexed (getSolos {}));
              names = _: mapItems (item: item.getName {});
              values = _: mapItems (item: item.getValue {});
              update = items: (OrderedOf T).new (concatSolos (getSolos {}) (solos items));
            };
        };

        Field = mkTypeShim "Field" {
          fields = [{rawFieldName = String; fieldSpec = TypeThunk;}];
          new = rawFieldName: fieldSpec_:
            with (log.safe.v 3).call "FieldShim.new" { inherit rawFieldName fieldSpec_; } ___;
            let get = {
              rawFieldName = String.new rawFieldName;
              fieldSpec = TypeThunk.new fieldSpec_;
            }; in
            return (mkInstanceShim Field get rec {
              parsedT = _: U.parseFieldSpec fieldSpec_;
              fieldName = _: rawFieldName;
              fieldType = _: (parsedT {}).fieldType;
              fieldDefault = _: when (parsedT {}).hasDefault ((parsedT {}).fieldDefault);
              fieldStatic = _: (parsedT {}).fieldStatic;
              hasDefault = _: (parsedT {}).hasDefault;
            });
        };

        Fields =
          mkTypeShim "Fields" (
            with (log.safe.v 2).call "mkTypeShim.Fields" ___;
            (return {
              fields =
                [{value = ListOf (OrderedItem Field);}
                 {fieldSpecs = Lambda;}
                ];
              new = fieldSpecs_:
                # Safe mode to avoid lets/return from forcing fieldSpecs which may use Fields.
                with (log.safe.v 2).call "FieldsShim.new" "unsafe:fieldSpecs_" ___;
                # We do not merge with 'ordered //' here to avoid circularity.
                let fieldSpecs = withCommonFieldSpecs opts SU fieldSpecs_;
                    #fieldShims = mapSolos mkFieldShim fieldSpecs;
                    fieldShims = mapSolos Field.new fieldSpecs;
                    strictGet = {
                      #value = mapSolos (OrderedItem Field).new fieldShims;
                      fieldSpecs = mkValueShim "Lambda" (_: fieldSpecs);
                    };
                in return (mkInstanceShim Fields strictGet rec {
                  getValue = _: mapSolos (OrderedItem Field).new fieldShims;
                  getSolos = _: fieldShims;
                  indexed = _: mergeAttrsList (cutils.attrsets.indexed (getSolos {}));
                  # Override update here to avoid requiring use of this.set in OrderedOf super.
                  update = newFieldSpecs:
                    Fields.new (concatSolos (strictGet.fieldSpecs {}) (solos newFieldSpecs));
                  getField = name: (indexed {}).${name}.value;
                  getFieldsWhere = pred: filterSolos pred (getSolos {});
                  instanceFields = _:
                    getFieldsWhere (_: field:
                      ((U.string field.rawFieldName) != "__Type")
                      && (U.isNull field
                          || !(field.fieldStatic {})));
                  instanceFieldsWithType = _:
                    getFieldsWhere (_: field:
                      U.isNull field
                      || !(field.fieldStatic {}));
                  requiredFields = _:
                    getFieldsWhere (_: field:
                      U.isNull field
                      || (!(field.fieldStatic {})
                          && !(field.hasDefault {})));
                });
            })
          );

        ThunkOf = T: mkTypeShim "ThunkOf<${toString T}>" rec {
          fields = [{thunk = U.Lambda;}];
          getName = _: "ThunkOf";
          new = x:
            let get = { thunk = U.Lambda.new (_: x); }; in
            mkInstanceShim (ThunkOf T) get rec {
              __resolve = self: get.thunk;
            };
        };

        TypeThunk = mkTypeShim "TypeThunk" rec {
          fields = [{thunk = Lambda; __isTypeThunk = Bool;}];
          new = T:
            let thunkOf = (ThunkOf TypeShim).new T;
                get = thunkOf.strictGet // { __isTypeThunk = Bool.new true; }; in
            thunkOf // mkInstanceShim TypeThunk get {};
        };

        SetOf = T: mkTypeShim "SetOf<${toString T}>" {
          fields = [{value = SetOf T;}];
          getName = _: "SetOf";
          new = x:
            let set = Set.new x;
                get = set.strictGet; in
            set // mkInstanceShim (SetOf T) get {};
        };

        ListOf = T: mkTypeShim "ListOf<${toString T}>" {
          fields = [{value = ListOf T;}];
          getName = _: "ListOf";
          new = x:
            let list = U.List.new x;
                get = list.strictGet; in
            list // mkInstanceShim (ListOf T) get {};
        };

        Constraint = mkTypeShim "Constraint" {
          fields = [{constraintType = TypeThunk;}];
          new = x:
            let get = { constraintType = U.TypeThunk.new x; }; in
            mkInstanceShim Constraint get {
              satisfiedBy = _: true;
            };
        };

        Literal = V: mkTypeShim "Literal<${log.print V}>" {
          fields = [{value = V;}];
          getName = _: "Literal";
          new = _:
            let get = {}; in
            mkInstanceShim (Literal V) get {
              getLiteral = _: V;
            };
          getLiteral = _: V;
        };

        Sized = n: T: mkTypeShim "Sized<${toString (Literal n)}, ${toString T}>" {
          fields = [{getSized = T;}];
          getName = _: "Sized";
          new = x:
            let get = { getSized = x; }; in
            mkInstanceShim (Sized n T) get {};
        };

        # Uninstantiable / type-level / field-type-only
        Any = mkTypeShim "Any" {};
        Unit = mkTypeShim "Unit" {};
        Void = mkTypeShim "Void" {};

        Static = T: mkTypeShim "Static<${toString T}>" { 
          getName = _: "Static";
          staticType = _: T;
        };

        Default = T: V: mkTypeShim "Default<${toString T}, ${toString (Literal V)}>" {
          getName = _: "Default";
          defaultType = _: T;
          defaultValue = _: V;
        };

        Union = Ts: mkTypeShim "Union<${toString (Literal Ts)}>" {
          getName = _: "Union";
        };

        NullOr = T: mkTypeShim "NullOr<${toString T}>" {
          getName = _: "NullOr";
        };
      };

    # Create a new universe descending from SU.
    #
    # At first, SU == U_0 == Quasiverse, and contains:
    #
    # - Shims for all Builtins and all component types
    #   - Constructed from each other, or pseudo-typed shims that do not recursively use
    #     other parts of U_0
    # - A functioning Type type, constructed out of U_0 shim instances
    # - Utility / Typelib functions for operating over the types in U_0 (e.g. isType, typeEq)
    #
    # A sub-universe U can then make full use of SU's Type and its components when constructing
    # its own versions of Type and those components.
    #
    # Before SU.Type is created, we first create U's common components.
    #
    # - Typelib
    # - Casting
    # - Shared Constructors
    # - Inheritance
    # - Instantiation (i.e. mkInstance)
    # - Builtin utilities
    #
    # These tertiary parts of U can only depend on and make use of the elements of SU.
    # Reaching forward after Type is created into the final U reference causes circularity,
    # as U.Type's construction depends on these elements.
    #
    # Next we create U.Type in terms of SU and the above tertiary parts of U.
    #
    # Finally we can construct the remainder of U as instances of U.Type:
    # - Ctor
    # - Builtins
    # - Trivials
    # - Components (Field, Fields, Ctor, etc)
    # 
    # These are ordered such that we have minimal need to reach back up to SU to resolve circularity.
    mkSubUniverse = SU:
      let opts = resolve SU.opts.descend; in
      with (log.v 4).call "mkSubUniverse" opts "<SU>" ___;
      with msg "Constructing ${opts.name} universe";
      let U = withCommonUniverse SU (rec {
        # Store the opts as U.opts
        inherit opts;

        ### Type

        __Bootstrap = mkBootstrappedType SU U;
        Type = __Bootstrap.Type;


        ### Ctor

        Ctor = Type.new "Ctor" {
          # Manually cast here for universes before typechecking to ensure homogeneity of
          # ctor lambda. Ctor cannot have a Ctor of itself, so uses SU.Ctor.
          ctor = SU.Ctor.new "CtorCtor" (This: name: ctorFn: {
            name = SU.String.new name;
            ctor = SU.Lambda.new ctorFn;
          });
          fields = This: SU.Fields.new [
            { name = SU.String; }
            { ctor = SU.Lambda; }
          ];
          methods = {
            bind = this: This: this.ctor.getValue {} This;
          };
        };

        ### Builtins

        AnyBuiltin = SU.Union SU.builtinNames;

        BuiltinOf_ = Type.template "BuiltinOf" { T = AnyBuiltin; } (_: {
          fields = This: SU.Fields.new [{ value = _.T; }];
        });
        BuiltinOf = T: BuiltinOf_.bind { inherit T; };

        mkBuiltinType = name:
          let
            withGetValue = methods: methods // {
              getValue = this: _: this.__value.value;
            };

            withToString = methods:
              let
                toStringF = {
                  String = self: self.getValue {};
                  Int = self: toString (self.getValue {});
                  Float = self: toString (self.getValue {});
                  Path = self: toString (self.getValue {});
                  Lambda = self: "_: ...";
                  Bool = self: boolToString (self.getValue {});
                  Set = self: log.vprintD 1 (self.getValue {});
                  List = self: log.vprintD 2 (self.getValue {});
                  Null = self: "";
                }.${name};
              in
                methods // {
                  # show (Int.new 6) returns e.g. "Int(6)"
                  __implements__show = this: self: "${U.typeIdOf self}(${toStringF self})";
                  # toString (Int.new 6) returns e.g. "6"
                  __implements__toString = this: self:
                    with (log.v 2).methodCall this "__toString" self ___;
                    return (toStringF self);
                };

            hasSize = { String = true; Path = true; List = true; Set = true; }.${name} or false;
            withSize = methods:
              if hasSize
              then let sizeFn = this: _: size (this.getValue {});
                  in methods // { size = sizeFn; }
              else methods;
          in
            Type.new name {
              ctor = Ctor.new "Ctor${name}" (This: x: {
                __value = (BuiltinOf (toLower name)).new x;
              });
              fields = This: SU.Fields.new [{
                __value = BuiltinOf (toLower name);
              }];
              methods = withToString (withSize (withGetValue ({
                List = {
                  fmap = this: f: this.modify.__value (this: this.modify.value (map f));
                  append = this: x: this.modify.__value (this: this.modify.value (xs: xs ++ [x]));
                };
                Set = {
                  fmap = this: f: this.modify.__value (this: this.modify.value (mapAttrs (_: f)));
                  names = this: _: attrNames (this.getValue {});
                  values = this: _: attrValues (this.getValue {});
                };
                Lambda = {
                  fmap = this: f: this.modify.__value (this: this.modify.value (compose f));
                  # Enable Lambda instances to be used as functors.
                  __implements__functor = this: self: arg: U.call this arg;
                };
              }.${name} or {})));
              # checkValue = that: true;
              checkValue = that: {
                # Additional check on sets s.t. we don't accept a typed value when expecting
                # a raw set.
                Set = !((that.getValue {}) ? __Type);
              }.${name} or true;
            };

        BuiltinTypes = mapAttrs (BuiltinName: _: mkBuiltinType BuiltinName) U.BuiltinNameTobuiltinName;

        inherit (BuiltinTypes) Null Int Float String Path Bool List Set Lambda;


        ### Trivial

        # Unit Type
        Unit = Type.new "Unit" {
          ctor = U.Ctors.CtorNullary;
        };
        unit = Unit.new {};

        # Uninhabited type
        Void = Type.new "Void" {
          ctor = Ctor.new "CtorVoid" (_: thunk (throw "Void: ctor"));
        };

        # Any type
        # Used in Type fields (via Literal binding) so must be SU.Type.
        Any = Type.new "Any" {overrideCheck = _: true;};

        ### Field Types

        # A type inhabited by only one value.
        Literal_ = Type.template "Literal" {V = Any;} (_: rec {
          ctor = U.Ctors.CtorNullary;
          staticMethods = {
            getLiteral = This: thunk _.V;
          };
        });
        Literal = V: Literal_.bind { inherit V; };
        literal = v: (Literal v).new {};

        # A type satisfied by any value of the given list of types.
        Union_ = Type.template "Union" {Ts = Type;} (_: {
          ctor = U.Ctors.None;
          overrideCheck = that: any (T: U.isType T that) (_.Ts.getLiteral {});
        });
        Union = Tlist:
          let Ts = Literal Tlist;
          in Union_.bind {inherit Ts;};

        # A type inhabited by literals of any of the given list of values
        Literals = Vs: Union (map Literal values);

        # A value or T or Null.
        NullOr = T: Union ["null" Null T];

        # A type indicating a default field type and value.
        Default_ = Type.template "Default" {T = Type; V = Type;} (_: {
          ctor = U.Ctors.None;
          staticMethods = {
            defaultType = This: thunk _.T;
            defaultValue = This: thunk (_.V.getLiteral {});
          };
          overrideCheck = that: _.T == null || _.T.check that;
        });
        Default = T: v:
          let V = Literal v;
          in Default_.bind { inherit T V; };

        # A type indicating a static field type.
        Static_ = Type.template "Static" {T = Type;} (_: {
          ctor = U.Ctors.None;
          staticMethods = {
            staticType = This: thunk _.T;
          };
        });
        Static = T: Static_.bind {inherit T;};

        ### Components

        ThunkOf_ = Type.template "ThunkOf" {T = Type;} (_: {
          fields = This: SU.Fields.new [{ thunk = Lambda; }];
          ctor = Ctor.new "CtorThunkOf" (This: x: {
            thunk = Lambda (_: x);
          });
          methods = {
            __resolve = this: self: self.thunk {};
          };
          checkValue = resolvesWith (U.isType _.T);
        });
        ThunkOf = T: ThunkOf_.bind { inherit T; };

        # TypeThunk cannot have a __Type field of type TypeThunk, because
        # we then have a cascade of non-lazy __Type.__Type.__Type.
        # Instead, since we only ever access the resolvable property of __Type,
        # fake this by unsafely injecting a regular resolvable {__resolve = _: ...}
        # DANGER ZONE
        TypeThunk = (ThunkOf Type).subType "TypeThunk" {
          # __Type = _: Type; # DANGER ZONE
          __Type = SU.TypeThunk.new Type;
          fields = This: SU.Fields.new [{
            __isTypeThunk = Default Bool (Bool true);
          }];
          ctor = Ctor.new "CtorTypeThunk" (This: T:
            let super = (ThunkOf Type).ctor.bind This T;
            in super // { __Type = SU.TypeThunk.new Type; });  # DANGER ZONE
        };

        # Subtype of List that enforces homogeneity of element types.
        ListOf_ = List.subTemplate "ListOf" {T = Type;} (_: {
          checkValue = that: all (x: U.isType _.T x) (that.getValue {});
        });
        ListOf = T: ListOf_.bind { inherit T; };

        # Subtype of Set that enforces homogeneity of value types.
        SetOf_ = Set.subTemplate "SetOf" {T = Type;} (_: {
          checkValue = that: all (x: U.isType _.T x) (that.values {});
        });
        SetOf = T: SetOf_.bind { inherit T; };

        # A type that enforces a size on the value.
        Sized_ = Type.template "Sized" {N = Type; T = Type;} (_: {
          fields = This: SU.Fields.new [{ getSized = _.T; }];
          checkValue = that:
            (_.T.checkValue or (const true)) that
            && (that.size {}) == _.N.getLiteral {};
        });
        Sized = n: T:
          let N = Literal n;
          in Sized_.bind { inherit N T; };

        # An attribute set with attributes zero-indexed by their definition order.
        # xs = Ordered.new [ {c = 1;} {b = 2;} {a = 3;} ];
        # xs.value == { a = 1; b = 2; c = 3; } (arbitrary order)
        # xs.names == [ "c" "b" "a" ] (in order of definition)
        # xs.values == [ 1 2 3 ] (in order of definition)
        OrderedItem_ = Type.template "OrderedItem" { T = Type; } (_: {
          # TODO: Could be a unary cast, or inherit from Sized 1 (SetOf T)
          ctor = Ctor.new "CtorOrderedItem" (This: x: {
            value =
              let setOfX = ((SetOf _.T).new x);
                  sizedSetOfX = (Sized 1 (SetOf _.T)).new setOfX;
              in sizedSetOfX;
          });

          fields = This: SU.Fields.new [{
            value = Sized 1 (SetOf _.T);
          }];

          methods = {
            getSolo = this: _: this.value.getSized.getValue {};
            getName = this: _: soloName (this.getSolo {});
            getValue = this: _: soloValue (this.getSolo {});
          };
        });
        OrderedItem = T: OrderedItem_.bind { inherit T; };

        OrderedOf_ = Type.subTemplateOf (_: ListOf (OrderedItem _.T)) "OrderedOf" {T = Type;} (_: {
          ctor = Ctor.new "CtorOrderedOf" (This: xs:
            # Pass OrderedItems to the underlying ListOf
            (ListOf (OrderedItem _.T)).ctor.bind
              This
              (map (x: (OrderedItem _.T).new x) (solos xs)));

          methods = {
            # Map over the underlying [OrderedItem T]
            # f :: (OrderedItem T -> a) -> [a]
            mapItems = this: f: map f (this.getValue {});

            # Get the solo attr list in order.
            getSolos = this: _: this.mapItems (item: item.getSolo {});

            # The merged attribute set with an additional 'index' field indicating
            # its place in the order.
            indexed = this: _: mergeAttrsList (indexed (this.getSolos {}));

            # The ordered attribute names.
            names = this: _: this.mapItems (item: item.getName {});

            # The ordered attribute values.
            values = this: _: this.mapItems (item: item.getValue {});

            # Update by inserting the given items sequentially at the end of the order.
            # If any already appear, they update the item in its original order.
            update = this: items:
              let newOrdered = (OrderedOf _.T).new (concatSolos (this.getSolos {}) (solos items));
              in this.set.__value (newOrdered.__value);
          };

          checkValue = that:
            # TODO: Make a __checkValue implicit method
            thunkDo (typeOf that).__Super (Super:
              Super.checkValue that
              && assertMsg
                (size (that.names {}) == size (that.indexed {}))
                "Duplicate keys in OrderedOf: ${joinSep ", " (that.names {})}");
        });
        OrderedOf = T: OrderedOf_.bind { inherit T; };

        # Either:
        # (Field.new "myField" Int
        # (Field.new "myField" (Static Int)) -> Field<Static<Int>>.fieldType == Int
        # (Field.new "myField" (Default Int 123)) -> Field<Default<Int, 123>>.fieldType == Int
        # (Field.new "myField" (Static (Default Int 123))) -> Field<Static<Default<Int, 123>>>.fieldType == Int
        Field = Type.new "Field" {
          ctor = Ctor.new "CtorField" (This: rawFieldName: fieldSpec:
            with (log.v 2).call "CtorField" rawFieldName fieldSpec ___;
            return {
              rawFieldName = String.new rawFieldName;
              fieldSpec = TypeThunk.new fieldSpec;
            });
          fields = This: SU.Fields.new [
            {rawFieldName = String;}
            {fieldSpec = TypeThunk;}
          ];
          methods = {
            parsedT = this: _: U.parseFieldSpec (resolve this.fieldSpec);
            fieldName = this: _: U.string this.rawFieldName;
            fieldType = this: _: (this.parsedT {}).fieldType;
            fieldDefault = this: _: when (this.hasDefault {}) (this.parsedT {}).fieldDefault;
            fieldStatic = this: _: (this.parsedT {}).fieldStatic;
            hasDefault = this: _: (this.parsedT {}).hasDefault;
          };
        };

        # Fields is an OrderedOf that first converts any RHS values into Field types.
        # Since it needs to inherit OrderedOf
        Fields = (OrderedOf Field).subType "Fields" {
          # Fields ctor just converts the incoming list or set of field specs into
          # the list of Field that OrderedOf Field expects.
          #
          # Fields.new { field = FieldType; ... }
          # Fields.new { field = Default FieldType defaultValue; ... }
          # Fields.new { field = Static FieldType; ... }
          # Fields.new [ { field = FieldType; ... } ... ]
          # Fields.new [ { field = Default FieldType defaultValue; ... } ... ]
          # Fields.new [ { field = Static FieldType; ... } ... ]
          fields = This: SU.Fields.new [{
            fieldSpecs = Lambda;
          }];
          ctor = Ctor.new "Fields.ctor" (This: fieldSpecs_:
              with (log.v 2).call "Fields.ctor" fieldSpecs_ ___;
              with lets rec {
                # Include Type field on all instances.
                fieldSpecs = withCommonFieldSpecs opts SU fieldSpecs_;
                fieldSolos = mapSolos Field.new fieldSpecs;
              };
              # Call the OrderedOf constructor to construct this from the list of solos.
              return (
                ((OrderedOf Field).ctor.bind This fieldSolos) // {
                  fieldSpecs = Lambda.new (thunk fieldSpecs);
                }
              )
          );
          methods = {
            getField = this: name: (this.indexed {}).${name}.value;
            getFieldsWhere = this: pred: filterSolos pred (this.getSolos {});
            instanceFields = this: _:
              this.getFieldsWhere (rawFieldName: field:
                ((U.string field.rawFieldName) != "__Type")
                && (U.isNull field
                    || !(field.fieldStatic {})));
            instanceFieldsWithType = this: _:
              this.getFieldsWhere (_: field:
                (U.isNull field)
                || !(field.fieldStatic {}));
            requiredFields = this: _:
              this.getFieldsWhere (_: field:
                (U.isNull field)
                || (!(field.fieldStatic {})
                    && !(field.hasDefault {})));
            update = this: newFieldSpecs:
              let newFields =
                Fields.new (concatSolos
                  ((U.lambda this.fieldSpecs) {})
                  (solos newFieldSpecs));
              in this.set.__value (newFields.__value);
          };
        };


        ### Independent (Only ever accessed as e.g. SU.Constraint) 

        # A constraint on a type variable.
        Constraint = Type.new "Constraint" {
          fields = This: Fields.new [
            { constraintType = TypeThunk; }
          ];
          methods = {
            # Whether a given type variable binding satisfies the constraint.
            # If the constraint is unbound, we treat as satisfied, but instantiating the unbound type
            # will throw an error.
            satisfiedBy = this: That:
              U.typeEq Void That
              || That.isInstance (resolve this.constraintType)
              || (resolve this.constraintType).check That;
          };
        };

        # Base type for enums.
        Enum = Type.new "Enum" {};

        # Create an enum type from a list of item names.
        # MyEnum = mkEnum "MyEnum" [ "Item1" "Item2" "Item3" ]
        # MyEnum.names == [ "Item1" "Item2" "Item3" ]
        # MyEnum.fromName "Item1" == 0
        # MyEnum.fromIndex 0 == "Item1"
        mkEnum = enumName: itemNames:
          let Item = Enum.subType enumName {
                fields = This: Fields.new [
                  {i = Int;}
                  {name = String;}
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

      });

      in U;
  };

  # nix eval --impure --expr '(import ./cutils/types.nix {})._tests'
  _tests =
    with cutils.tests;
    with lib;  # lib == untyped default pkgs.lib throughout __tests
    with Types;
    with Universe;

    let
      testInUniverse = U: test: test U;
      testInUniverses = Us: test: mapAttrs (_: U: testInUniverse U test) Us;

      TestTypes = U: with U; {
        MyType =
          assert Type ? new;
          assert Fields ? new;
          Type.new "MyType" {
            fields = This: Fields.new [{ myField = String; }];
            methods = {
              helloMyField = this: extra: "Hello, ${this.myField.getValue {}}${extra}";
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
            methods = {
              getValue = this: _: this.value.getValue {};
            };
          };

        Mystring =
          assert Type ? new;
          assert Fields ? new;
          Type.new "Mystring" {
            fields = This: Fields.new [{ value = "string"; }];
            methods = {
              getValue = this: _: this.value;
            };
          };

        WrapString =
          assert String ? subType;
          String.subType "WrapString" {};
      };

      TypelibTests = U: with U; let SU = U._SU; in {
        isTypeSet = {
          Type = expect.True (isTypeSet Type);
          newType = expect.True (isTypeSet (Type.new "T" {}));
          TypeThunk = expect.True (isTypeSet TypeThunk);
          newTypeThunk = expect.False (isTypeSet (TypeThunk.new Type));
          Int = expect.True (isTypeSet Int);
          int = expect.False (isTypeSet "int");
          newInt = expect.False (isTypeSet (Int.new 3));
          Null = expect.True (isTypeSet Null);
          null = expect.False (isTypeSet null);
          newNull = expect.False (isTypeSet (Nil));
        };

        isTypeLike = {
          Type = expect.True (isTypeLike Type);
          newType = expect.True (isTypeLike (Type.new "T" {}));
          TypeThunk = expect.True (isTypeLike TypeThunk);
          newTypeThunk = expect.False (isTypeLike (TypeThunk.new Type));
          Int = expect.True (isTypeLike Int);
          int = expect.True (isTypeLike "int");
          xyz = expect.False (isTypeLike "xyz");
          newInt = expect.False (isTypeLike (Int.new 3));
          Null = expect.True (isTypeLike Null);
          null = expect.False (isTypeLike null);
          newNull = expect.False (isTypeLike (Nil));
        };

        isTypeLikeOrNull = {
          Type = expect.True (isTypeLikeOrNull Type);
          newType = expect.True (isTypeLikeOrNull (Type.new "T" {}));
          TypeThunk = expect.True (isTypeLikeOrNull TypeThunk);
          newTypeThunk = expect.False (isTypeLikeOrNull (TypeThunk.new Type));
          Int = expect.True (isTypeLikeOrNull Int);
          int = expect.True (isTypeLikeOrNull "int");
          xyz = expect.False (isTypeLikeOrNull "xyz");
          newInt = expect.False (isTypeLikeOrNull (Int.new 3));
          Null = expect.True (isTypeLikeOrNull Null);
          null = expect.True (isTypeLikeOrNull null);
          newNull = expect.True (isTypeLikeOrNull (Nil));
        };

        isTyped = {
          Type = expect.True (isTyped Type);
          newType = expect.True (isTyped (Type.new "T" {}));
          TypeThunk = expect.True (isTyped TypeThunk);
          newTypeThunk = expect.True (isTyped (TypeThunk.new Type));
          Int = expect.True (isTyped Int);
          int = expect.False (isTyped "int");
          intInstance = expect.False (isTyped 123);
          xyz = expect.False (isTyped "xyz");
          newInt = expect.True (isTyped (Int.new 3));
          Null = expect.True (isTyped Null);
          null = expect.False (isTyped null);
          newNull = expect.True (isTyped (Nil));
        };

        isAttrs = solo {
          set.empty.typed = expect.True (isAttrs {});
          set.empty.untyped = expect.True (isUntypedAttrs {});
          Set.empty.typed = expect.True (isAttrs (Set {}));
          Set.empty.untyped = expect.True (isUntypedAttrs (Set {}));
          set.solo.Type.typed = expect.True (isAttrs {__Type = _: "string";});
          set.solo.Type.untyped = expect.True (isUntypedAttrs {__Type = _: "string";});
          Set.solo.Type.typed = expect.True (isAttrs (Set {__Type = _: "string";}));
          Set.solo.Type.untyped = expect.True (isUntypedAttrs (Set {__Type = _: "string";}));
          int.typed = expect.False (isAttrs 123);
          int.untyped = expect.False (isUntypedAttrs 123);
          Int.typed = expect.False (isAttrs (Int 123));
          Int.untyped = expect.False (isUntypedAttrs (Int 123));
        };

        typeEq = {
          TypeType = expect.True (typeEq Type Type);
          StringString = expect.True (typeEq String String);
          StringType = expect.False (typeEq String Type);
          invalidString = expect.False (typeEq "xyz" String);
          stringstring = expect.True (typeEq "string" "string");
          stringString = expect.False (typeEq "string" String);
          interuniverse = expect.True (typeEq U.String SU.String);
          boundSame = expect.True (typeEq (Literal 1) (Literal 1));
          boundDiff = expect.False (typeEq (Literal 1) (Literal 2));
        };

        isType = {
          TypeType = expect.True (isType Type Type);
          TypeString = expect.True (isType Type String);
          Typestring = expect.False (isType Type "string");
          newType = expect.True (isType Type (Type.new "T" {}));
          TypeThunk = expect.True (isType Type TypeThunk);
          newTypeThunk = expect.True (isType TypeThunk (TypeThunk.new Type));
          string = expect.True (isType "string" "xyz");
          xyzstring = expect.error (isType "xyz" "no");
          xyznull = expect.error (isType "xyz" null);
          nullnullLit = expect.error (isType null null);
          nullnull = expect.True (isType "null" null);
          Nullnull = expect.False (isType Null null);
          NullNull = expect.True (isType Null (Nil));
          Int = expect.True (isType Type Int);
          newInt = expect.True (isType Int (Int.new 3));
          IntintInstance = expect.False (isType Int 123);
          intstring = expect.False (isType "int" "no");
          intInstance = expect.True (isType "int" 123);
        };

        typeOf = {
          Type = expect.eqWith typeEq (typeOf Type) Type;
          newType = expect.eqWith typeEq (typeOf (Type.new "T" {})) Type;
          TypeThunk = expect.eqWith typeEq (typeOf TypeThunk) Type;
          newTypeThunk = expect.eqWith typeEq (typeOf (TypeThunk.new Type)) TypeThunk;
          Int = expect.eqWith typeEq (typeOf Int) Type;
          int = expect.eqWith typeEq (typeOf "int") "string";
          newInt = expect.eqWith typeEq (typeOf (Int.new 3)) Int;
          intInstance = expect.eqWith typeEq (typeOf 3) "int";
          Null = expect.eqWith typeEq (typeOf Null) Type;
          null = expect.eqWith typeEq (typeOf null) "null";
          newNull = expect.eqWith typeEq (typeOf (Nil)) Null;
          set = expect.eqWith typeEq (typeOf {}) "set";
          Set = expect.eqWith typeEq (typeOf (Set.new {})) Set;
          TypedSet = expect.eqWith typeEq (typeOf {__Type = _: "string";}) "string";
        };

        typeIdOf = {
          Type = expect.eq (typeIdOf Type) "Type";
          newType = expect.eq (typeIdOf (Type.new "T" {})) "Type";
          TypeThunk = expect.eq (typeIdOf TypeThunk) "Type";
          newTypeThunk = expect.eq (typeIdOf (TypeThunk.new Type)) "TypeThunk";
          Int = expect.eq (typeIdOf Int) "Type";
          int = expect.eq (typeIdOf "int") "string";
          newInt = expect.eq (typeIdOf (Int.new 3)) "Int";
          intInstance = expect.eq (typeIdOf 3) "int";
          Null = expect.eq (typeIdOf Null) "Type";
          null = expect.eq (typeIdOf null) "null";
          newNull = expect.eq (typeIdOf (Nil)) "Null";
          set = expect.eq (typeIdOf {}) "set";
          Set = expect.eq (typeIdOf (Set.new {})) "Set";
          TypedSetString = expect.eq (typeIdOf {__Type = _: "string";}) "string";
          TypedSetThunk = expect.eq (typeIdOf {__Type = _: {__TypeId = _: "string";};}) "string";
        };

        typeNameOf = {
          Type = expect.eq (typeNameOf Type) "Type";
          newType = expect.eq (typeNameOf (Type.new "T" {})) "Type";
          TypeThunk = expect.eq (typeNameOf TypeThunk) "Type";
          newTypeThunk = expect.eq (typeNameOf (TypeThunk.new Type)) "TypeThunk";
          Int = expect.eq (typeNameOf Int) "Type";
          int = expect.eq (typeNameOf "int") "string";
          newInt = expect.eq (typeNameOf (Int.new 3)) "Int";
          intInstance = expect.eq (typeNameOf 3) "int";
          Null = expect.eq (typeNameOf Null) "Type";
          null = expect.eq (typeNameOf null) "null";
          newNull = expect.eq (typeNameOf (Nil)) "Null";
          set = expect.eq (typeNameOf {}) "set";
          Set = expect.eq (typeNameOf (Set.new {})) "Set";
          TypedSetstring = expect.eq (typeNameOf {__Type = _: "fake";}) "fake";
          TypedSetThunk = expect.eq (typeNameOf {__Type = _: {name = String.new "fake";};}) "fake";
        };

        typeBoundNameOf = {
          Type = expect.eq (typeBoundNameOf Type) "Type";
          newType = expect.eq (typeBoundNameOf (Type.new "T" {})) "Type";
          TypeThunk = expect.eq (typeBoundNameOf TypeThunk) "Type";
          newTypeThunk = expect.eq (typeBoundNameOf (TypeThunk.new Type)) "TypeThunk";
          Int = expect.eq (typeBoundNameOf Int) "Type";
          int = expect.eq (typeBoundNameOf "int") "string";
          newInt = expect.eq (typeBoundNameOf (Int.new 3)) "Int";
          intInstance = expect.eq (typeBoundNameOf 3) "int";
          Null = expect.eq (typeBoundNameOf Null) "Type";
          null = expect.eq (typeBoundNameOf null) "null";
          newNull = expect.eq (typeBoundNameOf (Nil)) "Null";
          set = expect.eq (typeBoundNameOf {}) "set";
          Set = expect.eq (typeBoundNameOf (Set.new {})) "Set";
          TypedSetstring = expect.eq (typeBoundNameOf {__Type = _: "fake";}) "fake";
          TypedSetThunk = expect.eq (typeBoundNameOf {__Type = _: {getBoundName = _: "fake";};}) "fake";
        };
      };

      smokeTests = U: with U; let SU = U._SU; in {

        Type =
          assert Type ? new;
          let A = Type.new "A" {}; in
          assert A ? __Type;
          assert A ? __TypeId;
          assert (resolve A.__Type) ? __TypeId;
          assert A ? name;
          assert A ? getBoundName;
          {
            Type = expect.stringEq ((resolve A.__Type).__TypeId {}) "Type";
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

        TypeThunk =
          let T = Type.new "SomeType" {};
              TT = TypeThunk.new T; in
          expect.fieldsEq (resolve TT) T;

        OrderedItem =
          let
            validOI = ((OrderedItem String).new { abc = "xyz"; });
          in {
            getName = expect.eq (validOI.getName {}) "abc";
            getValue = expect.eq (validOI.getValue {}) "xyz";
            getSolo = expect.eq (validOI.getSolo {}) {abc = "xyz";};
          };

        OrderedOf =
          let
            validOO = (OrderedOf String).new [{c = "c";} {b = "b";} {a = "a";}];
          in {
            getValue =
              expect.noLambdasEq
                (validOO.getValue {})
                [((OrderedItem String).new {c = "c";})
                ((OrderedItem String).new {b = "b";})
                ((OrderedItem String).new {a = "a";})];
            mapItems = expect.eq (validOO.mapItems (item: item.getName {})) ["c" "b" "a"];
            getSolos = expect.eq (validOO.getSolos {}) [{c = "c";} {b = "b";} {a = "a";}];
            indexed = expect.eq (validOO.indexed {}) {
              a = { index = 2; value = "a"; };
              b = { index = 1; value = "b"; };
              c = { index = 0; value = "c"; };
            };
            updateSet =
              expect.eq
                ((validOO.update {b = "B"; d = "d";}).getSolos {})
                [{c = "c";} {b = "B";} {a = "a";} {d = "d";}];
            updateList =
              expect.eq
                ((validOO.update [{b = "B";} {d = "d";}]).getSolos {})
                [{c = "c";} {b = "B";} {a = "a";} {d = "d";}];
            names = expect.eq (validOO.names {}) ["c" "b" "a"];
          values = expect.eq (validOO.values {}) ["c" "b" "a"];
        };

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
            name = expect.eq (f.fieldName {}) "name";
            fieldType = expect.eq (f.fieldType {}) null;
            fieldStatic = expect.False (f.fieldStatic {});
            fieldDefault = expect.eq (f.fieldDefault {}) null;
          };

        Fields =
          assert Fields ? new;
          let
            testFields = fields: args: {
              getSolos = expect.noLambdasEq (fields.getSolos {}) args.expectedSolos;
              # indexed = mergeAttrsList (cutils.attrsets.indexed fieldSolos);
              # update = newFieldSpecs:
              #   Fields.new (concatSolos (solos fieldSpecs) (solos newFieldSpecs));
              # getField = name: indexed.${name}.value;
              # getFieldsWhere = pred: filterSolos pred (getSolos {});
              # instanceFields = _:
              #   getFieldsWhere (fieldName: field:
              #     fieldName != "__Type"
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
            expectedValues = rec {
              TypeField = Field.new "__Type" TypeThunk;
              aField = Field.new "a" null;
              bField = Field.new "b" "int";
              cField = Field.new "c" (Default Int 3);
              dField = Field.new "d" (Static Int);
              expectedSolos = [{__Type = TypeField;} {a = aField;} { b = bField;} {c = cField;} {d = dField;}];
            };
          in
          {
            fromSolos =
              testFields 
                (Fields.new [{a = null;} {b = "int";} {c = Default Int 3;} {d = Static Int;}])
                expectedValues;
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
                name = expect.stringEq (thunkDo x.__Type (T: T.name.getValue {})) name;
                value = expect.eq
                  (if name == "Lambda" then null else x.getValue {})
                  (if name == "Lambda" then null else rawValue);
                newIsBuiltin =
                  expect.False (isbuiltinValue x);
                rawIsBuiltin =
                  expect.True (isbuiltinValue (x.getValue {}));
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

          isbuiltinValue = {
            Type = {
              expr = isbuiltinValue Type;
              expected = false;
            };
            int = {
              expr = isbuiltinValue 123;
              expected = true;
            };
            set = {
              expr = isbuiltinValue {abc="xyz";};
              expected = true;
            };
            Bool =
              expect.False (
                assert Bool ? new;
                isbuiltinValue (Bool.new true)
              );
          };

          Lambda = {
            invoke.directly = expect.eq ((Lambda.new (x: x+1)).getValue {} 2) 3;
            invoke.call = expect.eq (call (Lambda.new (x: x+1)) 2) 3;
            invoke.functor.new = expect.eq (Lambda.new (x: x+1) 2) 3;
            invoke.functor.functor = expect.eq (Lambda (x: x+1) 2) 3;
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
              [ "__Type" "a" "b" "c" ];
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

          mkToTypedBuiltinTest = T: x: (
            expect.valueEq
              (cast_ T x)
              (assert T ? new; T.new x)
          );

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
            valid = expect.eq ((Int.new 123).getValue {}) 123;
            invalid = expect.error (Int.new "123");
          };

          A = {
            valid =
              let x = A.new 1 (Int.new 2) (Int.new 3) (Int.new 4);
              in
                expect.eq
                  [x.a (x.b.getValue {}) (x.c.getValue {}) (x.d.getValue {})]
                  [1 2 3 4];
            #wrongType = {
            #  a = expect.error (A.new "1" 2 3 4);
            #  b = expect.error (A.new 1 "2" 3 4);
            #  c = expect.error (A.new 1 2 "3" 4);
            #  d = expect.error (A.new 1 2 3 "4");
            #};
          };

          castInMk = with TestTypes U; {
            MyString = {
              mkFromstring = expect.eq ((MyString.mk { value = "hello"; }).getValue {}) "hello";
              newFromstring = expect.eq ((MyString.new "hello").getValue {}) "hello";
              eqString = expect.valueEq (MyString.new "hello") (String.new "hello");
            };

            WrapString = {
              mkFromstring = expect.eq ((WrapString.mk { __value = "hello"; }).getValue {}) "hello";
              newFromstring = expect.eq ((WrapString.new "hello").getValue {}) "hello";
              eqString = expect.valueEq (WrapString.new "hello") (String.new "hello");
            };

            MyType2 = {
              overrideDefault = {
                expr =
                  let this = MyType2.mk {
                        stringField = "hi";
                    intField = 123;
                    defaultIntField = 7;
                  };
                  in [(this.stringField.getValue {})
                      (this.intField.getValue {})
                      (this.defaultIntField.getValue {})];
                expected = ["hi" 123 7];
              };

              #missingRequired = expect.error (
              #  MyType2.mk {
              #    intField = 123;
              #    defaultIntField = 7;
              #  }
              #);

              missingDefault = {
                expr =
                  let 
                    this = MyType2.mk {
                      intField = 123;
                      stringField = "hi";
                    };
                  in this.defaultIntField.getValue {};
                expected = 666;
              };

              #wrongType = expect.error (
              #  MyType2.mk {
              #    intField = 123;
              #    stringField = true;
              #    defaultIntField = 7;
              #  }
              #);
            };
          };
        };

      inheritanceTests = U: with U; with TestTypes U;
      let
        A = Type.new "A" {
          fields = This: Fields.new [{
            a = String;
          }];
        };
        B = A.subType "B" {
          ctor = Ctor.new "CtorB" (This: a: b: {
            inherit a b;
          });
          fields = This: Fields.new [{
            b = Int;
          }];
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

      };

      typeFunctionalityTests = U: with U; with TestTypes U; {

        checks = {
          RootType = expect.stringEq Type.name "Type";
        };

        init = {
          byNew = expect.eq ((Type.new "MyType" {}).__TypeId {}) "MyType";
          byFunctor = expect.eq ((Type "MyType" {}).__TypeId {}) "MyType";
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
              in (this.modify.myField (x: String.new "${x.getValue {}}, World!"));
            expected = MyType.new (String.new "Hello, World!");
            compare = this: this.myField.getValue {};
          };
        };

      };

      peripheralTests = U: with U; let SU = U._SU; in {
        checks = {
          isTyped = {
            string = expect.False (U.isTyped "hello");
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

    # <nix>typelib._tests.run</nix>
    in suite rec {

      fastSmoke =
        testInUniverses {
          inherit
            U_0
            #U_1
            #U_2
            ;
        } (U: smokeTests U
          #// typeFunctionalityTests U
          #// instantiationTests U
          #// peripheralTests U
          #// inheritanceTests U
          // untypedTests U
          #// builtinTests U
          );

    } // unless false {

      smoke =
        #solo
          (testInUniverses {
            inherit
              U_0
              U_1
              U_2
              U_3
              ;
          } smokeTests);


      all =
        #solo
          (mergeAttrsList [
            { inherit Bootstrap; }
            (testInUniverses
              { inherit U_0 U_1 U_2 U_3 U_4; }
              (U: {
                peripheral = peripheralTests U;
              }))
            (testInUniverses
              { inherit U_0 U_1 U_2 U_3; }
              (U: {
                smoke = smokeTests U;
              }))
            (testInUniverses
              { inherit U_0 U_1 U_2; }
              (U: {
                Typelib = TypelibTests U;
                typeFunctionality = typeFunctionalityTests U;
                inheritance = inheritanceTests U;
                instantiation = instantiationTests U;
                builtin = builtinTests U;
                cast = castTests U;
                untyped = untypedTests U;
              }))
            (testInUniverses
              { inherit U_1 U_2 U_3; }
              (U: {
                typeChecking = typeCheckingTests U;
              }))
          ]);

      Bootstrap = testInUniverse U_0 (U:
        with U;
        let SU = U._SU; in 
        with __Bootstrap;
        {
          Type__args = {
            ctor.defaults =
              let expected = Type__args.ctor.bind Type__args "A" {};
              in expect.noLambdasEq
                expected
                {
                  __isTypeSet = true;
                  __Super = expected.__Super;  # TODO: Cheat due to named thunk comparison
                  checkValue = null;
                  ctor = U.Ctors.CtorDefault;
                  fields = This: SU.Fields.new [];
                  methods = expected.methods; # TODO: Cheat due to named thunk comparison
                  staticMethods = expected.staticMethods; # TODO: Cheat due to named thunk comparison
                  name = SU.String.new "A";
                  overrideCheck = null;
                  tvars = LazyAttrs {};
                  tvarBindings = LazyAttrs {};
                };
          };
        });

      peripheral =
        #solo
          (testInUniverses {
            inherit
              U_0
              U_1
              U_2
              U_3
              U_4
              TS
              ;
          } peripheralTests);

      Typelib =
        #solo
          (testInUniverses {
            inherit
              U_0
              U_1
              U_2
              #U_3
              ;
          } TypelibTests);

      typeFunctionality =
        #solo
          (testInUniverses {
            inherit
              U_0
              U_1
              U_2
              ;
          } typeFunctionalityTests);

      inheritance =
        #solo
          (testInUniverses {
            inherit
              U_0
              U_1
              U_2
              # U_3
              ;
          } inheritanceTests);

      instantiation =
        #solo
          (testInUniverses {
            inherit
              U_0
              U_1
              U_2
              #U_3
              ;
          } instantiationTests);

      builtin =
        #solo
          (testInUniverses {
            inherit
              U_0
              U_1
              U_2
              #U_3
              ;
          } builtinTests);

      cast =
        #solo
          (testInUniverses {
            inherit
              U_0
              U_1
              U_2
              #U_3
              ;
          } castTests);

      untyped =
        #solo
          (testInUniverses {
            inherit
              U_0
              U_1
              U_2
              #U_3
              ;
          } untypedTests);

      typeChecking =
        #solo
          (testInUniverses {
            inherit
              U_1  # U_1+ due to reliance upon subtype
              U_2
              ;
          } typeCheckingTests);

    };

  # Final module
  typelib = 
    let 
      module = Types.TS // {
        # Embed the whole of Types for isolated typelib.Types access.
        inherit Types;
      };
    in
      module
      // {
        # Exposes:
        # - The typesystem of this module at the top level
        # - .Types (just the Types, with the library-only functions available as .Typelib)
        # - .Typelib (isolated type library, replacing e.g. isNull)
        # - .lib (All the above merged deeply into lib)
        #
        # Not named as 'lib' so that 'with typelib;' does not make a global 'lib' unavailable.
        library = lib.recursiveUpdate lib module;

        # Embed the whole of _tests
        inherit _tests;
      };

  # Placeholder to store multiline string for fast expr execution using nixlike.el
  __nixlikeExprs = ''
# Import shortcut
(load-file "nixlike.el")

# Quick toggles

(setq debug-on-error t)
(setq debug-on-error nil)

(setq nixlike-default-nix-variant 'nix)
(setq nixlike-default-nix-variant 'tvix)

(setq nixlike-default-mode 'shell)
(setq nixlike-default-mode 'repl)

(setq nixlike-nix-eval-strategy 'eval)
(setq nixlike-nix-eval-strategy 'instantiate)

(nixlike-eval-command 'nix "123" 0 t nil)

<nix>
enumerate [1 2 3]
</nix>

<nix>
123
</nix>

<nix>
assert log.trace.over (log.short 123);
assert log.trace.over [(log.short 123)];
assert log.trace.over [(log.short 123)];
123
</nix>

<nix>
Types.Universe.U_2.Int
</nix>

<nix>
with Types.Universe.U_0;
rec {
  a = (Fields.new [{a = Int;}]).update [{b = Default Int 666;}];
  b = (Fields.new [{a = Int;}]);
  c = Fields.new {};
  d = Type;
  e = a.indexed {};
  f = (OrderedOf Int).new { a = 1; b = 2; };
  g = f.indexed {};
  h = a.getSolos {};
  i = a.instanceFields {};
  j = (Int.getFields {});
  k = (Field.new "a" Int);
  l = (lambda a.fieldSpecs) {};
  m = parseFieldSpec Int;
  n = Int;
  o = (String.new "wat").__value.value;
}.e
</nix>

<nix>
let fs = Types.Universe.U_0.__Bootstrap.Type__args.fields {}; in
fs
</nix>

<nix>
_tests.run
</nix>

<nix>
isFunction { __functor = self: arg: arg; }
</nix>

<nix>
types
</nix>

<nix>
collective-lib._testsUntyped.run
</nix>

<nix>
collective-lib._testsUntyped.debug
</nix>

<nix>
Types.Universe.U_0
</nix>

<nix>
typelib._tests.run
</nix>

<nix>
typelib._tests.debug
</nix>

<nix>
log._tests.run
</nix>

<nix>
dispatchlib._tests.run
</nix>

<nix>
attrsets._tests.run
</nix>
<nix>
attrsets._tests.debug
</nix>

<nix>
lists
</nix>
'';

in
  # Use as:
  #
  # - Directly:
  #   typelib.isNull etc
  #
  # - Alongside lib:
  #   with typelib; let lib = pkgs.lib; in (isNull Nil && !(lib.isNull Nil) && lib.isNull null) == true)
  #
  # - Replacing lib:
  #   with typelib.library; (isNull Nil && !(lib.isNull Nil) && lib.isNull null) == true)
  #
  # - Via collective-lib:
  #   collective-lib exposes the type system and all other utils under .typed
  #   with collective-lib.typed; (isNull Nil && isLazyAttrs (LazyAttrs.new {})) == true
  #
  # Isolated separately from collective-lib so that it can be used as a standalone type-system
  # without the rest of the collective-lib library.
  typelib

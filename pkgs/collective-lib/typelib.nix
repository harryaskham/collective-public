{ pkgs ? import <nixpkgs> {},
  collective-lib ? import ./. { inherit lib; },
  lib ? pkgs.lib,
  ... }:

with lib.strings;
with collective-lib.collections;
with collective-lib.clib;
with collective-lib.attrsets;
with collective-lib.functions;
with collective-lib.lists;
with collective-lib.strings;

# TODO:
# - nix-in-nix via eval:
#   - enables program transformation / quasiquoting(can get the argument names of functions)
#   - can layer typechecking on in a zero-cost or one-cost way
# - disallow null fields again
# - do notation using variadic
# - error/either monad
# - methods chain by being functors
# - Have the act of binding a field containing a typevar infer that typevar's value
#   - e.g. ListOf [1 2 3] -> ListOf<Int>
# - Hindley-Milner s.t. A {a = T}, A 123 ->A<Int>
# - Cache + mkmerge / functor setup - instances have unique IDs and can persist things at e.g.
#   cache.<instanceid>.<fieldname> = <value>
#   but means state needs threading through the whole usage of TS
# - could also just remove thunks, allow infinite Type->Type properties
#   - just never log deeply i.e. SeqN always
#   - issue only when recurrent usage to build WHNF value e.g. fields = Fields
#     - but getFeilds accessed always in newinstance so only deferring issue
#     - SU reference fixes this
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
#      String.check (String "abc") == true
#      String.check "abc" == false
#      isString (String "abc") == false
#      isString (String "abc").value == true
#
# Person = Type "Person" {
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
# OK:   alice = Person {name = "Alice"; age = 22;};
# OK:   bob = Person {name = "Robert"; nickname = "Bob"; age = 40;};
# FAIL: ethan = Person {nickname = "Bob"; age = 40;} -> Throws error for missing name field
# FAIL: jake = Person {name = "Jake"; age = "unknown";} -> Throws error for invalid age type
#
# alice.name = "Alice";
# bob.nickname = "Bob";
# alice.nickname = null;
# alice.hello "Binding works as expected" = "Hello, Alice, 22y old! Binding works as expected"
# bob.show = "Robert (Bob), 40y old! Cool."

let
  log = collective-lib.log;
  errors = collective-lib.errors;
  collections = collective-lib.collections;
  dispatchlib = collective-lib.dispatchlib;

  # fn has no dependencies, so goes top-level to avoid U/SU prefix.
  fn = (log.v 1).fn;
  fn_ = (log.v 1).fn_;

  Types =
    with lib;  # lib == untyped default pkgs.lib throughout Types
    rec {

    mkConstants = BuiltinTypes: with BuiltinTypes; {
      True = Bool true;
      False = Bool false;
      Nil = Null null;
    };

    # Nix library overrides to take Types into account.
    # U is provided for forward-references only to e.g. Builtins for 'wrap'; using those
    # functions here will cause a circular dependency.
    mkTypelib = U: SU: rec {

      ### logging wrappers
      mkLoggingWrapper = f: name: f name {inherit (U.opts) level;};
      call = v: mkLoggingWrapper (log.v v).call;
      methodCall = v: This: mkLoggingWrapper ((log.v v).methodCall This);
      callSafe = v: mkLoggingWrapper (log.safe.v v).call;
      methodCallSafe = v: This: mkLoggingWrapper ((log.safe.v v).methodCall This);

      ### warning wrappers
      warnTyped = fname: x:
        if isTyped x 
        then with log.trace; msg "WARN: ${fname} over typed value ${with log.prints; put x _line ___}"
        else x;
      warnTyped1 = fname: f: a: f (warnTyped fname a);
      warnTyped2 = fname: f: a: b: f a (warnTyped fname b);
      mapAttrs = warnTyped2 "mapAttrs" lib.mapAttrs;
      concatMapAttrs = warnTyped2 "concatMapAttrs" lib.concatMapAttrs;
      mapAttrsToList = warnTyped2 "mapAttrsToList" lib.mapAttrsToList;

      ### naming workarounds

      # Convert the given special method name to its intended name.
      # e.g. __implements__toString -> __toString
      implementsPrefix = "__implements";
      removeImplementsPrefix = replaceStrings [implementsPrefix] [""];
      addImplementsPrefixAttrs = 
        lib.concatMapAttrs (name: value: { "${implementsPrefix}${name}" = value; });
      removeImplementsPrefixAttrs = 
        lib.concatMapAttrs (name: value: { "${removeImplementsPrefix name}" = value; });

      # Convert the given special method name to its intended name.
      # e.g. __implements__toString -> __toString
      fieldPrefix = "__field";
      removeFieldPrefix = replaceStrings [fieldPrefix] [""];
      addFieldPrefixAttrs = 
        lib.concatMapAttrs (name: value: { "${fieldPrefix}${name}" = value; });
      removeFieldPrefixAttrs = 
        lib.concatMapAttrs (name: value: { "${removeFieldPrefix name}" = value; });

      ### unwrap

      unwrapperName = builtinName:
        if builtinName == "null" then "nil" else builtinName;

      BuiltinNameToUnwrapperSolo = 
        mapAttrs
          (BuiltinName: builtinName: { 
            ${unwrapperName builtinName} = 
              let unwrapByCast = x: SU.unwrapCastResult (SU.cast builtinName x);
              in dispatch.type.id.def unwrapByCast {
                ${builtinName} = id;
                ${BuiltinName} = x: x.getValue {};
              };
          })
        BuiltinNameTobuiltinName;

      BuiltinNameToUnwrapper = mapAttrs (_: soloValue) BuiltinNameToUnwrapperSolo;

      unwrappers = mergeAttrsList (attrValues BuiltinNameToUnwrapperSolo);

      inherit (unwrappers) nil bool string int float path list set lambda;

      unwrap = x:
        if isUntypedAttrs x then x
        else dispatch.type.name.def id BuiltinNameToUnwrapper x;

      wrap = x: 
        if isTyped x then x
        else dispatch.type.name.def id {
          null = U.Null;
          int = U.Int;
          float = U.Float;
          string = U.String;
          path = U.Path;
          bool = U.Bool;
          list = U.List;
          set = U.Set;
          lambda = U.Lambda;
        } x;

      ### type utilities

      # Get the Universe level of a type.
      getLevel = T: int T.__level;

      # Check if a given argument is a Type in the Type system.
      # 'isType' collides with lib.isType.
      isTypeSet = T: bool (T.__isTypeSet or false);

      # Check if a given argument is a Type or a builtin type.
      isTypeLike = T: isTypeSet T || isbuiltinName T;

      # Check if a given argument is a Type or a builtin type or null.
      isTypeLikeOrNull = T: isNull T || isTypeLike T;

      # Check if a given argument is a TypeThunk
      isTypeThunk = T: (T.__isTypeThunk or (const false)) {};

      # Check if a given argument is a custom Type.
      # Throws if not, otherwise true.
      checkTyped = x:
        with U.call 4 "checkTyped" x ___;
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
          || (isTypeSet A && isTypeSet B
              && A ? __TypeId && B ? __TypeId
                && A.__TypeId {} == B.__TypeId {})
        );

      # Override isType s.t. it operates per the module system as:
      # isType "string" {_type = "string"} == true
      #
      # but also as:
      # isType "string" "my string" == true
      #
      # and as:
      # isType String (String "my string") == true
      #
      # Where:
      # isType "set" (String "my string") == false
      # so that a typed value cannot be treated as a raw set.
      isType = T: x:
        assert assertMsg (isTypeLike T) (indent.block ''
          isType: Invalid T provided (expected builtin type string or Type):

            T = ${indent.here (log.print T)}
              = ${indent.here (log.vprintD 5 T)}
          '');
        # Check x for _type
        lib.isType T x || typeEq T (typeOf x);

      # Override typeOf s.t. on a raw builtin it operates normally, but on a typed value,
      # returns the resolved type.
      # typeOf true == "bool"
      # typeOf (Bool true) == Bool
      typeOf = x:
        # Extra check for callable __Type to avoid accidentally trying to resolve the type of __Type
        # in a solo of {__Type = TypeThunk; }
        if x ? __Type && lib.isFunction x.__Type # Matches functors and lambda shims
        then x.__Type {}
        else lib.typeOf x;

      # Get the type as a string ID. For builtins, operates as typeOf, and for others returns
      # the string form e.g. "Union<[Int Float]>"
      # typeIdOf true == "bool"
      # typeIdOf (Bool true) == "Bool"
      typeIdOf = x:
        let T = typeOf x;
        in if lib.isString T then T
          else if !(T ? __TypeId) then throw (indent.block ''
            typeIdOf: Type instance has no __TypeId field:
              typeOf x = ${lib.typeOf x}
              x.name = ${x.name or "<no .name>"}
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
              typeOf x = ${lib.typeOf x}
              T = ${indent.here (log.vprintD 5 T)}
          '')
          else T.getName {};

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
          id = {
            __functor = self: dispatchlib.dispatch.on typeIdOf;
            def = dispatchlib.dispatch.def.on typeIdOf;
          };

          # Dispatch on type name
          # Matches only the type name, not its bindings
          # i.e. can match any Union, not just Union<[Int Float]>
          name = {
            __functor = self: dispatchlib.dispatch.on typeNameOf;
            def = dispatchlib.dispatch.def.on typeNameOf;
          };
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
    };

    # TODO: Cast types for CastError CastSuccess via Either
    mkCast = SU: U: rec {

      # Cast between types.
      # Returns a set with a 'castError' attribute if the cast is not possible
      # or a set with a 'castSuccess' attribute containined the cast value.
      # Also includes a 'msgs' attribute with a list of messages recording
      # cast attempts.
      mkCastError = msg: { castError = msg; };
      mkCastSuccess = value: msg: { castSuccess = value; castSuccessMsg = msg; };
      mkCastSuccess_ = value: mkCastSuccess value "mkCastSuccess_";
      mkCastSuccessWith = {
        uncastValue = uncastValue: value: msg: (mkCastSuccess value msg) // {
          inherit uncastValue;
        };
      };
      isCastError = x: x ? castError;
      isCastSuccess = x: x ? castSuccess;
      isCastResult = x: isCastError x || isCastSuccess x;
      castErrorOr = xOrError: f:
        if isCastError xOrError
        then xOrError
        else f xOrError;
      combineCastErrors = results: mkCastError (indent.block ''
        Cast errors:
          ${indent.here (indent.lines (map (x: x.castError) results))}
      '');

      # Wrap a cast result to success unless it is already a castResult.
      wrapCastResult = x:
        if isCastResult x then x
        else mkCastSuccess x "wrapCastResult";

      wrapCastResultWith = x: successMsg:
        if isCastResult x then x
        else mkCastSuccess x successMsg;

      # Unwrap a cast result to its value if successful, otherwise handle the error.
      unwrapCastResult = x: unwrapCastResultOr x id;
      unwrapCastResultOr = x: handleError:
        if isCastError x then handleError x
        else if isCastSuccess x then unwrapCastResultOr x.castSuccess handleError
        else x;

      # Return the first successful cast from the types given,
      # or the combined cast errors.
      castFirst = Ts: x: castFirstOr Ts x id;
      castFirstOr = Ts: x: handleError:
        assert assertMsg (nonEmpty Ts) "castFirstOr: no casts given";
        let go = errors: castFns:
              if castFns == [] then combineCastErrors errors
              else let castFn = head castFns;
                       castFns' = tail castFns;
                       result = castFn x;
              in if isCastError result then go (errors ++ [result]) castFns'
              else wrapCastResult result;
            result = go [] (map (T: a: castEither T a id id) (U.list Ts));
        in unwrapCastResultOr result handleError;

      cast = T: x: unwrapCastResult (castEither T x id id);
      castEither = T: x: handleSuccess: handleError:
        with U.call 3 "castEither" T x handleSuccess handleError ___;

        if T == null then
          return (mkCastError "Cannot cast to null: T = ${log.print T}, x = ${log.print x}")

        else if !(U.isTypeLikeOrNull T) then
          return (mkCastError (indent.block ''
            Invalid target type provided for cast:

              T = ${indent.here (log.print T)}

              x = ${indent.here (log.print x)}
          ''))

        else with lets rec {

          printT = T: log.print T;

          TName = printT T;
          xTName = U.typeBoundNameOf x;

          xFields =
            if U.isTyped x then thunkDo x.__Type (T:
              if !(T.fields ? instanceFields) then throw (indent.block ''
                xFields: x.__Type.fields.instanceFields does not exist:
                  x = ${indent.here (log.print x)}
                  x.__Type {} = ${indent.here (log.print T)}
              '')
              else T.fields.instanceFields)
            else
              mkCastError ''
                Cannot get fields from untyped uncast value: T = ${log.print T}, x = ${log.print x}
              '';
          TFields =
            if U.isTypeSet T then
              if !(T.fields ? instanceFields) then throw ''TFields: T.fields.instanceFields does not exist: ${log.print T}''
              else T.fields.instanceFields
            else
              mkCastError ''
                Cannot get fields from non-Type target type: T = ${log.print T}, x = ${log.print x}
              '';

          xIsUnary = castErrorOr xFields (fields: size fields.solos == 1) == true;
          TIsUnary = castErrorOr TFields (fields: size fields.solos == 1) == true;

          xUnaryField = castErrorOr xFields (fields: maybeHead fields.solos);
          TUnaryField = castErrorOr TFields (fields: maybeHead fields.solos);

          xUnaryFieldName = castErrorOr xUnaryField (field:
            if field == null then mkCastError ''Unary field is not a solo: ${log.print field}''
            else soloName field);
          TUnaryFieldName = castErrorOr TUnaryField (field:
            if field == null then mkCastError ''Unary field is not a solo: ${log.print field}''
            else soloName field);

          xUnaryFieldT = castErrorOr xUnaryField (field:
            if field == null then mkCastError ''Unary field is not a solo: ${log.print field}''
            else (soloValue field).FieldType);
          TUnaryFieldT = castErrorOr TUnaryField (field:
            if field == null then mkCastError ''Unary field is not a solo: ${log.print field}''
            else (soloValue field).FieldType);

          xUnaryFieldTName = castErrorOr xUnaryFieldT printT;
          TUnaryFieldTName = castErrorOr TUnaryFieldT printT;

          xFieldNames = castErrorOr xFields (fields: fields.names);
          TFieldNames = castErrorOr TFields (fields: fields.names);

          castStr = "${xTName} -> ${TName}";

          mkOneCast = cast: if cast.when then cast else cast // {
            result = mkCastError "'when' condition not satisfied";
          };

          # Make a sidecast given:
          # - its cast name
          # - the condition for sidecasting
          # - a reason to show if this condition failed
          # - the computed per-field cast solos, which should contain solo names
          #   of the field assignments of the target type.
          mkSidecast = sidecastName: sidecastWhen: sidecastOr: TCastSolos: {
            name = "Coerce: ${sidecastName}";
            when = !(isCastError xFieldNames) && !(isCastError TFieldNames)
                  && sidecastWhen;
            orMsg = indent.block ''
              Sidecast not possible: ${indent.here castStr}
              Reason: ${sidecastOr}

              Source fields:
                ${indent.here (log.print (sort xFieldNames))}

              Target fields:
                ${indent.here (log.print (sort TFieldNames))}
              '';
            result =
              let
                castSolosPartitioned = partitionSolos (_: isCastSuccess) TCastSolos;
                castArgs =
                  mergeSolos
                    (mapSolos
                      (_: castResult: castResult.castSuccess)
                      castSolosPartitioned.right);
                castErrorMsgs =
                  soloValues
                    (mapSolos
                      (name: castResult: "${name}: ${castResult.castError}")
                      castSolosPartitioned.wrong);
                castSuccessMsgs =
                  soloValues
                    (mapSolos
                      (name: castResult: "${name}: ${castResult.castSuccessMsg}")
                      castSolosPartitioned.right);
              in if size castErrorMsgs > 0
                then mkCastError (indent.blocks castErrorMsgs)
                else mkCastSuccess (T.mk castArgs) (indent.blocks castSuccessMsgs);
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
          };

          # A list of casts to attempt in order.
          # The first cast satisfying 'when == true && isCastSuccess result' will be returned.
          # If no cast satisfies, then a cast error set is returned with the collated errors.
          casts = map mkOneCast [
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
              result = mkCastSuccess x (indent.block ''
                Identity cast succeeded:
                  ${indent.here castStr}
              '');
            }

            # Cast via T's typeclass instance if it exists.
            # After Identity, so that cast impls don't need to always
            # perform the identity check.
            {
              name = "Cast.cast";
              when = (SU.Cast.checkImplements T);
              orMsg = indent.block ''
                Type does not implement Cast:
                  ${indent.here castStr}
              '';
              result = (SU.Cast T).cast x;
              failMsg = castErrorMsg: indent.block ''
                Cast.cast failed for ${castStr}:
                  ${indent.here castErrorMsg}
              '';
              successMsg = castSuccessMsg: indent.block ''
                Cast.cast succeeded for ${castStr}:
                  ${indent.here castSuccessMsg}
              '';
            }

            # Downcasting for unary x types via a nested cast
            # e.g. String -> string, Constraint -> Type
            # TODO: Maybe a -> a
            {
              name = "Coerce: Downcast from Unary";
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

                ${indent.here "Downcast success: ${castSuccessMsg}"}
              '';
            }

            # Upcasting for unary x types via a nested cast
            # e.g. string -> String, Type -> Constraint
            # TODO: a -> Maybe a
            {
              name = "Coerce: Upcast to Unary";
              when = TIsUnary;
              orMsg = "Cannot upcast to a non-unary type: ${xTName} -> ${TName}";
              result =
                if !(T ? mk) then mkCastError (indent.block ''
                  ${TName} does not have a 'mk' method
                '')
                else
                  let xCast = cast TUnaryFieldT x;
                  in castErrorOr xCast (x':
                    mkCastSuccess (T.mk { ${TUnaryFieldName} = x'; }) (xCast.castSuccessMsg or ""));
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

            # Sidecasting types via a nested cast over ordered fields.
            # e.g. String -> Int via "string" -> "int"
            #      let T = Type "T" { fields = [{a = String;} {b = "int"}]; };
            #          U = Type "U" { fields = [{c = "string";} {b = Int}]; };
            #      ...
            #      cast T (U "c" (Int 123)) == T (String "c") 123
            #      cast U (T (String "a" 123)) == U "c" (Int 123)
            (mkSidecast "Sidecast by field name"
              # Fields must have equivalent names, but order doesn't matter
              ((sort xFieldNames) == (sort TFieldNames))
              "Source and target types do not have equivalent field names"
              # Cast x's fields one by one in the order of TFieldNames
              (map
                (TFieldName:
                  mkSolo
                    TFieldName
                    (cast
                      ((T.fields.allFields.lookup TFieldName).FieldType)
                      x.${TFieldName}))
                TFieldNames))

            (mkSidecast "Sidecast by field order"
              # Fields must have equivalent orders, but names don't matter.
              (length xFieldNames == length TFieldNames)
              "Source and target types do not have the same number of fields"
              # Cast each field in T from the field in x at the equivalent position.
              (zipListsWith
                (xFieldName: TFieldName:
                  mkSolo
                    TFieldName
                    (cast
                      ((T.fields.allFields.lookup TFieldName).FieldType)
                      x.${xFieldName}))
                xFieldNames
                TFieldNames))
          ];

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
                ${indent.here ((castResult.failMsg or id) castResult.result.castError)}
            '';

          getSuccessMsg = castResult:
            assert isCastSuccess castResult.result;
            indent.block ''
              ${castResult.name}:
                ${indent.here ((castResult.successMsg or id) castResult.result.castSuccessMsg)}
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
                      ${indent.here (log.vprintD 1 x)}

                    ${TName} type:
                      ${indent.here (log.vprintD 1 T)}

                    Attempted casts:
                      ${indent.here (indent.blocks msgs)}

                    Log State:
                      ${indent.here (log.print (__logState {}))}
                  ''))

              # Skip non-matching casts with a note message
              else if !castResult.when
                then let msgs' = msgs ++ [(getOrMsg castResult)]; in tryCasts msgs' casts'

              # Record nested cast errors
              else if isCastError castResult.result
                then let msgs' = msgs ++ [(getFailMsg castResult)]; in tryCasts msgs' casts'

              # Cast succeeded
              else 
                let wrappedResult = castResult // { result = wrapCastResult castResult.result; };
                    msgs' = msgs ++ [(getSuccessMsg wrappedResult)];
                in
                  mkCastSuccessWith.uncastValue x wrappedResult.result.castSuccess (indent.block ''
                    Cast succeeded: ${xTName} -> ${TName}

                    ${xTName} instance:
                      ${indent.here (log.print x)}

                    Attempted casts:
                      ${indent.here (joinLines msgs')}
                  '');

            result = tryCasts [] casts;
        };
        if isCastError result then handleError result
        else if isCastSuccess result then handleSuccess result
        else handleSuccess (wrapCastResultWith result "cast: success");
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
    typeFieldSpec = opts: SU:
      # The type of the instance as a thunk.
      # Set in mkInstance args_->args to the This TypeThunk.
      mkSolo "__Type" (maybeNulled opts SU SU.TypeThunk);

    withTypeFieldSpec = opts: SU: fieldSpecs:
      consSolo (typeFieldSpec opts SU) (solos fieldSpecs);

    # Construct the fields for a universe using the types of the universe above.
    # The 'Type' field is added in Fields.new in both shim and real implementations.
    mkTypeFieldSolosFor = SU: U: [
      # Indicates that this is a type. Should never be set manually.
      {__isTypeSet = maybeNulled U.opts SU (SU.Default SU.Bool SU.True);}
      # The Universe level on which this type is created.
      {__level = maybeNulled U.opts SU (SU.Default SU.Int (SU.Int U.level));}
      # The supertype of the type.
      {__Super = maybeNulled U.opts SU (SU.Default (SU.NullOr SU.TypeThunk) SU.Nil);}
      # The name of the type.
      {name = maybeNulled U.opts SU (SU.String);}
      # The type parameters of the type.
      {tvars = maybeNulled U.opts SU (SU.Default (SU.SolosOf SU.Type) (SU.SolosOf SU.Type []));}
      # The type parameter bindings of the type.
      {tvarBindings = maybeNulled U.opts SU (SU.Default SU.Set (SU.Set {}));}
      # The constructor function creating the fields of the type as a set to pass to mk.
      {ctor = maybeNulled U.opts SU (SU.Default SU.Ctor U.Ctors.CtorDefault);}
      # A set of ordered fields to make available as this.___ and this.has.___, this.set.___, etc
      {fields = maybeNulled U.opts SU (SU.Default SU.Fields (SU.Fields []));}
      # A set of methods from this to make available as this.___
      {methods = maybeNulled U.opts SU (SU.Default (SU.SetOf SU.Lambda) (SU.SetOf SU.Lambda {}));}
      # A set of methods from This to make available as This.___ and this.___
      {staticMethods = maybeNulled U.opts SU (SU.Default (SU.SetOf SU.Lambda) (SU.SetOf SU.Lambda {}));}
      # If set, use this lambda of (bindings: This') to rebuild the type after binding a type variable.
      {rebuild = maybeNulled U.opts SU (SU.Default (SU.NullOr SU.Lambda) SU.Nil);}
    ];

    # Methods that should be merged in to instances of Type.
    # These are to be made available on instances-of-instances-of-Type, and so
    # could only be inject via merge, or via having types inherit from Type rather
    # than be of type Type.
    typeMethodsFor = SU: U: {
      # Otherwise the raw set with ctor, without __show etc
      __implements__toString = this: self: indent.block ''
        ${errors.try (U.typeIdOf this) (_: "<typeIdOf failed>")} (
          ${indent.here (log.print (U.withoutUnsafeStringConversionAttrs this))} )
      '';

      # Expose subset of static methods on instances by default so that
      # values can do this.__new to rebuild in inheritable ways without referring
      # to __Type
      __new = this: arg: (this.__Type {}).new arg;
      __super = this: arg: (this.__Type {}).super arg;
      __superCtor = this: arg: (this.__Type {}).superCtor arg;
    };

    # We need to be very careful here to only access U from sites that are
    # called by a fully bound and constructed U.Type (i.e. during U.Type.newTemplate)
    # so that we do not attempt to access types that will themselves attempt to
    # force U.Type so they can be constructed.
    # See mkTemplating - essentially anywhere we produce a new Type here, we need
    # to do so from U.* - i.e. we need to use U.newSubType instead of SU.newSubType
    # so that ((U.Type "Parent" {}).subType "Child" {}).Type == U.Type and not SU.Type,
    # which would mean every subtype created in a chain would ascend a universe until
    # it reached the Quasiverse.
    typeStaticMethodsFor = SU: U: {
      # Get the resolved name of the type.
      getName = This: _:
        errors.try (U.string This.name) (_: throw "getName: ${log.vprint This}");

      # Is this instance an instance of type That.
      isInstance = This: that: U.isType This that;

      # A = Type "A" {};
      # B = Type "B" { __Super = A; };
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
      getBoundName = This: _:
        let
          tvars =
            assert assertMsg (U.isAttrs (This.tvars or null)) (indent.block ''
              getBoundName: This.tvars is not attrs:
                This = ${indent.here (This.getName {})}
            '');
            This.tvars;

          tvarBindings =
            assert assertMsg (U.isAttrs (This.tvarBindings or null)) (indent.block ''
              getBoundName: This.tvarBindings is not a LazyAttrs:
                This = ${indent.here (This.getName {})}
            '');
            U.set This.tvarBindings;
        in
          if empty tvars.solos then This.getName {}
          else
            let
              printBinding = tvarName:
                let
                  T = def SU.Void tvarBindings.${tvarName};
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
              printBindings = joinSep ", " (map printBinding tvars.names);
            in "${This.getName {}}<${printBindings}>";

    # Check that the given binding solo is permissible.
    # If so, returns the bindings with attrValues cast to the type.
    # e.g. Union [A B] -> Union { Ts = ListOf Type [A B];}
    # Does not bind; instead This.bind { T = Int; U = ...} enables multiple bindings without
    # needing to foldl' and construct many intermediate types.
    # e.g. ListOf.bind { T = Int; } == ListOf<Int>
    # C = Constraint (e.g. ListOf { T = Type; } -> C == Type)
    # T = binding type (e.g. ListOf.bind { T = Int; } -> T == Int)
    castBinding = This: tvarName: T:
      let 
        tvars = mergeSolos This.tvars.solos;
        tvarBindings = set This.tvarBindings;
        throwBindError = msg: throw (indent.block ''
          Bind error:
            ${indent.here "${This.getName {}}.${tvarName} <- ${log.print T}"}
            Constraints: ${indent.here "${log.print C} (${if TSatC then "" else "not "}satisfied)"}
            Existing binding: ${log.print B}
            ${indent.here msg}
          '');
      in
      assert errors.checks [
        {
          name = "bindSolo: tvar ${tvarName} exists on ${log.print This}";
          cond = tvars ? ${tvarName};
          msg = "Type ${This.getName {}} does not have type variable ${tvarName}";
        }
        {
          name = "bindSolo: tvarBinding ${tvarName} exists on ${log.print This}";
          cond = tvarBindings ? ${tvarName};
          msg = "Type ${This.getName {}} does not have a tvarBinding ${tvarName} (either Void or defined)";
        }
        {
          name = "bindSolo: tvar ${tvarName} not already bound";
          cond = U.Void.eq tvarBindings.${tvarName};
          msg = "Type ${This.getName {}} already has type variable ${tvarName} bound to ${tvarBindings.${tvarName}}";
        }
      ];
      let
        C = Constraint tvars.${tvarName};
        castT = C.castConstraint T;
      in
        assert errors.checks [
          {
            name = "bindSolo: ${log.print T} satisfies constraint: ${tvarName} = ${log.print C}";
            cond = U.isCastSuccess castT;
            msg = indent.block ''
              bindSolo: Binding ${log.print T} does not satisfy constraint: ${tvarName} = ${log.print C}
              Cast error:
                ${indent.here (log.print castT.castError)}
            '';
          }
        ];
        # Return the cast result ready to be merged into the bindings.
        castT.castSuccess;

      # Construct the type resulting from applying the given bindings to the parameterized This type.
      # Bind is only exposed after U.Type is constructed on instances of U.Type.
      # TODO: TypeVar object plus bootstraps
      # e.g. ListOf.bind { T = Int; } == ListOf<Int>
      bind = This: bindings:
        # Set the new bindings
        let 
          castBindings = mapAttrs This.castBinding bindings;
          This' = This.modify.tvarBindings (bs: bs // castBindings);
        in
          # Rebuild the type to reflect that it may inherit from the new bindings, or have attributes that
          # depend upon them.
          if (U.isNull This'.rebuild)
            then This'
            else This'.rebuild (This'.tvarBindings {});

      # Is That the same type as This?
      # TODO: Typeclass this
      eq = This: That: U.typeEq This That;

      # Does this type inherit from That?
      inheritsFrom = This: That:
        U.isTypeSet That
        && That.eq This
        || (!(U.isNull This.__Super)
            && thunkDo This.__Super (Super: Super.inheritsFrom That));

      # Does this type implement the given class, explicitly or implicitly?
      implements = This: class: class.checkImplements This;

      check = This: that:
        errors.tryBool (errors.checks [
          {
            name = "isType This that";
            cond = U.isType This that;
            msg = indent.block ''
              Type check failed against ${log.vprintD 1 This} for value: ${lib.typeOf that}
            '';
                #${indent.here (errors.try (log.vprint that) (_: "<error>"))}
          }
        ]);

      # Use the bound name with its ordered param assignments to determine type identity
      # and equality.
      # For bootstrap types this may not be bound yet, falling back to the name.
      __TypeId = This: _: (This.getBoundName or This.getName) {};

      # Create a new instance of the type by providing at least all required field values.
      mk = This: U.mkInstance This;

      # Create a new instance of the type by calling This's constructor
      # When This == Type, creates a new Type.
      # Consumes its first argument to avoid nullaries
      new = This: U.newInstance This;

      # Create a new instance using a custom Ctor.
      # Can be used to break recursion in cast for types that use cast in their ctor.
      # Does not actually set This.ctor, so that this.Type remains unmodified.
      newWithCtor = This: ctor: U.newInstanceWithCtor ctor This;

      # Call new on Super.
      # Can be used in a constructor to get an instance of Super to use to construct This.
      superNew = This: arg: (This.__Super {}).new arg;

      # Call ctor on Super.
      # Can be used in a constructor to get args from Super's ctor to construct This.
      superCtor = This: arg: (This.__Super {}).ctor This arg;

      # Create a new subType inheriting from This
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

      # True iff we have a tvarbinding for every tvar.
      isFullyBound = This: _: empty (This.getUnboundTvars {}).solos;

      # A list of the unbound tvar solos
      getUnboundTvars = This: _:
        assert assertMsg (This ? tvars) (with indent; block ''
          getUnboundTvars: This.tvars missing
          This = ${here (log.print This)}";
          typeOf This = ${log.print (typeOf This)}
        '');
        This.tvars.filter
          (tvarName: _: U.Void.eq (set This.tvarBindings).${tvarName});

      ### __implements: Converted to e.g. __toString upon binding

      # Print Type objects as just their name.
      __implements__toString = This: self: This.__TypeId {};

      # Enable creation of Types simply by calling the constructor.
      # When this method is bound, turns the Type instance itself into a callable functor.
      # Consumes one argument to avoid internal __functor machinery going infinite upon binding.
      # For templates, however, this acts as a template binding constructor:
      #
      # List [1 2 3] -> List([1 2 3])
      # ListOf {T = Int} (map Int [1 2 3]) -> ListOf<Int>([1 2 3])
      # ListOf Int (map Int [1 2 3]) -> ListOf<Int>([1 2 3])
      #
      # If the bindings are supplied as a set, then the tvar name is taken from the key.
      # If provided as type arguments, the names are inferred from the tvar bindings order,
      # just as with the default constructor and the field list order.
      __implements__functor = This: self: arg:
        if This.isFullyBound {}
          then This.new arg

        else if U.isUntypedAttrs arg
          then This.bind arg
        
        else
          let tvarSolos = This.getUnboundTvars {};
              tvarName = head tvarSolos.names;
          in This.bind { ${tvarName} = arg; };
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
      __Type = SU.TypeThunk U.Type;
      __isTypeSet = true;
      __level = U.opts.level;
      name = U.opts.typeName;
      __Super = null;
      ctor = U.Ctors.CtorType;
      fields = mkTypeFieldSolosFor SU U;
      # We have these are both methods and staticMethods on Type s.t. we have
      # them present in bootstrap and in Type.new instances, which get these as
      # methods via Ctors.CtorType.
      methods = typeStaticMethodsFor SU U;
      staticMethods = typeStaticMethodsFor SU U;
      tvars = [];
      tvarBindings = {};
      rebuild = null;
    };

    mkInstantiation = SU: U: rec {
      # e.g. parseFieldSpec Static<Default<Int, 123>> -> {isStatic = true, defaultValue = 123; FieldType = Int; }
      #      parseFieldSpec Static<Int> -> {isStatic = true; FieldType = Int; }
      #      parseFieldSpec Default<Int, 123> -> {defaultValue = 123; FieldType = Int; }
      #      parseFieldSpec Int -> { FieldType = Int; }
      parseFieldSpec = spec:
        with U.callSafe 3 "parseFieldSpec" spec ___;

        # Unwrap Static types.
        # Duck-typed to support bootstrap.
        if spec ? staticType
        then (parseFieldSpec (spec.staticType {})) // {
          isStatic = true;
        }

        # Unwrap Default types.
        # Duck-typed to support bootstrap.
        else if (spec ? defaultType) && (spec ? defaultValue)
        then 
          (parseFieldSpec (spec.defaultType {})) // {
          defaultValue = spec.defaultValue {};
          hasDefaultValue = true;
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
                  ${indent.here (log.print (errors.try (spec {}) (_: "Resolution failed")))}
              '')
            ]);
          {
            FieldType = spec;
            isStatic = false;
            defaultValue = null;
            hasDefaultValue = false;
          };

      # Gets the final value that should be set on the given field after casting and typechecks.
      # Returns only the cast value result or error; not the full { name = value }
      #
      # TODO: Outdated comment
      #
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
      #     SortedInts [1 3 2] -> Type error
      #     xs = SortedInts [1 2 3] -> ok
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
      castFieldValue = This: uncastValue: fieldName: field:
        with U.call 3 "castFieldValue" This uncastValue fieldName field ___;
        #assert check
        #  "Field valid before casting"
        #  (U.typeNameOf field == "Field")
        #  (with indent; block ''
        #    Invalid field encountered in setFields:

        #      This = ${here (print This)}

        #      field = ${here (print field)}
        #  '');
        with lets { 
          allowUntyped = U.opts.allowUntypedFields field; 
          fieldHasType = field ? FieldType && !(U.isNull field.FieldType);
        };
        return (
          if fieldHasType then U.cast field.FieldType uncastValue
          else 
            if allowUntyped
              then U.mkCastSuccess uncastValue "Untyped field ${fieldName}: no cast"
              else throw (indent.block ''
                Got disallowed null field:
                  ${This.name}.${fieldName} = ${log.print field}
              ''));

      castFieldArgValue = This: args: fieldName: field:
        let
          uncastValue =
            args.${fieldName}
              or field.defaultValue
              or (throw (indent.block ''
                    Field ${fieldName} is not set in args and has no default value:
                      This = ${here (print This)}
                      args = ${here (print args)}
                  ''));
        in castFieldValue This uncastValue fieldName field;

      # Make all field assignments on the instance
      mkFieldAssignmentSolos =
        fn_ "mkFieldAssignmentSolos"
        {This = U.Type;} {prefixedArgs = "set";}
        (_: with _;
          with lets rec {
            # We can remove the __field prefix here since we destructure to args of "__Type" and Type.
            fieldCastSolos = 
              This.fields.instanceFieldsWithType.fmap 
                (castFieldArgValue This (U.removeFieldPrefixAttrs prefixedArgs));
            partitioned = fieldCastSolos.partition (_: castResult: U.isCastError castResult);
            castErrorMsgs =
              (partitioned.right.fmap (name: e: "${This}.${name}: ${e.castError}")
              ).values;
            fieldAssignmentsSolos =
              partitioned.wrong.fmap (_: result: U.unwrapCastResult result);
          };
          assert checks [{
            name = "Field assignment casts succeeded";
            cond = empty castErrorMsgs;
            msg = with indent; block ''
              Errors occurred casting field assignment values for ${This} fields:
                ${here (blocks castErrorMsgs)}
            '';
          }];
          return fieldAssignmentsSolos
        );

      # Accessors to include on all instances.
      # Where these need binding, they are bound similarly to methods s.t. methods can call
      # accessors.
      mkAccessors = This:
        let
          instanceFields = This.fields.instanceFields;
        in rec {
          # Field checking interface.
          # e.g. this.has.someInt -> true
          #      this.has ? someInt -> true
          #      this.has.notAField -> throws error
          #      this.has ? notAField -> false
          #      this.has.notAField or default -> default
          has = (instanceFields.fmap (_: _: _: true)).merge {};

          # Field getting interface
          # e.g. this.get.someInt {} -> 123
          get = (instanceFields.fmap (name: _: this: _: this.${name})).merge {};

          # Field setting interface
          # e.g. this.set.someInt 123 -> this'
          #      this.set.someInt "123" -> throws Type error
          set =
            (instanceFields.fmap
              (fieldName: field:
                (this: uncastValue:
                  # Reinit here inside the thunk to avoid constant reinit while setting by name
                  # during construction.
                  let castResult = castFieldValue This uncastValue fieldName field; in
                  assert errors.checks [{
                    name = "set.${fieldName} cast succeeded";
                    cond = U.isCastSuccess castResult;
                    msg = (with indent; block ''
                    Cast failed calling set.${fieldName} on ${This}:
                      ${here castResult.castError}
                    '');
                  }];
                  let assignment = mkSolo fieldName castResult.castSuccess; in
                  bindThis This (this // assignment))))
            .merge {};

          # Field modification interface
          # e.g. this.modify.someInt (x: x+1) -> this'
          #      this.modify.someInt toString -> throws Type error
          modify =
            (instanceFields.fmap
              (fieldName: _:
                this: f: this.set.${fieldName} (f this.${fieldName})))
            .merge {};
        };

      checkNoNullaryBindings = check: strThis: vstrThis: bindings:
        let
          nullaryBindings = filterAttrs (_: binding: !(U.isFunctionNotFunctor binding)) bindings;
        in
        check
          "No nullary bindings"
          (empty nullaryBindings)
          (indent.block (''
            Nullary bindings encountered:

              Nullary bindings = ${indent.here (log.vprintD 3 nullaryBindings)}

              This = ${indent.here strThis}
                   = ${indent.here vstrThis}
            ''));

      mkStaticMethodBindings = strThis: vstrThis: staticMethods: this_:
        with U.call 3 "mkStaticMethodBindings" strThis vstrThis staticMethods "unsafe:this_" ___;
        let
          bindings =
            mapAttrs
              (methodName: staticMethod: staticMethod this_)
              (U.set staticMethods);
        in
          assert checkNoNullaryBindings check strThis vstrThis bindings;
          return bindings;

      mkMethodBindings = strThis: vstrThis: methods: this_:
        with U.call 3 "mkMethodBindings" strThis vstrThis methods "unsafe:this_" ___;
        let
          bindings =
            mapAttrs
              (methodName: method: method this_)
              methods;
        in
          assert checkNoNullaryBindings check strThis vStrThis bindings;
          with safety true;
          return bindings;

      mkAccessorBindings = This: this_:
        with U.call 3 "mkAccessorBindings" This "unsafe:this_" ___;
        let
          bindings =
            mapAttrs
              (_: mapAttrs (_: accessor: accessor this_))
              (mkAccessors This);
        in
          # No nullary check on accessors.
          return bindings;

      # Initialise a type from its This type, its partial this set, and any args.
      mkthis = This: prefixedArgs:
        with U.call 2 "mkthis" This prefixedArgs ___;
        with lets rec {
          fieldAssignmentSolos = mkFieldAssignmentSolos This prefixedArgs;
          this__fieldsOnly = fieldAssignmentSolos.merge {};
          this__bound = bindThis This this__fieldsOnly;
        };
        return this__bound;

      # Bind members that refer to this on construction and after any change.
      bindThis = This: this:
        let
          # Convert This to a label to pass into binding functions for use in logs.
          # Avoids a dependency on This s.t. we can bind from outside of mkInstance.
          strThis = toString This;
          vstrThis = log.vprintD 5 This;

          # If we are creating a Type, we need to bind its static methods too
          # When creating Type via Type.new: binds 'new' to the new Type
          # When creating Bool via Type.new: binds 'new' to Bool
          # When creating bool via Bool.new: this.__Type.staticMethods == Bool.staticMethods == {}
          staticMethodInstanceBindings =
            assert assertMsg (this ? __Type) (indent.block ''
              bindThis: no __Type on this
                This = ${log.vprint This}
                this = ${log.vprint this}
            '');
            assert assertMsg (lib.isFunction this.__Type) (log.print this);
            mkStaticMethodBindings strThis vstrThis (U.set ((this.__Type {}).staticMethods or {})) this_;

          # When creating Type via Type.new: binds 'new' to the new Type
          # When creating Bool via Type.new: this.staticMethods == Bool.staticMethods == {}
          # When creating bool via Bool.new: this ? staticMethods == false
          staticMethodBindings =
            mkStaticMethodBindings strThis vstrThis (U.set (this.staticMethods or {})) this_;

          methodBindings = mkMethodBindings strThis vstrThis (U.set (This.methods or {})) this_;

          accessorBindings = mkAccessorBindings This this_;

          this_ =
            # We can only elide the __implements and __elide prefixes after this merge, otherwise
            # mergeAttrsList fails when it encounters a perceived functor (i.e. staticMethodBindings
            # or methodBindings containing a bound __functor method).
            U.removeImplementsPrefixAttrs
              (mergeAttrsList [
                this
                staticMethodInstanceBindings
                staticMethodBindings
                accessorBindings
                methodBindings
              ]);
        in
          this_;

      # Create a new instance of a type by calling the given constructor.
      # The constructor's output arguments are then passed into mkInstance.
      # Or, if the constructor returns a fully-made type, return that instead.
      # For types, the constructor just merges a name parameter with an arguments
      # parameter and delegates to mkInstance.
      newInstance = This:
        #with U.callSafe 2 "newInstance" This ___;
        #assert assertMsg (This ? ctor) (indent.block ''
        #  newInstance: This does not have a ctor:
        #    This = ${indent.here (log.print This)}
        #          = ${indent.here (log.vprint This)}
        #'');
        #return (
        newInstanceWithCtor This.ctor This;

      newInstanceWithCtor = ctor: This:
        #with U.callSafe 2 "newInstanceWithCtor" ctor This ___;
        let
          # All instances have a common __Type field.
          # If this is specified explicitly by the ctor as args.__Type, use this value instead
          # This should be overridden / specified with caution - this is not typechecked to ensure
          # it is a valid TypeThunk, or that it refers to This.
          maybeSetType = args: args // {
            __Type = args.__Type or (SU.TypeThunk This);
          };
        in
          Variadic.composeFunctorsAreAttrs
            (argsOrInstance:
              if U.isTyped argsOrInstance
                then let this = argsOrInstance;
                     in
                       assert assertMsg (U.isType This this) (indent.block ''
                         newInstance: ${log.print This}.ctor returned non-instance of ${log.print This}:
                           ${log.print this}
                       '');
                       this
                else let args = argsOrInstance;
                     in mkInstance This (maybeSetType args))
            (arg: ctor This arg);

      # Build a new instance from a single dict of field values.
      # Construct an instance from a This type and attrset of field-to-value assignments.
      # arg = { fieldName = value, ... } for all fields.
      # Fields with defaults can be omitted.
      # Build a new instance from a single dict of field values.
      mkInstance = This: args_:
        with U.callSafe 2 "mkInstance" This args_ ___;
        with lets rec {
          # Construct the args to use in constructing field values for 'this'.
          args = args_ // {
            # Ensure that args.__Type is set to the This type.
            # This is used to ensure that the type of the instance is correct.
            # If set by ctor and constructed by new, the .ctor __Type takes precedence.
            # Otherwise if unset by ctor and constructed by new, the .new __Type takes precedence
            # Otherwise if constructed by mkInstance and unspecified in args_ (i.e. using .mk directly) we set here.
            __Type = args_.__Type or (SU.TypeThunk This);
          };

          # Construct 'this' as an instance of 'This'.
          # To pass around the args inside mkInstance, we add the __field prefix to the args.
          # This can be restored once 'this' is created.
          this = mkthis This (U.addFieldPrefixAttrs args);
        };
        assert checks [{ name = "This is validity-checkable";
            cond = This ? fields
                   && This.fields ? instanceFieldsWithType
                   && This.fields ? requiredFields
                   && This ? staticMethods;
            msg = indent.block ''
              Missing fields / accessors on This:
                This ? fields
                && This.fields ? instanceFieldsWithType
                && This.fields ? requiredFields
                && This ? staticMethods;

                This = ${indent.here (log.vprint This)}

                this = ${indent.here (log.print This)}
            '';
          }];
        with lets rec {
          # Check the validity of the constructed instance.
          validity = rec {
            allFieldNames = This.fields.instanceFieldsWithType.names;
            requiredFieldNames = This.fields.requiredFields.names;
            thisFieldNames = intersectLists (attrNames this) allFieldNames;
            unknownFieldNames = subtractLists allFieldNames thisFieldNames;
            missingFieldNames = subtractLists thisFieldNames requiredFieldNames;
            staticMethodNames = map U.removeImplementsPrefix (attrNames (U.set This.staticMethods));
            thisStaticMethodNames = intersectLists (attrNames this) staticMethodNames;
            missingStaticMethodNames = subtractLists thisStaticMethodNames staticMethodNames;
            checks = [
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
          };
        };
        assert checks validity.checks;
        return this;
      };

    mkCtors = SU: U: rec {
      Ctors = rec {
        None = This: _: throw ''Ctors.None evoked'';

        # Explicit nullary constructor s.t. X.new == X.mk {}
        # Still needs a thunk arg otherwise it will evaluate
        CtorNullary = This: _: {};

        # Default constructor for regular non-NewType/Builtin/Alias types.
        # Accepts required field values in order of Fields definition
        CtorDefault = This:
          let requiredFieldNames = This.fields.requiredFields.names;
          in if empty requiredFieldNames then _: {}
             else Variadic.mkOrdered requiredFieldNames;

        # Construct the object by casting its argument.
        # Relies on the Ctor behaviour of returning a full object.
        CtorCast = This: x: U.castErrorOr (cast This x) id;

        # The constructor for Type in U and its precursors / descendent Type types
        # in subuniverses of U.
        # TODO: Defaults should not need restating in U_2+
        # SU.Ctor since stored top-level on Type.
        CtorType = This: name: args: {
          __isTypeSet = true;
          __level = U.opts.level;
          __Super = args.__Super or null;
          inherit name;
          ctor = args.ctor or CtorDefault;
          fields = SU.Fields (args.fields or []);
          # Merge any provided methods with the given defaults. Any specified in args.methods
          # will be overridden.
          methods = (typeMethodsFor SU U) // (args.methods or {});
          staticMethods = args.staticMethods or {};
          tvars = SU.SolosOf SU.Type (args.tvars or []);
          tvarBindings = args.tvarBindings or {};
          rebuild = args.rebuild or null;
        };
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
      #     (This.Super.do (T: T.ctor This)) args
      inheritFrom = Super: name: ctorArgs:
        with U.call 3 "inheritFrom" Super name ctorArgs ___;
        assert check "U.isTypeSet Super"
          (U.isTypeSet Super)
          "inheritFrom: Super must be a Type, got ${log.print Super}";

        return (ctorArgs // {
          # Store Super as a Thunk.
          __Super = SU.TypeThunk Super;

          # Override or inherit the ctor
          ctor = ctorArgs.ctor or Super.ctor;

          # Merge fields inside Super into a new This: ... form
          # The Fields.concat method handles the solo-list duplicate merge.
          fields = 
            if ctorArgs ? fields
            then (Super.fields.allFields.concat ctorArgs.fields).solos
            else Super.fields;

          # Merge methods with Super's, overriding any named the same.
          methods = U.set Super.methods // (U.set (ctorArgs.methods or {}));

          # Merge methods with Super's, overriding any named the same.
          staticMethods = U.set Super.staticMethods // (U.set (ctorArgs.staticMethods or {}));
        });

      # For a given type, create a new type in the same universe.
      # Inheritance is performed as per inheritFrom.
      # Only ever used after U.Type is built, so we can safely access U.Type.new.
      newSubType = This: name: args:
        with U.call 2 "newSubType" This name args ___;
        assert check "U.isTypeSet This"
          (U.isTypeSet This)
          (indent.block ''
            Cannot subtype non-Type or Type-precursor:
              ${log.print This}
          '');
        with safety true;
        return (U.Type name (inheritFrom This name args));

      # Create a new template of a type accepting type parameters.
      # The parameters can be accessed via the _ argument surrounding the type specification.
      # We can't instantiate the type until we get bindings, since we inherit from
      # the bindings in the general case.
      # For now we end up inheriting from Void e.g. ListOf<T> -> ListOf<Void> until bound,
      # and we rebuild to e.g. ListOf<Int>.
      #
      # e.g.
      # MyInt = Int.subType "MyInt" {};
      # MyTemplate = Type.template "MyTemplate" { T = Type, U = Int; } (_: {
      #   fields = [{ t = _.T;} {u = _.U;}];
      # };
      # then
      # typeOf MyTemplate == lambda
      # (MyTemplate { T = String; U = Int }) "abc" 123 == MyTemplate<String, Int> {t="abc"; u=123;}
      # (MyTemplate { T = String; U = MyInt }) "abc" (MyInt 123) == MyTemplate<String, MyInt> {t="abc"; u=MyInt 123;}
      # (MyTemplate { T = String; U = Bool }) -> throws binding error, Bool not valid for U's Int constraint
      # MyStringInt = MyTemplate { T = String }) -> MyStringInt == MyTemplate<String, U = Int>, the partially bound template
      # MyStringInt.bind {U = MyInt} -> MyTemplate<String, MyInt>
      #
      # If bindingsToSuper is not null:
      # In this Universe, create a new template whose eventual bound type inherits from a function of its type variables.
      # For example:
      # ListOfSetOf = newSubTemplateOf (_: ListOf (SetOf T)) "ListOfSetOf" { T = Type; } (_: { ... });
      # (ListOfSetOf {T = Int}) [{x = 123;}]; typechecks
      __newTemplate = This: bindingsToSuperOrSuper: name: tvars: bindingsToArgs_:
        let
          # Convert the given tvars into a SetOf Constraints
          # This can use U.Constraint, because we have U.Type
          # U.Constraint uses SU.Fields, which subtype an (SU.SolosOf_ SU.Field) bound template.
          # That bound template uses SU.Constraint.
          voidBindings = mapSolos (_: _: U.Void) tvars;

          # Construct a new args set with the given bindings and rebuild function.
          mkArgsWithRebuild = bindings: rebuild: bindingsToArgs_ bindings // {
            inherit tvars tvarBindings rebuild;
          };
          mkVoidArgsWithRebuild = mkArgsWithRebuild voidBindings;

          # Homogenise so that we always have a (bindings: Super/null) function to work with in rebuild.
          bindingsToSuper =
            if U.isNull bindingsToSuperOrSuper then
              _: null
            else if U.isTypeSet bindingsToSuperOrSuper then 
              let Super = bindingsToSuperOrSuper;
              in _: Super
            else
              assert assertMsg (U.isFunction bindingsToSuperOrSuper) ''
                __newTemplate: bindingsToSuperOrSuper must be a Type or function returning a Type, got:
                ${log.print bindingsToSuperOrSuper}
              '';
              bindingsToSuperOrSuper;

          # rebuild recursively carries the ability to reconstruct the type from its current bindings.
          # Danger - any other modifications to the type not reflected in its original args will be lost.
          rebuild = bindings:
            let Super = bindingsToSuper bindings;
                args = mkArgsWithRebuild bindings rebuild;
            in if U.isNull Super 
              then U.Type name args
              else Super.subType name args;
        in 
          rebuild voidBindings;

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
    Universe = rec {
      U_0 = Quasiverse;
      U_1 = mkSubUniverse U_0;
      U_2 = mkSubUniverse U_1;
      U_3 = mkSubUniverse U_2;
      U_4 = mkSubUniverse U_3;
    };

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
    #TSUniverse = Universe.U_1;
    TSUniverse = Universe.U_0;
    TS = removeAttrs TSUniverse [
      "opts"
      "_U"
      "_SU"
    ];

    # Cause a Type to have itself as its own Type, eliding any information about
    # either bootstrap pseudo-types or the Type of its superuniverse.
    groundType = SU: U: Type:
      let Type__grounded = Type // {__Type = SU.TypeThunk Type__grounded;};
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
              Typelib = mkTypelib U SU;

              # Tersely print universes.
              __toString = _: "<Universe: ${opts.name}>";
            })
            (SU: U: U.Typelib) # Embed the contents of created typelib.
            (SU: U: mkCast SU U)
            (SU: U: mkInstantiation SU U)
            (SU: U: mkUniverseReferences opts SU U)
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
      with collective-lib.tests.Compare;
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
        (T: T typeName typeArgs)
        T;

    # Lazily cascading options that disable typechecking at levels U_0 and U_1.
    # Options for the next universe can be produced via 'resolve opts.descend'.
    mkUniverseOpts = U: level: rec {
      inherit level;
      name = "U_${toString level}";
      # Type is named identically at all levels.
      typeName = "Type";
      # Whether to allow 'null' to be used as an 'untyped' type.
      # Used in shims to break recursion.
      allowUntypedFields = field: U.getLevel (U.typeOf field) == 0;
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
      # We only expect Type to be fixed under Type "Type" when it contains and produces
      # no shim elements of the Quasiverse.
      # U_0 is entirely shim elements and a Type made of shims.
      # U_1 has a Type made from U_0's Type.new and is made of shim elements.
      # U_2 has a Type made from U_1's Type.new and is made of untyped U_1 elements.
      # U_3 has a Type made from U_2's Type.new and is made of typed U_2 elements.
      # Type in U3+ should be fixed under .new, made entirely of final typed elements and producing
      # types themselves made of final typed elements.
      checkTypeFixedUnderNew = level >= 5;
      # A thunk returning the options for the next-lower subuniverse.
      descend = SubU: NamedThunk "${name}.descend" (mkUniverseOpts SubU (level + 1));
    };

    # Make the "Type" Type in terms of the SU (and U, where safe)
    mkType = SU: U:
      with U.call 3 "mkType" SU U ___;
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
        Type__SU = assign "Type__SU" (
          SU.Type.new U.opts.typeName Type__args
        );

        # Construct Type as an instance of itself.
        # This is valid besides each application of (.__Type {}) ascending one universe level.
        # We fix this in the next stage by grounding.
        Type__bootstrapped =
          assert checks [{ name = "Type__SU has new";
                        cond = Type__SU ? new;
                        msg = indent.block ''
                          Type__SU must have a new function:
                            ${indent.here (log.print Type__SU)}
                          '';
                      }];
          assign "Type__bootstrapped" (
            Type__SU.new U.opts.typeName Type__argsGrounded
          );

        # Finally, ground this Type by setting Type.__Type to return itself, eliding any information
        # about the super-universe in the .__Type field.
        Type__grounded =
          assert checks [{ name = "Type__bootstrapped has new";
                        cond = Type__bootstrapped ? new;
                        msg = "Type__bootstrapped must have a new function";
                      }];
          assign "Type__grounded" (
            # In U_3 and beyond, this should now have reached a fixed point in terms
            # of further bootstrapping by Type.new, modulo lambda equality on created Type instances.
            # If enabled in the opts, an assertion checks that Type is fixed under further bootstrapping.
            if opts.checkTypeFixedUnderNew
              then groundTypeAndAssertFixed SU U opts Type__args Type__bootstrapped
              else groundType SU U Type__bootstrapped
          );

        # Expose the final version as Type.
        Type = Type__grounded;

        # Type = mkType SU U {}
        __functor = self: _: Type;
      };

    # The barest minimum universe to bootstrap the type system.
    # Constructs a bootstrapped Quasitype from a hand-build GroundType instance, which has no
    # typed fields.
    Quasiverse =
      with (log.v 4).attrs "Quasiverse" ___;
      with msg "Constructing Quasiverse";

      # Quasiverse is its own self- and super-universe, enabled by containing no circular dependencies
      # in quasitype construction.
      let U = Quasiverse; SU = Quasiverse; in

      withCommonUniverseSelf rec {
        # Take on the intial options for a root universe.
        # All other opts are generated from here via 'resolve opts.descend'
        opts = mkUniverseOpts U 0;

        # Can't check against U.isNull here due to Builtin recursion.
        safeIsNull = builtins.isNull;

        # Remove existing attributes
        noOverride = This: xs: removeAttrs xs (attrNames This);

        # Bind the given methods and staticMethods to the shim.
        # Does not override any existing shim attributes, allowing
        # shims to set up shim methods.
        bindShim = label: shim: methods_: staticMethods_:
          let methods = noOverride shim methods_;
              staticMethods = noOverride shim staticMethods_;
              shim_ = U.removeImplementsPrefixAttrs (mergeAttrsList [
                shim
                #(optionalAttrs
                #  ((shim.name or null) != "Type")
                #  (U.mkAccessorBindings (shim.__Type {}) shim_))
                (U.mkAccessorBindings (shim.__Type {}) shim_)
                (U.mkStaticMethodBindings
                  "bindShim.staticMethods ${label}"
                  (log.vprint shim)
                  staticMethods
                  shim_)
                (U.mkMethodBindings
                  "bindShim.methods ${label}"
                  (log.vprint shim)
                  methods
                  shim_)
              ]);
          in shim_;

        # A Type has bindings for Type.methods and This.staticMethods.
        # We could get these by merit of inspecting This.Type, but this
        # would cause recursive access to Type, so we manually use
        #   Type.methods == typeMethodsFor SU U // {} (no extra methods on Type)
        #   This.staticMethods == typeStaticMethodsFor SU U + {any extra staticMethods defined on This}
        bindTypeShim = This:
          let methods = typeMethodsFor SU U;
              staticMethods = typeStaticMethodsFor SU U // U.set This.staticMethods;
          in bindShim "bindTypeShim" This methods staticMethods;

        # An instance this of type This has bindings for This.methods.
        # If This == Type, then this is a Type, and has bindings for this.staticMethods
        # (any new static methods it defines for itself)
        # We can inspect __Type here without infinite recursion, since this is only
        # called inside new/ctor and not during Type definition itself.
        bindInstanceShim = this:
          let This = this.__Type {};
              methods = U.set This.methods;
              staticMethods = U.set (this.staticMethods or {});
          in bindShim "bindInstanceShim" this methods staticMethods;

        TypeThunk__new__lambdasOnly = T: {
          __Type = _: TypeThunk;
          __isTypeThunk = _: true;
          __functor = self: _: T;
        };

        TypeThunk__new = T: mkSafeUnboundInstanceShim "TypeThunk" {
          __Type = TypeThunk__new__lambdasOnly TypeThunk;
          __isTypeThunk = _: true;
          __functor = self: _: T;
        };

        TypeThunk = mkSafeUnboundTypeShimFunctor "TypeThunk" {
          __Type = TypeThunk__new__lambdasOnly Type;
          ctor = This: TypeThunk__new;
        };

        # Make a shim instance appearing as an instance of T.
        # T is only accessed in thunk bodies so can recursively specfiy the
        # containing type when this is called in a type shim's ctor.
        # Any given attrs are copied into the instance.
        mkSafeUnboundInstanceShimOf = T: attrs:
          (attrs // {
            __Type = attrs.__Type or (TypeThunk__new__lambdasOnly T);
          })
          // { __toString = self: "<unbound instance shim: ${T.name}>"; };

        mkSafeUnboundInstanceShim = name: attrs:
          mkSafeUnboundInstanceShimOf (mkSafeUnboundTypeShim name {}) attrs;

        mkSafeInstanceShimOf = T: attrs:
          bindInstanceShim (mkSafeUnboundInstanceShimOf T attrs)
          // { __toString = self: "<bound instance shim: ${T.name}>"; };

        # Make an instance of an adhoc type with the given name.
        # Any given attrs are copied into the instance (not the adhoc type)
        mkSafeInstanceShim = name: attrs:
          mkSafeInstanceShimOf (mkSafeTypeShim name {}) attrs;

        Solos__new = solos: {
          __Type = TypeThunk__new Solos;
          inherit solos;
          names = soloNames solos;
          values = soloValues solos;
          mapSolos = f: mapSolos f solos;
          partition = f: mapAttrs (_: Solos__new) (partitionSolos f solos);
          lookup = name: lookupSolos name solos;
          fmap = f: Solos__new (mapSolos f solos);
          filter = f: Solos__new (filterSolos f solos);
          concat = newSolos: Solos__new (concatSolos solos newSolos);
          merge = _: mergeSolos solos;
        };

        Solos = mkSafeUnboundTypeShimFunctor "Solos" {
          fields = [
            {solos = null;}
            {names = null;}
            {values = null;}
          ];
          ctor = This: Solos__new;
        };

        # Simulate constrained type
        SolosOf = T: mkSafeUnboundTypeShimFunctor "Solos" {
          fields = [
            {solos = null;}
            {names = null;}
            {values = null;}
          ];
          ctor = This: solos_:
            let solos = checkSolos solos_; in
            assert assertMsg (collections.all.solos (_: T.check) solos) "SolosOf shim: given solos not all of type ${T}: ${log.print solos}";
            Solos__new solos;
        };

        # Simulate taking the type arg in the first ctor arg.
        Field__new = FieldSpec: fieldName: mkSafeUnboundInstanceShim "Field" {
          inherit fieldName;
          inherit (U.parseFieldSpec FieldSpec)
            FieldType hasDefaultValue defaultValue isStatic;
        };

        Field = mkSafeTypeShim "Field" {
          fields = [
            {FieldType = null;}
            {hasDefaultValue = null;}
            {defaultValue = null;}
            {isStatic = null;}
          ];
          # Simulate taking the type arg in the first ctor arg.
          ctor = This: Field__new;
        };

        Fields__new = fieldSpecs:
          assert assertMsg (isSolos fieldSpecs) "fieldsShim: not solos: ${log.print fieldSpecs}";
          mkSafeUnboundInstanceShim "Fields" rec {
            FieldSpecs = (Solos__new [{__Type = null;}]).concat fieldSpecs;
            allFields = FieldSpecs.fmap (flip Field__new);
            instanceFields = Solos__new (mapSolos (flip Field__new) fieldSpecs);
            instanceFieldsWithType = allFields;
            requiredFields = instanceFields;
            staticFields = Solos__new [];
          };

        Fields = mkSafeUnboundTypeShimFunctor "Fields" rec {
          ctor = This: Fields__new;
          fields = [
            { FieldSpecs = null; }
            { allFields = null; }
            { instanceFields = null; }
            { instanceFieldsWithType = null; }
            { requiredFields = null; }
            { staticFields = null; }
          ];
        };

        # Make a shimmed instance of Type.
        mkSafeUnboundTypeShim = name: args: rec {
          __Type = args.__Type or (TypeThunk__new Type);
          __isTypeSet = args.__isTypeSet or True;
          __level = args.__level or 0;
          inherit name;
          __Super = args.__Super or null;
          fields = Fields__new (args.fields or []);
          ctor = args.ctor or Ctors.CtorDefault;
          methods = (typeMethodsFor SU U) // (args.methods or {});
          staticMethods = typeStaticMethodsFor SU U;
          tvars = Solos__new (args.tvars or []);
          tvarBindings = args.tvarBindings or {};
          rebuild = args.rebuild or null;

          __toString = self: "<unbound type shim ${name}>";
          __TypeId = _: name;
          getName = _: name;
          getBoundName = _: name;
        };

        mkSafeUnboundTypeShimFunctor = name: args:
          let
            T = mkSafeUnboundTypeShim name args // {
              __functor = self: arg: self.new arg;
              new = arg: T.ctor T arg;
            };
          in
            T;

        mkSafeTypeShim = name: args:
          (bindTypeShim (mkSafeUnboundTypeShim name args)) //
          {
            __toString = self: "<type shim: ${name}>";
            __TypeId = _: name;
            getName = _: name;
            getBoundName = _: name;

            # Need this otherwise __functor behaviour defaults to the bound 'new'
            __functor = self: arg: self.ctor self arg;
          };

        Ctor = (mkSafeUnboundTypeShimFunctor "Ctor" {
          fields = [{ctorFn = null;}];
          ctor = This: ctorFn: mkSafeUnboundInstanceShimOf Ctor rec {
             inherit ctorFn;
            __functor = self: arg: self.ctorFn self arg;
          };
        }) // ({
          __cast = x: U.castEither "lambda" x (r: Ctor (U.unwrapCastResult r)) id;
        });

        inherit (mkCtors SU U) Ctors;

        BuiltinTypeShims =
          mapAttrs
            (BuiltinName: builtinName:
              let
                BuiltinOf = 
                  let Tname = "BuiltinOf<${builtinName}>";
                  in mkSafeUnboundTypeShim "BuiltinOf" rec {
                    fields = [{value = builtinName;}];
                    ctor =
                      fn "ctor" {This = null;} {value = builtinName;}
                      (This: value: mkSafeUnboundInstanceShimOf BuiltinOf {
                        inherit value;
                      });
                  } // {
                    __cast = x: U.castEither builtinName x (r: BuiltinOf (U.unwrapCastResult r)) id;
                    mk = args: BuiltinOf args.value;
                    __functor = self: arg: self.ctor self arg;
                  };

                T__new = value: (mkSafeUnboundInstanceShimOf T {
                  getValue = _: value;
                  __value = BuiltinOf value;
                })
                // (optionalAttrs (BuiltinName == "Lambda") {
                  __functor = self: arg: self.__value.value arg;
                });

                T = mkSafeUnboundTypeShimFunctor BuiltinName {
                  ctor = This: T__new;
                  fields = [{__value = BuiltinOf;}];
                } // {
                  __functor = self: arg: self.ctor self arg;
                  mk = args: T args.__value;
                  __cast = 
                    dispatchlib.switch.def 
                      (x: U.castEither builtinName x (r: T (U.unwrapCastResult r)) id)
                      BuiltinName {
                        Set = x:
                          if U.isTyped x && size x != 1
                          then U.mkCastError "Set: cannot construct from non-solo attrs with __Type"
                          else U.castEither "set" x (r: Set (U.unwrapCastResult r)) id;
                      };
                };
              in 
                T
            )
            U.BuiltinNameTobuiltinName;
        inherit (BuiltinTypeShims) Null Int Float String Path Bool List Set Lambda;
        inherit (mkConstants BuiltinTypeShims) True False Nil;

        Any = mkSafeTypeShim "Any" {} // {
          __cast = x: x;
        };

        Void = mkSafeTypeShim "Void" {};

        Default = T: v: mkSafeUnboundInstanceShim "Default" {
          defaultType = _: T;
          defaultValue = _: v;
        };

        Static = T: mkSafeUnboundInstanceShim "Static" {
          staticType = _: T;
        };

        Union = Ts: mkSafeTypeShim "Union" {} // {
          __cast = x: U.unwrapCastResult (U.castFirst Ts x);
        };

        NullOr = T: Union [Null T];

        SetOf = T: Set // {
          __cast = x:
            let xs_ = mapAttrs (_: U.cast T) x;
                errors = filterAttrs (_: U.isCastError) xs_;
                xs = mapAttrs (_: U.unwrapCastResult) xs_;
            in if errors == [] then Set xs else U.combineCastErrors (attrValues errors);
        };

        ListOf = T: List // {
          __cast = x:
            let xs_ = map (U.cast T) x;
                errors = filter U.isCastError xs_;
                xs = map U.unwrapCastResult xs_;
            in if errors == [] then List xs else U.combineCastErrors errors;
        };

        # Shim out typeclass for (Cast T).cast x
        Cast = {
          checkImplements = T: T ? __cast;
          __functor = self: T: rec {
            cast = x: U.unwrapCastResult (T.__cast x);
          };
        };

        Type__fields = [
          { __isTypeSet = null; }
          { __level = null; }
          { name = null; }
          { __Super = null; }
          { ctor = null; }
          { fields = null; }
          { methods = null; }
          { staticMethods = null; }
          { tvars = null; }
          { tvarBindings = null; }
          { rebuild = null; }
        ];

        constructType = _: {
          __Type = TypeThunk__new {
            __TypeId = _: "Type";
            __isTypeSet = true;
            getName = _: "Type";
            getBoundName = _: "Type";
            name = "Type";
            fields = Fields__new Type__fields;
          };
          # Need to follow shim pattern of ctor returning full instance
          ctor = This:
            U.newInstanceWithCtor Ctors.CtorType This;
          methods = typeStaticMethodsFor SU U;
          staticMethods = typeStaticMethodsFor SU U;
          fields = Type__fields;
        };

        Type = mkSafeTypeShim "Type" (constructType {});
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
      with SU.call 4 "mkSubUniverse" "unsafe:SU" ___;
      with msg "Constructing ${opts.name} universe";
      let opts = SU.opts.descend U {};
          U = withCommonUniverse SU (rec {
        ### Universe

        inherit opts;

        ### Type

        __Bootstrap = mkType SU U;
        Type = __Bootstrap {};

        ### Ctor

        Ctor = Type "Ctor" {
          fields = [{ ctorFn = SU.Lambda; }];
          methods = {
            __implements__functor = this: This: arg: this.ctorFn This arg;
          };
        };

        inherit (mkCtors SU U) Ctors;

        ### Builtins

        Any_builtin = SU.Union SU.builtinNames;
        Any_Builtin = SU.Union SU.BuiltinNames;

        BuiltinOf = Type.template "BuiltinOf" { T = Any_builtin; } (_: {
          fields = [{ value = _.T; }];
        });

        mkBuiltinType = BuiltinName: builtinName:
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
                }.${BuiltinName};
              in
                methods 
                // {
                  # show (Int 6) returns e.g. "Int(6)"
                  __implements__show = this: self: "${U.typeIdOf self}(${toStringF self})";

                  # toString (Int 6) returns e.g. "6"
                  __implements__toString = this: self:
                    with U.methodCall 2 this "__toString" self ___;
                    return (toStringF self);
                } 
                // (optionalAttrs (builtinHasToShellValue builtinName) {
                  __implements__toShellValue = this: self:
                    toShellValue (self.getValue {});
                });

            hasSize = { String = true; Path = true; List = true; Set = true; }.${BuiltinName} or false;
            withSize = methods:
              if hasSize
              then let sizeFn = this: _: size (this.getValue {});
                  in methods // { size = sizeFn; }
              else methods;
          in
            Type BuiltinName {
              ctor = This: x: {
                __value = (BuiltinOf builtinName) x;
              };
              fields = [{
                __value = BuiltinOf builtinName;
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
                  __implements__functor = this: self: arg: U.lambda this arg;
                };
              }.${BuiltinName} or {})));
            };

        BuiltinTypes = mapAttrs mkBuiltinType U.BuiltinNameTobuiltinName;

        inherit (BuiltinTypes) Null Int Float String Path Bool List Set Lambda;

        ### Constants
        inherit (mkConstants BuiltinTypes) True False Nil;

        ### Trivial

        # Unit Type
        Unit = Type "Unit" {
          ctor = U.Ctors.CtorNullary;
          staticMethods.__implements__cast = This: x:
            if x == {} then unit
            else mkCastError "Unit: expected {}, got ${log.print x}";
        };
        unit = Unit {};

        # Uninhabited type
        Void = Type "Void" {
          ctor = This: _: throw "Void: ctor";
          staticMethods.__implements__cast = This: x:
            with indent; throws ''
              Cannot cast to Void; got ${here (log.print x)}
            '';
        };

        # Any type
        Any = Type "Any" {
          ctor = U.Ctors.CtorNone;
          staticMethods.__implements__cast = This: x: x;
        };

        ### Field Types

        # A type inhabited by only one value.
        Literal = Type.template "Literal" {V = Any;} (_: rec {
          ctor = U.Ctors.CtorNullary;
          staticMethods = {
            of = This: v: (Literal v) {};
            getLiteral = This: unused: _.V;
            __implements__cast = This: x:
              if x == _.V then x
              else mkCastError (indent.block ''
                Literal.cast:
                  Expected: ${indent.here (log.print V)}
                  Got: ${log.print x}
              '');
          };
        });

        # A type satisfied by any value of the given list of types.
        Union = Type.template "Union" {T = List;} (_: {
          ctor = U.Ctors.None;
          staticMethods.__implements__cast = This: x: castFirst_.T x;
        });

        # A value or T or Null.
        NullOr = T: Union [Null T];

        # A type indicating a default field type and value.
        Default_ = Type.template "Default" {T = Type; v = Any;} (_: {
          ctor = U.Ctors.None;
          # TODO: Static fields instead.
          staticMethods = {
            defaultType = This: _unused: _.T;
            defaultValue = This: _unused: _.V;
            __implements__cast = This: x: U.cast _.T x;
          };
        });

        # A type indicating a static field type.
        Static = Type.template "Static" {T = Type;} (_: {
          ctor = U.Ctors.None;
          # TODO: Static fields instead.
          staticMethods = {
            staticType = This: thunk _.T;
            __implements__cast = This: x: U.cast _.T x;
          };
        });

        ### Components

        ThunkOf = Type.template "ThunkOf" {T = Type;} (_: {
          fields = [{ thunk = Lambda; }];
          ctor = This: x: {
            thunk = Lambda (_:
              # Delegate the operation on x to avoid early forcing.
              # TODO: Typed lambdas instead.
              assert 
                U.castEither _.T x 
                  (_: true) 
                  (e: assertMsg false (indent.block ''
                    ${log.print This}: not castable to ${_.T}:
                      ${indent.here e.castError}
                  ''));
              x);
          };
          methods = {
            __implements__functor = this: self: _: self.thunk {};
          };
        });

        TypeLike = Union [Type Any_builtin];

        # TODO: Could just be a partially bound template
        TypeThunk = (ThunkOf TypeLike).subType "TypeThunk" {
          methods.__isTypeThunk = this: _: true;
          staticMethods = {
            __implements__cast = This: T:
              if U.isTypeLike T then This T
              else mkCastError_ "TypeThunk.cast: Not a TypeLike (${log.print T})";
          };
        };

        # TODO: Functor typeclass
        ContainerOf = Type.subTemplateOf (_: _.C) "ContainerOf" {C = Type; T = Type;} (_: {
          ctor = This: xs: {
            inherit ((This.superNew xs).fmap (cast _.T)) __value;
          };
        });

        ListOf = Type.subTemplateOf (_: (ContainerOf List _.T)) "ListOf" {T = Type;} (_: {});
        SetOf = Type.subTemplateOf (_: (ContainerOf Set _.T)) "SetOf" {T = Type;} (_: {});

        # A type that enforces a size on the value.
        Sized = Type.subTemplateOf (_: _.T) "Sized" {N = Int; T = Type;} (_: {
          ctor = This: arg:
            Variadic.compose
              (super: if x.size {} == _.N
                      then This.mk x.get
                      else indent.throws ''
                        ${This}: Got value of incorrect size:
                          ${indent.here (log.print x)}
                      '')
              (This.superNew arg);
        });

        # An attribute set with attributes zero-indexed by their definition order.
        # xs = Ordered [ {c = 1;} {b = 2;} {a = 3;} ];
        # xs.value == { a = 1; b = 2; c = 3; } (arbitrary order)
        # xs.names == [ "c" "b" "a" ] (in order of definition)
        # xs.values == [ 1 2 3 ] (in order of definition)
        SoloOf = Type.subTemplateOf (_: Sized 1 (SetOf _.T)) "SoloOf" { T = Type; } (_: {
          ctor = This: name: value: This.superCtor { ${name} = value; };
        });

        Solo = SoloOf Any;

        SolosOf = Type.subTemplateOf (_: ListOf (SoloOf _.T)) "SolosOf" {T = Type;} (_: {
          fields = [
            {solos = "list";}
            {names = "list";}
            {values = "list";}
          ];
          ctor = This: solos: This.superCtor xs // {
            inherit solos;
            names = soloNames solos;
            values = soloValues solos;
          };
          methods = {
            # TODO: General inference solution.
            fmap = this: f:
              let solos = this.mapSolos f;
                  T = U.typeOf (soloValue (def {_ = Any;} (maybeHead solos)));
              in SolosOf T solos;

            # Filter the solos by a pred of name: value: bool
            # TODO: typed lambdas avoid this
            # Returns a raw mapped solos until we can track the type
            mapSolos = this: f: mapSolos f this.solos;

            # Filter the solos by a pred of name: value: bool
            # Returns a new SolosOf T
            filter = this: pred: this.__new (filterSolos pred this.solos);

            # Filter the solos by a pred of name: value: bool
            # Returns a dict of {right = SolosOf T; wrong = SolosOf T;}
            partition = this: pred: mapAttrs (_: this.__new) (partitionSolos pred this.solos);

            # Look up a solo by name
            lookup = this: name: lookupSolos name this.solos;

            # Update by inserting the given items sequentially at the end of the order.
            # If any already appear, they update the item in its original order.
            # Returns a new SolosOf T
            concat = this: newSolos: this.__new (concatSolos this.solos (solos newSolos));

            # Return the raw set obtained by merging the solos into an attrset.
            merge = this: {}: mergeSolos this.solos;
          };
        });

        # Type-family-esque runtime extraction of field type from Spec type
        FieldSpec = Type.template "FieldSpec" [{Spec = Type;}] (_: {
          ctor = This: _: U.parseFieldSpec _.Spec;
          fields = [
            {FieldType = Type;}
            {isStatic = Bool;}
            {hasDefaultValue = Bool;}
            {defaultValue = NullOr (FieldTypeOf _.T);}
          ];
        });

        FieldTypeOf = T: (FieldSpec T).FieldType;

        # Intermediate type to enable (Field (Default Int 123)) to be both a SoloOf Type and a FieldOf Int as well as
        # Either:
        # (Field Int "name")
        # (Field (Static Int) "name") -> Field<Static<Int>>.FieldType == Int
        # (Field (Default Int 123) "name") -> Field<Default<Int, 123>>.FieldType == Int
        # (Field (Static (Default Int 123)) "name") -> Field<Static<Default<Int, 123>>>.FieldType == Int
        Field = Type.subTemplateOf (_: FieldSpec Spec) "Field" [{ Spec = Type; }] (_: {
          ctor = This: fieldName: This.superCtor {} // {inherit fieldName;};
          fields = [
            {fieldName = String;}
          ];
        });

        # Fields is an SolosOf that first converts any RHS values into Field types.
        # Fields ctor just converts the incoming list or set of field specs into
        # the list of Field that SolosOf Field expects.
        #
        # Fields { field = FieldType; ... }
        # Fields { field = Default FieldType defaultValue; ... }
        # Fields { field = Static FieldType; ... }
        # Fields [ { field = FieldType; ... } ... ]
        # Fields [ { field = Default FieldType defaultValue; ... } ... ]
        # Fields [ { field = Static FieldType; ... } ... ]
        Fields = Type "Fields" {
          fields = [
            {FieldSpecs = SolosOf Type;}
            {allFields = SolosOf Field;}
            {instanceFields = SolosOf Field;}
            {instanceFieldsWithType = SolosOf Field;}
            {requiredFields = SolosOf Field;}
            {staticFields = SolosOf Field;}
          ];
          ctor = This: FieldSpecs_: {
            FieldSpecs = SolosOf Type (withTypeFieldSpec opts SU FieldSpecs_);
            allFields = SolosOf Field (fieldSpecs.mapSolos (name: FieldSpec: Field FieldSpec name));
            staticFields = allFields.filter (_: field: !(U.isNull field) && field.isStatic);
            instanceFieldsWithType = allFields.filter (_: field: U.isNull field || !field.isStatic);
            instanceFields = instanceFieldsWithType.filter (name: _: name != "__Type");
            requiredFields = instanceField.filter (_: field: !(field.defaultValue == null));
          };
        };

        ### Typeclasses

        # A constraint on a type variable, class or instance.
        Constraint = Type "Constraint" {
          fields = [{ constraintType = Type; }];
          methods = {
            # Cast the given type variable according to the constraint.
            # Only do so if it doesn't already satisfy i.e. don't try to cast Void, Union, All, etc.
            checkConstraint = this: that:
              U.isCastSuccess (this.castConstraint that);

            # Cast the given type variable according to the constraint.
            castConstraint = this: that:
              U.cast this.constraintType that;
          };
        };

        # Constructed as either:
        # - Instance constraint head implementation
        # - Instance head implementation
        Instance = Type "Instance" {
          ctor = This: constraintOrHead:
            if U.isType Constraint constraintOrHead then
              let constraint = constraintOrHead; in
              head: implementation: {
                inherit constraint head implementation;
              }
            else
              let head = constraintOrHead; in
              implementation: {
                constraint = Any;
                inherit head implementation;
              };
          fields = [
            # The instance constraint, or Any.
            { constraint = Constraint; }
            # The instance head defining the types to which the instance applies.
            { head = Type; }
            # The implementation of the class (a set of the class methods without underscore names)
            { implementation = SetOf Lambda; }
          ];
          methods = {
            # True iff the instance is applicable to the given type.
            # For now only supports nullary instances.
            match = this: T:
              this.constraint.checkConstraint T
              && this.head.eq T;

            # Get 'that' but with the instance methods bound to it.
            # Binding occurs with double underscores such that e.g. other methods can predictably
            # refer to the bound version.
            # TODO: no, old methods are still bound against the original value s.t. they see __toString
            # as being its old value if it existed.
            bindInstance = this: that:
              mapAttrs (_: impl: impl that) (U.set this.implementation);
          };
        };

        # TODO: To support this we need a tvar/tvarbinding scheme for Classes/Constraints/Instances
        # and a way to go from e.g. ListOf Int to ListOf _T_ detecting _T_ == Int, so that e.g. ToString _T_
        # can be determined as ToString Int and checked as a constraint.
        #
        # A type variable that can be used as a placeholder in instance constraints and heads.
        # Type variable identity takes place against 'name', so
        # _T1_ = TypeVar "T"
        # _T2_ = TypeVar "T"
        # are not different type variables, but the same type variable, as far as instance resolution
        # is concerned.
        TypeVar = Type "TypeVar" {
          fields = [{name = String;}];
        };
        _T_ = TypeVar "T";

        # Typeclass equivalents with optional default instances.
        #
        # Class "ToString" { toString = ClassMethod; };
        #
        # or with default instances, using the __functor instance of Class:
        #
        # ToString =
        #   Class "ToString"
        #    { toString = ClassMethod; }
        #    (Instance Int {
        #      toString = this: self: toString (self.getValue {});
        #    })
        #    (Instance (ToString _T_) (ListOf _T_) {
        #      toString = this: self: concatStringsSep "\n" (map toString this);
        #    });
        #
        # TODO: That final one requires recursive reference to ToString
        Class = U.Type "Class" {
          fields = [
            # The name of the Class
            { name = String; }
            # The class methods named without underscores e.g.
            # {
            #   toString = self: self.id or "<no ID field in default toString>"
            # }
            # or without a default:
            # { toStringNoDefault = {}; } or { toStringNoDefault = unit; }
            { classMethods = SetOf (Union [Unit Lambda]); }
            # Any default instances to include in the class e.g. for
            # builtin types. These are provided via addInstance and the __functor implementation.
            { instances = Default (ListOf Instance) (ListOf Instance []); }
          ];
          methods = {
            addInstance = this: instance:
              this.modify.instances (instances: instances.append instance);

            implementsPrefixedClassMethods = this: _:
              concatMapAttrs
                (name: method: { "__implements__${name}" = method; })
                (this.classMethods.getValue {});

            underscorePrefixedClassMethods = this: _:
              concatMapAttrs
                (name: method: { "__${name}" = method; })
                (this.classMethods.getValue {});

            # Get the methods of the given TypeSet that match those of the class.
            # The TypeSet has these as __implements__ prefixed methods.
            # This returns them as e.g. toString rather than __implements__toString or __toString.
            getTypeSetInstance = this: T:
              assert assertMsg (U.isTypeSet T) (indent.block ''
                  Class.getTypeSetInstance: expected TypeSet, got:
                  ${indent.here (log.print T)}
                '');
              let
                methodsC = this.implementsPrefixedClassMethods {};
                defaultMethodsC = this.getDefaultInstanceMethods {};
                methodsT = U.set T.methods;
                prefixedInstanceMethodsT = intersectAttrs methodsC methodsT;
                instanceMethodsTNoDefaults = U.removeImplementsPrefixAttrs prefixedInstanceMethodsT;
                instanceMethodsT = defaultMethodsC // instanceMethodsTNoDefaults;
                instance = Instance T instanceMethodsT;
              in
                assert assertMsg (attrNames instanceMethodsT == attrNames (U.set this.classMethods)) (indent.block ''
                  Class.getTypeSetInstance: TypeSet does not implement ${this.name}:
                    Expected methods: ${indent.here (log.print this.classMethods)}
                    Default methods: ${indent.here (log.print defaultMethodsC)}
                    Found methods (without defaults): ${indent.here (log.print instanceMethodsTNoDefaults)}
                    Found methods (including defaults): ${indent.here (log.print instanceMethodsT)}
                  '');
                instance;

            # Get the default instance subset for the class, if any.
            getDefaultInstanceMethods = this: _:
              filterAttrs (_: U.isFunction) (U.set this.classMethods);

            # Get the default instance subset for the class, if any.
            getDefaultInstance = this: T:
              assert assertMsg (this.hasTotalDefaultInstance {}) (indent.block ''
                Class.getDefaultInstance: Class ${this.name} does not have a total default instance:
                  ${indent.here (log.print this.classMethods)}
              '');
              Instance T (this.getDefaultInstanceMethods {});

            # Get the default instance subset for the class, if any.
            hasTotalDefaultInstance = this: _:
              size (this.getDefaultInstanceMethods {}) == size (set this.classMethods);

            # Get a list of the instances that match the given type, besides any implemented
            # directly by the type.
            getMatchingInstances = this: T:
              (errors.try (this.getTypeSetInstance T) (_: []))
              ++ (filter (instance: instance.match T) (U.list this.instances))
              ++ (optionals (this.hasTotalDefaultInstance {}) [(this.getDefaultInstance {})]);

            # Gets the first matching instance for the given type, or null if there is no match.
            getInstance = this: T:
              assert assertMsg (U.isTypeLike T) "Class.getInstance: expected TypeLike, got ${log.print T}";
              maybeHead (this.getMatchingInstances T);

            # Gets the first matching instance for the given value.
            # e.g. getValueInstance (Int 123) == Instance Int ...
            getValueInstance = this: x: this.getInstance (typeOf x);

            # True iff T is an instance of this class.
            # Is an instance either if any instance of T has all of the class methods,
            # or if a registered instance matches T.
            checkImplements = this: T: !(U.isNull (this.getInstance T));

            # True iff x is a value that this class can be called upon.
            checkValueImplements = this: x: !(U.isNull (this.getValueInstance x));

            # If the argument is an Instance, add it to the class.
            # ToString = Class "ToString" { toString = ClassMethod; } (Instance Int { toString = this: ...;}) ...;
            #
            # Otherwise, check if the argument implements the class and expose the class's methods
            # against it.
            # (ToString 123).toString == "123"
            __implements__functor = this: self: x:
              if U.isType Instance x then
                this.addInstance x
              else
                this.try x;

            # Exposes for e.g. (ToString.try 123).toString == "123"
            # or (ToString.try (_: 123)).toString or (throw ''no instance for lambda'')
            try = this: x:
              let instance = this.getValueInstance x; in
              if U.isNull instance then {}
              else instance.bindInstance x;

            mustCall = this: x:
              let boundInstance = this.try x; in
              assert assertMsg (!(U.isNull boundInstance)) (indent.block ''
                Value of type ${typeOf x} does not implement Class ${this.name}:
                  value = ${indent.here (log.print x)}
              '');
              boundInstance;
          };
        };

        # True iff type T implements Class C.
        implements = C: T: C.checkImplements T;

        # True iff type T implements Class C.
        valueImplements = C: x: C.checkValueImplements x;

        ToString =
          Class "ToString" {
            toString = log.print;
          }
          (Instance "string" {
            toString = id;
          });

        Implements = Type.template "Implements" [{ C = Class; }] (_: {
          staticMethods.__implements__cast = This: That:
            if _.C.implementedBy That then That
            else mkCastError (indent.block ''
              Implements: Type ${That} does not implement ${_.C.name}:
                ${log.print That}
            '');
        });

        HasField = Type.template "HasField" { F = String; T = Type; } (_: {
          ctor = U.Ctors.None;
          staticMethods.__implements__cast = This: T:
            if _.T.fields.instanceFieldsWithType ? ${string _.F}
              then mkCastSuccess_ T
              else mkCastError "${This}: ${T} does not have field ${_.F}";
        });

        # Typeclass for casting
        Cast = Class "Cast" {
          cast = {};
        };

        ### Independent (Only ever accessed as e.g. SU.Constraint) 

        # Base type for enums.
        Enum = Type "Enum" {};

        # Create an enum type from a list of item names.
        # MyEnum = mkEnum "MyEnum" [ "Item1" "Item2" "Item3" ]
        # MyEnum.names == [ "Item1" "Item2" "Item3" ]
        # MyEnum.fromName "Item1" == 0
        # MyEnum.fromIndex 0 == "Item1"
        mkEnum = enumName: itemNames:
          let Item = Enum.subType enumName {
                fields = [
                  {i = Int;}
                  {name = String;}
                ];
                staticMethods = {
                  # Enum members live on the Enum type itself.
                  __items = This: _: zipListsWith Item (range 0 (length itemNames - 1)) names;
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

  _tests =
    with collective-lib.tests;
    with lib;  # lib == untyped default pkgs.lib throughout __tests

    let
      testInUniverses = Us: test: mapAttrs (_: U: test U) Us;

      TestTypes = U: with U; {
        MyType =
          Type "MyType" {
            fields = [{ myField = String; }];
            methods = {
              helloMyField = this: extra: "Hello, ${this.myField.getValue {}}${extra}";
            };
          };

        MyType2 =
          Type "MyType2" {
            fields = [
              { stringField = String; }
              { intField = Int; }
              { defaultIntField = Default Int 666; }
              { staticIntField = Static Int; }
              { staticDefaultIntField = Static (Default Int 666); }
            ];
          };

        MyString =
          Type "MyString" {
            fields = [{ value = String; }];
            methods = {
              getValue = this: _: this.value.getValue {};
            };
          };

        Mystring =
          Type "Mystring" {
            fields = [{ value = "string"; }];
            methods = {
              getValue = this: _: this.value;
            };
          };

        WrapString =
          String.subType "WrapString" {};
      };

      classTests = U: 
        with U; 
        let 
          SU = U._SU;

          withInstances = class:
            class
              (Instance class "int" {
                unaryMethod = this: x: this + x;
              })
              (Instance class "Int" {
                unaryMethod = this: x: (int this) - x;
              })
              (Instance class String {
                unaryMethod = this: x: "String: ${this} ${toString x}";
              })
              (Instance class "string" {
                unaryMethod = this: x: "string: ${this} ${toString x}";
              })
              (T: Instance (HasField "abc" T) class T {
                unaryMethod = this: x: "HasField abc: abc = ${toString this.abc}, x = ${toString x}";
              })
              (T: Instance (HasField "def" T) class T {
                unaryMethod = this: x: "HasField def: def = ${toString this.def}, x = ${toString x}";
              });

          classTestClasses = rec {
            UnaryClass =
              Class "UnaryClass" {
                classMethods = {
                  unaryMethod = {};
                };
              };

            UnaryClassWithInstances =
              withInstances
                (Class "UnaryClass" {
                  classMethods = {
                    unaryMethod = {};
                  };
                });

            UnaryClassWithDefaultImpl =
              Class "UnaryClassWithDefaultImpl" {
                classMethods = {
                  unaryMethod = this: x: "default: ${log.print this} | ${toString x}";
                };
              };

            UnaryClassWithDefaultImplAndInstances =
              withInstances
                (Class "UnaryClassWithDefaultImplAndInstances" {
                  classMethods = {
                    unaryMethod = this: x: "default: ${log.print this} | ${toString x}";
                  };
                });
          };

          classTestTypes = {
            withUnaryMethod = {
              TypeWithUnaryMethod = Type "TypeWithUnaryMethod" {
                methods = {
                  unaryMethod = this: x: "${typeNameOf this}: ${toString x}";
                };
              };

              TypeWithUnderscoredUnaryMethod = Type "TypeWithUnderscoredUnaryMethod" {
                methods = {
                  __unaryMethod = this: x: "${typeNameOf this}: ${toString x}";
                };
              };

              TypeWithPrefixedUnaryMethod = Type "TypeWithPrefixedUnaryMethod" {
                methods = {
                  __implements__unaryMethod = this: x: "${typeNameOf this}: ${toString x}";
                };
              };

              int = "int";
              inherit Int;
              string = "string";
              inherit String;
            };

            withoutUnaryMethod = {
              bool = "bool";
              inherit Bool;
            };

            TypeWith = {
              ABC = Type "TypeWithABC" {
                fields = [{ abc = Int; }];
              };

              DEF = Type "TypeWithABC" {
                fields = [{ def = Int; }];
              };

              XYZ = Type "TypeWithXYZ" {
                fields = [{ xyz = Int; }];
              };
            };

          };

        in {
          implements.byExplicitImpl =
            mapAttrs (_: C:
              mapAttrs (_: T:
                expect.True (implements C T))
                {
                  inherit (classTestTypes.withUnaryMethod) 
                    TypeWithUnaryMethod
                    TypeWithUnderscoredUnaryMethod
                    TypeWithPrefixedUnaryMethod;
                })
                # All classes satisfied by those with methods explicitly implemented
                classTestClasses;

          implements.byInstance =
            mapAttrs (_: C:
              mapAttrs (_: T: 
                expect.True (implements C T))
                { inherit (classTestTypes.withUnaryMethod) 
                    int Int string String;
                  inherit (classTestTypes)
                    TypeWithABC
                    TypeWithDEF; })
              # Only those classes with default implementations or that include
              # these types in their instances
              { inherit (classTestClasses)
                  UnaryClassWithInstances
                  UnaryClassWithDefaultImplAndInstances; };

          implements.byDefaultImpl =
            mapAttrs (_: C:
              mapAttrs (_: T: 
                expect.True (implements C T))
                # All types should be covered
                (classTestTypes.withoutUnaryMethod // classTestTypes.TypeWith))
              { inherit (classTestClasses)
                  UnaryClassWithDefaultImpl
                  UnaryClassWithDefaultImplAndInstances; };

          implements.not.byExplicitImpl =
            mapAttrs (_: C:
              mapAttrs (_: T: 
                expect.False (implements C T))
                { inherit (classTestTypes.withUnaryMethod) 
                  int Int string String; } // classTestTypes.TypeWith)
              { inherit (classTestClasses) UnaryClass; };

          implements.not.byExplicitImplOrInstance =
            mapAttrs (_: C:
              mapAttrs (_: T: 
                expect.False (implements C T))
                classTestTypes.withoutUnaryMethod // {
                  # XYZ does not match the contstraint.
                  inherit (classTestTypes.TypeWith) TypeWithXYZ;
                })
              { inherit (classTestClasses)
                  UnaryClass
                  UnaryClassWithInstances; };

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

        isAttrs = {
          set.empty.typed = expect.True (isAttrs {});
          set.empty.untyped = expect.True (isUntypedAttrs {});
          Set.empty.typed = expect.True (isAttrs (Set {}));
          Set.empty.untyped = expect.False (isUntypedAttrs (Set {}));  # Set is itself typed
          set.solo.Type.typed = expect.True (isAttrs {__Type = _: "string";});
          set.solo.Type.untyped = expect.False (isUntypedAttrs {__Type = _: "string";});
          Set.solo.Type.typed = expect.True (isAttrs (Set {__Type = _: "string";}));
          Set.solo.Type.untyped = expect.False (isUntypedAttrs (Set {__Type = _: "string";}));
          int.typed = expect.False (isAttrs 123);
          int.untyped = expect.False (isUntypedAttrs 123);
          Int.typed = expect.True (isAttrs (Int 123));
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
          # Presence of __Type triggers isTyped
          TypedSetstring = expect.eqWith typeEq (typeOf {__Type = TypeThunk "string";}) "string";
          TypedSetThunk = expect.eqWith typeEq (typeOf {__Type = TypeThunk Int;}) Int;
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
          # Presence of __Type triggers isTyped
          TypedSetstring = expect.eq (typeIdOf {__Type = TypeThunk "string";}) "string";
          TypedSetThunk = expect.eq (typeIdOf {__Type = TypeThunk (Literal 123);}) "Literal<123>";
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
          # Presence of __Type triggers isTyped
          TypedSetstring = expect.eq (typeNameOf {__Type = TypeThunk "string";}) "string";
          TypedSetThunk = expect.eq (typeNameOf {__Type = TypeThunk (Literal 123);}) "Literal";
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
          # Presence of __Type triggers isTyped
          TypedSetstring = expect.eq (typeBoundNameOf {__Type = TypeThunk "string";}) "string";
          TypedSetThunk = expect.eq (typeBoundNameOf {__Type = TypeThunk (Literal 123);}) "Literal<123>";
        };

        wrapping = 
          let wrapEq = typedExpectFn: untypedExpectFn: T: x: {
            wrap.raw = typedExpectFn (wrap x) (T x);
            wrap.wrapped = typedExpectFn (wrap (wrap x)) (T x);
            wrap.typed = typedExpectFn (wrap (T x)) (T x);
            unwrap.raw = untypedExpectFn (unwrap x) x;
            unwrap.wrapped = untypedExpectFn (unwrap (wrap x)) x;
            unwrap.unwrapped.typed = untypedExpectFn (unwrap (unwrap (T x))) x;
            unwrap.typed = untypedExpectFn (unwrap (T x)) x;
          };
          in {
            _null = wrapEq expect.valueEq expect.eq Null null;
            bool = wrapEq expect.valueEq expect.eq Bool true;
            int = wrapEq expect.valueEq expect.eq Int 123;
            string = wrapEq expect.valueEq expect.eq String "abc";
            list = wrapEq expect.valueEq expect.eq List [1 2 3];
            set = wrapEq expect.valueEq expect.eq Set {a = 1; b = 2; c = 3;};
            path = wrapEq expect.valueEq expect.eq Path ./.;
            lambda = wrapEq (expect.eqOn (f: f 2)) (expect.eqOn (f: f 2)) Lambda (x: x + 1);
          };
      };

      smokeTests = U: with U; let SU = U._SU; in {

        Type =
          let A = Type "A" {}; in
          {
            Type = expect.stringEq ((A.__Type {}).__TypeId {}) "Type";
            id = expect.stringEq (A.__TypeId {}) "A";
            name = expect.stringEq A.name "A";
            getBoundName = expect.stringEq (A.getBoundName {}) "A";
          };

      };

      shimTests = U: with U; {
        inherit (instantiationTests U)
          Type
          TypeThunk
          Fields
          ;
      };

      instantiationTests = U: with U; with TestTypes U; {
        Type = expect.eq
          (let SomeType = Type "SomeType" {}; in
          SomeType.__TypeId {})
          "SomeType";

        TypeThunk =
          let T = Type "SomeType" {};
              TT = TypeThunk T; in
            expect.fieldsEq (TT {}) T;

        Solo =
          let
            validSolo = ((Solo String) { abc = "xyz"; });
          in {
            getName = expect.eq (validSolo.getName {}) "abc";
            getValue = expect.eq (validSolo.getValue {}) "xyz";
            getSolo = expect.eq (validSolo.getSolo {}) {abc = "xyz";};
          };

        SolosOf =
          let
            validSolos = (SolosOf String) [{c = "c";} {b = "b";} {a = "a";}];
          in {
            names = expect.eq validSolos.names ["c" "b" "a"];
            values = expect.eq validSolos.values ["c" "b" "a"];
            mapItems = expect.eq (validSolos.mapItems (item: item.getName {})) ["c" "b" "a"];
            getSolos = expect.eq (validSolos.solos) [{c = "c";} {b = "b";} {a = "a";}];
            updateSet =
              expect.eq
                ((validSolos.concat {b = "B"; d = "d";}).solos)
                [{c = "c";} {b = "B";} {a = "a";} {d = "d";}];
            updateList =
              expect.eq
                ((validSolos.concat [{b = "B";} {d = "d";}]).solos)
                [{c = "c";} {b = "B";} {a = "a";} {d = "d";}];
        };

        Literal = {
          static =
            expect.eq
              (let L = Literal 123; in
              L.getLiteral {})
              123;
          instance =
            expect.eq
              (let L = Literal 123; in
              (L {}).getLiteral {})
              123;
        };

        Field =
          let F = {
                ofNull = Field null "nullField";
                ofint = Field "int" "intField";
                ofInt = Field Int "intField";
                ofDefaultint = Field (Default "int" 123) "defaultintField";
                ofDefaultIntUncast = Field (Default Int 123) "defaultintField";
                ofDefaultIntUpcast = Field (Default Int 123) "defaultintField";
                ofDefaultintDowncast = Field (Default "int" (Int 123)) "defaultintField";
                ofDefaultInt = Field (Default Int (Int 123)) "defaultintField";
              };
          in concatMapAttrs (name: f: {
            ${name} = {
              name = expect.stringEq f.name "name";
              FieldType = expect.eq (f.FieldType {}) null;
              isStatic = expect.False (f.isStatic {});
              defaultValue = expect.eq (f.defaultValue {}) null;
            };
          }) F;

        Fields =
          let
            testFields = fields: args: {
              getSoloNames =
                expect.eq (fields.allFields.names) (soloNames args.expectedSolos);
              getSoloFieldTypes =
                expect.eqOn typeNameOf (fields.allFields.values) (soloValues args.expectedSolos);
              # indexed = mergeAttrsList (collective-lib.attrsets.indexed fieldSolos);
              # update = newFieldSpecs:
              #   Fields (concatSolos (solos fieldSpecs) (solos newFieldSpecs));
              # getField = name: indexed.${name}.value;
              # getFieldsWhere = pred: filterSolos pred (solos);
              # instanceFields = _:
              #   getFieldsWhere (name: field:
              #     name != "__Type"
              #     && (field == null
              #         || !(field.isStatic {})));
              # instanceFieldsWithType = _:
              #   getFieldsWhere (name: field:
              #     field == null
              #     || !(field.isStatic {}));
              # requiredFields = _:
              #   getFieldsWhere (_: field:
              #     field == null
              #     || (!(field.isStatic {})
              #         && !(field.hasDefaultValue {})));
            };
            expectedValues = rec {
              TypeField = Field TypeThunk "__Type";
              aField = Field null "a";
              bField = Field "int" "b";
              cField = Field (Default Int 3) "c";
              dField = Field (Static Int) "d";
              expectedSolos = [{__Type = TypeField;} {a = aField;} { b = bField;} {c = cField;} {d = dField;}];
            };
          in
          {
            fromSolos =
              testFields 
                (Fields [{a = null;} {b = "int";} {c = Default Int 3;} {d = Static Int;}])
                expectedValues;
          };

        Mystring = {
          mkFromString =
            expect.stringEq
              ((MyString.mk { value = "hello"; }).value)
              "hello";
          newFromString =
            expect.stringEq
              ((MyString.new "hello").value)
              "hello";
        };

        MyType = {
          mk.nocast =
            expect.stringEq
              (MyType.mk { myField = String "World"; }).myField
              "World";

          new.nocast =
            expect.stringEq
              (MyType.new (String "World")).myField
              "World";
        };
      };

      builtinTests = U: with U; {
        mk =
          let
            mkBuiltinTest = T: name: rawValue:
              let x = T rawValue;
              in {
                name = expect.stringEq (x.__Type {}).name name;
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
            Bool = expect.False (isbuiltinValue (Bool true));
          };

          Lambda = {
            invoke.directly = expect.eq ((Lambda (x: x+1)).getValue {} 2) 3;
            invoke.lambda = expect.eq (lambda (Lambda (x: x+1)) 2) 3;
            invoke.functor.new = expect.eq (Lambda.new (x: x+1) 2) 3;
            invoke.functor.functor = expect.eq (Lambda (x: x+1) 2) 3;
          };

          Set = {
            notype.invoke.new = 
              expect.eq ((Set.new {a = 1; b = 2; c = 3;}).getValue {}) {a = 1; b = 2; c = 3;};
            notype.invoke.functor = 
              expect.eq ((Set {a = 1; b = 2; c = 3;}).getValue {}) {a = 1; b = 2; c = 3;};
            typeSolo.invoke.new = 
              expect.eq ((Set.new {__Type = null;}).getValue {}) {__Type = null;};
            typeSolo.invoke.functor = 
              expect.eq ((Set {__Type = null;}).getValue {}) {__Type = null;};
            type.invoke.new =
              expect.error (Set.new {__Type = null; other = 123;});
            type.invoke.functor =
              expect.error (Set {__Type = null; other = 123;});
          };
        };

      untypedTests = U: with U;
        let
          fields = [
            { a = null; }
            { b = null; }
            { c = null; }
          ];
          fieldsFromSet = {
            c = null;
            b = null;
            a = null;
          };
        in {
          fromListEqFromSet =
            expect.eqOn
              (this: soloNames (this.solos))
              fields fieldsFromSet;

          getSolos =
            expect.eq
              (soloNames (fields.solos))
              [ "__Type" "a" "b" "c" ];
        };

      castTests = U: with U;
        let
          cases = [
            { name = "Null"; T = Null; v = null; }
            { name = "Int"; T = Int; v = 123; }
            { name = "Float"; T = Float; v = 12.3; }
            { name = "String"; T = String; v = "abc"; }
            { name = "Path"; T = Path; v = ./.; }
            { name = "Bool"; T = Bool; v = true; }
            { name = "List"; T = List; v = [1 2 3]; }
            { name = "Set"; T = Set; v = { a = 1; b = 2; c = 3; }; }
          ];
        in {
          to = mergeAttrsList (map (case: {
            ${case.name} = {
              cast = expect.valueEq (cast case.T case.v) (case.T case.v);
              castEither = 
                let result = castEither case.T case.v unwrapCastResult id;
                in expect.eq (result.getValue {}) case.v;
              wrap = expect.eq ((wrap case.v).getValue {}) case.v;
              new = expect.eq ((case.T.new case.v).getValue {}) case.v;
              functor = expect.eq ((case.T case.v).getValue {}) case.v;
              field = 
                let fieldName = "field${case.name}";
                    field = Field case.T fieldName;
                    args = { ${fieldName} = case.v; };
                in {
                  by.arg = 
                    expect.eqOn (x: x.value)
                      (rec { result = castFieldArgValue "This" args fieldName field;
                             value = castErrorOr result (result: if result ? getValue then result.getValue {} else result); })
                      ({value = case.v;});
                  by.value = 
                    expect.eqOn (x: x.value)
                      (rec { result = castFieldValue "This" case.v fieldName field;
                             value = castErrorOr result (result: if result ? getValue then result.getValue {} else result); })
                      ({value = case.v;});
                };
            };
          }) cases);
        };

      typeCheckingTests = U: with U;
        let
          A = Type "A" {
            fields = [
              { a = "int"; }
              { b = Int; }
              { c = Default Int 5; }
              { d = Default Int (Int 10); }
            ];
          };
        in {
          Int = {
            valid = expect.eq ((Int 123).getValue {}) 123;
            invalid = expect.error (Int "123");
          };

          A = {
            valid =
              let x = A 1 (Int 2);
              in
                expect.eq
                  [x.a (x.b.getValue {}) (x.c.getValue {}) (x.d.getValue {})]
                  [1 2 5 10];
            #wrongType = {
            #  a = expect.error (A "1" 2 3 4);
            #  b = expect.error (A 1 "2" 3 4);
            #  c = expect.error (A 1 2 "3" 4);
            #  d = expect.error (A 1 2 3 "4");
            #};
          };

          castInMk = with TestTypes U; {
            MyString = {
              mkFromstring = expect.eq ((MyString.mk { value = "hello"; }).getValue {}) "hello";
              newFromstring = expect.eq ((MyString.new "hello").getValue {}) "hello";
              eqString = expect.valueEq (MyString "hello") (String "hello");
            };

            WrapString = {
              mkFromstring = expect.eq ((WrapString.mk { __value = "hello"; }).getValue {}) "hello";
              newFromstring = expect.eq ((WrapString.new "hello").getValue {}) "hello";
              eqString = expect.valueEq (WrapString "hello") (String "hello");
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

      inheritanceTests = U:
        with U;
        with TestTypes U;
        let
          A = Type "A" {
            fields = [{
              a = String;
            }];
          };
          B = A.subType "B" {
            ctor = This: a: b: {
              inherit a b;
            };
            fields = [{
              b = Int;
            }];
          };
        in
          {
            newA = expect.lazyEqOn Compare.Fields (_: A "a") (_: A.mk { a = "a"; });

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
          TypeIsType = expect.True (isType Type Type);
        };

        init = {
          byNew = expect.eq ((Type.new "MyType" {}).__TypeId {}) "MyType";
          byFunctor = expect.eq ((Type "MyType" {}).__TypeId {}) "MyType";
        };

        methodCalls = {
          MyType_methods = {
            expr = attrNames (U.set MyType.methods);
            expected = [ "helloMyField" ];
          };
          MyType_call = {
            expr =
              let this = MyType (String "World");
              in this.helloMyField "!";
            expected = "Hello, World!";
          };
        };

        setField = {
          MyType_set_nocast = {
            expr =
              let this = MyType (String "");
              in [
                (this.helloMyField "!")
                ((this.set.myField (String "World")).helloMyField "!")
              ];
            expected = [ "Hello, !" "Hello, World!" ];
          };
        };

        modifyField = {
          MyType_modify = {
            expr =
              let this = MyType (String "Hello");
              in (this.modify.myField (x: String "${x.getValue {}}, World!"));
            expected = MyType (String "Hello, World!");
            compare = this: this.myField.getValue {};
          };
        };

        defaults =
          let
            WithDefault = Type "WithDefault" {
              fields = _: [
                { a = Default Int 123; }
                { b = Default Int (Int 123); }
              ];
            };
            F = Field (Default Int 123) "F";
          in {
            field.parse.name = expect.eq ((parseFieldSpec Int).FieldType.getName {}) "Int";
            field.parse.hasNoDefault = expect.False ((parseFieldSpec Int).hasDefaultValue);
            field.parse.hasDefaultValue = expect.True ((parseFieldSpec (Default Int 123)).hasDefaultValue);
            field.parse.defaultValue = expect.eq ((parseFieldSpec (Default Int 123)).defaultValue) 123;
            field.FieldType = expect.eq ((F.FieldType {}).getName {}) "Int";
            field.hasDefaultValue = expect.True (F.hasDefaultValue {});
            field.defaultValue = expect.eq (F.defaultValue {}) 123;
            sets.untyped = expect.eq (int (WithDefault {}).a) 123;
            sets.typed = expect.eq ((WithDefault {}).b.getValue {}) 123;
            overrides = expect.eq ((WithDefault.mk { a = Int 456; }).a.getValue {}) 456;
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

      bootstrapTests = U:
        with U;
        let SU = U._SU; in
        with __Bootstrap;
        {
          Type__args = {
            ctor.defaults =
              let expected = Type__args.ctor Type__args "A" {};
              in expect.noLambdasEq
                expected
                {
                  __isTypeSet = true;
                  __level = U.opts.level;
                  __Super = null;  # TODO: Cheat due to named thunk comparison
                  ctor = U.Ctors.CtorDefault;
                  fields = expected.fields;
                  methods = expected.methods; # TODO: Cheat due to named thunk comparison
                  staticMethods = expected.staticMethods; # TODO: Cheat due to named thunk comparison
                  name = "A";
                  tvars = SolosOf Type [];
                  tvarBindings = {};
                  rebuild = null;
                };
          };
        };

      __separatedTests = {
        tmpTests =
          testInUniverses {
            inherit (Types.Universe)
              U_0
              U_1
              #U_2
              ;
          } (U: smokeTests U
            // shimTests U
            // typeFunctionalityTests U
            // instantiationTests U
            // classTests U
            // peripheralTests U
            // bootstrapTests U
            // inheritanceTests U
            // untypedTests U
            // builtinTests U
            // TypelibTests U
            );

        quasiverseInstantiation =
          testInUniverses { inherit (Types.Universe) U_0; } (U: quasiverseInstantiationTests U);

        smoke =
          #solo
            (testInUniverses {
              inherit (Types.Universe)
                U_0
                U_1
                U_2
                U_3
                ;
            } smokeTests);

        shim =
          #solo
            (testInUniverses {
              inherit (Types.Universe)
                U_0
                ;
            } shimTests);

        peripheral =
          #solo
            (testInUniverses {
              inherit (Types.Universe)
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
              inherit (Types.Universe)
                U_0
                U_1
                U_2
                #U_3
                ;
            } TypelibTests);

        typeFunctionality =
          #solo
            (testInUniverses {
              inherit (Types.Universe)
                U_0
                U_1
                U_2
                ;
            } typeFunctionalityTests);

        class =
          #solo
            (testInUniverses {
              inherit (Types.Universe)
                U_1
                ;
            } classTests);

        inheritance =
          #solo
            (testInUniverses {
              inherit (Types.Universe)
                U_0
                U_1
                U_2
                # U_3
                ;
            } inheritanceTests);

        instantiation =
          #solo
            (testInUniverses {
              inherit (Types.Universe)
                U_0
                U_1
                U_2
                #U_3
                ;
            } instantiationTests);

        builtin =
          #solo
            (testInUniverses {
              inherit (Types.Universe)
                U_0
                U_1
                U_2
                #U_3
                ;
            } builtinTests);

        cast =
          #solo
            (testInUniverses {
              inherit (Types.Universe)
                U_0
                U_1
                U_2
                #U_3
                ;
            } castTests);

        untyped =
          #solo
            (testInUniverses {
              inherit (Types.Universe)
                U_0
                U_1
                U_2
                #U_3
                ;
            } untypedTests);

        typeChecking =
          #solo
            (testInUniverses {
              inherit (Types.Universe)
                U_1  # U_1+ due to reliance upon subtype
                U_2
                ;
            } typeCheckingTests);
      };

    # <nix>typelib._tests.run</nix>
    # <nix>functions._tests.run</nix>
    # <nix>log._tests.run</nix>
    in suite rec {

      all = 
        let onlyU_0 = { inherit (Types.Universe) U_0; };
            fromU_0 = { inherit (Types.Universe) U_0 U_1; };
            fromU_1 = removeAttrs fromU_0 [ "U_0" ];
        in mergeAttrsList [
          (testInUniverses
            { inherit (Types.Universe) U_0; }
            (U: {
              shim = shimTests U;
            }))
          (testInUniverses
            { inherit (Types.Universe) U_0 U_1; }
            (U: {
          #    peripheral = peripheralTests U;
          #    smoke = smokeTests U;
          #    Typelib = TypelibTests U;
          #    typeFunctionality = typeFunctionalityTests U;
          #    inheritance = inheritanceTests U;
          #    instantiation = instantiationTests U;
              builtin = builtinTests U;
              #cast = castTests U;
          #    untyped = untypedTests U;
            }))
          (testInUniverses
            { inherit (Types.Universe) U_1; }
            (U: {
              # bootstrap = bootstrapTests U;
              #typeChecking = typeCheckingTests U;
          #    class = classTests U;
            }))
        ];

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

(setq nixlike-nix-variant 'nix)
(setq nixlike-nix-variant 'tvix)

(setq nixlike-kill-repl-before-eval nil)
(setq nixlike-kill-repl-before-eval t)

(setq nixlike-mode 'shell)
(setq nixlike-mode 'repl)

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
Types.Universe.U_0.Any
</nix>

<nix>
with Types.Universe.U_2;
with call 1 "no" ___;
return ((
  (TypeThunk Bool) {}
))
</nix>

<nix>
with Types.Universe.U_0;
Type
#let A = Type "A" { a = Int; }; in
#A 123
</nix>

<nix>
with Types.Universe.U_0;
rec {
  a = (Fields [{a = Int;}]).concat [{b = Default Int 666;}];
  b = (Fields [{a = Int;}]);
  c = Fields {};
  d = Type;
  e = a.indexed {};
  f = (SolosOf Int) { a = 1; b = 2; };
  g = f.indexed {};
  h = a.solos;
  i = a.instanceFields {};
  j = (Int.getFields {});
  k = (Field Int) "a";
  l = (lambda a.fieldSpecs) {};
  m = parseFieldSpec Int;
  n = Int;
  o = (String "wat").__value.value;
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
collective-lib._tests.run
</nix>

<nix>
collective-lib._testsUntyped.run
</nix>
<nix>
collective-lib._testsUntyped.run
</nix>

<nix>
with Types.Universe.U_1;
(Literal 123).getLiteral {}
</nix>

<nix>
Types.Universe.U_2
</nix>

<nix>
let
  script-utils = import ../../../pkgs/script-utils { inherit pkgs lib collective-lib; };
in
  script-utils.log-utils._tests.run
</nix>

<nix>
Union 1
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

<nix>
let fs = Types.Universe.U_0.Fields [{a = Int;}];
in fs.solos
</nix>
<nix>
dispatchlib._tests.run
</nix

<nix>
with Types.Universe.U_0;
cast Nil null
</nix>

<nix>
typelib._tests.run
#typelib._tests.debug
</nix>

<nix>
log._tests.run
</nix>
<nix>
functions._tests.run
</nix>

<nix>
let U = Types.Universe.U_0; in
#with U;
#mkSafeUnboundTypeShim "wat" {}).fields
#((U.Fields [{b = "int";}]).__Type{}).fields.allFields
#U.Lambda (a: 123) {}
(U.Type.__Type {}).getName {}
</nix>

<nix>
Types.Universe.U_1.opts.name
</nix>

<nix>
Path.new ./.
</nix>

<nix>
collective-lib._testsUntyped.run
</nix>

<nix>
typelib._tests.run
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
  #   with collective-lib.typed; (isNull Nil && isLazyAttrs (LazyAttrs {})) == true
  #
  # Isolated separately from collective-lib so that it can be used as a standalone type-system
  # without the rest of the collective-lib library.
  typelib

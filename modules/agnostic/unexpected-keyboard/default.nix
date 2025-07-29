{ testableModule, lib, collective-lib, ... }: 

testableModule (

{ config, lib, typed, ... }: 

with typed;

let
  # Library for building layouts exposed as services.unexpected-keyboard.lib.
  mkLib = cfg: rec {
    defaultKeyboards = import ./keyboards.nix {
      inherit typed; uklib = cfg.lib;
    };

    getLayout = name: cfg.layouts.${name};

    getRows = keyboard: keyboard.rows;
    numRows = keyboard: length keyboard.rows;
    getRow = rowI: keyboard: elemAt keyboard.rows rowI;
    updateRows = f: keyboard: keyboard // {rows = f keyboard.rows;};
    updateRowKeys = f: row: row // {keys = f row.keys;};
    insertRow = rowI: row: updateRows (insertAt rowI row);
    deleteRow = rowI: updateRows (deleteAt rowI);

    getKey = rowI: colI: keyboard: let row = getRow rowI keyboard; in elemAt row.keys colI;
    setKey = rowI: colI: key_: # Supports singleton lists to allow K ... K format.
      let key = if isList key_ then head key_ else key_;
      in updateRows (updateAt rowI (updateRowKeys (setAt colI key)));
    updateKey = rowI: colI: f: updateRows (updateAt rowI (updateRowKeys (updateAt colI f)));
    insertKey = rowI: colI: key_:
      let key = if isList key_ then head key_ else key_;
      in updateRows (updateAt rowI (updateRowKeys (insertAt colI key)));
    deleteKey = rowI: colI: updateRows (updateAt rowI (updateRowKeys (deleteAt colI)));

    swapKeys = rowI: colI: rowI': colI': keyboard:
      let key = getKey rowI colI keyboard;
          key' = getKey rowI' colI' keyboard;
      in compose (setKey rowI colI key') (setKey rowI' colI' key) keyboard;

    setCardinal = {
      c = m: key: key // {c = m;};
      n = m: key: key // {n = m;};
      ne = m: key: key // {ne = m;};
      e = m: key: key // {e = m;};
      se = m: key: key // {se = m;};
      s = m: key: key // {s = m;};
      sw = m: key: key // {sw = m;};
      w = m: key: key // {w = m;};
      nw = m: key: key // {nw = m;};
      anticircle = m: key: key // {anticircle = m;};
    };

    updateCardinal = {
      c = f: key: key // {c = f key.c;};
      n = f: key: key // {n = f key.n;};
      ne = f: key: key // {ne = f key.ne;};
      e = f: key: key // {e = f key.e;};
      se = f: key: key // {se = f key.se;};
      s = f: key: key // {s = f key.s;};
      sw = f: key: key // {sw = f key.sw;};
      w = f: key: key // {w = f key.w;};
      nw = f: key: key // {nw = f key.nw;};
      anticircle = f: key: key // {anticircle = f key.anticircle;};
    };

    clearCardinal = {
      c = key: key // {c = null;};
      n = key: key // {n = null;};
      ne = key: key // {ne = null;};
      e = key: key // {e = null;};
      se = key: key // {se = null;};
      s = key: key // {s = null;};
      sw = key: key // {sw = null;};
      w = key: key // {w = null;};
      nw = key: key // {nw = null;};
      anticircle = key: key // {anticircle = null;};
    };

    # A col can be constructed like a row, i.e. K c.tab _ c.ctrl _ c.shift . c.meta K
    insertCol = colI: colKeys: composeMany (imap0 (rowI: insertKey rowI colI) colKeys);

    # (rowType -> rowType) -> keyboardType -> keyboardType
    mapRows = f: k: k // {rows = map f k.rows;};

    # (int -> rowType -> rowType) -> keyboardType -> keyboardType
    imapRows = f: k: k // {rows = imap0 f k.rows;};

    # (keyType -> keyType) -> rowType -> rowType
    mapRowKeys = f: r: r // {keys = map f r.keys;};

    # (int -> keyType -> keyType) -> rowType -> rowType;
    imapRowKeys = f: r: r // {keys = imap0 f r.keys;};

    # f :: keyType -> keyType
    mapKeys = f: mapRows (mapRowKeys f);

    # (int -> int -> keyType -> keyType) -> keyboardType -> keyboardType
    imapKeys = f: imapRows (rowI: imapRowKeys (colI: f rowI colI));

    modify = prop: f: x: x // { ${prop} = f x.${prop}; };
    modifyKeys = prop: f: mapKeys (modify prop f);
    imodifyKeys = prop: f: imapKeys (rowI: colI: modify prop (f rowI colI));
    modifyRows = prop: f: mapRows (modify prop f);

    addShift = s: modify "shift" (s': s + s');
    addWidth = w: modify "width" (w': w + w');

    _4dp = x: trunc 4 x;

    scaleWidth = c: composeMany [
      (modifyKeys "width" (w: _4dp (w * c)))
      (modifyKeys "shift" (s: _4dp (s * c)))
    ];
    scaleHeight = c: modifyRows "height" (h: _4dp (h * c));
    scaleGap = w: gap: scaleWidth ((w - gap) / w);
    scaleGap_ = scaleGap 10.0;

    # Scale a keyboard to fit left-to-right according to its largest row.
    fitWidth = k: scaleWidth (10.0 / (getMaxRowWidth k)) k;

    # Shift all rows to the right by the given amount.
    shiftRight = shift:
      imapKeys (_: colI: if colI == 0 then addShift shift else id);

    # Get the unnormalized width of a row (widths plus shifts)
    getRowWidth = row: sum (map (key: key.width + key.shift) row.keys);

    # Get the maximum unnormalized width of a row in a keyboard.
    getMaxRowWidth = keyboard: maximum (map getRowWidth keyboard.rows);

    appendKey = rowI: key: keyboard:
      let row = getRow rowI keyboard;
      in insertKey rowI (length row.keys) key keyboard;

    Variants = with codes; {
      # Left-aligned one-handed layout.
      lefty = gap: precompose [
        fitWidth
        (scaleGap_ gap)
        # Hack required since UK scales the longest row to 10.0, so we need to insert a
        # dummy key of gap size at the end of each row.
        (precompose [
          numRows
          (genList (flip appendKey (K 0 gap " " c.removed K)))
          compose
        ])
      ];

      # Right-aligned one-handed layout
      righty = gap: precompose [
        fitWidth
        (scaleGap_ gap)
        (shiftRight gap)
      ];
    };

    # Default variants to apply if includeDefaultVariants is set.
    defaultVariants = {
      lefty = Variants.lefty 2.0;
      righty = Variants.righty 2.0;
    };

    defaultKeyValueAliases = with codes; {
      "€" = "€";
      euro = "€";
      gbp = "£";
      dollar = "$";
      caps = allCodes.capslock;
      pgup = allCodes.page_up;
      pgdn = allCodes.page_down;
      bsp = allCodes.backspace;
      del = allCodes.delete;
      ins = allCodes.insert;
      spc = allCodes.space;
      pastePlain = allCodes.pasteAsPlainText;
      abc = allCodes.switch_text;
      _123 = allCodes.switch_numeric;
      emoji = allCodes.switch_emoji;
      emojiBack = allCodes.switch_back_emoji;
      math = allCodes.switch_greekmath;
      sub = allCodes.subscript;
      sup = allCodes.superscript;
      fwd = allCodes.switch_forward;
      bwd = allCodes.switch_backward;
      cur_u = allCodes.cursor_up;
      cur_d = allCodes.cursor_down;
      cur_l = allCodes.cursor_left;
      cur_r = allCodes.cursor_right;
    };

    # The key codes used to build layouts.
    # Shorthand keyed by key-generation or global-generation type.
    # Can be overridden to change default behaviours or make new keys or aliases available.

    # By default the following in-config shorthand is available, using:
    # with config.services.unexpected-keyboard.codes; ...

    # # Combine multiple shorthand codes with __:
    # __ [ c.m sw."." ] -> <key c="m" sw="." />

    # or using the variadic 'K' function:

    # K c.m sw."." K -> <key c="m" sw="." />

    # # Apply settings to keys:
    # wd 1.5 (__ [ c.shift ne.caps ]) -> <key width="1.5" c="shift" ne="caps" />)
    # o 0.5 (wd 1.5 (__ [ c.a w.ctrl])) -> <key width="1.5" shift="0.5" c="a" w="ctrl" />)

    # # Key values
    # ([gesture_] [mkCodeFn]).[keyValue]
    # # Literal keypress/sequence:
    # [gesture].[keyValue]
    # or [gesture]_.[keyValue] k -> <key [gesture]="[keyValue]" />
    # e.g. c.a -> <key c="a" />
    #      c."\"" -> <key c="&quot;" />

    # # Keycode:
    # c_ kc [eventCode] -> <key e="[eventCode]" />
    # e.g. e
    codes =
      let
        categories = rec {
          ascii =
            let
              to4CharHexString = i:
                let h = toHexString i;
                    pad0 = 4 - stringLength h;
                in "${lib.strings.replicate pad0 "0"}${h}";
              toAsciiChar = i: 
                builtins.fromJSON ''"\u${to4CharHexString (i + 1)}"'';
            in 
              selfAttrs (genList toAsciiChar 254);

          mod = selfAttrs [ "ctrl" "shift" "alt" "meta" "fn" "compose" "capslock" ];
          function = selfAttrs ["esc" "enter" "up" "right" "down" "left"
                                "page_up" "page_down" "home" "end" "backspace"
                                "delete" "insert" "scroll_lock" "tab" "copy"
                                "paste" "cut" "selectAll" "pasteAsPlainText"
                                "undo" "redo"];
          edit = selfAttrs [ "cursor_left" "cursor_right" "cursor_up" "cursor_down"
                            "delete_word" "forward_delete_word" ];

          fKeys = selfAttrs (genList (n: "f${toString n}") 13);
          spacing = selfAttrs [ "space" "nbsp" "nnbsp" "zwj" "zwnj" ];
          math = selfAttrs [ "superscript" "subscript" ];

          behaviour = selfAttrs [
            "config" "switch_text" "switch_numeric" "switch_emoji" "switch_back_emoji"
            "switch_forward" "switch_backward" "switch_greekmath" "switch_clipboard"
            "change_method" "change_method_prev" "action" "voice_typing"
            "voice_typing_chooser" "shareText" ];

          unused = selfAttrs [ "replaceText" "textAssist" "autofill" "removed" ];
        };

        # Print an individual gesture mapping e.g. printMapping { c = { k = "!" } } == "c -> !"
        # Used in printing pre-built keys that may contain duplicate gestures.
        printMapping = mapping:
          let lines = mapAttrsToList (gesture: kv: "${gesture} -> ${xml.from.keyValue kv}") mapping;
          in assert (length lines == 1); head lines;
        printMappings = ms: joinLines (map printMapping ms);

      in rec {
        # Shorthand for:
        # - Alpha chars as _.a ... _.z and _.A ... _.Z
        # - Other ASCII chars as e.g. _."0" .. _."9", _."!", etc.
        # - Unknown Keyboard special key names as _.keyname e.g. _.ctrl, _.capslock, _.switch_numeric
        # - Registered key value aliases as _.aliasname e.g. _.gbp, _.caps
        allCodes = mergeAttrsList (attrValues categories);
        _ = {
          # A marker enabling detection of passing the _ into variadic functions.
          __shorthandMarker = true; #
        } // allCodes // cfg.keyValueAliases;

        # Is the given value '_'
        isShorthandMarker = x:
          isAttrs x && (x.__shorthandMarker or false);

        # Shorthand for accessing registered whole-key aliases as e.g. Key.myCtrl
        Key = cfg.keyAliases;

        # Shorthand for constructing a key variadically.
        #
        # Will consume arguments of type keyValue until a callable function argument is consumed.
        #
        # Any function argument will cause termination, but the callable K itself can be used
        # to visually book-end the block.
        #
        # If the '_' attr set is encountered, then the currently built key is emitted and
        # the state reset, such that multiple keys can be built in a single call.
        #
        # If only a single key occurs between K ... K markers it will be returned as a keyValue,
        # otherwise a list is returned. This way K ... K keys can sit alongside other definition
        # methods in a list, or a single K ... K can be used to define a whole row.
        #
        # Any non-attribute set parameters appearing after the K will be treated as parameters
        # to apply to the next occuring key value, after which they will be reset. These are
        # interpreted based on type and order:
        #
        # Strings:
        # - A first string literal is interpreted as a legend
        # - Subsequent strings after the first will throw an exception
        #
        # Ints/Floats:
        # - A first numeric value is interpreted as the key width
        # - A second numeric value is interpreted as the key offset/shift
        # - Subsequent numeric values after the second will throw an exception
        #
        # Bools:
        # - A first bool is interpreted as the "loc" setting
        # - Subsequent bools after the first will throw an exception
        # - The 'LOC' alias for 'true' can also be used.
        #
        # For example:
        #
        # keys =
        #    K
        #      1 0.5
        #      "▤" nw.fn     "⎋" LOC ne.esc
        #             "✲" c.ctrl
        #      "λ" sw.math   "ℕ" se._123
        #    _
        #      <key>
        #    _
        #      <key>
        #    ...
        #    K;
        # ]
        #
        # is equivalent to:
        #
        # keys = [
        #   {
        #     width = 1;
        #     shift = 0.5;
        #     nw = {
        #       k = "fn";
        #       legend = "▤";
        #     };
        #     ne = {
        #       k = "escape";
        #       loc = true;
        #       legend = "⎋";
        #     };
        #     c = {
        #       k = "ctrl";
        #       legend = "✲";
        #     };
        #     sw = {
        #       k = "switch_greekmath"
        #       legend = "λ";
        #     };
        #     se = {
        #       k = "switch_numeric"
        #       legend = "ℕ";
        #     };
        #   }
        #   ...
        # ]
        K = Variadic.mk (rec {
          initialState = {
            keys = [];
            mappings = [];

            keyValueAttrs = {
              legend = null;
              loc = false;
              hasSet = {
                legend = false;
                loc = false;
              };
            };

            keyAttrs = {
              width = 1.0;
              shift = 0.0;
              hasSet = {
                width = false;
                shift = false;
              };
            };
          };

          handle = state: x:
            let
              keys_ =
                if isFunction x || isShorthandMarker x
                then
                  # Emit the current key if we hit the shorthand marker '_'
                  let mappedGestures = map attrNames state.mappings;
                  in
                    if !(allUnique mappedGestures)
                    then throw "Duplicate gesture mapping in:\n${printMappings state.mappings}"
                    else
                      (state.keys ++ [
                        ((removeAttrs state.keyAttrs [ "hasSet" ])
                          // (mergeAttrsList state.mappings))])
                else
                  # Otherwise continue
                  state.keys;

              # Consume any partial key value attributes and add them to the mappings if we hit a key value.
              mappings_ =
                if isFunction x || isShorthandMarker x then initialState.mappings
                else {
                  set = state.mappings ++ [
                    (mapAttrs (_: key: key // (removeAttrs state.keyValueAttrs [ "hasSet" ])) x)
                  ];
                }.${typeOf x} or state.mappings;

              # Accrue partially built key attributes to apply on seeing _ or on termination.
              keyAttrs_ =
                if isFunction x || isShorthandMarker x then initialState.keyAttrs
                else
                  let intFloat =
                    if (state.keyAttrs.hasSet.width && state.keyAttrs.hasSet.shift)
                    then throw ''Invalid K syntax: >2 numeric values (existing = { width = ${toJSON state.keyAttrs.width}; shift = ${toJSON state.keyAttrs.shift}; }, new = ${toJSON x})''
                    else if (state.keyAttrs.hasSet.width) then state.keyAttrs // { shift = x; hasSet = statete.keyAttrs.hasSet // { shift = true; }; }
                    else state.keyAttrs // { width = x; hasSet = state.keyAttrs.hasSet // { width = true; }; };
                  in {
                    # Interpret numbers first as width then as shift.
                    int = intFloat;
                    float = intFloat;
                  }.${typeOf x} or state.keyAttrs;

              # Accrue partially built key value attributes until we hit a key value, then reset
              keyValueAttrs_ =
                if isFunction x || isShorthandMarker x then initialState.keyValueAttrs
                else {
                  set = initialState.keyValueAttrs; # Reset the partial state when we move on from an attribute key.
                  string =  # String as legend for the next key
                    if state.keyValueAttrs.hasSet.legend
                    then throw ''Invalid K syntax: >1 legend provided (existing = { l = "${state.keyValueAttrs.legend}"; }, new = "${x}")''
                    else state.keyValueAttrs // { legend = x; hasSet = state.keyValueAttrs.hasSet // { legend = true; }; };
                  bool =   # Bool as loc for the next key
                    if state.keyValueAttrs.hasSet.loc
                    then throw ''Invalid K syntax: >1 loc provided (existing = { loc = "${toJSON state.keyValueAttrs.loc}"; }, new = "${toJSON x}")''
                    else state.keyValueAttrs // { loc = x; hasSet = state.keyValueAttrs.hasSet // { loc = true; }; };
                }.${typeOf x} or state.keyValueAttrs;

              # Build the updated state.
              state_ = {
                keys = keys_;
                mappings = mappings_;
                keyValueAttrs = keyValueAttrs_;
                keyAttrs = keyAttrs_;
              };

            in state_;

          isTerminal = _: isFunction;

          terminate = state: _:
            if length state.mappings != 0 then throw "K: Unconsumed mappings:\n${printMappings state.mappings}"
            else if state.keyValueAttrs.hasSet.legend then throw "K: Unconsumed legend ${state.keyValueAttrs.legend}"
            else if state.keyValueAttrs.hasSet.loc then throw "K: Unconsumed loc"
            else if length state.keys == 1 then head state.keys else state.keys;
        });

        # Alias for setting LOC on a variadically set key value
        LOC = true;

        # Shorthand for merging multiple mappings into a single key.
        # Useful when not using the K variadic helper for explicitly merging
        # multiple key value mappings:
        #
        # For example, reproducing the stock compose/arrow key:
        #
        # wd 1.1 (__ [
        #            n.up
        #     w.left c.compose e.right
        #            s.down
        # ])
        __ = mergeAttrsList;

        # Key-value shorthand
        kv = {
          # Literal keypresses (can be multi-char, cannot contain colons):
          k = keypressStr: { k = keypressStr; };
          # KeyEvent input:
          e = keyCode: { e = keyCode; };
          # Literal string input with legend (can be multi-char, can contain colons)
          s = legend: text: { s = text; l = legend; };
          # Macro input with legend.
          m = legend: keyList: { m = keyList; l = legend; };
        };

        # Shorthand for placing known keys/aliases at mapping locations.
        #
        # For keys in '_':
        # e.g. c.a c.b c."?" c.config c."&"
        c = mapAttrs (_: code: c_ (kv.k code)) _;
        n = mapAttrs (_: code: n_ (kv.k code)) _;
        ne = mapAttrs (_: code: ne_ (kv.k code)) _;
        e = mapAttrs (_: code: e_ (kv.k code)) _;
        se = mapAttrs (_: code: se_ (kv.k code)) _;
        s = mapAttrs (_: code: s_ (kv.k code)) _;
        sw = mapAttrs (_: code: sw_ (kv.k code)) _;
        w = mapAttrs (_: code: w_ (kv.k code)) _;
        nw = mapAttrs (_: code: nw_ (kv.k code)) _;
        ac = mapAttrs (_: code: ac_ (kv.k code)) _;

        # Shorthand for placing arbitrary keyvalues at mapping locations.
        #
        # For entering literals not represented in the raw known keys or aliases
        # or other key value types:
        # e.g. (c_ (kv.k "arbitrary")) (ne_ (kv.m [c.ctrl c.a]))
        c_ = keyValue: { c = keyValue; };
        n_ = keyValue: { n = keyValue; };
        ne_ = keyValue: { ne = keyValue; };
        e_ = keyValue: { e = keyValue; };
        se_ = keyValue: { se = keyValue; };
        s_ = keyValue: { s = keyValue; };
        sw_ = keyValue: { sw = keyValue; };
        w_ = keyValue: { w = keyValue; };
        nw_ = keyValue: { nw = keyValue; };
        ac_ = keyValue: { anticircle = keyValue; };

        # Shorthand for legend setting generation
        # Applied to a key value, not a key
        # e.g. __ [(l "fmap" (ne_ (kv.k "<$>"))) (l "bind" (se_ (kv.k ">>=")))]
        legend = l: mapAttrs (_: v: v // {legend = l;});
        l = legend;

        # Shorthand for setting loc parameter
        # Applied to a key value, not a key
        # e.g. __ [ c.q (loc sw.esc) ]
        loc = mapAttrs (_: v: v // {loc = true;});

        # Shorthand for key width setting
        # Also aliased to wd
        # Applied to a key
        # e.g. wd 1.5 (__ [ c.shift ne.caps ])
        width = value: key: key // {width = value;};
        wd = width;

        # Shorthand for key shift setting
        # Also aliased to offset/o to avoid confusion with the shift key
        # Applied to a key
        # e.g. o 0.5 (__ [ c.a ])
        shift = value: key: key // {shift = value;};
        offset = shift;
        o = shift;

        # Shorthand for key indication setting
        # Also aliased to i
        # Applied to a key
        # e.g. i ";)" (__ [ (c_ (kv.m [_.ctrl _.shift _.n])) ])
        indication = value: key: key // {indication = value;};
        i = indication;
      };
    };

  # Directories and paths under use in /etc
  rootDir = "unexpected_keyboard";
  layoutsDir = "${rootDir}/layouts";
  layoutSafeName = keyboard: 
    replaceStrings [" " "\t" "\n" "\r"] ["_" "_" "_" "_"] keyboard.name;
  layoutPath = keyboard: "${layoutsDir}/${layoutSafeName keyboard}.xml";
  etcDir = path: "/etc/${path}";

  # XML conversion utilities.
  xml = rec {
    # Escape a string for insertion.
    escape = replaceStrings ["&" "<" ">" "\""] ["&amp;" "&lt;" "&gt;" "&quot;"];

    # Convert the attribute with the given name in attribute set x to an XML
    # key-value setting, escaping if necessary.
    # Partially polymorphic in that if x.${name} is an attribute set, it is
    # interpreted as a key value setting, and converted according to its
    # k/s/e/m type; otherwise the value is converted to an XML-compliant string.
    attr = x: name:
      let
        rawValue = x.${name};
        f = {
          set = xml.from.keyValue;
          int = builtins.toJSON;
          float = builtins.toJSON;
          bool = b: if b then "true" else "false";
          string = id;
        }.${typeOf rawValue};
        value = xml.escape (f rawValue);
      in optionalString (x ? ${name} && rawValue != null) ''${name}="${value}"'';

    # Convert all of the attributes in x to a list of XML key-value settings.
    attrs = x: map (xml.attr x) (attrNames x);

    # Conversions from attribute sets to lists of 'name="value"' strings
    # for insertion into the XML layout.
    from = {
      # Mask the 'rows', 'modmap' in the XML tag conversion.
      # Convert the remainder from camel to snake case.
      keyboard = keyboard_: xml.attrs {
        inherit (keyboard_) name script;
        numpad_script = keyboard_.numpadScript;
        bottom_row = keyboard_.bottomRow;
        embedded_number_row = keyboard_.embeddedNumberRow;
        locale_extra_keys = keyboard_.embeddedNumberRow;
      };

      # Mask the 'keys' in the XML tag conversion.
      row = row_: xml.attrs {
        inherit (row_) height;
      };

      # A key type contains only valid XML attributes.
      key = xml.attrs;

      # Convert a key value attribute set into an Unexpected Keyboard compatible key string.
      # { legend = "MyA"; k = "a"; } -> "MyA:a"
      # { k = "&"; } -> "&"
      # { legend = "ss"; s = "some:string"; } -> "ss:'some:string'"
      # { e = 100; } -> "keyevent:100"
      # { legend = "MyMacro"; m = [ { k = "ctrl"; } { e = 100; } {s = "xxx";} ]; } -> "MyMacro:ctrl,keyevent:100,'xxx'"
      # Any HTML escaping required takes places in xml.attr before insertion into the XML.
      keyValue = kv:
        let
          prefix = joinOptionals [
            (optionalString (kv.loc or false) "loc ")
            (optionalString ((kv.legend or null) != null) "${kv.legend}:")
          ];
          value =
            if (kv ? k) then kv.k
            else if (kv ? e) then "keyevent:${kv.e}"
            else if (kv ? s) then "'${replaceStrings ["'"] ["\\'"] kv.s}'"
            else if (kv ? m) then "${joinSep "," (map mkKeyValue kv.m)}"
            else throw "Unexpected mapping type in: ${joinWords (attrNames kv)}";
        in "${prefix}${value}";
    };

    # If a type defines an XML override, wrap a generating function to use it
    # instead of generated new XML.
    respectOverrideXML = f: arg:
      if arg.overrideXML == null
      then f arg
      else arg.overrideXML;

    # Construct the <key> element for the given key attributes.
    mkKey = respectOverrideXML (key:
      "<key ${joinOptionalWords (xml.from.key key)} />");

    # Construct the <row> element for the given row attributes.
    mkRow = respectOverrideXML (row: indent.block ''
      <row ${joinWords (xml.from.row row)}>
        ${indent.here (joinLines (map mkKey row.keys))}
      </row>
    '');

    # Construct the <modmap> element for the given modmap attributes.
    mkModmap = respectOverrideXML (modmap:
      let
        mkOneModmap = mod: maps:
          optionalString
            (maps != null)
            (indent.lines
              (mapAttrsToList
                (before: after: ''<${mod} a="${before}" b="${after}" />'')
                maps));
        modmapLines = nonEmpties (mapAttrsToList mkOneModmap modmap);
      in
        indent.block (optionalString (modmapLines != []) ''
          <modmap>
            ${indent.here (indent.lines modmapLines)}
          </modmap>
        ''));

    # Construct the <keyboard> element for the given keyboard attributes.
    mkKeyboard = respectOverrideXML (keyboard:
      indent.block ''
        <keyboard
          ${indent.here (indent.lines (xml.from.keyboard keyboard))}
          >
          ${indent.here (mkModmap keyboard.modmap)}
          ${indent.here (indent.blocks (map mkRow (keyboard.rows)))}

        </keyboard>
      '');

    # Construct the full XML layout for the given keyboard attributes.
    mkLayoutXML = keyboard: indent.block ''
      <?xml version="1.0" encoding="utf-8"?>

      ${mkKeyboard keyboard}
    '';
  };

  # Options common to each gesture mapping location.
  mkMappingOptions = arg: arg.options // {
    legend = mkOption {
      type = if arg.legendRequired then types.str else types.nullOr types.str;
      description = ''
        A legend to display at the gesture location (colon not permitted).
      '';
    } // (optionalAttrs (!arg.legendRequired) { default = null; });

    loc = mkOption {
      type = types.bool;
      default = false;
      description = ''
        If true, prefix key value with 'loc ', indicating it should be enabled by
        the 'Add keys to the keyboard' settings.
      '';
    };
  };

  # Option to override the XML for a submodule representing a tag.
  mkOverrideXMLOption = name: mkOption {
    type = types.nullOr types.str;
    default = null;
    description = ''
      If non-null, ignores the other attributes and inserts the XML here verbatim
      in place of the <${name}> tag that would otherwise be generated.

      Use this if you:
        - are generating XML elsewhere
        - want to use part or all of a prebuilt layout without translating it to Nix configuration:
          - User layouts: https://github.com/Julow/Unexpected-Keyboard-layouts.
          - Official layouts: https://github.com/Julow/Unexpected-Keyboard/blob/master/srcs/layouts/
        - want to use some new Unexpected Keyboard feature or setting that was introduced since
          this module was written.
    '';
  };

  keyLitMappingOptions = {
    k = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        The literal key string or known special value to input when the gesture is triggered.
        Exactly one of k/s/e/m must be set.

        The only escaping required is for Nix itself. Regular XML layout definitions require
        escaping '&' as '&quot;', '<' as '&lt;', etc. This is not required here and
        XML escaping is taken care of during config generation.
        e.g. to input an ampersand: { k = "&"; }
              to input a double-quote: { k = "\""; }

        All documented keys as well as ASCII punctuation are aliased to '_'.
        Includes keys from:
          https://github.com/Julow/Unexpected-Keyboard/blob/master/doc/Possible-key-values.md
        As well as aliases configured in services.unexpected-keyboard.aliases

        e.g.
        with services.unexpected-keyboard.lib.codes;
        _ == {
          a = "a";
          b = "b";
          ...
          _0 = "0"
          _1 = "1"
          ...
          space = "space"
          spc = "space"
          ...
          "!" = "!"
          "\"" = "\""
          "&" = "&"
          ...
          switch_numeric = "switch_numeric"
          _123 = "switch_numeric"
          ...
        };

        These keys are also available in shorthand nested under each of the gestures:
        e.g. c.a == { c = {k = "a";}; }
              ne."$" == { ne = {k = "$";}; }
              c.cur_r == { c = {k = "cursor_right";}; }
              ...

        These shorthands cannot be used to input arbitrary sequences, which do not exist
        as keys on '_'. Instead use { k = "arbitrary"; } directly, though note that this
        cannot include ':' characters.
        Colons can be included using { legend = "My Key"; s = "arbitrary:with:colons"; }.

        Special keys are handled if Unexpected Keyboard recognises the name.
        e.g. If { k = "esc"; } is set, this will input the Escape key.
              To create a mapping that instead inputs the literal string "esc", use:
              { legend = "esc"; s = "esc"; };
      '';
    };
  };

  stringLitMappingOptions = {
    s = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        The literal string to input when the gesture is triggered.
        Requires that a legend is set for the gesture.
      '';
    };
  };

  keyEventMappingOptions = {
    e = mkOption {
      type = types.nullOr types.int;
      default = null;
      description = ''
        The Android KeyEvent code to input when the gesture is triggered.

        e.g. { e = 102; } will input the KEYCODE_BUTTON_L1 key
        (https://developer.android.com/reference/android/view/KeyEvent#KEYCODE_BUTTON_L1)

        A full list of codes is available at:
        https://developer.android.com/reference/android/view/KeyEvent#summary
      '';
    };
  };

  # Ensure mutual exclusivity and that s has a legend always
  withKeyValueCheck = T:
    let check =
          x:
            let mutEx = sum [(x.k or 1) (x.s or 1) (x.e or 1) (x.m or 1)] == 3;
                smLeg = x ? k || x ? e || x ? legend;
            in mutEx && smLeg;
    in types.addCheck T check;

  # Type for a component value in a macro's list of keys.
  # Disallows macro nesting, currently unsupported in Unexpected Keyboard.
  # We can't have a oneOf with multiple submodules so we need to manually
  # disambiguate based on key here.
  macroKeyValueType =
    withKeyValueCheck (types.submodule {
      options = mkMappingOptions {
        legendRequired = false;
        options = mergeAttrsList [
          keyLitMappingOptions
          stringLitMappingOptions
          keyEventMappingOptions
        ];
      };
    });

  macroMappingOptions = {
    m = mkOption {
      type = types.nullOr (types.listOf macroKeyValueType);
      default = null;
      description = ''
        The macro to execute when the gesture is triggered.
        Requires that a legend is set for the gesture.
      '';
    };
  };

  # Type for a single key value on the RHS of a gesture-to-value mapping.
  # We can't have a oneOf with multiple submodules so we need to manually
  # disambiguate based on key here.
  keyValueType = withKeyValueCheck (types.submodule {
    options = mkMappingOptions {
      legendRequired = false;
      options = mergeAttrsList [
        keyLitMappingOptions
        stringLitMappingOptions
        keyEventMappingOptions
        macroMappingOptions
      ];
    };
  });

  # Option type representing the <key> element.
  keyType =
    let
      # Construct an option for setting the mapping of a gesture on the key.
      mkMappingOption = gesture: {
        ${gesture} = mkOption {
          type = types.nullOr keyValueType;
          default = null;
          description = ''
            The key value to map to the '${gesture}' gesture for the key.
            One of:
              - a literal keypress string or special key name
              - a string literal input
              - a KeyEvent keycode input
              - a macro defined as a list of the above three types to be input in sequence
          '';
        };
      };

      # The gestures available for mapping a key.
      gestures = ["c" "n" "ne" "e" "se" "s" "sw" "w" "nw" "anticircle"];

      # Options for setting mappings for each of the gesture types.
      mappingOptions = mergeAttrsList (map mkMappingOption gestures);
    in
      types.submodule {
        options = mappingOptions // {
          overrideXML = mkOverrideXMLOption "key";
          shift = mkOption {
            type = types.oneOf [ types.int types.float ];
            default = 0.0;
            description = "The left offset of the key as a multiple of default key width.";
          };
          width = mkOption {
            type = types.oneOf [ types.int types.float ];
            default = 1.0;
            description = "The width of the key as a multiple of default key width.";
          };
          indication = mkOption {
            type = types.nullOr types.str;
            default = null;
            description = "The indication to display at south of the key.";
          };
        };
      };

  # Option type representing a single <row> element.
  rowType = types.submodule {
    options = {
      overrideXML = mkOverrideXMLOption "row";
      height = mkOption {
        type = types.float;
        default = 1.0;
        description = "The height of the row.";
      };
      keys = mkOption {
        type = types.listOf keyType;
        default = [];
        description = "The keys of the row in the order they appear.";
      };
    };
  };

  # Option type representing the <modmap> element.
  modmapType =
    let mkModmapOption = modifier:
      mkOption {
        type = types.attrsOf types.str;
        default = {};
        description = ''
          The modmap entries to set for the ${modifier} key.
          The key is the name of the key to modify when ${modifier} is pressed,
          and the value is the value to insert.
          e.g. { "i" = "İ"; } will make ${modifier}+i act as İ.
        '';
      };
    in types.submodule {
      options = {
        overrideXML = mkOverrideXMLOption "modmap";
        shift = mkModmapOption "shift";
        ctrl = mkModmapOption "ctrl";
        alt = mkModmapOption "alt";
      };
    };

  # Option type storing a keyboard along with its compiled XML.
  layoutType = types.submodule {
    options = {
      keyboard = mkOption {
        type = keyboardType;
        description = "The keyboard layout that generated this layout file";
      };
      xmlSource = mkOption {
        type = types.str;
        description = "The compiled XML layout.";
      };
      relPath = mkOption {
        type = types.str;
        description = "The path of the compiled XML layout.";
      };
      mkFile = mkOption {
        type = types.attrs;
        description = "Singleton set to be merged into config adding the layout to environment.etc.";
      };
    };
  };

  # Option type representing the <keyboard> element.
  keyboardType = types.submodule {
    options = {
      overrideXML = mkOverrideXMLOption "keyboard";
      name = mkOption {
        type = types.str;
        description = "The name of the keyboard layout to generate.";
      };
      includeDefaultVariants = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether to include the default variants.
        '';
      };
      variants = mkOption {
        type = types.attrs;
        default = {};
        description = ''
          A set of variant names to layout variants to generate.
        '';
      };
      script = mkOption {
        type = types.str;
        default = "latin";
        description = "The script of the keyboard.";
      };
      numpadScript = mkOption {
        type = types.str;
        default = "latin";
        description = "The script of the numpad.";
      };
      bottomRow = mkOption {
        type = types.bool;
        default = true;
        description = "Whether or not to include a default bottom row.";
      };
      embeddedNumberRow = mkOption {
        type = types.bool;
        default = false;
        description = "Whether or not the keyboard has a number row (stops another being added if enabled).";
      };
      localeExtraKeys = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to add locale-specific extra keys from:
          https://github.com/Julow/Unexpected-Keyboard/blob/master/res/xml/method.xml
        '';
      };
      rows = mkOption {
        type = types.listOf rowType;
        description = ''
          The keyboard layout as a list of rows of keys.
          Each key has settings common to the whole key, plus location-specific mappings.
        '';
      };
      modmap = mkOption {
        type = modmapType;
        default = {
          shift = {};
          ctrl = {};
          alt = {};
        };
        description = ''
          A map determining the behaviour of the modifiers.
        '';
      };
    };
  };

in {

  options.services.unexpected-keyboard = {
    lib = mkOption {
      type = types.attrs;
      description = ''The Unexpected Keyboard library exposed on the service config.'';
    };
    enable = mkEnable "Whether to enable Unexpected Keyboard configuration.";
    keyboards = mkOption {
      type = types.listOf keyboardType;
      default = [];
      description = ''
        The Unexpected Keyboard layouts to generate.
        Place your layout definitions here. See ./keyboards.nix for examples.
      '';
    };
    includeDefaultKeyboards = mkOption {
      type = types.bool;
      default = true;
      description = ''
        If true, include the default keyboards from ./keyboards.nix.
      '';
    };
    keyboardsWithVariants = mkOption {
      type = types.attrsOf keyboardType;
      description = ''
        The expanded Unexpected Keyboard layouts including variants.
      '';
    };
    layouts = mkOption {
      type = types.attrsOf layoutType;
      description = ''
        The compiled Unexpected Keyboard layouts including variants.
      '';
    };
    includeDefaultKeyValueAliases = mkOption {
      type = types.bool;
      default = true;
      description = ''
        If true, include default key value aliases.
      '';
    };
    keyValueAliases = mkOption {
      type = types.attrsOf types.str;
      default = {};
      description = ''
        Any literal aliases to make available for mapping when building layouts in shorthand.
        Exposed in services.unexpected-keyboard.lib.codes.

        The value can be any valid Unexpected Keyboard string, pre-XML-escaping, and
        by default will be interpreted as a literal keypress sequence or special key, but
        can also include macro, keyevent or string literal indicators, as well as legend and loc
        syntax.

        These are available on all gesture locations and on the raw codemap 'codes._'
        An alias can be created for any individually mappable event, and can then be
        accessed directly as codes._.<aliasname>, or as a gesture mapping as
        c.<aliasname>, ne.<aliasname>, etc.

        For example:

        keyValueAliases = {
          xyz = "XYZ";                          # Literal insertion available as _.xyz

          "₿" = "₿";                            # Literal extra non-alpha available as _."₿"

          _10x = "10X:xxxxxxxxxx";              # Legend support on literals
          or
          _10x = l "10X" (k "xxxxxxxxxx");      # Using same syntax as in 'codes'

          myPrint = "Print:ctrl,p";             # Macro support by manually constructing macro string
          or
          myPrint = m "Print" [_.ctrl _.p];     # Using same syntax as in 'codes'

          atMe = "My@:'mailto:me@test.com';     # Literal string support allowing colons
          or
          atMe = s "My@" "mailto:me@test.com";  # Using same syntax as in 'codes'
        };

        This enables defining a key as:

        __ [ nw.atMe       ne.myPrint
                     c.xyz
             sw.btc        se._10x    ]

        Without the extra literals registered here this would need to be the
        longer inlined version:

        __ [ (nw_ (s "My@" "mailto:me@test.com")) (ne_ (m "Print" [_.ctrl _.p]))
                                  (c_ (k "XYZ"))
             (sw_ (k "₿"))                        (l "10X" (se_ (k "xxxxxxxxxx"))) ]
      '';
    };

    keyAliases = mkOption {
      type = types.attrsOf keyType;
      default = {};
      description = ''
        A set of named keys that can be used across multiple layouts.
        Exposed in services.unexpected-keyboard.lib.codes.

        For example, setting:
        keyAliases = with services.unexpected-keyboard.lib.codes; {
          myCtrl = wd 1.7 (__ [
            (l "▤" nw.fn)                  (l "❖" ne.meta)
                            (l "✲" c.ctrl)
            (l "λ" sw.math)                (l "ℕ" se._123)  ]);
        }

        defines the key for use in layout creation under the '_' shorthand alias:

        ...
        rows = [
          ...
          {
            keys = [
              _.key.myCtrl
              (wd 1.1 (__ [nw.change_method ne.alt c.fn sw.emoji se.config]))
              ...
            ];
          };
        ];
        ...
      '';
    };
  };

  config = 
    let cfg = config.services.unexpected-keyboard;
    in mkIf cfg.enable (mkMerge [
      {
        # Expose the library for building layouts.
        services.unexpected-keyboard.lib = mkLib cfg;
      }

      {
        services.unexpected-keyboard = mkMerge [
          (mkIf cfg.includeDefaultKeyboards {
            keyboards = cfg.lib.defaultKeyboards;
          })

          (mkIf cfg.includeDefaultKeyValueAliases {
            keyValueAliases = cfg.lib.defaultKeyValueAliases;
          })
          
          {
            # Generate and merge with the variants.
            keyboardsWithVariants =
              let
                keyboards = keyByName cfg.keyboards;
                variants =
                  concatMapAttrs (kName: k:
                    concatMapAttrs (vName: transform:
                      let name = "${kName} (${vName})";
                      in {
                        ${name} = transform (k // { inherit name; variants = {}; });
                      })
                    ((optionalAttrs k.includeDefaultVariants cfg.lib.defaultVariants)
                    // k.variants))
                  keyboards;
              in
                keyboards // variants;

            # Write compiled layouts out to the service config.
            layouts = 
              mapAttrs
                (_: keyboard: rec {
                  inherit keyboard;
                  xmlSource = xml.mkLayoutXML keyboard;
                  relPath = layoutPath keyboard;
                  mkFile = { ${relPath}.text = xmlSource; };
                })
                cfg.keyboardsWithVariants;
          }
        ];

        # Write layouts out to /etc
        # Generates a set of { environment.etc = { layout0.text = xmlSource; layout1.text = xmlSource; ... }; }
        agnostic.environment.etc = concatMapAttrs (_: layout: layout.mkFile) cfg.layouts;
      }

    ]);

})

(with collective-lib; with typed; with typed.tests;
let 
  fakeModule =
    {...}: {
      options.agnostic.environment.etc = mkOption { type = types.attrs; default = {}; };
    };
  mkConfigModule = includeDefaultKeyboards: keyboards:
    {...}: { 
      services.unexpected-keyboard = {
        enable = true;
        inherit keyboards includeDefaultKeyboards;
      };
    };
  mkConfig = configModule:
    let evaluated = evalModules {
      specialArgs = { inherit lib typed; testableModule = lib.const; };
      modules = [ fakeModule (getModuleFromTestableModule ./.) configModule ];
    };
    in evaluated.config;
in {
  _tests = with typed.tests; suite {
    empty = 
      let config = mkConfig (mkConfigModule false []);
      in {
        layouts = expect.eq (config.services.unexpected-keyboard.layouts) {};
        etc = expect.eq (config.agnostic.environment.etc) {};
      };
    defaults = 
      let config = mkConfig (mkConfigModule true []);
      in with config.services.unexpected-keyboard.lib; {
        etc.size = expect.eq (size config.agnostic.environment.etc) 16;
        layouts.size = expect.eq (size config.services.unexpected-keyboard.layouts) 16;
        layouts.golden = 
          expect.eq config.services.unexpected-keyboard.layouts."QWERTY (US)".xmlSource
          "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n\n<keyboard\n  bottom_row=\"true\"\n  embedded_number_row=\"false\"\n  locale_extra_keys=\"false\"\n  name=\"QWERTY (US)\"\n  numpad_script=\"latin\"\n  script=\"latin\"\n  >\n  \n  <row height=\"1.0\">\n    <key c=\"q\" ne=\"1\" se=\"loc esc\" shift=\"0.0\" width=\"1.0\" />\n    <key c=\"w\" ne=\"2\" nw=\"~\" shift=\"0.0\" sw=\"@\" width=\"1.0\" />\n    <key c=\"e\" ne=\"3\" nw=\"!\" se=\"loc €\" shift=\"0.0\" sw=\"#\" width=\"1.0\" />\n    <key c=\"r\" ne=\"4\" shift=\"0.0\" sw=\"$\" width=\"1.0\" />\n    <key c=\"t\" ne=\"5\" shift=\"0.0\" sw=\"%\" width=\"1.0\" />\n    <key c=\"y\" ne=\"6\" shift=\"0.0\" sw=\"^\" width=\"1.0\" />\n    <key c=\"u\" ne=\"7\" shift=\"0.0\" sw=\"&amp;\" width=\"1.0\" />\n    <key c=\"i\" ne=\"8\" shift=\"0.0\" sw=\"*\" width=\"1.0\" />\n    <key c=\"o\" ne=\"9\" se=\")\" shift=\"0.0\" sw=\"(\" width=\"1.0\" />\n    <key c=\"p\" ne=\"0\" shift=\"0.0\" width=\"1.0\" />\n  </row>\n  \n  <row height=\"1.0\">\n    <key c=\"a\" ne=\"`\" shift=\"0.5\" width=\"1.0\" />\n    <key c=\"s\" ne=\"loc §\" shift=\"0.0\" sw=\"loc ß\" width=\"1.0\" />\n    <key c=\"d\" shift=\"0.0\" width=\"1.0\" />\n    <key c=\"f\" shift=\"0.0\" width=\"1.0\" />\n    <key c=\"g\" ne=\"-\" shift=\"0.0\" sw=\"_\" width=\"1.0\" />\n    <key c=\"h\" ne=\"=\" shift=\"0.0\" sw=\"+\" width=\"1.0\" />\n    <key c=\"j\" se=\"}\" shift=\"0.0\" sw=\"{\" width=\"1.0\" />\n    <key c=\"k\" se=\"]\" shift=\"0.0\" sw=\"[\" width=\"1.0\" />\n    <key c=\"l\" ne=\"|\" shift=\"0.0\" sw=\"\\\" width=\"1.0\" />\n  </row>\n  \n  <row height=\"1.0\">\n    <key c=\"shift\" ne=\"loc capslock\" shift=\"0.0\" width=\"1.5\" />\n    <key c=\"z\" shift=\"0.0\" width=\"1.0\" />\n    <key c=\"x\" ne=\"†\" shift=\"0.0\" width=\"1.0\" />\n    <key c=\"c\" ne=\"&lt;\" shift=\"0.0\" sw=\".\" width=\"1.0\" />\n    <key c=\"v\" ne=\"&gt;\" shift=\"0.0\" sw=\",\" width=\"1.0\" />\n    <key c=\"b\" ne=\"?\" shift=\"0.0\" sw=\"/\" width=\"1.0\" />\n    <key c=\"n\" ne=\":\" shift=\"0.0\" sw=\";\" width=\"1.0\" />\n    <key c=\"m\" ne=\"&quot;\" shift=\"0.0\" sw=\"'\" width=\"1.0\" />\n    <key c=\"backspace\" ne=\"delete\" shift=\"0.0\" width=\"1.5\" />\n  </row>\n\n</keyboard>";

        keys.testOne =
          let l = getLayout "QWERTY (US)";
              k = l.keyboard;
              q_key = getKey 0 0 k;
          in expect.eq q_key {
            anticircle = null;
            c = { e = null; k = "q"; legend = null; loc = false; m = null; s = null; };
            e = null;
            indication = null;
            n = null;
            ne = { e = null; k = "1"; legend = null; loc = false; m = null; s = null; };
            nw = null;
            overrideXML = null;
            s = null;
            se = { e = null; k = "esc"; legend = null; loc = true; m = null; s = null; };
            shift = 0.0;
            sw = null;
            w = null;
            width = 1.0;
          };

        variants.noMods =
          let l = getLayout "Code QWERTY Compact";
              k = l.keyboard;
              q_key = getKey 0 0 k;
          in expect.eq q_key.c.k "q";

        variants.addMods =
          let l = getLayout "Code QWERTY Compact (leftMods)";
              k = l.keyboard;
              tl_key = getKey 0 0 k;
          in expect.eq [tl_key.c.k tl_key.width] [ "esc" (_4dp (10.0 / 11.0)) ];
      };
  };
})

# WM layout validation

## Scope

Configuration-only change, tested against the embedded Omni source at android-utils
`3b8184c5720e5c15277c58f14f3fb8100e28d37c` (same product tree as the published
1.33.27 release). No Android application source, device preferences, signing,
permissions, manager configuration or system installation changed.

- **50 Nix module checks passed**, including all ten compact entry/launch mappings,
  preservation of focus/Fn and E's mounted-terminal gestures, workspace/send
  modifier sequences, custom return targets, both variants and upstream-disabled
  keys. Existing XML golden and transform tests passed as well.
- **14 XML layouts parsed with Omni's production parser.** Every named layout
  jump resolved using `LayoutSwitchingUtils`; ten compact layouts retained the
  semantic new-terminal event.
- **Six actual `Keyboard2View` renderings**: compact typing, WM swipe pads and WM
  tap arrows, each at 360 and 600 pixels wide. Native-Skia Robolectric API28,
  Nord theme, isolated mock keyboard event sink. Hit-testing reached every key.
- This is renderer/parser evidence, **not** a physical Samsung test, Android input
  routing test, live manager enrollment test or crash-fix verification.

## Visual review

| Capture | Finding |
|---|---|
| [Swipes, 360](wm-swipes-360.png) | Nine numeric targets align across the top; send-window gestures remain visible underneath. Focus/Move/Resize have distinct large swipe regions. Footer remains reachable and labels do not clip. Corner labels are small at this width, as on the original compact keyboard; the tap variant is the alternative to precision swipes. |
| [Arrows, 360](wm-arrows-360.png) | Three columns keep Focus/Move/Size separate; all twelve directional tap targets are reachable. The seven-row layout is intentionally taller than the four-row swipe layout. |
| [Swipes, 600](wm-swipes-600.png) | Direction arrows, workspace gestures and return labels are clearer; no overlap or off-grid row edges. Existing Omni theme owns all colors and key styling. |

Without a live Flotilla connection the slot fallback labels are WM1/WM2/WM3/WM5.
When connected Omni supplies the configured live labels. Screenshots deliberately
do not fabricate that connection. Omni's configured extra Settings key appears on
the first key through its production layout modifier.

## Reproducing the cross-repository render

`tests/OmniGeneratedLayoutTest.java` is an optional Robolectric fixture, not part of
the Android app. Export the full generated layout set as individual XML files,
then copy the fixture into the embedded Omni `test/com.harryaskham.omni/` directory
only if no file of that name exists. Run from `Unexpected-Keyboard/`:

```sh
OMNI_LAYOUT_DIR=/absolute/path/to/generated-xml \
OMNI_SCREENSHOT_DIR=/absolute/path/to/screenshots \
  nix develop --command gradle :testDebugUnitTest \
    --tests com.harryaskham.omni.OmniGeneratedLayoutTest
```

Remove the temporary test copy afterwards. Check Gradle-generated ComposeKeyData
and check_layout.output against the starting worktree rather than committing that
noise or overwriting another person's edits. Tests expect the fourteen default
layouts including the new pair. An explicitly narrower export is not this fixture.

The active Android crash report (tap/focus/raise on a floating NOD terminal) remains
unidentified. These layout checks do not diagnose or repair it.

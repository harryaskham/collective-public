package com.harryaskham.omni;

// Optional cross-repository acceptance fixture. Copy into the embedded Omni
// test directory, run through its pinned Nix Gradle shell, then remove the copy.
// OMNI_LAYOUT_DIR holds generated XML; OMNI_SCREENSHOT_DIR receives native-Skia
// renderings of the real Keyboard2View (not physical-device acceptance).
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.view.ContextThemeWrapper;
import android.view.View;
import java.io.File;
import java.io.FileOutputStream;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashSet;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.annotation.GraphicsMode;
import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

@RunWith(RobolectricTestRunner.class)
@org.robolectric.annotation.Config(sdk = 28, qualifiers = "w600dp-h800dp-mdpi")
@GraphicsMode(GraphicsMode.Mode.NATIVE)
public class OmniGeneratedLayoutTest {
  @Test public void generatedLayoutsParseSwitchAndRender() throws Exception {
    File directory = new File(System.getenv("OMNI_LAYOUT_DIR"));
    File[] files = directory.listFiles((dir, name) -> name.endsWith(".xml"));
    assertNotNull(files);
    Context context = new ContextThemeWrapper(RuntimeEnvironment.getApplication(), R.style.Nord);
    Config.initGlobalConfig(DirectBootAwarePreferences.get_shared_preferences(context),
        context.getResources(), mock(Config.IKeyEventHandler.class), false);
    Config cfg = Config.globalConfig();
    cfg.layouts = new ArrayList<>();
    for (File file : files) cfg.layouts.add(KeyboardData.load_string_exn(new String(Files.readAllBytes(file.toPath()), java.nio.charset.StandardCharsets.UTF_8)));
    assertEquals(14, cfg.layouts.size());
    int compact = 0;
    for (KeyboardData data : cfg.layouts) {
      // Every named jump resolves through Omni's actual normalizer and switcher.
      for (KeyValue value : data.getKeys().keySet()) {
        if (value.getKind() == KeyValue.Kind.Event && value.getEvent() == KeyValue.Event.SWITCH_TO_LAYOUT) {
          int[] result = {-1};
          LayoutSwitchingUtils.switchToLayoutByName(value.getLayoutName(), cfg, i -> result[0] = i);
          assertTrue(data.name + ": missing " + value.getLayoutName(), result[0] >= 0);
        }
      }
      if (data.name.startsWith("Code QWERTY Compact")) {
        compact++;
        assertTrue(data.getKeys().keySet().stream().anyMatch(v -> v.getKind() == KeyValue.Kind.Event
            && v.getEvent() == KeyValue.Event.WM_OPEN_TERMINAL));
      }
      if (!data.name.equals("Code QWERTY Compact") && !data.name.startsWith("Flotilla WM")) continue;
      for (int width : new int[]{360, 600}) {
        cfg.floatingKeyboardWidthPercent = 100;
        cfg.floatingKeyboardHeightPercent = 45;
        Keyboard2View keyboard = (Keyboard2View)View.inflate(context, R.layout.keyboard, null);
        keyboard.setFloatingViewport(width, 800);
        keyboard.setKeyboard(LayoutModifier.modify_layout(data));
        keyboard.measure(View.MeasureSpec.makeMeasureSpec(width, View.MeasureSpec.EXACTLY),
            View.MeasureSpec.makeMeasureSpec(800, View.MeasureSpec.AT_MOST));
        keyboard.layout(0, 0, width, keyboard.getMeasuredHeight());
        assertTrue(keyboard.getHeight() > 0 && keyboard.getHeight() <= 800);
        // All layout keys must survive production layout modification and be reachable.
        HashSet<KeyboardData.Key> reachable = new HashSet<>();
        for (int y = 1; y < keyboard.getHeight(); y += 2)
          for (int x = 1; x < width; x += 2) {
            KeyboardData.Key k = keyboard.getKeyAtPosition(x, y);
            if (k != null) reachable.add(k);
          }
        int expected = 0;
        for (KeyboardData.Row row : data.rows) expected += row.keys.size();
        assertTrue(data.name + ": unreachable keys", reachable.size() >= expected);
        Bitmap bitmap = Bitmap.createBitmap(width, keyboard.getHeight(), Bitmap.Config.ARGB_8888);
        keyboard.draw(new Canvas(bitmap));
        File out = new File(System.getenv("OMNI_SCREENSHOT_DIR"),
            data.name.replaceAll("[^A-Za-z0-9]+", "-") + "-" + width + ".png");
        out.getParentFile().mkdirs();
        try (FileOutputStream stream = new FileOutputStream(out)) {
          assertTrue(bitmap.compress(Bitmap.CompressFormat.PNG, 100, stream));
        }
        bitmap.recycle();
      }
    }
    assertEquals(10, compact);
  }
}

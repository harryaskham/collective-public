{ lib ? import <nixpkgs/lib>, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;
with collective-lib.clib;

rec {
  curveTemplates = {
    # Stock settings for the GPD Win Mini
    winMini = {
      model = "GPD Win Mini";
      suffix = "";
      author = "Collective Flake";
      ecPollInterval = 5000;
      readWriteWords = false;
      criticalTemperature = 85;
      readRegister = 122;
      writeRegister = 122;
      minSpeedValue = 1;
      maxSpeedValue = 244;
      resetRequired = true;
      fanSpeedResetValue = 0;
      fanDisplayName = "Fan";
    };
    # Stock settings for the GPD Win Mini
    pocket4 = {
      model = "GPD Pocket 4";
      suffix = "";
      author = "Collective Flake";
      ecPollInterval = 5000;
      readWriteWords = false;
      criticalTemperature = 85;
      readRegister = 122;
      writeRegister = 122;
      minSpeedValue = 1;
      maxSpeedValue = 244;
      resetRequired = true;
      fanSpeedResetValue = 0;
      fanDisplayName = "Fan";
    };
  };
  mkCurve = tmpl: suffix: temperatureThresholds: tmpl // {
    inherit suffix temperatureThresholds;
  };
  curves = rec {
    # Slightly higher temp triggers than stock to avoid fan noise
    winMini = mkCurve curveTemplates.winMini "" [
        { up = 30; down = 0; speed = 0.0; }
        { up = 45; down = 25; speed = 30.0; }
        { up = 60; down = 40; speed = 40.0; }
        { up = 70; down = 50; speed = 60.0; }
        { up = 80; down = 65; speed = 80.0; }
        { up = 85; down = 75; speed = 100.0; }
      ];
    pocket4 = mkCurve curveTemplates.pocket4 "" [
        { up = 30; down = 0; speed = 0.0; }
        { up = 45; down = 25; speed = 30.0; }
        { up = 60; down = 40; speed = 40.0; }
        { up = 70; down = 50; speed = 60.0; }
        { up = 80; down = 65; speed = 80.0; }
        { up = 85; down = 75; speed = 100.0; }
      ];
  };
  curveId = curve: "${curve.model} ${curve.suffix}";
  curveFilename = curve: "${curveId curve}.json";
}

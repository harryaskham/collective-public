{ lib ? import <nixpkgs/lib>, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;
with collective-lib.clib;

rec {
  curveTemplates = {
    # Stock settings for GPD devices
    # See https://github.com/Cryolitia/gpd-fan-driver/blob/main/gpd-fan.c
    # Should be good for at least Win Mini, Pocket 4
    GPD = {
      model = "GPD";
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
    # Slightly higher temp triggers than stock NBFC to avoid fan noise
    GPD = {
      quiet = mkCurve curveTemplates.GPD "" [
        { up = 30; down = 0; speed = 0.0; }
        { up = 45; down = 25; speed = 30.0; }
        { up = 60; down = 40; speed = 40.0; }
        { up = 70; down = 50; speed = 60.0; }
        { up = 80; down = 65; speed = 80.0; }
        { up = 85; down = 75; speed = 100.0; }
      ];
    };
  };
  curveId = curve: "${curve.model} ${curve.suffix}";
  curveFilename = curve: "${curveId curve}.json";
}

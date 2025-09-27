{ lib ? import <nixpkgs/lib>, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;
with collective-lib.typed;

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

  mkTemperatureThreshold' = curve: ''
    {
      "UpThreshold": ${toString curve.up},
      "DownThreshold": ${toString curve.down},
      "FanSpeed": ${toString curve.speed}
    }
  '';

  mkFanCurveConfig' = curve: ''
    {
      "NotebookModel": "${curve.model}",
      "Author": "${curve.author}",
      "EcPollInterval": ${toString curve.ecPollInterval},
      "ReadWriteWords": ${boolToString curve.readWriteWords},
      "CriticalTemperature": ${toString curve.criticalTemperature},
      "FanConfigurations": [
        {
          "ReadRegister": ${toString curve.readRegister},
          "WriteRegister": ${toString curve.writeRegister},
          "MinSpeedValue": ${toString curve.minSpeedValue},
          "MaxSpeedValue": ${toString curve.maxSpeedValue},
          "ResetRequired": ${boolToString curve.resetRequired},
          "FanSpeedResetValue": ${toString curve.fanSpeedResetValue},
          "FanDisplayName": "${curve.fanDisplayName}",
          "TemperatureThresholds": [
            ${concatStringsSep ",\n" (map mkTemperatureThreshold curve.temperatureThresholds)}
          ],
          "FanSpeedPercentageOverrides": []
        }
      ],
      "RegisterWriteConfigurations": ${curve.registerWriteConfigurations or "[]"}
    }
  '';

  mkTemperatureThreshold = curve: {
    UpThreshold = curve.up;
    DownThreshold = curve.down;
    FanSpeed = curve.speed;
  };

  mkFanCurveConfig = curve: builtins.toJSON {
    NotebookModel = curve.model;
    Author = curve.author;
    EcPollInterval = curve.ecPollInterval;
    ReadWriteWords = curve.readWriteWords;
    CriticalTemperature = curve.criticalTemperature;
    FanConfigurations = [
      {
        ReadRegister = curve.readRegister;
        WriteRegister = curve.writeRegister;
        MinSpeedValue = curve.minSpeedValue;
        MaxSpeedValue = curve.maxSpeedValue;
        ResetRequired = curve.resetRequired;
        FanSpeedResetValue = curve.fanSpeedResetValue;
        FanDisplayName = curve.fanDisplayName;
        TemperatureThresholds = map mkTemperatureThreshold curve.temperatureThresholds;
        FanSpeedPercentageOverrides = [];
      }
    ];
    RegisterWriteConfigurations = curve.registerWriteConfigurations or [];
  };

  _tests = with tests; suite {
    mkFanCurveConfig = {
      GPD.quiet = expect.eq (mkFanCurveConfig curves.GPD.quiet)
        ''{"Author":"Collective Flake","CriticalTemperature":85,"EcPollInterval":5000,"FanConfigurations":[{"FanDisplayName":"Fan","FanSpeedPercentageOverrides":[],"FanSpeedResetValue":0,"MaxSpeedValue":244,"MinSpeedValue":1,"ReadRegister":122,"ResetRequired":true,"TemperatureThresholds":[{"DownThreshold":0,"FanSpeed":0.0,"UpThreshold":30},{"DownThreshold":25,"FanSpeed":30.0,"UpThreshold":45},{"DownThreshold":40,"FanSpeed":40.0,"UpThreshold":60},{"DownThreshold":50,"FanSpeed":60.0,"UpThreshold":70},{"DownThreshold":65,"FanSpeed":80.0,"UpThreshold":80},{"DownThreshold":75,"FanSpeed":100.0,"UpThreshold":85}],"WriteRegister":122}],"NotebookModel":"GPD","ReadWriteWords":false,"RegisterWriteConfigurations":[]}'';
    };
  };
}

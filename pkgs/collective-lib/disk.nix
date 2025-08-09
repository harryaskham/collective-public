{ lib ? import <nixpkgs/lib>, collective-lib ? import ./. { inherit lib; }, ... }:

with lib;
with collective-lib.clib;

# Common FS system config generation
{
  mkWindows = partition: {
    inherit partition;
    fsType = "ntfs-3g";
  };
  mkSD = partition: {
    inherit partition;
    fsType = "exfat";
  };
  mkExtHD = partition: {
    inherit partition;
    fsType = "exfat";
  };
  deviceType = types.submodule {
    options = {
      partition = mkOption {
        type = types.str;
        description = "Partition to mount";
      };
      fsType = mkOption {
        type = types.str;
        description = "Type of device to mount";
      };
      options = mkOption {
        type = types.listOf types.str;
        description = "Options for mounting";
        default = [ "rw" "uid=1000" "x-gvfs-show" "nofail" ];
      };
      extraOptions = mkOption {
        type = types.listOf types.str;
        description = "Extra options for mounting";
        default = [];
      };
    };
  };
  toFS = dev: {
    device = dev.partition;
    fsType = dev.fsType;
    options = dev.options ++ dev.extraOptions;
  };
}

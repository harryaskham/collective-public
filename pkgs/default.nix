{ pkgs, ...}:

rec {
  # Public subset of the Collective base library.
  collective-lib = import ./collective-lib { inherit pkgs; };

  collective-lib-drv = builtins.derivation {
      name    = "collective-lib";
      system  = builtins.currentSystem;
      builder = "/bin/sh";
      args    = [ "-c" "echo ${collective-lib.typed.toShellValue collective-lib._tests.run} > $out" ];
      outputs = [ "out" ];
    };

  default = collective-lib-drv;

  # Namespaced python packages.
  collective-pythonPackages = import ./pythonPackages { inherit pkgs; };

  # Application wrapping of the HHD adjustor CLI
  handheld-daemon-adjustor =
    pkgs.python3Packages.toPythonApplication
      collective-pythonPackages.handheld-daemon-adjustor;
}

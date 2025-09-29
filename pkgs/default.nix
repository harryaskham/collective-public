{ pkgs, inputs, ...}:

rec {
  # Public subset of the Collective base library.
  collective-lib = import ./collective-lib { inherit pkgs inputs; inherit (pkgs) lib; };

  # Namespaced python packages.
  collective-pythonPackages = import ./pythonPackages { inherit pkgs; };

  # Application wrapping of the HHD adjustor CLI
  handheld-daemon-adjustor =
    pkgs.python3Packages.toPythonApplication
      collective-pythonPackages.handheld-daemon-adjustor;

  # Vast.ai CLI
  vastai-cli = pkgs.callPackage ./vastai-cli { };
}

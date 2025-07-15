{ pkgs, nix-parsec, ...}:

rec {
  # Public subset of the Collective base library.
  collective-lib = import ./collective-lib { inherit pkgs; inherit nix-parsec; };

  # Namespaced python packages.
  collective-pythonPackages = import ./pythonPackages { inherit pkgs; };

  # Application wrapping of the HHD adjustor CLI
  handheld-daemon-adjustor =
    pkgs.python3Packages.toPythonApplication
      collective-pythonPackages.handheld-daemon-adjustor;
}

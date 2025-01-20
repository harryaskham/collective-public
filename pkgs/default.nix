{ pkgs, ...}:

let
  pythonPackages = import ./pythonPackages { inherit pkgs; };
in {
  handheld-daemon-adjustor = pkgs.python3Packages.toPythonApplication pythonPackages.handheld-daemon-adjustor;
}

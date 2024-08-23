{inputs, ...}:

{
  additions = final: _prev: import ../pkgs final.pkgs;

  modifications = final: prev: rec {
    python3 = prev.python3.override {
      packageOverrides = self: super: super // final.toOverlay.pythonPackages;
    };
    python3Packages = final.python3.pkgs;

    handheld-daemon = final.toOverlay.handheld-daemon;
  };
}

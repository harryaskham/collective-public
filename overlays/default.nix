{inputs, ...}:

{
  additions = final: _prev: import ../pkgs final.pkgs;

  modifications = final: prev: {
    handheld-daemon = final.new.handheld-daemon;
    # TODO: Messy below, probably only need the top one
    python3 = prev.python3.override {
      packageOverrides = self: super: final.new.pythonPackages;
    };
    pythonPackages = prev.pythonPackages // final.new.pythonPackages;
    python3Packages = prev.python3Packages // final.new.pythonPackages;
    python312Packages = prev.python312Packages // final.new.pythonPackages;
  };
}

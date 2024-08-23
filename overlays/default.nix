{inputs, ...}:

{
  additions = final: _prev: import ../pkgs final.pkgs;

  modifications = final: prev: {
    # handheld-daemon = final.new.handheld-daemon;
  };
}

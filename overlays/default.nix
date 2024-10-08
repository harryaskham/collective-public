{inputs, ...}:

let
  image-go-nord-overlay = self: super: {
    image-go-nord = super.image-go-nord.overrideAttrs (_: {
      doCheck = false;
      propagatedBuildInputs =
        let missingBuildInputs = with super; [numpy ffmpeg-python requests];
        in super.image-go-nord.propagatedBuildInputs ++ missingBuildInputs;
    });
  };
in
  let myPkgs = final: import ../pkgs { pkgs = final; };
  in
  {
    "python" = final: prev: {
       python3 = prev.python3.override {
         packageOverrides = self: super:
           (myPkgs final).python3PackageOverrides
           // (image-go-nord-overlay self super);
       };
       python3Packages = final.python3.pkgs;
     };
     "replacements" = final: _: (myPkgs final).replacements;
     "new" = final: _: (myPkgs final).new;
   }

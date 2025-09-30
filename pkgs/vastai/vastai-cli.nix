{ pkgs, stdenv, python3, fetchurl, ...}:

stdenv.mkDerivation rec {
  pname = "vastai-cli";
  version = "0.4.0";
  buildInputs = [
    (python3.withPackages (ps: [ ps.requests ]))
  ];
  src = pkgs.fetchFromGitHub {
    owner = "vast-ai";
    repo = "vast-cli";
    rev = "refs/tags/v${version}";
    hash = "sha256-vtYSOfUmOvULLBULtabL15D82QxC2I00RbvCDrCoI3w=";
  };
  installPhase = ''
    install -Dm755 $src/vast.py $out/bin/vast
  '';
}

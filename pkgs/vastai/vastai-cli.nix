{ pkgs, stdenv, python3, fetchurl, ...}:

stdenv.mkDerivation rec {
  pname = "vastai-cli";
  version = "0.4.0";
  buildInputs = [
    (python3.withPackages (ps: [ ps.requests ]))
  ];
  src = fetchurl {
    url = "https://raw.githubusercontent.com/vast-ai/vast-cli/refs/tags/v${version}/vast.py";
    hash = "sha256-mqgE8VwPJAbn+s+G8pI0mzouE0MX/2ikHBjNHVrP0ac=";
  };
  dontUnpack = true;
  installPhase = ''
    install -Dm755 $src $out/bin/vast
  '';
}

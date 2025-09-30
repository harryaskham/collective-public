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
    hash = "sha256-imzzAOOGdn4xoZEOd1KQRMNlOpZtenWgp58g6AqBK+I=";
  };
  installPhase = ''
    install -Dm755 $src/vast.py $out/bin/vast
  '';
}

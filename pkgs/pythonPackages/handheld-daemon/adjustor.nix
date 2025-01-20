{
  fetchFromGitHub,
  lib,
  python3,
}:
python3.pkgs.buildPythonPackage rec {
  pname = "hhd-adjustor";
  version = "3.6.4";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "hhd-dev";
    repo = "adjustor";
    rev = "refs/tags/v${version}";
    hash = "sha256-vtYSOfUmOvULLBULtabL15D82QxC2I00RbvCDrCoI3w=";
  };

  propagatedBuildInputs = with python3.pkgs; [
    pyroute2
    fuse
    pygobject3
    dbus-python
    pyyaml
    rich
    setuptools
  ];

  doCheck = false;

  meta = with lib; {
    homepage = "https://github.com/hhd-dev/adjustor/";
    description = "The Adjustor TDP plugin for Handheld Daemon.";
    platforms = platforms.linux;
    license = licenses.mit;
    maintainers = with maintainers; [ harryaskham ];
    mainProgram = "adjustor";
  };
}

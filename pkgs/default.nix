{ pkgs, inputs, ...}:

rec {
  # Public subset of the Collective base library.
  collective-lib = import ./collective-lib { inherit pkgs inputs; inherit (pkgs) lib; };

  # Namespaced python packages.
  collective-pythonPackages = import ./pythonPackages { inherit pkgs; };

  # Application wrapping of the HHD adjustor CLI
  handheld-daemon-adjustor =
    pkgs.python3Packages.toPythonApplication
      collective-pythonPackages.handheld-daemon-adjustor;

  # Vast.ai CLI
  vastai-cli = (pkgs.callPackage ./vastai { }).vastai-cli;

  # termux-exec: TCP client for running native Termux commands from proot
  termux-exec = pkgs.callPackage ./termux-exec.nix {};

  # nod-exec: TCP command server + clients for Nix-on-Droid
  nod-exec = pkgs.callPackage ./nod-exec.nix {};

  # nod-tmux: tmux management + floating terminal launcher for nod-term
  nod-tmux = pkgs.callPackage ./nod-tmux.nix {};

  # nod-install: install APKs via rish/Shizuku from nix-on-droid
  nod-install = pkgs.callPackage ./nod-install.nix {};

  # am-supervisor: generic health-check watchdog for Android apps
  am-supervisor = pkgs.callPackage ./am-supervisor.nix {};

  # tasker-run: trigger named Tasker tasks via am broadcast
  tasker-run = pkgs.callPackage ./tasker-run.nix {};
}

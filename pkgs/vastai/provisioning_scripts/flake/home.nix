{ config, pkgs, ... }:

{
  home.username = "root";
  home.homeDirectory = "/root";
  home.stateVersion = "25.05";
  home.packages = [
  ];

  home.file = {
  };

  home.sessionVariables = {
  };

  programs = {
    home-manager.enable = true;
    bash.enable = true;
    zsh.enable = true;
    #ollama.enable = true;
  };
}

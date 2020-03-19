{ config, pkgs, lib, ... } :
{
  i18n.consoleFont = "Lat2-Terminus16";
  i18n.consoleKeyMap = "us";
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "Europe/Rome";
  system.stateVersion = "20.09";
  fonts.fonts = with pkgs; [
    powerline-fonts
    source-code-pro
    nerdfonts
    fira-code
    fira-code-symbols
  ];
imports = [
  ./hardware-configuration.nix
];
boot.loader.grub = {
  enable = true;
  version = 2;
  device = "/dev/sda";
};
networking.useDHCP = false;
networking.interfaces.enp1s0.useDHCP = true;
nixpkgs.config.allowUnfree = true;
environment.systemPackages = with pkgs; [
  # Editors
  emacs vim

  # Window manager
  sway swaylock swayidle xwayland waybar redshift-wlr termite

  # General
  qutebrowser
  gimp
  spotify
  rawtherapee

  # Programming
  python37 python37Packages.powerline 
  gcc gdb cmake
  guile

  # Cli
  zsh antigen
  tmux
  git
  stow
  cmatrix
  exa
  ranger
  irssi
  p7zip
  wget
  playerctl
  
  xterm 
];
services.xserver.enable = true;
services.xserver.displayManager.startx.enable = true;
environment.variables.ANTIGEN = "${pkgs.antigen}";
environment.variables.POWERLINE = "${pkgs.python37Packages.powerline}";
services = {
  openssh = {
    enable = true;
  };
  emacs = {
    enable = true;
  };
};
programs.sway.enable = true;
sound.enable = true;
hardware.pulseaudio.enable = true;
users.users.andrea = {
  isNormalUser = true;
  uid = 1000;
  home = "/home/andrea/";
  shell = "${pkgs.zsh}/bin/zsh";
  extraGroups = [ "wheel" ];
};
}

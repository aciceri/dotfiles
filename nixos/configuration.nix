{ config, pkgs, lib, ... }:

let 
  user = import ./user.nix;
  sources = import ./sources.nix;
in
{
 
  imports = builtins.filter builtins.pathExists [
    sources.home-manager
    /etc/nixos/hardware-configuration.nix
  ];
  
  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
  };
 
  networking.hostName = user.hostname;
  
  nixpkgs.config = {
    allowUnfree = true;
  };

  networking.dhcpcd.enable = true;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  time.timeZone = "Europe/Rome";
  location.latitude = 45.46;
  location.longitude = 9.18;
  
  fonts.fonts = with pkgs; [
    source-code-pro
    emacs-all-the-icons-fonts
    fira-code
    fira-code-symbols
  ];

  boot.extraModulePackages = with config.boot.kernelPackages; [
    v4l2loopback  # to use my digital camera as webcam
  ];
  
  environment.systemPackages = with pkgs; [ 
    vim  # useful in case of emergency if logged as root
    git  # same as above
    sshfs  # for ssh mounting
    xorg.xorgserver
    xorg.xf86inputevdev
    xorg.xf86videointel
  ];

  fileSystems = let # my public key must be appended to ~/.ssh/authorized_keys in the nas
    nasUser = "andrea";
    nasHost = "192.168.1.73";
    fsType = "fuse.sshfs";
    options = [
        "delay_connect"
        "_netdev,user"
        "idmap=user"
        "transform_symlinks"
        "identityfile=/home/andrea/.ssh/id_rsa"
        "allow_other"
        "default_permissions"
        "uid=1000"
        "gid=100"
        "nofail" # otherwise if the mounting fails the system will enter emergency mode
    ];
  in {
    "/home/${user.username}/nas/amule" = {
      inherit fsType options;
      device = "${nasUser}@${nasHost}:/mnt/archivio/amule";
    };
    "/home/${user.username}/nas/transmission" = {
      inherit fsType options;
      device = "${nasUser}@${nasHost}:/mnt/archivio/transmission";
    };
    "/home/${user.username}/nas/calibre" = {
      inherit fsType options;
      device = "${nasUser}@${nasHost}:/mnt/archivio/calibre";
    };
    "/home/${user.username}/nas/archivio" = {
      inherit fsType options;
      device = "${nasUser}@${nasHost}:/mnt/archivio/archivio";
    };
    "/home/${user.username}/nas/film" = {
      inherit fsType options;
      device = "${nasUser}@${nasHost}:/mnt/film/film";
    };
    "/home/${user.username}/nas/syncthing" = {
      inherit fsType options;
      device = "${nasUser}@${nasHost}:/mnt/archivio/syncthing";
    };
  };

  programs = {
    gnupg.agent.enable = true;
  };
  
  services = {
    openssh.enable = true;
    
    xserver = {
      enable = true;
      displayManager.startx.enable = true;
    };

    redshift = {
      enable = true;
    };

    mingetty.autologinUser = user.username;
  };

  users.extraUsers.${user.username} = {
    home = "/home/${user.username}";
    isNormalUser = true;
    uid = 10000;
    extraGroups = [
      "wheel"
      "fuse"
    ];
    shell = "${pkgs.zsh}/bin/zsh";
  };

  virtualisation.virtualbox.host.enable = true;
  users.extraGroups.vboxusers.members = [ user.username ];
  
  home-manager.users.${user.username} = args: import ./home.nix (args // { inherit pkgs user; });

  nixpkgs.overlays = with sources; [
    (import emacs-overlay)
    (import custom-overlay)
  ];

  system.stateVersion = "20.09";

}  

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
  location.provider = "geoclue2";
  
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

  # The local .ssh/id_rsa.pub must be appended to ~/.ssh/authorized_keys in the nas
  # since root mounts these folders I cannot use the ssh key generated by gpg
  # because the agent is started configured only for the #{user.username} user.
  # So it's needed to generate a ssh keypair with ssh-agent only for this purpose.
  fileSystems = let
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
        "nofail"  # otherwise if the mounting fails the system will enter emergency mode
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
    "/home/${user.username}/nas/musica" = {
      inherit fsType options;
      device = "${nasUser}@${nasHost}:/mnt/film/musica";
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
    
    mingetty.autologinUser = user.username;
  };

  users.extraUsers.${user.username} = {
    home = "/home/${user.username}";
    isNormalUser = true;
    uid = 1000;
    extraGroups = [
      "wheel"
      "fuse"
      "video"
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

  system.stateVersion = "20.03";

}  

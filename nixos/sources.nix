{
  home-manager = "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos";
  emacs-overlay = "${builtins.fetchTarball https://github.com/nix-community/emacs-overlay/archive/dcb4f8e97b3a6f215e8a30bc01028fc67a4015e7.tar.gz}";
  wayland-overlay = "${builtins.fetchTarball https://github.com/colemicklens/nixpkgs-wayland/archive/master.tar.gz}";
  custom-overlay = ./custom-pkgs;
}

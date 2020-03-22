{
  home-manager = "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos";
  emacs-overlay = "${builtins.fetchTarball https://github.com/nix-community/emacs-overlay/archive/master.tar.gz}";
  custom-overlay = ./custom-pkgs;
}

{
  home-manager = "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/96d7de6db18d9a5bf254ddf3525bb4ef1d2a6bda.tar.gz}/nixos";
  emacs-overlay = "${builtins.fetchTarball https://github.com/nix-community/emacs-overlay/archive/master.tar.gz}";
  custom-overlay = ./custom-pkgs;
}

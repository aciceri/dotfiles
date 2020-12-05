{
  home-manager = "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/a3a0f1289acac24ce2ffe0481bf8cabd3a6ccc64.tar.gz}/nixos";
  #home-manager = "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos";
  emacs-overlay = "${builtins.fetchTarball https://github.com/nix-community/emacs-overlay/archive/master.tar.gz}";
  wayland-overlay = "${builtins.fetchTarball https://github.com/colemicklens/nixpkgs-wayland/archive/master.tar.gz}";
  custom-overlay = ./custom-pkgs;
}

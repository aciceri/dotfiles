{self, super}:
super.pkgs.emacsWithPackagesFromUsePackage {
  config = builtins.readFile ../../dotfiles/emacs/init.el;
  package = super.pkgs.emacsUnstable.override({
    imagemagick = super.pkgs.imagemagickBig;
  });
}

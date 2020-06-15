{self, super}:
super.pkgs.emacsWithPackagesFromUsePackage {
  config = builtins.readFile ../../dotfiles/emacs/init.el;
  package = (super.pkgs.emacsUnstable.override({
    imagemagick = super.pkgs.imagemagick;
  })).overrideAttrs(old: rec {
    configureFlags = (old.configureFlags or []) ++ ["--with-imagemagick"];
  });
}

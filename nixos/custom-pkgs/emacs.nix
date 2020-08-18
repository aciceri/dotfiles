{self, super}:
super.pkgs.emacsWithPackagesFromUsePackage {
  config = builtins.readFile ../../dotfiles/emacs/init.el;
  alwaysEnsure = true;
  package = (super.pkgs.emacs.override({
    imagemagick = super.pkgs.imagemagick;
  })).overrideAttrs(old: rec {
    configureFlags = (old.configureFlags or []) ++ ["--with-imagemagick"];
  });
}

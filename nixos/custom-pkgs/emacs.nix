{self, super}:
super.pkgs.emacsWithPackagesFromUsePackage {
  config = ../../dotfiles/emacs/README.org;
  alwaysEnsure = true;
  alwaysTangle = true;
  package = (super.pkgs.emacsUnstable.override({
   imagemagick = super.pkgs.imagemagick;
  })).overrideAttrs(old: rec {
    configureFlags = (old.configureFlags or []) ++ [
     "--with-imagemagick"
    ];
  });
}

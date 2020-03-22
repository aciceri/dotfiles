{ pkgs }:
pkgs.emacsWithPackagesFromUsePackage {
    config = builtins.readFile ../../dotfiles/emacs/init.el;
}

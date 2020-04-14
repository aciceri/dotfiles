self: super: {
  spotify-adkiller = import ./spotify-adkiller.nix { self=self; super=super; };
  calibre = import ./calibre.nix {self=self; super=super; };
}

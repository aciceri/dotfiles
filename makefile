

build: README.org
	emacs README.org --batch -f org-babel-tangle 

install: configuration.nix
	sudo rm -f /etc/nixos/configuration.nix
	sudo ln -s configuration.nix /etc/nixos/configuration.nix


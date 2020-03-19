build: README.org
	emacs README.org --batch -f org-babel-tangle 
	mkdir -p ~/.config/
	cd dotfiles/ && stow -R -t ~/ *

install: configuration.nix
	sudo rm -f /etc/nixos/configuration.nix
	sudo cp configuration.nix /etc/nixos/configuration.nix
	sudo nixos-rebuild switch
	stow -d dotfiles/ -S vim -t ~/

test: configuration.nix
	sudo rm -f /etc/nixos/configuration.nix
	sudo cp configuration.nix /etc/nixos/configuration.nix
	sudo nixos-rebuild test


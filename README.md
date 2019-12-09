# My dotfiles
This repo contains my dotfiles and includes [dotdrop](https://github.com/deadc0de6/dotdrop)
as a submodule in the git tree, the `./dotdrop.sh` script should automatically updates itself.
The configurations are inside the folder `dotfiles`, the directory structure is preserved but the file names may miss the dot in the beginning, however when the dotfiles are installed the dot is restored.


## Usage
This repo contains dotfiles that, indeed, are jinja2 template. So there are some files which contain variables like `{{@@ username @@}}` or `{{@@ password @@}}`, the values of these variables is stored inside the file `vars.yaml`, that, for obvious reasons is in the `.gitignore`. This file is necessary and it's structured as follows:
```
variables:
  freenodeNick: ""
  freenodePassword: ""
  irssiNick: ""
```
To *install* the dotfiles in the machine, i.e. copy from the repo to the correct folders the configuration, the command is

```./dotdrop.sh install```

This automatically replaces the names of the variables with their values.

To *import* a new (already existing) dotfile to the repo, i.e. copy from the folders of the machine to the repo, the command is 

```./dotdrop.sh import ~/.config/program/config```

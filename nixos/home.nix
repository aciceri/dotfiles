{ user, pkgs, ... }:

let 
  sources = import ./sources.nix;
in
{
  home.packages = with pkgs; [
    # Emacs
    (import ./custom-pkgs/emacs.nix { pkgs=pkgs; })
    
    # Cli
    vim
    git
    exa  # ls replacement
    translate-shell
    ack  # used by Helm for realtime grepping
    surfraw
    
    
    # Not cli
    zathura
    qutebrowser
    gimp
    rawtherapee
    spotify-adkiller
    
    # Games
    cmatrix
    nethack
  ];

  home.file = {
    ".emacs.d" = {
      source = ../dotfiles/emacs;
      recursive = true;
    };
    "test.txt".text = "${pkgs.dbus}";
    ".xinitrc".text = ''
                      #if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
                      #  eval $(dbus-launch --exit-with-session --sh-syntax)
                      #fi
                      #systemctl --user import-environment $DISPLAY $XAUTHORITY
                      #if command -v dbus-update-activation-environment >/dev/null 2>&1; then
                      #   dbus-update-activation-environment $DISPLAY $XAUTHORITY
                      #fi
                      
                      #xsetroot -cursor_name left_ptr
                      #exec emacsclient -c -F "'(fullscreen . maximized)"
                      exec dbus-launch --exit-with-session emacsclient -c -F "'(fullscreen . maximized)"
                    '';
    ".zlogin".text = ''
                     [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && sleep 2 &&exec startx
                     '';
    ".config/qutebrowser/config.py".source = ../dotfiles/qutebrowser/config.py;
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    autocd = true;
    plugins = [
      {
        name = "nix-zsh-completions";
        src = pkgs.nix-zsh-completions;
      }
      {
        name = "spaceship";
        src = pkgs.spaceship-prompt;
        file = "share/zsh/themes/spaceship.zsh-theme";
      }
      {
        name = "zsh-syntax-highlighting";
        src = pkgs.zsh-syntax-highlighting;
        file = "share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh";
      }
      
    ];
    oh-my-zsh = {
      enable = true;
      plugins = [
        "git"
        "sudo"
        "command-not-found"
        "colored-man-pages"
        "colorize"
      ];
    };
    shellAliases = {
      "ls" = "exa -l";
    };
    localVariables = {
      SPACESHIP_TIME_SHOW = "true";
      SPACESHIP_USER_SHOW = "always";
      SPACESHIP_HOST_SHOW = "always";
    };
  };

}

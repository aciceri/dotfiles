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
    surfraw  # used by Helm for www searching
    youtube-dl
    gphoto2  # to manage my digital camera
    ffmpeg  # useful to convert and edit my videos
    busybox 
    rclone
    gnupg
    gitAndTools.git-annex
    gitAndTools.git-annex-remote-rclone
    p7zip
    tree
    
    # Not cli
    zathura
    qutebrowser
    gimp
    rawtherapee
    spotify
    spotify-adkiller
    mpv
    skypeforlinux
    calibre  # overridden with custom overlay to have unrar support

    # Developing
    python3
    
    # Games
    cmatrix
    nethack
    cataclysm-dda
    angband
  ];

  home.file = {
    ".emacs.d" = {
      source = ../dotfiles/emacs;
      recursive = true;
    };
    ".xinitrc".source = ../dotfiles/xorg/.xinitrc;
    ".zlogin".source = ../dotfiles/zsh/.zlogin;
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
      "webcam-gphoto2" = "gphoto2 --stdout --capture-movie | ffmpeg -i - -vcodec rawvideo -pix_fmt yuv420p -threads 0 -f v4l2 /dev/video0";  # to use my digital camera as a webcam
    };
    localVariables = {
      SPACESHIP_TIME_SHOW = "true";
      SPACESHIP_USER_SHOW = "always";
      SPACESHIP_HOST_SHOW = "always";
    };
  };

}

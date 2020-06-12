{ user, pkgs, ... }:

let 
  sources = import ./sources.nix;
in
{
  home.packages = with pkgs; [
    # Emacs
    customEmacs
    
    # Cli
    vim
    git
    exa  # ls replacement
    pass
    mailutils
    translate-shell
    ack  # used by Helm for realtime grepping
    surfraw  # used by Helm for www searching
    youtube-dl
    gphoto2  # to manage my digital camera
    ffmpeg  # useful to convert and edit my videos
    #gnutls
    rclone
    gnupg
    pinentry
    tree
    imagemagick
    scrot
    xclip
    argyllcms
    xcalib
    texlive.combined.scheme-full
    beets
    redshift
    
    
    # Not cli
    zathura
    qutebrowser
    (firefox.override { extraNativeMessagingHosts = [ passff-host ]; })
    tor-browser-bundle-bin
    gimp
    rawtherapee
    spotify
    spotify-adkiller
    mpv
    skypeforlinux
    calibre  # overridden with custom overlay to have unrar support
    riot-desktop
    signal-desktop
    tdesktop
    displaycal
    xcalib
    postman  # temporary
    
    # Developing
    python3
    
    # Games
    cmatrix
    nethack
    cataclysm-dda
    angband
    qemu
  ];

  home.file = {
    ".emacs.d" = {
      source = ../dotfiles/emacs;
      recursive = true;
    };
    ".xinitrc".source = ../dotfiles/xorg/.xinitrc;
    ".zlogin".source = ../dotfiles/zsh/.zlogin;
    ".config/qutebrowser/config.py".source = ../dotfiles/qutebrowser/config.py;
    ".config/beets/config.yaml".source = ../dotfiles/beets/config.yaml;
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
      "screenshot" = "scrot '/home/${user.username}/shots/%F_%T_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f' -s";
    };
    localVariables = {
      SPACESHIP_TIME_SHOW = "true";
      SPACESHIP_USER_SHOW = "always";
      SPACESHIP_HOST_SHOW = "always";
      EDITOR = "emacsclient";
    };
  };

  programs.git = {
    enable = true;
    userName = user.githubUsername;
    userEmail = user.email;
    signing = {
      signByDefault = true;
      key = user.gpgKey;
    };
    extraConfig  ={
      url = {
        "ssh://git@github.com/" = { insteadOf = https://github.com/; };
      };
    };
  };

  services.mpd = {
    enable = true;
    musicDirectory = "/home/${user.username}/nas/musica/";
    network.listenAddress = "::";
    network.port = 6600;
    extraConfig = ''
      audio_output {
        type    "pulse"
        name    "MPD"
      }
    '';
  };
  
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    sshKeys = [ user.gpgSshKeygrip ];
  };

  programs.gpg = {
    enable = true;
    settings = {
    };
  };

}

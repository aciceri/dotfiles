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
    xcompmgr  # test
    xorg.transset  # test
    openbox  # test
    nur.repos.mic92.nixos-shell
    nixops
    go-ethereum
    solc  # solidty compiler
    guile
    clisp
    rclone
    
    
    # Not cli
    zathura
    qutebrowser
    tor-browser-bundle-bin
    gimp
    rawtherapee
    spotify
    spotify-adkiller
    mpv
    skypeforlinux
    (calibre.override {
      unrarSupport = true;  # why doesn't work?
    }) 
    riot-desktop
    signal-desktop
    tdesktop
    discord
    deltachat-electron
    displaycal
    xcalib
    postman  # temporary

    libvterm

    obs-studio

    # Job
    citrix_workspace
    teams
    remmina
    vscode
    docker
    
    # Developing
    (python3.withPackages (ps: with ps; [
      matplotlib
      gpxpy
      pip
    ]))
    yarn2nix
    yarn
    nodejs
    
    # Games
    cmatrix
    nethack
    cataclysm-dda
    angband
    lutris
    vulkan-tools
    gnome3.adwaita-icon-theme
    wineWowPackages.stable
    winetricks
    qemu
    dolphinEmu
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
    ".guile".source = ../dotfiles/guile/.guile;
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

  programs.firefox = {
    enable = true;
    package = (pkgs.firefox.override { extraNativeMessagingHosts = [
      # pkgs.browserpass
      pkgs.passff-host
    ]; });
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      https-everywhere
      privacy-badger
      ublock-origin
      react-devtools
      org-capture
      clearurls
      # browserpass  # not working, manually installed passff
      firefox-color
      darkreader
      cookie-autodelete
      # and manually installed ghost-text for atomic-chrome
    ];
    profiles."${user.username}" = {
      id = 0;  # implies isDefault = true
      settings = {
        "browser.startup.homepage" = "https://google.it";
        "browser.search.region" = "IT";
        "browser.search.isUS" = false;
        "distribution.searchplugins.defaultLocale" = "it-IT";
        "general.useragent.locale" = "it-IT";
        "browser.bookmarks.showMobileBookmarks" = true;
      };
      userChrome = ''
        /* Hide tab bar in FF Quantum * /
        @-moz-document url("chrome://browser/content/browser.xul") {
          #TabsToolbar {
          visibility: collapse !important;
            margin-bottom: 21px !important;
          }

          #sidebar-box[sidebarcommand="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header {
           visibility: collapse !important;
          }
        }
        '';
        userContent = "";
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

  programs.ssh = {
    enable = true;
    extraConfig = ''
      Host *
      KexAlgorithms +diffie-hellman-group1-sha1
    '';
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

  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  programs.notmuch = {
    enable = true;
    hooks = {
      preNew = "mbsync --all";
    };
  };

  accounts.email = {
    accounts.autistici = {
      address = "andrea.ciceri@autistici.org";
      gpg = {
        key = user.gpgSshKeygrip;
        signByDefault = true;
      };
      imap.host = "mail.autistici.org";
      mbsync = {
        enable = true;
        create = "maildir";
      };
      msmtp.enable = true;
      notmuch.enable = true;
      primary = true;
      realName = "Andrea Ciceri";
      signature = {
        text = ''
          La mia firma!
        '';
        showSignature = "append";
      };
      passwordCommand = "pass show autistici/password";
      smtp = {
        host = "smtp.autistici.org";
      };
      userName = "andrea.ciceri@autistici.org";
    };
  };

}

{ user, pkgs, ... }:

let
  sources = import ./sources.nix;
in
{
  home.packages = with pkgs; [
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
    #nur.repos.mic92.nixos-shell
    #nixops  # broken
    guile
    rclone
    wl-clipboard
    gcc  # gccEmacs needs its binaries in PATH

    # Not cli
    bemenu
    zathura
    qutebrowser
    tor-browser-bundle-bin
    gimp
    rawtherapee
    spotify
    spotify-adkiller
    mpv
    (calibre.override {
      unrarSupport = true;  # why doesn't work?
    })
    discord
    gnome3.adwaita-icon-theme

    citrix_workspace
    teams
    remmina
    chromium

    # DEVELOPING
    (python3.withPackages (ps: with ps; [
      pip
      black
    ]))
    yarn2nix
    yarn
    nodejs

    nomachine-client

    # Games
    cmatrix
    tmatrix
    nethack
    cataclysm-dda
    angband

    # Test
    pipewire
    xdg-desktop-portal
    xdg-desktop-portal-wlr


    gphoto2fs
  ];

  home.file = {
    ".emacs.d" = {
      recursive = true;
      source = pkgs.fetchFromGitHub {
        owner = "syl20bnr";
        repo = "spacemacs";
        rev = "bc713b194381234366aa2fe5b4246eb9958c46bc";
        sha256 = "sha256-m2BBOlKirVYsFQVNeZGR7nC9fQfZ9aTXeeyEvLPVHdM=";
      };
    };
    ".zlogin".source = ../dotfiles/zsh/.zlogin;
    ".config/qutebrowser/config.py".source = ../dotfiles/qutebrowser/config.py;
    ".config/beets/config.yaml".source = ../dotfiles/beets/config.yaml;
    ".guile".source = ../dotfiles/guile/.guile;
  };

  wayland = {
    windowManager.sway = let
      modifier = "Mod4";
    in {
      enable = true;
      config = {
        modifier = modifier;
        menu = "${pkgs.bemenu}/bin/bemenu-run -b -m 1";
        output = if user.notebook then
	      {
          LVDS-1 = {
            bg = "~/dotfiles/dotfiles/xorg/wallpaper.jpg fill";
          };
        } else
        {
          HDMI-A-2 = {
            bg = "~/dotfiles/dotfiles/xorg/wallpaper.jpg fill";
          };
        };
        fonts = [ "Font Awesome" "Fira Code" ];
        terminal = "alacritty";
        bars = [{
          command = "${pkgs.waybar}/bin/waybar";
        }];
        startup = [{
          command = "systemctl --user restart redshift";
          always = true;
        }];
        floating = {
          criteria = [
            { title = "MetaMask Notification.*"; }
          ];
        };
      };
      extraConfig = ''
        bindsym ${modifier}+p move workspace to output right
        exec systemctl --user import-environment
        exec systemctl --user start graphical-session.target
      '';
      xwayland = true;
      systemdIntegration = false;
    };
  };

  programs.waybar = {
    enable = true;
    style = builtins.readFile ../dotfiles/waybar/style.css;
    settings = [{
      layer = "top";
      position = "top";
      height = 30;
      output = if user.notebook then [
        "LVDS-1"
      ] else
      [
        "HDMI-A-2"
      ];
      modules-left = [ "sway/workspaces" "sway/mode" ];
      modules-center = [ "sway/window" ];
      modules-right = [ "cpu" "network" "mpd" "clock" ];
      modules = {
        "sway/workspaces" = {
          disable-scroll = true;
          all-outputs = false;
        };
        "network" = {
          format-ethernet = "{ipaddr} - {bandwidthDownBits} - {bandwidthUpBits}";
        };
        "cpu" = {
          interval = 1;
        };
        "clock" = {
          format = "{:%d %b %H:%M}";
        };
      };
    }];
  };

  services.redshift = {
    enable = true;
    temperature = {
      day = 6500;
      night = 3700;
    };
    provider = "geoclue2";
    package = pkgs.redshift-wlr;
  };

  programs.alacritty = {
    enable = true;
    settings = {
      font.normal.family = "Fira Code";
      background_opacity = 0.85;
    };
  };

  programs.direnv = {
    enable = true;
    enableNixDirenvIntegration = true;
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
      {
        name = "zsh-nix-shell";
        file = "nix-shell.plugin.zsh";
        src = pkgs.zsh-nix-shell;
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
      "em" = "emacsclient -c";
      "emnw" = "emacsclient -c -nw";
    };
    localVariables = {
      SPACESHIP_TIME_SHOW = "true";
      SPACESHIP_USER_SHOW = "always";
      SPACESHIP_HOST_SHOW = "always";
      EDITOR = "emacsclient";
      NIX_BUILD_SHELL = "${pkgs.zsh-nix-shell}/scripts/buildShellShim.zsh";
      PROMPT = "\\\${IN_NIX_SHELL:+[nix-shell] }$PROMPT";
    };
  };

  programs.vscode = {
    enable = true;
    package = pkgs.vscode;
  };

  programs.browserpass = {
    enable = true;
    browsers = ["firefox"];
  };

  programs.firefox = {
    enable = true;
    #package = (pkgs.firefox.override { extraNativeMessagingHosts = [
    #  pkgs.browserpass
      # pkgs.passff-host
    #]; });
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      #https-everywhere
      #privacy-badger
      #ublock-origin
      #react-devtools
      #org-capture
      #clearurls
      browserpass  # not working, manually installed passff
      #firefox-color
      #darkreader
      #cookie-autodelete
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
        "browser.download.folderList" = 2;
        "browser.download.lastDir" = "/home/${user.username}/downloads/";
      };
      userChrome = ''
        /* Hide tab bar in FF Quantum * /
        @-moz-document url("chrome://browser/content/browser.xul") {
          #TabsToolbar {
          visibility: collapse !important;
            margin-bottom: 21px !emportant;
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

  programs.ncmpcpp = {
    enable = true;
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

  home.sessionVariables = {
    MOZ_ENABLE_WAYLAND = 1;
    XDG_CURRENT_DESKTOP = "sway";
    XDG_SESSION_TYPE = "wayland";
  };

}

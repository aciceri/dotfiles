{self, super}:
  super.pkgs.writeScriptBin "spotify-adkiller"
        ''
        #!${super.pkgs.stdenv.shell}

        status=unmuted
        delta=1

        is_spotify_running() {
            [ "$(ps -A | grep 'spotify')" ] && return 0 || return 1
        }

        is_playing_ad() {
            [ "$1" = "Advertisement" -o "$1" = "Spotify" ] && return 0 || return 1
        }

        if is_spotify_running 
        then
            echo "Detected running Spotify"
        else    
            echo "Starting Spotify..."
            ${super.pkgs.spotify}/bin/spotify > /dev/null 2>&1 &
            sleep 1
        fi

        for (( ; ; ))
        do
            if ! is_spotify_running      
            then
        echo "Spotify has been closed"
        exit 0
            fi

            title=$(${self.pkgs.playerctl}/bin/playerctl metadata --format "{{title}}")
            now=$(date "+%H:%m:%S")

            if is_playing_ad "$title" 
            then
                ${super.pkgs.alsaUtils}/bin/amixer  sset Master mute > /dev/null 2>&1
                status=muted
            else
                ${super.pkgs.alsaUtils}/bin/amixer sset Master unmute > /dev/null 2>&1
                status=unmuted
            fi

            echo -n -e "\e[95m$now\e[0m — "
            [ "$status" = "muted" ] && echo -n -e "system is \e[31m\e[1mMuted\e[0m"
            [ "$status" = "unmuted" ] && echo -n -e "system is \e[32m\e[1mUnmuted\e[0m"
            echo -e " ➜ Playing \"$title\""

            sleep $delta
        done
        ''

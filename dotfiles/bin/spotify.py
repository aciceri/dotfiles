#!/usr/bin/env python3


import dbus
from subprocess import run

def mute():
    print("Muted!")
    run(['amixer sset Master mute'], shell=True)

def unmute():
    print("Unmuted!")
    run(['amixer sset Master unmute'], shell=True)

run(["spotify&"], shell=True)
run(["sleep 3"], shell=True)

session_bus = dbus.SessionBus()
bus_data = ("org.mpris.MediaPlayer2.spotify", "/org/mpris/MediaPlayer2")
spotify_bus = session_bus.get_object(*bus_data)
interface = dbus.Interface(spotify_bus, "org.freedesktop.DBus.Properties")

def getSpotifyTitle():
    return interface.Get("org.mpris.MediaPlayer2.Player", "Metadata").get("xesam:title")

muted = False

while True:
    try:
        if muted == False and getSpotifyTitle() in ["Advertisement", "Spotify"]:
            mute()
            muted = True
        elif muted == True and not getSpotifyTitle() in ["Advertisement", "Spotify"]:
            unmute()
            muted = False

    except dbus.exceptions.DBusException:
        print("Spotify has closed")
        break

    run(['sleep 1'], shell=True)



if test -z "$DBUS_SESSION_BUS_ADDRESS"; then
    eval $(dbus-launch --exit-with-session --sh-syntax)
fi

systemctl --user import-environment $DISPLAY $XAUTHORITY

if command -v dbus-update-activation-environment >/dev/null 2>&1; then
    dbus-update-activation-environment $DISPLAY $XAUTHORITY
fi

xsetroot -cursor_name left_ptr

exec xcalib dotfiles/dotfiles/xorg/profile.icc &
exec redshift -l geoclue2 -t 6500:3700 -b 1:1 &

exec dbus-launch --exit-with-session emacs -c -F "'(fullscreen . maximized)"

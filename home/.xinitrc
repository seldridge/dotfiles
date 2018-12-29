# Try using gnome-keyring
dbus-update-activation-environment --systemd DISPLAY

# Possibly needed to get dunst working
eval $(dbus-launch --sh-syntax --exit-with-session)

# reread .Xresource
xrdb $HOME/.Xresources

# start xbindkeys
xbindkeys

xset -b

urxvtd -q -o -f

export TERMINAL=urxvtc
exec i3
# exec xmonad

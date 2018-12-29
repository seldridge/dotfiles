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

xautolock \
  -time 10 \
  -notify 30 \
  -notifier "notify-send -u critical -t 5000 'Screen lock in 30 seconds...'" \
  -locker "$HOME/usr/bin/lock-pixelize" &

export TERMINAL=urxvtc
# exec i3
exec xmonad

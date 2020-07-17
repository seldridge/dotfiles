# Try using gnome-keyring
# dbus-update-activation-environment --systemd DISPLAY

# Possibly needed to get dunst working
# eval $(dbus-launch --sh-syntax --exit-with-session)

# . /etc/X11/xinit/xinitrc

# reread .Xresource
xrdb $HOME/.Xresources
xmodmap $HOME/.Xmodmap

# start xbindkeys
xbindkeys

xset -b

urxvtd -q -o -f

xautolock \
  -time 10 \
  -notify 30 \
  -notifier "notify-send -u critical -t 5000 'Screen lock in 30 seconds...'" \
  -locker "$HOME/usr/bin/lock-pixelize" &

stalonetray &

if [ -x /opt/ibm/ibmsam/bin/ibmsaml ]; then
  /opt/ibm/ibmsam/bin/ibmsaml &
fi

if [ -x /opt/ibm/c4eb/wst/bin/wst-applet ]; then
  /opt/ibm/c4eb/wst/bin/wst-applet &
fi

if [ -x /usr/bin/nm-applet ]; then
   nm-applet --sm-disable &
fi

if [ -x /usr/bin/blueman-applet ]; then
  blueman-applet &
fi

if [ -x /usr/bin/dunst ]; then
  dunst &
fi

export TERMINAL=urxvtc
export TERM=rxvt-unicode-256color

exec xmonad

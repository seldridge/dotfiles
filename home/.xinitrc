# copied from arch wiki on ratpoison:
#  https://wiki.archlinux.org/index.php/Ratpoison
# xsetroot -solid black &
# xcompmgr -c -f -D 5 &
# exec /usr/bin/ratpoison

# WALLPAPER=$HOME/wallpaper/wallpaper-54773.png

# reread .Xresource
xrdb $HOME/.Xresources

# set the background
# rm -f ~/wallpaper/wallpaper
# ln -s $WALLPAPER ~/wallpaper/wallpaper
# feh --bg-fill $WALLPAPER

# start xbindkeys
xbindkeys

xset -b

# unclutter --timeout 1 -b

urxvtd -q -o -f

# Try using gnome-keyring
dbus-update-activation-environment --systemd DISPLAY
# eval $(gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
# echo $SSH_AUTH_SOCK
# export SSH_AUTH_SOCK

# Possibly needed to get dunst working
eval $(dbus-launch --sh-syntax --exit-with-session)

# i3 configuration
export TERMINAL=urxvtc
exec i3

# copied from arch wiki on ratpoison:
#  https://wiki.archlinux.org/index.php/Ratpoison
# xsetroot -solid black &
# xcompmgr -c -f -D 5 &
# exec /usr/bin/ratpoison

WALLPAPER=~/wallpaper/wallpaper-54773.png

# reread .Xresource
xrdb ~/.Xresources

# set the background
rm -f ~/wallpaper/wallpaper
ln -s $WALLPAPER ~/wallpaper/wallpaper
feh --bg-fill $WALLPAPER

# start xbindkeys
xbindkeys

xset -b

unclutter --timeout 1 -b

urxvtd -q -f

# i3 configuration
exec i3

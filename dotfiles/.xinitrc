#export QT_DEVICE_PIXEL_RATIO=2
#export QT_SCALE_FACTOR=2
export GDK_SCALE=2
#export GDK_DPI_SCALE=0.5
xrandr --dpi 192
xrdb -merge ~/.Xresources
xmodmap ~/.Xmodmap
exec i3
#exec sway
#exec xmonad
#exec gnome-session

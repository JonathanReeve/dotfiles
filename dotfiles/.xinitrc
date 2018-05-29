#export QT_DEVICE_PIXEL_RATIO=2
#export QT_SCALE_FACTOR=2
#export GDK_SCALE=2
#export GDK_DPI_SCALE=0.5
#xrandr --dpi 192
#xrdb -merge ~/.Xresources
#xrdb -merge ~/.extend.Xresources
#xmodmap ~/.Xmodmap
#setxkbmap -layout us,us -variant colemak,altgr-intl -option caps:escape,grp:alt_shift_toggle
#exec i3
#exec sway
#exec xmonad
#exec gnome-session
#export SXHKD_SHELL=/usr/bin/sh
#sxhkd &
#exec bspwm
#exec polybar main_bar
#exec gnome-session
export PLASMA_USE_QT_SCALING=1
exec startkde

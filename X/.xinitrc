# Enable for Japanese support
#export GTK_IM_MODULE=fcitx
#export QT_IM_MODULE=fcitx
#export XMODIFIERS="@im=fcitx"
#fcitx

# General Environmental Setup
autocutsel &
xset +fp /usr/share/fonts/local
xset fp rehash
xset +fp ~/.fonts
xset fp rehash
xrdb -merge .Xresources
hsetroot -solid "#282828"
xsetroot -cursor_name left_ptr
# setxkbmap -option caps:swapescape
dunst &
rm /tmp/statuspipe.fifo && mkfifo /tmp/statuspipe.fifo
~/bin/pipestatus/battery.sh &
exec xmonad
# exec startxfce4


## Disable access control
#xhost +SI:localuser:$USER
#
## Themes, etc
#gnome-settings-daemon &
#
## Fallback cursor
#xsetroot -cursor_name left_ptr
#
## Keyboard repeat rate
#xset r rate 200 60
#
## Start Emacs
#exec dbus-launch --exit-with-session emacs

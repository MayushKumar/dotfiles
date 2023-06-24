#!/bin/sh

pgrep -x sxhkd > /dev/null || sxhkd -c ~/.config/awesome/sxhkdrc &
dunst &
#nm-applet &
feh --no-fehbg --bg-fill $(cat ~/.config/wallpaper)
picom &
unclutter &
easyeffects --gapplication-service &
# transmission-daemon
#pgrep -f "emacs --daemon" || emacs --daemon &
#~/.config/polybar/launch.sh

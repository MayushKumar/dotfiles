#!/bin/sh

pgrep -x sxhkd > /dev/null || sxhkd -c ~/.config/awesome/sxhkdrc &
dunst &
feh --no-fehbg --bg-fill $(cat ~/.config/wallpaper)
picom &
brillo -I
unclutter &
easyeffects --gapplication-service &
# transmission-daemon
#pgrep -f "emacs --daemon" || emacs --daemon &

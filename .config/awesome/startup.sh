#!/bin/sh

brillo -I
pgrep -x sxhkd > /dev/null || sxhkd -c ~/.config/awesome/sxhkdrc &
unclutter &
dunst &
feh --no-fehbg --bg-fill $(cat ~/.config/wallpaper)
easyeffects --gapplication-service &
picom &
# transmission-daemon
#pgrep -f "emacs --daemon" || emacs --daemon &

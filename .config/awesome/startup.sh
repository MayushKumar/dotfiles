#!/bin/sh

pgrep -x sxhkd > /dev/null || sxhkd -c ~/.config/awesome/sxhkdrc &
dunst &
#nm-applet &
~/.fehbg 
picom --experimental-backends &
unclutter &
pgrep -f "emacs --daemon" || emacs --daemon &
#~/.config/polybar/launch.sh

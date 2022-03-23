#!/bin/sh

pgrep -x sxhkd > /dev/null || sxhkd -c ~/.config/awesome/sxhkdrc &
dunst &
#nm-applet &
~/.fehbg 
picom --experimental-backends &
unclutter &
#~/.config/polybar/launch.sh

#!/bin/sh

pgrep -x sxhkd > /dev/null || sxhkd -c ~/.config/awesome/sxhkdrc &
dunst &
#nm-applet &
nitrogen --restore
picom --experimental-backends &
unclutter &
#~/.config/polybar/launch.sh

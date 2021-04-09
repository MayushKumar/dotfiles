#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
# PS1='[\u@\h \W]\$ '
PS1="\[\e[36m\]\u\[\e[m\] > "

set -o vi
alias config='/usr/bin/git --git-dir=/home/mayush/.cfg/ --work-tree=/home/mayush'
alias ch='cd /run/media/Storage/Personal/Dev/C++/Charcoal\ Engine/'

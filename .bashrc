#
# ~/.bashrc
#

vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

export EDITOR=nvim
export PATH=$PATH:/home/mayush/.local/bin

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='exa'
alias la='exa -al'

PS1="\[\e[36m\]\u\[\e[m\] > "

set -o vi

alias config='/usr/bin/git --git-dir=/home/mayush/.cfg/ --work-tree=/home/mayush'
alias ch='cd /run/media/Storage/Personal/Dev/C++/Charcoal\ Engine/'

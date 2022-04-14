# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/mayush/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

fpath+=~/.local/share/zsh/pure

autoload -Uz promptinit
promptinit

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

setopt auto_cd

alias ls='exa'
alias la='exa -al'
alias ec='emacsclient -r -a ""'

alias config='/usr/bin/git --git-dir=/home/mayush/.cfg/ --work-tree=/home/mayush'
alias ch='cd /run/media/Storage/Personal/Dev/C++/Charcoal\ Engine/'
alias nr='cd /run/media/Storage/Personal/Dev/C++/Noor/'
alias dw='cd /run/media/Storage/Personal/Downloads/'
alias dc='cd /run/media/Storage/Personal/Documents/'

prompt pure

source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source /usr/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh

bindkey "^?" backward-delete-char
bindkey "^[[Z" autosuggest-accept

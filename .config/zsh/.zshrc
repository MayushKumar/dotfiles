# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Lines configured by zsh-newuser-install
HISTFILE=~/.cache/zsh/histfile
HISTSIZE=10000
SAVEHIST=10000
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
alias lf='lfrun'
alias ping='prettyping'
# alias valgrind='colour-valgrind'
alias dlp='aria2c -x 6 -s 6'
alias ff='fastfetch'

alias config='/usr/bin/git --git-dir=/home/mayush/.cfg/ --work-tree=/home/mayush'
alias nr='cd ~/dev/Noor/'
alias dw='cd ~/Downloads/'
alias dc='cd ~/Documents/'
alias dv='cd ~/dev/'
alias sx='startx ~/.config/X11/xinitrc'
alias ns='niri --session'
alias dns-check='cat /etc/resolv.conf | grep nameserver'

function y() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
	command yazi "$@" --cwd-file="$tmp"
	IFS= read -r -d '' cwd < "$tmp"
	[ "$cwd" != "$PWD" ] && [ -d "$cwd" ] && builtin cd -- "$cwd"
	rm -f -- "$tmp"
}



# source /usr/share/zsh/plugins/zsh-autocomplete/zsh-autocomplete.plugin.zsh 
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source /usr/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
source /usr/share/zsh/plugins/zsh-vi-mode/zsh-vi-mode.plugin.zsh

bindkey "^?" backward-delete-char
bindkey "^[[Z" autosuggest-accept

eval "$(zoxide init zsh)"
source <(fzf --zsh)


eval "$(starship init zsh)"

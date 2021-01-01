# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Lines configured by zsh-newuser-install
HISTFILE=~/.cache/zsh/histfile
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
alias lf='lfrun'
alias valgrind='colour-valgrind'
alias dlp='aria2c -x 6 -s 6'
alias ff='fastfetch'

alias config='/usr/bin/git --git-dir=/home/mayush/.cfg/ --work-tree=/home/mayush'
alias nr='cd ~/dev/Noor/'
alias dw='cd ~/Downloads/'
alias dc='cd ~/Documents/'
alias dv='cd ~/dev/'
alias tr='cd ~/Downloads/torrents/'
alias sx='startx ~/.config/X11/xinitrc'

#prompt pure

source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source /usr/share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
source ~/.local/share/zsh/powerlevel10k/powerlevel10k.zsh-theme

bindkey "^?" backward-delete-char
bindkey "^[[Z" autosuggest-accept

eval $(opam env)
eval "$(zoxide init zsh)"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh


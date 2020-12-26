#!/bin/bash
PS1='[\W]$ '
shopt -s checkwinsize

SOURCES=$HOME/conf/scripts/bashrc-sources
source "$SOURCES"/git-completion-bash

# ===================
# ALIASES & FUNCTIONS
# ===================
# misc
alias df='df -h'
alias du='du -h'
alias grep='egrep --color=auto'
alias poogle='ping www.google.com'
# restart
alias rebash='source ~/.bashrc'
# ls
alias ls='ls --color=auto -p'
alias ll='ls -lh'
alias la='ls -a'
alias lal='ls -lah'
# why?
alias al='sl -a'
alias lll='sl -l'

alias R="R --no-save"

alias op="exo-open"

toup(){ touch "$1" && op "$1";}
mkcd (){ mkdir -p "$*"; cd "$*";}

# yaourt reminders
alias yaoupg='echo yay -Syu'
alias yaoinst='echo yay -S'
alias yaoreps='echo yay -Ss'
alias yaorem='echo yay -Rns'

# Toggle novartis proxy.
lekproxy () {
    if [ -e "$http_proxy" ]; then
        echo "lekproxy on"
        export {http,https,ftp}_proxy='http://simg-proxy.eu.novartis.net:2010'
    else
        echo "lekproxy off"
        unset {http,https,ftp}_proxy
    fi
}

PROMPT_COMMAND='printf "\033]0;%s\007" "${PWD/#$HOME/"~"}"'

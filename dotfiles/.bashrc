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
alias grep='egrep'
alias tcpy='pwd | urxvtc'
alias poogle='ping www.google.com'
# restart
alias rebash='source ~/.bashrc'
alias remacs='emacsclient -e "(kill-emacs)"; emacs --daemon'
# ls
alias ls='ls --color=auto -p'
alias ll='ls -lh'
alias la='ls -a'
alias lal='ls -lah'
# why?
alias al='sl -a'
alias lll='sl -l'

alias grep='grep --color=auto'

alias R="R --no-save"

alias op="exo-open"

toup(){ touch "$1" && op "$1";}

mkcd (){ mkdir -p "$*"; cd "$*";}
cs (){ cd "$*"; ls;}

# yaourt
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

lfcd () {
    tmp="$(mktemp)"
    /bin/lf -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp"
        if [ -d "$dir" ]; then
            if [ "$dir" != "$(pwd)" ]; then
                cd "$dir"
            fi
        fi
    fi
}

# ====
# VARS
# ====
export EDITOR="emacsclient -c"
export PAGER=vimpager
export PATH=$PATH:$HOME/conf/bin

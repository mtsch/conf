#!/bin/sh
PS1='[\W]$ '
shopt -s checkwinsize

SOURCES=$HOME/conf/scripts/bashrc-sources
ls $SOURCES/git-completion-bash
source $SOURCES/git-completion-bash

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
alias lf='ls -f'
alias la='ls -a'
alias lal='ls -lah'
# why?
alias al='sl -a'
alias lll='sl -l'

alias R="R --no-save"

alias op="exo-open"
alias op.="exo-open ."
alias op..="exo-open .."

toup(){ touch $1 && op $1;}

mkcd (){ mkdir -p "$*"; cd "$*";}
cs (){ cd "$*"; ls;}

# yaourt
alias yaoupg='yaourt -Syu'
alias yaoinst='yaourt -S'
alias yaoinstnc='yaourt -S --noconfirm'
alias yaoreps='yaourt -Ss'
alias yaocre='yaourt -R'
alias yaorem='yaourt -Rns'

# Toggle novartis proxy.
function lekproxy() {
    if [ -e $http_proxy ]; then
        echo "lekproxy on"
        export {http,https,ftp}_proxy='http://simg-proxy.eu.novartis.net:2010'
    else
        echo "lekproxy off"
        unset {http,https,ftp}_proxy
    fi
}

# ====
# VARS
# ====
export EDITOR="emacsclient -c"
export PAGER=vimpager
export PATH=$PATH:$HOME/conf/bin

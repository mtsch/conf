#!/bin/sh
PS1='[\W]$ '
shopt -s checkwinsize

# additional sources - aliases, variables and scripts
SOURCES=~/conf/scripts/bashrc-sources
source $SOURCES/git-completion-bash
source $SOURCES/bashmarks.sh

# added by travis gem
[ -f /home/m/.travis/travis.sh ] && source /home/m/.travis/travis.sh

# ======================================================================================== #
# VARS
# ======================================================================================== #
export EDITOR="emacsclient -c"
export PAGER=vimpager
export PATH=$PATH:/home/m/conf/bin:/home/m/.cabal/bin:/usr/lib/smlnj/bin
export SMLNJ_HOME=/usr/lib/smlnj/
export PYTHONPATH=$PYTHONPATH:/home/m/git-stuff/gudhi/build/cython

# JULIA
export CONDA_JL_VERSION=2
export JULIA_NUM_THREADS=4

source /usr/bin/thisroot.sh

# ======================================================================================== #
# ALIASES & FUNCTIONS
# ======================================================================================== #
# misc
alias df='df -h'
alias whereis='find . -iname'
alias grep='egrep'
alias tcpy='pwd | urxvtc'
alias poogle='ping www.google.com'
alias clr='clear'
# restart
alias rebash='source ~/.bashrc'
alias remonad='xmonad --recompile && xmonad --restart'
alias remacs='emacsclient -e "(kill-emacs)"; emacs --daemon'
# ls
alias ls='ls --color=auto -p'
alias ll='ls -la'
alias lf='ls -f'
alias la='ls -a'
# why?
alias al='sl -a'
alias lll='sl -l'
# rlwrap
alias ocaml='rlwrap ocaml'
alias octave=octave-cli
# emacs client in terminal
alias em="emacsclient -t"
alias R="R --no-save"

alias op="exo-open"

toup(){
    touch $1 && op $1
}
steam(){
    LD_PRELOAD='/usr/$LIB/libstdc++.so.6 /usr/$LIB/libgcc_s.so.1 /usr/$LIB/libxcb.so.1 /usr/$LIB/libgpg-error.so' /bin/steam
}
mkcd (){ mkdir -p "$*"; cd "$*";}
cs (){ cd "$*"; ls;}

# yaourt
alias yaoupg='yaourt -Syu'
alias yaoinst='yaourt -S'
alias yaoinstnc='yaourt -S --noconfirm'
alias yaoreps='yaourt -Ss'
alias yaocre='yaourt -R'
alias yaorem='yaourt -Rns'

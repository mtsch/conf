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
alias grep='grep -E --color=auto'
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

#alias op="exo-open"
op() { setsid ${OPENER:-exo-open} "$@" & disown; }
opc() { setsid ${OPENER:-exo-open} "$@" & disown; exit; }

toup(){ touch "$1" && op "$1";}
mkcd (){ mkdir -p "$*"; cd "$*";}

PROMPT_COMMAND='printf "\033]0;%s\007" "${PWD/#$HOME/"~"}"'

export HISTSIZE=-1
export HISTFILESIZE=-1

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/m/miniconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/m/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/m/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/m/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
  
conda deactivate

export PYTHONPATH=$PYTHONPATH:/home/m/wrk/aqmc/ad_afqmc

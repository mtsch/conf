#!/bin/bash
[[ -f ~/.bashrc ]] && . ~/.bashrc
export INPUTRC=~/.inputrc

export EDITOR="emacsclient -c"
export PAGER=vimpager
export PATH=$PATH:$HOME/conf/bin

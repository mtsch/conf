#!/bin/bash
[[ -f ~/.bashrc ]] && . ~/.bashrc
export INPUTRC=~/.inputrc

export EDITOR="emacsclient -c"
export PATH=$PATH:$HOME/conf/bin:$HOME/.local/bin:$HOME/miniconda3/bin

export XDG_DESKTOP_DIR="$HOME/desktop"
export XDG_DOCUMENTS="$HOME/docs"
export XDG_PICTURES_DIR="$HOME/pictures"
export XDG_DOWNLOAD_DIR="$HOME/downloads"

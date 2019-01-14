#!/bin/sh
PS1='[\W]$ '
shopt -s checkwinsize

# If not running interactively, don't do anything
# [[ $- != *i* ]] && return

# additional sources - aliases, variables and scripts
SETTINGDIR=~/conf/scripts/bashrc-sources
source $SETTINGDIR/git-completion-bash
source $SETTINGDIR/bashmarks.sh
source $SETTINGDIR/vars
source $SETTINGDIR/aliases-misc
source $SETTINGDIR/aliases-t
source $SETTINGDIR/aliases-pacman-yaourt
source $SETTINGDIR/functions

# added by travis gem
[ -f /home/m/.travis/travis.sh ] && source /home/m/.travis/travis.sh

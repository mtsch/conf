#!/bin/sh
PS1='[\W]$ '
shopt -s checkwinsize

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# additional sources - aliases, variables and scripts
SETTINGDIR=~/conf/scripts/bashrc-source
source $SETTINGDIR/git-completion-bash
source $SETTINGDIR/bashmarks.sh
source $SETTINGDIR/vars
source $SETTINGDIR/aliases-misc
source $SETTINGDIR/aliases-t
source $SETTINGDIR/aliases-pacman-yaourt

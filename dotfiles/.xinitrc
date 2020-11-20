#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# Keyboard stuff
setxkbmap us
setxkbmap -option compose:ralt
setxkbmap -option ctrl:nocaps
syndaemon -t -k -i 1 -d & # disable touchpad when typing
xset -b # disable beep

# Daemons
emacs --daemon &
urxvtd &
redshift -l -36.8:174.8 -t 6000:3500 &

picom -b --config .picom.conf
startxfce4
#xmonad --replace &
#sleep 1
#xfce4-panel --restart

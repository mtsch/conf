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
redshift -l -36.8:174.8 -t 6000:3500 &
picom -b --config .picom.conf &
feh --bg-fill ~/conf/wallpapers/solarized-carpet.jpg

# Tray stuff
#trayer --align right --width 5 --transparent true --alpha 0 --tint 0x001E26 --edge top &
#cadence-session --maybe-system-start &
#cadence &
#nm-applet &
#/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
#xfce4-power-manager &
#pasystray --notify=all &
#/usr/lib/xfce4/notifyd/xfce4-notifyd &

# WM
exec startxfce4
#exec xmonad

#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

export QT_QPA_PLATFORMTHEME=gtk2

xrdb ~/.Xresources

# Keyboard stuff
syndaemon -t -k -i 1 -d # disable touchpad when typing
xset -b # disable beep

# Daemons
emacs --daemon
redshift -l -36.8:174.8 -t 6000:3500 &
/usr/lib/xfce4/notifyd/xfce4-notifyd &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
~/.dropbox-dist/dropboxd &

# Tray stuff
trayer --align right --width 10 --transparent true --alpha 0 --tint 0x001E26 --edge top &
xfce4-power-manager --daemon
nm-applet &
pasystray --notify=sink_default &
cadence &
lxappearance &

# Recompile xmonad in case it was updated recently
xmonad --recompile && exec xmonad

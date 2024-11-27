#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

export QT_QPA_PLATFORMTHEME=gtk2

setxkbmap -option compose:ralt

xrdb ~/.Xresources

# Keyboard stuff
xset -b # disable beep
xmodmap ~/.Xmodmap

# Daemons
emacs --daemon &
# NZ
redshift -l -36.8:174.8 -t 6000:3500 &
# LJ
# redshift -l -46.05:14.505 -t 6000:3500 &
xfce4-power-manager &
/usr/lib/xfce4/notifyd/xfce4-notifyd &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
~/.dropbox-dist/dropboxd &

# Tray stuff
trayer --align right --width 5 --transparent true --alpha 0 --tint 0x001E26 --edge top &
nm-applet &
conky &
pasystray --notify=sink_default &

source ~/.screenlayout/default.sh

# Recompile xmonad in case it was updated recently
#xmonad --recompile && exec xmonad
exec xmonad


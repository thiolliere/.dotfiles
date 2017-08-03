setterm -powerdown 0
xset s off
xset s noblank
xset -dpms
urxvtd --quiet --opendisplay --fork
xss-lock -- slock &
stalonetray &
nm-applet &
lxpolkit &
exec xmonad

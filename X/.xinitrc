xmodmap ~/.Xmodmap
[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources
xrandr --output DVI-D-1 --mode 1920x1080 --rate 60.0 --output DP-1 --mode 3840x2160 --rate 60.0 --primary --right-of DVI-D-1 
exec xmonad
 


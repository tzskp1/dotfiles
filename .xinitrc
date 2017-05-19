xmodmap ~/.Xmodmap
xrandr --output HDMI-A-1 --mode 1920x1080 --rate 60.0 --output DVI-D-0 --mode 1920x1080 --rate 60.0 --primary --right-of HDMI-A-1
exec xmonad

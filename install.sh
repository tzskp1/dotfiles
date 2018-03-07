#!/bin/sh

cud=$(pwd)

if [ -f ~/.zshenv ]; then
	mv ~/.zshenv ~/.zshenv.bak 
fi

cat << EOF > ~/.zshenv
#! /usr/bin/env zsh 
# -*- mode: sh ; coding: utf-8 -*- 

#--- copy to each machine ---
export ZDOTDIR=$cud/zsh
source \${ZDOTDIR}/.zshenv
#---------------------------- " 
EOF

if [ -e ~/.emacs.d ]; then
	mv ~/.emacs.d ~/.emacs.d.bak
fi
rm ~/.emacs.d
ln -s $cud/emacs ~/.emacs.d

if [ -e ~/texmf ]; then
	mv ~/texmf ~/texmf.bak
fi
ln -s $cud/texmf ~/texmf 

if [ -e ~/.xmonad ]; then
	mv ~/.xmonad ~/.xmonad.bak
fi
rm ~/.xmonad
ln -s $cud/xmonad ~/.xmonad

cd /tmp
rm -rf ~/.icons
git clone https://github.com/gmmeyer/awesome-dangerzone
mv /tmp/awesome-dangerzone/icons ~/.icons
rm -rf /tmp/awesome-dangerzone

mkdir ~/.config
rm -rf ~/.config/nvim
ln -s $cud/nvim ~/.config/nvim

rm -rf ~/.config/peco
ln -s $cud/peco ~/.config/peco

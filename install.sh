#!/bin/sh

nh=$(pwd)

if [ -f ~/.zshenv ]; then
	mv ~/.zshenv ~/.zshenv.bak 
fi

echo "#! /usr/bin/env zsh 
# -*- mode: sh ; coding: utf-8 -*- 

#--- copy to each machine ---
export ZDOTDIR=$nh/.zsh.d
source \${ZDOTDIR}/.zshenv
#---------------------------- " > ~/.zshenv

if [ -e ~/.emacs.d ]; then
	mv ~/.emacs.d ~/.emacs.d.bak
fi
ln -s $nh/.emacs.d ~/

if [ -e ~/texmf ]; then
	mv ~/texmf ~/texmf.bak
fi
ln -s $nh/texmf ~/texmf 

if [ -e ~/.xmonad ]; then
	mv ~/.xmonad ~/.xmonad.bak
fi
ln -s $nh/.xmonad ~/

cd ~
git clone https://github.com/gmmeyer/awesome-dangerzone
mv ./awesome-dangerzone/icons ./.icons
rm -rf ./awesome-dangerzone

mkdir ~/.config
ln -s $nh/nvim ~/.config/nvim

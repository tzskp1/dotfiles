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
	sudo mv ~/texmf ~/texmf.bak
fi
sudo ln -s $nh/texmf ~/texmf 

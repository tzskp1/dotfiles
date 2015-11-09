#!/bin/sh

nh=$(pwd)

if [ -f ~/.zshenv ]; then
echo "#--- copy to each machine ---
export ZDOTDIR=$nh/.zsh.d
source \${ZDOTDIR}/.zshenv
#---------------------------- " >> ~/.zshenv
else
echo "#! /usr/bin/env zsh 
# -*- mode: sh ; coding: utf-8 -*- 

#--- copy to each machine ---
export ZDOTDIR=$nh/.zsh.d
source \${ZDOTDIR}/.zshenv
#---------------------------- " > ~/.zshenv
fi

if [ -e ~/.emacs.d ]; then
	rm -rf ~/.emacs.d 
fi
ln -s $nh/.emacs.d ~/

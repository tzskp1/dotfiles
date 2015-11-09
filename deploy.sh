#!/bin/sh

nh=$(pwd)
if [ -f ~/.zshenv ]; then
echo "#--- copy to each machine ---
export ZDOTDIR=$nh/.zsh.d
source ${ZDOTDIR}/.zshenv
#---------------------------- " >> ~/.zshenv
elif
echo "#! /usr/bin/env zsh 
# -*- mode: sh ; coding: utf-8 -*- 

#--- copy to each machine ---
export ZDOTDIR=$nh/.zsh.d
source ${ZDOTDIR}/.zshenv
#---------------------------- " > ~/.zshenv
fi

ln -s $nh/.emacs.d ~/.emacs.d

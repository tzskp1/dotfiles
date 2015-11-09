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

if [ -e /usr/local/texlive/texmf-local ]; then
	mv /usr/local/texlive/texmf-local /usr/local/texlive/texmf-local.bak
fi
ln -s $nh/texmf-local /usr/local/texlive/

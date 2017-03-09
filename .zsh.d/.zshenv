#! /usr/bin/env zsh
# -*- mode: sh ; coding: utf-8 -*-

#--- copy to each machine ---
# export ZDOTDIR=${HOME}/Dropbox/Dotfiles/.zsh.d
# source ${ZDOTDIR}/.zshenv
#----------------------------
export GOPATH=$HOME/go
export GOROOT=/usr/local/go
export PATH=$HOME/.cabal/bin:$GOROOT/bin:$PATH
export EDITOR="${ZDOTDIR}/em@csclient.sh"
#export EDITOR="emacsclient"  # until zsh 5.2
export WAREHOUSE=192.168.0.7
export WAREHOUSE_ADDR=40:16:7e:27:bd:83
export TERM="xterm-256color"
export LANG=ja_JP.UTF-8  # 文字コードをUTF-8に設定
export KCODE=u           # KCODEにUTF-8を設定
export AUTOFEATURE=true  # autotestでfeatureを動かす

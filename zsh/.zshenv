#! /usr/bin/env zsh
# -*- mode: sh ; coding: utf-8 -*-

export GOPATH=$HOME/go
export PATH=/usr/local/texlive/2017/bin/x86_64-linux:$PATH
export PATH=$HOME/go/bin:$DOTDIR/bin:$HOME/.cabal/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PAGER="most"
export EDITOR="nvim"
export TERM="xterm-256color"
export LANG=en_US.UTF-8
export KCODE=u  
export LESSKEY=$HOME/.lesskey
export LESSOPEN="| src-hilite-lesspipe.sh %s"
export LESS='-R'
export ANDROID_HOME=$HOME'/Android/Sdk'
# Have less display colours
# from: https://wiki.archlinux.org/index.php/Color_output_in_console#man
export LESS_TERMCAP_mb=$'\e[1;31m'     # begin bold
export LESS_TERMCAP_md=$'\e[1;33m'     # begin blink
export LESS_TERMCAP_so=$'\e[01;44;37m' # begin reverse video
export LESS_TERMCAP_us=$'\e[01;37m'    # begin underline
export LESS_TERMCAP_me=$'\e[0m'        # reset bold/blink
export LESS_TERMCAP_se=$'\e[0m'        # reset reverse video
export LESS_TERMCAP_ue=$'\e[0m'        # reset underline
export GROFF_NO_SGR=1                  # for konsole and gnome-terminal
export MANPAGER='less -s -M +Gg'
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

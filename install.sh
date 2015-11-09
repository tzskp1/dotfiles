#!/bin/sh

nh=$(pwd)
apt-get install zsh

# emacs 24.4
wget -O-  http://ftp.gnu.org/gnu/emacs/emacs-24.4.tar.xz | tar xJvf -
cd emacs-24.4
./configure && make install

# login shell
chsh -s /usr/bin/zsh

source ./deploy.sh



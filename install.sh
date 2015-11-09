#!/bin/sh
function install_emacs(){
	wget -O-  http://ftp.gnu.org/gnu/emacs/emacs-24.4.tar.xz | tar xJvf -
	cd emacs-24.4
	./configure && make install
}

nh=$(pwd)
yes | sudo apt-get install zsh build-essential
yes | sudo apt-get build-dep emacs24

# emacs 24.4
which emacs > /dev/null 2>&1
if [ $? -eq 0 ]; then
	str=$(emacs --version | grep -e "24.4" | awk '{print 1}')
	if [ ! $str ]; then
		install_emacs
	fi
else
	install_emacs
fi

# login shell
chsh -s /usr/bin/zsh

source ./deploy.sh



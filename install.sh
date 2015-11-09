#!/bin/sh
install_emacs() {
	wget -O-  http://ftp.gnu.org/gnu/emacs/emacs-24.4.tar.xz | tar xJvf -
	cd emacs-24.4
	./configure && make install
}
install_texlive() {
	sudo apt-get install texlive-full xzdec
	tlmgr init-usertree
	sudo apt-get install texlive-lang-cjk
	tlmgr init-usertree
	#wget -O- http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz | tar xfvz - 
	#cd install-tl-*
	#echo "I" | sudo ./install-tl
}

nh=$(pwd)
yes | sudo apt-get update
yes | sudo apt-get upgrade
yes | sudo apt-get install zsh build-essential llvm libclang-dev silversearcher-ag mercurial git ddskk
yes | sudo apt-get build-dep emacs24

#tex live 2015
which platex > /dev/null 2>&1
if [ $? -eq 0 ]; then
	echo " "
else
	install_texlive
fi

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
sudo chsh -s /usr/bin/zsh

sh ./deploy.sh



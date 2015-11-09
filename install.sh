#!/bin/sh
nh=$(pwd)

yes_no() {
  MSG=$1
  while :
  do
    echo -n "${MSG} y/N: "
    read ans
    case $ans in
    [yY]) return 0 ;;
    [nN]) return 1 ;;
    esac
  done
}

install_emacs() {
	wget -O-  http://ftp.gnu.org/gnu/emacs/emacs-24.4.tar.xz | tar xJvf -
	cd emacs-24.4
	./configure && make install
}
install_texlive() {
	echo "y" | sudo apt-get install texlive-full xzdec
	tlmgr init-usertree
	echo "y" | sudo apt-get install texlive-lang-cjk
	tlmgr init-usertree
	#wget -O- http://mirror.ctan.org/systems/texlive/tlnet/install-tl-unx.tar.gz | tar xfvz - 
	#cd install-tl-*
	#echo "I" | sudo ./install-tl
}
install_ricky() {
	unzip migumigu.zip
	cd migu-1m-20150712 
	mkdir ~./.fonts/
	mv migu-1m-*.ttf ~/.fonts/
	fc-cache -fv
	git clone https://github.com/yascentur/Ricty.git
	cd Ricty/
	git checkout refs/tags/3.2.3
	./ricty_generator.sh auto
	mv Ricty*.ttf ~/.fonts/
	fc-cache -fv
	cd $nh
}

echo "y" | sudo apt-get update
echo "y" | sudo apt-get upgrade
echo "y" | sudo apt-get install zsh build-essential llvm libclang-dev silversearcher-ag mercurial git ddskk pandoc fontforge fonts-inconsolata
echo "y" | sudo apt-get build-dep emacs24

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

yes_no "Do you want to install ricky's fonts?"
if [ $? ]; then
	install_ricky
fi

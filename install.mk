MAKEFILE_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
dots := .emacs .config .sshrc .sshrc.d .mostrc .dircolors texmf .xmonad

.PHONY: all sym $(dots) zsh lesskey misc

all: $(dots) zsh lesskey misc

define make_symlink
	@echo -e "\e[34mCreating symlink of "$@" ..."
	@rm -rf ~/$@
	@ln -sfn $(MAKEFILE_DIR)$@ ~/$@
endef

define zsh
	@cat << EOF > ~/.zshenv
#! /usr/bin/env zsh
# -*- mode: sh ; coding: utf-8 -*- 
#--- copy to each machine ---
export ZDOTDIR=$(MAKEFILE_DIR)/zsh
source ${ZDOTDIR}/.zshenv
#---------------------------- " 
EOF
	@cd ~
	@curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
endef

define lesskey
	@lesskey <<EOF
#command
m               repeat-search 
\em             repeat-search-all 
M               reverse-search 
\eM             reverse-search-all 
h               forw-line 
t               back-line 
H               forw-line-force 
T               back-line-force 
EOF
endef

define misc
	@cd ~

	@go get github.com/peco/peco/cmd/peco

	@wget https://github.com/adobe-fonts/source-han-code-jp/archive/2.011R.zip
	@unzip 2.011R.zip
	@cp ~/source-han-code-jp-2.011R/OTC/SourceHanCodeJP.ttc ~/.local/share/fonts

	@git clone https://github.com/powerline/fonts
	@cd ~/fonts/
	@./install.sh

	@cd /tmp
	@rm -rf ~/.icons
	@git clone https://github.com/gmmeyer/awesome-dangerzone
	@mv /tmp/awesome-dangerzone/icons ~/.icons
	@rm -rf /tmp/awesome-dangerzone
endef

zsh:
	$(zsh)

lesskey:
	$(lesskey)

misc:
	$(misc)

$(dots):
	$(make_symlink)

sym: $(dots)

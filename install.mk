MAKEFILE_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
dots := .emacs .config .sshrc .sshrc.d .mostrc .dircolors texmf .xmonad
inits := zsh lesskey peco font icon

.PHONY: all sym $(dots) $(inits)

all: $(dots) $(inits)

define make_symlink
	@echo -e "\e[34mCreating symlink of "$@" ..."
	@rm -rf ~/$@
	@ln -sfn $(MAKEFILE_DIR)$@ ~/$@
endef

$(inits):
	$(MAKEFILE_DIR)init/$@

$(dots):
	$(make_symlink)

sym: $(dots)

init: $(inits)

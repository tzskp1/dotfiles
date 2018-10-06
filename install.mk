MAKEFILE_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
dots := .emacs .config .sshrc .sshrc.d .mostrc .dircolors texmf .xmonad
inits := prerequire zsh lesskey peco font icon

.PHONY: all sym $(dots) $(inits)

all: $(dots) $(inits)

define colorecho
      @tput setaf 6
      @echo $1
      @tput sgr0
endef

define make_symlink
	@$(colorecho) "Creating symlink of "$@" ..."
	@rm -rf ~/$@
	@ln -sfn $(MAKEFILE_DIR)$@ ~/$@
endef

$(inits):
	$(MAKEFILE_DIR)init/$@

$(dots):
	$(make_symlink)

sym: $(dots)

init: $(inits)

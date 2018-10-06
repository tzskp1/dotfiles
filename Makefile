MAKEFILE_DIR := $(dir $(realpath $(lastword $(MAKEFILE_LIST))))
dots := .emacs.d .config .sshrc .sshrc.d .mostrc .dircolors .lesskey texmf .xmonad
inits := prerequire zsh lesskey peco font icon

.PHONY: all sym init $(dots) $(inits)

all: sym init 

define make_symlink
	@tput setaf 1 && echo "Creating symlink of "$@""
	@mv ~/$@ ~/$@.bak 2> /dev/null || true
	@ln -sfn $(MAKEFILE_DIR)$@ ~/$@
endef

$(inits):
	$(MAKEFILE_DIR)init/$@

$(dots):
	$(make_symlink)

sym: $(dots)

init: $(filter-out prerequire ,$(inits))

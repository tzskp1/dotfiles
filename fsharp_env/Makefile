USER ?= dotnet
commands := yo dotnet fsharpi fsharpc
mask_commands := bash emacs
VPATH := $(CURDIR)/../bin

.PHONY: all force clean

all: $(commands) $(mask_commands) 

%: %.m4
	m4 -P --define=HOST_USER=$(USER) $< > $@

define make_symlink
	@echo "Creating symlink of "$@" ..."
	@mkdir -p $(CURDIR)/../bin
	@$(RM) $(CURDIR)/../bin/$@
	@ln -s $(CURDIR)/$< $(CURDIR)/../bin/$@
endef

define make_symlink_at_local
	@echo "Creating symlink of "$@" ..."
	@$(RM) $(CURDIR)/$@
	@ln -s $(CURDIR)/$< $(CURDIR)/$@
endef

define copy_emacs_settings
	mkdir -p $(CURDIR)/.emacs.d
	cp ~/.emacs.d/init.el $(CURDIR)/.emacs.d/
	cp ~/.emacs.d/custom.el $(CURDIR)/.emacs.d/
	cp -RH ~/.emacs.d/snippets $(CURDIR)/.emacs.d/
	cp ./packages.el $(CURDIR)/.emacs.d/
endef

build: Dockerfile packages.el 
	@mkdir -p $(CURDIR)/../bin
	@mkdir -p $(HOME)/sources
	@$(copy_emacs_settings)
	docker build -t dotnet_core $(CURDIR)
	touch build

up_container_with: % : %.m4 build 
	m4 -P --define=HOST_USER=$(USER) $< > $@
	chmod +x $@

$(commands): % : up_container_with 
	$(make_symlink)

$(mask_commands): % : up_container_with 
	$(make_symlink_at_local)

force:

clean:
	$(RM) $(CURDIR)/build
	$(RM) $(CURDIR)/Dockerfile

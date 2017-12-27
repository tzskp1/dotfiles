all: build up_container_with
curdir := $(CURDIR)
user := $(USER)

Dockerfile: Dockerfile.in
	m4 -P --define=HOST_USER=$(user) Dockerfile.in > Dockerfile 

up_container_with: up_container_with.in 
	m4 -P --define=HOST_USER=$(user) up_container_with.in > up_container_with
	chmod 775 up_container_with
	mkdir -p $(curdir)/../bin
	ln -s $(curdir)/up_container_with $(curdir)/../bin/bash
	ln -s $(curdir)/up_container_with $(curdir)/../bin/dotnet
	ln -s $(curdir)/up_container_with $(curdir)/../bin/mono
	ln -s $(curdir)/up_container_with $(curdir)/../bin/fsharpi
	ln -s $(curdir)/up_container_with $(curdir)/../bin/fsharpc

build: Dockerfile packages.el
	mkdir -p $(curdir)/.emacs.d
	cp ~/.emacs.d/init.el $(curdir)/.emacs.d/
	cp ~/.emacs.d/custom.el $(curdir)/.emacs.d/
	cp -RH ~/.emacs.d/snippets $(curdir)/.emacs.d/
	cp ./packages.el $(curdir)/.emacs.d/
	docker build -t dotnet_core $(curdir)

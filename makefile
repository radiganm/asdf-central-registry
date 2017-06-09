#!/usr/bin/make
## makefile (for asdf-central-registry)
## Copyright 2016 Mac Radigan
## All Rights Reserved

.PHONY: install
.DEFAULT_GOAL := install bootstrap update

install:
	sudo mkdir -p /opt/asdf-central-registry && sudo cp -Rf ./dists /opt/asdf-central-registry

boostrap:
	./bootstrap-quicklisp.lisp

## *EOF*

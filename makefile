#!/usr/bin/make
## makefile (for asdf-central-repository)
## Copyright 2016 Mac Radigan
## All Rights Reserved

.PHONY: install
.DEFAULT_GOAL := install bootstrap

install:
	sudo mkdir -p /opt/asdf-central-repository && sudo cp -f ./dist /opt/asdf-central-repository

boostrap:
	./bootstrap-quicklisp.lisp

## *EOF*

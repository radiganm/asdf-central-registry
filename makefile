#!/usr/bin/make
## makefile (for asdf-central-registry)
## Copyright 2016 Mac Radigan
## All Rights Reserved

.PHONY: install compile
.DEFAULT_GOAL := install

install:
	sudo mkdir -p /opt/asdf-central-registry && sudo cp -Rf ./dists /opt/asdf-central-registry

compile:
	./compile.lisp

boostrap:
	./bootstrap-quicklisp.lisp

## *EOF*

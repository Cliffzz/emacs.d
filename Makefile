.PHONY: clean compile install
.DEFAULT_GOAL := build

build: | clean install compile

clean:
	rm -rf elpa **.elc **/**.elc

compile:
	emacs --script .make/compile.el

install:
	emacs --script .make/install.el

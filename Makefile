.PHONY: clean compile install
.DEFAULT_GOAL := build

build: | clean install compile

clean:
	npm uninstall -g jsonlint
	rm -rf elpa **.elc **/**.elc

compile:
	emacs --script .make/compile.el

install:
	npm install -g jsonlint@1.6.3
	emacs --script .make/install.el

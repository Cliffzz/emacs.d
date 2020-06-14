.PHONY: clean compile install mirror-elpa
.DEFAULT_GOAL := build

build: | clean install compile

clean:
	npm uninstall -g jsonlint typescript-language-server typescript vscode-json-languageserver vscode-html-languageserver-bin dockerfile-language-server-nodejs
	rm -rf elpa .bin **.elc **/**.elc **/**/**.elc

compile:
	emacs --script .make/compile.el

install:
	npm install -g jsonlint@1.6.3 typescript-language-server@0.4.0 typescript@3.8.3 vscode-json-languageserver@1.2.3 vscode-html-languageserver-bin@1.4.0 dockerfile-language-server-nodejs@0.0.22
	mkdir .bin
	cd .bin
	git clone https://github.com/microsoft/vscode-eslint.git
	cd vscode-eslint
	npm install
	npm run webpack
	cd ../../
	emacs --script .make/install.el

mirror-elpa:
	emacs --script .make/mirror-elpa.el

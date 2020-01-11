.PHONY: clean compile install
.DEFAULT_GOAL := build

build: | clean install compile

clean:
	npm uninstall -g jsonlint typescript-language-server typescript prettier-eslint-cli prettier vscode-json-languageserver vscode-html-languageserver-bin dockerfile-language-server-nodejs
	rm -rf elpa **.elc **/**.elc

compile:
	emacs --script .make/compile.el

install:
	npm install -g jsonlint@1.6.3 typescript-language-server@0.4.0 typescript@3.7.4 prettier-eslint-cli@5.0.0 prettier@1.19.1 vscode-json-languageserver@1.2.1 vscode-html-languageserver-bin@1.4.0 dockerfile-language-server-nodejs@0.0.21
	emacs --script .make/install.el

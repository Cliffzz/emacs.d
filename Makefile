.PHONY: install

install:
	emacs --script .make/install.el

clean:
	rm -rf elpa **/**.elc

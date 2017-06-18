UNAME:=$(shell uname -s)

ifeq ($(UNAME),Darwin)
EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
else
EMACS=emacs
endif

.PHONY: test

test:
	$(EMACS) -Q --batch -L . -l test/test-ttt.el

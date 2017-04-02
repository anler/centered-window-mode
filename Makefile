CASK = cask
EMACS = emacs
CASKEMACS = $(CASK) exec $(EMACS)

LOAD = -l centered-window-mode.el -l centered-window-mode-test.el

all: test

cask:
	$(shell EMACS=$(EMACS) $(CASK))

compile:
	$(CASKEMACS) -Q $(LOAD) centered-window-mode.el

test:
	$(CASKEMACS) -batch $(LOAD) -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc

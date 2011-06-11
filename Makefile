SHELL = /bin/sh

.SUFFIXES:			# no implicit rules

EMACS = emacs
BATCHFLAGS = -batch -Q
LOADFLAGS = -eval "(add-to-list 'load-path \".\")"
BYTECOMPCMD = $(EMACS) $(BATCHFLAGS) $(LOADFLAGS) -f batch-byte-compile

ELFILES = vimpulse-compatibility.el vimpulse-dependencies.el vimpulse.el   \
	  vimpulse-ex.el vimpulse-misc-keybindings.el vimpulse-modal.el	   \
	  vimpulse-operator.el vimpulse-paren-matching.el vimpulse-test.el \
	  vimpulse-text-object-system.el vimpulse-utils.el		   \
	  vimpulse-viper-function-redefinitions.el vimpulse-visual-mode.el

BIGSRC = Header						\
	 INSTALL					\
	 README						\
	 NEWS						\
	 Acknowledgements				\
	 BUGS						\
	 TODO						\
	 LICENSE					\
	 vimpulse-dependencies.el			\
	 vimpulse-viper-function-redefinitions.el	\
	 vimpulse-utils.el				\
	 vimpulse-modal.el				\
	 vimpulse-ex.el					\
	 vimpulse-paren-matching.el			\
	 vimpulse-visual-mode.el			\
	 vimpulse-operator.el				\
	 vimpulse-text-object-system.el			\
	 vimpulse-misc-keybindings.el			\
	 vimpulse-compatibility.el			\
	 vimpulse.el

BIGDIR = bigd

.PHONY: all
all: elisp

.PHONY: elisp
elisp: $(ELFILES)
	$(BYTECOMPCMD) $(ELFILES)

.PHONY: big
big: $(BIGSRC)
	test -d $(BIGDIR) || mkdir $(BIGDIR)
	sed "/(\(provide\|require\|declare-function\) \('\|\)vimpulse-/d" $(BIGSRC) > $(BIGDIR)/vimpulse.el

.SUFFIXES: .el .elc
%.elc: %.el
	$(BYTECOMPCMD) $<

.PHONY: test
test:
	$(EMACS) -Q $(LOADFLAGS) -l vimpulse-test.el -f test-interactive-suite

.PHONY: test-big
test-big:
	cd $(BIGDIR) && $(EMACS) -Q $(LOADFLAGS) -l ../vimpulse-test.el -f test-interactive-suite

emacs: clean
	$(EMACS) -Q -L . --eval "(require 'vimpulse)" &

.PHONY: clean
clean:
	rm -f *.elc
	rm -rf $(BIGDIR)

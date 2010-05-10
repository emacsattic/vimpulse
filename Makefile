.PHONY: all
all: vimpulse-big.el

vimpulse-big.el: Header INSTALL README Changelog Acknowledgements \
                 BUGS TODO LICENSE vimpulse-dependencies.el \
                 vimpulse-viper-function-redefinitions.el \
                 vimpulse-utils.el vimpulse-misc-keybindings.el \
                 vimpulse-modal.el vimpulse-ex.el \
                 vimpulse-paren-matching.el vimpulse-operator.el \
                 vimpulse-text-object-system.el \
                 vimpulse-visual-mode.el vimpulse-compatibility.el \
                 vimpulse.el
	sed "/^(\(provide\|require\) 'vimpulse-/d" \
	    Header \
	    INSTALL \
	    README \
	    Changelog \
	    Acknowledgements \
	    BUGS \
	    TODO \
	    LICENSE \
	    vimpulse-dependencies.el \
	    vimpulse-viper-function-redefinitions.el \
	    vimpulse-utils.el \
	    vimpulse-misc-keybindings.el \
	    vimpulse-modal.el \
	    vimpulse-ex.el \
	    vimpulse-paren-matching.el \
	    vimpulse-operator.el \
	    vimpulse-text-object-system.el \
	    vimpulse-visual-mode.el \
	    vimpulse-compatibility.el \
	    vimpulse.el \
	> vimpulse-big.el

.PHONY: clean
clean:
	rm -f vimpulse-big.el

all: vimpulse-big.el

vimpulse-big.el: Header INSTALL README Changelog Acknowledgements \
                 BUGS TODO LICENSE vimpulse-dependencies.el \
                 vimpulse-viper-function-redefinitions.el \
                 vimpulse-utils.el vimpulse-modal.el \
                 vimpulse-misc-keybindings.el vimpulse-ex.el \
                 vimpulse-paren-matching.el \
                 vimpulse-text-object-system.el \
                 vimpulse-visual-mode.el vimpulse.el
	cat Header \
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
	    vimpulse-modal.el \
	    vimpulse-misc-keybindings.el \
	    vimpulse-ex.el \
	    vimpulse-paren-matching.el \
	    vimpulse-text-object-system.el \
	    vimpulse-visual-mode.el \
	    vimpulse.el \
	> vimpulse-big.el

clean:
	-rm vimpulse-big.el



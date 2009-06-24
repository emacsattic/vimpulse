all: vimpulse-big.el


vimpulse-big.el: Header INSTALL README BUGS TODO Changelog Acknowledgements LICENSE vimpulse-dependencies.el vimpulse-utils.el  vimpulse-misc-keybindings.el vimpulse-ex.el vimpulse-viper-function-redefinitions.el vimpulse-paren-matching.el vimpulse-text-object-system.el vimpulse-visual-mode.el vimpulse.el
	cat Header INSTALL README BUGS TODO Changelog Acknowledgements LICENSE vimpulse-dependencies.el vimpulse-utils.el  vimpulse-misc-keybindings.el vimpulse-ex.el vimpulse-viper-function-redefinitions.el vimpulse-paren-matching.el vimpulse-text-object-system.el vimpulse-visual-mode.el vimpulse.el> vimpulse-big.el

clean: 
	-rm vimpulse-big.el
	


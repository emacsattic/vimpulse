all: vimpulse.el


vimpulse.el: Header INSTALL README BUGS TODO Changelog Acknowledgements LICENSE vimpulse-code.el
	cat Header INSTALL README BUGS TODO Changelog Acknowledgements LICENSE vimpulse-code.el > vimpulse.el

clean: 
	-rm vimpulse.el
	


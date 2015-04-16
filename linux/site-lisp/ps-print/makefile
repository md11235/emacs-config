# This makefile compiles the ps-print lisp files.

# Adapted by Klaus Berndl from the makefile to compile the JDE

RM=rm -f
EBATCH=emacs -batch -no-site-file

all:
	$(RM) *.elc ps-compile-script-init
	echo "(add-to-list 'load-path nil)" > ps-compile-script-init
	echo "(setq debug-on-error t)" >> ps-compile-script-init
	$(EBATCH) -l ps-compile-script-init -f batch-byte-compile *.el
	$(RM) ps-compile-script-init

clean:
	$(RM) *.elc ps-compile-script-init

# End of makefile

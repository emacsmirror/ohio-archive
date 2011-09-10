# Author: Eyvind Ness (eyvind) 
# Date:	  Friday, May 8 1992 06:23 GMT
# File:	  /usr/local/gnu/emacs/elisp/rpc-hm-1.0/Makefile

# srcs which other modules REQUIRE must be loaded first:
# LIBSRC = -l bourne-cmd-string-input -l common-lisp-indent -l rpc-hm
#
# Not necessary when runing init first. init.el also sets up the correct
# load-path:
INIT= -l init.el -f _init

# Name of GNU Emacs executable to be used for compiling the elisp 
# sources:
EMACSX=emacs

all:
	$(EMACSX) -batch -q $(INIT) -f _make

clean:
	rm -f *.elc *~ core \#*

# Local Variables:
# mode: Fundamental
# write-file-hooks: nil
# End:

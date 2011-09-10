LISPDIR=/usr/local/lib/gnuemacs/lisp
EMACS=emacs

SHARFILES=	README makefile rcs.el rcsco

rcs:
	cp rcs.el $(LISPDIR)
	$(EMACS) -batch -f batch-byte-compile $(LISPDIR)/rcs.el

rcs.shar: $(SHARFILES)
	shar -cv $(SHARFILES) > rcs.shar

###########################################################################
###
###  Makefile  -  for se (send-emacs) and emacs-server programs
###  ----------------------------------------------------------
###
###  SCCS ID: 91/12/20 10:21:35 1.4 Makefile
###
###  Copyright (C) 1991 Claus Bo Nielsen
###########################################################################

CFLAGS = -O

all:     se emacs-server
install: binprog elisp


se:	 se.o
	 cc -o se se.o

emacs-server:	emacs-server.o
	 cc -o emacs-server emacs-server.o

clean:
	 rm -f *.o *~ *.elc emacs-server se

se.o:           se.h Makefile
emacs-server.o: se.h Makefile

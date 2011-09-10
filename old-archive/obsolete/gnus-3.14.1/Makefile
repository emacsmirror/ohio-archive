# Makefile for GNUS newsreader
# Written by Masanobu UMEDA for version 3.14

GNUS= gnus.elc gnuspost.elc gnusmail.elc gnusmisc.elc nntp.elc nnspool.elc mhspool.elc tcp.elc timezone.elc

CONTRIB= gnus-user-tale.elc gnus-user-sxa.elc

TCP= tcp

# Specify the byte-compiler
ELC= xemacs -nw -batch -l ~/.emacs -f batch-byte-compile

# Specify the byte-compiler for SX/A Emacs (or Version 17.64).
#ELC= emacs -batch -l gnus-user-sxa.el -f batch-byte-compile

CFLAGS = -O

.SUFFIXES: .elc .el

.el.elc:
	$(ELC) $<

all: $(GNUS)

contrib: $(CONTRIB)

gnus.elc: nntp.elc
gnuspost.elc: nntp.elc
gnusmail.elc: nntp.elc
gnusmisc.elc: nntp.elc

$(CONTRIB): $(GNUS)

# Make the tcp module by yourself according to the operating system
# and machine.  If your emacs has the function open-network-stream, it
# is unnecessary.

# For BSD system (do you really need it?)
bsdtcp:
	$(CC) -o $(TCP) $(CFLAGS) tcp.c

# For USG system
usgtcp:
	$(CC) -o $(TCP) $(CFLAGS) tcp.c -DUSG

# For Fujitsu UTS system
utstcp:
	$(CC) -o $(TCP) $(CFLAGS) tcp.c -DFUJITSU_UTS -lu -lsocket

shar:
	shar2 -s -t -c -f -l50 -oGNUS3.13- Makefile *.GNUS *.el *.c *.texinfo

tar:
	tar cvf - Makefile *.GNUS *.el *.c *.texinfo | compress > gnus-3.14.1.tar.Z

clean:
	-rm -fr *.o *~

distclean:
	-rm -fr *.o *.elc *~

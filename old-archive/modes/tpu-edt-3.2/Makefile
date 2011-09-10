# Makefile for TPU-edt
#
# Users:
#
#   Don't start here!  Read tpu-doc.el first.
#
#   Good, now that you know about TPU-edt, you'll be happy to know that you
#   can use this makefile to byte-compile the TPU-edt sources.  First make
#   sure the EMACS macro (below) points to your emacs program, and then type
#   "make".  As a bonus you get TPU-edt.ref a one-page TPU-edt reference,
#   suitable for framing.
#
# Sys Admins:
#
#   TPU-edt is a set of elisp programs that need to be byte-compiled and
#   plopped into the emacs lisp directory.  You may need to change the
#   EMACS_ROOT macro if your emacs distribution is installed in a non-
#   standard place.  Also, make sure the EMACS macro points to the proper
#   emacs program.  This makefile can also create and install a TPU-edt
#   reference page, not to mention the familiar VMS edt and eve commands.
#
#   Possible make commands and their results -
#
#   Command           Results
#   ================  =========================================================
#   make              byte-compile sources and generate reference page
#   make install      install TPU-edt in the the emacs lisp directory
#   make install-ref  install the reference page in the emacs etc directory
#   make install-edt  install "edt" & "eve" commands in the local bin directory
#   make install-all  perform all of the above installations
#

.SUFFIXES: .el .elc

.el.elc:
	$(EMACS) -batch -f batch-byte-compile $<

# macros you may need to modify -------------------------------------------

BINDIR=      /usr/local/bin#            # location of local programs
EMACS=       $(BINDIR)/emacs#           # location of the emacs program
EMACS_ROOT=  /usr/local/emacs#          # emacs distribution directory
EMACS_LISP=  $(EMACS_ROOT)/lisp#        # emacs lisp directory

# you shouldn't have to change anything else ------------------------------

EMACS_ETC=   $(EMACS_ROOT)/etc#         # emacs etc directory

SHELL=/bin/sh

SOURCE=  tpu-edt.el tpu-doc.el tpu-extras.el tpu-recall.el tpu-mapper.el \
         vt-control.el Changes README edt Makefile LCD-entry NOTICE

ELISP=   tpu-edt.el tpu-doc.el tpu-extras.el tpu-recall.el tpu-mapper.el \
         vt-control.el

GLISP=   tpu-edt.el tpu-doc.el tpu-extras.el tpu-mapper.el vt-control.el

CLISP=   tpu-edt.elc tpu-doc.elc tpu-extras.elc tpu-recall.elc tpu-mapper.elc \
         vt-control.elc

REF=     TPU-edt.ref

PRODUCT= $(CLISP) $(REF)

EDT=     edt
EVE=     eve

all: $(SOURCE) $(PRODUCT)

install-all: install install-ref install-edt

install: $(ELISP) $(CLISP)
	for file in $?; do \
	    cp $$file $(EMACS_LISP); \
	    chmod 644 $(EMACS_LISP)/$$file; \
	done

install-ref: $(REF)
	cp $(REF) $(EMACS_ETC)
	chmod 644 $(EMACS_ETC)/$(REF)

install-edt: edt
	cp $(EDT) $(BINDIR)
	cp $(EDT) $(BINDIR)/$(EVE)
	chmod 755 $(BINDIR)/$(EDT) $(BINDIR)/$(EVE)

$(REF): tpu-doc.el
	sed -ne '/;; %% TPU-edt/,/^$$/p' tpu-doc.el | sed -e '/^$$/d' > $(REF)

release: $(SOURCE)
	-rm -f tpu-edt.tar*
	tar cvf tpu-edt.tar $(SOURCE)
	compress tpu-edt.tar

gnu-release: $(GLISP)
	-rm -f tpu-edt.tar*
	tar cvf tpu-edt.tar $(GLISP)
	gzip tpu-edt.tar

clean:
	-rm -f tpu-edt*tar* $(PRODUCT)

spotless: clean
	rcsclean $(SOURCE)

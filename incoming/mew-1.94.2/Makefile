###
### Makefile for byte-compile
###
### Author:  Kazu Yamamoto <Kazu@Mew.org>
### Created: Nov 18, 1994
### Revised: Aug 27, 1999

###
### (1) Set the following variables as you like.
### (2) make
### (3) make install
### (4) make info
### (5) make install-info
### (6) make install-etc
###

################################################################
##
## EDIT THE FOLLOWINGS
##

PREFIX = /usr/local
INFODIR = $(PREFIX)/info

##
## Compile engines
##

CC = gcc
CFLAGS = -O

#EMACS = emacs
EMACS = xemacs
#EMACS = mule

PERL=`which perl`
#PERL=/usr/local/bin/perl

##
## A directory where "mewencode" will be installed.
##

BINDIR = $(PREFIX)/bin

##
## A directory where mew*.el[c] will be installed.
##

ELISPDIR  = $(PREFIX)/lib/$(EMACS)/site-lisp
#ELISPDIR = $(PREFIX)/share/emacs/site-lisp
#ELISPDIR = $(PREFIX)/lib/emacs

##
## A directory where etc file will be installed.
##

ETCDIR = $(PREFIX)/lib/$(EMACS)/etc/Mew

################################################################
##
## DO NOT EDIT THE FOLLOWINGS
##

OBJS =  mew-attach.elc   mew-bq.elc      mew-cache.elc   \
	mew-complete.elc mew-decode.elc  mew-demo.elc    \
	mew-draft.elc    mew-encode.elc  mew-env.elc     \
	mew-ext.elc      mew-fib.elc     mew-func.elc    \
	mew-header.elc   mew-mark.elc    mew-message.elc \
	mew-mime.elc     mew-minibuf.elc mew-pgp.elc     \
	mew-pick.elc     mew-refile.elc  mew-scan.elc    \
	mew-sort.elc     mew-summary.elc mew-syntax.elc  \
	mew-virtual.elc  mew-highlight.elc mew-vars.elc  \
	mew-addrbook.elc mew.elc

SRCS =  mew-attach.el   mew-bq.el      mew-cache.el   \
	mew-complete.el mew-decode.el  mew-demo.el    \
	mew-draft.el    mew-encode.el  mew-env.el     \
	mew-ext.el      mew-fib.el     mew-func.el    \
	mew-header.el   mew-mark.el    mew-message.el \
	mew-mime.el     mew-minibuf.el mew-pgp.el     \
	mew-pick.el     mew-refile.el  mew-scan.el    \
	mew-sort.el     mew-summary.el mew-syntax.el  \
	mew-virtual.el  mew-mule.el                   \
	mew-mule0.el    mew-mule2.el   mew-mule3.el   \
	mew-highlight.el	mew-vars.el	      \
	mew-unix.el	mew-win32.el   mew-os2.el     \
	mew-lang-jp.el				      \
	mew-temacs.el   mew-xemacs.el  mew-addrbook.el \
	mew.el

TEMPFILE = temp.el

CP = cp
RM = rm -f
MKDIR = mkdir -p
BIN = bin

all: $(OBJS)
	@echo 'Compiling EL files of Mew ... '
	@echo 'PLEASE IGNORE WARNINGS IF DISPLAYED. TAKE IT EASY!'
	$(EMACS) -batch -q -no-site-file -l ./$(TEMPFILE) -f mew-compile
	cd $(BIN); $(MAKE) CFLAGS="$(CFLAGS)" CC="$(CC)" PERL="$(PERL)"
	@echo 'Compiling EL files of Mew ... done'

$(OBJS): $(TEMPFILE) mew.el

$(TEMPFILE):
	@echo '(setq load-path (cons "." load-path))' > $(TEMPFILE)
	@echo '(defun mew-compile () (mapcar (function (lambda (x) (byte-compile-file x))) (list ' >> $(TEMPFILE)
	@echo $(OBJS) | sed -e 's/\(mew[^ ]*\.el\)c/"\1"/g'  >> $(TEMPFILE)
	@echo ')))' >> $(TEMPFILE)

install: $(OBJS)
	-@if [ ! -d $(ELISPDIR) ]; then \
		$(MKDIR) $(ELISPDIR); \
	fi; \
	$(CP) $(SRCS) $(ELISPDIR)
	$(CP) $(OBJS) $(ELISPDIR)
	cd $(BIN); $(MAKE) install CFLAGS="$(CFLAGS)" CC="$(CC)" BINDIR="$(BINDIR)"

clean:
	$(RM) $(OBJS) $(TEMPFILE)
	cd $(BIN); $(MAKE) clean

info::
	cd info; $(MAKE) info EMACS=$(EMACS)

install-info:
	cd info; $(MAKE) install-info INFODIR=$(INFODIR)

jinfo::
	cd info; $(MAKE) jinfo EMACS=$(EMACS)

install-jinfo:
	cd info; $(MAKE) install-jinfo INFODIR=$(INFODIR)

install-etc:
	-@if [ ! -d $(ETCDIR) ]; then \
		$(MKDIR) $(ETCDIR); \
	fi; \
	cd etc; $(CP) -r * $(ETCDIR)/

##
## End of Makefile
##

#=======================================================================
# Makefile for files in /u/sy/beebe/emacs/primes/
#
# Current target list:
#	all			make everything (same as primes.dvi)
#	check			run validation suite
#	clean			remove unnecessary files
#	clobber			same as distclean
#	dist			make distribution file
#	distclean		remove almost everything make can rebuild
#	doc			make PostScript documentation
#	dw			doubled-word check	
#	maintainer-clean	remove absolutely everything make can rebuild	
#	mostlyclean		same as clean
#	primes-1.00.tar.gz	distribution file
#	primes.dvi		TeX DVI file for Emacs Lisp manual chapter
#	spell			spelling check
#	tgrind			typeset program listings
#
# Dummy targets:
#	install
#	install-exe
#	install-lib
#	install-man
#	install-show
#	install-shrlib
#	uninstall
#	uninstall-exe
#	uninstall-lib
#	uninstall-man
#	uninstall-shrlib
#
# [02-Mar-1999]
#=======================================================================
# Program definitions

PROGRAM		= primes
PROGLIB		= CHANGE-THIS-VALUE
VERSION		= 1.00

#=======================================================================
# Installation locations

prefix		= /usr/local
bindir		= $(prefix)/bin
catdir		= $(prefix)/man/cat$(manext)
libdir		= $(libtop)/$(PROGRAM)-$(VERSION)
libtop		= $(prefix)/$(PROGLIB)/
mandir		= $(prefix)/man/man$(manext)
manext		= 1
shrlibdir	= $(libtop)/$(PROGRAM)-$(VERSION)
shrlibtop	= $(prefix)/share/$(PROGLIB)/

#=======================================================================
# Macro definitions

CAT		= cat

CHMOD		= chmod

CP		= cp -p
CP		= rcp -p

DETEX		= detex

DETEXFLAGS	= -m -n -s

DETEXINFO	= ./detexinfo

DETEXINFOFLAGS	=

DISTFILES       = ChangeLog COPYING README Makefile NEWS TO-DO detexinfo \
		  primes.dvi primes.el primes.pdf primes.ps primes.sok \
		  primes.texi profile.awk profile-support.el test-primes.el

DVIPS		= dvips
DVIPS		= dvips-type1

DVIPSFLAGS	=

DW		= dw

EMACS		= emacs

EMACS-FILES	= more-primes.el primes.el profile-support.el test-primes.el
EMACS-FILES	= primes.el profile-support.el test-primes.el

GZIP		= GZIP=-9 gzip

ISPELL		= ispell

LIBFILES	= /dev/null

MAKEINFO	= makeinfo

MAKEINFOFLAGS	=

LN		= ln

LS		= ls

MKDIR		= mkdir -p

MV		= mv

PSTOPDF		= ps2pdf
PSTOPDF		= distill

PSTOPDFFLAGS	=

RM		= rm -f

SED		= sed

SHELL		= /bin/sh

SHRLIBFILES	= /dev/null

SORT		= sort

SPELL		= spell

SPELLFILTER	= $(CAT)
SPELLFILTER	= $(SED) -e 's/[0-9,.:;?&]/ /g'

TAR		= tar

TEXINDEX	= texindex

TEXINDEXFLAGS	=

TEXINFO		= texinfo

TEXINFOFLAGS	=

# NB: This enhanced tgrind is available at:
#
#	ftp://ftp.math.utah.edu/pub/misc/tgrind-3.00.tar.gz
#	ftp://ftp.math.utah.edu/pub/misc/tgrind-3.00.zip
#	ftp://ftp.math.utah.edu/pub/misc/tgrind-3.00.zoo
#	http://www.math.utah.edu/pub/misc/index.html#tgrind-3.00
#
TGRIND		= tgrind

TGRINDFLAGS	= -l el -fn Bookman

WC		= wc

#=======================================================================
# Compiler flags and source files

# We must set load-path to include the current directory so that
# (require 'xxx) statements can be satisfied during byte-compiling.
ELFLAGS		= $(ELOPT) --eval '(setq load-path (cons "." load-path))'
ELOPT		=
ELOBJS		= $(ELSRCS:.el=.elc)
ELSRCS		= primes.el test-primes.el

EMACSFLAGS	= --no-site-file --batch -q

TEXINFOFLAGS	= $(TEXINFOOPT)
TEXINFOOPT	=
TEXINFOOBJS	= $(TEXINFOSRCS:.texi=.dvi)
TEXINFOSRCS	= primes.texi

#=======================================================================
# Suffixes and extra rule(s)

.SUFFIXES:	.elc .el .info .texi .pdf .ps .dvi .dw .ser .texi

.ltx.dw .tex.dw .texi.dw .texinfo.dw:
	$(DETEX) $(DETEXFLAGS) $< | $(DW) >$@
	@if test -s $*.dw ; \
	then \
		echo "`$(WC) -l $*.dw` doubled word(s) found" ; \
	fi

.dvi.ps:
	$(DVIPS) -o$@ $(DVIPSFLAGS) $<

.dvi.pdf:
	$(DVIPS) -o$*.ps $(DVIPSFLAGS) $<
	$(PSTOPDF) $(PSTOPDFFLAGS) $*.ps

.el.elc:
	$(EMACS) $(EMACSFLAGS) $(ELFLAGS) --eval '(byte-compile-file "$<")'

.ltx.ser .tex.ser:
	if test -f $*.sok ; then true ; else touch $*.sok ; fi
	$(DETEX) $(DETEXFLAGS) $< | $(SPELLFILTER) | $(SPELL) +$*.sok >$@
	$(DETEX) $(DETEXFLAGS) $< | $(SPELLFILTER) | $(ISPELL) -l -p $*.sok >>$@
	$(MV) $@ $@.tmp
	$(SORT) -u $@.tmp >$@
	@-$(RM) $@.tmp
	@if test -s $*.ser ; \
	then \
		echo "`$(WC) -l $*.ser` spelling error(s) found" ; \
	fi

.ps.pdf:
	$(PSTOPDF) $(PSTOPDFFLAGS) $<

.texi.dvi:
	$(TEXINFO) $(TEXINFOFLAGS) $<
	$(TEXINFO) $(TEXINFOFLAGS) $<
	$(TEXINFO) $(TEXINFOFLAGS) $<

.texi.info:
	$(MAKEINFO) $(MAKEINFOFLAGS) $<

.texi.ser .texinfo.ser:
	if test -f $*.sok ; then true ; else touch $*.sok ; fi
	$(DETEXINFO) $(DETEXINFOFLAGS) $< | $(SPELLFILTER) | $(SPELL) +$*.sok >$@
	$(DETEXINFO) $(DETEXINFOFLAGS) $< | $(SPELLFILTER) | $(ISPELL) -l -p $*.sok >>$@
	$(MV) $@ $@.tmp
	$(SORT) -u $@.tmp >$@
	@-$(RM) $@.tmp
	@if test -s $*.ser ; \
	then \
		echo "`$(WC) -l $*.ser` spelling error(s) found" ; \
	fi

#=======================================================================
# Targets:

all:	$(PROGRAM).dvi

# Because the test-gcd step takes a long time, it is important
# to use byte-compiled versions of the library files:
check:	profile-support.elc primes.elc test-primes.elc
	@echo Running validation suite in batch mode
	$(EMACS) $(EMACSFLAGS) $(ELFLAGS) \
		-l test-primes.el \
		-f test-primes-with-profile-and-kill-emacs
	@echo See test-primes.profile for detailed profile results

clean:
	-$(RM) *.dvi
	-$(RM) *.dw
	-$(RM) *.i
	-$(RM) *.log
	-$(RM) *.o
	-$(RM) *.ser
	-$(RM) *~
	-$(RM) \#*
	-$(RM) a.out
	-$(RM) core
	-$(RM) tg_[0-9][0-9][0-9][0-9][0-9].*

clobber:	distclean

dist:	$(PROGRAM)-$(VERSION).tar.gz

distclean:	mostlyclean
	-$(RM) *.aux
	-$(RM) *.cp
	-$(RM) *.elc
	-$(RM) *.fn
	-$(RM) *.info
	-$(RM) *.ky
	-$(RM) *.pg
	-$(RM) *.ps
	-$(RM) *.toc
	-$(RM) *.tp
	-$(RM) *.vr
	-$(RM) $(PROGRAM)-$(VERSION).tar.gz
	-$(RM) -rf $(PROGRAM)-$(VERSION)
	-$(RM) test-primes.profile
	-$(RM) test-primes.results
#	-$(RM) $(PROGRAM)

doc:	$(PROGRAM).ps

dw:	$(PROGRAM).dw

install:	install-exe install-lib install-shrlib install-man install-show

install-exe:	uninstall-exe
##	$(CP) $(PROGRAM) $(bindir)/$(PROGRAM)
##	$(LN) $(bindir)/$(PROGRAM) $(bindir)/$(PROGRAM)-$(VERSION)
##	$(CHMOD) 775 $(bindir)/$(PROGRAM) $(bindir)/$(PROGRAM)-$(VERSION)

install-lib:	uninstall-lib
##	$(MKDIR) $(libtop)
##	@if test -d $(libtop) ; then true ; else echo Cannot create $(libtop) ; exit 1 ; fi
##	$(MKDIR) $(libdir)
##	@if test -d $(libdir) ; then true ; else echo Cannot create $(libdir) ; exit 1 ; fi
##	$(CP) $(LIBFILES) $(libdir)/
##	$(CHMOD) -R 664 $(libdir)/

install-man:	uninstall-man
##	$(CP) $(PROGRAM).man $(mandir)/$(PROGRAM).$(manext)
##	$(CHMOD) 664 $(mandir)/$(PROGRAM).$(manext)

install-show:
##	@echo ''
##	@echo Installed files...
##	@$(LS) -l $(bindir)/$(PROGRAM) $(bindir)/$(PROGRAM)-$(VERSION) \
##		$(mandir)/$(PROGRAM).$(manext)
##	@if test -d $(libdir) ; then $(LS) -lR $(libdir) ; fi
##	@echo ''

install-shrlib:	uninstall-shrlib
##	$(MKDIR) $(shrlibtop)
##	@if test -d $(shrlibtop) ; then true ; else echo Cannot create $(shrlibtop) ; exit 1 ; fi
##	$(MKDIR) $(shrlibdir)
##	@if test -d $(shrlibdir) ; then true ; else echo Cannot create $(shrlibdir) ; exit 1 ; fi
##	$(CP) $(SHRLIBFILES) $(shrlibdir)/
##	$(CHMOD) -R 664 $(shrlibdir)/

maintainer-clean:	distclean
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."
	-$(RM) *.pdf

mostlyclean:	clean

$(PROGRAM)-$(VERSION).tar.gz: $(DISTFILES)
	-$(RM) -rf $(PROGRAM)-$(VERSION)
	$(MKDIR) $(PROGRAM)-$(VERSION)
	$(LN) $(DISTFILES) $(PROGRAM)-$(VERSION)/
	$(TAR) cf - $(PROGRAM)-$(VERSION) | $(GZIP) >$(PROGRAM)-$(VERSION).tar.gz
	-$(RM) -rf $(PROGRAM)-$(VERSION)

spell:	$(PROGRAM).ser

tgrind:
	for f in $(EMACS-FILES) ; \
	do \
		$(TGRIND) $(TGRINDFLAGS) $$f ; \
	done

uninstall:	uninstall-exe uninstall-lib uninstall-shrlib uninstall-man

uninstall-exe:
##	-$(RM) $(bindir)/$(PROGRAM)
##	-$(RM) $(bindir)/$(PROGRAM)-$(VERSION)

uninstall-lib:
##	-$(RM) -r $(libdir)

uninstall-man:
##	-$(RM) $(mandir)/$(PROGRAM).$(manext)
##	-$(RM) $(catdir)/$(PROGRAM).$(manext)

uninstall-shrlib:
##	-$(RM) -r $(shrlibdir)

#=======================================================================
# File dependencies

#=======================================================================

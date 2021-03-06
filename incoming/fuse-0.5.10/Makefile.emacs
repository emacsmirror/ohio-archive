
### Set the following variables appropriate to your installation
EMACS = emacs
PERL  = perl
INSTALL =

## uncomment the next line to compile FUSE support for xanes and correct
#XANES  = --eval "(setq fuse-xanes-p t)"

### End of user defined variables ####################################

VNUM  = 0.5.7
FLAGS = -batch -q -no-site-file -l ./fuse-compile.el

.SUFFIXES:
.SUFFIXES:	.elc .el
.el.elc:
	$(EMACS) $(FLAGS) -f batch-byte-compile $*.el

.PHONY:	default lisp docs progs clean install
default:
	$(MAKE) lisp
	$(MAKE) progs
	@echo ' '
	@echo '*******************************************************'
	@echo ' Now do "make install" to install FUSE.'
	@echo ' The installation step is not necessary if this is the'
	@echo ' install directory.'
	@echo '*******************************************************'
	@echo ' '

lisp:
	$(EMACS) $(FLAGS) $(XANES) -f fuse-compile-files

progs:
	$(PERL) scripts/fixin scripts/intrp
	$(PERL) scripts/fixin scripts/kw
	$(PERL) scripts/fixin scripts/eshift
	$(PERL) scripts/fixin scripts/gnufix
	$(PERL) scripts/fixin scripts/rhofix
	$(PERL) scripts/fixin scripts/minmax
	$(PERL) scripts/fixin scripts/mr
	$(PERL) scripts/fixin scripts/lsdf

clean:
	rm -f *.elc Makefile .config.fuse scripts/*.bak *~
	mv -f input.el.in input.el

#docs:
#	$(MAKE) -C docs/

install:
	install -d $(INSTALL)
	install -m 644 *.el  $(INSTALL)
	sleep 1s
	install -m 644 *.elc $(INSTALL)
	install -m 644 Makefile $(INSTALL)
	install -d $(INSTALL)/docs
	install -d $(INSTALL)/emulation
	install -d $(INSTALL)/example
	install -d $(INSTALL)/fortran
#	install -d $(INSTALL)/pixmaps
	install -d $(INSTALL)/scripts
	cp -f docs/*           $(INSTALL)/docs
	cp -f emulation/*      $(INSTALL)/emulation
	cp -f example/*        $(INSTALL)/example
	cp -f fortran/*        $(INSTALL)/fortran
#	cp -f pixmaps/*.x[pb]m $(INSTALL)/pixmaps
	cp -f scripts/*        $(INSTALL)/scripts
	chmod +x $(INSTALL)/scripts/*

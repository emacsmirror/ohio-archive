# Makefile for bugtrack.
#
# This is really not much help, but this is all you get for now.

VERSION=1.3

prefix = /usr/gnu
EL = bugtrack.el bugtrack-edit.el bugtrack-sum.el bugtrack-rmail.el \
	bugtrack-init.el
ELC = bugtrack.elc bugtrack-edit.elc bugtrack-sum.elc bugtrack-rmail.elc \
	bugtrack-init.elc
DESTDIR = $(prefix)/lib/emacs/site-lisp

DISTFILES = $(EL) Makefile NEWS LCD-entry ChangeLog Contributors \
	COPYING README BUGS

.PHONY: install
install: $(ELC)
	(cd $(DESTDIR); rm -f $(EL) $(ELC))
	cp $(EL) $(DESTDIR)/
	cp $(ELC) $(DESTDIR)/
	(cd $(DESTDIR) ; chmod a-w $(EL) $(ELC))

$(ELC): $(EL)
	echo Directory needs recompiling
	false

.PHONY: tags TAGS
tags TAGS:
	etags *.el

.PHONY: clean
clean:
	rm -f *.elc core TAGS *.tar *.tar.gz bugtrack-1.*

# The rules below are for making a distribution of Bugtrack.

bugtrack-$(VERSION).tar.gz: bugtrack-$(VERSION).tar
	gzip -9 < bugtrack-$(VERSION).tar > bugtrack-$(VERSION).tar.gz

.PHONY: tar
tar: bugtrack-$(VERSION).tar

bugtrack-$(VERSION): $(DISTFILES)
	rm -rf bugtrack-$(VERSION)*
	mkdir bugtrack-$(VERSION)
	# Copy files, substituting $(VERSION) for XXRELEASE
	# and Id: foo for $I?: foo$
	for i in $(DISTFILES) ;\
	do \
		sed s/1.3/$(VERSION)/ $$i \
		| sed 's/\$Id\([^$]*\)\$$/Id\1/' >bugtrack-$(VERSION)/$$i; \
	done

bugtrack-$(VERSION).tar: bugtrack-$(VERSION)
	tar cvf bugtrack-$(VERSION).tar bugtrack-$(VERSION)

.PHONY: ftp
ftp: bugtrack-$(VERSION).tar.gz
	rm -f /usr/ftp/pub/emacs/bugtrack-$(VERSION).tar.gz
	cp bugtrack-$(VERSION).tar.gz /usr/ftp/pub/emacs

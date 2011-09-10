# We cannot use makeinfo to generate *.info because of incompatibility 
# between Emacs and XEmacs on Japanese character handling. [lookup-ja 1028]

# info_TEXINFOS = lookup.texi lookup-guide.texi
# EXTRA_DIST = texinfo-js.tex

TEXIS = lookup.texi lookup-guide.texi
INFOS = lookup.info lookup-guide.info
DVIS  = $(TEXIS:.texi=.dvi)
SUB_INFOS = lookup.info-1 lookup.info-2 lookup.info-3

EXTRA_DIST = texinfo-js.tex $(TEXIS) $(INFOS) $(SUB_INFOS)
MOSTLYCLEANFILES = lookup.pgs lookup-guide.pgs $(DVIS)
MAINTAINERCLEANFILES = Makefile.in
SUFFIXES = .texi .dvi .info

.texi.dvi:
	ptex $<
	texindex $(<:.texi=.??)
	ptex $<

clean-generic:
	rm -f *.aux *.toc *.log *.vrs *.cp *.pg *.cps *.fn *.fns\
		*.ky *.kys *.tp *.tps *.vr 

all: lookup.info lookup-guide.info

%.info: %.texi
	$(EMACS) -batch -q -no-site-file -l texinfmt \
		-f batch-texinfo-format $?

install-data-local:
	@$(NORMAL_INSTALL)
	$(mkinstalldirs) $(DESTDIR)$(infodir)
	@list='$(INFOS)'; \
	for file in $$list; do \
	  d=$(srcdir); \
	  for ifile in `cd $$d && echo $$file $$file-[0-9] $$file-[0-9][0-9]`; do \
	    if test -f $$d/$$ifile; then \
	      echo " $(INSTALL_DATA) $$d/$$ifile $(DESTDIR)$(infodir)/$$ifile"; \
	      $(INSTALL_DATA) $$d/$$ifile $(DESTDIR)$(infodir)/$$ifile; \
	    else : ; fi; \
	  done; \
	done
	@$(POST_INSTALL)
	@if $(SHELL) -c 'install-info --version | sed 1q | fgrep -s -v -i debian' >/dev/null 2>&1; then \
	  list='$(INFOS)'; \
	  for file in $$list; do \
	    echo " install-info --info-dir=$(DESTDIR)$(infodir) $(DESTDIR)$(infodir)/$$file";\
	    install-info --info-dir=$(DESTDIR)$(infodir) $(DESTDIR)$(infodir)/$$file || :;\
	  done; \
	else : ; fi

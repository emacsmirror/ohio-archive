SOURCES = evi.el evi-mule.el lookup.el lookup-utils.el lookup-types.el \
	  lookup-vse.el lookup-package.el lookup-kanji.el \
	  lookup-entry.el lookup-content.el lookup-select.el \
	  ndic.el ndeb.el ndtp.el ndict.el ndkks.el ndspell.el \
	  ndcookie.el ndmisc.el ndsrd.el sdicf.el stem-english.el
EXTRA_DIST = lookup-vars.el.in lookup-compile.el ndnmz.el \
	     lookup-logo.xbm lookup-logo.xpm
DISTCLEANFILES = lookup-vars.el
MAINTAINERCLEANFILES = Makefile.in

lisp_LISP = $(SOURCES) lookup-vars.el
ELCFILES = $(lisp_LISP:.el=.elc)

$(ELCFILES): lookup-vars.el $(srcdir)/lookup-compile.el $(lisp_LISP)
	@$(EMACS) -batch -l $(srcdir)/lookup-compile.el $(lisp_LISP)

lookup-vars.el: $(top_builddir)/config.status $(srcdir)/lookup-vars.el.in
	@rm -f lookup-vars.el
	@sed -e "s/\@VERSION\@/$(VERSION)/" \
	     -e "s!\@pkgemacsdir\@!$(emacsdir)/lookup!" \
	     $(srcdir)/lookup-vars.el.in > lookup-vars.el

###
### Makefile for info
###
### Author:  Kazu Yamamoto <Kazu@Mew.org>
### Created: Aug 22, 1999
### Revised: 

###
### (1) make install EMACS=xemacs
### (2) make install-info PREFIX=/usr/local
###

################################################################
##
## EDIT THE FOLLOWINGS
##

#EMACS=xemacs
#PREFIX=/usr/local
#INFODIR

INSTALLINFO = `which install-info`
#INSTALLINFO = /usr/local/bin/install-info

RM = rm -f
MKDIR = mkdir -p
TOUCH = touch

################################################################
##
## DO NOT EDIT THE FOLLOWINGS
##

INFO  = mew.info mew.info-1 mew.info-2 mew.info-3
JINFO = mew.jis.info mew.jis.info-1 mew.jis.info-2

info::
	sed -e 's/@setfilename mew.jis.info/@setfilename mew.info/' \
	    -e 's/@set jp/@set us/' mew.texi > tmp.texi
	mv tmp.texi mew.texi
	$(RM) mew.info*
	$(TOUCH) mew.info
	$(EMACS) -batch -q -no-site-file -l texinfmt \
		-f batch-texinfo-format mew.texi

jinfo::
	sed -e 's/@setfilename mew.info/@setfilename mew.jis.info/' \
	    -e 's/@set us/@set jp/' mew.texi > tmp.texi
	mv tmp.texi mew.texi
	$(RM) mew.jis.info*
	$(TOUCH) mew.jis.info
	$(EMACS) -batch -q -no-site-file -l texinfmt \
		-f batch-texinfo-format mew.texi

install-info:
	-@if [ ! -d $(INFODIR) ]; then \
		$(MKDIR) $(INFODIR); \
	fi; \
	cp $(INFO) $(INFODIR)
	-@if [ -x "$(INSTALLINFO)" ]; then \
		$(INSTALLINFO) mew.info $(INFODIR)/dir; \
	else \
		echo "Don't forget to edit $(INFODIR)/dir for Mew."; \
	fi

install-jinfo:
	-@if [ ! -d $(INFODIR) ]; then \
		$(MKDIR) $(INFODIR); \
	fi; \
	cp $(JINFO) $(INFODIR)
	-@if [ -x "$(INSTALLINFO)" ]; then \
		$(INSTALLINFO) mew.jis.info $(INFODIR)/dir; \
	else \
		echo "Don't forget to edit $(INFODIR)/dir for Mew."; \
	fi
##
## End of Makefile
##

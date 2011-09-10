# The name of the file
MANUAL=latexinfo

# The name of your DVI to PS filter
DVIPS=dvips

# The name of your GNU Emacs
EMACS=Xemacs

SPLITS = $(MANUAL).tex-aa $(MANUAL).tex-ab $(MANUAL).tex-ac \
		$(MANUAL).tex-ad $(MANUAL).tex-ae

all:	$(MANUAL).dvi $(MANUAL).info

texindex:	texindex.c
	cc -O -o texindex texindex.c

$(MANUAL).dvi:	texindex $(MANUAL).bbl $(MANUAL).tex
	latex $(MANUAL)

$(MANUAL).bbl:	$(MANUAL).aux $(MANUAL).bib
	-bibtex $(MANUAL)

$(MANUAL).aux:	$(MANUAL).tex
	latex $(MANUAL)
	if [ -s $(MANUAL).cp ] ; then \
		texindex $(MANUAL).cp ;\
		fi
	if [ -s $(MANUAL).pg ] ; then \
		texindex $(MANUAL).pg ;\
		fi
	if [ -s $(MANUAL).vr ] ; then \
		texindex $(MANUAL).vr ;\
		fi
	if [ -s $(MANUAL).fn ] ; then \
		texindex $(MANUAL).fn ;\
		fi
	if [ -s $(MANUAL).ky ] ; then \
		texindex $(MANUAL).ky ;\
		fi
	if [ -s $(MANUAL).tp ] ; then \
		texindex $(MANUAL).tp ;\
		fi
	latex $(MANUAL)

$(MANUAL).ps:	$(MANUAL).dvi
	$(DVIPS) $(MANUAL)

$(MANUAL).info:	$(MANUAL).tex $(LATEXINFO)/latexinfo.el
	rm -f make.el
	sed -e "s/MANUAL/$(MANUAL)/" makeinfofile.el > make.el
	$(EMACS) -batch -q -l make.el

$(MANUAL).tex::	$(SPLITS)
	cat $(SPLITS) >  $(MANUAL).tex


shar::
	split $(MANUAL).tex $(MANUAL).tex-

clean:	
	rm -f $(MANUAL).log $(MANUAL).blg make.el *~

veryclean:
	rm -f $(MANUAL).ps $(MANUAL).dvi $(MANUAL).tex-??


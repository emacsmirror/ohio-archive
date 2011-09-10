SHARDIR=/tmp/latexinfo

UTILS = .login .emacs Makefile README COPYING

STYS = format.sty \
	head.sty \
	latexinfo.sty \
	macsyma.sty \
	praxis.sty \
	psfonts.sty \
	scheme.sty \
	smallverb.sty


ELS = get-node.el \
	head-fmt.el \
	latexinfo-mode.el \
	latexinfo.el \
	macsyma-fmt.el \
	nodify-new.el \
	nodify.el \
	praxis-fmt.el \
	scheme-fmt.el \
	tex-to-latexinfo.el


MANS=	manual/Makefile \
	manual/README \
	manual/latexinfo.bib \
	manual/latexinfo.tex-aa \
	manual/latexinfo.tex-ab \
	manual/latexinfo.tex-ac \
	manual/latexinfo.tex-ad \
	manual/latexinfo.tex-ae \
	manual/makeinfofile.el \
	manual/sample.tex \
	manual/texindex.c

all::	manual

manual::
	(cd manual ; make)

shar::
	(cd manual ; make shar)
	makekit -o MANIFEST $(UTILS) $(SRCS) $(STYS) $(ELS) $(MANS)
	rm -f Part00
	cp README Part00
	echo "#! /bin/sh" >> Part00
	echo "mkdir manual" >> Part00
	-mkdir $(SHARDIR)
	mv Part?? $(SHARDIR)

clean::	
	rm *.el~ #*
	(cd manual ; make clean)


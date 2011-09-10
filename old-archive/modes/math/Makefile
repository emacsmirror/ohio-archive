 ### this is the skeletal makefile for LaLiberte''s emacs front end.


 #####
 ##### This makefile has been machine generated.
 ##### Any changes to it will not be permanent.
 #####
 
 




















 # ---------------------- makefile.options list ----------------------
 #
 # This file created on Tue May 23 02:06:40 CDT 1989
 # Hostname : wri
 # User     : maeder
 #
 # "LOCAL"
 # "CROSS"        : 0
 # "TRUNCATE"     : 0
 # "DUMP"         : 1
 # "TIMING"       : 1
 # "PIPES"        : 1
 # "BSTRING"      : 0
 # "LOCALCFLAGS"  : -f68881 -O /usr/lib/f68881.il
 # "REMOTECFLAGS" : REMOTECFLAGS
 # "LINKFLAGS"    : -f68881
 # "PSTARGETS"    : tekps ttyps animate x11ps sunps
 # "MAKEDUMP"     : 1
 # ------------------------------------------------------------------


MTBINDIR = ../MathTalk
MTSRCDIR = ../../../../Common/Source/MathTalk
# SRCDIR   = ../../../../Common/EmacsFrontEnd
SRCDIR = .

INCLDIR=../../../../Includes
#CCFLAGS = -I${MTSRCDIR} -I${INCLDIR}


CFLAGS  = $(CCFLAGS) -DSTANDALONE -I../Includes -I../MathTalk

all : emacsfrontend math-mode.el mathtalk.el mathremote.emacs macnb2nbl \
	mathtalk.elc math-mode.elc

notebook.h y.tab.c: ${SRCDIR}/notebook.y
	yacc -d ${SRCDIR}/notebook.y
	cp y.tab.h notebook.h

y.tab.o:	y.tab.c
	${CC} ${CFLAGS} -DYYMAXDEPTH=20000 -c y.tab.c

lex.yy.c:   ${SRCDIR}/notebook.l
	lex ${SRCDIR}/notebook.l

lex.yy.o:	lex.yy.c notebook.h
	${CC} ${CFLAGS} -c lex.yy.c

macnb2nbl:	y.tab.o lex.yy.o
	${CC} ${CFLAGS} -o macnb2nbl y.tab.o lex.yy.o -ll

mathdescemacs.h : $(SRCDIR)/mathdescemacs.awk
	awk -f $(SRCDIR)/mathdescemacs.awk \
		 < $(MTSRCDIR)/mathdesc.table > mathdescemacs.h

emacsfrontend : $(SRCDIR)/emacsfrontend.c mathdescemacs.h
	$(CC) $(CFLAGS) $(SRCDIR)/emacsfrontend.c \
		$(MTBINDIR)/mathtalk.o $(MTBINDIR)/mathdesc.o \
		$(MTBINDIR)/libtw.a \
		-o emacsfrontend

math-mode.el : $(SRCDIR)/math-mode.el
	cp $(SRCDIR)/math-mode.el .

mathtalk.el : $(SRCDIR)/mathtalk.el
	cp $(SRCDIR)/mathtalk.el .

math-mode.elc : $(SRCDIR)/math-mode.elc
	cp $(SRCDIR)/math-mode.elc .

mathtalk.elc : $(SRCDIR)/mathtalk.elc
	cp $(SRCDIR)/mathtalk.elc .

mathremote.emacs : $(SRCDIR)/mathremote.emacs
	cp $(SRCDIR)/mathremote.emacs .

dist : 
	tar cf math.tar \
		README \
		math-mode.texinfo \
		math-mode \
		math-mode.el \
		mathtalk.el \
		macnb2nbl.sun3 \
		macnb2nbl.sun4 \
		notebook.l \
		notebook.y \
		emacsfrontend.c \
		emacsfrontend.sun3 \
		emacsfrontend.sun4 \
		mathdescemacs.awk \
		mathdescemacs.h \
		mathremote.emacs1.1 \
		mathremote.emacs1.2 \
		Makefile
	compress math.tar
	uuencode math.tar.Z math.tar.Z > math.tar.Z.uu


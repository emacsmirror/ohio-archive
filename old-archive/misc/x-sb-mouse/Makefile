#
# Makefile for x-sb-mouse : Improved Emacs mouse interface
#
# x-sb-mouse by: Sullivan Beck (beck@qtp.ufl.edu)
# Makefile by: George Ferguson (ferguson@cs.rochester.edu)
#

#
# Paths and programs
#
ELISPDIR = /usr/emacs/lisp
INFODIR  = /usr/emacs/info

EMACS    = emacs
MAKEINFO = makeinfo +fill-column 75
INSTALL  = install -c -m 0644

#
# Suffix rules
#
.SUFFIXES: .el .elc .texi .info
.el.elc:
	echo '(byte-compile-file "$<")' > /tmp/$*.cmp
	$(EMACS) -batch $< -l /tmp/$*.cmp
	rm -f /tmp/$*.cmp

.texi.info:
	$(MAKEINFO) $<

#
# Package rules
#
SRCS = x-sb-mouse.el  xsbm-funs.el  xsbm-keys.el  xsbm-userfuns.el  thing.el
OBJS = x-sb-mouse.elc xsbm-funs.elc xsbm-keys.elc xsbm-userfuns.elc thing.elc

all: $(OBJS) x-sb-mouse.info

src: $(OBJS)

info: x-sb-mouse.info

install: $(OBJS) x-sb-mouse.info
	$(INSTALL) $(SRCS) $(ELISPDIR)
	$(INSTALL) $(OBJS) $(ELISPDIR)
	$(INSTALL) x-sb-mouse.info $(INFODIR)

install.src: $(OBJS)
	$(INSTALL) $(SRCS) $(ELISPDIR)
	$(INSTALL) $(OBJS) $(ELISPDIR)

install.info: x-sb-mouse.info
	$(INSTALL) x-sb-mouse.info $(INFODIR)

clean:
	rm -f $(OBJS) x-sb-mouse.info *.~


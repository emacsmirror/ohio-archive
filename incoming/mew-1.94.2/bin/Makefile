#
#  Makefile for mewencode and uumerge
#

SRCS = mewencode.c getopt.c
OBJS = mewencode.o getopt.o
PRG = mewencode
ALT = mewdecode mewcat
OTH = uumerge

BINDIR = /usr/local/bin
PERL = /usr/local/bin/perl

RM = rm -f
MKDIR = mkdir -p

#CFLAGS = -g

all: $(PRG) $(OTH)

$(PRG) : $(OBJS)
	$(CC) -o $@ $(OBJS)
	$(RM) $(ALT)
	for f in $(ALT); do \
	  ln $(PRG) $${f}; done

uumerge: uumerge.in
	sed -e "s|@PERL@|${PERL}|" < uumerge.in > uumerge
	chmod 755 uumerge

install: $(PRG) $(OTH)
	-@if [ ! -d $(BINDIR) ]; then \
		$(MKDIR) $(BINDIR); \
	fi; \
	cp $(PRG) $(BINDIR)
	for f in $(ALT); do \
	  $(RM) $(BINDIR)/$${f}; ln $(BINDIR)/$(PRG) $(BINDIR)/$${f}; done
	cp $(OTH) $(BINDIR)

clean:
	$(RM) $(OBJS) $(PRG) $(ALT) $(OTH)

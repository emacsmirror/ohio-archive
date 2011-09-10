SHELL = cmd.exe
SRC = mewencode.c getopt.c
OBJ = mewencode.o getopt.o
PRG = mewencode.exe
AL1 = mewdecode.exe
AL2 = mewcat.exe
OTH = uumerge uumerge.cmd utime.cmd
CNF = config.status config.cache config.log os2config.cache
RM = rm -f
#CC = gcc -O -s
CC = gcc -Zcrtdll -O -s

BINDIR = d:/usr/local/bin

CFLAGS = -DOS2

all: $(PRG) $(OTH)

$(PRG) : $(OBJ)
	$(CC) -o $@ $(OBJ)
	$(RM) $(AL1)
	$(RM) $(AL2)
	cp $(PRG) $(AL1)
	cp $(PRG) $(AL2)

$(OTH) :
	.\configure

install: $(PRG) $(OTH)
	$(RM) $(BINDIR)/$(AL1)
	$(RM) $(BINDIR)/$(AL2)
	cp $(PRG) $(BINDIR)
	cp $(BINDIR)/$(PRG) $(BINDIR)/$(AL1)
	cp $(BINDIR)/$(PRG) $(BINDIR)/$(AL2)
	cp $(OTH) $(BINDIR)

clean:
	$(RM) $(OBJ) $(PRG) $(AL1) $(AL2) $(CNF) $(OTH)

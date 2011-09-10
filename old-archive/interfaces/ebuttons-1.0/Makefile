##
## Makefile for ebuttons.
##
## There are at most 4 things you may want to do to this Makefile.
##
##     1) Change the MAX_BUTTONS value
##     2) and 4) Make a change if you have the 3D Athena widget set.
##     3) add a path to your X library.
##
## The locations where you can do this are marked with the above numbers
## in what follows.
##
## I am imake illiterate, sorry.
##
## Terry Jones (terry@santafe.edu), November 18, 1992.
## Santa Fe Institute
##

#
# 1) The maximum number of buttons.
#
MAX_BUTTONS = 50


#
# 2) Change the LIBS line according to whether or not you
#    have the 3d Athena widgets installed. If in doubt, just use
#    the first line (with -lXaw).
#
# 3) Add a -L/path/to/your/X/libs if your X libraries are installed
#    somewhere non-standard.
#
LIBS= -lXaw -lXmu -lXext -lXt -lX11
# LIBS= -lXaw3d -lXmu -lXext -lXt -lX11


#
# 4) Add -DHAVE_XAW3D if you have the 3d Athena widgets.
#
CFLAGS = -O -DMAX_BUTTONS=$(MAX_BUTTONS)


## Nothing below here should change.

all: ebuttons ebuttons.cat

ebuttons: buttons ebuttons.o 
	$(CC) $(CFLAGS) -o $@ ebuttons.o $(LIBS)

buttons:
	./make_ebuttons $(MAX_BUTTONS)

ebuttons.cat: ebuttons.1
	nroff -man ebuttons.1 > ebuttons.cat
clean:
	-rm -f *.o

clobber: clean
	-rm -f ebuttons ebuttons.h



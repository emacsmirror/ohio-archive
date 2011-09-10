
all:    dun-globals.el
	-mkdir ../bin
	-/amber/nview/tom/emacs-19.15/src/emacs -batch -l bytecomp -f batch-byte-compile *.el
	-emacs -batch -l `pwd`/dun-util -f compile-globals
	-rm dun-globals.elc
	-mv dun-globals.dat ../bin
	-mv *.elc ../bin

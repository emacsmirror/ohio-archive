CC = gcc -g

tzc: tzc.o lread.o
	$(CC) -o tzc.new tzc.o lread.o -lzephyr -lkrb -ldes -lcom_err
	/bin/rm -f tzc
	/bin/mv tzc.new tzc

lread.o: lread.h
tzc.o: lread.h

install: tzc
	cp tzc $(HOME)/bin

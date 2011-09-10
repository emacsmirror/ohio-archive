CFLAGS=-g -Wall

all: brkdb.so

brkdb.so: brkdb.c
	gcc $(CFLAGS) -ldb -shared -fpic brkdb.c -o brkdb.so

clean:
	rm -f brkdb.so


install: all
	cp brkdb.so /usr/local/lib
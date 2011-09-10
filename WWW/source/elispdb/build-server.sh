#!/bin/bash

LOC=$1

if [ ! -d $LOC ]; then
    echo "$LOC is not a directory."
    exit 1
fi

cd $LOC
if [ ! -d sockets ]; then
    echo "Couldn't find sockets directory in $LOC"
    exit 1
fi

cd lisp-brkdb
make install || exit 1
cd ..
cd sockets
make || exit 1
cd ..
lisp -load server-build.lisp -eval '(cl::quit)'



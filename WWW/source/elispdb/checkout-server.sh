#!/bin/bash

LOC=$1
HOSTNAME=$2

if [ ! -d $LOC ]; then
    echo "$LOC is not a directory."
    exit 1
fi

if [ -z $HOSTNAME ]; then
    HOSTNAME=`hostname --fqdn`
    echo "Using hostname $HOSTNAME"
fi

cd $LOC
FULLLOC=`pwd`
echo -n "Checking out elispdb, lisp-pcre, lisp-brkdb..."
(cvs -d :ext:walters@meta:/usr/local/cvsroot checkout elispdb lisp-pcre lisp-brkdb 1>/dev/null) 2>&1 | grep -v "cvs server: Updating"
echo "done."
echo -n "Rsyncing araneida, sockets..."
rsync -a -e ssh walters@meta:/usr/src/cvs-foreign/telent/araneida .
rsync -a -e ssh walters@meta:/usr/src/cvs-foreign/telent/sockets .
echo "done."
mkdir -p systems
cd systems
echo -n "Making system symlinks..."
for x in araneida lisp-brkdb lisp-pcre elispdb sockets; do
    for y in `(cd ../$x; ls *.system)`; do
	perl -pe 's/src:cvs\///' -i ../$x/$y
	ln -s ../$x/$y $y;
    done
done
cd ..
echo "done."
echo -n "Substituting hostname..."
cd elispdb
perl -pe "s/HOSTNAMEHERE/$HOSTNAME/" -i variables.lisp
cd ..
echo "done."
echo -n "Creating server-init.lisp..."
echo '(defparameter *server-base* ' "\"`pwd`/\"" ')' | cat - elispdb/server-init.lisp > server-init.lisp
echo "done."
echo -n "Creating server-build.lisp..."
echo '(defparameter *server-base* ' "\"`pwd`/\"" ')' | cat - elispdb/server-build.lisp > server-build.lisp
echo "done."
echo -n "Modifying sockets/Makefile..."
cd sockets
perl -pe "s#SOURCEPATH#$FULLLOC/#" -i Makefile
cd ..
echo "done."
echo -n "Cleaning up..."
find . -name '*.x86f' -exec rm -f {} \;
echo "done."


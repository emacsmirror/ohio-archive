#!/bin/sh

##
# Simple install script for elisp files
#
# Copyright (C) 2000 Iain Lowe <ilowe@cryogen.com>
##

if [ $1 != "-u" ]; then
    for i in *.elc; do {
	if [ -f $1/$i ]; then
	    rm -f $1/$i;
	    cp $i $1;
	else
	    cp $i $1;
	fi
    }; done
else
    for i in *.elc; do {
	if [ -f $2/$i ]; then
	    rm -f $2/$i;
	fi
    }; done
fi

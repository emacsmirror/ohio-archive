#!/bin/sh

# imls.sh: emulate imget program using MH's scan
#   options
#     --src=+inbox    -> +inbox
#     --width=80      -> -width 80
#     other options   -> ignore
#

mhdir=/usr/local/bin/mh
arg=

for opt in $*
do
    case $opt in
	-*=*) optarg=`echo $opt | sed 's/[-_a-zA-Z0-9]*=//'` ;;
	*) optarg= ;;
    esac
    
    case $opt in
	--width=*)
	    arg="$arg -width $optarg"
	    ;;
	--src=*)
	    arg="$arg $optarg"
	    ;;
	--*)
	    ;;  # ignore
	*)
	    arg="$arg $opt"
	    ;;
    esac
done

exec $mhdir/scan $arg | sed 's/^\( *[0-9]*\)+/\1 /'

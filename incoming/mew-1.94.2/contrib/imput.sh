#!/bin/sh

# imput.sh: emulate imput program using MH's send
#   options
#     -Draftfolder +dradt   -> -draftfolder +dradt
#     -draftmessage 1       -> -draftmessage 1 (add From: field)
#     --preserve=off        -> (ignore)
#     --help=no             -> (ignore)
#     --debug=no            -> (ignore)
#     --verbose             -> (ignore)
#     other options         -> (through)
#

mhdir=/usr/local/bin/mh
arg= fflag=0 dflag=0 folder=

addr="`sed -n 's/^Address=\([^ 	]*\).*/\1/p' $HOME/.im/Config`"
[ "$addr" = "" ] && addr="$USER@_your_domain_address_"

sig="`sed -n 's/[ 	]*#.*//;s/^Name=\(.*\)$/\1/p' $HOME/.im/Config`"
[ "$sig" = "" ] && sig="`sed -n 's/^[Ss]ignature: //p' $HOME/.mh_profile`"
[ "$sig" = "" ] && sig="`awk -F: '{print $5}' /etc/passwd`"
if [ "$sig" != "" ]; then
    if [ "`echo $sig | grep '[.@]'`" = "" ]; then
        from="From: $sig <$addr>"
    else
        from="From: "\""$sig"\"" <$addr>"
    fi
else
    from="From: $addr"
fi

for opt in $*
do
    case $opt in
	-*=*) optarg=`echo $opt | sed 's/[-_a-zA-Z0-9]*=//'` ;;
	*) optarg= ;;
    esac
    
    case $opt in
	--preserve=*)
	    ;;  # ignore
	--help=*)
	    ;;  # ignore
	--debug=*)
	    ;;  # ignore
	-draftfolder)
	    fflag=1
	    arg="$arg $opt"
	    ;;
	-draftmessage)
	    dflag=1
	    arg="$arg $opt"
	    ;;
	-verbose)
	    ;;  # ignore
	*)
	    if [ $fflag = 1 ]; then
		folder=`echo $opt | sed 's/^+//'`
	    fi
	    if [ $dflag = 1 ]; then
		cd $HOME/Mail/$folder
		mv $opt ,$opt
		sed -n '1,/^-*$/p' ,$opt | sed 's/^\(-*\)$/'"$from"'\
\1/' > $opt
		sed '1,/^-*$/d' ,$opt >> $opt
	    fi
	    arg="$arg $opt" fflag=0 dflag=0
	    ;;
    esac
done

exec $mhdir/send $arg 2>&1 | sed "s/^\([^<=][^=>]\)/`basename $0`: ERROR: \1/"

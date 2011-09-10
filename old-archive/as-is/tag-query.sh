Date: 26 Aug 88 05:18:32 GMT
From: Rene' Seindal <mcvax!dkuug!freja!seindal@uunet.uu.net>
Subject: Re: query-replace-regexp for multiple files
Organization: DIKU, U of Copenhagen, DK

fuchs@gmdka.UUCP (Harald Fuchs) writes:

>Before reinventing the wheel I ask Netland for an interactive tool in (GNU)
>Emacs Lisp doing a replacement in ALL Emacs buffers, something like
>query-replace-regexp for multiple files.
>I'm developing a C program consisting of many files, and I'd like to rename
>some global identifiers.
>-- 

Make a TAGS file using etags and use "M-x tags-query-replace"

These tags functions are so useful, that I made a shell script which makes
TAGS files for any files.  It doesn't put any tags in it, but who cares.  I
can then make a TAGS file for any kind of file, and visit them all at once
using "M-x tags-search aaaaaaaa" and make query replaces in all files in onw
fell swoop.

--- CUT --------------------------------------------------------------
#!/bin/sh
# mktags: make an empty TAGS file for emacs.
cp /dev/null TAGS

exec > TAGS
for file 
do
	echo '
'"${file},0"
done
----------------------------------------------------------------------

>                               Harald Fuchs

>fuchs@karlsruhe.gmd.dbp.de    ...!uunet!mcvax!unido!gmdka!fuchs

Rene' Seindal, CS. Dept, U. of Copenhagen, Denmark. (seindal@diku.dk)

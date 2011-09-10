$ !  GNU-EMACS PCMAIL mail reader support utility
$
$ !  Written by Mark L. Lambert
$ !  Architecture Group, Network Products Division
$ !  Oracle Corporation
$ !  20 Davis Dr,
$ !  Belmont CA, 94002
$
$ !  internet: markl@oracle.com or markl%oracle.com@apple.com
$ !  UUCP:     {hplabs,uunet,apple}!oracle!markl
$
$ ! This file is not officially part of GNU Emacs, but is being
$ ! donated to the Free Software Foundation.  As such, it is
$ ! subject to the standard GNU-Emacs General Public License,
$ ! referred to below.
$ ! 
$ ! GNU Emacs is distributed in the hope that it will be useful,
$ ! but WITHOUT ANY WARRANTY.  No author or distributor
$ ! accepts responsibility to anyone for the consequences of using it
$ ! or for whether it serves any particular purpose or works at all,
$ ! unless he says so in writing.  Refer to the GNU Emacs General Public
$ ! License for full details.
$ ! 
$ ! Everyone is granted permission to copy, modify and redistribute
$ ! GNU Emacs, but only under the conditions described in the
$ ! GNU Emacs General Public License.   A copy of this license is
$ ! supposed to have been given to you along with GNU Emacs so you
$ ! can know your rights and responsibilities.  It should be in a
$ ! file named COPYING.  Among other things, the copyright notice
$ ! and this notice must be preserved on all copies.
$
$ ! COM file to transfer new mail from VMS into an ASCII file readable by
$ ! EMACS.  If there is no new mail, mail.temp;1 is empty.
$ ! P1 is folder from which to select (currently ignored)
$ ! P2 is name of temp file to finally leave extracted mail in
$ ! P3 is mail reader directory
$ 
$ ! This COM files works fine on our local VMS 5.1.  Since it does delete 
$ ! mail in the newmail folder after extracting, it would be a very good
$ ! idea to perform a test on some test messages before trusting it on your
$ ! system.  I don't understand VMS all that well (and have no desire to),
$ ! so this file may not be bulletproof.
$
$ msg = f$environment("message")
$ set message /noident/notext/noseverity/nofacility
$ set default 'p3'
$ tempfile = "$$extract$$.temp"
$ dropfile = "'p2'"
$ del 'tempfile'.*,'dropfile'.*
$ !emacs dcl-command fn chokes on output so reroute to file and delete later
$ define /user_mode sys$output dcl-junk.txt
$ mail
select newmail
extract/all $$extract$$.temp
delete/all
purge/reclaim
exit
$ delete dcl-junk.txt.*
$ exists_p = f$search(tempfile)
$ if exists_p .eqs. "" then create 'tempfile'
$ rename 'tempfile' 'dropfile'
$ set message 'msg'
$ exit



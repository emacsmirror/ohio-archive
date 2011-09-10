$ !  GNU-EMACS PCMAIL mail reader support utility
$ ! 
$ !  Written by Mark L. Lambert
$ !  Architecture Group, Network Products Division
$ !  Oracle Corporation
$ !  20 Davis Dr,
$ !  Belmont CA, 94002
$ ! 
$ !  internet: markl@oracle.com or markl%oracle.com@apple.com
$ !  UUCP:     {hplabs,uunet,apple}!oracle!markl
$ ! 
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
$ !
$ ! COM file to send a named file to a named printer queue via EMACS
$ ! P1 is the printer queue name, P2 is the file to print, and
$ ! if parameter P3 is "delete", delete P2 after printing
$
$ ! define /user_mode sys$output dcl-junk.txt
$ print /queue='p1' 'p2'
$ if p3 .eqs. "delete" then del 'p2'.*
$ ! delete dcl-junk.txt.*
$ exit

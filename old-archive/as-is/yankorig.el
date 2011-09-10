;To: unix-emacs@bbn.com
;Date: 21 Apr 89 03:30:29 GMT
;From: Joel Spolsky <spolsky-joel@yale.ARPA>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: yangorig.el -> allow user defined quote character replying to mail
;Reply-To: Joel Spolsky <spolsky-joel@cs.yale.edu>
;Organization: Yale University Computer Science Dept, New Haven CT  06520-2158
;Source-Info:  From (or Sender) name not authenticated.
;
;
;Here is a slightly modified version of Barry Warsaw's yankorig.el.
;It allows you to insert any user-selected character (or string) at the
;beginning of each quoted line, when using C-c C-y to reply to email. I
;use it to insert a "|" in the left of all quoted text. ">" is another
;favorite. Just modify the line
;
;	(setq yank-character "| ")
;
;to set your favorite quote character.
;
;This version is an improvement on Barry's version in that it doesn't
;redefine indent-rigidly, it simply uses a replace-regexp. Also, it
;also quotes blank lines.
; 
;You will need:
;       (setq mail-setup-hook
;	         '(lambda () (load "yankorig")))
;in your .emacs file.
;
;
;+----------------+----------------------------------------------------------+
;|  Joel Spolsky  | bitnet: spolsky@yalecs.bitnet     uucp: ...!yale!spolsky |
;|                | internet: spolsky@cs.yale.edu     voicenet: 203-436-1483 |
;+----------------+----------------------------------------------------------+
;                                                      #include <disclaimer.h>


;; yankorig.el
;;
;; inserts a user-selected character at the beginning of a reply line
;;
;; You will need:
;;
;;       (setq mail-setup-hook
;;	         '(lambda () (load "yankorig")))
;;
;; in your .emacs file.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; NAME: Barry A. Warsaw                USMAIL: National Institute of Standards
;; TELE: (301) 975-3460                         and Technology (formerly NBS)
;; UUCP: {...}!uunet!cme-durer!warsaw           Rm. B-124, Bldg. 220
;; ARPA: warsaw@cme.nbs.gov                     Gaithersburg, MD 20899
;;   or: warsaw@cme-durer.arpa
;;
;; Modified by
;;+----------------+----------------------------------------------------------+
;;|  Joel Spolsky  | bitnet: spolsky@yalecs.bitnet     uucp: ...!yale!spolsky |
;;|                | internet: spolsky@cs.yale.edu     voicenet: 203-436-1483 |
;;+----------------+----------------------------------------------------------+
;; Modification history:
;;
;; 16-Feb-1989 by baw: modified funcs "indent-rigidly", "mail-yank-original"
;; 20-Apr-1989 by js:  rearranged to use replace-regexp instead
;;                     of indent-rigidly and thus quote blank lines as well.
;;

(setq yank-character "| ")

(defun mail-yank-original (arg)
  "Insert the message being replied to, if any (in rmail).
Puts point before the text and mark after.
Inserts the value yank-character before each line.
Just \\[universal-argument] as argument means don't indent
and don't delete any header fields."
  (interactive "P")
  (if mail-reply-buffer
      (let ((start (point)))
	(delete-windows-on mail-reply-buffer)
	(insert-buffer mail-reply-buffer)
	(if (consp arg)
	    nil
	  (mail-yank-clear-headers start (mark))
	  (replace-regexp "^" yank-character nil)
	  )
	(exchange-point-and-mark)
	(if (not (eolp)) 
	    (insert ?\n))
	(exchange-point-and-mark)
	(insert ?\n))))


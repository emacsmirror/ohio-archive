;From arpa-unix-emacs-request@CHIPS.BBN.COM Sun Mar 26 23:50:16 1989
;Received: from chips by CHIPS.BBN.COM id aa16392; 26 Mar 89 23:34 EST
;Received: from BBN.COM by CHIPS.BBN.COM id aa16388; 26 Mar 89 23:33 EST
;Received: from USENET by bbn.com with netnews
;	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com);
;	contact usenet@bbn.com if you have questions.
;To: unix-emacs@bbn.com
;Date: 26 Mar 89 23:39:43 GMT
;From: "Randal L. Schwartz @ Stonehenge" <ogccse!littlei!omepd!merlyn@husc6.harvard.edu>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: Re: Dumb monkey mode question
;Reply-To: "Randal L. Schwartz @ Stonehenge" <merlyn@intelob.intel.com>
;Message-Id: <4244@omepd.UUCP>
;References: <9102@alice.UUCP>
;Organization: Stonehenge; netaccess via BiiN, Hillsboro, Oregon, USA
;Source-Info:  From (or Sender) name not authenticated.
;Status: R
;
;In article <9102@alice.UUCP>, wilber@alice (Bob Wilber) writes:
;| I have the code for monkey.el and have an exceedingly fundamental
;| question:
;| 	How do I invoke monkey-mode?
;| 
;| Presumably one is supposed to go into monkey-mode automatically when
;| a directory is visited.  How does one make this happen?  (I tried setting
;| dired-mode-hook to 'monkey-mode and got a bizarre mixture of dired and
;| monkey.  Apparently I'm supposed to bypass dired entirely.)
;| 
;| Expecting an embarrassingly simple answer,
;
;Not so simple.  I first used the ideas in the monkey code to replace
;some of the keyboard invocations with the corresponding monkey
;commands...  then I thought... hey, dired already has all those
;bindings, and somewhere there must be some common routines... I'll
;just replace those.  The attached code, when attached to the end of
;the distributed 'monkey.el' will cause monkey to be considered as an
;across-the-board replacement for dired.  See the comments for details.
;
;Randal L. Schwartz, Stonehenge Consulting Services (503)777-0095
;on contract to BiiN (for now :-), Hillsboro, Oregon, USA.
;ARPA: <@intel-iwarp.arpa:merlyn@intelob> (fastest!)
;MX-Internet: <merlyn@intelob.intel.com> UUCP: ...[!uunet]!tektronix!biin!merlyn
;Standard disclaimer: I *am* my employer!
;Cute quote: "Welcome to Oregon... home of the California Raisins!"
;
;===== cut here =====

;;;
;;; replace dired-noselect with monkey-directory-noselect
;;; (causes every invocation of dired to try monkey instead!)
;;;

(require 'dired)

(or (fboundp 'monkey-dired-noselect)
    (fset 'monkey-dired-noselect (symbol-function 'dired-noselect)))

(defvar monkey-instead-of-dired 'ask
  "*Select `monkey' in place of `dired' invocations if t.
If not t or nil, ask.")

(defun dired-noselect (dirname)
  "Invoke monkey or dired on DIRNAME.
Select `monkey-dired-noselect' [the original `dired-noselect'] or
`monkey-directory-noselect' depending on the value of
`monkey-instead-of-dired'."
  (funcall (if (or (eq t monkey-instead-of-dired) ; yes if t
		   (and monkey-instead-of-dired	; maybe if non-nil
			(y-or-n-p "Use monkey instead of dired? ")))
	       'monkey-directory-noselect
	     'monkey-dired-noselect)
	   dirname))


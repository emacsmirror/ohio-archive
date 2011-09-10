;;; min-ispl.el - Electric minor mode interface to ispell package.
;;
;; Maintainer: Per Abrahamsen <abraham@iesd.auc.dk>
;; Version: $Id: min-ispl.el,v 5.13 1993/12/15 21:42:16 amanda Exp $
;;
;; LCD Archive Entry:
;; min-ispl|Per Abrahamsen|abraham@iesd.auc.dk|
;; Minor mode to spell words as you type them|
;; $Date: 1993/12/15 21:42:16 $|$Revision: 5.13 $|~/modes/min-ispl.el.gz|
;;
;; This file is not part of the GNU Emacs distribution (yet).
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;; Commentary:

;; In auto-ispell-mode `space' or `return' automatically trigger
;; `ispell-word' on previously typed word.
;;
;; The idea and first implementation of auto-ispell-mode was by
;; Avishai Yacoby.

;;; Code:

(require 'min-map)

;;; Beep

;; With GNU Emacs 19 the following should make ispell-word to beep
;; when an error is found.  With other version of GNU Emacs, you may
;; have to find and install `advice.el'.  You can get the latest
;; version of this package either via anonymous ftp from
;; ftp.cs.buffalo.edu (128.205.32.9) with pathname
;; /pub/Emacs/advice.el, or send email to hans@cs.buffalo.edu and he
;; will mail it to you.

;; This allows us to survive even if `advice.el' is not installed.
(condition-case nil
    (require 'advice)
  (error (provide 'advice)
	 (defmacro defadvice (&rest ignore))))

;; Make FSF Emacs 19 ispell.el beep.
(defadvice ispell-command-loop (before with-beep activate)
  (if auto-ispell-mode (beep)))

;; Make ispell3 ispell.el beep.
(defadvice ispell-choose (before with-beep activate)
  (if auto-ispell-mode (beep)))
  
;;; The Mode Line

(defvar auto-ispell-mode nil
  "True, iff we are in auto ispell mode.")

  (make-variable-buffer-local 'auto-ispell-mode)

(or (assq 'auto-ispell-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(auto-ispell-mode " Spell") minor-mode-alist)))

;;; Minor Map

(defvar auto-ispell-keymap nil
  "Keymap used for auto-ispell-mode commands.")

(if auto-ispell-keymap
    ()
  (setq auto-ispell-keymap (make-sparse-keymap)) 
  (define-key auto-ispell-keymap " " 'auto-ispell-check)
  (define-key auto-ispell-keymap "\r" 'auto-ispell-check)
  (define-key auto-ispell-keymap "\e\t" 'ispell-complete-word))

(or (assoc 'auto-ispell-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'auto-ispell-mode auto-ispell-keymap)
		minor-mode-map-alist)))
;;; The Mode

;;;###autoload
(defun auto-ispell-mode (&optional arg)
  "Toggle auto-ispell-mode.
With prefix arg, turn auto-ispell-mode on iff arg is positive.

In auto-ispell-mode, inserting a space or a new line applies ispell-word
on the previous word.

Pressing M-TAB activates ispell-complete-word."
  (interactive "P")
  (setq auto-ispell-mode
	(not (or (and (null arg) auto-ispell-mode)
		 (<= (prefix-numeric-value arg) 0))))
  (minor-mode-rehash)
  (set-buffer-modified-p (buffer-modified-p)))

(defun auto-ispell-check ()
  (interactive "*")
  (minor-call-shadow 'auto-ispell-mode (this-command-keys))
  ;; This odd way to call `ispell-word' has the advantage that it
  ;; works with both the FSF and the independent ispell mode.
  
  (save-excursion
    (condition-case what
	;; The ispell.el in 3.0.9 has the opposite meaning of the
	;; first argument to ispell-word than the ispell.el in Lucid
	;; 19.8.  Use the existence of the variable
	;; `ispell:filter-continue' to distinguish.
	(ispell-word (boundp 'ispell:filter-continue) t)
      ;; The ispell.el used in FSF 19 for Ispell 4.0 does not take two
      ;; arguments.  Thus the above call will generate an error which
      ;; we can catch and proceed to do the FSF 19 thing.
      (error (backward-word 1) 
	     (ispell-word)))))

(provide 'min-ispl)

;;; min-ispl.el ends here

;;; @ min-ispl.el - Electric minor mode interface to ispell package.
;;;
;;; $Id: min-ispl.el,v 5.6 1993/07/27 22:50:47 amanda Exp $
;;;
;;; LCD Archive Entry:
;;; min-ispl|Per Abrahamsen|abraham@iesd.auc.dk|
;;; Minor mode to spell words as you type them|
;;; 27-Jul-1993|5.6|~/modes/min-ispl.el.Z|

(provide 'min-ispl)
(require 'min-bind)

;;; @@ Desciption
;;;
;;; In auto-ispell-mode `space' or `return' automatically trigger
;;; `ispell-word' on previously typed word.

;;; @@ Making ispell-word beep

;;; With GNU Emacs 19 and Ispell 4.0 the following should make
;;; ispell-word to beep when an error is found.
 
;;;        (defadvice ispell-command-loop (before with-beep activate) (beep))
 
;;; With other versions of ispell, you might be able to use
 
;;;        (defadvice ispell-choose (before with-beep activate) (beep))
 
;;; With other version of GNU Emacs, you may have to find and install 
;;; `advice.el'.  You can get the latest version of this package
;;; either via anonymous ftp from ftp.cs.buffalo.edu (128.205.32.9)
;;; with pathname /pub/Emacs/advice.el, or send email to
;;; hans@cs.buffalo.edu and he will mail it to you. 
 
;;; I have only tested the GNU Emacs 19 and Ispell 4 combination.
 
;;; @@ Author
;;;
;;; Avishai Yacoby
;;; 
;;; Adopted to use minor keymaps by Per Abrahamsen
;;;
;;; This file is not part of the GNU Emacs distribution (yet).
;;;
;;; This file is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; this file, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.

;;; @@ The Mode Line

(defvar auto-ispell-mode nil
  "True, iff we are in auto ispell mode.")

  (make-variable-buffer-local 'auto-ispell-mode)

(or (assq 'auto-ispell-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(auto-ispell-mode " Spell") minor-mode-alist)))

;;; @@ Minor Map

(defvar auto-ispell-keymap nil
  "Keymap used for auto-ispell-mode commands.")

(if auto-ispell-keymap
    ()
  (setq auto-ispell-keymap (make-sparse-keymap)) 
  (define-key auto-ispell-keymap " " 'auto-ispell-check)
  (define-key auto-ispell-keymap "\r" 'auto-ispell-check)
  (define-key auto-ispell-keymap "\e\t" 'ispell-complete-word))

;;; @@ The Mode

(defun auto-ispell-mode (&optional arg)
  "Toggle auto-ispell-mode.
With prefix arg, turn auto-ispell-mode on iff arg is positive.

In auto-ispell-mode, inserting a space or a new line applies ispell-word
on the previous word.

Pressing M-TAB activates ispell-complete-word."
  (interactive "P")
  (if (or (and (null arg) auto-ispell-mode)
	  (<= (prefix-numeric-value arg) 0))
      ;; Turn it off
      (if auto-ispell-mode
	  (minor-unbind 'auto-ispell-mode))
    ;; Turn it on
    (if auto-ispell-mode
	()
      (minor-add-to-keymap 'auto-ispell-mode auto-ispell-keymap)
      (minor-set-variable 'auto-ispell-mode 'auto-ispell-mode t)))
  (set-buffer-modified-p (buffer-modified-p)))

(defun auto-ispell-check ()
  (interactive "*")
  (minor-call-shadow 'auto-ispell-mode (this-command-keys))
  (save-excursion
    (backward-word 1)
    (ispell-word)))

;;; @@ Emacs

(run-hooks 'after-min-ispl-hook)

;;; Local Variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: ";;; @+\\|(......"
;;; End:

; Date: Wed, 8 May 91 13:22:00 BST
; From: Graham Gough <graham@computer-science.manchester.ac.uk>

;;
;; sun-fun.el
;;
;;
;; An attempt(!) to provide a uniform mechanism for binding Sun function keys
;;
;; Main functions are bind-sun-fun-key and local-bind-sun-fun-key
;; which use an appropriate global or local keymap
;;
;; Written by Graham Gough  graham@cs.man.ac.uk
;; Send all bugs/suggestions to author
;; 24/1/89
;;
;; Copyright (C) 1989 Graham D. Gough
;;
;; This file is not  part of GNU Emacs, however, GNU copyleft applies
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;;


;; The point of these functions is to separate the  user  from  the
;; exact control sequences produced by the Sun keys and just use their names.

;; Here is a sample excerpt from a .emacs
;; 
;; (setq term-setup-hook 'term-setup-hook)
;; (defun term-setup-hook()
;;   (require 'sun-fun)
;;   (bind-sun-fun-key "l2" 'my-favourite-fuction) 
;;   (bind-sun-fun-key "M-l2" 'another-good-one) 
;;   (bind-sun-fun-key "f9" 'describe-sun-fun-bindings t) 
;;   (bind-sun-fun-key "f8" 'enscript-buffer t)
;;   (bind-sun-fun-key "r3" 'electric-command-history) 
;;   (bind-sun-fun-key "r5" 'dot-to-top)
;;   (bind-sun-fun-key "r7" 'sip:scroll-down-in-place))
;;  

(provide 'sun-fun)

(defun bind-sun-fun-key (str fun &optional warn)
  "Bind Sun function key STR to FUN (using the appropriate keymap)
Key names are l1, l2, etc, f1, f2 etc, r1, r2 etc. Capitalized versions work 
for emacstool users. If the optional argument WARN is non-nil a warning will 
be given for illegal keynames."
  (if (boundp 'suntool-map)
      (bind-sun-fun-key-st str fun warn)
    (bind-sun-fun-key-raw str fun warn)))

(defun local-bind-sun-fun-key (str fun &optional warn)
  "Bind Sun function key STR to FUN (using the local keymap)
Key names are l1, l2, etc, f1, f2 etc, r1, r2 etc. Capitalized versions work 
for emacstool users. If the optional argument WARN is non-nil a warning will 
be given for illegal keynames."
  (if (boundp 'suntool-map)
      (local-bind-sun-fun-key-st str fun warn)
    (local-bind-sun-fun-key-raw str fun warn)))

(defconst sun-raw-key-list 
  '(("l1" . "192" )  ("l2" . "193" ) ("l3" . "194" ) ("l4" . "195" )
    ("l5" . "196" )  ("l6" . "197" ) ("l7" . "198" ) ("l8" . "199" ) 
    ("l9" . "200" )  ("l10" . "201" ) 
    ("f1" . "224" ) ("f2" . "225" )  ("f3" . "226" ) ("f4" . "227" )
    ("f5" . "228" ) ("f6" . "229" )  ("f7" . "230" ) ("f8" . "231" )
    ("f9" . "232" )
    ("r1" . "208" ) ("r2" . "209" ) ("r3" . "210" ) ("r4" . "211" ) 
    ("r5" . "212" ) ("r6" . "213" ) ("r7" . "214" ) ("r8" . "215" ) 
    ("r9" . "216" ) ("r10" . "217" ) ("r11" . "218" ) ("r12" . "219" ) 
    ("r13" . "220" ) ("r14" . "221" ) ("r15" . "222" ) ("r16" . "223")))


(defconst sun-st-key-list
  '(("l1" . "bl" )  ("L1" . "bL" ) 
    ("M-l1" . "b\M-l" ) ("M-L1" . "b\M-L" ) 
    ("l2" . "bl" )  ("L2" . "bL" ) 
    ("M-l2" . "b\M-l" ) ("M-L2" . "b\M-L" ) 
    ("l3" . "cl" ) ("L3" . "cL" )
    ("l4" . "dl" )  ("L4" . "dL" ) 
    ("l5" . "el" )  ("L5" . "eL" ) 
    ("l6" . "fl" )  ("L6" . "fL" ) 
    ("C-l6" . "f," ) 
    ("l7" . "gl" )  ("L7" . "gL" ) 
    ("l8" . "hl" )  ("L8" . "hL" ) 
    ("C-l8" . "h," )
    ("l9" . "il" )  ("L9" . "iL" ) 
    ("C-l9" . "i," ) ("M-l9" . "i\M-l" )
    ("C-M-l9" . "i\M-," )
    ("l10" . "jl" ) ("L10" . "jL" )
    ("M-l10" . "j\M-l" ) ("C-l10" . "j," )
    ("f1" . "at" ) ("F1" . "aT" )
    ("f2" . "bt" ) ("F2" . "bT" )
    ("f3" . "ct" ) ("F3" . "cT")
    ("f4" . "dt" ) ("F4" . "dTt" )
    ("f5" . "et" ) ("F5" . "eT" )
    ("f6" . "ft" ) ("F6" . "fT" )
    ("f7" . "gt" ) ("F7" . "gT" )
    ("f8" . "ht" ) ("F8" . "hT" )
    ("f9" . "it" ) ("F9" . "iT" )
    ("r1" . "ar" )  ("R1" . "aR" ) 
    ("r2" . "br" )  ("R2" . "bR" ) 
    ("r3" . "cr" )  ("R3" . "cR" )
    ("r4" . "dr" )  ("R4" . "dR" ) 
    ("r5" . "er" )  ("R5" . "eR" ) 
    ("r6" . "fr" )  ("R6" . "fR" ) 
    ("r7" . "gr" )  ("R7" . "gR" ) 
    ("r8" . "hr" )  ("R8" . "hR" ) 
    ("r9" . "ir" )  ("R9" . "iR" ) 
    ("r10" . "jr" ) ("R10" . "jR" ) 
    ("r11" . "kr" ) ("R11" . "kR" )
    ("r12" . "lr" ) ("R12" . "lR" ) 
    ("r13" . "mr" ) ("R13" . "mR" ) 
    ("r14" . "nr" ) ("R14" . "nR" )
    ("r15" . "or" ) ("R15" . "oR" )
    ))

(defun bind-sun-fun-key-raw (str fun &optional warn)
  (let ((code (cdr (assoc str sun-raw-key-list))))
    (if code
	(define-key sun-raw-map (concat code "z") fun)
      (if warn
	  (message (format "No key \"%s\"" str))))))

(defun bind-sun-fun-key-st (str fun &optional warn)
  (let ((code (cdr (assoc str sun-st-key-list))))
    (if code
	(define-key suntool-map code fun)
      (message (format "No key \"%s\"" str)))))


(defun local-bind-sun-fun-key-raw (str fun &optional warn)
  (let ((code (cdr (assoc str sun-raw-key-list))))
    (if code
	(local-set-key (concat "\e[" code "z") fun)
      (if warn
	  (message (format "No key \"%s\"" str))))))

(defun local-bind-sun-fun-key-st (str fun &optional warn)
  (let ((code (cdr (assoc str sun-st-key-list))))
    (if code
	(local-set-key (concat "\C-x*" code) fun)
      (if warn
	  (message (format "No key \"%s\"" str))))))

;;
;;

(defun describe-sun-fun-bindings ()
  "Generate and display documentation strings for functions bound to fun keys"
  (interactive)
  (let ((docbuff (get-buffer-create "*Help*")))
    (set-buffer docbuff)
    (delete-region (point-min) (point-max))
    (insert "           ****** Sun Function Key Bindings ******\n")
    (if (boundp 'suntool-map)
	(mapcar 'sf:one-key-binding-st sun-st-key-list)
      (mapcar 'sf:one-key-binding-raw sun-raw-key-list))
    (goto-char (point-min))
    (pop-to-buffer docbuff)))

(defun sf:one-key-binding-raw(pr)
  (let ((binding (key-binding (concat "\e[" (cdr pr) "z"))))
    (if binding
	(insert  (car pr) "\t\t" (format "%s\n" binding )))))

(defun sf:one-key-binding-st(pr)
  (let ((binding (key-binding (concat "\C-x*" (cdr pr) ))))
    (if binding
	(insert  (car pr) "\t\t" (format "%s\n" binding )))))


;;
;; Inverse maps, used in exec-extended-sun
;;

(defun sf:swap (pair)
  (cons (cdr pair) (car pair)))

(defvar sun-raw-inv-map nil)

(defvar sun-st-inv-map nil)

(defun sun-raw-inv-map ()
  (or sun-raw-inv-map
      (setq sun-raw-inv-map (mapcar 'sf:swap sun-raw-key-list))))

(defun sun-st-inv-map ()
  (or sun-st-inv-map
      (setq sun-st-inv-map (mapcar 'sf:swap sun-st-key-list))))


(defvar sun-raw-regexp  (concat "^" (regexp-quote "\e[") "\\([12][0-9][0-9]\\)z$"))
(defvar sun-st-regexp    (concat "^" (regexp-quote "*") "\\(.*\\)$"))

(defun  key-description-sun (str)
  "Return a pretty description of key-sequence KEYS.
Control characters turn into \"C-foo\" sequences, meta into \"M-foo\"
spaces are put between sequence elements, etc.
also converts codes for Sun function keys to key names."
  (let ((map (cond ((string-match sun-raw-regexp str)
		      (sun-raw-inv-map))
		   ((string-match sun-st-regexp str)
		    (sun-st-inv-map))
		   (t nil))))
    (if (not map)
	(meta-key-description str)
      (setq str (substring str (match-beginning 1) (match-end 1)))
      (cdr (assoc str map)))))


(defun meta-key-description (keys)
  "Works like key-description except that sequences containing
meta-prefix-char that can be expressed meta sequences, are.
E.g. `\"\\ea\" becomes \"M-a\".

If the ambient value of meta-flag in nil, this function is equivalent to
key-description."
  (if (not (and meta-flag (numberp meta-prefix-char)))
      (key-description keys)
    (let (pattern start)
      (setq pattern (concat (char-to-string meta-prefix-char) "[\000-\177]"))
      (while (string-match pattern keys start)
	(setq keys
	      (concat
	       (substring keys 0 (match-beginning 0))
	       (char-to-string (logior (aref keys (1- (match-end 0))) 128))
	       (substring keys (match-end 0)))
	      start (match-beginning 0)))
      (key-description keys))))

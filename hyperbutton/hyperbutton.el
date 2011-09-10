;; ========================================================================
;; hyperbutton.el -- Hypertext-style buttons
;; Author          : Mike Williams <mike-w@cs.aukuni.ac.nz>
;; Created On      : Thu Mar 28 13:42:44 1991
;; Last Modified By: Mike Williams
;; Last Modified On: Tue Jun 25 15:42:31 1991
;; RCS Info        : $Revision: 1.14 $ $Locker:  $
;; ========================================================================
;; [[ CheckMeOut ]] [[ CheckMeIn ]]
;; 
;; NOTE: this file must be recompiled if changed.
;;
;; Copyright (C) Mike Williams <mike-w@cs.aukuni.ac.nz> 1991
;;
;; This file is not part of GNU Emacs, but is made available under the
;; same conditions.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
;; to anyone for the consequences of using it or for whether it serves
;; any particular purpose or works at all, unless he says so in writing.
;; Refer to the GNU Emacs General Public License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs, but only under the conditions described in the GNU Emacs
;; General Public License.  A copy of this license is supposed to have
;; been given to you along with GNU Emacs so you can know your rights and
;; responsibilities.  It should be in a file named COPYING.  Among other
;; things, the copyright notice and this notice must be preserved on all
;; copies.

;; This package provides support for hypertext-like buttons.  The user may
;; define `button handlers' to handle hyperbutton `events'.  Handlers will
;; usually return nil, indicating that the preconditions for their
;; selection have not been fulfilled -- however, when conditions are right,
;; they may return a lisp form to be evaluated.  A list of handlers is
;; maintained ... the 'determine-hyperbutton-form function will iterate
;; over these until one returns a form to be evaluated.  The functions
;; hyperbutton:{add,remove}-{global,local}-handler make installing and
;; uninstalling handlers easier, eg;
;;
;;   (hyperbutton:remove-global-handler 'hyperbutton:view-file)
;;
;; One standard hyperbutton handler, 'hyperbutton:lookup-button, provides
;; an easier and more efficient (although less general) interface for
;; defining buttons.  This function uses an association list of regular
;; expressions to lisp forms to search for a form to evaluate.  See the
;; documentation for variable 'hyperbutton:global-button-alist for details.
;; 
;; The functions {define,undefine}-{global,local}-hyperbutton make
;; manipulating (adding to and deleting from) the button-alists a bit
;; easier, eg.  
;;
;;   (define-local-hyperbutton 'word "CheckMeIn" '(rcs-ci-buffer))
;;   (undefine-local-hyperbutton 'word "CheckMeIn")

;; Many thanks to Rick Mugridge <rick@cs.aukuni.ac.nz> for the idea.

(require 'backquote)
(require 'thing-at-point)

(provide 'hyperbutton)

;; LCD Archive Entry:
;; hyperbutton|Mike Williams|mike-w@cs.aukuni.ac.nz
;; |Run lisp code when arbitrary text patterns are clicked on
;; |91-06-25|$Revision: 1.14 $|~/interfaces/hyperbutton.shar.Z

;;=== Usage ===============================================================
;; 
;; (autoload 'call-hyperbutton "hyperbutton" nil t)
;; (autoload 'determine-hyperbutton-form "hyperbutton")
;; (autoload 'hyperbutton:goto-card "hyperbutton")
;; 
;; (autoload 'define-global-hyperbutton "hyperbutton" nil t)
;; (autoload 'define-local-hyperbutton "hyperbutton" nil t)
;; (autoload 'undefine-global-hyperbutton "hyperbutton")
;; (autoload 'undefine-local-hyperbutton "hyperbutton")
;;
;; And for the adventurous ...
;; (autoload 'hyperbutton:add-global-handler "hyperbutton")
;; (autoload 'hyperbutton:add-local-handler "hyperbutton")
;; (autoload 'hyperbutton:remove-global-handler "hyperbutton")
;; (autoload 'hyperbutton:remove-local-handler "hyperbutton")

;;=== Version =============================================================

(defconst hyperbutton:version (substring "$Revision: 1.14 $" 11 -2)
  "The revision number of dired (as string).  The complete RCS id is:

  $Id: hyperbutton.el,v 1.14 1991/06/25 03:43:55 mike-w Exp $")

;;=== How it works ========================================================
;;
;; Determine-hyperbutton-form runs thru the list of handler functions,
;; calling each in turn until one returns non-nil.  Each handler returns a
;; form to be evaluated, or nil.
;;
;; Call-hyperbutton evaluates the result of a call to
;; determine-hyperbutton-form.  The two are separated, as it is reasonable
;; to use determine-hyperbutton-form elsewhere.  For instance, I use a
;; mouse handler which determines the hyperbutton form at the point the
;; mouse is clicked, but returns to the original window before evaluating
;; it.  Here's a pair of x-mouse functions which should work with the
;; standard x-mouse.el. 

(cond 
 ((eq window-system 'x)
  
  (defconst x-previous-window nil 
    "Window you were in before mouse-down")
  (defconst x-previous-position (make-marker) 
    "Position in target buffer before mouse-down")

  (defun x-mouse-hyperbutton-down (arg)
    (setq x-previous-window (selected-window))
    (x-mouse-select arg)
    (set-marker x-previous-position (point))
    (x-mouse-set-point arg))

  (defun x-mouse-hyperbutton-up (arg)
    (let (form)
      (unwind-protect
	  (progn
	    (x-mouse-set-point arg)
	    (setq form (determine-hyperbutton-form)))
	(goto-char (marker-position x-previous-position))
	(select-window x-previous-window)
	(setq x-previous-window nil)
	(set-marker x-previous-position nil))
      (eval form)))

  ))

;; Bind the first to a mouse-down event, and the second to the
;; corresponding mouse-up event.  eg.
;;
;;   (define-key mouse-map x-button-c-left     'x-mouse-hyperbutton-down) 
;;   (define-key mouse-map x-button-c-left-up  'x-mouse-hyperbutton-up)
;;   
;; Writing similar functions for use with emacstool (under suntools), or
;; for epoch, should be straightforward.  Alternatively, you could bind
;; call-hyperbutton to a key. eg.
;;
;;   (global-set-key "\M-+" 'call-hyperbutton)

;;=== Standard buttons and handlers =======================================
;;
;; * File browsing [hyperbutton:view-file]
;;   Click on filename in any buffer to view the corresponding file.
;;   Hyperbutton searches for this file on the hyperbutton:find-file-path.
;;
;; * Evaluate arbitrary elisp form:
;;   [[ Eval: (message "Hi there") ]]	<-- click me
;;
;; * Find or view a file:
;;   [[ Find: ~/.emacs ]]		<-- click me
;;   [[ View: /etc/motd ]]		<-- click me
;;
;; * Send mail:
;;   [[ Mail: groucho, chico ]]		<-- click me
;;
;; * View a buffer in my ~/.HyperCard directory:
;;   [[ Card: Home ]]			<-- click me

;;=== Ideas ===============================================================
;;
;;  * Define hyperbuttons for commonly executed functions, eg. reading
;;    mail [[ ReadMail ]], compiling an Emacs-Lisp file [[ CompileMe ]],
;;    banging your head against a brick wall [[ BrickWall ]].
;;
;;   (setq hyperbutton:global-button-alist
;;         (append
;;          hyperbutton:global-button-alist
;;          (list
;;           '(hyperbutton
;;             ("\\s +ReadMail\\s +" . (if (fboundp 'vm) (vm) (rmail)))
;;             ("\\s +CompileMe\\s +" . (byte-compile-file buffer-file-name))
;;             ))))
;;             
;;   (define-global-hyperbutton 'hyperbutton "BrickWall" '(doctor))
;;   
;;  * Create a handler that will popup help on certain keywords when they
;;    are clicked on.
;;  
;;  * Define handlers for easy browsing in info.
;;    [see info-hyper.el package]
;;    
;;  * Define handler to visit clicked-on error in *Compilation* buffer.
;;    [write me for details]
;;
;;  * Define a handler execute find-tag in source code buffers.
;;
;;  * Define handlers/buttons for GNUS/VM to allow easy selection of
;;    groups, articles and mail messages.

;;=== Main user functions =================================================

(defun call-hyperbutton ()
  "Apply functions in hyperbutton-handlers in turn, until one returns non-nil."
  (interactive)
  (eval (determine-hyperbutton-form)))

(defun determine-hyperbutton-form ()
  (let ((button-list (append hyperbutton:local-handlers
			     hyperbutton:global-handlers
			     '(hyperbutton:undefined)))
	return-val)
    (while (and (not return-val) button-list)
      (setq return-val (call-interactively (car button-list)))
      (setq button-list (cdr button-list)))
    return-val))

;;=== Handler variables ===================================================

(defvar hyperbutton:global-handlers 
  '(hyperbutton:lookup-button hyperbutton:view-file)
  "Global list of functions to be called in turn by \\[hyperbutton], until
one returns a form to be evaluated.
Note that the hyperbutton:local-handlers take precedence.") 

(defvar hyperbutton:local-handlers nil
  "Buffer-local list of functions to be called in turn by \\[hyperbutton],
until one returns a form to be evaluated.
These take precedence over the hyperbutton:global-handlers.") 
(make-variable-buffer-local 'hyperbutton:local-handlers)

;;=== Useful handlers =====================================================

;;--- Default handler ---

(defun hyperbutton:undefined ()
  (interactive)
  '(message "No matching button"))

;;--- Find/View files ---

(defvar hyperbutton:find-file-path '(nil "/usr/include")
  "List of directories in which hyperbutton:find-file tries to locate files.
It might be useful to set this to include directories like /usr/include.")
(defvar hyperbutton:find-file-extensions '("" ".el" ".h")
  "List of file extensions allowed by hyperbutton:find-file.")

(defun hyperbutton:find-file (FILE &optional VIEW)
  "Find filename at point."
  (interactive (list (thing-at-point 'filename)))
  ;; Requires locate-file, from the lib-complete.el package
  (if (not (featurep 'lib-complete))
      (error "hyperbutton:find-file requires the lib-complete package"))
  (let ((path (if FILE
		  (locate-file (substitute-in-file-name FILE)
			       hyperbutton:find-file-path
			       hyperbutton:find-file-extensions))))
    (if path
	(if VIEW 
	    (` (view-file (, path))) 
	  (` (find-file (, path)))))
    ))

(defun hyperbutton:view-file (FILE)
  "View filename at point."
  (interactive (list (thing-at-point 'filename)))
  (hyperbutton:find-file FILE 'view))

;;--- View files in hypercard-directory ---

(defvar hyperbutton:hypercard-path '(nil "~/.HyperCard"))

(defun hyperbutton:goto-card (CARD)
  "Goto specified card."
  (interactive (list (thing-at-point 'sexp)))
  (let ((path (and CARD 
		   (locate-file CARD hyperbutton:hypercard-path))))
    (if path
	(` (view-file (, path))))))

;;=== General button handler ==============================================

;;--- Hyperbutton name extraction ---

(defvar hyperbutton:start-re (concat (regexp-quote "[[") "[ \t]*"))
(defvar hyperbutton:end-re   (concat "[ \t]*" (regexp-quote "]]")))

(defun beginning-of-hyperbutton () 
  "Search backward for hyperbutton:start-re, and position point at end."
  (re-search-backward hyperbutton:start-re)
  (re-search-forward hyperbutton:start-re))

(defun end-of-hyperbutton () 
  "Search forward for hyperbutton:end-re, and position point at beginning."
  (re-search-forward hyperbutton:end-re)
  (re-search-backward hyperbutton:end-re))

;;--- Button/function association ---

(defvar hyperbutton:global-button-alist

  '((hyperbutton
     ("\\`\\s *Eval:" . 
      (message "%s"
	       (eval (read-from-whole-string 
		      (substring hyperbutton-name (match-end 0))))))
     ("^\\s *Find:\\s *\\(\\S +\\)\\s *$" . 
      (find-file (substring hyperbutton-name 
			    (match-beginning 1) (match-end 1))))
     ("^\\s *View:\\s *\\(\\S +\\)\\s *$" . 
      (view-file (substring hyperbutton-name 
			    (match-beginning 1) (match-end 1))))
     ("^\\s *Card:\\s *\\(\\S +\\)\\s *$" . 
      (hyperbutton:goto-card
       (substring hyperbutton-name 
		  (match-beginning 1) (match-end 1))))
     ("^\\s *Mail:\\s *\\(.+\\)$" . 
      (mail nil (substring hyperbutton-name 
			   (match-beginning 1) (match-end 1))))))
  
  "Alist used by hyperbutton:lookup-button to determine a form to evaluate.

  ((THING 
    (REGEXP . BODY)
    (REGEXP . BODY)
    ...)
   (THING 
    (REGEXP . BODY)
    (REGEXP . BODY)
    ...))

  When the THING at point (cf. thing-at-point) matches associated regular 
expression REGEXP, execute BODY with the symbol 'hyperbutton-name 
dynamically bound to the THING matched.") 

(defvar hyperbutton:local-button-alist nil
  "Local alist used by hyperbutton:lookup-button to determine a form to 
evaluate.  See documentation for hyperbutton:global-button-alist for 
details.  Note that local definitions take precedence over global ones.")
(make-variable-buffer-local 'hyperbutton:local-button-alist)

(defun hyperbutton:lookup-button ()
  "Determine a form to be evaluated using hyperbutton:local-button-alist 
and hyperbutton:global-button-alist."
  (interactive)
  (catch 'hyperbutton-form
    (let ((clauses (append hyperbutton:local-button-alist 
			   hyperbutton:global-button-alist)))
      (while clauses 
	(let ((button-name (thing-at-point (car (car clauses))))
	      (alist (cdr (car clauses))))
	  (if (not button-name) nil
	    (while alist
	      (if (string-match (car (car alist)) button-name)
		  (throw 'hyperbutton-form 
			 (` (let ((hyperbutton-name (, button-name)))
			      (eval (, (cdr (car alist))))))))
	      (setq alist (cdr alist)))))
	(setq clauses (cdr clauses))))))

;;=========================================================================
;;=== Utilities ===========================================================

(defun hyperbutton:filter (LIST PRED)
  "Return list of elements in LIST for which PRED is true."
  (cond 
   ((not LIST) nil)
   ((funcall PRED (car LIST))
    (cons (car LIST) (hyperbutton:filter (cdr LIST) PRED)))
   (t (hyperbutton:filter (cdr LIST) PRED))))

;;=== Add/remove handlers =================================================

(defun hyperbutton:add-handler (HANDLER-LIST HANDLER)
  "Update HANDLER-LIST to include HANDLER."
  (if (catch 'member
	(mapcar (function (lambda (elt) (if (equal elt HANDLER) 
					    (throw 'member t))))
		(symbol-value HANDLER-LIST))
	nil) nil
    (set HANDLER-LIST (append (symbol-value HANDLER-LIST) (list HANDLER)))))

(defun hyperbutton:add-global-handler (HANDLER)
  "Update hyperbutton:global-handlers to include HANDLER."
  (hyperbutton:add-handler 'hyperbutton:global-handlers HANDLER))

(defun hyperbutton:add-local-handler (HANDLER)
  "Update hyperbutton:local-handlers to include HANDLER."
  (hyperbutton:add-handler 'hyperbutton:local-handlers HANDLER))
			   
(defun hyperbutton:remove-handler (HANDLER-LIST HANDLER)
  "Update HANDLER-LIST to exclude HANDLER."
  (set HANDLER-LIST
       (hyperbutton:filter 
	(symbol-value HANDLER-LIST)
	(function (lambda (handler) (not (equal handler HANDLER)))))))

(defun hyperbutton:remove-global-handler (HANDLER)
  "Update hyperbutton:global-handlers to exclude HANDLER."
  (hyperbutton:remove-handler 'hyperbutton:global-handlers HANDLER))

(defun hyperbutton:remove-local-handler (HANDLER)
  "Update hyperbutton:local-handlers to exclude HANDLER."
  (hyperbutton:remove-handler 'hyperbutton:local-handlers HANDLER))
			
;;=== Easy button definition ==============================================

(defun hyperbutton:define-hyperbutton (ALIST THING REGEXP BODY)
  "Update ALIST so that when the THING at point (cf. thing-at-point) 
matches REGEXP, BODY will be executed with hyperbutton-name bound to the 
value of the THING matched."
  (let ((entry (list THING (cons REGEXP BODY))))
    (hyperbutton:undefine-hyperbutton ALIST THING REGEXP)
    (set ALIST (append (symbol-value ALIST) (list entry)))))

(defun define-global-hyperbutton (THING REGEXP BODY)
  "Update hyperbutton:global-button-alist so that when the THING at point
\(cf. thing-at-point\) matches REGEXP, BODY will be executed with 
hyperbutton-name bound to the value of the THING matched."
  (interactive 
   (list (read-from-whole-string 
	  (completing-read "Thing to define: " obarray))
	 (read-string "Regular expression: ")
	 (read-from-minibuffer "Form to eval: " nil nil 'read)))
  (hyperbutton:define-hyperbutton 
   'hyperbutton:global-button-alist THING REGEXP BODY))

(defun define-local-hyperbutton (THING REGEXP BODY)
  "Update hyperbutton:local-button-alist so that when the THING at point
\(cf. thing-at-point\) matches REGEXP, BODY will be executed with 
hyperbutton-name bound to the value of the THING matched."
  (interactive 
   (list (read-from-whole-string 
	  (completing-read "Thing to define: " obarray))
	 (read-string "Regular expression: ")
	 (read-from-minibuffer "Form to eval: " nil nil 'read)))
  (hyperbutton:define-hyperbutton 
   'hyperbutton:local-button-alist THING REGEXP BODY))

(defun hyperbutton:undefine-hyperbutton (ALIST THING REGEXP)
  "Remove any entry in ALIST for THING matching REGEXP."
  (set ALIST
       (hyperbutton:filter 
	(mapcar 
	 (function 
	  (lambda (thing-assoc) 
	    (cons (car thing-assoc)
		  (hyperbutton:filter 
		   (cdr thing-assoc)
		   (function (lambda (regexp-assoc)
			       (not (equal (car regexp-assoc) REGEXP))))))))
	 (symbol-value ALIST))
	(function (lambda (thing-assoc)
		    (not (null (cdr thing-assoc))))))))

(defun undefine-global-hyperbutton (THING REGEXP)
  "Remove global hyperbutton entry for THING matching REGEXP."
  (hyperbutton:undefine-hyperbutton 
   'hyperbutton:global-button-alist THING REGEXP))

(defun undefine-local-hyperbutton (THING REGEXP)
  "Remove local hyperbutton entry for THING matching REGEXP."
  (hyperbutton:undefine-hyperbutton 
   'hyperbutton:local-button-alist THING REGEXP))

;;=== That's all folks ====================================================

(run-hooks 'hyperbutton-load-hooks)

;;=== END of hyperbutton.el ===============================================


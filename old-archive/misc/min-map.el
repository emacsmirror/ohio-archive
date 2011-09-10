; 
; FSF Emacs 19 provides `minor-mode-map-alist' that allows minor modes
; to use keymaps.  This feature is still missing in Lucid Emacs and
; Emacs 18.  If you have a minor mode you want to keep portable, you can
; use this file as a stop-gap until Lucid Emacs implements
; `minor-mode-map-alist' and Emacs 18 compatibility is no longer
; important. 
; 
; If you are writing a non-trivial minor mode, consider using Inge
; Frick's <inge@nada.kth.se> `min-bind' package instead.  It provides
; much better support for minor modes than `min-map', and avoids some
; serious design flaws in the FSF 19 `minor-mode-map-alist'.  However
; the price is a much larger and more complex package, and a different
; interface than FSF 19.
; 
; ------------------------------------------------------------
; 
;;; min-map.el -- Support for FSF emacs 19 style minor mode keymaps.

;; Copyright (C) 1993 Per Abrahamsen.
;; Copyright abandoned.  This file is donated to the public domain.

;; Author: Per Abrahamsen <abraham@iesd.auc.dk>
;; Version: $Id: min-map.el,v 5.1 1994/01/11 00:32:33 amanda Exp $
;; Bogus-Bureaucratic-Cruft: How 'bout ESR and the LCD people agreed
;; 	on a common format?

;; LCD Archive Entry:
;; min-map|Per Abrahamsen|abraham@iesd.auc.dk|
;; Support for FSF emacs 19 style minor mode keymaps|
;; 11-Jan-1994|5.1|~/misc/min-map.el.Z|

;;; Commentary:

;; This package emulates FSF emacs 19 style minor mode keymaps in
;; emacs 18 and Lucid Emacs 19.

;; To use the package, simple write your mode as you would have done
;; under FSF Emacs 19, except that each time you want a change to an
;; active keymap to take effect, you must call the function
;; `minor-mode-rehash'. 

;; minor-mode-rehash 
;;   merge all active keymaps listed in `minor-mode-map-alist' into
;;   the major mode keymap under emacs 18 and Lucid Emacs 19.  

;; minor-call-shadow MODE KEY
;;   Call the function shadowed by MODE for KEY interactively.

;; minor-define-key MODE MAP KEY VALUE
;;   Optional alternative to `define-key'.  The dfifference is that
;;   `minor-define-key' will accept key to be an atom, list, vector,
;;   or string, and will attempt to translate FSF Emacs 19 key
;;   bindinsg into Emacs 18 or Lucid Emacs key bindings.  The MODE is
;;   use when defining a prefix key, the keymap used for the prefix
;;   key will have the default binding (minor-call-shadow mode key),
;;   that is, the original binding for the prfix will be used.  Pass
;;   'major-mode as MODE if you are not defining a minor mode keymap.

;;; Code:

(cond					; Switch between Emacs versions.

;;; Emacs 18

((< (string-to-int emacs-version) 19)

;; List of all minor mode keymaps
(defvar minor-mode-map-alist nil
  "Alist of keymaps to use for minor modes.
Each element looks like (VARIABLE . KEYMAP); KEYMAP is used to read
key sequences and look up bindings iff VARIABLE's value is non-nil.
If two active keymaps bind the same key, the keymap appearing earlier
in the list takes precedence.")
  
;; We need a variable to keep the original major mode.
(defvar major-mode-keymap nil)
(make-variable-buffer-local 'major-mode-keymap)

(defun minor-mode-rehash ()
  "Merge minor modes and major modes keymaps into a single map."
  (setq major-mode-keymap	;Keep copy of original
	(or major-mode-keymap
	    (current-local-map)
	    (make-sparse-keymap)))
	     

  (let ((keymap (minor-mode-copy-sparse major-mode-keymap))
	(index (reverse minor-mode-map-alist)))
    (while index
      (let* ((entry (car index))
	     (mode (car entry))
	     (map (cdr entry)))
	(setq index (cdr index))
	(if (symbol-value mode)
	    (minor-mode-add-keymap keymap "" map))))
    (use-local-map keymap)))

(defun minor-mode-copy-sparse (keymap)
  ;; Copy KEYMAP to sparse keymap.
  (if (vectorp keymap)
      (let ((map (make-sparse-keymap))
	    (key 0)
	    (length (length keymap)))
	(while (< key length)
	  (let* ((string (char-to-string key))
		 (def (lookup-key keymap string)))
	    (cond ((null def))
		  ((keymapp def)
		   (define-key map string (copy-keymap def)))
		  (t
		   (define-key map string def))))
	  (setq key (1+ key)))
	map)
    (copy-keymap keymap)))

(defun minor-mode-add-keymap (keymap binding map)
  ;; Add to KEYMAP under BINDING the entries in MAP.
  (let ((index (cdr map)))
    (while index
      (let* ((entry (car index))
	     (key (char-to-string (car entry)))
	     (def (cdr entry)))
	(setq index (cdr index))

	(cond ((null def))
	      ((keymapp def)
	       (minor-mode-add-keymap keymap (concat binding key) def))
	      (t
	       (define-key keymap (concat binding key) def)))))))

(defun minor-fsf-key-p (key)
  ;; True iff KEY is a valid Emacs 18 key sequence.
  (let ((found nil))
    (while key
      (if (not (and (numberp (car key))
		    (< (car key) 256)))
	  (setq found t))
      (setq key (cdr key)))
    (not found)))

(defun minor-define-key (mode map key value)
  ;; Safe interface to define-key.
  (if (not (sequencep key))
      (setq key (list key))
    (setq key (append key nil)))

  (if (not (minor-fsf-key-p key))
      ()
    (while (cdr key)
      (if (not (keymapp (lookup-key map (char-to-string (car key)))))
	  (define-key map (char-to-string (car key)) (make-sparse-keymap)))
      (setq map (lookup-key map (char-to-string (car key))))
      (setq key (cdr key)))

    (define-key map (char-to-string (car key)) value)))

(defun minor-call-shadow (mode binding)
  "Temporarily disable MODE while calling BINDING."
  (let ((old (symbol-value mode))
	(prefix nil)
	(events (append binding nil)))
    (if (not (minor-fsf-key-p events))

	()
      (set-variable mode nil)
      (condition-case err
	  (progn 
	    (minor-mode-rehash)
	    (while (or prefix events) 
	      (let ((binding (if (and (= (length prefix) 1)
				      (> (car prefix) 127))
				 'self-insert-command
			       (key-binding (concat prefix)))))
		(cond ((or (null prefix) (keymapp binding))
		       (if events
			   (setq prefix (append prefix (list (car events)))
				 events (cdr events))
			 (setq prefix (append prefix (list (read-char))))))
		      ((eq 'self-insert-command binding)
		       (let ((last-command-char (nth (1- (length prefix))
						     prefix)))
			 (call-interactively 'self-insert-command))
		       (setq prefix nil))
		      ((commandp binding)
		       (call-interactively binding)
		       (setq prefix nil))
		      (t
		       (error "Unhandled binding, `%s' for `%s'."
			      binding prefix))))))
	(error (beep) (message "%s" error)))
      (set-variable mode old)
      (minor-mode-rehash))))

)					; Emacs 18

;;; Lucid Emacs
       
((string-match "Lucid" emacs-version)

;; List of all minor mode keymaps
(defvar minor-mode-map-alist nil
  "Alist of keymaps to use for minor modes.
Each element looks like (VARIABLE . KEYMAP); KEYMAP is used to read
key sequences and look up bindings iff VARIABLE's value is non-nil.
If two active keymaps bind the same key, the keymap appearing earlier
in the list takes precedence.")

;; We need a variable to keep the original major mode.
(defvar major-mode-keymap nil)
(make-variable-buffer-local 'major-mode-keymap)

(defun minor-mode-rehash ()
  "Merge minor modes and major modes keymaps into a single map."
  (setq major-mode-keymap;Keep copy of original
	(or major-mode-keymap
	    (current-local-map)
	    (make-sparse-keymap)))

  (let ((keymap (make-sparse-keymap))
	(index (reverse minor-mode-map-alist)))
    (minor-mode-add-keymap 'major-mode keymap [] major-mode-keymap)
    (while index
      (let* ((entry (car index))
	     (mode (car entry))
	     (map (cdr entry)))
	(setq index (cdr index))
	(if (symbol-value mode)
	    (minor-mode-add-keymap mode keymap [] map))))
    (use-local-map keymap)))

(defun minor-mode-add-keymap (mode keymap binding map)
  ;; In MODE, add to KEYMAP under BINDING the entries in MAP.
  (if (keymap-parent map)
      (minor-mode-add-keymap mode keymap binding (keymap-parent map)))
  (map-keymap 'minor-mode-add-binding map))

(defun minor-mode-add-binding (key value)
  ;; Bind KEY to VALUE.  in the free variable KEYMAP using the
  ;; prefix BINDING for MODE.
  (let ((prefix (apply 'vector (append binding (list key)))))
    (if (keymapp value)
	(minor-mode-add-keymap mode keymap prefix value)
      (minor-define-key mode keymap prefix value))))

(defun minor-define-key (mode map key value)
  ;; Safe interface to define-key.
  (setq key (mapcar 'minor-fsf-to-lucid-key
		    (if (sequencep key) (append key nil) (list key))))
  
  (while (cdr key)
    (if (not (keymapp (lookup-key map (vector (car key)))))
	(let ((new (make-sparse-keymap))
	      (fun (list 'lambda nil
			 (list 'interactive)
			 (list 'minor-call-shadow
			       (list 'quote mode)
			       (list 'this-command-keys)))))
	  (define-key map (vector (car key)) new)
	  (define-key new [default] fun)))

    (setq map (lookup-key map (vector (car key))))
    (setq key (cdr key)))

  (define-key map (car key) value))

(defun minor-call-shadow (mode binding)
  "Temporarily disable MODE while calling BINDING."
  (let ((old (symbol-value mode))
	(fun nil)
	(prefix nil)
	(events (mapcar 'minor-fsf-to-lucid-key (append binding nil))))
    (set-variable mode nil)
    (condition-case err
	(progn
	  (minor-mode-rehash)
	  (while (or prefix events) 
	    (let ((binding (or (key-binding (apply 'vector prefix))
			       fun)))
	      (cond ((or (null prefix) 
			 (keymapp binding))
		     (if (keymapp binding)
			 (setq fun (lookup-key binding [default])))
		     (if events
			 (setq prefix (append prefix (list (car events)))
			       events (cdr events))
		       (setq prefix (append prefix (list (read-event))))))
		    ((eq 'self-insert-command binding)
		     (let ((last-command-char
			    (minor-lucid-to-fsf-key (nth (1- (length prefix))
							 prefix))))
		       (call-interactively 'self-insert-command))
		     (setq prefix nil))
		    ((commandp binding)
		     (call-interactively binding)
		     (setq prefix nil))
		    (t
		     (error "Unhandled binding, `%S' for `%S'."
			    binding prefix))))))
      (error (beep) (message "%s" error)))
    (set-variable mode old)
    (minor-mode-rehash)))

(defun minor-fsf-to-lucid-key (number)
  ;; Convert NUMBER to X event.
  (cond ((not (numberp number))
	 number)
	((< number 27)			;Letters are automatically downcased.
	 (list 'control (+ number ?`)))
	((< number 32)			;Symbols are not!
	 (list 'control (+ number ?@)))
	((or (> number 255) (and (> number 127) (< number 160)))
	 'unhandled-fsf-event)
	(t number)))

(defun minor-lucid-to-fsf-key (key)
  ;; Convert X event to number
  (cond ((numberp key)
	 key)
	((eq (car-safe key) 'control)
	 (mod (cdr-safe key) 32))
	(t 0)))

)					; Lucid Emacs

;;; FSF Emacs 19
      
(t					; FSF Emacs 19

(defun minor-mode-rehash ()
  "Merge minor modes and major modes keymaps into a single map.")

(defun minor-define-key (mode map binding value)
  ;; Safe interface to define-key.
  (let ((key (if (sequencep binding)
		 (append binding nil)
	       (list binding))))
    (while (cdr key)
      (if (not (keymapp (lookup-key map (vector (car key)))))
	  (let ((new (make-sparse-keymap))
		(fun (list 'lambda nil
			   (list 'interactive)
			   (list 'minor-call-shadow
				 (list 'quote mode)
				 (list 'this-command-keys)))))
	    (define-key map (vector (car key)) new)
	    (define-key new [t] fun)))
      (setq map (lookup-key map (vector (car key))))
      (setq key (cdr key)))

  (define-key map (vector (car key)) value)))

(defun minor-call-shadow (mode binding)
  "Temporarily disable MODE while calling BINDING."
  (let ((old (symbol-value mode))
	(prefix nil)
	(events (append binding nil)))
    (set-variable mode nil)
    (condition-case error
	(while (or prefix events) 
	  (let ((binding (key-binding (apply 'vector prefix) t)))
	    (cond ((keymapp binding)
		   (if events
		       (setq prefix (append prefix (list (car events)))
			     events (cdr events))
		     (setq prefix (append prefix (list (read-event))))))
		  ((eq 'self-insert-command binding)
		   (let ((last-command-char (nth (1- (length prefix))
						 prefix)))
		     (call-interactively 'self-insert-command))
		   (setq prefix nil))
		  ((commandp binding)
		   (call-interactively binding)
		   (setq prefix nil))
		  (t
		   (error "Unhandled binding, `%S' for `%S'."
			  binding prefix)))))
      (error (beep) (message "%s" error)))
    (set-variable mode old)))

)					; FSF Emacs 19

)					; Cond

(provide 'min-map)

;;; min-map.el ends here

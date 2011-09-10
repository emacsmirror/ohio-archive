;;; -*-Mode:Lisp-Interaction -*-

;;; This file contains the code for a reasonable describe function.  The author
;;; wrote it up himself by looking at the type functions available in GNU
;;; Emacs.  This is a bit different from the Common Lisp describe function in
;;; that it also describes details of objects like keymaps and buffers.

(defun describe (object)
  "Print out a description of an object."
  (terpri)
  (princ object)
  (princ " is ")
  (cond ((null object)
	 (princ "nil."))
	((integerp object)
	 (princ "an integer."))
	((bufferp object)
	 (describe-buffer-internal object))
	;; Must put this before lists and vectors
	((keymapp object)
	 (describe-keymap-internal object))
	((processp object)
	 (describe-process-internal object))
	((markerp object)
	 (describe-marker-internal object))
	((symbolp object)
	 (describe-symbol-internal object))
	((windowp object)
	 (describe-window-internal object))
	((stringp object)
	 (princ (format "a string of length %d." (length object))))
	((listp object)
	 (princ (format "a list of length %d." (length object))))
	((vectorp object)
	 (princ (format "a vector of length %d." (length object))))
	((functionp object)
	 (describe-function-internal object))
	(t
	  (princ "an unknown object.")))
  object)

(defun describe-buffer-internal (object)
  "Print out a detailed description of a buffer object."
  (princ "a buffer object")
  (if (eq object (current-buffer))
      (princ " for the current buffer"))
  (princ ".")
  (let ((old-buffer (current-buffer))
	read-only)
    (setq current-buffer object)
    (princ (format "\nBuffer %s been modified and contains %d characters."
		   (if (buffer-modified-p object)
		       "has"
		     "has not")
		   (buffer-size)))
    (setq read-only buffer-read-only)
    (setq current-buffer old-buffer)
    (if read-only
	(princ (format "\nBuffer is read only."))))
  (princ (format "\nFile name for buffer is %s." (buffer-file-name object))))

(defun describe-marker-internal (object)
  "Print out a detailed description of a marker object."
  (princ (format "a marker object for the buffer %s."
		 (buffer-name (marker-buffer object))))
  (princ (format "\nMarker is at position %d."
		 (marker-position object))))

(defun describe-symbol-internal (object)
  "Print out a detailed description of a symbol, complete with its
name, properties, function definition and value."
  (princ (format "a symbol with the name %s."
		 (symbol-name object)))
  (if (boundp object)
      (progn
	(princ "\nThe value of the symbol is ")
	(princ (symbol-value object))
	(princ "."))
    ;;ELSE
    (princ "\nSymbol has no value."))
  (if (symbol-plist object)
      (progn
	(princ "\nSymbol has the following properties:")
	(let ((properties (symbol-plist object))
	      prop)
	  (while (not (null properties))
	    (setq prop (car properties))
	    (princ (format "\n  %s - " (symbol-name prop)))
	    (cond ((eq prop 'variable-documentation)
		   (princ (documentation-property 'obarray
						  'variable-documentation))
		   (terpri))
		  (t
		    (princ (car (cdr properties)))))
	    (setq properties (cdr (cdr properties))))))
    ;;ELSE
    (princ "\nSymbol has no properties."))
  (if (fboundp object)
      (progn
	(princ "\nSymbol has ")
	(describe-function-internal (symbol-function object)))))

(defun describe-process-internal (object)
  "Print out a detailed description of a process object."
  (princ "a process with ")
  (princ (if (process-name object)
	     (format "a name of %s"
		     (process-name object))
	   "no name"))
  (princ ".")
  (princ (format "\nProcess status is %s, process id is %d."
		 (process-status object)
		 (process-id object))))

(defun describe-window-internal (object)
  "Print out a detailed description of a window object."
  (princ
    (if (eq (selected-window) object)
	"the currently selected window"
      "a window"))
  (princ ".")
  (princ (format "\nWindow's width is %d and height is %d."
		 (window-width object) (window-height object)))
  (princ (format "\nWindow starts at character position %d."
		 (window-start object)))
  (let ((edges (window-edges object)))
    (princ 
      (format 
	"\nWindow's edges are: top is %d, bottom is %d, left is %d, right is %d."
	(nth 1 edges) (nth 3 edges) (nth 0 edges) (nth 2 edges)))))

(defun describe-keymap-internal (object)
  "Print out a detailed description of a keymap."
  (princ "a ")
  (if (listp object)
      (princ "sparce "))
  (let ((keymap-length (if (listp object)
			   (1- (length object))
			 (length object))))
    (princ (format "keymap with %d key definitio%s."
		   keymap-length
		   (if (= keymap-length 1)
		       "n"
		     "ns"))))
  (princ "\nKey definitions are:")
  (if (listp object)
      ;; This is a sparse keymap.
      (let ((keys (cdr object)))
	(while (not (null keys))
	  (terpri)
	  (princ (character-name (car (car keys))))
	  (princ "\t")
	  (princ (cdr (car keys)))
	  (setq keys (cdr keys))))
    ;;ELSE
    ;; This is the vector form of a keymap.
    (let ((char-index 0)
	  (keymap-length (length object)))
      (while (< char-index keymap-length)
	(terpri)
	(princ (character-name char-index))
	(princ "\t")
	(princ (aref object char-index))
	(setq char-index (1+ char-index))))))

(defun character-name (character)
  "Return a the name of a character."
  (cond ((> character ?\ )
	 (char-to-string character))
	((= character 0)
	 "null")
	((= character ?\b)
	 "backspace")
	((= character ?\e)
	 "escape")
	((= character ?\f)
	 "form feed")
	((= character ?\n)
	 "newline")
	((= character ?\r)
	 "return")
	((= character ?\t)
	 "tab")
	((= character ?\v)
	 "vertical tab")
	(t
	  (format "control-%c"
		  (- (+ character ?\a) 1)))))

(defun describe-function-internal (object)
  (princ (format "a function value for %s function "
		 (if (commandp object)
		     (if (subrp object)
			 "an interactive built-in"
		       "an interactive")
		   ;;ELSE
		   (if (subrp object)
		       "a built-in"
		     "a"))))
  (princ object)
  (princ ".")
  (if (documentation object)
      (princ (format "\nFunction has the following documentation:\n  %s\n"
		     (documentation object)))))

(defun functionp (object)
  "Returns t if OBJECT is a function."
  ;; We need to fail on symbols and strings because commandp
  ;; will return t for them.
  (cond ((symbolp object)
	 nil)
	((stringp object)
	 nil)
	((subrp object)
	 ;; We have a built-in function.
	 t)
	((commandp object)
	 ;; We have an interactive function.
	 t)
	((listp object)
	 ;; This should only identify non-interactive, non-built-in functions.
	 (eq (car object) 'lambda))
	(t
	  nil)))


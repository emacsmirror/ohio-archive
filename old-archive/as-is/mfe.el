;;;Date: 19 Nov 87 16:42:57 GMT
;;;From: peter a mroz <steinmetz!moose!mroz@UUNET.UU.NET>
;;;Organization: General Electric CRD, Schenectady, NY
;;;Subject: mfe.el: Forms Entry Mode for GNU Emacs

;;;A while back I posted a query for forms entry mode for GNU Emacs.  I
;;;got some interesting responses but nothing like what I wanted, so I
;;;buckled down and did it meself.  

;;;Mfe.el has a screen generator routine that builds a forms entry buffer
;;;using informational labels and data-entry field information, and a
;;;"forms" mode that allows the user to move around the different fields
;;;with the cursor keys and change values.  When they're all done,
;;;hitting C-c C-c calls your done-hook function.  The done-hook function
;;;can call the routine mfe-get-all-def-info to check for changes in
;;;values.

;;;This is actually in two parts, the first of which is a little test
;;;program to demonstrate what the forms entry stuff can do.  The second
;;;part is the actual forms entry stuff.  It's pretty much self
;;;explanatory and hopefully useful to someone

;;;Enjoy!!

;;;Peter Mroz

;;;testmfe.el:
;;;-----------snip-----------snip-----------snip-----------snip-----------snip----
(setq a '((1 1 "This is the first label")
	  (1 2 "And this is the second")
	  (1 3 "Fourth 1 3")
	  (1 4 "Fifth one 1 4")
	  (1 5 "Sixth 1 5")
	  (4 7 "Values for")
	  (3 8 "X:")
	  (3 9 "Y:")
	  (3 10 "Z:")
	  (3 11 "T:")
	  (40 13 "Miscellaneous Labels" c)
	  (13 14 "Main Title:" r)
	  (13 15 "X Axis:" r)
	  (13 16 "Y Axis:" r)
	  (13 17 "Z Axis:" r)
	  (13 18 "T Axis:" r)
	  (13 19 "Legend:" r)
	  (20 21 "Plot line:" r)
	  (20 22 "Plot points:" r)))
(setq b '((30 1 30 nil a)
	  (30 2 nil "LUMBAR" ("LUMBAR" "LUMBER" "LUMBAGO" "LARGO"))
	  (30 3 10 nil i)
	  (30 4 10 "1.2334E+01" f)
	  (30 5 25 "default value" a)
	  (6 8 15 nil f)
	  (6 9 15 nil f)
	  (6 10 15 nil f)
	  (6 11 15 nil f)
	  (15 14 40 nil a)
	  (15 15 40 nil a)
	  (15 16 40 nil a)
	  (15 17 40 nil a)
	  (15 18 40 nil a)
	  (15 19 40 nil a)
	  (22 21 1 t x)
	  (22 22 1 nil x)))

(defun ok ()
  (message "OK"))

(mfe-screen-gen a b "test" 'ok)
-----------snip-----------snip-----------snip-----------snip-----------snip----

mfe.el:
-----------snip-----------snip-----------snip-----------snip-----------snip----
;; mfe.el: Forms Entry routines for GNU Emacs 10/1/87 Peter Mroz
;; Copyright (C) 1987 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

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

;; These routines allow for forms entry.  These routines consist of two basic 
;; parts:
;;
;;    1. A set of screen generator routines that build the forms entry screen
;;    2. Routines to traverse the forms entry screen and modify values.
;;
;; Input to the screen generator consists of two types of fields, informational
;; labels, and data entry fields.  Each requires the following information:
;;
;;    1. Informational labels 
;;         - x,y screen location, where (0,0) is the upper left hand corner 
;;           of the buffer
;;         - text
;;         - optional justification (l=left (default), c=centered, r=right)
;;
;;    2. Data entry fields
;;         - x,y screen location, where (0,0) is the upper left hand corner
;;           of the buffer
;;         - justification (l=left, c=centered, r=right)
;;         - maximum length
;;         - default value (or none)
;;         - type, indicated by a one letter symbol (not a string): (i)nteger,
;;           (f)loat, (a)lphanumeric, (u)ppercase alphanumeric, (x) for one
;;           character marker selections, or a list of acceptable values
;;         - optional justification (l=left (default), c=centered, r=right)
;;
;; The screen generator also needs a buffer name and the name of a
;; function to call when the user is done entering values.  The 
;; "entry-finished" function is called when the user types C-c C-c.
;;
;; The forms traversal routines assume VT100 cursor keys, which you can
;; modify by setting MFE-HOOK to a routine that binds your keys.  
;;
;; The current approach to modifying a data entry field is to bind every 
;; printable ASCII character except for ? to the function 
;; (MFE-INSERT-CHAR CHARACTER).  The function MFE-INSERT-CHAR checks the 
;; present location of the point against the list of data entry field 
;; coordinates, and checks CHARACTER against the allowed type.  If a list of 
;; allowable types is provided, command completion is performed by hitting the 
;; space bar or <cR>.
;;
;; If the above implementation takes too long for gnu lisp to execute, an 
;; alternate approach would be to maintain information on the current
;; data entry field.  The data that was entered would only be checked upon
;; transition to another field.  The danger of this lies in the possible
;; movement off of the data entry field by globally bound emacs keys, like
;; ^N, ^P, ESC <, etc.

;;(require 'picture)

(defvar mfe-mode-hook nil
  "If non-nil, its value is called on entry to mfe mode")

(defvar mfe-def nil
  "List of Data Entry Field info for a particular buffer (local).  Each
list sublist contains the following information:
     - minimum and maximum point values bounding the field
     - default value (or none)
     - type (i)nteger, (f)loat, (a)lphanumeric, (u)ppercase alphanumeric, or
       list of acceptable values)")
(make-variable-buffer-local 'mfe-def)

(defvar mfe-done-hook nil
  "Routine to call when all done with this form")
(make-variable-buffer-local 'mfe-done-hook)

(defvar mfe-map nil
  "Local keymap to use in mfe mode")

(if mfe-map nil
  (setq mfe-map (make-sparse-keymap))
  (define-key mfe-map "\e[D"     'mfe-left)
  (define-key mfe-map "\e[C"     'mfe-right)
  (define-key mfe-map "\e[B"     'mfe-down)
  (define-key mfe-map "\e[A"     'mfe-up)
  ; Define both types of VT arrow keys
  (define-key mfe-map "\eOD"     'mfe-left)
  (define-key mfe-map "\eOC"     'mfe-right)
  (define-key mfe-map "\eOB"     'mfe-down)
  (define-key mfe-map "\eOA"     'mfe-up)
;;  ; Define default emacs movement keys as well
  (define-key mfe-map "\C-b"     'mfe-left)
  (define-key mfe-map "\C-f"     'mfe-right)
  (define-key mfe-map "\C-n"     'mfe-down)
  (define-key mfe-map "\C-p"     'mfe-up)
  (define-key mfe-map "\r"       'mfe-enter)
  (define-key mfe-map "\t"       'mfe-down)
  (define-key mfe-map "\n"       'mfe-down)
  (define-key mfe-map "\177"     'mfe-delete-backward)
  (define-key mfe-map "\C-d"     'mfe-delete)
  (define-key mfe-map "\e\177"   'mfe-backward-kill-def-word)
  (define-key mfe-map "\ed"      'mfe-kill-def-word)
  (define-key mfe-map "\C-k"     'mfe-kill-def)
  (define-key mfe-map "\C-y"     'mfe-yank)
  (define-key mfe-map " "        'mfe-look-for-completion)
  (define-key mfe-map "\C-c\C-c" 'mfe-done-with-form)
  (define-key mfe-map "\C-a"     'mfe-beginning-of-def)
  (define-key mfe-map "\C-e"     'mfe-eod)
  (define-key mfe-map "\C-t"     'mfe-display-def-type)
  ; Loop for all printable ASCII characters except for ? and DEL
  (let ((i 33)
	(imax 127))
    (while i
      (if (< i imax)
	  (progn
	    (define-key mfe-map (char-to-string i) 'mfe-insert-char)
	    (setq i (1+ i)))
	(setq i nil))))
  ; Override the definition of the question mark
  (define-key mfe-map "?"    'mfe-help))

;------------------------------------------------------------------------------
(defun mfe-mode ()
  "Major mode for forms entry for gnu emacs.  Characters can only be entered in
data entry fields (DEF).  Note that some DEFs will only accept numeric input
Commands are:
    ?            This message             C-t          Display field type
    C-a          Beginning of field       C-k          Kill to end of field
    C-e          End of field             C-y          Yank killed text
    up-arrow     Previous field           left-arrow   Move left 
    down-arrow   Next field	          right-arrow  Move right
    TAB,return   Move to next field
    C-c C-c      Invoke user-supplied finished-with-form function
    space        Look for completion or insert a space if no completion list

Entry to this mode calls the value of mfe-mode-hook if that value is
non-nil."

  (interactive)
  (use-local-map mfe-map)
  (setq major-mode 'mfe-mode
	mode-name "Forms Entry")
  (setq buffer-read-only t)
  (run-hooks 'mfe-mode-hook))

;------------------------------------------------------------------------------
(defun mfe-screen-gen (info-labels def bufname done-hook)
  "Generates a screen for forms entry using INFO-LABELS and DEF (Data Entry
Fields), in BUFNAME.  Calls DONE-HOOK when the user is ready for this form
to be processed (activated by \\[mfe-done-with-form]).  The user's DONE-HOOK
function is responsible for parsing the various data entry fields."
  (get-buffer-create bufname)
  (set-buffer bufname)
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (let (one-def
	 tmp-text
	 x y poslist max-len default-value def-type one-lab just)
    ; Place the information labels
    (while info-labels
      (setq one-lab (car info-labels))
      (mfe-place-text (nth 0 one-lab)     ; x
		      (nth 1 one-lab)     ; y
		      (nth 2 one-lab)     ; text
		      (nth 3 one-lab))    ; optional justification
      (setq info-labels (cdr info-labels)))
    ; Nullify the global variable for the Data Entry Fields
    (setq mfe-def nil)
    ; And now place the data entry fields
    (while def
      (setq one-def (car def))
      (setq x             (nth 0 one-def))
      (setq y             (nth 1 one-def))
      (setq max-len       (nth 2 one-def))
      (setq default-value (nth 3 one-def))
      (setq def-type      (nth 4 one-def))
      (setq just          (nth 5 one-def))
      ; If a list was provided find the element of max length
      (if (listp def-type)
	  (setq max-len (mfe-max-length def-type)))
      (if (eq def-type 'x)
	  (progn
	    (setq max-len 1)
	    (if default-value
		(setq default-value "x"))))
      ; Build the text string to place
      (setq tmp-text (mfe-build-def max-len default-value))
      ; The text placement routine returns the point boundaries of the DEF
      (setq pos-list (mfe-place-text x y tmp-text just))
      ; Build the global variable for the Data Entry Fields 
      (setq mfe-def
	    (append mfe-def
		    (list
		      (list pos-list default-value def-type))))
      (setq def (cdr def))))
  ; All done placing text for now
  (setq mfe-done-hook done-hook)
  ; Move to the first field
  (goto-char (point-min))
  (mfe-down 1)
  (mfe-mode))
  ; Return the value of the DEF structure for debugging
;;  mfe-def)

;------------------------------------------------------------------------------
(defun mfe-place-text (x y text &optional just)
  "Force-moves to the requested X,Y position in the current buffer and writes 
TEXT.  Returns a two element list containing the point minimum and maximum
used for TEXT.  This will only work if this routine is used to place the 
informational labels first, and then the DEFs in order of occurrence.  If 
optional fourth argument JUST is supplied, attempts to justify the text
according to l=left (default), c=center, and r=right"
  (let ((a (count-lines 1 (point-max)))
	start
	end)
    ; Allow ourselves to enter stuff in this buffer
    (setq buffer-read-only nil)
    ; Figure out where to place the text in terms of its justification
    (cond ((eq just 'c)
	   (setq x (- x (/ (length text) 2))))
	  ((eq just 'r)
	   (setq x (- x (length text))))
	  (t nil))
    (goto-char (point-max))
    (if (not (= 0 (current-column)))
	(setq a (1- a)))
    (goto-char (point-min))
    (if (> y a)
	(progn
	  (goto-char (point-max))
	  (insert-char 10 (- y a)))
      (next-line y))
    (if (> 0 x)
	(setq x 0))
    (move-to-column-force x)
    (setq start (point))
    (if (not (eolp))
	; There's something on this line - just write over it.
	(progn
	  (end-of-line 1)
	  (setq end (point))
	  (goto-char start)
	  (delete-char (min (- end start) (length text)))))
    ; OK to write the text now
    (insert text)
    (setq buffer-read-only t)
    ; Return the min and max point values used as a list
    (list start (point))))

;------------------------------------------------------------------------------
(defun mfe-max-length (a)
  "Finds the string of maximum length in a"
  (let ((b 0))
    (while a
      (setq b (max (length (car a)) b))
      (setq a (cdr a)))
    ; Return the maximum string length
    b))

;------------------------------------------------------------------------------
(defun mfe-build-def (maxlen &optional default-text)
  "Builds a data entry field string consisting of MAXLEN underscores.  If
the optional argument DEFAULT-TEXT is supplied, it is filled out to MAXLEN
with trailing underscores"
  (let ((i 0)
	string)
    (if default-text
	(progn
	  (setq string default-text)
	  (setq i (length string))))
    (while (< i maxlen)
      (setq string (concat string "_"))
      (setq i (1+ i)))
    string))

;------------------------------------------------------------------------------
;; The next set of functions are for traversing and entering values on the 
;; screen that we've built
;------------------------------------------------------------------------------
(defun mfe-get-def-info ()
  "Looks at the current location in the buffer and returns a list containing
this def's information, namely:
         - pmin and pmax point boundaries
         - default value (or none)
         - type, indicated by a one letter symbol (not a string): (i)nteger,
           (f)loat, (a)lphanumeric, (u)ppercase alphanumeric, (x) for a marker,
           or list of acceptable values
If the point isn't on a valid field, nil is returned"
  (let ((pos (point))
	pmin pmax
	def-val
	def-type
	(this-def mfe-def)
	found)
    (while (and this-def (not found))
      (setq pmin (caaar this-def))
      (setq pmax (cadaar this-def))
      (if (and (>= pos pmin) (<= pos pmax))
	  ; This is the DEF we've been looking for!!
	  (progn
	    (setq found t)
	    (setq def-val (nth 1 (car this-def)))
	    (setq def-type (nth 2 (car this-def)))))
      (setq this-def (cdr this-def)))
    (if found
	(list pmin pmax def-val def-type)
      nil)))

;------------------------------------------------------------------------------
(defun mfe-left (arg)
  "Moves ARG characters to the left, unless at the leftmost point in a data
entry field (DEF), in which case it moves to the previous DEF"
  (interactive "p")
  (forward-char -1)
  (if (not (mfe-get-def-info))
      (mfe-up 1))
  (if (> arg 1)
      (mfe-left (1- arg))))

;------------------------------------------------------------------------------
(defun mfe-right (arg)
  "Moves ARG characters to the right, unless at the rightmost point in a data
entry field (DEF), in which case it moves to the next DEF"
  (interactive "p")
  (forward-char 1)
  (if (not (mfe-get-def-info))
      (mfe-down 1))
  (if (> arg 1)
      (mfe-right (1- arg))))

;------------------------------------------------------------------------------
(defun mfe-down (arg)
  "Moves ARG data entry fields down, unless at the last DEF in the buffer"
  (interactive "p")
  (let ((pos (point))
	pmin1 pmax1
	pmin2 pmax2
	def-val
	def-type
	(this-def mfe-def)
	next-def
	found
	(direction "down"))
    ; If ARG is negative we're going in the opposite direction
    (if (> 0 arg)
	(progn
	  (setq this-def (reverse mfe-def))
	  (setq direction "up")))
;;    (scratch-debug-print (concat "Going " direction "   pos = "
;;				 (int-to-string pos)))
;;    (scratch-debug-print (concat "arg = " (int-to-string arg)))
    (while (and this-def (not found))
      (setq pmin1 (caaar this-def))
      (setq pmax1 (cadaar this-def))
      ; Check to see if we're at the VERY end or VERY beginning
      (if (and (< 0 arg) (< pos pmin1))
	  (setq found t))
      ; For moving up
      (if (and (> 0 arg) (> pos pmax1))
	  (setq found t))
      (if found
	  (progn
	    (setq pmin2 pmin1)
	    (setq pmax2 pmax1))
	; Otherwise plow through the rest of this stuff
	(setq next-def (cdr this-def))
	(if (and next-def (not found))
	    (progn
	      (setq pmin2 (caaar next-def))
	      (setq pmax2 (cadaar next-def))
	      ; For moving down
	      (if (and (< 0 arg) (>= pos pmin1) (< pos pmin2))
		  ; This is the DEF we've been looking for!!
		  (setq found t))	      
	      ; For moving up
	      (if (and (> 0 arg) (<= pos pmax1) (> pos pmax2))
		  ; This is the DEF we've been looking for!!
		  (setq found t)))
	  ; Otherwise we're at the end or beginning of the buffer
	  (message (concat "Can't move " direction " anymore")))
	(setq this-def next-def))
      ; If we've been successful move to the end of the next field and move
      ; backwards until just before any text
      (if found
	  (progn
;;	    (scratch-debug-print (prin1-to-string
;;				   (list pmin1 pmax1 pmin2 pmax2)))
	    (mfe-end-of-def pmin2 pmax2)
	    ; If there's any argument keep on goin'
	    (if (> arg 1)
		(mfe-down (1- arg))))))))

;------------------------------------------------------------------------------
(defun mfe-end-of-def (pmin pmax)
  "Moves to the end of the text between pmin and pmax"
  (goto-char pmax)
  (if (re-search-backward "[^_]" pmin t)
      (forward-char 1)
    (goto-char pmin)))

;------------------------------------------------------------------------------
(defun mfe-eod ()
  "Interactive version of mfe-end-of-def that has to first find out which def
it's on.  Returns def-info if successful, nil if not."
  (interactive)
  (let ((a (mfe-get-def-info)))
    (if a
	(progn
	  (mfe-end-of-def (car a) (cadr a))
	  a)
      nil)))

;------------------------------------------------------------------------------
(defun mfe-beginning-of-def ()
  "Moves to the beginning of a data entry field.  Returns nil if not on
a DEF, returns def info otherwise"
  (interactive)
  (let ((a (mfe-get-def-info)))
    (if a
	(progn
	  (goto-char (car a))
	  a)
      nil)))

;------------------------------------------------------------------------------
(defun mfe-up (arg)
  "Moves ARG data entry fields up, unless at the first DEF in the buffer"
  (interactive "p")
  (mfe-down -1)
  (if (> arg 1)
      (mfe-up (1- arg))))

;------------------------------------------------------------------------------
(defun mfe-enter ()
  (interactive)
  (mfe-down 1))

;------------------------------------------------------------------------------
(defun mfe-delete-backward (arg)
  "Deletes ARG characters previous to the point.  If not on a valid field or
at the first point in a field takes no action"
  (interactive "p")
  (let ((pos (point))
	(def-data (mfe-get-def-info))
	pmin pmax)
    (if def-data
	(progn
	  (setq pmin (car  def-data))
	  (setq pmax (cadr def-data))
	  (if (< pmin pos)
	      (progn
		(goto-char pmax)
		(setq buffer-read-only nil)
		(insert-char ?_ 1)
		(goto-char pos)
		(delete-backward-char 1)
		(setq buffer-read-only t)
		(if (> arg 1)
		    (mfe-delete-backward (1- arg)))))))))

;------------------------------------------------------------------------------
(defun mfe-delete (arg)
  "Deletes ARG characters following point.  If not on a valid field or
at the end of text in a field takes no action"
  (interactive "p")
  (let ((pos (point))
	def-data)
    ; Calling the mfe-eod function places the point at the end of the DEF text
    (if (setq def-data (mfe-eod))
	(progn
	  (if (> (point) pos)
	      (progn
		(setq buffer-read-only nil)
		(insert-char ?_ 1)
		(goto-char pos)
		(delete-char 1)
		(setq buffer-read-only t)
		(if (> arg 1)
		    (mfe-delete (1- arg)))))))))

;------------------------------------------------------------------------------
(defun mfe-look-for-completion (arg)
  (interactive "p")
  (let ((a (mfe-get-def-info))
	comp-list)
    (if (and a (listp (setq comp-list (nth 3 a))))
	; It's a list so try to do completions with the string that's there 
	; already
	(progn
	  (let ((pmin (car a))
		(pmax (cadr a))
		(pos (point))
		def-string
		completions)
	    (setq completion-ignore-case t)
	    (setq def-string (buffer-substring pmin (point)))
	    ; See what we can complete
	    (setq completions
		  (try-completion def-string
				   (mfe-make-alist comp-list)))
	    (cond ((or (eq completions t)
		       (null completions))
		   (message "Can't find completion for \"%s\"" def-string))
		  ; For the case of a completion longer than def-string
		  ((not (string= def-string completions))
		   (setq buffer-read-only nil)
		   (delete-region pmin (+ pmin (length completions)))
		   (insert completions)
		   (setq buffer-read-only nil))
		  ; Need to display a completion list to resolve things
		  (t
		    (setq completions
			  (all-completions def-string
					   (mfe-make-alist comp-list)))
		    (with-output-to-temp-buffer "*Help*"
		      (display-completion-list completions)
		      (message "Type C-x 1 to remove completions window"))))))

      ; Otherwise just try and insert arg spaces
      (mfe-insert-char arg))))

;------------------------------------------------------------------------------
(defun mfe-insert-char (arg)
  "Insert ARG characters into the MFE buffer, where the character is the last
one typed.  Checks the data entry field for validity and type"
  (interactive "p")
  (let ((pos (point))
	def-data pmax def-type ok-match
	(err-message "integers")
	(char-to-insert (char-to-string last-input-char)))
    ; Calling mfe-eod moves the point to the end of any text in the DEF
    (if (setq def-data (mfe-eod))
	(progn
	  ; Check to see whether or not we can even insert characters anymore
	  (setq pmax (cadr def-data))
	  (if (= (point) pmax)
	      (message
		"Can't insert characters - at end of field or field is full")
	    ; OK to insert - check the type 
	    (setq def-type (nth 3 def-data))
	    ; Look for alphanumerics or a list of possibles first
	    (cond ((or (eq def-type 'a) (listp def-type))
		   (setq ok-match t))
		  ; Uppercase only
		  ((eq def-type 'u)
		   (setq char-to-insert (upcase char-to-insert))
		   (setq ok-match t))
		  ; Integers
		  ((eq def-type 'i)
		   (setq ok-match (string-match "[0-9---]" char-to-insert)))
		  ; Floating point numbers
		  ((eq def-type 'f)
		   (setq err-message "floating point numbers")
		   (setq ok-match 
			 (string-match "[0-9---+eE.]" char-to-insert)))
		  ; x marker for any character
		  ((eq def-type 'x)
		   (setq ok-match t)
		   (setq char-to-insert "x"))
		  (t (message "Error in your DEF definition")))
	    (if ok-match
		(progn
		  (setq buffer-read-only nil)
		  (delete-char 1)
		  (goto-char pos)
		  (insert char-to-insert)
		  (setq buffer-read-only t)
		  (if (> arg 1)
		      (mfe-insert-char (1- arg))))
	      (message (concat "Try again - this field is for " 
			       err-message " only")))))
      ; Otherwise we're not even on a valid field
      (message "Not on a valid field for input"))))

;------------------------------------------------------------------------------
(defun mfe-help ()
  (interactive)
  (describe-function 'mfe-mode))

;------------------------------------------------------------------------------
(defun mfe-make-alist (a)
  "Takes a list of things A and makes each item an alist.  For example, if the
input list A were '(a b c), this routine would return '((a) (b) (c))."
  (let (b)
    (while (and a (listp a))
      (setq b (append b (list (list (car a)))))
      (setq a (cdr a)))
    b))

;------------------------------------------------------------------------------
(defun mfe-backward-kill-def-word (arg)
  "Kills ARG words previous to point in a def"
  (interactive "p")
  (mfe-kill-def-word -1)
  (if (> arg 1)
      (mfe-backward-kill-def-word (1- arg))))

;------------------------------------------------------------------------------
(defun mfe-kill-def-word (arg)
  "Kills ARG words ahead of point in a def.  If ARG is negative kills 
backwards"
  (interactive "p")
  (let ((def-info (mfe-get-def-info)))
    (if def-info
	(progn	
	  (let ((pmin (car def-info))
		(pmax (cadr def-info))
		(pos (point))
		end)
	    ; Move forward or backwards ARG words and take note of where we are
	    (forward-word arg)
	    (if (and (> arg 0) (< pmax (point)))
		(goto-char pmax))
	    (if (and (< arg 0) (> pmin (point)))
		(goto-char pmin))
	    (setq end (point))
	    ; Check to see if we've really gone anywhere
	    (if (/= pos end)
		(progn
		  (goto-char pmax)
		  (setq buffer-read-only nil)
		  (insert-char ?_ (abs (- pos end)))
		  ; Delete those words FINALLY
		  (kill-region pos end)
		  (setq buffer-read-only t)
		  ; Move to the proper place
		  (goto-char pos)
		  (if (< arg 0)
		      (goto-char end)))))))))

;------------------------------------------------------------------------------
(defun mfe-kill-def ()
  "Kill the rest of the def from point to the end"
  (interactive)
  (let ((pos (point))
	a end)
    (if (setq a (mfe-eod))
	(progn
	  (setq end (point))
	  (setq buffer-read-only nil)
	  (kill-region pos end)
	  (insert-char ?_ (- end pos))
	  (setq buffer-read-only t)
	  (goto-char pos)))))

;------------------------------------------------------------------------------
(defun mfe-yank ()
  "Yanks the last kill from the kill ring and inserts as much as it can onto
the present def"
  (interactive)
  (mfe-insert-string (car kill-ring)))

;------------------------------------------------------------------------------
(defun mfe-insert-string (a)
  "Inserts as much of the string A onto the present data entry field as 
possible"
  (let ((pos (point))
	(alen (length a))
	pmin pmax b end blen)
    (if (setq b (mfe-eod))
	(progn
	  (setq end (point))
	  (setq pmin (car b))
	  (setq pmax (cadr b))
	  ; Find out how much we can insert
	  (setq blen (min alen (- pmax end)))
	  (if (> blen 0)
	      (progn
		(setq buffer-read-only nil)
		(delete-char blen)
		(goto-char pos)
		(insert (substring a 0 blen))
		(setq buffer-read-only t)))))))

;------------------------------------------------------------------------------
(defun abs (a)
  "Takes the absolute value of a, which should be an integer"
  (if (integerp a)
      (if (< a 0)
	  (- a)
	a)))

;------------------------------------------------------------------------------
(defun mfe-get-all-def-info ()
  "Called by the user's function - goes through the entire form and returns
a list of strings.  If there is no entry for a field nil is returned.  
Doesn't check the results - just gets them"
  (save-excursion
    (let ((pos (point))
	  (this-def mfe-def)
	  pmin pmax all-def-info def-string)
      (while this-def
	(setq pmin (caaar  this-def))
	(setq pmax (cadaar this-def))
	(mfe-end-of-def pmin pmax)
	(if (= pmin (point))
	    ; If we're sitting on the first field there's nothing here
	    (setq def-string nil)
	  ; Otherwise capture the string that's there
	  (setq def-string (buffer-substring pmin (point))))
	(setq all-def-info (append all-def-info (list def-string)))
	(setq this-def (cdr this-def)))
      all-def-info)))

;------------------------------------------------------------------------------
(defun mfe-display-def-type ()
  "Displays what type of DEF this is: (a)lphanumeric, (u)ppercase, (l)ist,
 (i)nteger, or (f)loating point"
  (interactive)
  (let (def-data
	 (info-message "alphanumeric data"))
    (if (setq def-data (mfe-get-def-info))
	(progn
	  (setq def-data (nth 3 def-data))
	  (cond ((listp def-data)
		 (setq info-message
		       (concat "one of: " 
			       (mfe-concat-list def-data))))
		; Uppercase only
		((eq def-data 'u)
		 (setq info-message "uppercase alphanumeric data"))
		; Integers
		((eq def-data 'i)
		 (setq info-message "integers"))
		; Floating point numbers
		((eq def-data 'f)
		 (setq info-message "floating point numbers"))
		; x marker
		((eq def-data 'x)
		 (setq info-message "an x selection marker")))
	  (message (concat "This field is for " info-message)))
      ; Otherwise we're not even on a valid field
      (message "Not on a valid field for input"))))

;------------------------------------------------------------------------------
(defun mfe-done-with-form ()
  "Calls the users done-hook"
  (interactive)
  (funcall mfe-done-hook))

;------------------------------------------------------------------------------
(defun move-to-column-force (column)
  "Move to column COLUMN in current line.  Differs from move-to-column
in that it creates or modifies whitespace if necessary to attain
exactly the specified column.  Pulled out of picture.el"
  (move-to-column column)
  (let ((col (current-column)))
    (if (< col column)
	(indent-to column)
      (if (and (/= col column)
	       (= (preceding-char) ?\t))
	  (let (indent-tabs-mode)
	    (delete-char -1)
            (indent-to col)
            (move-to-column column))))))

;------------------------------------------------------------------------------
(defun mfe-concat-list (a)
  "Returns A unless it's a list, in which case it concatenates all of the
elements together, separated by spaces.  Works for both lists of strings 
and atoms"
  (if (listp a)
      (progn
	(let (b)
	  (while a
	    (if (stringp (car a))
		(setq b (concat b (car a) " "))
	      (setq b (concat b (symbol-name (car a)) " ")))
	    (setq a (cdr a)))
	  b))
    ; else it's not a list
    a))

;------------------------------------------------------------------------------
; Miscellaneous Utility Functions
;------------------------------------------------------------------------------
(defun cadr (x) (car (cdr x)))
;------------------------------------------------------------------------------
(defun caadr (x) (car (car (cdr x))))
;------------------------------------------------------------------------------
(defun caddr (x) (car (cdr (cdr x))))
;------------------------------------------------------------------------------
(defun caaddr (x) (car (car (cdr (cdr x)))))
;------------------------------------------------------------------------------
(defun cadaar (x) (car (cdr (car (car x)))))
;------------------------------------------------------------------------------
(defun caddar (x) (car (cdr (cdr (car x)))))
;------------------------------------------------------------------------------
(defun cadddr (x) (car (cdr (cdr (cdr x)))))
;------------------------------------------------------------------------------
(defun cadar (x) (car (cdr (car x))))
;------------------------------------------------------------------------------
(defun cdar (x) (cdr (car x)))
;------------------------------------------------------------------------------
(defun cddr (x) (cdr (cdr x)))
;------------------------------------------------------------------------------
(defun cdddr (x) (cdr (cdr (cdr x))))
;------------------------------------------------------------------------------
(defun caar (x) (car (car x)))
;------------------------------------------------------------------------------
(defun caaar (x) (car (car (car x))))
;;;-----------snip-----------snip-----------snip-----------snip-----------snip----

;;;Peter Mroz
;;;General Electric			| ARPA: mroz@ge-crd.arpa
;;;Corporate Research and Development	| UUCP: mroz@moose.steinmetz.ge.com
;;;PO Box 8, 37-2081			| UUCP: {uunet!}steinmetz!mroz!crd
;;;Schenectady, NY 12301			| 518-387-6021

;;; Mathematica notebook interface.
;;; Copyright 1989 Daniel LaLiberte

;;; $Header: /home/srg/1/liberte/math/RCS/math-mode.el,v 1.5 89/05/24 23:43:00 liberte Exp Locker: liberte $
;;; $Log:	math-mode.el,v $
;;; Revision 1.5  89/05/24  23:43:00  liberte
;;; Fix math-package-mode self-insert-command.
;;; Fix adjustment of indentation for closed cells.
;;; 
;;; Revision 1.4  89/05/24  02:13:37  liberte
;;; Fix problem with missing output prompts.
;;; Fix cell relabelling problem for closed cells.
;;; 
;;; Revision 1.3  89/05/22  00:03:24  liberte
;;; Add math-package-mode.
;;; Fix save of math buffer some more - always set write-file-hook.
;;; Fix indentation of strings.
;;; 

;;------------------------------------------------------------
;; To use math-mode, include the following in your .emacs
;; Make sure that math-mode.el and mathtalk.el are in your load-path.
;; Also make sure that math-path is set to the name of the
;; program to execute the emacsfrontend and that that program
;; is in your Unix PATH variable.

;; (setq auto-mode-alist (append
;; 		       (list
;;			(cons "\\.nb$" 'math-mode)
;;			(cons "\\.nbl$" 'math-mode))
;;			auto-mode-alist))

;; (autoload 'math-mode "math-mode")
;; (autoload 'math "math-mode" "start up mathematica in math buffer" t)

;;-------------------------------------------------------------
;; Start up math-mode by executing M-x math
;; or edit a file with .nb or .nbl suffix.   The .nb suffix
;; is for notebook files created with the Macintosh version
;; of Mathematica.  These files are automatically converted
;; to a file with the .nbl suffix that uses the math-mode notebook format.

;; Once in math-mode, use describe-mode (C-h m) to learn
;; about math-mode commands.  You may want to read the manual
;; to learn about the structure of notebooks and evaluation
;; of Mathematica expressions.

;;-------------------------------------------------------------

;; Byte compiling will speed up execution enormously because math-mode
;; uses macros that use macros.

(provide 'math-mode)
(require 'math-mode)  ; must load for byte-compile so cell macros are defined

;;; uses mathtalk to communicate with Mathematica
(require 'mathtalk)


;;------------------
;; Math mode

(defvar math-eval-init-cells 'ask
  "*nil if should never evaluate initialization cells.
t if should always evaluate without asking.
Otherwise ask.")

(defvar math-use-structure-labels nil
  "*If non-nil, then include within each label the cell structure indicator.
Buffer local.  Default nil.")

(defconst math-buffer-name "math" 
  "Name of buffer for doing math in.")

(defconst math-macnb2nbl "macnb2nbl"
  "Program to convert macintosh notebooks to nbl form.")

;;(defvar math-save-notebook-lisp t
;;  "*Non-nil if notebooks should be saved in lisp form.")

;; (defvar math-save-notebook-text nil
;;  "*Non-nil if notebooks should also be saved in text form.")

(defvar math-show-debug nil
  "*If non-nil, show the mathematica debug buffer.")

(defvar math-mode-map nil "")

(if (and math-mode-map   ; do this just once
	 (not math-show-debug)) ; unless debugging
    nil

  (progn
    (setq math-mode-map (make-keymap))

    (let ((i 32))
      (while (< i 127)
	(define-key math-mode-map (char-to-string i) 'math-self-insert-command)
	(setq i (1+ i)))
      )

    (define-key math-mode-map "\C-cK" 'kill-math)

    ;; could combine into one math-save-buffer and request format
    (define-key math-mode-map "\C-cM" 'write-math-mac-notebook)
    (define-key math-mode-map "\C-cP" 'write-math-package)
    ;;(define-key math-mode-map "\C-xs" 'write-math-lisp-notebook) ; not needed

    (define-key math-mode-map "\C-c\C-n" 'math-next-cell)
    (define-key math-mode-map "\C-c\C-p" 'math-previous-cell)
    (define-key math-mode-map "\C-c\C-u" 'math-parent-cell)
    (define-key math-mode-map "\C-c\C-f" 'math-forward-cell)
    (define-key math-mode-map "\C-c\C-b" 'math-backward-cell)
    (define-key math-mode-map "\C-c\C-i" 'math-insert-new-text-cell)
    (define-key math-mode-map "\C-c\C-k" 'math-kill-current-cell)
    (define-key math-mode-map "\C-ck" 'math-kill-region)
    (define-key math-mode-map "\C-c\C-w" 'math-set-current-cell-as-kill)
    (define-key math-mode-map "\C-c\C-y" 'math-yank-killed-cells)
    (define-key math-mode-map "\C-c\C-@" 'math-mark-current-cell)
    (define-key math-mode-map "\C-cc" 'math-copy-backward-cell)
    (define-key math-mode-map "\C-cg" 'math-group-region)
    (define-key math-mode-map "\C-cG" 'math-ungroup-current-group)
    (define-key math-mode-map "\C-c\C-s" 'math-split-group)
    (define-key math-mode-map "\C-c\C-j" 'math-join-cell)

    (define-key math-mode-map "\C-c\C-o" 'math-toggle-cell-output)
    (define-key math-mode-map "\C-c\C-c" 'math-toggle-close-cell)
    (define-key math-mode-map "\C-c\C-l" 'math-toggle-structure-labels)

    (define-key math-mode-map "\C-cA" 'math-toggle-active)
    (define-key math-mode-map "\C-cI" 'math-toggle-initialization)
    (define-key math-mode-map "\C-cU" 'math-toggle-auto-active)
    (define-key math-mode-map "\C-cF" 'math-set-style)

    (define-key math-mode-map "\C-cc" 'math-complete-symbol)
    (define-key math-mode-map "\C-j" 'math-eval-current-cell)

    (define-key math-mode-map "\C-m" 'math-newline-and-indent)
    (define-key math-mode-map "\t" 'math-maybe-indent-line)
    (define-key math-mode-map "\M-\C-q" 'math-indent-exp)

    ;;  (define-key math-mode-map "\C-cC" 'check-math-cell)

    ))

(defvar math-mode-syntax-table nil)

(if math-mode-syntax-table
    ()
  (setq math-mode-syntax-table (make-syntax-table))
  (let ((i 0))
    (while (< i 128)			; control chars
      (modify-syntax-entry i "_   " math-mode-syntax-table)
      (setq i (1+ i)))
    )

  (modify-syntax-entry ?\  "    " math-mode-syntax-table)
  (modify-syntax-entry ?\t "    " math-mode-syntax-table)
  (modify-syntax-entry ?\n "    " math-mode-syntax-table)
  (modify-syntax-entry ?\r "    " math-mode-syntax-table)
  (modify-syntax-entry ?\f "    " math-mode-syntax-table)

  (modify-syntax-entry ?\[ "(]  " math-mode-syntax-table)
  (modify-syntax-entry ?\] ")[  " math-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}  " math-mode-syntax-table)
  (modify-syntax-entry ?\} "){  " math-mode-syntax-table)
  (modify-syntax-entry ?\( "() 1" math-mode-syntax-table)
  (modify-syntax-entry ?\) ")( 4" math-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" math-mode-syntax-table)

  (modify-syntax-entry ?+ "." math-mode-syntax-table)
  (modify-syntax-entry ?- "." math-mode-syntax-table)
  (modify-syntax-entry ?= "." math-mode-syntax-table)
  (modify-syntax-entry ?< "." math-mode-syntax-table)
  (modify-syntax-entry ?> "." math-mode-syntax-table)
  (modify-syntax-entry ?| "." math-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" math-mode-syntax-table)
  (modify-syntax-entry ?/ ".   " math-mode-syntax-table)
  (modify-syntax-entry ?' ".   " math-mode-syntax-table)
  (modify-syntax-entry ?` "'   " math-mode-syntax-table)
  (modify-syntax-entry ?, ".   " math-mode-syntax-table)
  (modify-syntax-entry ?. ".   " math-mode-syntax-table)
  (modify-syntax-entry ?; ".   " math-mode-syntax-table)
  (modify-syntax-entry ?: ".   " math-mode-syntax-table)
  (modify-syntax-entry ?_ "'   " math-mode-syntax-table)
  (modify-syntax-entry ?\? ".   " math-mode-syntax-table)
  (modify-syntax-entry ?~ "_   " math-mode-syntax-table)

  (modify-syntax-entry ?!".   " math-mode-syntax-table)
  (modify-syntax-entry ?@ ".   " math-mode-syntax-table)
  (modify-syntax-entry ?# "'   " math-mode-syntax-table)
  (modify-syntax-entry ?$ "'   " math-mode-syntax-table)
  (modify-syntax-entry ?% "'" math-mode-syntax-table)
  (modify-syntax-entry ?^ "." math-mode-syntax-table)
  (modify-syntax-entry ?& "." math-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\   " math-mode-syntax-table)
  )


(defvar math-mode nil
  "Non-nil if math-mode is active for this buffer.")

(defun math ()
  "Start up math-mode in a new buffer called 'math'.
Or if it already exists, switch to math."
  ;; could allow more than one by changing buffer names!!
  (interactive)
  (let ((buf (get-buffer-create math-buffer-name)))
    (switch-to-buffer buf)
    (if (not math-mode)			; did it exist already?
	(math-mode)
      )))


(defun math-mode ()
  "Major mode to interface with Mathematica.
This should only be called automatically on a new or existing
notebook file.  Use math to start up Mathematica in a math buffer.

Commands:
\\{math-mode-map}

Turning on math mode calls the value of text-mode-hook and then
math-mode-hook, if they are non-nil."

  (setup-math-mode)
  (if (buffer-file-name)
      (math-mode-on-notebook-file))
  (finish-math-mode-initialization)
  )


(defun math-mode-maybe ()
  "Ask to invoke math-mode for the current buffer.
For use with .m and .nb files in auto-mode-alist."
  (if (y-or-n-p "Invoke math-mode? ")
      (math-mode)
    (math-package-mode)
    ))


(defun setup-math-mode ()
  "Internal.  Set up local variables for math-mode."

  (kill-all-local-variables)

  (setq selective-display t)

  (use-local-map math-mode-map)

  (setq mode-name "Math")
  (setq major-mode 'math-mode)
  (make-local-variable 'math-mode)
  (setq math-mode t)

  ;; probably could include a few abbreviations!!
  (if (not (boundp 'math-mode-abbrev-table))
      (define-abbrev-table 'math-mode-abbrev-table ()))
  (setq local-abbrev-table math-mode-abbrev-table)

  (set-syntax-table math-mode-syntax-table)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'math-maybe-indent-line)

  ;; this needs some work - possibly replace the paragraph filler
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^[ \t\n\f]\\|^"
				math-cell-label-text-regexp
				"\\|" page-delimiter))

  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate (concat "^[ \t\f]*$\\|^"
				   math-cell-label-text-regexp))

  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'math-comment-indent)
;;  (make-local-variable 'parse-sexp-ignore-comments)
;;  (setq parse-sexp-ignore-comments t)

  (init-math-cell-vector)
  (make-local-variable 'math-last-input-cell)
  (setq math-last-input-cell nil)

  (make-local-variable 'current-math-cell-start)
  (setq current-math-cell-start (point-marker))
  (make-local-variable 'current-math-cell-end)
  (setq current-math-cell-end (point-marker))
  (make-local-variable 'current-math-cell-index)
  (setq current-math-cell-index 0)
  
  (make-local-variable 'math-use-structure-labels)
  ;; use default value
  (setq math-use-structure-labels (default-value 'math-use-structure-labels))
  )



;; add math-goto-first-cell to find-file-hooks - could be buffer local
(or (memq 'math-goto-first-cell find-file-hooks)
    (setq find-file-hooks (cons 'math-goto-first-cell find-file-hooks)))

;; add math-save-buffer to write-file-hooks
(if (not (memq 'math-save-buffer write-file-hooks))
    (setq write-file-hooks (cons 'math-save-buffer write-file-hooks)))


(defun math-goto-first-cell ()
  "Go to the first math-cell in notebook."
  (if math-mode
      (progn
	(if (not math-cell-vector)
	    (error "math-mode is not properly initialized."))
	(let ((first-math-cell (first-math-cell)))
	  (if first-math-cell
	      (math-move-to-cell first-math-cell))))))



;; These should match the auto-mode-alist entries.
(defconst math-nb-suffix ".nb" "Suffix for macintosh format notebook file.")
(defconst math-nbl-suffix ".nbl" "Suffix for math-mode format notebook file.")
(defconst math-nbl-regexp "\\.nbl$" "Pattern for matching math-nbl-suffix")

(defconst math-package-suffix ".m" "Suffix for Mathematica package file.")


(defun math-mode-on-notebook-file ()
  "Called to determine how math-mode should be activated for the
current buffer that has an associated file.  Ask to use the newer of the .nb
or .nbl versions of a notebook."

  (let* ((filename (file-name-nondirectory (buffer-file-name)))
	 (start-with-nbl (string-match (concat "\\" math-nbl-suffix "$")
				       filename))
	 (filename-root (math-file-name-root filename))
	 (nb-filename (concat filename-root math-nb-suffix))
	 (nbl-filename (concat filename-root math-nbl-suffix))
	 (package-filename (concat filename-root math-package-suffix))

	 ;; find the newest of the three files
	 (newest-file
	  (if (file-newer-than-file-p nbl-filename nb-filename)
	      (if (file-newer-than-file-p nbl-filename package-filename)
		  nbl-filename
		package-filename)
	    (if (file-newer-than-file-p nb-filename package-filename)
		nb-filename
	      package-filename))))
	  
    (if (and (file-exists-p newest-file)
	     (not (string= newest-file filename)))
	;; ask to use newest file instead
	(if (yes-or-no-p
	     (format "%s is newer.  Use it instead? "
		     newest-file))
	    (progn
	      (erase-buffer)
	      (insert-file-contents newest-file)
	      (setq filename newest-file))))

    (if (> (buffer-size) 0)
	(cond
	 ((string= filename nb-filename)
	  (convert-math-notebook-file))

	 ((string= filename nbl-filename)
	  (convert-math-mode-lisp-to-internal))
      
	 ((string= filename package-filename)
	  (convert-math-package))

	 (t (error "Bad filename: %s" filename))))

    ;; change the visited file to the nbl form.
    (set-visited-file-name nbl-filename)
    (if start-with-nbl
	(not-modified))

    (auto-save-mode -1)			; avoid annoying auto-save messages
   
    ;; the following should be a user option I suppose
    (setq file-precious-flag t)		; copy the original file on save
   
    ))


(defun math-package-to-nbl ()
  "Convert the package in the current buffer to an nbl buffer."
  (interactive)
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
	 (start-with-nbl (string-match (concat "\\" math-nbl-suffix "$")
				       filename))
	 (filename-root (math-file-name-root filename))
	 (nbl-filename (concat filename-root math-nbl-suffix))
	 (package-filename (concat filename-root math-package-suffix))
	 )
    (if start-with-nbl
	(error "%s is already an nbl buffer." filename))
    (if (not (string= filename package-filename))
	(error "%s is not a package buffer." filename))
    (if (get-buffer nbl-filename)
	(error "%s buffer already exists." nbl-filename))

    (setup-math-mode)
    (convert-math-package)
	  
    ;; change the visited file to the nbl form.
    (set-visited-file-name nbl-filename)
    (if start-with-nbl
	(not-modified))
  
    (auto-save-mode -1)			; avoid annoying auto-save messages
   
    ;; the following should be a user option I suppose
    (setq file-precious-flag t)		; copy the original file on save
  
    (finish-math-mode-initialization)
    ))


(defun math-file-name-root (name)
  "Return the root of the file name NAME.
The root is missing the tail end suffix of the name, that is,
everything following the last \".\"."
  (setq name (file-name-nondirectory
	      (or name
		  (read-file-name "Save to file: " default-directory "math"))))
  (if (string-match "\\.[^.]*$" name)
      (substring name 0 (match-beginning 0))
    name)
  )
      

(defun finish-math-mode-initialization ()
  "Finish up math-mode initialization on the current buffer.
The buffer is either empty or contains an internally formatted notebook."
  (run-hooks 'text-mode-hook 'math-mode-hook)

  (if (= 0 (buffer-size))		; if empty
      (progn
	(math-insert-new-input-cell nil)
	(math-check-current-cell nil))
	
    ;; not empty
    (math-check-current-cell nil)
    (if math-eval-init-cells
	(if (or (eq t math-eval-init-cells)
		(y-or-n-p "Evaluate initialization cells? "))
	    (math-eval-init-cells)))
    )

  (math-setup-change-hooks)
  (buffer-enable-undo (current-buffer))
  )




;;;---------------------------------------------
;; Math package mode.

(defvar math-package-mode-map nil "")

(if (and math-package-mode-map   ; do this just once
	 (not math-show-debug)) ; unless debugging
    nil

  (progn
    (setq math-package-mode-map (make-keymap))

    (let ((i 32))
      (while (< i 127)
	(define-key math-package-mode-map (char-to-string i) 
	  'self-insert-command)
	(setq i (1+ i)))
      )

    (define-key math-package-mode-map "\C-m" 'newline-and-indent)
    (define-key math-package-mode-map "\t" 'math-package-indent-line)
    (define-key math-package-mode-map "\M-\C-q" 'math-package-indent-exp)

    ))



(defun math-package-mode ()
  "Set up local variables for math-package-mode."
  (interactive)
  (kill-all-local-variables)

;;  (setq selective-display t)

  (use-local-map math-package-mode-map)

  (setq mode-name "Math Package")
  (setq major-mode 'math-package-mode)
  (make-local-variable 'math-package-mode)
  (setq math-package-mode t)

  ;; probably could include a few abbreviations!!
;;  (if (not (boundp 'math-package-mode-abbrev-table))
;;      (define-abbrev-table 'math-package-mode-abbrev-table ()))
;;  (setq local-abbrev-table math-package-mode-abbrev-table)

  (set-syntax-table math-mode-syntax-table)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'math-package-indent-line)

  ;; this needs some work - possibly replace the paragraph filler
;;  (make-local-variable 'paragraph-start)
;;  (setq paragraph-start (concat "^[ \t\n\f]\\|^"
;;				math-cell-label-text-regexp
;;				"\\|" page-delimiter))

;;  (make-local-variable 'paragraph-separate)
;;  (setq paragraph-separate (concat "^[ \t\f]*$\\|^"
;;				   math-cell-label-text-regexp))

  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'math-comment-indent)
;;  (make-local-variable 'parse-sexp-ignore-comments)
;;  (setq parse-sexp-ignore-comments t)

  )

(defun math-package-indent-line (&optional arg)
  "Indent current line as Math code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive "P")
  (math-indent-line arg))




;---------------------------------------------
; Debugging and errors

(defun to-math-debug (msg)
  "Same as to-math-buffer except to a debug window."
  (if math-show-debug
      (let
          ((obuf (current-buffer))
           (owin (selected-window))
           math-debug-window
           math-debug-buffer
           )
        (set-buffer
	 (setq math-debug-buffer (get-buffer-create "*math-debug*")))
        (setq math-debug-window
              (display-buffer math-debug-buffer))

        (goto-char (point-max))
        (insert msg)
        (forward-line (- 1 (window-height math-debug-window)))
        (set-window-start math-debug-window (point) )
;;;       (message "point: %s  window: %s  buffer: %s" (point)
;;;                math-debug-window math-debug-buffer)
;;;       (sit-for 1)
;;;       (update-display)
        (bury-buffer math-debug-buffer)
        (set-buffer obuf)
        )
    )
  )

(defun math-error (str &rest args)
  "Copy error to math-debug and show error message to user."
  (if math-show-debug
      (progn
        (to-math-debug (apply 'format (concat " >>> " str " <<<\n") args))
        (apply 'error str args)
        )))



  

;;------------
;; Math-Cell labels


;;(defconst math-nb-heading-start-regexp "^::\\["
;;  "Regular expression to match the special notebook control heading.")

;;(defconst math-cell-heading-or-subheading-start-regexp "^[;:]\\["
;;  "Regular expression to match the beginning of a heading or subheading line.")

(defconst math-cell-heading-start-regexp "^:\\["
  "Regular expression to match the beginning of a heading line.
Any line whose beginning matches this regexp is considered a math-cell-heading.")


(defconst math-cell-depth-char ?\>
  "Character used to show depth of a math-cell.")

(defconst math-cell-beginning-char "-"
  "String used to label the start of every math-cell, unless it has a name.
It should be at least one character long to hide the marker after.")

(defconst math-group-beginning-char "="
  "String used to label the start of every math-cell, unless it has a name.
It should be at least one character long to hide the marker after.")

;;(defconst show-depth-always nil
;;  "If non-nil, show the depth of a math-cell even if there is a name field.")

(defconst math-cell-message-label "[a-zA-Z0-9]+::[a-zA-Z0-9]+:"
  "Regular expression to match a message math-cell label.")

(defconst math-cell-label-text-regexp
  (concat
   "\\("
   "\\("
   "[" math-cell-beginning-char math-group-beginning-char "]"
   (char-to-string math-cell-depth-char)
   "+ \\)?\\(In\\[[0-9]+\\]:=\\|Out\\[[0-9]+\\]\\(//[^=]+\\)?=\\|"
   math-cell-message-label
   "\\)\\|"
   "[" math-cell-beginning-char math-group-beginning-char "]"
   (char-to-string math-cell-depth-char)
   "+\\) ?")
  "Same as math-cell-label-regexp, but without prefix.")

(defconst math-cell-label-regexp
  (concat
   "[\n\r]" ; must be at beginning of line, may be hidden
   math-cell-label-text-regexp)
  "Regular expression matching a math-cell label.
Last space is optional.")

(defun math-match-cell-label ()
  "Match the next math-cell label after point."
  (re-search-forward math-cell-label-regexp nil 'move)
  )

(defun math-back-to-cell-label ()
  "Move point back to the previous math-cell label.
This is the char before the label so that math-match-cell-label will match it.
 If at end of file, back up to the label of last math-cell."
  (if (re-search-forward "[\n\r]\\|\\'" nil 'move)
      (re-search-backward math-cell-label-regexp nil 'move))
  )


(defun math-label-to-contents ()
  "Move point from somewhere in the label to the start of the math-cell contents."
  (re-search-backward "[\n\r]")  ; first math-cell not at beginning of buffer
  (math-match-cell-label))

(defun math-end-of-contents ()
  "Move point from somewhere in a math-cell to the end of its contents."
  (if (math-match-cell-label) ; goto next math-cell or end of file
      (progn
	(goto-char (match-beginning 0))
	(skip-chars-backward "\n\r")
	))
  )



;;;-------------------------------
;;; Accessing math-cell info fields


(defconst math-cell-fields '
  (name
   font
   closed
   inactive
   initialization
   autoActive
   Cclosed
   startGroup
   endGroup
   output

   extras   ;; unrecognized extra fields

   ;; the rest are for the emacs frontend
   output-form  ;; ;[o]
   input-form   ;; ;[i] ??
   eval

   styles  ;; ;[s]
   contents
   next-indent-width
   last-indent-width

   previous
   next
   backward
   forward
   parent

   index
   depth
   point ; should be the last field
   )
  "List of names of fields of a math-cell heading.
The first few are standard names; remaining are internal to
the emacs front end.")

(defconst number-of-math-cell-fields (length math-cell-fields))

;;; set the math-cell-field property of each field name to an ordinal value.
(let ((i 0))
  (mapcar (function (lambda (field)
		      (put field 'math-cell-field i)
		      (setq i (1+ i))))
	  math-cell-fields))
		    
;; previous expression must be evaluated before next
(defconst math-cell-standard-fields
  (get 'contents 'math-cell-field)
  "Number of standard (non-internal) fields in math-cell heading.")


(defmacro math-cell-fieldq (math-cell-index field)
  "Access the FIELD field of the math-cell referenced by math-cell-index.
FIELD is unevaluated."
  (` (aref (aref math-cell-vector (, math-cell-index))
	   (, (get field 'math-cell-field)))
     ))


(defmacro math-cell-field (math-cell-index field)
  "Same as math-cell-field except field is not quoted."
  (` (aref (aref math-cell-vector (, math-cell-index))
	   (get (, field) 'math-cell-field))
     ))



;; the argument to the following macros
;; should always be math-cell-index since the above field macros use that

(defmacro math-cell-contents (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) contents)))

(defmacro math-cell-name (math-cell-index)
  (` (car (math-cell-fieldq (, math-cell-index) name))))

(defmacro math-cell-font (math-cell-index)
  (` (car (math-cell-fieldq (, math-cell-index) font))))
  
(defmacro math-cell-eval-p (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) eval)))

(defmacro math-cell-closed-p (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) closed)))

(defmacro math-cell-inactive-p (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) inactive)))

(defmacro math-cell-initialization-p (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) initialization)))

(defmacro math-cell-autoActive-p (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) autoActive)))

(defmacro math-cell-group-closed-p (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) Cclosed)))

(defmacro math-cell-start-group-p (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) startGroup)))

(defmacro math-cell-end-group-p (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) endGroup)))


(defmacro math-cell-end-group (math-cell-index)
  "Return the number of endGroups for this math-cell"
  (` (or (math-cell-fieldq (, math-cell-index) endGroup) 0)))


(defmacro math-cell-previous (math-cell-index)
  "previous math-cell at the leaf level"
  (` (math-cell-fieldq (, math-cell-index) previous)))

(defmacro math-cell-next (math-cell-index)
  "next math-cell at the leaf level"
  (` (math-cell-fieldq (, math-cell-index) next)))

(defmacro math-cell-backward (math-cell-index)
  "previous math-cell at the same level in the group."
  (` (math-cell-fieldq (, math-cell-index) backward)))

(defmacro math-cell-forward (math-cell-index)
  "next math-cell at the same level in the group."
  (` (math-cell-fieldq (, math-cell-index) forward)))

(defmacro math-cell-parent (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) parent)))

(defmacro math-cell-offspring (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) next)))



(defmacro math-cell-depth (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) depth)))

(defmacro math-cell-point (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) point)))

(defmacro math-cell-output-p (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) output)))

(defmacro math-cell-output-form (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) output-form)))

(defmacro math-cell-input-form (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) input-form)))

(defmacro math-cell-next-indent (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) next-indent-width)))

(defmacro math-cell-last-indent (math-cell-index)
  (` (math-cell-fieldq (, math-cell-index) last-indent-width)))


;;;------------------
;;; setting math-cell fields

(defmacro set-math-cell  (math-cell-index field &optional value)
  "Set math-cell MATH-CELL-INDEX field FIELD to VALUE, which may be nil."
  (` (and (, math-cell-index)		; this could be dangerous!!
	  (aset (aref math-cell-vector (, math-cell-index))
		(, (get (eval field) 'math-cell-field))
		(, value)
		))))

;;-----------------------------------
;; The math-cell vector
;; This vector holds all the cells for a buffer.

(defvar math-cell-vector-length 0
  "Current length of math-cell-vector.")

(defconst math-cell-vector-increment 100
  "How much to add to math-cell-vector as necessary.")
  
(defvar math-cell-vector nil
  "A vector of all cells in a buffer.  nil values indicate an empty cell.")

(defvar free-math-cells nil
  "List of indexes of free math-cells.")

(defun init-math-cell-vector ()
  "Initialize math-cell-vector and associated things to empty.
Make all associated variables buffer-local."
  (make-local-variable 'math-cell-vector)
  (setq math-cell-vector [])
  (make-local-variable 'math-cell-vector-length)
  (setq math-cell-vector-length 0)
  (make-local-variable 'free-math-cells)
  (setq free-math-cells nil)
  (make-local-variable 'math-cell-markers)
  (setq math-cell-markers nil)
  )


;; the math-cell-vector never shrinks!!
(defun grow-math-cell-vector ()
  "More math-cells are needed, so add the amount in math-cell-vector-increment."
  (let ((i (+ math-cell-vector-length math-cell-vector-increment)))
    (setq math-cell-vector
	  (vconcat math-cell-vector
		   (make-vector math-cell-vector-increment nil)))
    (while (> i math-cell-vector-length) ; watch for off-by-one error
      (setq i (1- i))
      (setq free-math-cells (cons i free-math-cells))
      ))
  (setq math-cell-vector-length (length math-cell-vector))
  )


(defun new-math-cell-info (&optional math-cell-heading)
  "Find an empty spot in math-cell-vector to put MATH-CELL-HEADING.  
Add index field and return the new index."

  (if (not math-cell-heading)
      (setq math-cell-heading (make-vector (length math-cell-fields) nil)))
  
  (let (math-cell-index)
    (if (not free-math-cells)
	(progn
	  ;;	(garbage-collect-math-cells)
	  ;;      (if (not free-math-cells) )
	  (grow-math-cell-vector)))
    (setq math-cell-index  (car free-math-cells))
    (setq free-math-cells (cdr free-math-cells))
      
    (aset math-cell-vector math-cell-index math-cell-heading)
    (set-math-cell math-cell-index 'index math-cell-index)
    math-cell-index
    ))


(defun delete-math-cell-info (math-cell-index)
  "Delete the math-cell-heading info of MATH-CELL-INDEX.
The math-cell info may already be nil, in which case nothing is done."
  (if (and (< math-cell-index math-cell-vector-length)
	   (aref math-cell-vector math-cell-index))
      (progn
	(delete-math-cell-marker math-cell-index)
	(setq free-math-cells (cons math-cell-index free-math-cells))
	(aset math-cell-vector math-cell-index nil)
	)))



;;------------------------------
;; Consistency checks

(defun check-math-cell (&optional math-cell-index)
  "Check that MATH-CELL-INDEX is correctly connected to its neighbors."
  (interactive)
  (or math-cell-index
      (setq math-cell-index (current-math-cell)))
  (let ((next (math-cell-next math-cell-index))
	(previous (math-cell-previous math-cell-index))
	(forward (math-cell-forward math-cell-index))
	(backward (math-cell-backward math-cell-index))
	(parent (math-cell-parent math-cell-index))
	(pnt (math-cell-point math-cell-index))
	)
    (if pnt
	(save-excursion (goto-math-cell math-cell-index)))
    (if next
	(or (and
	     (= math-cell-index (math-cell-previous next))
	     (/= math-cell-index next))
	    (math-error "%s <> (previous next:%s)" math-cell-index next)))
    (if previous
	(or (and
	     (= math-cell-index (math-cell-next previous))
	     (/= math-cell-index previous))
	    (math-error "%s <> (next previous:%s)" math-cell-index previous)))
    (if forward
	(or (and
	     (= math-cell-index (math-cell-backward forward))
	     (/= math-cell-index forward))
	    (math-error "%s <> (backward forward:%s)" math-cell-index forward)))
    (if backward
	(or (and
	     (= math-cell-index (math-cell-forward backward))
	     (/= math-cell-index backward))
	    (math-error "%s <> (forward backward:%s)" math-cell-index backward)))
    (if parent
	(let ((child (math-cell-next parent)))
	  (while (and child (not (eq child math-cell-index)))
	    (or (= parent (math-cell-parent child))
		(math-error "%s <> (child parent:%s)" child parent))
	    (setq child (math-cell-forward child)))
	  (or (eq math-cell-index child)
	      (math-error "%s <> (child parent:%s)" math-cell-index parent))))
    ))
      
    



;;--------------------------------
;; Killing cells
;; These are globals since only one set of killed cells
;; can exist, like the character kills without the kill-ring

(defvar killed-math-cells-buffer nil
  "The buffer in which killed-math-cells exist.  There may only be one.
Thus, a new kill-math-cells command must first remove old killed-math-cells.")

(defvar killed-math-cells nil
  "List of killed math-cells that havent been removed yet
because they may be yanked later.")

(defvar dead-math-cells nil 
  "List of other math-cells which havent been removed,
but will never be yanked.  The purpose of using this is that some killed
math-cells may also be permanently (unrecoverably) dead, but we cannot
easily remove only some killed math-cells, so we just remember them 
for later removal.")

(defvar math-yank-with-copy nil
  "Global flag that indicates whether a math-yank should be performed
with copy instead of by simply relinking.
A copy must be done if the math-cells
being yanked already have been yanked in the notebook.
So, if math-yank-with-copy is non-nil, then the killed math-cells should
not be removed.
A copy must also be made if the yank is across buffers, but that is
checked separately since it can change as the current buffer changes.
Dont change this yourself.")


(defun kill-math-cell-info (math-cell-index)
  "Kill the math-cell info of MATH-CELL-INDEX.
The killed math-cell is put on killed-math-cell list.
This is called by math-kill-cell."
  (setq killed-math-cells-buffer (current-buffer))
  (setq math-yank-with-copy nil)	; fresh kill can be relinked
  (setq killed-math-cells (cons math-cell-index killed-math-cells))
  ;; delete the math-cell marker too
  (delete-math-cell-marker math-cell-index)
  )

;;(defun live-buffer-p (buf)
;;  "Return nil if BUF is nil or a deleted buffer."
;;  (and buf (buffer-name buf)))

(defun remove-killed-math-cells ()
  "Remove the killed-math-cells and any connected by 'next field.
Also remove any dead-math-cells."
  (if (and killed-math-cells-buffer	; anything killed cells?
	   (buffer-name killed-math-cells-buffer)) ; buffer killed?

      (let ((sav-buffer (current-buffer)))
	(set-buffer killed-math-cells-buffer)

	;; dont remove killed-math-cells if yanked or in another buffer
	(if (not math-yank-with-copy)
	    (while killed-math-cells
	      (let ((math-cell-index (car killed-math-cells)))
		(while math-cell-index
		  (delete-math-cell-info
		   (prog1 math-cell-index ; get the next cell before deleting
		     (setq math-cell-index (math-cell-next math-cell-index))
		     ))))
	      (setq killed-math-cells (cdr killed-math-cells))
	      ))

	;; always remove dead math-cells
	(while dead-math-cells
	  (let ((math-cell-index (car dead-math-cells)))
	    (while math-cell-index
	      (delete-math-cell-info
	       (prog1 math-cell-index	; get the next cell before deleting
		 (setq math-cell-index (math-cell-next math-cell-index))
		 ))))
	  (setq dead-math-cells (cdr dead-math-cells))
	  )

	(set-buffer sav-buffer)
	))

  ;; no more killed math cells
  (setq killed-math-cells nil)
  (setq dead-math-cells nil)
  (setq killed-math-cells-buffer nil)
  (setq math-yank-with-copy nil)
  )



;;;-----------------------------
;;; Math-Cell Markers
;;; We need to have a separate list of math-cell markers
;;; so that it can be searched through for the current math-cell.

(defvar math-cell-markers nil
  "An alist of math-cell markers and the math-cell index it is for.")

(defun add-math-cell-marker (math-cell-index marker)
  "Add an marker/index pair to the front of math-cell-markers."
  (setq math-cell-markers 
	(cons (cons marker math-cell-index) math-cell-markers)))

(defun delete-math-cell-marker (math-cell-index)
  "Delete the marker from math-cell-markers indexed by MATH-CELL-INDEX."
  (setq math-cell-markers
	(delq (rassq math-cell-index math-cell-markers) math-cell-markers)))

(defun replace-math-cell-marker (math-cell-index marker)
  (delete-math-cell-marker math-cell-index)
  (add-math-cell-marker math-cell-index marker))


(defun find-math-cell-marker ()
  "Find the math-cell with a marker at the current point.
Return that math-cell index.  Should not return nil."
  (let* ((temp-marker (point-marker))
	 (math-cell (cdr (assoc temp-marker math-cell-markers))))
    (if (not math-cell)
	(math-error
	 "Bad cell marker at point %s.  This is a bug; please report."
	 (point)))
    math-cell
    ))

	  
(defun current-math-cell ()
  "Return the index of the enclosing open math-cell.
If before the first math-cell, return the first math-cell index."
  ;; this doesnt save-excursion since often that is not needed.  

  (if (or (eobp) (looking-at "[ \t\n\r\f]"))
      (skip-chars-backward " \t\n\r\f"))
  (beginning-of-line)			; skip any closed math-cells
  (if (not (math-back-to-cell-label))
      (progn				; then look forward
	(math-match-cell-label)
	(math-back-to-cell-label)))

  (if (not (eobp))
      (forward-char 1))
  (if (not (eobp))
      (forward-char 1))
  (if (eobp)
      nil
    (find-math-cell-marker)
    )
  )


(defun goto-math-cell (math-cell-index)
  "Goto the math-cell point of MATH-CELL-INDEX, if it exists,
else try to find where it might be."
  (if math-cell-index
      (let ((pnt (math-cell-point math-cell-index)))
	(if pnt
	    (progn
	      (goto-char pnt)
	      (and math-show-debug 
		   ;; consistency check
		   (not (eq (find-math-cell-marker) math-cell-index))
		   (math-error "cell marker not at cell: %s"
			       math-cell-index))
	      pnt)
	  (math-error "cell %s has no point" math-cell-index)
	  ))))


;;---------------------------
;; Changing math-cell info fields

(defun edit-math-cell-attributes ()
  "Allow user to edit some of the the cell attributes of the current cell."
  (interactive)
  (let ((math-cell-index (current-math-cell)))
    (if math-cell-index
	(let ((active (not (math-cell-inactive-p math-cell-index)))
	      (initialization (math-cell-initialization-p math-cell-index))
	      (auto-active (math-cell-autoActive-p math-cell-index))
	      )
	  (edit-options			; uses a modified edit-options
 	   '(active initialization auto-active))
	  (recursive-edit)
	  
	  ;; done with options
	  ;; should check each option to see if it was changed.
	  (set-math-cell math-cell-index 'inactive (not active))
	  (set-math-cell math-cell-index 'initialization initialization)
	  (set-math-cell math-cell-index 'autoActive auto-active)
	  ))))


;;; These are not generally useful - could be macros

(defun open-math-cell (math-cell-index)
  (set-math-cell math-cell-index 'closed))

(defun open-math-group (math-cell-index)
  (set-math-cell math-cell-index 'Cclosed)
  )



;;;--------------------
;;; Moving between math-cells


(defun math-move-to-cell (math-cell-index)
  (if math-cell-index
      (progn
	(goto-math-cell math-cell-index)
	(math-label-to-contents))))

(defun math-move-to-current-cell ()
  (interactive)
  (math-move-to-cell (current-math-cell)))


(defun move-to-math-cell-rel (direction)
  "Move the point to inside the math-cell in the given DIRECTION relative
to the current math-cell."
  (let* ((math-cell-index (save-excursion (current-math-cell)))
	 (dest-index (math-cell-field math-cell-index direction))
	 )
    (if (not dest-index)
	(message "No cell in that direction")
      (goto-math-cell dest-index)

      (if (memq direction '(next forward))
	  (progn
	    (re-search-backward "[\n\r]")
	    (search-forward "\n"))
	(current-math-cell))
      (math-label-to-contents))
    ))

(defun math-next-cell ()
  (interactive)
  (move-to-math-cell-rel 'next))

(defun math-previous-cell ()
  (interactive)
  (move-to-math-cell-rel 'previous))

(defun math-backward-cell ()
  (interactive)
  (move-to-math-cell-rel 'backward))

(defun math-forward-cell ()
  (interactive)
  (move-to-math-cell-rel 'forward))

(defun math-parent-cell ()
  (interactive)
  (move-to-math-cell-rel 'parent))


(defun first-math-cell ()
  "Return the first math-cell of the notebook."
  ;; find the first math-cell
  (if (> (length math-cell-vector) 0)
      (let ((first-math-cell 0))
	;; go forward until non-nil
	(while (not (aref math-cell-vector first-math-cell))
	  (setq first-math-cell (1+ first-math-cell)))
	;; backup til no previous
	(while (math-cell-previous first-math-cell)
	  (setq first-math-cell (math-cell-previous first-math-cell)))
	first-math-cell
	)))




;;;-----------------------------
;;; Math-Cell hiding and showing

(defun math-ensure-blank-line ()
  "Make sure there are at least two blank lines around point."
  ;; bob and eob count for one blank line
  (if (not (bobp))
      (progn
	(forward-char -1)
	(if (looking-at "[\n\r]")
	    (forward-char 1)
	  (if (not (eobp))
	      (forward-char 1))
	  (insert "\n"))))

  (if (or (eobp) (looking-at "[\n\r]"))
      nil
    (insert "\n")
    (forward-char -1)
    ))


(defun math-flag-region (from to flag)
  "Hides or shows lines from FROM to TO, according to FLAG.  If FLAG
is \\n (newline character) then text is shown, while if FLAG is \\r
\(control-M) the text is hidden."
  (subst-char-in-region from to
                        (if (= flag ?\n) ?\r ?\n)
                        flag 'noundo))


(defun math-hide-group (math-cell-index)
  "Hide the group of which the MATH-CELL-INDEX is the head."

  (let ((forward-math-cell (math-cell-forward math-cell-index))
	(next-math-cell (math-cell-next math-cell-index))
	(parent-math-cell nil))

    ;; show the head math-cell (up to next math-cell)
    (goto-math-cell math-cell-index)
    (math-back-to-cell-label)
    (math-ensure-blank-line)
    (if (not (bobp))
	(forward-char -1))

    (math-flag-region (point)
		      (progn
			(goto-math-cell next-math-cell)
			(math-back-to-cell-label)
			(point))
		      ?\n)
    
    ;; positioned just before next-math-cell label
    ;; insert an extra newline if needed
    (math-ensure-blank-line)
    (insert "\n") (delete-backward-char 1) ; work around to force redisplay

    ;; hide the math-cells in the group
    (math-flag-region
     (point)
     (progn
       (if (not forward-math-cell)
	   ;; then this is the last math-cell in a group
	   (progn
	     ;; loop to find a parent that has a forward sibling
	     (setq parent-math-cell math-cell-index)
	     (while
		 (and (setq parent-math-cell
			    (math-cell-parent parent-math-cell))
		      (not
		       (setq forward-math-cell
			     (math-cell-forward parent-math-cell))))
	       )
	     ))

       (if (not forward-math-cell)
	   (goto-char (point-max))	; must be last math-cell in notebook
	 (goto-math-cell forward-math-cell)
	 (math-back-to-cell-label)
	 (math-ensure-blank-line)
	 (forward-char -1)		; leave a blank line showing
	 )

       (point))
     ?\r)
    )
  )


(defun math-show-group (math-cell-index)
  "Show the open group of which the MATH-CELL-INDEX is the head.
Called recursively on subgroups if they are open."

  (let ((next-math-cell (math-cell-next math-cell-index)) ; first offspring
	)

    ;; show the header math-cell
    (goto-math-cell math-cell-index)
    (math-back-to-cell-label)
    (math-ensure-blank-line)
    
    (if (not (bobp))
	(forward-char -1))		; show any previous blank line

    (math-flag-region (point)
		      (progn
			(goto-math-cell next-math-cell)
			(math-back-to-cell-label)
			(forward-char 1)
			(point))
		      ?\n)

    ;; delete any extra newlines at end of math-cell
    (forward-char -2)
    (if (looking-at  "[\n\r][\n\r]")	; two in a row?
	(delete-char 1))
    (insert "\n") (delete-backward-char 1) ; force redisplay

    (while next-math-cell
      (show-or-hide-math-cell next-math-cell)
      (setq next-math-cell (math-cell-forward next-math-cell)))
    )
  )


(defun math-hide-entry (math-cell-index)
  "Hide the body directly following this math-cell-heading."

  (progn
    (goto-math-cell math-cell-index)

    ;; explicitly show the label
    (math-back-to-cell-label)
    (math-ensure-blank-line)
    (forward-char -1)

    (math-flag-region (point)
		      (progn
			(goto-math-cell math-cell-index)
			(point)
			)
		      ?\n)

    (math-label-to-contents)
    (setq end-of-contents )

    ;; delete any extra newline at start of contents
    (delete-region (point) 
		   (progn
		     (skip-chars-forward "\n\r") 
		     (if (looking-at math-cell-label-text-regexp)
			 (skip-chars-backward "\n\r")) ; too far
		     (point)))
    (insert "\r")			; hide the first line of the contents

    (math-flag-region (point)		; after newline
		      (progn
			(math-end-of-contents)
			(point))
		      ?\r))
  )


(defun math-show-entry (math-cell-index)
  "Show the body of MATH-CELL-INDEX."
  (save-excursion
    (let ((math-cell-start (goto-math-cell math-cell-index)))

      (math-label-to-contents)

      ;; delete extra newlines before content
      (delete-region (point)
		     (progn
		       (skip-chars-forward "\n\r")
		       (if (looking-at math-cell-label-text-regexp)
			   (skip-chars-backward "\n\r")) ; too far
		       (point)))
      ;; could delete lines with only whitespace
      ;; on beginning of contents

      (math-back-to-cell-label)
      (math-ensure-blank-line)
      (if (not (bobp))
	  (forward-char -1))

      (math-flag-region (point)
			(progn
			  (forward-char 1)
			  (math-label-to-contents)
			  (math-end-of-contents)
			  (if (not (eobp))
			      (forward-char 1)) ; include extra newline
			  (math-ensure-blank-line)
			  (if (looking-at "[\n\r][\n\r]")
			      ;; delete extra newline
			      (delete-char 1))
			  (point))
			?\n)
      ;; this is needed if labels changed while cell was closed
      (adjust-math-cell-indent math-cell-index)
      )))


(defun show-or-hide-math-cell (math-cell-index)
  "Show or hide the MATH-CELL depending on its current setting."
  (if (math-cell-start-group-p math-cell-index)
      
      (if (math-cell-group-closed-p math-cell-index)
	  (math-hide-group math-cell-index)
	(math-show-group math-cell-index)
	)

    (if (math-cell-closed-p math-cell-index)
	(math-hide-entry math-cell-index)
      (math-show-entry math-cell-index))))


(defun adjust-math-cell-indent (math-cell-index)
  "Adjust the indent level of the open output math-cell MATH-CELL-INDEX."
  (if  (and (not (math-cell-closed-p math-cell-index))
	    (math-cell-output-p math-cell-index))
      (let ((last-indent-width (or (math-cell-last-indent math-cell-index) 0))
	    (next-indent-width (or (math-cell-next-indent math-cell-index) 0)))
	(if (/= last-indent-width next-indent-width)
	    (let ((indent-width
		   (- next-indent-width last-indent-width))
		  (math-cell-start (goto-math-cell math-cell-index)))
	      (goto-char math-cell-start)
	  
	      (next-line 1)
	      (beginning-of-line)	; first line to indent

	      (let* ((first-line (point))
		     (last-line
		      (max first-line
			   (progn
			     (math-match-cell-label) ; next math-cell
			     (if (not (eobp))
				 (math-back-to-cell-label))
			     (point))))) ; last line to indent
		(if (< first-line last-line)
		    (indent-rigidly first-line last-line
				    indent-width)))

	      (set-math-cell math-cell-index
			     'last-indent-width
			     next-indent-width)
	      )))))


(defun math-mark-current-cell ()
  "Put point at start of math-cell and mark at end."
  (interactive)
  (let ((math-cell-region (math-cell-contents-region (current-math-cell))))
    (goto-char (cdr math-cell-region))
    (set-mark-command nil)
    (goto-char (car math-cell-region))
    ))

(defun math-copy-backward-cell ()
  "Copy the contents of the previous sibling into the current math-cell."
  (interactive)
  (let* ((backward-math-cell (save-excursion
			       (math-cell-backward (current-math-cell))))
	 (math-cell-contents (and backward-math-cell
				  (save-excursion
				    (math-cell-contents-string
				     backward-math-cell))))
	 )
    (and math-cell-contents
	 (insert math-cell-contents))
    ))

(defun math-cell-contents-without-indent (math-cell-index)
  "Return the contents of MATH-CELL-INDEX with no indentation."
  (let ((last-indent-width (or (math-cell-last-indent math-cell-index) 0))
	(contents (math-cell-contents-string math-cell-index))
	)
    (if (= last-indent-width 0)
	contents
      (let ((indent-width
	     (- last-indent-width))
	    (old-buf (current-buffer))
	    (buf (get-buffer-create " *math-cell*"))
	    )
	  
	(set-buffer buf)
	(erase-buffer)
	(insert contents)
	(goto-char (point-min))
		
	(next-line 1)
	(beginning-of-line)		; first line to indent

	(indent-rigidly (point)
			(progn
			  (goto-char (point-max))
			  (point))	; last line to indent
			indent-width)

	(prog1
	    (buffer-substring (point-min) (point-max))
	  (set-buffer old-buf))
	))))


(defun math-toggle-close-cell ()
  "Open the math-cell at point if it is closed, close it if open.
Applies to groups too."
  (interactive)
  (save-excursion
    (let ((math-cell-index (current-math-cell))
	  (before-change-hook nil)	; allow change anywhere
	  (inhibit-quit t)              ; no quiting in the middle
	  )
      (buffer-flush-undo (current-buffer)) ; disable undoing of cell changes

      (unwind-protect
	  (if (math-cell-start-group-p math-cell-index)

	      (if (math-cell-group-closed-p math-cell-index)
		  (progn		; open it
		    (message "Open group...")
		    (math-show-group math-cell-index)
		    (open-math-group math-cell-index)
		    (message "")
		    )
		;; else close it
		(message "Close group...")
		(math-hide-group math-cell-index)
		(set-math-cell math-cell-index 'Cclosed t)
		(message "")
		)

	    (if (math-cell-closed-p math-cell-index)
		(progn			; open it
		  (math-show-entry math-cell-index)
		  (open-math-cell math-cell-index)
		  )
	      ;; else close it
	      (set-math-cell math-cell-index 'closed t)
	      (math-hide-entry math-cell-index)
	      )
	    )
	(buffer-enable-undo)
	(math-check-current-cell nil)
	))))



(defun math-toggle-cell-output ()
  "Toggle whether math-cell displays input or output form."
  (interactive)
  (save-excursion
    (let ((math-cell-index (current-math-cell))
	  (before-change-hook nil)	; allow changes
	  (inhibit-quit t)              ; no quiting in the middle
	  )
      (buffer-flush-undo (current-buffer))
      (unwind-protect
	  (if (math-cell-start-group-p math-cell-index)
	      (error "Not an output math-cell.")

	    (if (math-cell-output-p math-cell-index)

		(if (not (math-cell-input-form math-cell-index))
		    (message "No input form available.")
	      
		  ;; show input form
		  (let ((contents-region
			 (math-cell-contents-region math-cell-index)))

		    ;; store output form in cell info and delete it from buffer
		    ;; we wont need this if the input form is changed
		    (set-math-cell math-cell-index 'output-form
				   (buffer-substring (car contents-region)
						     (cdr contents-region)))
		    (delete-region (car contents-region) (cdr contents-region))
	    
		    ;; insert the input form from math-cell info
		    (goto-char (car contents-region))
		    (insert (math-cell-input-form math-cell-index))
		    (set-math-cell math-cell-index 'input-form)
		    (set-math-cell math-cell-index 'font '(input))

		    (set-math-cell math-cell-index 'output)
		    (math-check-current-cell nil)
		    (math-hide-entry math-cell-index)
		    (open-math-cell math-cell-index) ; make it showable
		    (math-show-entry math-cell-index)
		    (message "Input form")
		    ))
	
	      ;; else not an output cell
	      (if (not (math-cell-output-form math-cell-index))
		  (message "No output form available.")

		;; show output form (check whether input form was changed??)
		;; could just ask mathematica for the output form!!
		(let ((contents-region
		       (math-cell-contents-region math-cell-index)))

		  ;; store input form in math-cell info and delete it
		  (set-math-cell math-cell-index 'input-form
				 (buffer-substring (car contents-region)
						   (cdr contents-region)))
		  (delete-region (car contents-region) (cdr contents-region))
	    
		  ;; insert the output form from cell info
		  (goto-char (car contents-region))
		  (insert (math-cell-output-form math-cell-index))
		  (set-math-cell math-cell-index 'output-form)
		  (set-math-cell math-cell-index 'font '(output))

		  (set-math-cell math-cell-index 'output t)
		  (math-check-current-cell nil)
		  (math-hide-entry math-cell-index)
		  (open-math-cell math-cell-index) ; make it showable
		  (math-show-entry math-cell-index)
		  (message "Output form")
		  ))
	      ))
	(buffer-enable-undo (current-buffer))
	))))



;;;----------------------------
;;; Setting other attributes

(defun math-toggle-active ()
  "Ask whether to toggle active attribute of current cell."
  (interactive)
  (let* ((math-cell-index (save-excursion (current-math-cell)))
	 (active (not (math-cell-inactive-p math-cell-index))))
    (if (y-or-n-p
	 (format "Toggle whether cell is active (currently %s)? "
		 (if active "active" "inactive")))
	(setq active (not active))
      )
    (set-math-cell math-cell-index 'inactive (not active))
    (message "Cell is %s"
	     (if active "active" "inactive"))
    ))

(defun math-toggle-auto-active ()
  "Ask whether to toggle autoActive attribute of current cell."
  (interactive)
  (let* ((math-cell-index (save-excursion (current-math-cell)))
	 (autoActive (math-cell-autoActive-p math-cell-index)))
    (cond 
     ((and (not autoActive)
	   (not (math-cell-start-group-p math-cell-index)))
      (message "Cell must first be the start of a group (and active)."))
     ((and (not autoActive)
	   (math-cell-inactive-p math-cell-index))
      (message "Cell must first be active (and the start of a group)."))
     (t
      (if (y-or-n-p
	   (format "Toggle whether cell is autoActive (currently %s)? "
		   (if autoActive "autoActive" "not autoActive")))
	  (setq autoActive (not autoActive)))
	
      (set-math-cell math-cell-index 'autoActive autoActive)
      (message "Cell is %s"
	       (if autoActive "autoActive" "not autoActive"))
      ))))


(defun math-toggle-initialization ()
  "Ask whether to toggle initialization attribute of current cell."
  (interactive)
  (let* ((math-cell-index (save-excursion (current-math-cell)))
	 (initialization (math-cell-initialization-p math-cell-index)))
    (if (y-or-n-p
	 (format "Toggle initialization attribute (currently %s)? "
		 (if initialization "on" "off")))
	(setq initialization (not initialization)))
    (set-math-cell math-cell-index 'initialization initialization)
    (message "Cell initialization is %s"
	     (if initialization
		 (if (math-cell-inactive-p math-cell-index)
		     "on; it must also be active to be evaluated." "on")
	       "off"))
    ))


(defconst math-style-alist
  '(("title")
    ("subtitle")
    ("subsubtitle")
    ("section")
    ("subsection")
    ("subsubsection")
    ("text")
    ("smalltext")
    ("input")
    ("output")
    ("message")
    ("print")
    ("info")
    ("postscript")
    ("name")
    ("header")
    ("footer")
    ("help")
    ("clipboard")
    ("completions")
    ("network")
    ("graphlabel")
    ("special1")
    ("special2")
    ("special3")
    ("special5")))


(defun math-set-style ()
  "Ask for style of the current cell."
  (interactive)
  (let* ((math-cell-index (save-excursion (current-math-cell)))
	 (font (math-cell-font math-cell-index))
	 (newfont (completing-read
		   (format "Enter style for this cell (Return for %s): "
			   font)
		   math-style-alist nil 'require-match))
	 )
    (if (< 0 (length newfont))
	(progn
	  (set-math-cell math-cell-index 'font (list (intern newfont)))))
    ))
	  


;;;------------------------------
;;; Inserting and deleting math-cells

(defun math-cell-empty (math-cell-index)
  "Test whether cell MATH-CELL-INDEX is empty.
An empty cell has no non-white chars in content."
  (let ((next-math-cell (math-cell-next math-cell-index))
	end-point)
    (save-excursion
      (goto-math-cell math-cell-index)
      (math-label-to-contents)

      (setq end-point
	    (if (not next-math-cell)
		(point-max)
	      (save-excursion
		(goto-math-cell next-math-cell)
		(math-back-to-cell-label)
		(point))))

      (not (re-search-forward "[^ \t\r\n\f]" end-point 'noerror))
      )))


(defun math-insert-new-cell (math-cell-index &optional insert-before font name)
  "Insert a new math-cell after MATH-CELL-INDEX.
If non-nil INSERT-BEFORE, then insert before MATH-CELL-INDEX instead.  
Use FONT and NAME if provided.  Return the new math-cell.
MATH-CELL-INDEX may be nil."
  (let ((new-math-cell-index (new-math-cell-info)))

    ;; add empty fields 
    (set-math-cell new-math-cell-index 'contents "")
    (if name
	(set-math-cell new-math-cell-index 'name name))
    (if font
	(set-math-cell new-math-cell-index 'font font))
    (math-yank-cells math-cell-index insert-before new-math-cell-index)
    (math-move-to-cell new-math-cell-index)
    new-math-cell-index
    ))


(defun math-insert-new-text-cell (arg)
  "Create and add a math-cell before or after the math-cell at point.
If non-nil ARG, then insert before."
  (interactive "P")
  (let* ((math-cell-index (current-math-cell))
	 (insert-before arg)
	 (before-change-hook nil)	; allow changes
	 (inhibit-quit t)		; no quiting in the middle
	 )
    (buffer-flush-undo (current-buffer))
    (unwind-protect
	(progn
	  (if (and (not insert-before)
		   math-cell-index
		   (math-cell-start-group-p math-cell-index))
	      ;; convert to insertion before first offspring
	      (progn		
		(if (math-cell-group-closed-p math-cell-index)
		    (progn		; make sure group is visible
		      (open-math-group math-cell-index)
		      (math-show-group math-cell-index)))

		(setq insert-before t)
		(setq math-cell-index
		      (math-cell-offspring math-cell-index)) ; first offspring
		))

	  (math-insert-new-cell math-cell-index insert-before '(text) nil)
	  )
      (buffer-enable-undo (current-buffer))
      )))
    



(defun math-kill-current-cell ()
  "Kill the current math-cell or group."
  (interactive)
  (let ((math-cell-index (current-math-cell))
	(before-change-hook nil)
	(inhibit-quit t)		; no quiting in the middle
	)
    (buffer-flush-undo (current-buffer))
    (unwind-protect
	(progn
	  ;; remove any previously killed math-cells
	  (remove-killed-math-cells)
	  (save-excursion
	    (math-kill-cell math-cell-index)))
      (buffer-enable-undo (current-buffer)))

;;    (math-move-to-current-cell)
    ))


(defun math-kill-region (start end)
  "Kill the cells in the region between mark and point.
The region is expanded so that all cells that are within the
enclosing group common between mark and point are killed.

Not tested!!"
  (interactive "r")
  (let ((before-change-hook nil)	; allow changes
	(start-math-cell (progn (goto-char start)
				(beginning-of-line)
				(skip-chars-forward " \t\n\r")
				(current-math-cell)))
	(end-math-cell  (progn (goto-char end)
			       (current-math-cell)))
	)
    (buffer-flush-undo (current-buffer))
    (unwind-protect
	(let* ((common (common-math-cell-parent start-math-cell end-math-cell))
	       (first (car common))
	       (last (cdr common))
	       (cell-index first))
	  (remove-killed-math-cells)
	  (while (/= cell-index last)
	    (setq cell-index 
		  (prog1 (math-cell-forward cell-index)
		    (let ((inhibit-quit t)) ; no quiting in the middle
		      (math-kill-cell cell-index)))))

	  )
      (math-move-to-current-cell)
      (buffer-enable-undo (current-buffer))
      )))


(defun math-kill-cell (math-cell-index)
  "Kill the MATH-CELL-INDEX math-cell.
Killing the only member of a group makes the group header into
a normal math-cell."

  (let ((parent-math-cell (math-cell-parent math-cell-index))
	(next-math-cell)
	(following-math-cell)
	(previous-math-cell)
	(backward-math-cell (math-cell-backward math-cell-index))
	(forward-math-cell (math-cell-forward math-cell-index))
	(depth)

	start-point end-point
	new-math-cell-index		; index of new math-cell
	last-math-cell			; the cell that ends group of math-cell-index
	last-end-groups			; how many groups does last-math-cell end
	end-groups			; how many groups does math-cell end
	end-depth
	)

    ;; if only one math-cell remains in group, ungroup it
    (if (and (not backward-math-cell) (not forward-math-cell)
	     parent-math-cell)
	(progn
	  (math-ungroup-cell parent-math-cell)
	  (setq parent-math-cell (math-cell-parent math-cell-index))
	  (setq backward-math-cell (math-cell-backward math-cell-index))
	  (setq forward-math-cell (math-cell-forward math-cell-index))
	  ))

    (setq previous-math-cell (math-cell-previous math-cell-index))
    (setq depth (or (math-cell-depth math-cell-index) 0))

    (goto-math-cell math-cell-index)
    (math-back-to-cell-label)
    (setq start-point (point))		; point to start of deletion

    (setq last-math-cell math-cell-index)
    (if (or (not forward-math-cell) (math-cell-start-group-p math-cell-index))
	(progn
	  ;; find math-cell that ends the math-cell-index or parent group
	  ;; and how deep it is
	  (setq last-math-cell (find-end-of-math-group math-cell-index))
	  (setq end-depth (- (or (math-cell-depth last-math-cell) 0) depth))
	  ))

    (if (not forward-math-cell)
	;; math-cell-index is the last sibling in a group or in the notebook
	(progn
	  ;; change the endGroup values accordingly
	  (if backward-math-cell
	      (setq end-groups (math-cell-end-group backward-math-cell)))
	  (setq last-end-groups (math-cell-end-group last-math-cell))
	  (let ((new-backward-depth (+ (or end-groups 0)
				       (- last-end-groups end-depth)))
		(new-last-depth (- last-end-groups
				   (- last-end-groups end-depth)))
		)
	    (if backward-math-cell
		(if (< 0 new-backward-depth)
		    (set-math-cell backward-math-cell 'endGroup
				   new-backward-depth)
		  (set-math-cell backward-math-cell 'endGroup)))
	    (if (< 0 new-last-depth)
		(set-math-cell last-math-cell 'endGroup new-last-depth)
	      (set-math-cell last-math-cell 'endGroup))
	    )))
	  

    (setq following-math-cell
	  (math-cell-next last-math-cell)) ; math-cell after last

    ;; store contents of math-cells in info for each math-cell under the group
    (let ((math-cell math-cell-index))
      (while (and math-cell (not (eq math-cell following-math-cell)))
	;; put the contents in the math-cell info
	(set-math-cell math-cell 'contents
		       (math-cell-contents-string math-cell))
	;; delete the cell's point field and marker
	(set-math-cell math-cell 'point)
	(delete-math-cell-marker math-cell)
	(setq last-math-cell math-cell)
	(setq math-cell (math-cell-next math-cell))
	))

    ;; remove the text of the cells
    (if following-math-cell
	(progn
	  (goto-math-cell following-math-cell)
	  (math-back-to-cell-label)
	  (setq end-point (point))
	  )
      (setq end-point (point-max)))

    (delete-region start-point end-point)

    ;; disconnect deleted math-cells
    (set-math-cell math-cell-index 'previous nil)
    (set-math-cell last-math-cell 'next nil)
    (set-math-cell math-cell-index 'backward nil)
    (set-math-cell math-cell-index 'forward nil)

    (kill-math-cell-info math-cell-index) ; kill the info for the cell or group

    ;; link around deleted math-cells
    (if previous-math-cell
	(set-math-cell previous-math-cell 'next following-math-cell))
    (if following-math-cell
	(set-math-cell following-math-cell 'previous previous-math-cell))
    (if backward-math-cell
	(set-math-cell backward-math-cell 'forward forward-math-cell))
    (if forward-math-cell
	(set-math-cell forward-math-cell 'backward backward-math-cell))
      
    ))


(defun math-cell-contents-region (math-cell-index)
  "Return a pair of numbers which are the beginning and ending
points of the contents of the math-cell MATH-CELL-INDEX."
  (let ((math-cell-start-point)
	(math-cell-end-point))
    (goto-math-cell math-cell-index)
    (math-label-to-contents)
    (setq math-cell-start-point (point))
    (math-match-cell-label)		; find start of next math-cell
    (if (not (eobp))
	(math-back-to-cell-label))
    (skip-chars-backward "\n\r")
    ;;    (forward-char 1)
    (setq math-cell-end-point (point))
    (cons math-cell-start-point math-cell-end-point)
    ))


(defun math-cell-contents-string (math-cell-index)
  "Return the math-cell contents of MATH-CELL-INDEX from the notebook."
  (let* ((contents-region (math-cell-contents-region math-cell-index))
	 (contents
	  (buffer-substring (car contents-region) (cdr contents-region)))
	 (start 0))
    (while (string-match "\r" contents start)
      (aset contents (match-beginning 0) ?\n)
      (setq start (match-beginning 0)))
    contents
    ))



(defun math-yank-killed-cells (arg)
  "Yank the last killed math-cell as an insertion
after (or before if non-nil ARG) current math-cell."
  (interactive "P")
  (let* ((math-cell-index (current-math-cell))
	 (insert-before arg)
	 (before-change-hook nil)	; allow changes
	 (inhibit-quit t)		; no quiting in the middle
	 )
    (if (and (not insert-before)
	     math-cell-index
	     (math-cell-start-group-p math-cell-index))
	;; convert to insertion before first offspring
	(progn
	  (if (math-cell-group-closed-p math-cell-index)
	      (progn			; make sure group is visible
		(open-math-group math-cell-index)
		(math-show-group math-cell-index)))

	  (setq insert-before t)
	  (setq math-cell-index
		(math-cell-offspring math-cell-index)) ; first offspring
	  ))

    ;; check whether yank is allowed.
    ;; dont allow yank into any of the structures being yanked
    (let (parent-math-cell
	  (yank-allowed t))
      (if (and math-yank-with-copy
	       math-cell-index
	       (eq killed-math-cells-buffer (current-buffer)))
	  (progn
	    ;; from the yank point, check that each parent is not killed
	    (setq parent-math-cell (math-cell-parent math-cell-index))
	    (while (and yank-allowed parent-math-cell)
	      (setq yank-allowed
		    (not (memq parent-math-cell killed-math-cells)))
	      (setq parent-math-cell (math-cell-parent parent-math-cell)))
	    ))

      (if (not yank-allowed)
	  (message "Cannot yank within cells being yanked.")

	;; reverse if inserting before to keep same order
	(let ((yanked-math-cells (if insert-before
				     killed-math-cells
				   (reverse killed-math-cells)))
	      yanked-math-cell)

	  (buffer-flush-undo (current-buffer))
	  (unwind-protect
	      (while yanked-math-cells
		(setq yanked-math-cell (car yanked-math-cells))
		(if (or math-yank-with-copy
			(not (eq killed-math-cells-buffer (current-buffer))))
		    ;; then first copy it
		    (setq yanked-math-cell
			  (math-copy-cells yanked-math-cell 
					   killed-math-cells-buffer)))
		(math-yank-cells
		 math-cell-index insert-before yanked-math-cell)
		(setq yanked-math-cells (cdr yanked-math-cells))
		)
	    (setq math-yank-with-copy
		  (or math-yank-with-copy
		      (eq killed-math-cells-buffer (current-buffer))))
	    (math-move-to-cell yanked-math-cell) ; last yanked math-cell
	    (buffer-enable-undo (current-buffer))
	    ))))
    ))


(defun math-yank-cells (math-cell-index insert-before yanked-math-cell)
  "Yank math-cells before or after MATH-CELL-INDEX depending on INSERT-BEFORE.
Yank all math-cells connected to YANKED-MATH-CELL."
  ;; works with nil math-cell-index for yank of first math-cell.

  (let ((parent-math-cell 
	 (and math-cell-index (math-cell-parent math-cell-index)))
	(next-math-cell 
	 (and math-cell-index (math-cell-next math-cell-index)))
	(previous-math-cell 
	 (and math-cell-index (math-cell-previous math-cell-index)))
	(backward-math-cell
	 (and math-cell-index (math-cell-backward math-cell-index)))
	(forward-math-cell 
	 (and math-cell-index (math-cell-forward math-cell-index)))
	(depth (or (and math-cell-index (math-cell-depth math-cell-index)) 0))

	yanked-forwardmost-sibling	; last sibling in yanked group
	yanked-group-end		; last cell of yanked group
	last-math-cell			; the cell that closes a group
	following-math-cell		; the cell following a cell or group
	end-groups			; how many groups does cell end
	end-depth
	)

    (if math-cell-index
	(if insert-before

	    (progn
	      ;; insert before math-cell math-cell-index
	      (setq following-math-cell math-cell-index)

	      ;; for all siblings, set parent and remember forwardmost sibling
	      (let ((math-cell yanked-math-cell))
		(while math-cell
		  (set-math-cell math-cell 'parent parent-math-cell)
		  (setq yanked-forwardmost-sibling math-cell)
		  (setq math-cell (math-cell-forward math-cell))))

	      ;; find the last math-cell of yanked group
	      (setq yanked-group-end yanked-forwardmost-sibling)
	      (while (math-cell-next yanked-group-end)
		(setq yanked-group-end (math-cell-next yanked-group-end)))

	      ;; Add other linkage to other math-cells

	      (set-math-cell yanked-math-cell 'previous previous-math-cell)
	      (set-math-cell yanked-group-end 'next following-math-cell)
	      (if following-math-cell
		  (set-math-cell following-math-cell 'previous
				 yanked-group-end))
	      (if previous-math-cell
		  (set-math-cell previous-math-cell 'next yanked-math-cell))
  
	      (set-math-cell yanked-math-cell 'backward backward-math-cell)
	      (set-math-cell yanked-forwardmost-sibling 'forward
			     math-cell-index)
	      (if backward-math-cell
		  (set-math-cell backward-math-cell 'forward yanked-math-cell))
	      (if math-cell-index
		  (set-math-cell math-cell-index
				 'backward yanked-forwardmost-sibling))

	      ;; goto insertion point
	      (goto-math-cell math-cell-index)
	      (math-back-to-cell-label)
	      )

	  ;; else insert after current math-cell

	  ;; find math-cell that ends the parent group and how deep it is
	  (setq last-math-cell math-cell-index)
	  (if (or (not forward-math-cell)
		  (math-cell-start-group-p math-cell-index))
	      (progn
		(setq end-depth 0)
		(setq last-math-cell (find-end-of-math-group last-math-cell))
		(setq end-depth 
		      (- (or (and last-math-cell 
				  (math-cell-depth last-math-cell)) 0)
			 depth))
		))

	  ;; find math-cell 'after' math-cell-index
	  (setq following-math-cell
		(or forward-math-cell 
		    (and last-math-cell (math-cell-next last-math-cell))))

	  ;; set the parent of yanked siblings and remember forwardmost sibling
	  (let ((math-cell yanked-math-cell))
	    (while math-cell
	      (set-math-cell math-cell 'parent parent-math-cell)
	      (setq yanked-forwardmost-sibling math-cell)
	      (setq math-cell (math-cell-forward math-cell))))

	  ;; find the last math-cell of yanked group
	  (setq yanked-group-end yanked-forwardmost-sibling)
	  (while (math-cell-next yanked-group-end)
	    (setq yanked-group-end (math-cell-next yanked-group-end)))

	  ;; adjust depths of neighbors
	  (if (not forward-math-cell)
	      ;; math-cell-index is the last cell in a group or in notebook
	      (progn
		;; change the endGroup values accordingly
		(setq end-groups
		      (or (and last-math-cell
			       (math-cell-end-group last-math-cell)) 0))
		(if (and last-math-cell (< 0 end-depth))
		    (set-math-cell last-math-cell 'endGroup end-depth)
		  (set-math-cell last-math-cell 'endGroup))
		;; add endGroups to end of yanked group
		(let ((new-depth (- end-groups end-depth)))
		  (set-math-cell yanked-group-end 'endGroup
				 (+ (math-cell-end-group yanked-group-end) 
				    new-depth))
		  )))

	  ;; Add linkage to other math-cells
      
	  (set-math-cell yanked-math-cell 'previous last-math-cell)
	  (set-math-cell yanked-group-end 'next following-math-cell)
	  (if following-math-cell
	      (set-math-cell following-math-cell 'previous yanked-group-end))
	  (if last-math-cell
	      (set-math-cell last-math-cell 'next yanked-math-cell))
  
	  (set-math-cell yanked-math-cell 'backward math-cell-index)
	  (set-math-cell yanked-forwardmost-sibling 'forward forward-math-cell)
	  (if forward-math-cell
	      (set-math-cell forward-math-cell 
			     'backward yanked-forwardmost-sibling))
	  (if math-cell-index
	      (set-math-cell math-cell-index 'forward yanked-math-cell))


	  ;; find insertion point
	  (if following-math-cell
	      (progn
		(goto-math-cell following-math-cell)
		(math-back-to-cell-label))
	    (goto-char (point-max))
	    (skip-chars-backward "\n\r")
	    (if (looking-at "[\n\r]")
		(forward-char 1))
	    )
	  )				; if insert-before
      )					; math-cell-index

    ;; fix depth, insert label, insert contents of each math-cell
    (let ((math-cell yanked-math-cell)
	  (delta-depth (- depth (or (math-cell-depth yanked-math-cell) 0))))
      (while (and math-cell (not (eq math-cell following-math-cell)))
	(set-math-cell math-cell 'depth (+ (or (math-cell-depth math-cell) 0)
					   delta-depth))
	(math-ensure-blank-line)
	(insert-new-math-cell-label math-cell)
	(math-label-to-contents)
	(insert (math-cell-contents math-cell) "\n")
	(math-ensure-blank-line)
	(set-math-cell math-cell 'contents)
	(setq yanked-group-end math-cell)
	(setq math-cell (math-cell-next math-cell))
	))
    (show-or-hide-math-cell yanked-math-cell)

    ))


(defun math-set-current-cell-as-kill ()
  "Add the current math-cell or group to the killed-math-cells list,
but dont kill it."
  (interactive)
  (save-excursion 
    (let ((math-cell-index (current-math-cell))
	  (before-change-hook nil)
	  (inhibit-quit t)              ; no quiting in the middle
	  )
      (buffer-flush-undo (current-buffer))
      (unwind-protect
	  (progn
	    ;; remove any previously killed math-cells
	    (remove-killed-math-cells)
	    ;; Add math-cell-index to killed-math-cells, 
	    ;; but specify yank-with-copy.
	    (setq killed-math-cells-buffer (current-buffer))
	    (setq math-yank-with-copy t) ; copies cannot be relinked
	    (setq killed-math-cells (cons math-cell-index killed-math-cells))
	    )
	(buffer-enable-undo (current-buffer)))
      (message "Set current math-cell as kill")
      )))


(defun math-copy-cells (copied-math-cell copied-buffer)
  "Copy the math-cell structure COPIED-MATH-CELL from COPIED-BUFFER
to the math-cell vector in the current buffer.
If copied-math-cell is a group, all math-cells in the group are copied."

  (save-excursion
    (let (math-cell-index
	  new-math-cell
	  copied-math-cell-index
	  copied-math-cell-info
	  copied-math-cell-contents
	  next-math-cell
	  previous-math-cell
	  backward-math-cell
	  parent-math-cell
	  ancestry
	  (depth 0)
	  depth-difference
	  (copy-to-buffer (current-buffer)) ; the buffer to copy to
	  )

      (set-buffer copied-buffer)
      ;; find the math-cell after the math-cell(s) to be copied
      (setq following-copied-math-cell 
	    (if (math-cell-start-group-p copied-math-cell)
		(find-math-cell-after-group copied-math-cell)
	      (math-cell-next copied-math-cell)))

      (setq copied-math-cell-index copied-math-cell)
      (setq depth-difference (math-cell-depth copied-math-cell-index))
    
      (while (not (eq copied-math-cell-index following-copied-math-cell))
	(setq copied-math-cell-info 
	      (copy-sequence
	       (aref math-cell-vector copied-math-cell-index)))
	(setq copied-math-cell-contents
	      (or (math-cell-contents copied-math-cell-index)
		  (math-cell-contents-string copied-math-cell-index)))
	(set-buffer copy-to-buffer)

	(setq math-cell-index (new-math-cell-info copied-math-cell-info))
	;; remember the first new math-cell
	(setq new-math-cell (or new-math-cell math-cell-index))
	(set-math-cell math-cell-index 'contents copied-math-cell-contents)
	(set-math-cell math-cell-index 'point nil)

	;; Set depth
	(set-math-cell math-cell-index 'depth
		  (- (math-cell-depth math-cell-index) depth-difference))

	;; Set linkage to other math-cells
	(set-math-cell math-cell-index 'parent parent-math-cell)

	(set-math-cell math-cell-index 'next nil)
	(set-math-cell math-cell-index 'previous previous-math-cell)
	(if previous-math-cell
	    (set-math-cell previous-math-cell 'next math-cell-index))
	(setq previous-math-cell math-cell-index)
	
	(set-math-cell math-cell-index 'forward nil)
	(set-math-cell math-cell-index 'backward backward-math-cell)
	(if backward-math-cell
	    (set-math-cell backward-math-cell 'forward math-cell-index))
	(setq backward-math-cell math-cell-index)

	;; if it starts a group, remember parent
	(if (math-cell-start-group-p math-cell-index)
	    (progn
	      (setq ancestry (cons math-cell-index ancestry))
	      (setq parent-math-cell math-cell-index) ; eq (car ancestry)
	      (setq backward-math-cell nil)
	      )
	  )

	(let* ((end-groups (or (math-cell-end-group-p math-cell-index) 0))
	       (end-depth (- depth end-groups)))
	   
	  (if (/= end-depth depth)
	      (progn
		(setq backward-math-cell (nth (1- end-groups) ancestry))
		(setq ancestry (nthcdr end-groups ancestry))

		(setq depth end-depth)
		(setq parent-math-cell (car ancestry))
		)))
      
	(set-buffer copied-buffer)
	(setq copied-math-cell-index (math-cell-next copied-math-cell-index))
	)				; while
      (set-buffer copy-to-buffer)
      new-math-cell			; return the first new math-cell
      )))



;;;------------------------------------
;;; Grouping and Ungrouping

(defun math-group-region (start end)
  "Group the math-cells within the region START to END.
Group the math-cells within the common enclosing group
or at the top level."

  (interactive "r")
  (let ((before-change-hook nil)	; allow changes
	(inhibit-quit t)		; no quiting in the middle
	(start-math-cell (progn (goto-char start)
				(beginning-of-line)
				(skip-chars-forward " \t\n\r")
				(current-math-cell)))
	(end-math-cell  (progn (goto-char end)
			       (current-math-cell)))
	)
    (buffer-flush-undo (current-buffer))
    (unwind-protect
	(let ((common (common-math-cell-parent start-math-cell end-math-cell)))
	  (math-group-cells (car common) (cdr common)))
      (math-move-to-cell start-math-cell)
      (buffer-enable-undo (current-buffer))
      )))


(defun math-group-cells (start-of-group end-offspring)
  "Group the math-cells between start-of-group and end-offspring."
  (let (group-math-cell-index
	end-depth
	last-math-cell next-math-cell
	preface)

    ;; Check whether grouping is allowed
    (if (or (= start-of-group end-offspring)
	    (math-cell-start-group-p start-of-group))
	(setq start-of-group
	      (math-insert-new-cell start-of-group 'insert-before
				    '(section))))

    ;; Make start-of-group into start of group

    (goto-math-cell start-of-group)

    (set-math-cell start-of-group 'startGroup t)

    ;; find math-cell that ends the new group
    (setq last-math-cell (find-end-of-math-group end-offspring))

    ;; make last math-cell end one more group
    (let ((new-last-depth (1+ (math-cell-end-group last-math-cell))))
      (if (< 0 new-last-depth)
	  (set-math-cell last-math-cell 'endGroup new-last-depth)
	(set-math-cell last-math-cell 'endGroup)))

    ;; Add linkage to other math-cells
    (let ((next-math-cell (math-cell-next end-offspring))
	  (previous-math-cell (math-cell-previous start-of-group))
	  (backward-math-cell (math-cell-backward start-of-group))
	  (forward-math-cell (math-cell-forward end-offspring))
	  (depth (math-cell-depth start-of-group))
	  )

      (set-math-cell start-of-group 'forward forward-math-cell)
      (set-math-cell end-offspring 'forward nil)
      (if forward-math-cell
	  (set-math-cell forward-math-cell 'backward start-of-group))

      (setq next-math-cell (math-cell-next start-of-group))
      (set-math-cell next-math-cell 'backward nil)

      ;; set parents of offspring to new parent
      (setq last-math-cell
	    (math-cell-next last-math-cell)) ; reuse last-math-cell var
      (while (and next-math-cell (not (eq next-math-cell last-math-cell)))
	(set-math-cell next-math-cell 'parent start-of-group)
	(setq next-math-cell (math-cell-forward next-math-cell)))

      ;; set the depths of all submath-cells to one deeper and fix labels
      (setq next-math-cell (math-cell-next start-of-group))
      (while (and next-math-cell (not (eq next-math-cell last-math-cell)))
	(set-math-cell next-math-cell
		       'depth (1+ (math-cell-depth next-math-cell)))
	(delete-math-cell-label next-math-cell)
	(insert-new-math-cell-label next-math-cell)
	(setq next-math-cell (math-cell-next next-math-cell)))

      (show-or-hide-math-cell start-of-group)
      (delete-math-cell-label start-of-group)
      (insert-new-math-cell-label start-of-group)
      )))


(defun math-ungroup-current-group ()
  "Ungroup the math-cells within the group around point."
  (interactive)
  (let ((before-change-hook nil)	; allow changes
	(inhibit-quit t)		; no quiting in the middle
	(math-cell-index (current-math-cell))
	(group-math-cell-index))
    (setq group-math-cell-index
	  (if (math-cell-start-group-p math-cell-index)
	      math-cell-index
	    (math-cell-parent math-cell-index)))
    (buffer-flush-undo (current-buffer))
    (unwind-protect
	(save-excursion
	  (math-ungroup-cell group-math-cell-index))
      (math-move-to-cell math-cell-index)
      (buffer-enable-undo (current-buffer)))
    ))


(defun math-ungroup-cell (math-cell-index)
  "Ungroup the group MATH-CELL-INDEX."
  (let ((group-math-cell-index math-cell-index)
	last-child-math-cell		; last cell at one level below group
	end-group-math-cell		; cell which ends the group
	)

    (if (math-cell-group-closed-p group-math-cell-index)
	(progn
	  (open-math-group group-math-cell-index)
	  (math-show-group group-math-cell-index)
	  ))
      
    ;; find the math-cell that ends this group
    (setq end-group-math-cell (find-end-of-math-group group-math-cell-index))

    ;; make last math-cell end one less group
    (let ((new-end-depth  (1- (math-cell-end-group end-group-math-cell))))
      (if (< 0 new-end-depth)
	  (set-math-cell end-group-math-cell 'endGroup new-end-depth)
	(set-math-cell end-group-math-cell 'endGroup)))

    ;; make it not a group
    (set-math-cell group-math-cell-index 'startGroup)

    ;; Fix linkage to other math-cells
    (let ((next-math-cell (math-cell-next group-math-cell-index))
	  (previous-math-cell (math-cell-previous group-math-cell-index))
	  (backward-math-cell (math-cell-backward group-math-cell-index))
	  (forward-math-cell (math-cell-forward group-math-cell-index))
	  (parent (math-cell-parent group-math-cell-index))
	  (last-math-cell
	   (math-cell-next end-group-math-cell))
	  )

      (setq last-child-math-cell
	    (find-last-child-math-cell group-math-cell-index))
      (set-math-cell group-math-cell-index 'forward next-math-cell)
      (set-math-cell last-child-math-cell 'forward forward-math-cell)
      (if forward-math-cell
	  (set-math-cell forward-math-cell 'backward last-child-math-cell))

      (set-math-cell next-math-cell 'backward group-math-cell-index)

      ;; set parents of offspring to new parent
      (let ((index next-math-cell))
	(while (and index
		    (not (eq index last-math-cell)))
	  (set-math-cell index 'parent parent)
	  (setq index (math-cell-forward index))))

      ;; set the depths of all sub-cells to one less and fix labels
      (while (and next-math-cell (not (eq next-math-cell last-math-cell)))
	(set-math-cell next-math-cell 'depth
		       (1- (math-cell-depth next-math-cell)))
	(delete-math-cell-label next-math-cell)
	(insert-new-math-cell-label next-math-cell)
	(setq next-math-cell (math-cell-next next-math-cell)))

      (let ((index group-math-cell-index))
	(while (and index
		    (not (eq index last-math-cell)))
	  (show-or-hide-math-cell index)
	  (setq index (math-cell-forward index))))

      (delete-math-cell-label group-math-cell-index)
      (insert-new-math-cell-label group-math-cell-index)
      )))


(defun find-last-child-math-cell (math-cell)
  "Find the last child of the group MATH-CELL."
  (let ((forward-math-cell (math-cell-next math-cell)))
    (while (and forward-math-cell
		(setq math-cell (math-cell-forward forward-math-cell)))
      (setq forward-math-cell math-cell))
    forward-math-cell))


(defun find-end-of-math-group (math-cell)
  "Find the math-cell that ends the group MATH-CELL
or return the math-cell itself if it is not a group."
  (let ((forward-math-cell (find-math-cell-after-group math-cell)))
    (if forward-math-cell
	(math-cell-previous forward-math-cell)
      ;; no more math-cells, so find last math-cell in file
      (let ((last-math-cell))
	(while math-cell
	  (setq last-math-cell math-cell)
	  (setq math-cell (math-cell-next math-cell)))
	last-math-cell
	))))


(defun find-math-cell-after-group (math-cell)
  "Climb the tree until a parent of MATH-CELL has a forward-math-cell.
Return nil if MATH-CELL is nil, or if it has no forward-math-cell."
  (let ((forward-math-cell math-cell))
    (while (and math-cell 
		(not (setq forward-math-cell (math-cell-forward math-cell))))
      (setq math-cell (math-cell-parent math-cell)))
    forward-math-cell))


(defun common-math-cell-parent (math-cell1 math-cell2)
  "Return a cons of the lowest math-cells above math-cell1 and math-cell2
that have the same parent.
One math-cell might be the ancestor of the other."
  (let* ((depth1 (math-cell-depth math-cell1))
	 (depth2 (math-cell-depth math-cell2))
	 (i (- depth1 depth2))
	 )
    (while (< i 0)			; depth1 < depth2
      (setq math-cell2 (math-cell-parent math-cell2))
      (setq i (1+ i)))
    (while (> i 0)			; depth1 > depth2
      (setq math-cell1 (math-cell-parent math-cell1))
      (setq i (1- i)))

    ;; now math-cell1 and math-cell2 are at same depth
    (let ((p1 math-cell1)
	  (p2 math-cell2))
      (while (and p1 p2 (/= p1 p2))
	(setq math-cell1 p1)
	(setq p1 (math-cell-parent p1))
	(setq math-cell2 p2)
	(setq p2 (math-cell-parent p2))
	)
      (cons math-cell1 math-cell2)
      )))



;;;-------------------------------
;;; Splitting and Joining

(defun math-split-group ()
  "Split the current group by dropping the current math-cell and all
following math-cells in the group to the next lower level."
  (interactive)
  (let* ((math-cell-index (current-math-cell))
	 (parent-math-cell (math-cell-parent math-cell-index))
	 (before-change-hook nil)
	 (inhibit-quit t)		; no quiting in the middle
	 )
    (buffer-flush-undo (current-buffer))
    (unwind-protect
	(if (not parent-math-cell)
	    (message "Top level already")

	  (let (;;(last-math-cell (find-last-child-math-cell parent-math-cell))
		(backward-math-cell (math-cell-backward math-cell-index)))
	    (math-ungroup-cell parent-math-cell)
	    (if backward-math-cell
		(math-group-cells parent-math-cell backward-math-cell))
	    ;;	    (check-math-cell math-cell-index)
	    ;;	    (check-math-cell backward-math-cell)
	    ;;	    (check-math-cell parent-math-cell)
	    )
	  )
      (math-move-to-cell math-cell-index)
      (buffer-enable-undo (current-buffer)))
    ))


(defun math-drop-cell ()
  "Drop the current math-cell from the parent group.  not tested!!"
  (interactive)
  (let* ((math-cell-index (current-math-cell))
	 (parent-math-cell (math-cell-parent math-cell-index))
	 (killed-math-cells)		; protect global killed-math-cells
	 (killed-math-cells-buffer)
	 (math-yank-with-copy)
	 (before-change-hook nil)
	 (inhibit-quit t)		; no quiting in the middle
	 )
    (buffer-flush-undo (current-buffer))
    (unwind-protect
	(if parent-math-cell
	    (progn			; trick to move math-cells
	      (math-kill-cell math-cell-index)
	      (math-yank-cells parent-math-cell nil
			       (car killed-math-cells)) ; == math-cell-index
	      )
	  (message "Top level already"))
      (math-move-to-cell math-cell-index)
      (buffer-enable-undo (current-buffer)))
    ))


(defun math-join-cell ()
  "Join the current math-cell with the previous group, or create a group
if the previous math-cell is not the end of a group."
  (interactive)
  (let* ((math-cell-index (current-math-cell))
	 (previous-math-cell (math-cell-previous math-cell-index))
	 (killed-math-cells)		; protect killed math-cells
	 (killed-math-cells-buffer)
	 (math-yank-with-copy)
	 (before-change-hook nil)
	 (inhibit-quit t)		; no quiting in the middle
	 )
    (buffer-flush-undo (current-buffer))
    (unwind-protect
	(if (not previous-math-cell)
	    (message "Nothing to join with.")

	  (if (not (< 0 (math-cell-end-group previous-math-cell)))
	      (if (math-cell-start-group-p previous-math-cell)
		  (message "Can't join with start of group.")
		(math-group-cells previous-math-cell math-cell-index))
	    ;;	    (check-math-cell math-cell-index)
	    ;;	    (check-math-cell previous-math-cell)
	    (math-kill-cell math-cell-index)
	    (math-yank-cells previous-math-cell nil
			     (car killed-math-cells)) ; == math-cell-index
	    ;;	    (check-math-cell math-cell-index)
	    ;;	    (check-math-cell previous-math-cell)
	    ))
      (math-move-to-cell math-cell-index)
      (buffer-enable-undo (current-buffer))
      )))



;;;---------------------------------
;;; Evaluation

(defun math-eval-current-cell ()
  "Evaluate the current math-cell."
  (interactive)
  (let ((math-cell-index (save-excursion (current-math-cell)))
	(before-change-hook nil)
	(inhibit-quit t)		; no quiting in the middle
	)
    (message "eval current math-cell")
    (if (math-cell-inactive-p math-cell-index)
	(message "Cannot evaluate inactive math-cell.  Make it active first.")
      (buffer-flush-undo (current-buffer))
      (unwind-protect
	(progn
	  (if (not (eq math-last-input-cell math-cell-index))
	      (math-autodelete-last-input-cell))
	  (eval-math-cells math-cell-index math-cell-index)
	  ;; assume math-cell-index is still the input cell
	  ;; may be after the wrong math-cell !!
	  (if (not (math-cell-empty math-cell-index))
	      (math-insert-new-input-cell math-cell-index))
	  )
	(buffer-enable-undo (current-buffer)))
      )))


(defun math-eval-region (start end)
  "Evaluate all active math-cells in region."
  (interactive "r")
  (let ((before-change-hook nil)	; allow changes
	(inhibit-quit t)		; no quiting in the middle
	(start-math-cell (progn (goto-char start)
				(beginning-of-line)
				(skip-chars-forward " \t\n\r")
				(current-math-cell)))
	(end-math-cell  (progn (goto-char end)
			       (current-math-cell)))
	)
    (buffer-flush-undo (current-buffer))
    (unwind-protect
	(progn
	  (eval-math-cells start-math-cell end-math-cell)
	  )
      (buffer-enable-undo (current-buffer)))
    ))


(defun eval-math-cells (start-math-cell end-math-cell)
  "Evaluate all the active cells between start-math-cell and end-math-cell.
Return t if anything is evaluated; nil otherwise."
  (let ((active-math-cells
	 (find-math-cells
	  (function (lambda (math-cell)
		      (not (math-cell-inactive-p math-cell))))
	  start-math-cell end-math-cell)))
    ;; first find all math-cells that need to be evaled
    (setq active-math-cells
	  (find-eval-math-cells active-math-cells))

    ;; now loop through all the math-cells and evaluate them
    (mapcar 'eval-math-cell active-math-cells)
    ))


(defun find-eval-math-cells (active-math-cells)
  "Find all the math-cells that need to be evaluated in active-math-cells list.
If any autoActive group is found above an active math-cell, then add all
active math-cells in the group to the evaluation list."
  (let ((autoActive-parents nil)
	(math-cell-list active-math-cells))
    ;;  For each cell in ACTIVE-MATH-CELLS, find the outermost autoActive group
    (while math-cell-list
      (let* ((autoActive-parent nil)
	     (math-cell-index  (car math-cell-list))
	     (parent (math-cell-parent math-cell-index)))
	(setq math-cell-list (cdr math-cell-list))
	(set-math-cell math-cell-index 'eval t)

	;; find the outermost parent that is autoActive
	(while parent
	  (if (math-cell-autoActive-p parent)
	      (setq autoActive-parent parent))
	  (setq parent (math-cell-parent parent))
	  )

	(if (and autoActive-parent
		 (not (memq autoActive-parent autoActive-parents)))

	    ;; eval every active math-cell under this parent 
	    (let ((following-math-cell
		   (find-math-cell-after-group autoActive-parent))
		  (math-cell))
	      (setq autoActive-parents
		    (cons autoActive-parent autoActive-parents))
	      (while (and math-cell (not (eq math-cell following-math-cell)))
		(if (not (math-cell-inactive-p math-cell))
		    (set-math-cell math-cell 'eval t))
		(setq math-cell (math-cell-next math-cell)))))))

    ;; if any autoActive parents found
    (if autoActive-parents
	;; then need to search for all eval math-cells.
	(progn
	  (find-math-cells
	   (function (lambda (math-cell)
		       (math-cell-eval-p math-cell)))))
      active-math-cells)
    ))
	  


(defun math-eval-init-cells ()
  "Evaluate all the active initialization math-cells."
  (let ((init-math-cells
	 (find-math-cells
	  (function (lambda (math-cell)
		      (and (math-cell-initialization-p math-cell)
			   (not (math-cell-inactive-p math-cell)))))
	  )))
    ;; now loop through all the math-cells and evaluate the active ones
    ;; should this be EVALONLY ??
    (mapcar 'eval-only-math-cell init-math-cells)
    ))


(defun find-math-cells (pred &optional start-math-cell end-math-cell)
  "Return a list of all math-cells that match a predicate between
start-math-cell and end-math-cell."

  (if (not start-math-cell)
      (setq start-math-cell (first-math-cell)))
  (let ((math-cell start-math-cell)
	(math-cell-list nil))
    (while (not (eq math-cell end-math-cell) )
      (if (funcall pred math-cell)
	  (setq math-cell-list (cons math-cell math-cell-list))
	)
      (setq math-cell (math-cell-next math-cell))
      )
    ;; check end-math-cell
    (if (and end-math-cell (funcall pred math-cell))
	(setq math-cell-list (cons math-cell math-cell-list))
      )
    (nreverse math-cell-list)
    ))


(defun eval-math-cell (math-cell-index)
  "Evaluate the contents of math-cell-index.
Return nil if there was a syntax error."
  ;; needs to work with closed math-cells too!!
  (let ((contents
	 (if (math-cell-output-p math-cell-index)
	     (math-cell-input-form math-cell-index)
	   (math-cell-contents-string math-cell-index)))
	)

    ;; cancel eval field
    (set-math-cell math-cell-index 'eval nil)

    (if (not (string-match "[^ \t\f\n\r]" contents))
	(progn
	  (message "Empty cell")
	  (ding) (sit-for 1))
      
      (if (not math-process)
	  (run-math))			; do this now to get input prompt

      (setq math-this-input-cell math-cell-index)

      ;; relabel the cell to be evaluated and set font to input
      (set-math-cell math-cell-index 'name (list math-input-prompt))
      (set-math-cell math-cell-index 'font '(input))
      (delete-math-cell-label math-cell-index)
      (insert-new-math-cell-label math-cell-index)

      (math-reset-pagewidth)

      (setq math-message-blocks nil)	; remove previous messages before eval

      (math-send-string-action contents 'aEVAL)
      (math-kernel-loop)
      
      
      ;; check whether syntax error occurred
      (if (eq 'syntax-error math-eval-result)
	  ;; goto beginning of math-cell in error and move to error
	  (progn
	    (math-move-to-cell math-cell-index)
	    (forward-char math-error-offset)
	    (error "Syntax Error at this position")
	    nil
	    )

	(insert-math-result math-cell-index)
	t)
      )))


(defun eval-only-math-cell (math-cell-index)
  "Evaluate the contents of math-cell-index with EVALONLY."
  ;; needs to work with closed math-cells too!!
  (let ((contents
	 (if (math-cell-output-p math-cell-index)
	     (math-cell-input-form math-cell-index)
	   (math-cell-contents-string math-cell-index)))
	)

    (if (not (string-match "[^ \t\f\n\r]" contents))
	(progn
	  (message "Empty cell")
	  (ding) (sit-for 1))
      
      ;; relabel the math-cell to be evaluated
;;;      (set-math-cell math-cell-index 'name (list math-input-prompt))

      (math-send-string-action contents 'aEVALONLY)
      (math-kernel-loop)
      )))


(defun insert-math-result (math-cell-index)
  "Insert a math-cell that represents the mathematica output of evaluating
the contents of math-cell-index.
Group the new math-cell with math-cell-index, unless math-cell-index is nil.
If the input math-cell was the start of a group, kill the members
of the group that are output math-cells from a previous evaluation."

  (let ((next-math-cell math-cell-index)) ; insert cells after input cell
    
    (if (and math-cell-index (math-cell-start-group-p math-cell-index))
	(progn
	  (remove-killed-math-cells)
	  (math-ungroup-cell math-cell-index)

	  ;; delete any intervening print, info, and message cells
	  (setq next-math-cell (math-cell-next next-math-cell))
	  (while (and next-math-cell
		      (memq (math-cell-font next-math-cell)
			    '(message print info)))
	    (math-kill-cell (prog1 next-math-cell
			      (setq next-math-cell
				    (math-cell-next next-math-cell)))))
	  
	  ;; delete the output math-cell
	  (if (and next-math-cell
		   (math-cell-output-p next-math-cell))
	      (math-kill-cell next-math-cell))
	  ))

    (setq next-math-cell math-cell-index)
    ;; first insert any info, print, and message output

    (setq math-info-blocks
	  (nreverse math-info-blocks))
    (if math-info-blocks
	(let ((info-block (apply 'concat math-info-blocks)))
	  (setq next-math-cell
		(math-insert-output-cell info-block next-math-cell 'print))
	  ))

    (setq math-print-blocks 
	  (nreverse math-print-blocks))
    (if math-print-blocks
	(let ((print-block (apply 'concat math-print-blocks)))
	  (setq next-math-cell
		(math-insert-output-cell print-block next-math-cell 'print))
	  ))
    
    ;; each message gets its own math-cell
    (setq math-message-blocks
	  (nreverse math-message-blocks))
    (while math-message-blocks
      (setq next-math-cell
	    (math-insert-output-cell (car math-message-blocks)
				     next-math-cell 'message))
      (setq math-message-blocks (cdr math-message-blocks))
      )

    ;; add output math-cells
    (if math-output-form
	(let ((output-math-cell-index (new-math-cell-info)))

	  (set-math-cell output-math-cell-index 'contents math-output-form)
	  (if math-input-form
	      (set-math-cell output-math-cell-index
			     'input-form math-input-form))
	  (set-math-cell output-math-cell-index 'name
			 (list (concat math-output-prompt " ")))
	  (set-math-cell output-math-cell-index 'last-indent-width 0)
	  (set-math-cell output-math-cell-index 'inactive t)
	  (set-math-cell output-math-cell-index 'output t)
	  (set-math-cell output-math-cell-index 'font '(output))

	  ;; insert the output math-cell after math-cell-index
	  (math-yank-cells next-math-cell nil output-math-cell-index)
	  ;; ensure correct indentation
	  (adjust-math-cell-indent output-math-cell-index)

	  (setq next-math-cell output-math-cell-index)
	  ))

    ;; group the input with all the output math-cells
    (if (and math-cell-index 
	     (not (eq math-cell-index next-math-cell)))
	(math-group-cells math-cell-index next-math-cell)
      ;; set up for insertion of new math-cell.
      (setq math-cell-index next-math-cell)
      )))


(defun math-insert-output-cell (contents last-math-cell font)
  "Insert an output math-cell with CONTENTS after LAST-MATH-CELL.
Set the font of the math-cell to FONT.
Return the new math-cell index."
  (let ((math-cell-index (new-math-cell-info))
	math-cell-name)
	
    ;; add fields to output math-cell
    (if (string-match (concat "^" math-cell-message-label) contents)
	(progn
	  (setq math-cell-name (substring contents
					  (match-beginning 0)
					  (match-end 0)))
	  (setq contents (substring contents (1+ (match-end 0))))))
	  
    (set-math-cell math-cell-index 'contents contents)
    (if math-cell-name
	(set-math-cell math-cell-index 'name
		       (list (concat math-cell-name " "))))
    (set-math-cell math-cell-index 'inactive t)
    (set-math-cell math-cell-index 'font
		   (list font))
    (math-yank-cells last-math-cell nil math-cell-index)
    math-cell-index
    )
  )


(defvar math-last-input-cell nil
  "Last input math-cell - used to autodelete if empty.")

(defvar math-this-input-cell nil
  "This input cell - used to avoid autodeleting name.")


(defun math-autodelete-last-input-cell ()
  "Autodelete last input math-cell if empty.  Remove its name anyway."

  (if (and math-last-input-cell
	   (aref math-cell-vector math-last-input-cell)
	   ;; killed or dead cells have no point, no need to delete them
	   (math-cell-point math-last-input-cell))
	       
      (if (and (math-cell-empty math-last-input-cell)
	       (not (math-cell-start-group-p math-last-input-cell)))
	  (save-excursion
	    (remove-killed-math-cells)
	    (math-kill-cell math-last-input-cell)
	    (remove-killed-math-cells)
	    )

	;; not empty
	(if (not (eq math-last-input-cell math-this-input-cell))
	    (progn			; remove the old input prompt anyway
	      (set-math-cell math-last-input-cell 'name nil)
	      (delete-math-cell-label math-last-input-cell)
	      (insert-new-math-cell-label math-last-input-cell)))
	))
  (setq math-last-input-cell nil)
  )



(defun math-insert-new-input-cell (math-cell-index)
  "Insert a new input math-cell after MATH-CELL-INDEX."
  (setq math-last-input-cell
	(math-insert-new-cell math-cell-index
			nil
			'(input)
			(list math-input-prompt))))



;;;------------------
;;; Completion

(defun math-complete-symbol ()
  ;; adapted from lisp-complete-symbol
  ;; should remove the window afterwards!!
  "Perform completion on Mathematica symbol preceding point.
That symbol is compared against the symbols that exist
and any additional characters determined by what is there
are inserted.
If the symbol starts just after an open-parenthesis,
only symbols with function definitions are considered.
Otherwise, all symbols with function definitions, values
or properties are considered."
  (interactive)
  (let* ((end (point))
	 (beg (save-excursion
		(skip-chars-backward "a-zA-Z0-9")
		(backward-prefix-chars)
		(math-check-insertion (point) (- end (point)))
		(point)))
	 (pattern (buffer-substring beg end))
		      
	 math-symbol-list
	 completion)
    
    (if (= 0 (length pattern))
	(message "Nothing to complete.")

      (setq math-symbol-list (math-get-completion pattern))
      (setq completion (try-completion pattern math-symbol-list))

      (cond ((eq completion t)
	     (message "You got it already."))
	    ((null completion)
	     (message "Can't find completion for \"%s\"" pattern)
	     (ding))
	    ((not (string= pattern completion))
	     (delete-region beg end)
	     (insert completion))
	    (t
	     (message "Making completion list...")
	     (let ((list (all-completions pattern math-symbol-list)))
	       (with-output-to-temp-buffer "*Completions*"
		 (display-completion-list list)))
	     (message "Making completion list...%s" "done"))))))



;;;--------------------------------
;;; Converting the notebook to internal and external form


(defun math-next-preface ()
  "Skip forward to just before the next math-cell-heading line.
If no next preface then move to after last non-whitespace and return nil"
  (if (re-search-forward math-cell-heading-start-regexp
                         nil 'move)
      (goto-char (match-beginning 0))
    (goto-char (point-max))
;;    (skip-chars-backward " \t\n\r\f")  ; should be last non-whitespace
;;    (re-search-backward "[\n\r]")
;;    (if (looking-at math-cell-marking-regexp)
;;      (goto-char (match-end 0)))
    nil
    ))


;;(defun math-next-cell-heading ()
;;  "Move to just inside the beginning of the next math-cell-heading line."
;;  (interactive)
;;  (re-search-forward math-cell-heading-start-regexp
;;                     nil 'move))


;;(defun math-end-of-cell-heading ()
;;  "Move to just after the end of the current or next math-cell-heading line."
;;  (re-search-forward math-cell-heading-end-regexp
;;                         nil 'move)
;;;      (goto-char (match-beginning 0))
;;  )



(defvar math-nb-control nil
  "The heading for the notebook.")


(defun print-math-cell-value (field value)
  (if value
      (progn
	(princ (format "%s" field))
	(if (listp value)
	    (if (not (eq (car value) t)) ; kluge
		(progn
		  (princ "=")
		  (while value
		    (princ (format "%s" (car value)))
		    (setq value (cdr value))
		    (if value (princ ", ")))))
	  (if (not (eq value t))
	      (progn
		(princ "=")
		(princ (format "%s" value))))
	  ))))


(defun write-macnb-math-cell (info contents)
  "Write out the heading info in INFO with content CONTENTS."
  (let (output-form
	styles
	(init (and math-cell-index  ; from write-notebook
		   (math-cell-initialization-p math-cell-index)
		   (not (math-cell-inactive-p math-cell-index))))
	)

;;;    (princ "\n") ; to be sure
    (if (not contents)
	(princ ":"))			; extra colon for notebook control
    (princ ":[")
    
    (if info
	(let ((i 0)
	      (field-list math-cell-fields)
	      field
	      value)
	  (while (< i math-cell-standard-fields)
	    (setq field (car field-list))
	    (setq field-list (cdr field-list))
	    (setq value (aref info i))
	    (setq i (1+ i))
	
	    (if value
		(progn
		  (cond
		   ((and (eq field 'endGroup) value)
					; unravel endGroups
		    (while (> value 1)
		      (setq value (1- value))
		      (princ "endGroup; "))
		    (princ "endGroup"))

		   ((eq field 'name)
		    (princ (format "%s=\"%s\"" field (car value)))
		    )

		   ((eq field 'input-form)
		    (setq output-form contents)
		    (setq contents value))

		   ((eq field 'output-form)
		    (setq output-form value))

		   ((eq field 'styles)
		    (setq styles value))
	 
		   ((eq field 'extras)
		    (while value
		      (print-math-cell-value
		       (car (car value)) (cdr (car value)))
		      (setq value (cdr value))
		      (if value (princ ";\n\t"))
		      ))
		  
		   (t			; any other field
		    (print-math-cell-value field value)
		    ))
		  ;; if more to follow, append semi
		  (if (< i number-of-math-cell-fields)
		      (princ ";\n\t"))
		  )))))
    (princ "]\n")

    ;; if an init math-cell, uncomment the contents
    (if init
	(princ "*)\n"))
    (if contents
	(princ contents))
    (princ "\n")
    (if init
	(princ "(*\n"))
    
    (if output-form
	(progn
	  (princ ";[o]\n")
	  (princ output-form)
	  (princ "\n")))

    (if styles
	(progn
	  (princ ";[s]")
	  (princ styles)
;;;	  (princ "\n")
	  ))
    ))


(defun write-math-mac-notebook ()
  "Write the current math buffer as a Mac-style notebook."
  (interactive)
  (let ((filename (buffer-file-name))
	(buf (get-buffer-create "*notebook*"))
	(math-cell-index 0)
	(vector-length math-cell-vector-length)
;;	(temp-buffer-show-hook 'write-notebook-file) ; kludgy
	)
    (save-excursion
      (remove-killed-math-cells)

      (with-output-to-temp-buffer (buffer-name buf)

	(princ "(*^\n")
	
	;; print notebook math-cell
	(write-macnb-math-cell 
	 (math-cell-alist-to-vector math-nb-control) nil)

	(setq math-cell-index (first-math-cell))
	
	(while math-cell-index
	  (if math-show-debug (progn
				(message "") (sit-for 0)
				(message "Writing notebook...")))
	  ;; dont modify the actual math-cell info
	  (let ((math-cell-info (aref math-cell-vector math-cell-index)))
	    (if (not math-cell-info)
		nil
	      (write-macnb-math-cell
	       math-cell-info
	       (math-cell-contents-without-indent math-cell-index))
	      )
	    )				; let
	  (setq math-cell-index (math-cell-next math-cell-index))
	  )

	(princ "\n^*)\n")
	)				; with-output

      (set-buffer buf)
      (math-flag-region (point-min) (point-max) ?\n) ; make all readable
      (write-file (concat (math-file-name-root filename) math-nb-suffix))

      (kill-buffer buf)
      filename
      ))
;;  (clear-visited-file-modtime)
  )


(defun write-math-package ()
  "Write the current math buffer as a math package."
  (interactive)
  (let ((filename (buffer-file-name))
	(buf (get-buffer-create "*package*"))
	(math-cell-index 0)
	(vector-length math-cell-vector-length)
	)
    (save-excursion
      (remove-killed-math-cells)

      (with-output-to-temp-buffer (buffer-name buf)

	(setq math-cell-index (first-math-cell))
	
	(while math-cell-index
	  (if (eq 'input (math-cell-font math-cell-index))
	      (progn
		(princ (math-cell-contents-without-indent math-cell-index))
		(princ "\n\n"))
	    (princ "(*\n")
	    (princ (math-cell-contents-without-indent math-cell-index))
	    (princ "\n*)\n\n"))
	  (setq math-cell-index (math-cell-next math-cell-index))
	  )
	)				; with-output

      (set-buffer buf)
      (math-flag-region (point-min) (point-max) ?\n) ; make all readable
      (write-file (concat (math-file-name-root filename) math-package-suffix))

      (kill-buffer buf)
      filename
      ))
;;  (clear-visited-file-modtime)
  )


(defun write-math-lisp-notebook ()
  "Write the notebook as an emacs lisp notebook file.
The file name is the same as the current buffer's filename."
  (let ((filename (buffer-file-name))  ; used by write-notebook-lisp-file
	(notebook-buf (current-buffer))
	(notebook-len (buffer-size))
	(buf (get-buffer-create " *math-cell-vector*"))
	)
    (save-excursion
      (remove-killed-math-cells)

      (set-buffer buf)
      (set-visited-file-name nil)

      ;; insert the contents of the notebook buffer
      (erase-buffer)
      (insert-buffer-substring notebook-buf 1 notebook-len)

      ;; create the external lisp representation
      (set-buffer notebook-buf)

      (let ((nbl)
	    ;; a vector of all math-cells with marks converted to points
	    (math-cells (make-vector math-cell-vector-length nil))
	    (math-cell-index 0)
	    (point-field (get 'point 'math-cell-field))
	    )

	;; loop through each member of math-cell-vector
	(while (< math-cell-index math-cell-vector-length)
	  (if (aref math-cell-vector math-cell-index)
	      
	      ;; build a copy of the math-cell
	      (let ((info-vector
		     (copy-sequence (aref math-cell-vector math-cell-index))))
		;; convert mark to point
		(aset info-vector point-field
		      (marker-position (aref info-vector point-field)))
		(aset math-cells math-cell-index info-vector)  ; store it
		))
	  (setq math-cell-index (1+ math-cell-index)))

	;; now build the structure
	(setq nbl (` (progn
		       (setq math-mode-version 1)
		       (setq math-use-structure-labels
			     (, math-use-structure-labels))
		       (setq free-cells (quote (, free-math-cells)) )
		       (setq math-last-input-cell
			     (, math-last-input-cell))
		       (setq notebook-control 
			     (quote (, math-nb-control)))
		       ;; write out the fields used so future changes work
		       (setq notebook-cell-fields
			     (quote (, math-cell-fields)))
		       (setq cell-vector (, math-cells))
		       )))

	;; append the structure to the new buffer
	(set-buffer buf)
	(goto-char (point-max))
	(print nbl (current-buffer))
	(write-region (point-min) (point-max) 
		      filename nil t)
	))

    (kill-buffer buf)
    (set-buffer notebook-buf)
    (not-modified)
    (clear-visited-file-modtime)
    ))


;;(defun pairify (names values)
;;  "Return an alist of NAMES and VALUES using
;;pairwise elements of those lists.  If there are not enough VALUES, use nil."
;;  (mapcar (function (lambda (name)
;;		      (cons name (prog1 (car values)
;;				   (setq values (cdr values))))))
;;	  names))
  


(defun convert-math-notebook-file ()
  "Convert a buffer containing Mac-style notebook.
First convert it to lisp forms using the external converter.
Then convert the lisp forms into internal form."
  (message "Reading notebook...")

  ;; first replace any odd chars with octal equivalent
  (goto-char (point-min))
  (while (re-search-forward "[^\n\r\b\t -~]" nil t)
    (replace-match (format "\\\\%o" (char-after (match-beginning 0)))))

  ;; now send through converter to get lisp forms.
  (call-process-region (point-min) (point-max) math-macnb2nbl t t)
  
  (let ((before-change-hook nil)
	(inhibit-quit t)
	(first-math-cell 0)
	math-cell-index
	(i 0)
	)
    
    (let (math-mode-version
	  free-cells
	  ;;	    math-last-input-cell  ; same
	  notebook-control
	  notebook-cell-fields
	  cell-vector)

      (eval-current-buffer)		; buffer of elisp representing notebook

      ;; do version specific conversion here
      (setq free-math-cells free-cells)
      (setq math-nb-control notebook-control)
      (setq math-cell-vector cell-vector)
      )
    (setq math-cell-vector-length (length math-cell-vector))

    ;; convert the math-cells from alists to vectors
    (while (< i math-cell-vector-length)
      (aset math-cell-vector i
	    (math-cell-alist-to-vector (aref math-cell-vector i)))
      (setq i (1+ i)))

    ;; add the linkage between math-cells
    (add-math-cell-structure)

    (setq first-math-cell (first-math-cell))

    (erase-buffer)			; erase the lisp forms


    ;; insert contents of all math-cells
    (math-yank-cells nil nil first-math-cell)

    (if (not first-math-cell)
	(math-insert-new-input-cell nil)  ; no cells, so make one
      (save-excursion
	;; loop through all top level math-cells and show or hide them
	(setq math-cell-index first-math-cell)
	(while math-cell-index
	  (show-or-hide-math-cell math-cell-index)
	  (setq math-cell-index (math-cell-forward math-cell-index))
	  )))

    (message "")
    ))



(defun math-cell-alist-to-vector (info)
  "Convert the math-cell heading alist INFO to a vector, if non-nil."
  (if info
      (let ((heading-vector (make-vector number-of-math-cell-fields nil))
	    (extra-index (get 'extras 'math-cell-field)))
	(mapcar (function
		 (lambda (item)
		   (let* ((index (get (car item) 'math-cell-field))
			  (value (and index (aref heading-vector index))))
		     (if index
			 (aset heading-vector index
			       ;; if already has a value
			       (if value
				   ;; cons new value with old
				   (cons (cdr item) value)
				 (cdr item)))
		       (aset heading-vector extra-index
			     (cons item
				   (aref heading-vector extra-index)))))
		   ))
		info)
	heading-vector
	)))


(defun add-math-cell-structure ()
  "Add the math-cell structure to the list of math-cells."

  (let (math-cell-index
	first-math-cell
	next-math-cell
	previous-math-cell
	backward-math-cell
	parent-math-cell
	ancestry
	(depth 0)
	)

    (message "Converting notebook...")

    (setq math-cell-index 0)
    (while (< math-cell-index math-cell-vector-length)

      ;; Add depth
      (set-math-cell math-cell-index 'depth depth)

      ;; Add linkage to other math-cells
      (set-math-cell math-cell-index 'previous previous-math-cell)
      (if previous-math-cell
	  (set-math-cell previous-math-cell 'next math-cell-index))
      (setq previous-math-cell math-cell-index)
      (set-math-cell math-cell-index 'parent parent-math-cell)
	
      (if backward-math-cell
	  (set-math-cell backward-math-cell 'forward math-cell-index))
      (set-math-cell math-cell-index 'backward backward-math-cell)
      (setq backward-math-cell math-cell-index)

	
      (if (math-cell-start-group-p math-cell-index)
	  (progn
	    (setq depth (1+ depth))
	    (setq ancestry (cons math-cell-index ancestry))
	    (setq parent-math-cell math-cell-index) ; eq (car ancestry)
	    (setq backward-math-cell nil)
	    )
	)

      (let ((end-depth depth)
	    (end-groups (math-cell-end-group-p math-cell-index))) ; a list of nils
	;; count endGroups to collect into one field
	(while end-groups
	  (setq backward-math-cell (car ancestry))
	  (setq ancestry (cdr ancestry))
	  (setq end-depth (1- end-depth))
	  (setq end-groups (and (listp end-groups) (cdr end-groups))))
	   
	(if (/= end-depth depth)
	    (progn
	      (set-math-cell math-cell-index 'endGroup (- depth end-depth))
	      (setq depth end-depth)
	      (setq parent-math-cell (car ancestry))
	      )))
	
      (setq math-cell-index (1+ math-cell-index))
      )					; while
    ))


(defun convert-math-mode-lisp-to-internal ()
  "Convert a buffer containing math-mode notebook form into internal form."
  (let ((before-change-hook nil)
	(filename (buffer-file-name))
	(first-math-cell 0)
	math-cell-index
	(i 0)
	)
    (message "Reading math-mode notebook...")

    ;; evaluate elisp representing math-cell structure
    (goto-char (point-max))
    (skip-chars-backward " \n\t")
    (backward-sexp 1)
    (save-excursion
      (let (math-mode-version
	    free-cells
;;	    math-last-input-cell  ; same
	    notebook-control
	    notebook-cell-fields
	    cell-vector)
	(eval (read (current-buffer)))
	
	;; do version specific conversion here
	(setq free-math-cells free-cells)
	(setq math-nb-control notebook-control)
	(setq math-cell-vector cell-vector)
	))
    (delete-region (point) (point-max))

    (setq math-cell-vector-length (length math-cell-vector))

    ;; future versions should check that math-cell-fields is correct
    ;; and convert math-cells if needed
    (let ((point-field (get 'point 'math-cell-field)))
      (setq math-cell-index 0)
      (while (< math-cell-index math-cell-vector-length)
	(if (aref math-cell-vector math-cell-index)
	    ;; convert points to marks
	    (let ((math-cell-marker
		   (progn
		     (goto-char (math-cell-point math-cell-index))
		     (point-marker))))
	      (set-math-cell math-cell-index 'point math-cell-marker)
	      (add-math-cell-marker math-cell-index math-cell-marker)
	      ))
	(setq math-cell-index (1+ math-cell-index))
	))

    (setq first-math-cell (first-math-cell))

    (if (not first-math-cell)
	(math-insert-new-input-cell nil)  ;no cells, so make one
      (save-excursion
	;; loop through all top level math-cells and show or hide them
	;; this is redundant with the show/hide routine
	(setq math-cell-index first-math-cell)
	(while math-cell-index
	    (if (math-cell-start-group-p math-cell-index)

		(if (math-cell-group-closed-p math-cell-index)
		    (math-hide-group math-cell-index)
		  (math-show-group math-cell-index)
		  )

	      (if (math-cell-closed-p math-cell-index)
		  (math-hide-entry math-cell-index)
		(math-show-entry math-cell-index)))
	    (setq math-cell-index (math-cell-forward math-cell-index))
	    )))

    (message "")
    ))


(defun convert-math-package ()
  "Convert the package format Mathematica expressions in current buffer."
  ;; Make a cell out of every group of lines separated by a blank line.
  ;; Remove any cell labels at the start of each line (or each group?)
  ;; This can be used to recover a notebook file that got messed up.

  ;; Could make this work for insertion of a package in a file!!
  
  ;; Test if in math-mode?


  (goto-char (point-min))
  (skip-chars-forward " \t\n\r")
  (delete-region (point-min) (point))  ; delete extra whitespace
  (insert "\n")

  (let ((paragraph-separator-regexp "[\n\r][ \t\n\r]*[\n\r]") ; at least 2
	(current-cell nil)
	(before-change-hook nil))

    (message "Converting Mathematica package...")

    (while (not (= (point) (point-max)))
      (forward-char -1)  ; at start of label?
      (if (looking-at math-cell-label-regexp)
	  ;; delete any existing cell label
	  (delete-region (point)
			 (progn
			   (re-search-forward math-cell-label-regexp)
			   (point))))
      (save-restriction
	(narrow-to-region (point-min) (point))
	(setq current-cell (math-insert-new-input-cell current-cell)))

      ;; delete the whitespace between new cell label and start of next text
      (delete-region (point)
		     (progn
		       (skip-chars-forward " \t\n\r")
		       (point)))

      ;; find start of next cell
      (re-search-forward paragraph-separator-regexp (point-max) 'move)
      )
    (math-goto-first-cell)
    ))


(defun math-save-buffer ()
  "Save a math buffer by saving the lisp file instead.
Called as a write-file-hooks function."

  (if math-mode
      (progn
        (if math-show-debug
            (to-math-debug (format "Save math buffer: %s\n" (current-buffer))))

	(let* ((filename (file-name-nondirectory (buffer-file-name)))
	       (start-with-nbl (string-match math-nbl-regexp filename))
	       (filename-root (math-file-name-root filename))
	       (nb-filename (concat filename-root math-nb-suffix))
	       (nbl-filename (concat filename-root math-nbl-suffix)))

	  (set-visited-file-name nbl-filename)
	  (if (not start-with-nbl)  ; this should never be true
	      (write-math-mac-notebook))
	  ;; write as lisp-notebook anyway
	  (write-math-lisp-notebook)
	  )

	(set-buffer-modified-p nil)
	t ; dont save the buffer text
        ;; note: even if the buffer text is not saved, the visited file will
	;; still have been copied to the backup file.
	))
  )




;;;---------------------------------------
;;; Math-Cell labels


(defun delete-math-cell-label (math-cell-index)
  "Delete the math-cell label before MATH-CELL-INDEX.
If nil, then delete math-cell label before the last math-cell.
Leave point at the deletion."

  (if (not math-cell-index)
      (error "delete-math-cell-label")
    (goto-math-cell math-cell-index)
    (math-back-to-cell-label)
    )

  (let ((where (point)))
    (delete-region
     where
     (progn
       (math-match-cell-label)
       (point))
     ))

  ;; delete the math-cell marker since we will need to create another one
  (delete-math-cell-marker math-cell-index)
  )
   


(defun math-toggle-structure-labels ()
  "Toggle whether the math-cell structure is shown for math-cell labels."
  ;; works ok, but messes up with closed math-cells!!
  (interactive)
  (message "Relabeling...")
  (setq math-use-structure-labels (not math-use-structure-labels))
  (let ((math-cell-index (first-math-cell))
	;;	  (current-math-cell (current-math-cell))
	(before-change-hook nil)
	(inhibit-quit t)		; no quiting in the middle
	)
    (buffer-flush-undo (current-buffer)) ; better not fail
    (unwind-protect
	(save-excursion
	  (while (and math-cell-index (not quit-flag)) ; allow quiting
	    (delete-math-cell-label math-cell-index)
	    (insert-new-math-cell-label math-cell-index)
	    (setq math-cell-index (math-cell-next math-cell-index))
	    ;;	(sit-for 0)
	    )
	  (setq math-cell-index (first-math-cell))
	  (while math-cell-index
	    (show-or-hide-math-cell math-cell-index)
	    (setq math-cell-index (math-cell-forward math-cell-index)))
	  )
      (buffer-enable-undo (current-buffer))))
  (message "")
  )
      
  

(defun insert-new-math-cell-label (math-cell-index)
  "Insert label for math-cell MATH-CELL-INDEX.  
If nil, insert label at end of file."

  (let ((before-change-hook nil))

    (if (not math-cell-index)
	(error "Called insert-new-math-cell-label with nil argument."))
    
    (let* ((depth (or (math-cell-depth math-cell-index) 0))
	   (group (math-cell-start-group-p math-cell-index))
	   (where (math-cell-point math-cell-index))
	   (name (math-cell-name math-cell-index)))

      (if (not where)			; assume we are in the right place.
	  (setq where (point)))
      (goto-char where)

      (insert "\n")			; necessary to match label
      (if (or (not name) math-use-structure-labels)
	  (insert (if group
		      math-group-beginning-char
		    math-cell-beginning-char)
		  (make-string (1+ depth) math-cell-depth-char) " "))
      (if name (insert name))
      
      ;; remember the length of the label for the next indentation
      (set-math-cell math-cell-index 'next-indent-width
		(- (point) where 1))

      ;; mark second char of math-cell label
      (beginning-of-line)
      (forward-char 1)
      (let ((math-cell-marker (point-marker)))
	(set-math-cell math-cell-index 'point math-cell-marker)
	(replace-math-cell-marker math-cell-index math-cell-marker))
      )))



;;;--------------------------------
;;; Parsing math-cell headings
;;; This is only done when a notebook is converted to internal form.
;;;  **not used since it is too slow.  use macnb2nbl instead

(defun math-list-string (list)
  "Convert the elements of the LIST into a string."
  (substring (prin1-to-string list) 1 -1))

(defun math-parse-cell-heading (heading-string)
  "Parse the HEADING-STRING and return an alist representation of its
contents where each element is a control/token-list pair.  The
HEADING-STRING does not include the enclosing brackets."

  (let ((heading nil)
	(heading-start 0)
	(heading-length (length heading-string))
	(next-token nil)
	)
    (math-next-cell-heading-token) ; get first token
    (while (eq next-token 'semi)
      (math-next-cell-heading-token))
    (while next-token
;;      (message "next token: %s" next-token) (sit-for 1)
      (let ((control next-token)
	    (value-list nil)
	    (value nil)
	    )
	(math-next-cell-heading-token)
	(while (eq next-token 'semi)
	    (math-next-cell-heading-token))
	(if (eq next-token 'equal)
	    (progn  ; get list of values
	      (setq next-token 'comma) ; hack

	      (while (eq next-token 'comma)
		(setq value nil)
		(math-next-cell-heading-token)
;;		(message "next value: %s" next-token) (sit-for 1)
		(while (not (memq next-token '(comma semi nil)))
		  (setq value (cons next-token value))
		  (math-next-cell-heading-token)
		  )
		(if (< 1 (length value))  ; convert to string of elements
		    (setq value (math-list-string value))
		  (setq value (car value)))
		(setq value-list (cons value value-list))
		)))
	(setq heading
	      (cons (cons control (or (nreverse value-list) t))
		    heading))
	(while (eq next-token 'semi)
	    (math-next-cell-heading-token))
	))
    heading
    )
  )

  

(defconst math-cell-heading-token-regexp  ; dont put anything after next lines
  "^\\([,;=]\\|\
\"[^\"\n]*\"\\|\
In\\[[0-9]+\\] ?:= ?\\|\
Out\\[[0-9]+\\]= ?\\|\
\\w+\\)"
  "Regexp that matches the next token in the input string.
Probably wrong for In and/or Out prompts.")


;; (defconst math-end-of-comment "\\*)")


(defun math-next-cell-heading-token ()
  "Get the next math-cell heading token."
  ;; too slow!!
  ;;   (message "heading-start = %d" heading-start) (sit-for 1)

  (if (string-match "^[ \t\n\r\f]+" heading-string heading-start)
      (progn
	;;	(message "skipping: %s" heading-string) (sit-for 2)
	;; skip whitespace
	;;	(setq heading-start (match-end 0))
	;; stick newline in string so ^ matches on the next string-match
	(aset heading-string (1- (match-end 0)) ?\n)
	))

  ;; no comments allowed inside headers
  ;;  (if (string-match "^(\\*" heading-string heading-start)
  ;;    (setq heading-start (match-end 0))
  ;;    (aset heading-string (1- heading-start) ?\n)
  ;;    (string-match math-end-of-comment
  ;;          heading-string heading-start) ; eat comment
  ;;    (setq heading-start (match-end 0))
  ;;    (aset heading-string (1- heading-start) ?\n)
  ;;    )


  (if (< (1+ heading-start) heading-length)
      (if (string-match math-cell-heading-token-regexp
			heading-string heading-start)
	  (let ((token (substring heading-string
				  (match-beginning 0)
				  (match-end 0))))
	    (setq heading-start (match-end 0))
	    (aset heading-string (1- heading-start) ?\n)
	    ;;	    (message "token: %s" token) (sit-for 1)
	    (setq next-token
		  (cond
		   ((string-equal token ",") 'comma)
		   ((string-equal token ";") 'semi)
		   ((string-equal token "=") 'equal)
		   ((string-match "^[0-9]+$" token) (string-to-int token))
		   ((string-match "\"[^\"\n]*\"" token)
		    (car (read-from-string token)))
		   (t (intern token))))
	    )
	;; (error "Bad heading: %s" heading-string)
	(setq next-token nil)
	)
    (setq next-token nil)		; end of tokens
    )
  )



;;;-------------------
;;; Editing constraints
;;; These use the before-change-hook, if implemented, to check
;;; that editing does not destroy math-cell labels and hidden marks.


(defvar current-math-cell-start nil
  "Buffer local.  A mark at the start position of current math-cell in current
buffer.")

(defvar current-math-cell-end nil
  "Buffer local.  A mark at the end position of current math-cell in current
buffer.")

(defvar current-math-cell-index nil
  "Buffer local.  The index of the current math-cell.")


(defun math-check-deletion (pos len)
  "Check that deletion is within a math-cell."
  (if (or (< pos current-math-cell-start)
	  (> (+ pos len) current-math-cell-end))
      (progn
	(math-check-current-cell t)
	(if (or (< pos current-math-cell-start)
		(> (+ pos len) current-math-cell-end))
	    (error "Deletion outside of cell contents is not allowed."))
	)))


(defun math-check-insertion (pos len)
  "Check that start of insertion is within a math-cell."
  (if (or (< pos current-math-cell-start)
	  (> pos current-math-cell-end))
      (progn
	(math-check-current-cell t)
	(if (or (< pos current-math-cell-start)
		(> pos current-math-cell-end))
	    (error "Insertion outside of a math-cell is not allowed."))
	)))

  
(defun math-check-current-cell (error-out)
  "Put internal marks at start and end of current math-cell. 
If ERROR-OUT non-nil and (closed and not group) or output
then give error."
  ;; it would be handy to have two kinds of mark.  The end mark
  ;; should be an insert-before mark!!
  (save-restriction
    (widen)				; just in case
    (let ((math-match-data (match-data)) ; save since we have to search
	  ;; find currently visible math-cell
	  next-math-cell
	  )

      (save-excursion
	(setq current-math-cell-index (current-math-cell))

	;; find the start
	(math-label-to-contents)
	(skip-chars-backward " ")
	(set-marker current-math-cell-start (point))

	;; find the end
	(if (setq next-math-cell (math-cell-next current-math-cell-index))
	    (progn
	      (goto-math-cell next-math-cell)
	      (forward-char -1)		; mark is on char 2
	      (set-marker current-math-cell-end (1- (point)))
	      )
	  ;; no next math-cell, so set to end of buffer
	  (set-marker current-math-cell-end (point-max))
	  (goto-char (point-max))
	  (let ((before-change-hook nil)) ; dont check recursively
	    ;; put extra char at end to speed up inserts
	    (insert "\n")
	    )))

      (store-match-data math-match-data)

      (if (and error-out
	       (< (point) current-math-cell-start))
	  (error "Don't edit cell labels.")

	(let ((output))
	  (if (or (setq output (math-cell-output-p current-math-cell-index))
		  (math-cell-closed-p current-math-cell-index))

	      (progn			; not supposed to modify
		(set-marker current-math-cell-end
			    (1- current-math-cell-start))
		(if error-out
		    (error "Can't modify %s cell.  %s it first."
			   (if output "output" "closed")
			   (if output "Unformat" "Open") ; give key commands.
			   )))
	    )))
      )))


(defun math-check-before-change (kind pos len)
  "Check that hidden text and math-cell label are not going to be modified."
  (cond
   ((= kind 0) (math-check-deletion pos len))
   ((= kind 1) (math-check-insertion pos len))
   ((= kind 2) (math-check-deletion pos len)) ; replacement is same
   (t
    (error (concat "Math-check-before-change: " kind " " pos " " len))
    (sit-for 2))			; this should never happen
   ))

(defun math-no-check (kind pos len)
  "Dont do anything."
  )




;;;------------------
;;; Substitutes for common deletion and insertion commands.
;;; Used if before-change-hook is not available

(defun math-delete-char (arg &optional killflag)
  "delete-char for math-mode.  Check if deletion is ok first."
  (interactive "*p")
  ;;  (message "delete-char arg: %s" arg)
  (if (> 0 arg)
      (math-delete-backward-char (- arg) killflag)
    (or	(not before-change-hook)
	(not math-mode)
	(math-check-deletion (point) arg))
    (emacs-delete-char arg killflag))
  )


(defun math-delete-backward-char (arg &optional killflag)
  "delete-backward-char for math-mode.  Check if deletion is ok first."
  (interactive "*p")
  ;;  (message "delete-backward-char")
  (if (> 0 arg)
      (math-delete-char (- arg) killflag)
    (or (not before-change-hook)
	(not math-mode)
	(math-check-deletion (- (point) arg) arg))
    (emacs-delete-backward-char arg killflag))
  )


(defun math-delete-region (start end)
  "delete-region for math-mode.  Check if deletion is ok first."
  (interactive "*r")
  (or (not before-change-hook)
      (not math-mode)
      (math-check-deletion start (- end start)))
  (emacs-delete-region start end)
  )


(defun math-self-insert-command (arg)
  "Do self-insert-command after checking if it is OK."
  ;; this doesnt work as a substitute for self-insert-command
  ;; but if all keys that do self-insert-command are rebound, it works fine.
  (interactive "*p")
  (or (not before-change-hook)
;;;      (not math-mode)
      (math-check-insertion (point) 0))
  (emacs-self-insert-command arg))


(defun math-yank (arg)
  "Do yank after checking if it is OK."
  ;; could also check that text doesnt look like label!!
  (interactive "*p")
  (or (not before-change-hook)
      (not math-mode)
      (math-check-insertion (point) 0))
  (emacs-yank arg))


;;(defun change-hooks-available-p ()
;;  "Test whether the change hooks are available in this version
;;of emacs."
;;  ;; doesnt seem to work!!  dont use it
;;  (let* ((new-buf (set-buffer
;;		   (get-buffer-create "test-hooks")))
;;	 before-change-hook)
;;    (setq before-change-hook t)
;;    (prog1
;;	(assq 'before-buffer-hook
;;	      (buffer-local-variables))
;;      (kill-buffer new-buf)
;;      )))

;; only do this once.  save standard defs so they may be referred to.
(if (not (fboundp 'emacs-delete-char))
    (progn
      (fset 'emacs-delete-char (symbol-function 'delete-char))
      (fset 'emacs-delete-backward-char (symbol-function 'delete-backward-char))
      (fset 'emacs-delete-region (symbol-function 'delete-region))
      (fset 'emacs-self-insert-command (symbol-function 'self-insert-command))
      (fset 'emacs-yank (symbol-function 'yank))
      ))

(defconst change-hooks-available nil
  "If non-nil, before-change-hook and after-change-hook are implemented.
These are not part of standard Emacs.")

(defun math-setup-change-hooks ()
  "Substitute math- commands for the common emacs insertion and delection commands"
  (if change-hooks-available
      (setq before-change-hook 'math-check-before-change)
    (setq before-change-hook 'math-no-check)
    (make-variable-buffer-local 'before-change-hook)
    (fset 'delete-char 'math-delete-char)
    (fset 'delete-backward-char 'math-delete-backward-char)
    (fset 'delete-region 'math-delete-region)
;;;    (fset 'self-insert-command 'math-self-insert-command)  ; dont do this
    (fset 'yank (symbol-function 'math-yank))
    ))


(if nil;; for debugging only
    (progn
      (fset 'delete-char (symbol-function 'emacs-delete-char))
      (fset 'delete-backward-char (symbol-function 'emacs-delete-backward-char))
      (fset 'delete-region (symbol-function 'emacs-delete-region))
;;;      (fset 'self-insert-command (symbol-function 'emacs-self-insert-command))
      (fset 'yank (symbol-function 'emacs-yank))
      ))




;;-------------------
;; Indentation
;; Modified from lisp-mode indentation code


(defun math-newline-and-indent ()
  "Do newline-and-indent if in active math-cell,
otherwise redo command without math-newline-and-indent."
  (interactive)
  (if (or (< (point) current-math-cell-start)
	  (> (point) current-math-cell-end))
      (math-check-current-cell nil))
  (if (eq 'input (math-cell-font current-math-cell-index))
      (newline-and-indent)

    ;; redo key without math-newline-and-indent
    (let ((indent-line-function 'indent-to-left-margin) ; from text mode
	  (local-keymap (current-local-map)) ; math-mode-map
	  )
      (use-local-map text-mode-map)
      (unwind-protect
	  (progn
	    ;; (setq current-prefix-arg arg)
	    (command-execute (this-command-keys)))
	(use-local-map local-keymap))
      )))



(defun math-maybe-indent-line (&optional arg)
  "Indent current line as Math code, if in an active math-cell.
Otherwise redo command without math-maybe-indent-line.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive "P")
  (if (or (< (point) current-math-cell-start)
	  (> (point) current-math-cell-end))
      (math-check-current-cell nil))
  (if (eq 'input (math-cell-font current-math-cell-index))
      (math-indent-line arg)

    ;; redo key without math-maybe-indent-line
    (let ((indent-line-function 'indent-to-left-margin) ; from text mode
	  (local-keymap (current-local-map)) ; math-mode-map
	  )
      (use-local-map text-mode-map)
      (unwind-protect
	  (progn
	    (setq current-prefix-arg arg)
	    (command-execute (this-command-keys)))
	(use-local-map local-keymap))
      )))



(defconst math-indent-offset nil "")
(defconst math-indent-hook 'math-indent-hook "")

;; should use restriction around current cell to limit scope!!

(defun math-indent-line (&optional whole-exp)
  "Indent current line as Math code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (let ((indent (calculate-math-indent)) shift-amt beg end
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (looking-at "(\\*\\*")
	(progn
	  (indent-for-comment)
	  (forward-char -1))
      (if (listp indent)
	  (setq indent (car indent)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
	  nil
	(delete-region beg (point))
	(indent-to indent))
      )

    ;; If initial point was within line's indentation,
    ;; position after the indentation.  Else stay at same point in text.
    (if (> (- (point-max) pos) (point))
	(goto-char (- (point-max) pos)))
    ;; If desired, shift remaining lines of expression the same amount.
    (and whole-exp (not (zerop shift-amt))
	 (save-excursion
	   (goto-char beg)
	   (forward-sexp 1)
	   (setq end (point))
	   (goto-char beg)
	   (forward-line 1)
	   (setq beg (point))
	   (> end beg))
	 (indent-code-rigidly beg end shift-amt)
	 )))

(defun calculate-math-indent (&optional parse-start)
  "Return appropriate indentation for current line as Math code.
In usual case returns an integer: the column to indent to.
Can instead return a list, whose car is the column to indent to.
This means that following lines at the same level of indentation
should not necessarily be indented the same way.
The second element of the list is the buffer position
of the start of the containing expression."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state paren-depth
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          last-sexp containing-sexp
	  start-of-expr			; point where current expression starts
	  expr-char			; the char that is before this expression
	  )
      (if parse-start
          (goto-char parse-start)
	(if (and (boundp 'math-mode)
		 math-mode)
	    (math-move-to-cell (current-math-cell))
	  ;; math-package
	  (beginning-of-line)
	  (if (not (bobp))
	      (forward-char -1))
	  (re-search-backward "^[ \t]*$\\|\\`")
	  ))
      (setq containing-sexp (1- (point)))

      ;; Find outermost containing sexp
      (while (< (point) indent-point)
        (setq state (parse-partial-sexp (point) indent-point 0)))

      ;; Find innermost containing sexp () [] or {}.
      (while (and retry
		  state
                  (> (setq paren-depth (elt state 0)) 0))
        (setq retry nil)
        (setq last-sexp (elt state 2))
        (setq containing-sexp (or (elt state 1) containing-sexp))

        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))

        ;; Is there a complete sexp since then?
        (if (and last-sexp (> last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp last-sexp indent-point 0)))
              (if (setq retry (car (cdr peek)))
		  (setq state peek)))))

      (if (not desired-indent)
	  (progn
	    ;; find innermost comma or semi seperated expression
	    (goto-char indent-point)
	    (math-back-to-previous-expression (1+ containing-sexp))
	    (setq expr-char (preceding-char)) ; remember which kind of expr
	    (save-excursion
	      (math-skip-whitespace)
	      (setq start-of-expr (point)))
            
	    ;; Innermost containing sexp found
	    (if (save-excursion
		  (goto-char start-of-expr)
		  (forward-line 1)
		  (<= (point) indent-point))
		;; Unfinished expression
		;; Indent beneath start of current expression + some
		(progn
		  (goto-char start-of-expr)
		  (setq desired-indent (+ (current-column) math-body-indent)))

	      (if (eq expr-char ?\;)
		  (progn
		    ;; Indent beneath first expression in semi list + some
		    (forward-char -1)
		    (math-back-to-previous-expression
		     (1+ containing-sexp) '(?\,))
		    (math-skip-whitespace)
		    (setq desired-indent (+ (current-column) math-body-indent))
		    )
		(goto-char (1+ containing-sexp)))
	      )))

      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overriden by math-indent-offset
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
	(cond ((elt state 3)
	       ;; Inside a string, don't change indentation.
	       (goto-char indent-point)
	       (skip-chars-forward " \t")
	       (current-column))

	      ((and (integerp math-indent-offset) containing-sexp)
	       ;; Indent by constant offset
	       (goto-char containing-sexp)
	       (+ normal-indent math-indent-offset))

	      (desired-indent)

	      ((and (boundp 'math-indent-hook)
		    math-indent-hook
		    (not retry))
	       (or (funcall math-indent-hook containing-sexp indent-point)
		   normal-indent))
	      (t
	       normal-indent)))
      )))


(defun math-indent-hook (containing-sexp indent-point)
  "Check whether special indenting is required.
Assume point is after opening paren of containing expression."
  (if (eq (preceding-char) ?\[)
      (let ((normal-indent (current-column))
	    (starting-point (point)))
        
	(let* ((function-name
		(progn
		  (if (not (bobp))
		      (forward-char -1))
		  (if (= ?\[ (preceding-char)) ; looking at \[\[
		      (list)
		    (math-skip-whitespace-backwards)
		    (if (and (not (bobp))
			     (string-match "\\s_"
					   (char-to-string (preceding-char))))
			(buffer-substring (point)
					  (progn 		  
					    (forward-sexp -1) 
					    (point)) ; leave point here 
					  )))))
	       (start-of-header (progn (point) (current-column)))
	       (method (and function-name
			    (get (intern-soft function-name) 
				 'math-indent-hook))))

	  (cond ((integerp method)
		 (goto-char (1+ containing-sexp))
		 (math-indent-specform method
				       indent-point
				       (+ start-of-header math-body-indent)))
		;; should be a function - not used
		(method
		 (funcall method indent-point))

		(t
		 ;; test if function-name is an unknown header
		 ;; or an expression
		 ;; First, does it have any funny chars in it?
		 (if (or (null function-name)
			 (string-match "[#%_$\(\[\{]" function-name))
		     normal-indent
		   ;; starting at beginning of symbol before []
		   ;; check if there is a preceeding :: or ?
		   (math-skip-whitespace-backwards)
		   (forward-char -1)
		   (if (or (= (following-char) ?\?)
			   (and (= (following-char) ?\:)
				(= (preceding-char) ?\:)))
		       normal-indent
		     (+ start-of-header math-body-indent)
		     ))
		 )))
	)))


(defconst math-body-indent 2
  "How much to indent body of math expressions.")

(defun math-indent-specform (count indent-point normal-indent)
  "Return the column to indent to given the number, COUNT, of the
first distinguished expression, the INDENT-POINT of the line to indent,
and the NORMAL-INDENT column."
  (let ((containing-form-start (point))
        (i count)
        body-indent 
	containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count).
    ;; math-indent-hook guarantees that there is at
    ;; least one expression following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    ;;    (setq containing-form-column (current-column))
    (setq body-indent (+ math-body-indent normal-indent))

    ;; Now count the number of expressions up to count
    (while (and (< (point) indent-point)
		(<= 0 (setq count (1- count))))
      (math-forward-to-next-expression indent-point '(?\,))
      (skip-chars-forward ",")
      (math-skip-whitespace)
      )
      
    ;; Point is sitting on first character of last (or count) expression.
    (if (> count 0)
        ;; A distinguished form.  If it is before the method'th form use double
        ;; math-body-indent, else just normal-indent. 
	(list (+ normal-indent math-body-indent)
	      (+ normal-indent math-body-indent))

      ;; A non-distinguished form.  Use body-indent if there are no
      ;; distinguished forms and this is the first undistinguished form,
      ;; or if this is the first undistinguished form and the preceding
      ;; distinguished form has indentation at least as great as body-indent.
      (if (or (and (= i 0) (= count 0))
              (and (= count 0) (<= body-indent normal-indent)))
          normal-indent
	normal-indent			; was normal-indent
	))))

(put 'Block 'math-indent-hook 1)
(put 'For 'math-indent-hook 3)
(put 'While 'math-indent-hook 1)
(put 'Do 'math-indent-hook 1)
(put 'Next 'math-indent-hook 2)
(put 'FixedPoint 'math-indent-hook 1)
(put 'If 'math-indent-hook 2)
(put 'Switch 'math-indent-hook 1)
(put 'Which 'math-indent-hook 0)
(put 'Check 'math-indent-hook 1)


(defun math-indent-exp ()
  "Indent each line of the expression starting just after point."
  (interactive)
  (if (or (< (point) current-math-cell-start)
	  (> (point) current-math-cell-end))
      (math-check-current-cell nil))
  (if (not (eq 'input (math-cell-font current-math-cell-index)))
      (undefined))
  (let* ((start-of-exp (point))
	 (end-of-exp (save-excursion
		       (math-forward-expression 1)
		       (skip-chars-backward " \t\n\r")
		       (point-marker))))
    (save-excursion
      (forward-line)
      (while (< (point) end-of-exp)
	(math-indent-line)
	(forward-line)
	))
    ))



(defun math-back-to-previous-expression (limit-point &optional term-chars)
  "Move point back to previous expression at the same level.
Dont move past LIMIT-POINT, or past the start of a list.
If optional TERM-CHARS is non-nil, it is a list of characters that
may terminate the search.  If nil, use ',' and ';'.
Leave point after the term char."
  (or term-chars (setq term-chars '(?\; ?\,)))
  (while (and (> (point) limit-point)
	      (not (or (memq (preceding-char) term-chars)
		       (memq (preceding-char) '(?\( ?\[ ?\{)))))
    (if (memq (preceding-char) '(?\) ?\] ?\} ?\"))
	(forward-sexp -1)
      (forward-char -1)			; skip single char
      (if (looking-at "\\s_")
	  (progn
	    (forward-char 1)
	    (forward-sexp -1)		; skip the whole symbol
	    ))
      )
    (skip-chars-backward " \t\n\r")
    )
  (if (> limit-point (point))
      (goto-char limit-point)		; too far	
    ;; else found something
    ))


(defun math-forward-to-next-expression (limit-point &optional term-chars)
  "Move point forward to next expression at the same level.
Dont move past LIMIT-POINT, or past the start of a list.
If optional TERM-CHARS is non-nil, it is a list of characters that
may terminate the search.  If nil, use ',' and ';'.
Leave point after the term char."
  (or term-chars (setq term-chars '(?\; ?\,)))
  (while (and (< (point) limit-point)
	      (not (or (memq (following-char) term-chars)
		       (memq (following-char) '(?\) ?\] ?\})))))
    (if (memq (following-char) '(?\( ?\[ ?\{ ?\"))
	(forward-sexp 1)
      (if (looking-at "\\s_")
	  (forward-sexp 1)		; skip the whole symbol
	(forward-char 1)
	)
      )
    (skip-chars-forward " \t\n\r")
    )
  (if (< limit-point (point))
      (goto-char limit-point)		; too far	
    ;; else found something
    ))



(defun math-skip-whitespace ()
  "Skip forward skipping comments and whitespace."
  (skip-chars-forward " \t\n\r")
  (while (looking-at "(\\*")		; skip leading comments
    (forward-sexp 1)
    (skip-chars-forward " \t\n\r")
    ))

(defun math-skip-whitespace-backwards ()
  "Skip backwards skipping comments and whitespace."
  (skip-chars-backward " \t\n\r")
  (while (save-excursion 
	   (forward-char -2)
	   (looking-at "(\\*")		; skip leading comments
	   )
    (backward-sexp 1)
    (skip-chars-backward " \t\n\r")
    ))


(defun math-forward-expression (arg)
  "Move forward across one balanced math expression.
With argumment, do this that many times."
  (interactive "p")
  (if (or (< (point) current-math-cell-start)
	  (> (point) current-math-cell-end))
      (math-check-current-cell nil))
  (or (and arg (> arg 0))
      (setq arg 1))
  (while (> arg 0)
    (setq arg (1- arg))
    (if (memq (following-char) '(?\, ?\;))
	(forward-char 1))
    (math-forward-to-next-expression current-math-cell-end)
    )
  )


(defun math-backward-expression (arg)
  "Move backward across one balanced math expression.
With argumment, do this that many times."
  (interactive "p")
  (if (or (< (point) current-math-cell-start)
	  (> (point) current-math-cell-end))
      (math-check-current-cell nil))
  (or (and arg (> arg 0))
      (setq arg 1))
  (math-back-to-previous-expression 0)
  (while (> arg 0)
    (setq arg (1- arg))
    (if (memq (preceding-char) '(?\, ?\;))
	(forward-char -1))
    (math-back-to-previous-expression current-math-cell-start)
    )
  (math-skip-whitespace)
  )

;; This is used by indent-for-comment
;; to decide how much to indent a comment in math code
;; based on its context.
(defun math-comment-indent ()
  (if (looking-at "(\\*\\*")
      0				;Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column))	;Else indent at comment column
	   comment-column))))	; except leave at least one space.



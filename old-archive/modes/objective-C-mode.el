;; objective-C-mode.el
;; -------------------
;;
;; LCD Archive Entry:
;; objective-C-mode|Kenneth Persson|benneth@eb.se|
;; Major mode for editing Objective-C programs.|
;; 91-11-28|3.02|~/modes/objective-C-mode.el.Z|
;;
;; Major mode for editing Objective-C programs.
;; The Objective-C-mode is an extension of the
;; default C-mode. Unfortunately there are many
;; hardcoded parts for EB SIGNAL use only. However
;; it can be changed.  (And has: changed for RDR
;; by Chris Walters.
;;
;; Author: Kenneth Persson (kenneth@eb.se)
;;         EB Signal AB, Stockholm, Sweden
;;
;; Modified by:
;;         Douglas Worthington,
;;         dougw@grebyn.com
;;
;; This is version 3.02.
;;
;; Put this in your .emacs file if its not in site-init.el:
;;
;; (autoload 'objective-C-mode "yourLispCodeDirectory/objective-C-mode"
;;           "Objective-C mode" t nil)
;; (setq auto-mode-alist
;;       (append '(("\\.h$" . objective-C-mode)
;; 		   ("\\.m$" . objective-C-mode))
;; 	      auto-mode-alist))

(provide 'objective-C-mode)

(defun objective-C-mode-version ()
  "3.03a")

(defun echo-objective-C-mode-version ()
  (interactive)
  (message (concat "Version "
		   (objective-C-mode-version)
		   " of "
		   mode-name
		   " mode.")))

(defvar objective-C-interface-file-dir nil
  "The directory where to put generated interface files")

(defvar objective-C-document-file-dir nil
  "The directory where to put generated document files")

(defvar objective-C-mode-map nil
  "*Keymap used in objective-C mode.")
(if objective-C-mode-map
    nil
  (let ((map (make-sparse-keymap))) 
    (define-key map "\C-cc"     'objective-C-mfile-header)
    (define-key map "\C-ch"     'objective-C-hfile-header)   
    (define-key map "\C-cp"     'objective-C-protocol)
    (define-key map "\C-cf"     'objective-C-factory-method)
    (define-key map "\C-cm"     'objective-C-instance-method)
    (define-key map "\C-cl"     'objective-C-codelimit)
    (define-key map "\C-c\C-m"  'objective-C-method-comment)
    (define-key map "\C-c\C-cd" 'generate-Objective-C-documentation)
    (define-key map "\C-c\C-ch" 'generate-Objective-C-hfile)   
    (define-key map "\C-c?"     'describe-objective-C-mode)   
    (define-key map "\C-ci"     'indent-region)
    (define-key map "\C-ca"     'add-user-sign)
    (define-key map "\C-cv"     'echo-objective-C-mode-version)
    (define-key map "{"         'electric-objective-C-brace)
    (define-key map "}"         'electric-objective-C-brace)
    (define-key map ":"         'electric-objective-C-keyword-match)
    (define-key map "\177"      'backward-delete-char-untabify)
    (define-key map "\t"        'objective-C-indent-command)
    (setq objective-C-mode-map map)))

(define-abbrev-table 'objective-C-mode-abbrev-table ())
(defvar objective-C-mode-abbrev-table nil
  "abbrevations to use in objective-c")

;;      (append objective-C-mode-abbrev-table
;;	      (list '("ryes" "return YES"      nil)
;;		    '("ry"   "return YES"      nil)
;;		    '("rno"  "return NO"       nil)
;;		    '("rn"   "return NO"       nil)
;;		    '("rs"   "return self"     nil)
;;		    '("rnil" "return nil"      nil)
;;		    '("imp"  "@implementation" nil)
;;		    '("intf" "@interface"      nil)
;;		    '("slef" "self"            nil)
;;		    '("st"   "STR"             nil)
;;		    '("bo"   "BOOL"            nil))))

(defvar objective-C-mode-syntax-table nil
  "Syntax table in use in Objective-C-Mode buffers.")
(if objective-C-mode-syntax-table
    nil
  (setq objective-C-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" objective-C-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14" objective-C-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" objective-C-mode-syntax-table)
  (modify-syntax-entry ?+ "." objective-C-mode-syntax-table)
  (modify-syntax-entry ?- "." objective-C-mode-syntax-table)
  (modify-syntax-entry ?= "." objective-C-mode-syntax-table)
  (modify-syntax-entry ?% "." objective-C-mode-syntax-table)
  (modify-syntax-entry ?< "." objective-C-mode-syntax-table)
  (modify-syntax-entry ?> "." objective-C-mode-syntax-table)
  (modify-syntax-entry ?& "." objective-C-mode-syntax-table)
  (modify-syntax-entry ?| "." objective-C-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" objective-C-mode-syntax-table))

(defconst objective-C-indent-level 4
  "*Indentation of C statements with respect to containing block.")
(defconst objective-C-brace-imaginary-offset 0
  "*Imagined indentation of a C open brace that actually follows a statement.")
(defconst objective-C-brace-offset -4
  "*Extra indentation for braces, compared with other text in same context.")
(defconst objective-C-argdecl-indent 5
  "*Indentation level of declarations of C function arguments.")
(defconst objective-C-label-offset -4
  "*Offset of C label lines and case statements relative to usual indentation.")
(defconst objective-C-continued-statement-offset 4
  "*Extra indent for lines not starting new statements.")
(defconst objective-C-auto-newline nil
  "*Non-nil means automatically newline before and after braces,
and after colons and semicolons, inserted in C code.")
(defconst objective-C-tab-always-indent t
  "*Non-nil means TAB in C mode should always reindent the current line,
regardless of where in the line point is when the TAB command is used.")

(defun objective-C-mode ()
  "  A major mode for the Objective-C language.
Commands:
  Expression and list commands understand all Objective-C brackets.
  Tab anywhere on a line indents it according to Objective-C
  conventions. LF does a CR and then an indentation like above.
  Comments are delimited with /* ... */ or // to cr.
  Paragraphs are separated by blank lines only.
  Delete converts tabs to spaces as it moves back.
\\{objective-C-mode-map}

Variables controlling indentation style:
  objective-C-indent-level
    Indentation of C statements within surrounding block.
    The surrounding block's indentation is the indentation
    of the line on which the open-brace appears.

Variables controlling directories of generated files:
    objective-C-interface-file-dir
      If non nil generated interface files will be put into
      this directory. The default is current directory.
    objective-C-document-file-dir
      If non nil generated dokument files will be put into
      this directory. The default is current directory.

Skeletons of the major Objective-C constructs are inserted with:
  C-c c class header    C-c m instanceMethod     C-c f factoryMethod
  C-c p protocol        C-c l limiter            C-c h header in h file
  C-c <RET> method comment

Other useful stuff
  C-c ? Describe objective-C mode
  C-c a Add signature to use in comments
  C-c v Return the version of this mode
  C-c C-c h Generate an interface file from the implementation file
  C-c C-c d Generate an documentation file from the implementation file

Abbreviations:
  ry   & ryes  = return YES
  rn   & rno   = return NO
  rs   & rself = return self
  rnil         = return nil
  imp          = @implementation
  intf         = @interface
  st           = STR
  bo           = BOOL

Turning on Objective-C mode calls the value of the variable
objective-C-mode-hook with abbrevs, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map objective-C-mode-map)
  (setq major-mode 'objective-C-mode)
  (setq mode-name "Objective-C")
  (setq local-abbrev-table objective-C-mode-abbrev-table)
  (set-syntax-table objective-C-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'objective-C-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'signatures)
  (setq signatures nil)
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'objective-C-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (setq objective-C-line-length 70)
  (make-local-variable 'super-filename)
  (setq super-filename nil)
  (run-hooks 'objective-C-mode-hook))

;; ------------------------ EB Stuff ---------------------------

;;--------------- site-init.el --------------- START
(defun eb-day (aString)
  (let ((a
	 (car (cdr (assoc aString '((" 1"  "01")
				    (" 2"  "02")
				    (" 3"  "03")
				    (" 4"  "04")
				    (" 5"  "05")
				    (" 6"  "06")
				    (" 7"  "07")
				    (" 8"  "08")
				    (" 9"  "09")))))))
    (if (null a) aString a)))

(defun eb-month (aString)
  (let ((a
	 (car (cdr (assoc aString '(("JAN"  "01")
				    ("FEB"  "02")
				    ("MAR"  "03")
				    ("APR"  "04")
				    ("MAY"  "05")
				    ("JUN"  "06")
				    ("JUL"  "07")
				    ("AUG"  "08")
				    ("SEP"  "09")
				    ("OCT"  "10")
				    ("NOV"  "11")
				    ("DEC"  "12")
				    ))))))
    (if (null a) aString a)))

(defun eb-date ()
  "Return the current date in an EB Signal standard form"
  (concat (substring (current-time-string) -4 nil)
	  "-"
	  (eb-month (substring (current-time-string) 4 7))
	  "-"
	  (eb-day (substring (current-time-string) 8 10))))
;;--------------- site-init.el --------------- END

;; ------------------ File and Class name ------------------------

;; return the type ".m" or ".h" kan be improved
(defun file-type (&optional name)
  (substring (if (null name) (m-filename) name) -2 nil))

(defun m-filename ()
  (file-name-nondirectory (file-name-sans-versions buffer-file-name)))

(defun is-m-file (&optional name)
  (string-equal ".m" (file-type name)))

(defun h-filename ()
  (concat (substring (m-filename) 0 -2) ".h"))

(defun h-file-dir ()
  (if (null objective-C-interface-file-dir)
      ""
    objective-C-interface-file-dir))

(defun is-h-file (&optional name)
  (string-equal ".h" (file-type name)))

(defun doc-filename ()
  (concat (substring (m-filename) 0 -2) ".txt"))

(defun doc-dir ()
  (if (null objective-C-document-file-dir)
      ""
    objective-C-document-file-dir))

(defun class-from-filename (&optional name)
  (substring (if (null name) (m-filename) name)
	     0 (string-match "\\.\\([0-9]*\\.\\)?\\(m\\|h\\|txt\\)"
			     (if (null name) (m-filename) name))))

(defun number-from-filename (&optional name)
  (let ((m-file (if (null name) (m-filename) name)))
    (substring m-file
	       (string-match "\\([0-9]*\\)?\\.\\(m\\|h\\)" m-file)
	       (- (length m-file) 2))))

;; Tested and used. Not a very goodlooking
(defun super-class ()
  (let* ((row (concat "@implementation "
		      (class-from-filename)
		      "\\( \\|\t\\|\n\\)*"
		      ":"
		      "\\( \\|\t\\|\n\\)*"))
	 (row2 (concat row "[a-zA-Z][a-zA-Z0-9$_]*"))
	 (a-list (list (string-match row (buffer-string))
		       (match-end 0)))
	 (b-list (list (string-match row2 (buffer-string))
		       (match-end 0))))
    (cond ((null (car a-list)) nil)
	  ((null (car b-list)) nil)
	  (t (buffer-substring (1+ (cadr a-list))
			       (1+ (cadr b-list)))))))

(defun super-class-number ()
  (let ((sc (super-class))
	(no nil))
    (if (null sc) "NO_SUPER_CLASS_FOUND"
      (setq no (read-string (concat "Class number for superclass "
				    (super-class)
				    ": ")))
      (if (string-equal "" no) no
	(concat "." no)))))

(defun super-hfilename (&optional number)
  (let ((sc (super-class))
	(no (if (null number) (super-class-number) number)))
    (if (null sc) "NO_SUPER_CLASS_FOUND" (concat sc no ".h"))))

;; ---------------------- AUX ------------------------

(defun user-sign ()
  (car (cdr (assoc (user-login-name) signatures))))

(defun cadr (l)
  (car (cdr l)))

(defun odd (e)
  "True if it's argument is odd."
  (eq (% e 2) 1))

(defun e-empty-line-p ()
  "True if current line is empty."
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \\t]*$")))

(defun insert-right (name diff &optional c)
  (let* ((u  (- objective-C-line-length (length name) diff))
	 (fillChar (if (char-or-string-p c) c (string-to-char " "))))
    (insert-char fillChar u)
    (insert name)))

(defun insert-centered (name diff &optional c)
  "Inserts the name centered in the Objective-C environment
diff is a number that tells how many positions that already
are used on the line (/*     */ = 4) and the optional c
character is used to fill white space with."
  (let* ((toshare  (- objective-C-line-length (length name) diff))
	 (u        (/ toshare 2))
	 (fillChar (if (char-or-string-p c) c (string-to-char " "))))
    (insert-char fillChar u)
    (insert name)
    (insert-char fillChar (if (odd toshare) (1+ u) u))))

(defun add-user-sign ()
  (interactive)
  (let* ((name (user-login-name))
	 (sign (read-string (concat "Signature for " name ": "))))
    (setq signatures (cons (list name sign) signatures))))

(defun classes-used ()
  "Extract the used classes from the file"
  (save-excursion
    (let ((aString ""))
      (beginning-of-buffer)
      (re-search-forward "Classes used")
      (forward-line 1)
      (while (e-empty-line-p)
	(forward-line 1))
      (beginning-of-line)
      (while (and
	      (not (eobp))
	      (not (e-empty-line-p))
	      (looking-at "#import"))
	(setq aString (concat aString (grep-class-from-line) " "))
	(forward-line 1)
	(beginning-of-line))
      aString)))

(defun beginning-of-line-point (&optional aBool)
  (save-excursion
    (beginning-of-line)
    (cond (aBool (skip-chars-forward " \t*/")))
    (point)))

(defun end-of-line-point (&optional aBool)
  (save-excursion
    (end-of-line)
    (cond (aBool (skip-chars-backward " \t*/")))
    (point)))

(defun grep-class-from-line ()
  (let ((bp (beginning-of-line-point))
	(ep (end-of-line-point)))
    (buffer-substring (+ bp 9) (- ep 3))))

(defun start-of-instance-point ()
  (save-excursion
    (beginning-of-buffer)
    (search-forward "@implementation")
    (forward-line 1)
    (while (or (e-empty-line-p)
	       (looking-at "{"))
      (forward-line 1)
      (beginning-of-line))
    (point)))

(defun end-of-instance-point ()
  (save-excursion
    (beginning-of-buffer)
    (search-forward "@implementation")
    (search-forward "}")
    (forward-line -1)
    (while (e-empty-line-p)
      (forward-line -1))
    (end-of-line)
    (point)))

(defun have-instance-variables ()
  (save-excursion
    (beginning-of-buffer)
    (search-forward "@implementation")
    (forward-line 1)
    (while (e-empty-line-p)      
      (forward-line 1))
    (skip-chars-forward " \t")
    (looking-at "{")))

(defun instance-variables ()
  (cond ((have-instance-variables)
	 (buffer-substring (start-of-instance-point)
			   (end-of-instance-point)))
	(t "\t // none")))

(defun start-of-intro-point ()
  (save-excursion
    (beginning-of-buffer)
    (cond ((null (search-forward "Introduction"
				 (start-of-instance-point)
				 t))
	   1)
	  (t (forward-line 1)
	     (while (e-empty-line-p)
	       (forward-line 1))
	     (beginning-of-line)
	     (point)))))

(defun end-of-intro-point ()
  (save-excursion
    (beginning-of-buffer)
    (cond ((null (search-forward "Revision History"
				 (start-of-instance-point)
				 t))
	   1)
	  (t (forward-line -1)
	     (while (e-empty-line-p)
	       (forward-line -1))
	     (end-of-line)
	     (point)))))

(defun class-intro ()
  (let ((si (start-of-intro-point))
	(ei (end-of-intro-point)))
    (if (or (= si ei) (= si 1) (= ei 1))
	"\tCan't extract the class description since the strings\n\tIntroduction and/or Revision History are missing in the source code."
      (buffer-substring si ei))))

(defun find-next-protocol-or-method ()
  (beginning-of-line)
  (while (and (not (eobp))
	      (not (looking-at "/\\*====="))
	      (not (looking-at "+[a-zA-Z( ]"))
	      (not (looking-at "-[a-zA-Z( ]")))
    (forward-line 1)
    (beginning-of-line))
  (cond ((looking-at "/\\*=====")
	 (forward-line 1)
	 (beginning-of-line)))
  (not (eobp)))

(defun line-is-method ()
  (save-excursion
    (beginning-of-line)
    (or (looking-at "+[a-zA-Z( ]")
	(looking-at "-[a-zA-Z( ]"))))

(defun line-is-protocol ()
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (looking-at "/\\*=====")))

(defun is-gps-comment ()
  (save-excursion
    (let ((ordinary (is-ordinary-comment)))
      (beginning-of-line)
      (and (looking-at "---------") ordinary))))

(defun is-ordinary-comment ()
  (save-excursion
    (and (char-equal ?* (char-after (- (point) 2)))
	 (char-equal ?/ (char-after (- (point) 1)))
	 (not (char-equal ?= (char-after (beginning-of-line-point)))))))

(defun is-date-entry ()
  (< 50 (- (beginning-of-line-point t) (beginning-of-line-point))))

(defun is-method-name-entry ()
  (and (char-equal ?| (char-after (beginning-of-line-point t)))
       (char-equal ?| (char-after (- (end-of-line-point t) 1)))))

(defun is-empty-line-info ()
  (string-equal "" (buffer-substring (beginning-of-line-point t)
				     (end-of-line-point))))

(defun is-not-crap-line ()
    (and (not (is-date-entry))
	 (not (is-method-name-entry))
	 (not (is-empty-line-info))))

(defun look-for-comment-at-previous-line ()
  (save-excursion
    (forward-line -1)
    (beginning-of-line)
    (skip-chars-forward "\t ")
    (and (char-equal ?/ (char-after (point)))
	 (char-equal ?* (char-after (+ (point) 1))))))

(defun is-still-comment ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "\t ")
    (if (and (char-equal ?/ (char-after (point)))
	     (char-equal ?* (char-after (+ (point) 1))))
	(look-for-comment-at-previous-line)
      t)))

(defun is-objc-comment ()
  (save-excursion
    (beginning-of-line)
    (looking-at "//")))

(defun gps-comment ()
  (ordinary-comment))

(defun ordinary-comment ()
  (let ((aStr ""))
    (forward-line -1)
    (while (is-still-comment)
      (beginning-of-line)
      (skip-chars-forward " \t*/")
      (cond ((is-not-crap-line)
	     (setq aStr
		   (concat (buffer-substring (point) (end-of-line-point t))
			   "\n"
			   aStr))))
      (forward-line -1))
    aStr))

(defun objc-comment ()
  (let ((aStr ""))
    (while (is-objc-comment)
      (beginning-of-line)
      (skip-chars-forward "/ ")
      (cond ((is-not-crap-line)
	     (setq aStr
		   (concat (buffer-substring (point) (end-of-line-point t))
			   "\n"
			   aStr))))
      (forward-line -1))
    aStr))

(defun no-comment ()
  "This method has not been documented in the source code.")

(defun line-as-doc-file-delimiter ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (setq start (point))
    (end-of-line)
    (skip-chars-backward " \t")
    (concat "\n" (buffer-substring start (point)) "\n")))

(defun line-as-inteface-file-delimiter ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (setq start (point))
    (end-of-line)
    (skip-chars-backward " \t")
    (setq name     (buffer-substring start (point)))
    (setq toshare  (- objective-C-line-length (length name) 8))
    (concat "\n/* "
	    (make-string (/ toshare 2) ?-)
	    " "
	    (buffer-substring start (point))
	    " "
	    (make-string (if (odd toshare)
			     (+ (/ toshare 2) 1)
			   (/ toshare 2))
			 ?-)
	    " */")))

(defun skip-comment (str)
  (let ((p (string-match "//" str)))
    (cond ((null p) str)
	  (t (substring str 0 p)))))

(defun line-as-inteface-file-method ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (setq start (point))
    (re-search-forward "{")
    (backward-char 1)
    (skip-chars-backward " \t\n")
    (concat (skip-comment (buffer-substring start (point))) ";")))

(defun extract-method-comment ()
  (forward-line -1)
  (while (e-empty-line-p)
    (forward-line -1))
  (end-of-line)
  (skip-chars-backward " \t")
  (cond ((is-gps-comment)      (gps-comment))
	((is-ordinary-comment) (ordinary-comment))
	((is-objc-comment)     (objc-comment))
	(t                     (no-comment))))
 
(defun method-description (method-name start-point)
  (save-excursion
    (goto-char start-point)
    (concat method-name "\n" (extract-method-comment))))

(defun line-as-doc-file-method ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (setq start (point))
    (re-search-forward "{")
    (backward-char 1)
    (skip-chars-backward " \t\n")
    (method-description
     (concat "|" (buffer-substring start (point)) "|") start)))

(defun class-as-short ()
  (save-excursion
    (let ((aString ""))
      (beginning-of-buffer)
      (re-search-forward "@implementation")
      (while (and (not (eobp)) (find-next-protocol-or-method))
	(setq aString
	      (concat
	       aString
	       (cond ((line-is-protocol) (line-as-doc-file-delimiter))
		     ((line-is-method)   (line-as-doc-file-method))
		     (t ""))
	       "\n"))
	(forward-line))
      aString)))

(defun class-as-short-no-comments ()
  (save-excursion
    (let ((aString ""))
      (beginning-of-buffer)
      (re-search-forward "@implementation")
      (while (and (not (eobp)) (find-next-protocol-or-method))
	(setq aString
	      (concat
	       aString
	       (cond ((line-is-protocol) (line-as-inteface-file-delimiter))
		     ((line-is-method)   (line-as-inteface-file-method))
		     (t ""))
	       "\n"))
	(forward-line))
      aString)))

;; ------------------ Objective-C mode -------------------

(defun describe-objective-C-mode ()
  (interactive)
  (describe-mode))

(defun objective-C-mfile-header ()
  "Build a class skeleton prompting for class name."
  (interactive)
  (if (not (is-m-file))
      (message "This is not an implementation file (.m)!")
    (let* ((file  (m-filename))
	   (class (class-from-filename))
	   (nr    (number-from-filename))
	   (cname (read-string "Class: " class))
	   (super (read-string "Super: ")))
      (if (not (e-empty-line-p))
	  (progn (end-of-line)(newline)))
      (indent-to 0)				
      (insert "/* ")
      (insert-char ?- (- objective-C-line-length 3))
      (insert "\n\n")
      (insert "\tRDR, Inc. \n\n")
      (insert "\tObjective-C source file for the class " cname "\n\n")
      (insert "\tCOPYRIGHT (C), " (substring  (current-time-string) -4 nil)
	      ", RDR, Inc.\n")
      (insert "\tALL RIGHTS RESERVED.\n\n")
      (insert "\tResponsible:\t\t\tApproved:\n")
      (insert "\tRDR:" (user-full-name) "\t\t\n\n")
      (insert "\tDate:\t\t\t\tRev:\n")
      (insert "\t" (eb-date)    "\t\t\t___\n\n")
;      (insert "\tReference:\t\t\tDocument no:\n")
;      (insert "\tLXA 108 104\t\t\t1/190 55 - CAL 124 1"
;	      (if (string-equal "" nr) "***" nr) "\n\n\n\n\n")
      (insert-centered cname 0)
      (insert "\n\n")
      (insert "\t1. Introduction")
      (insert "\n\n\n")
      (insert "\t2. Revision History")
      (insert "\n\n")
      (insert "\t___\tThe starting point.\t\t\t"
	      (user-full-name) "/" (eb-date) "\n\n\n")
      (insert "\t3. Source Code")
      (insert "\n\n")
      (insert-char ?- (- objective-C-line-length 3))
      (insert " */\n\n")
      (comment-line "Import files")
      (insert "#import <appkit/appkit.h>\n\n")
      (comment-line " Classes used ")
      (insert "#import \"foo.h\"\n\n")
      (comment-line " Externals ")
      (insert "\n\n")
      (comment-line " Defines ")
      (insert "\n\n")
      (comment-line " Class variables ")
      (insert "\n\n")
      (comment-box (concat "Implementation of class " cname))
      (insert "#import \"" (concat (substring file 0 -1) "h") "\"\n\n")
      (insert "@implementation " cname " : " super"\n{\n\n}\n\n")
      (comment-box "Instance Creation")
      (insert "\n\n")
      (comment-box "Initialize")
      (insert "\n\n")
      (comment-box "Free")
      (insert "\n\n")
      (insert "@end\n"))
    (re-search-backward "@implementation")
    (next-line 2)
    (objective-C-indent-line)))

(defun objective-C-hfile-header (&optional classname super-filename)
  "Gives an interface file sceleton."
  (interactive)
  (if (not (is-h-file))
      (message "This is not an interface file (.h)!")
    (let* ((class (if (null classname) (class-from-filename) classname))
	   (cname (read-string "Class: " class))
	   (nr    (number-from-filename))
	   (super (if (null super-filename)
		      (concat (read-string "Super: ") ".h")
		    super-filename)))
      (insert "/* ")
      (insert-char ?- (- objective-C-line-length 3))
      (insert "\n\n")
      (insert "\tRDR, Inc. \n\n")
      (insert "\tObjective-C interface file for the class " cname "\n\n")
      (insert "\tCOPYRIGHT (C), " (substring  (current-time-string) -4 nil)
	      ", RDR, Inc.\n")
      (insert "\tALL RIGHTS RESERVED.\n\n")
      (insert "\tResponsible:\t\t\tApproved:\n")
      (insert "\tRDR:" (user-full-name) "\t\t\n\n")
      (insert "\tDate:\t\t\t\tRev:\n")
      (insert "\t" (eb-date)    "\t\t\t___\n\n")
      (insert "\tReference:\t\t\tDocument no:\n")
;      (insert "\tLXA 108 104\t\t\t2/190 55 - CAL 124 1"
;	      (if (string-equal "" nr) "***" nr) "\n\n")
;      (insert-char ?- (- objective-C-line-length 3))
      (insert " */\n\n")
      (comment-box (concat "Interface of class " cname))
      (insert "#import \"" super "\"\n\n")
      (insert "@interface " cname " : " (class-from-filename super)
	      "\n{\n\n}\n\n")
      (insert "@end\n"))
    (re-search-backward "@interface")
    (next-line 2)
    (objective-C-indent-line)))

(defun generate-Objective-C-hfile ()
  "Generate the interface file for the current implementation file."
  (interactive)
  (let* ((file-name (read-string "Interface file name: "
				 (concat (h-file-dir) (h-filename))))
	 (buf       (create-file-buffer file-name))
	 (class     (class-from-filename))
	 (no        (number-from-filename))
	 (instance  (instance-variables))
	 (short     (class-as-short-no-comments))
	 (super     (super-hfilename)))
    (set-buffer buf)
    (set-visited-file-name file-name)
    (insert "/* ")
    (insert-char ?- (- objective-C-line-length 3))
    (insert "\n\n")
    (insert "\tRDR, Inc. \n\n")
    (insert "\tObjective-C interface file for the class " class "\n\n")
    (insert "\tCOPYRIGHT (C), " (substring  (current-time-string) -4 nil)
	    ", RDR, Inc.\n")
    (insert "\tALL RIGHTS RESERVED.\n\n")
    (insert "\tResponsible:\t\t\tApproved:\n")
    (insert "\tRDR:" (user-full-name) "\t\t\n\n")
    (insert "\tDate:\t\t\t\tRev:\n")
    (insert "\t" (eb-date)    "\t\t\t___\n\n")
    (insert "\tReference:\t\t\tDocument no:\n")
;    (insert "\tLXA 108 104\t\t\t2/190 55 - CAL 124 1"
;	    (if (string-equal "" no) "***" no) "\n\n")
;    (insert-char ?- (- objective-C-line-length 3))
    (insert " */\n\n")
    (comment-box (concat "Interface of class " class))
    (insert "#import \"" super "\"\n\n")
    (insert "@interface " class " : " (class-from-filename super) "\n{\n")
    (insert instance)
    (insert "\n}\n")
    (insert short)
    (insert "\n@end\n")
    (beginning-of-buffer)
    (display-buffer buf)))

(defun generate-Objective-C-documentation ()
  "Open a new buffer and write the documentation there."
  (interactive)
  (let* ((file-name (read-string "Documentation file name: "
				 (concat (doc-dir) (doc-filename))))
	 (buf       (create-file-buffer file-name))
	 (used      (classes-used))
	 (class     (class-from-filename))
	 (super     (super-class))
	 (no        (number-from-filename))
	 (instance  (instance-variables))
	 (short     (class-as-short))
	 (intro     (class-intro)))
    (set-buffer buf)
    (set-visited-file-name file-name)
    (indent-to 0)				
    (insert-char ?- objective-C-line-length)
    (insert "\n\n")
    (insert "\tRDR, Inc. \n\n")
    (insert "\tObjective-C documentation file for the class "
	    class "\n")
    (insert "\tCOPYRIGHT (C), " (substring  (current-time-string) -4 nil)
	    ", RDR, Inc.\n\n")
    (insert "\tResponsible:\t\t\tApproved:\n")
    (insert "\tRDR:" (user-full-name) "\t\t\n\n")
    (insert "\tDate:\t\t\t\tRev:\n")
    (insert "\t" (eb-date)    "\t\t\t___\n\n")
    (insert "\tReference:\t\t\tDocument no:\n")
;    (insert "\tLXA 108 104\t\t\t1551 - CAL 124 1"
;            (if (string-equal "" no)
;		"***"
;	      no) "\n\n")
;    (insert-char ?- objective-C-line-length)
    (insert "\n\n\n")
    (insert-centered class 0)
    (insert "\n\n\n\tInherits from:\t\t" super "\n\n")
    (insert "\tClasses used:\t\t" used "\n\n")
    (insert "\tInterface file:\t\t" class "." no ".h" "\n\n")
    (insert "\tImplementation file:\t" class "." no ".m" "\n\n\n")
    (insert-centered "Introduction" 0)
    (insert "\n\n")
    (insert intro)
    (insert "\n\n")
    (insert-centered "Instance Variables" 0)
    (insert "\n\n")
    (insert instance)
    (insert "\n\n")
    (insert-centered "Methods" 0)
    (insert "\n")
    (insert short)   
    (insert "\nEnd\n")
    (beginning-of-buffer)
    (display-buffer buf)))

(defun comment-box (name)
  "Print name in an comment-box"
  (if (not (e-empty-line-p))
      (progn (end-of-line)(newline)))
  (insert "/*")
  (insert-centered "" 2 ?=)
  (insert "\n")
  (insert-centered name 0 (string-to-char " "))
  (insert "\n")
  (insert-centered "" 2 ?=)
  (insert "*/\n"))

(defun comment-line (name)
  "Print name in an comment-line"
  (if (not (e-empty-line-p))
      (progn (end-of-line)(newline)))
  (insert "/* ")
  (insert-centered (concat " " name " ") 6 ?-)
  (insert " */\n"))

(defun objective-C-protocol ()
  "Build a protocol skeleton prompting for protocol name."
  (interactive)
  (comment-box (read-string "Protocol name: ")))

(defun objective-C-codelimit ()
  "Print name on a centered line."
  (interactive)
  (comment-line (read-string "Centered text: ")))

(defun objective-C-method (prefix name)
  (indent-to 0)				
  (insert "/*")
  (insert-centered "" 2 ?-)
  (insert "\n")
  (insert "|" prefix name "| \n" (if (string-equal "-" prefix)
				     "Return self.\n"
				   "\n"))
  (insert-right (concat (user-full-name) "/" (eb-date)) 0)
  (insert "\n")
  (insert-centered "" 2 ?-)
  (insert "*/\n")
  (insert prefix name "\n")
  (insert "{\n")
  (indent-to objective-C-indent-level)
  (if (string-equal "+" prefix)
      (insert "self = [super new];\n")
    (insert "\n"))
  (indent-to objective-C-indent-level)
  (insert "return self;\n")
  (insert "}\n\n")
  (next-line -4))

(defun objective-C-method-comment ()
  "Insert a comment according to the style used with methods in GPS"
  (interactive)
  (indent-to 0)				
  (insert "/*")
  (insert-centered "" 2 ?-)
  (insert "\n\n\n")
  (insert-right (concat (user-full-name) "/" (eb-date)) 0)
  (insert "\n")
  (insert-centered "" 2 ?-)
  (insert "*/\n"))

(defun objective-C-instance-method ()
  "Build a routine skeleton prompting for method name."
  (interactive)
  (if (not (e-empty-line-p))
      (progn (end-of-line)(newline)))
  (objective-C-method "-" (read-string "Instance method name: ")))

(defun objective-C-factory-method ()
  "Build a routine skeleton prompting for method name."
  (interactive)
  (if (not (e-empty-line-p))
      (progn (end-of-line)(newline)))
  (objective-C-method "+" (read-string "Factory method name: ")))

;; ------------------ C- mode fix ---------------------

;; This is used by indent-for-comment
;; to decide how much to indent a comment in C code
;; based on its context.
(defun objective-C-comment-indent ()
  (if (looking-at "^/\\*")
      0				;Existing comment at bol stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column))	;Else indent at comment column
	   comment-column))))	; except leave at least one space.

;; implement this function later
(defun electric-objective-C-keyword-match ()
  "Match two following lines with keyword arguments"
  (interactive)
  (insert ":"))

(defun electric-objective-C-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (if (and (not arg)
	     (eolp)
	     (or (save-excursion
		   (skip-chars-backward " \t")
		   (bolp))
		 (if objective-C-auto-newline
		     (progn (objective-C-indent-line) (newline) t) nil)))
	(progn
	  (insert last-command-char)
	  (objective-C-indent-line)
	  (if objective-C-auto-newline
	      (progn
		(setq insertpos (1- (point)))
		(newline)
		(objective-C-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun electric-objective-C-semi (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (if objective-C-auto-newline
      (electric-objective-C-terminator arg)
    (self-insert-command (prefix-numeric-value arg))))

(defun electric-objective-C-terminator (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos (end (point)))
    (if (and (not arg) (eolp)
	     (not (save-excursion
		    (beginning-of-line)
		    (skip-chars-forward " \t")
		    (or (= (following-char) ?#)
			;; Colon is special only after a label, or case ....
			;; So quickly rule out most other uses of colon
			;; and do no indentation for them.
			(and (eq last-command-char ?:)
			     (not (looking-at "case"))
			     (save-excursion
			       (forward-word 2)
			       (<= (point) end)))
			(progn
			  (beginning-of-defun)
			  (let ((pps (parse-partial-sexp (point) end)))
			    (or (nth 3 pps) (nth 4 pps) (nth 5 pps))))))))
	(progn
	  (insert last-command-char)
	  (objective-C-indent-line)
	  (and objective-C-auto-newline
	       (not (objective-C-inside-parens-p))
	       (progn
		 (setq insertpos (1- (point)))
		 (newline)
		 (objective-C-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun objective-C-inside-parens-p ()
  (condition-case ()
      (save-excursion
	(save-restriction
	  (narrow-to-region (point)
			    (progn (beginning-of-defun) (point)))
	  (goto-char (point-max))
	  (= (char-after (or (scan-lists (point) -1 1) (point-min))) ?\()))
    (error nil)))

(defun objective-C-indent-command (&optional whole-exp)
  (interactive "P")
  "Indent current line as C code, or in some cases insert a tab character.
If objective-C-tab-always-indent is non-nil (the default), always indent current line.
Otherwise, indent the current line only if point is at the left margin
or in the line's indentation; otherwise insert a tab.

A numeric argument, regardless of its value,
means indent rigidly all the lines of the expression starting after point
so that this line becomes properly indented.
The relative indentation among the lines of the expression are preserved."
  (if whole-exp
      ;; If arg, always indent this line as C
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (objective-C-indent-line))
	    beg end)
	(save-excursion
	  (if objective-C-tab-always-indent
	      (beginning-of-line))
	  (setq beg (point))
	  (forward-sexp 1)
	  (setq end (point))
	  (goto-char beg)
	  (forward-line 1)
	  (setq beg (point)))
	(if (> end beg)
	    (indent-code-rigidly beg end shift-amt "#")))
    (if (and (not objective-C-tab-always-indent)
	     (save-excursion
	       (skip-chars-backward " \t")
	       (not (bolp))))
	(insert-tab)
      (objective-C-indent-line))))

(defun objective-C-indent-line ()
  "Indent current line as C code.
Return the amount the indentation changed by."
  (let ((indent (calculate-objective-C-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  ((eq indent t)
	   (setq indent (calculate-objective-C-indent-within-comment)))
	  ((looking-at "[ \t]*#")
	   (setq indent 0))
	  (t
	   (skip-chars-forward " \t")
	   (if (listp indent) (setq indent (car indent)))
	   (cond ((or (looking-at "case\\b")
		      (and (looking-at "[A-Za-z]")
			   (save-excursion
			     (forward-sexp 1)
			     (looking-at ":"))))
		  (setq indent (max 1 (+ indent objective-C-label-offset))))
		 ((and (looking-at "else\\b")
		       (not (looking-at "else\\s_")))
		  (setq indent (save-excursion
				 (objective-C-backward-to-start-of-if)
				 (current-indentation))))
		 ((= (following-char) ?})
		  (setq indent (- indent objective-C-indent-level)))
		 ((= (following-char) ?{)
		  (setq indent (+ indent objective-C-brace-offset))))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun calculate-objective-C-indent (&optional parse-start)
  "Return appropriate indentation for current line as C code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state
	  containing-sexp)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))
      (while (< (point) indent-point)
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
	(setq containing-sexp (car (cdr state))))
      (cond ((or (nth 3 state) (nth 4 state))
	     ;; return nil or t if should not change this line
	     (nth 4 state))
	    ((null containing-sexp)
	     ;; Line is at top level.  May be data or function definition,
	     ;; or may be function argument declaration.
	     ;; Indent like the previous top level line
	     ;; unless that ends in a closeparen without semicolon,
	     ;; in which case this line is the first argument decl.
	     (goto-char indent-point)
	     (skip-chars-forward " \t")
	     (if (= (following-char) ?{)
		 0   ; Unless it starts a function body
	       (objective-C-backward-to-noncomment (or parse-start (point-min)))
	       ;; Look at previous line that's at column 0
	       ;; to determine whether we are in top-level decls
	       ;; or function's arg decls.  Set basic-indent accordinglu.
	       (let ((basic-indent
		      (save-excursion
			(re-search-backward "^[^ \^L\t\n#]" nil 'move)
			(if (and (looking-at "\\sw\\|\\s_")
				 (looking-at ".*(")
				 (progn
				   (goto-char (1- (match-end 0)))
				   (forward-sexp 1)
				   (and (< (point) indent-point)
					(not (memq (following-char)
						   '(?\, ?\;))))))
			    objective-C-argdecl-indent 0))))
		
  		 ;; Now add a little if this is a continuation line.
  		 (+ basic-indent
		    (if	(or (bobp)
			    (memq (preceding-char) '(?\) ?\; ?\})))
			0 0)))))
  	    ((/= (char-after containing-sexp) ?{)
  	     ;; line is expression, not statement:
  	     ;; indent to just after the surrounding open.
	    
	     (goto-char (1+ containing-sexp))
	     (current-column))
	    (t
	     ;; Statement level.  Is it a continuation or a new statement?
	     ;; Find previous non-comment character.
	     (goto-char indent-point)
	     (objective-C-backward-to-noncomment containing-sexp)
	     ;; Back up over label lines, since they don't
	     ;; affect whether our line is a continuation.
	     (while (or (eq (preceding-char) ?\,)
			(and (eq (preceding-char) ?:)
			     (or (eq (char-after (- (point) 2)) ?\')
				 (memq (char-syntax (char-after (- (point) 2)))
				       '(?w ?_)))))
	       (if (eq (preceding-char) ?\,)
		   (objective-C-backward-to-start-of-continued-exp containing-sexp))
	       (beginning-of-line)
	       (objective-C-backward-to-noncomment containing-sexp))
	     ;; Now we get the answer.
	     (if (not (memq (preceding-char) '(nil ?\, ?\; ?\} ?\{)))
		 ;; This line is continuation of preceding line's statement;
		 ;; indent  objective-C-continued-statement-offset  more than the
		 ;; previous line of the statement.
		 (progn
		   (objective-C-backward-to-start-of-continued-exp containing-sexp)
		   (+ objective-C-continued-statement-offset (current-column)))
	       ;; This line starts a new statement.
	       ;; Position following last unclosed open.
	       (goto-char containing-sexp)
	       ;; Is line first statement after an open-brace?
	       (or
		;; If no, find that first statement and indent like it.
		(save-excursion
		  (forward-char 1)
		  (let ((colon-line-end 0))
		    (while (progn (skip-chars-forward " \t\n")
				  (looking-at "#\\|/\\*\\|case[ \t\n].*:\\|[a-zA-Z0-9_$]*:"))
		      ;; Skip over comments and labels following openbrace.
		      (cond ((= (following-char) ?\#)
			     (forward-line 1))
			    ((= (following-char) ?\/)
			     (forward-char 2)
			     (search-forward "*/" nil 'move))
			    ;; case or label:
			    (t
			     (save-excursion (end-of-line)
					     (setq colon-line-end (point)))
			     (search-forward ":"))))
		    ;; The first following code counts
		    ;; if it is before the line we want to indent.
		    (and (< (point) indent-point)
			 (if (> colon-line-end (point))
			     (- (current-indentation) objective-C-label-offset)
			   (current-column)))))
		;; If no previous statement,
		;; indent it relative to line brace is on.
		;; For open brace in column zero, don't let statement
		;; start there too.  If objective-C-indent-offset is zero,
		;; use objective-C-brace-offset + objective-C-continued-statement-offset instead.
		;; For open-braces not the first thing in a line,
		;; add in objective-C-brace-imaginary-offset.
		(+ (if (and (bolp) (zerop objective-C-indent-level))
		       (+ objective-C-brace-offset objective-C-continued-statement-offset)
		     objective-C-indent-level)
		   ;; Move back over whitespace before the openbrace.
		   ;; If openbrace is not first nonwhite thing on the line,
		   ;; add the objective-C-brace-imaginary-offset.
		   (progn (skip-chars-backward " \t")
			  (if (bolp) 0 objective-C-brace-imaginary-offset))
		   ;; If the openbrace is preceded by a parenthesized exp,
		   ;; move to the beginning of that;
		   ;; possibly a different line
		   (progn
		     (if (eq (preceding-char) ?\))
			 (forward-sexp -1))
		     ;; Get initial indentation of the line we are on.
		     (current-indentation))))))))))

(defun calculate-objective-C-indent-within-comment ()
  "Return the indentation amount for line, assuming that
the current line is to be regarded as part of a block comment."
  (let (end star-start)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (setq star-start (= (following-char) ?\*))
      (skip-chars-backward " \t\n")
      (setq end (point))
      (beginning-of-line)
      (skip-chars-forward " \t")
      (and (re-search-forward "/\\*[ \t]*" end t)
	   star-start
	   (goto-char (1+ (match-beginning 0))))
      (current-column))))


(defun objective-C-backward-to-noncomment (lim)
  (let (opoint stop)
    (while (not stop)
      (skip-chars-backward " \t\n\f" lim)
      (setq opoint (point))
      (if (and (>= (point) (+ 2 lim))
	       (save-excursion
		 (forward-char -2)
		 (looking-at "\\*/")))
	  (search-backward "/*" lim 'move)
	(beginning-of-line)
	(skip-chars-forward " \t")
	(if (looking-at "#")
	    (setq stop (<= (point) lim))
	  (setq stop t)
	  (goto-char opoint))))))  

(defun objective-C-backward-to-start-of-continued-exp (lim)
  (if (= (preceding-char) ?\))
      (forward-sexp -1))
  (beginning-of-line)
  (if (<= (point) lim)
      (goto-char (1+ lim)))
  (skip-chars-forward " \t"))

(defun objective-C-backward-to-start-of-if (&optional limit)
  "Move to the start of the last ``unbalanced'' if."
  (or limit (setq limit (save-excursion (beginning-of-defun) (point))))
  (let ((if-level 1)
	(case-fold-search nil))
    (while (not (zerop if-level))
      (backward-sexp 1)
      (cond ((looking-at "else\\b")
	     (setq if-level (1+ if-level)))
	    ((looking-at "if\\b")
	     (setq if-level (1- if-level)))
	    ((< (point) limit)
	     (setq if-level 0)
	     (goto-char limit))))))

(defun mark-objective-C-function ()
  "Put mark at end of C function, point at beginning."
  (interactive)
  (push-mark (point))
  (end-of-defun)
  (push-mark (point))
  (beginning-of-defun)
  (backward-paragraph))



/* End of text from cm-next-9:next */

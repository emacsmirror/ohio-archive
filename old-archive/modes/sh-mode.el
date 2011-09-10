; Path: hal.com!decwrl!sdd.hp.com!usc!cs.utexas.edu!uunet!ogicse!das-news.harvard.edu!cantaloupe.srv.cs.cmu.edu!crabapple.srv.cs.cmu.edu!andrew.cmu.edu!ts49+
; From: strong+@CMU.EDU (Thomas W. Strong, Jr.)
; Newsgroups: gnu.emacs.sources
; Subject: sh-mode.el
; Date: 15 Nov 92 18:28:42 GMT
; Article-I.D.: andrew.Qf1mBui00VpdM8mzYs
; Organization: Pittsburgh Supercomputing Center, Carnegie Mellon, Pittsburgh, PA
; 
; Here's a beginning of sh-mode.el.  Any comments or suggestions for
; improvements would be appreciated
; 
; -------------- cut here -------------------
;;; sh-mode.el
;;
;;; LCD Archive Entry:
;;; sh-mode|Thomas W. Strong, Jr.|strong+@cmu.edu|
;;; Beginning of an sh mode.|
;;; 92-11-15||~/modes/sh-mode.el.Z|
;;; 
;;; hacked out of forth.el from the tile-forth package
;;;
;;; comments/complaints/flames to strong+@cmu.edu
;;;

(defvar sh-mode-positives
  " for case while else elif if \) ) "
  "Contains all words which will cause the indent-level to be incremented
on the next line.
OBS! All words in sh-mode-positives must be surrounded by spaces.")

(defvar sh-mode-negatives
  " fi else elif done esac \;\; ;; "
  "Contains all words which will cause the indent-level to be decremented
on the current line.
OBS! All words in sh-mode-negatives must be surrounded by spaces.")

(defvar sh-mode-zeroes
  " #!/bin/sh "
  "Contains all words which causes the indent to go to zero")

(defvar sh-mode-abbrev-table nil
  "Abbrev table in use in sh-mode buffers.")

(define-abbrev-table 'sh-mode-abbrev-table ())

(defvar sh-mode-map nil
  "Keymap used in sh mode.")

(if (not sh-mode-map)
    (setq sh-mode-map (make-sparse-keymap)))

(define-key sh-mode-map "\t" 'sh-indent-command)
(define-key sh-mode-map "\C-m" 'reindent-then-newline-and-indent)

(defvar sh-mode-syntax-table nil
  "Syntax table in use in sh-mode buffers.")

(defvar sh-indent-level 2
  "*Indentation of sh statements.")

(defun sh-mode-variables ()
  (setq local-abbrev-table sh-mode-abbrev-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'sh-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t))
  
(defun sh-mode ()
  "Major mode for editing sh scripts.
\\[sh-indent-command] properly indents subexpressions of multi-line
if, while, foreach, and switch statements, taking nesting into account.
Caveats:
The file must start with '#!/bin/sh' for the indentation to start properly.
Extra spaces should be inserted to make sure the indentation algorithm can
figure out what is a keyword, string, etc.  For example, write
    if ($foo == \"bar\")
not
    if($foo==\"bar\")
or later lines may get indented wrong.  (Many lines like this are also
illegal sh code, so this shouldn't cramp your style.)

The variable sh-indent-level controls the amount of indentation.
\\{sh-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map sh-mode-map)
  (setq mode-name "sh")
  (setq major-mode 'sh-mode)
  (sh-mode-variables)
  (run-hooks 'sh-mode-hook))

(defun sh-current-indentation ()
  (save-excursion
    (beginning-of-line)
    (back-to-indentation)
    (current-column)))

(defun sh-delete-indentation ()
  (let
      ((b nil)
       (m nil))
    (save-excursion
      (beginning-of-line)
      (setq b (point))
      (back-to-indentation)
      (setq m (point)))
    (delete-region b m)))

(defun sh-indent-line (&optional flag)
  "Correct indentation of the current sh line."
  (let
      ((x (sh-calculate-indent)))
    (sh-indent-to x)))
  
(defun sh-indent-command ()
  (interactive)
  (sh-indent-line t))

(defun sh-indent-to (x)
  (let
      ((p nil))
    (setq p (- (current-column) (sh-current-indentation)))
    (sh-delete-indentation)
    (beginning-of-line)
    (indent-to x)
    (if (> p 0) (forward-char p))))

;;Calculate indent
(defun sh-calculate-indent ()
  (let ((w1 nil)
	(indent 0)
	(centre 0))
    (save-excursion
      (beginning-of-line)
      (skip-chars-backward " \t\n")
      (beginning-of-line)
      (back-to-indentation)
      (setq indent (current-column))
      (setq centre indent)
      (setq indent (+ indent (sh-sum-line-indentation))))
    (save-excursion
      (beginning-of-line)
      (back-to-indentation)
      (let ((p (point)))
	(skip-chars-forward "^ \t\n")
	(setq w1 (buffer-substring p (point)))))
    (if (> (- indent centre) sh-indent-level)
	(setq indent (+ centre sh-indent-level)))
    (if (> (- centre indent) sh-indent-level)
	(setq indent (- centre sh-indent-level)))
    (if (< indent 0) (setq indent 0))
    (setq indent (- indent
		    (if (string-match 
			 (regexp-quote (concat " " w1 " "))
			 sh-mode-negatives)
			sh-indent-level 0)))
    (if (string-match (regexp-quote (concat " " w1 " ")) sh-mode-zeroes)
	(setq indent 0))
    indent))

(defun sh-sum-line-indentation ()
  "Add up the positive and negative weights of all words on the current line."
  (let ((b (point))
	(e nil)
	(sum 0)
	(w nil)
	(t1 nil)
	(t2 nil)
	(first t))
    (end-of-line) (setq e (point))
    (goto-char b)
    (while (< (point) e)
      (setq w (sh-next-word))
      (setq t1 (string-match (regexp-quote (concat " " w " "))
			     sh-mode-positives))
      (setq t2 (string-match (regexp-quote (concat " " w " "))
			     sh-mode-negatives))
      (if (and t1 t2)
	  (setq sum (+ sum sh-indent-level)))
      (if t1
	  (setq sum (+ sum sh-indent-level)))
      (if (and t2 (not first))
	  (setq sum (- sum sh-indent-level)))
      (skip-chars-forward " \t")
      (setq first nil))
    sum))

(defun sh-next-word ()
  "Return the next sh-word. Skip anything enclosed in double quotes."
  (let ((w1 nil))
    (while (not w1)
      (skip-chars-forward " \t\n")
      (if (not (looking-at "\""))
	  (let ((p (point)))
	    (skip-chars-forward "^ \t\n")
	    (setq w1 (buffer-substring p (point))))
	(skip-chars-forward "^\"")
	(forward-char 1)))
    w1))

-------------- cut here -------------------


 -----------------------------------------------------------------
  Tom Strong              N3NBB              ts49+@andrew.cmu.edu

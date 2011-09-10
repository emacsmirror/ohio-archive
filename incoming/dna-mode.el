;; dna-mode.el --- a major mode for editing dna sequences
;;
;; ~/lib/emacs/jhg-lisp/dna-mode.el ---
;;
;; $Id: dna-mode.el,v 1.28 1999/07/11 06:16:14 harley Exp $
;;
;; Author:  harley@bcm.tmc.edu
;; URL:     http://www.hgsc.bcm.tmc.edu/~harley/elisp/dna-mode.el
;;

;;; Commentary:
;; --------------------
;; A collection of functions for editing DNA sequences.  It
;; provides functions to make editing easier.
;;
;; Dna-mode will:
;;  * Fontify keywords and line numbers in sequences, but not bases.
;;  * Incrementally search dna over pads and numbers
;;  * Complement and reverse complement a region.
;;  * Move over bases and entire sequences.
;;  * Detect sequence files by content.

;;; Installation:
;; --------------------
;; Here are two suggested ways for installing this package.
;; You can choose to autoload it when needed, or load it
;; each time emacs is started.  Put one of the following
;; sections in your .emacs:
;;
;; ---Autoload:
;;  (autoload 'dna-mode "dna-mode" "Major mode for dna" t)
;;  (add-to-list
;;     'auto-mode-alist
;;     '("\\.\\(fasta\\|fa\\|exp\\|ace\\|gb\\)\\'" . dna-mode))
;;  (add-hook 'dna-mode-hook 'turn-on-font-lock)
;;
;; ---Load:
;;  (setq dna-do-setup-on-load t)
;;  (load "/pathname/dna-mode")
;;
;;; 
;;
;; The dna-isearch-forward function (and isearch in general)
;; is much more useful with something like the following:
;;  (make-face 'isearch)
;;  (set-face-background 'isearch "yellow")
;;  (setq-default search-highlight t)


;;; Code:

;; Setup
(defvar dna-mode-hook nil
  "*Hook to setup `dna-mode'.")

(defvar dna-mode-load-hook nil
  "*Hook to run when `dna-mode' is loaded.")

(defvar dna-setup-on-load nil
  "*If not nil setup dna mode on load by running `dna-`add-hook's'.")


;; Bases
(defvar dna-valid-base-regexp
  "[-*:acgtmrwsykvhdbxnACGTMRWSYKVHDBXN]"
  "*A regexp which matches a single base.")

(defvar dna-base-complement-list
  '(
    (?-.?-) (?n.?n) (?*.?*) (?x.?x) (?:.?:)         ; identity
    (?a.?t) (?c.?g) (?g.?c) (?t.?a)                 ; single
    (?m.?k) (?r.?y) (?w.?w) (?s.?s) (?y.?r) (?k.?m) ; double
    (?v.?b) (?h.?d) (?d.?h) (?b.?v)                 ; triple
    )
  "*List of bases and their complements.
Bases should be lowercase, as they are upcased when the vector is made."
  )

(defvar dna-base-complement-vector
  (let (
	(c-vec (make-vector 255 nil))
	(c-list dna-base-complement-list)
	)
    (while c-list
      (aset c-vec (car (car c-list)) (cdr (car c-list)))
      (aset c-vec (upcase (car (car c-list))) (upcase (cdr (car c-list))))
      (setq c-list (cdr c-list)))
    c-vec)
  "A vector of upper and lower case bases and their complements.")


;; Dna-isearch
(defvar dna-cruft-regexp "[* 0-9\t\n]"
  "*Regexp to match cruft which may appear between bases.
Skip over it during dna-motion and dna-isearch.")

(defvar dna-isearch-case-fold-search t
  "*Case fold dna-isearches.")


;; Sequence
(defvar dna-sequence-start-regexp
  "^\\(>\\|ID\\|LOCUS\\|DNA\\)"
  "A regexp which matches the start of a sequence.")


;; I also use "Alt" as C-c is too much to type for cursor motions.
(defvar dna-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Ctrl bindings
    (define-key map "\C-c\C-f"	'dna-forward-base)
    (define-key map "\C-c\C-b"	'dna-backward-base)
    (define-key map "\C-c\C-s"	'dna-isearch-forward)
    (define-key map "\C-cr"	'dna-reverse-complement-region)
    (define-key map "\C-cc"	'dna-complement-region)
    (define-key map "\C-cb"	'dna-complement)
    (define-key map "\C-c#"	'dna-count-bases-region)
    (define-key map "\M-\C-h"	'dna-mark-sequence)
    (define-key map "\M-\C-a"	'dna-beginning-of-sequence)
    (define-key map "\M-\C-e"	'dna-end-of-sequence)
    ;; XEmacs does not like the Alt bindings
    (if (string-match "XEmacs" (emacs-version)) t
      (define-key map [A-right]	'dna-forward-base)
      (define-key map [A-left]	'dna-backward-base)
      (define-key map [A-up]	'dna-beginning-of-sequence)
      (define-key map [A-down]	'dna-end-of-sequence)
      (define-key map [?\A-\C-s]	'dna-isearch-forward))
    map)
  "The local keymap for `dna-mode'.")

;;;###autoload
(defun dna-mode ()
  "Major mode for editing DNA sequences.

This mode also customizes isearch to search over line breaks.

\\{dna-mode-map}"
  (interactive)
  ;;
  (kill-all-local-variables)
  (setq mode-name "dna")
  (setq major-mode 'dna-mode)
  (use-local-map dna-mode-map)
  ;;
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(dna-font-lock-keywords))
  ;;
  (make-local-variable 'dna-valid-base-regexp)
  (make-local-variable 'dna-sequence-start-regexp)
  (make-local-variable 'dna-cruft-regexp)
  (make-local-variable 'dna-isearch-case-fold-search)
  ;;
  (run-hooks 'dna-mode-hook)
  )


;; Keywords
;; Todo: Seperate the keywords into a list for each format, rather
;; than one for all.
(defvar dna-font-lock-keywords
  '(
    ;; Fasta
    ("^\\(>\\)\\([-_.|a-zA-Z0-9]+\\)\\([ \t]+.*\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face)
     (3 font-lock-comment-face nil t))

    ;; Exp
    ("^\\(ID\\) +\\([-_.a-zA-Z_0-9]+\\)"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    ("^\\(CC\\|SQ\\)\\([ \t]\\(.*\\)\\)?$"
     (1 font-lock-keyword-face) (3 font-lock-comment-face nil t))
    ("^\\(\\sw\\sw\\)[ \t]"
     (1 font-lock-keyword-face))
    ("^\\(//\\)"
     (1 font-lock-keyword-face))

    ;; Ace (phrap output)
    ("^\\(DNA\\|Sequence\\|BaseQuality\\) +\\([-_.a-zA-Z_0-9]+\\)"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))

    ;; Genbank
    ("^\\(LOCUS\\) +\\([-_.a-zA-Z_0-9]+\\)" ;; are '-_.' allowed?
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    "ORIGIN"

    ; More genbank keywords...
    "ACCESSION" "AUTHORS" "AUTHORS" "BASE COUNT" "DEFINITION"
    "FEATURES" "JOURNAL" "JOURNAL" "KEYWORDS" "MEDLINE" "NID"
    "ORGANISM" "REFERENCE" "SEGMENT" "SOURCE" "TITLE"

    ;; line numbers...
    ("^[ \t]*\\([0-9]+\\)"
     (1 font-lock-string-face))

    ;; others...?
    )
  "Expressions to hilight in `dna-mode'.")


;;; Setup functions
(defun dna-find-file-func ()
  "Invoke `dna-mode' if the buffer look like a sequence.
and another mode is not active.
This function is added to `find-file-hooks'."
  (if (and (eq major-mode 'fundamental-mode)
	   (looking-at dna-sequence-start-regexp))
      (dna-mode)))

;;;###autoload
(defun dna-add-hooks ()
  "Add a default set of dna-hooks.
These hooks will activate `dna-mode' when visiting a file
which has a dna-like name (.fasta or .gb) or whose contents
looks like dna.  It will also turn enable fontification for `dna-mode'."
  (add-hook 'dna-mode-hook 'turn-on-font-lock)
  (add-hook 'find-file-hooks 'dna-find-file-func)
  (add-to-list
   'auto-mode-alist
   '("\\.\\(fasta\\|fa\\|exp\\|ace\\|gb\\)\\'" . dna-mode))
  )

;; Setup hooks on request when this mode is loaded.
(if dna-setup-on-load
    (dna-add-hooks))

(defun dna-next-char-func ()
  "Should never be called.  Overridden in dna-forward-base.")

;; Motion
(defun dna-forward-base (count)
  "Move forward COUNT bases.  Move backward if negative.
Skip over dna-isearch-cruft.  Stop on non-base or
non-whitespace characters."
  (interactive "p")
  (let ((c 0)
	(abscount (abs count))
	(dir (if (< count 0) -1 1))
	dna-next-char-func
	bstr
	)

    ;; 
    (fset 'dna-next-char-func (if (< dir 0) 'preceding-char 'following-char))

    ;;
    (while (< c abscount)
      (setq bstr (char-to-string (dna-next-char-func)))
      (cond
       ((string-match dna-valid-base-regexp bstr)
	(forward-char dir)
	(setq c (1+ c)))
       ((string-match dna-cruft-regexp bstr)
	(forward-char dir))
       (t
	(message "Moved %d bases forward." c)
	(setq abscount c)))) ; stop the while

    ;; Move over trailing junk when moving forward
    (if (= dir 1)
	(while (string-match dna-cruft-regexp
			     (char-to-string (dna-next-char-func)))
	  (forward-char dir))
      )
    ;; return the distance moved
    (* dir abscount)
    ))

;; aaaaaaaaaa cccccccccc | gggggggggg tttttttttt

(defun dna-backward-base (count)
  "Move backward COUNT bases.  See dna-forward-base."
  (interactive "p")
  (dna-forward-base (- count)))

(defun dna-beginning-of-sequence ()
  "Move the start of the sequence or the buffer."
  (interactive)
  (goto-char
   (or
    (search-backward-regexp dna-sequence-start-regexp (point-min) t)
    (point-min))))

(defun dna-end-of-sequence ()
  "Move to the end of the sequence or the buffer."
  (interactive)
  (end-of-line)
  (skip-syntax-forward "-")
  (let (
	(seqstart
	 (search-forward-regexp dna-sequence-start-regexp (point-max) t))
	)
    (if seqstart (progn
		   (goto-char seqstart)
		   (beginning-of-line))
      (goto-char (point-max)))))

(defun dna-mark-sequence ()
  "Put point at the beginning of a sequence, mark at end."
  (interactive)
  (dna-end-of-sequence)
  (set-mark (point))
  (dna-beginning-of-sequence)
  )

(defun dna-count-bases-region (d-start d-end)
   "Count the number of bases in the region D-START to D-END.
Echos the number of bases counted.
If an invalid base is found, stops on the base and signals an error."
  (interactive "r")
  (let ((basecount 0))
    (goto-char d-start)
    (while (< (point) d-end)
      (cond
       ((looking-at dna-valid-base-regexp)
	(setq basecount (1+ basecount))
	(forward-char 1))
       ((looking-at dna-cruft-regexp)
	(forward-char 1))
       (t
	(error "Bad base found.  '%s'"
	       (buffer-substring (point) (1+ (point)))))
       ))
    (message "There are %d bases in the region." basecount)
    basecount
    ))


;;; reverse and complement
(defun dna-complement-base-list (base)
  "Complement the BASE using a list based method.
Returns the complement of the base.
It can also be used to test if the character is a base,
as all bases should have a complement."
  (cdr (assq base dna-base-complement-list)))

(defun dna-complement-base (base)
  "Complement a BASE using a vector based method.
See `dna-complement-base-list' for more info."
  (aref dna-base-complement-vector base))

(defun dna-complement (base)
  "Look up the complement of the BASE and print a message.
Handy for us CS types."
  (interactive "cComplement of base:")
  (message "Complement of '%c' is '%c'." base (dna-complement-base base)))

(defun dna-complement-region (r-start r-end)
  "Complement a region of bases from R-START to R-END.
Complement a region of the buffer by deleting it and
inserting the complements, base by base.  Non-bases are
passed over unchanged."
  (interactive "r")
  (let (r-string r-length r-point r-base r-cbase)
    (goto-char r-start)
    (setq r-string (buffer-substring-no-properties r-start r-end))
    (setq r-length (length r-string))
    (delete-region r-start r-end)
    (setq r-point 0)
    (while (< r-point r-length)
      (setq r-base (aref r-string r-point))
      (setq r-cbase (dna-complement-base r-base))
      (insert (if r-cbase r-cbase r-base))
      (setq r-point (1+ r-point)))
    ))

;;;###autoload
(defun dna-reverse-complement-region (r-start r-end)
  "Reverse complement a region of dna from R-START to R-END.
Works by deleting the region and inserting bases reversed
and complemented, while entering non-bases in the order
found."
  (interactive "r")
  (let (r-string r-length r-base r-cbase r-point r-mark)
 
    (goto-char r-start)
    (setq r-string (buffer-substring-no-properties r-start r-end))
    (setq r-length (length r-string))
    (setq r-mark (1- r-length))
    (setq r-point 0)

    ;; goodbye
    (delete-region r-start r-end)

    ;; insert the bases from back to front base by base
    ;; insert non-bases from front to back to preserve spacing
    (while (< r-point r-length)
      (setq r-base (aref r-string r-point))
      (setq r-cbase (dna-complement-base r-base))
      (if r-cbase
	  (progn
	    ;; it is a base. find the reverse and complement it
	    (while (not (dna-complement-base (aref r-string r-mark)))
	      (setq r-mark (1- r-mark)))
	    (insert (dna-complement-base (aref r-string r-mark)))
	    (setq r-mark (1- r-mark))
	    )
	;; not a base, no change
	(insert r-base))
      (setq r-point (1+ r-point)))
    ))



;; format
(defun dna-guess-format-func ()
  "Guess the format of the sequence the point is at or after.
Returns the format or nil."
  (save-excursion
    (end-of-line)
    (dna-beginning-of-sequence)
    (cond
     ((looking-at "^>")   'fasta)
     ((looking-at "^DNA") 'phrap)
     ((looking-at "^ID")  'exp)
     (t nil)
     )))

(defun dna-guess-format ()
  "Guess and print the format of the sequence."
  (interactive)
  (message "%s" (dna-guess-format-func)))



;;; dna-isearch stuff
(defun dna-isearch-mangle-str (str)
  "Mangle the string STR into a regexp to search over cruft in sequence.
Inserts a regexp between each base which matches sequence formatting cruft.
For example, if `dna-cruft-regexp' is            '[ ]',
the search string 'acgt' would transformed into  'a[ ]*c[ ]*g[ ]*t[ ]*'"
  (let ((i 0) (out ""))
    (while (< i (length str))
      (setq out (concat out (substring str i (1+ i)) dna-cruft-regexp "*"))
      (setq i (1+ i)))
    out))

(defadvice isearch-message-prefix (around dna-isearch-ismp)
  "Set the isearch prompt string to show dna search is active.
This serves as a warning that the string is being mangled."
  ad-do-it
  (setq ad-return-value (concat "DNA " ad-return-value)))

(defadvice isearch-search (around dna-isearch-iss)
  "The advice used to mangle the search string in isearch."
  (let (
	(isearch-regexp t)
	;; force case folding
	(isearch-case-fold-search dna-isearch-case-fold-search)
	(isearch-string (dna-isearch-mangle-str isearch-string))
	)
    ad-do-it))

;;;###autoload
(defun dna-isearch-forward ()
  "Isearch forward on dna sequence.
Enable the `dna-mode' search string mangling advice and start variable `isearch-forward'."
  (interactive)

  ;; Enable the prompt
  (ad-enable-advice 'isearch-message-prefix 'around 'dna-isearch-ismp)
  (ad-activate 'isearch-message-prefix)
  ;; Enable the mangling
  (ad-enable-advice 'isearch-search 'around 'dna-isearch-iss)
  (ad-activate 'isearch-search)

  ;; run the search
  (isearch-forward)

  ;;
  (ad-disable-advice 'isearch-message-prefix 'around 'dna-isearch-ismp)
  (ad-activate 'isearch-message-prefix)
  ;; 
  (ad-disable-advice 'isearch-search 'around 'dna-isearch-iss)
  (ad-activate 'isearch-search)
 )


;;; Work with columns of sequences.

(defun dna-column-select-func ()
  "Return the start and end of the column as a cons.
Point is moved forward one."
  (let (s m e)
    (setq m (point))
    ;; work our way up
    (while (looking-at dna-valid-base-regexp)
      (setq s (point))
      (previous-line 1))
    (goto-char m)
    ;; work our way down
    (while (looking-at dna-valid-base-regexp)
      (setq e (point))
      (next-line 1))
    (goto-char m)
    ;; return the start and end of the column
    (cons s (1+ e))))

(defun dna-column-select ()
  "Select the current column of text.
Sets the mark at the top and the point at the bottom of a non-blank column."
  (interactive)
  (let ((se (dna-column-select-func)))
    (goto-char (car se))
    (push-mark)
    (goto-char (cdr se))))

(defvar dna-column-pad "*"
  "Character to use when inserting a column of pads.")

(defun dna-column-insert-pad ()
  "Insert a column of pads."
  (interactive)
    (save-excursion
      (let ((se (dna-column-select-func)))
	(string-rectangle (car se) (cdr se) dna-column-pad))))

(defun dna-column-delete ()
  "Delete the current column of dna."
  (interactive)
    (save-excursion
      (let ((se (dna-column-select-func)))
	(kill-rectangle (car se) (cdr se)))))

;;; Per base colors

(defvar dna-color-a "blue")
(defvar dna-color-c "black")
(defvar dna-color-g "green")
(defvar dna-color-t "red")

(defun dna-make-faces ()
  "Build a face for to display bases with."
  (let ((blist '("a" "c" "g" "t"))
	b-face)
    (while blist
      (setq b (car blist))
      (setq b-face (intern (concat "dna-face-" b)))
      (make-face b-face)
      (set-face-foreground
       b-face (symbol-value (intern (concat "dna-color-" b))))
      (setq blist (cdr blist))
      )))

;;(dna-make-faces)

(defun dna-color-bases (s e)
  "Color the bases in the region S to E."
  (interactive "r")
  (save-excursion
    (let (c)
      (goto-char s)
      (while (< s e)
	(setq c (downcase (char-after s)))
	(cond
	 ((eq c ?a)
	  (set-text-properties s (+ s 1) '(face dna-face-a)))
	 ((eq c ?c)		 (+ s 1)
	  (set-text-properties s (+ s 1) '(face dna-face-c)))
	 ((eq c ?g)		 (+ s 1)
	  (set-text-properties s (+ s 1) '(face dna-face-g)))
	 ((eq c ?t)		 (+ s 1)
	  (set-text-properties s (+ s 1) '(face dna-face-t)))
	 (t nil)
	 )
	(setq s (+ s 1))
	)
      )))
	 

(defun dna-uncolor-bases (s e)
  "Uncolor the bases from S to E."
  (interactive "r")
  (remove-text-properties s e '(face nil)))

;;; Functions for me.

;; I like to datestamp sequences I work with.
(defvar dna-timestamp-format "%Y%m%d"
  "Format of the time stamp which `dna-timestamp-seq' uses.")

(defun dna-timestamp-seq ()
  "Insert the current date into the sequence.
Assumes fasta format."
  (interactive)
  (end-of-line)
  (dna-beginning-of-sequence)
  (end-of-line)
  (insert "   " (format-time-string dna-timestamp-format (current-time))))

;; done loading
(run-hooks 'dna-mode-load-hook)
(provide 'dna-mode)

;;; dna-mode.el ends here

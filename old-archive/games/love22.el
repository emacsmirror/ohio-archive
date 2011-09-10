;;; -*- Mode:  Emacs-Lisp -*-

;;; LOVE22.EL
;;; Michael D. Ernst <mernst@theory.lcs.mit.edu>
;;; 10/2/90
;;; last modified 10/18/90

;; LCD Archive Entry:
;; love22|Michael D. Ernst|mernst@theory.lcs.mit.edu|
;; Support for writing and recognizing text in the style of Love 22.|
;; 90-10-18|1.0|~/games/love22.el.Z|


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description of love22.el
;;;

;; This file helps you write text in the style of Love 22, a peripatetic
;; jester and perennial presidential candidate (on a platform of "FOOD",
;; "CLOTH", and "RENTS") who looks exactly like Uncle Sam, and dresses the
;; part.  He notices words such as "LOVE22" whose letters add up to 22 on
;; the ABC chart.
;;
;;    The ABC Chart
;;   A B C D E F G H I
;;   J K L M N O P Q R
;;   S T U V W X Y Z
;;   1 2 3 4 5 6 7 8 9
;;
;; In conversations, he points them out in real time, whether he's talking
;; or listening, and in writing he emphasizes them with capitalization and
;; quotation marks, sometimes substituting homonyms to make the addition
;; work out.  This program lets you do the same, automatically.  The effect
;; can be quite humorous in text -- some people mistake it for
;; Zippification.  Try it on some text you have lying around to see the
;; result.  (I have my mail-before-send-hook do this automatically on
;; certain mail, depending on the recipient.)
;;
;; You can get $22 bills, a copy of Love 22's platform, and other fun stuff
;; by sending a self-addressed stamped envelope (with a small donation, if
;; you're feeling generous; I sent $2.22 last time) to
;;    Love 22
;;    PO Box 4022
;;    Key West, FL  33040

;; The commands of interest are:
;; 
;; show-abc-chart
;;    Displays the ABC chart in the *Help* buffer.
;; abc-chart-word
;;    Reports the ABC chart value of the word at point.
;;    In Love22 mode, bound to C-x =.
;; abc-chart-region
;;    Reports the ABC chart value of the current region.
;;    In Love22 mode, bound to M-=.
;; love22-buffer
;;    Attempts to find words whose ABC chart values are 22 (or its multiples).
;;    Will group words together, substitute homonyms, look for roots of words,
;;    and try various other tricks.  The function 22-hook is called on each
;;    such word (or group of words) found; its default action is to upcase the
;;    word(s), surround it by quotes, and change spaces to hyphens if
;;    substitution (e.g. "2" for "too") occurred.
;; love22-region
;;    Like love22-buffer; acts on the current region (between point and mark).
;; love22-mode
;;    A minor mode to support writing of Love22 text.  Love22 mode causes the
;;    (almost) continual display of the ABC chart value value of the current
;;    word, as well as emphasizing words whose value is a multiple of 22, if 
;;    love22-emphasize is non-nil.  (This mode's emphasis functionality is
;;    considerably less than that of love22-buffer, but it operates in real
;;    time.)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; How to use this file
;;;

;;; Save this file as love22.el somewhere on your load-path.

;; *****
;; Don't byte-compile this file; it only works interpreted.
;; *****

;;; Add to your .emacs file (without the leading semicolons):
;;;   (autoload 'show-abc-chart "love22"
;;;   	  "Show the ABC chart in the *Help* window." t)
;;;   (autoload 'abc-chart-word "love22"
;;;   	  "Compute and display the ABC chart value of the word at point." t)
;;;   (autoload 'abc-chart-region "love22"
;;;   	  "Compute and display the ABC chart value of the current region." t)
;;;   (autoload 'love22-buffer "love22"
;;;   	  "Process a buffer to look like Love22 wrote it." t)
;;;   (autoload 'love22-region "love22"
;;;   	  "Process a region to look like Love22 wrote it." t)
;;;   (autoload 'love22-mode "love22"
;;;   	  "Minor mode for writing Love22 text." t)
;;; If love22.el is not on your load-path, you may need to specify a full
;;; path for the filename instead of just "love22" (eg "~/emacs/love22", if
;;; "~/emacs/" isn't on your load-path but that's where love22.el is).

;;; Now in future editing sessions you'll be ready to use the love22 
;;; commands (for instance, M-x love22-region).  To use them now, first do
;;;   M-x load-file RET love22.el

;;; Changing values in the "Global variables" section modifies the behavior
;;; of the program; the experienced user may wish to play with these.

;;; Yes, you could use this as a filter from the command line by running emacs
;;; in batch mode.  This exercise is left to the reader.

;;; Enjoy!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Contents
;;;

;;; Description of love22.el
;;; How to use this file
;;; Contents
;;; To do
;;; Global variables
;;; Datatypes
;;; ABC Chart manipulation
;;; "Love22" a region or buffer
;;; Word roots
;;; 22-hook
;;; Love22 minor mode
;;; Love22-new-command
;;; Overlay and restore by modifying function cell binding
;;; Overlay and restore by replacing in keymap
;;; Overlay and restore -- top-level functions
;;; Utilities


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; To do
;;;

;;; Bug:  After turning love22 mode on and off, C-= is still bound to
;;;       abc-chart-word.

;;; Make this usable in batch mode, and describe how to do it.

;;; Figure out why this won't byte-compile, fix it, add the following
;;; to the instructions.
;; ;;; Type 
;; ;;;   M-x byte-compile-file RET love22.el
;; ;;; to byte-compile the file (it's too slow when it runs interpreted).
;; ;;; (M-x = Meta-x (hold down meta, press x), RET = the return key)

;;; Perhaps interactively ask each time that a love22 word is about to be
;;; 22-hook'ed.  (Only problem is that the current solution doesn't permit
;;; overlapping 22 words, since when something qualifies the rest of the
;;; history is thrown away.  This shouldn't be overly difficult to fix.)

;;; Instead of building lists front-to-back, I ought to build them in
;;; reverse order, then reverse them later.  nconc costs time to walk down
;;; the list to the end...

;;; Make the tex stuff settable, so I could get, say, bold instead of
;;; quotation with uppercasing.

;;; Make suffix-checking more efficient.
;;;  * rewrite suffixp better
;;;  * reverse the word and then compare from the front in word-root

(provide 'love22)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global variables
;;;

;;; These variables are grouped into behavior modifiers, root exceptions, 
;;; conversion list, and syntax table.  The asterisk at the beginning of 
;;; the documentation string means that the user is permitted to change 
;;; the variable.


;;; BEHAVIOR MODIFIERS

(defvar love22-emphasize t
  "*If non-nil, then in Love22 mode 22-words are automatically emphasized.")

(defvar love22-explicit-sum nil
  "*If non-nil, when a word with ABC chart value > 22 is emphasized, the actual
value is noted afterward.  This is done by love22-buffer and love22-region,
not by love22-mode, which never notes the value.")


;;; ROOT EXCEPTIONS

;;; These variables enumerate exceptions to the usual rules for finding
;;; roots of words.  The strings should be lowercase.

;; Consider adding "est" to the list of suffixes.  It might be too
;; complicated, too many exceptions, though.

(defvar love22-ly-exceptions 
  '("belly" "only" "really")
  "*Words whose roots can't be found by removing the \"ly\".")

(defvar love22-nt-exceptions 
  '("ain't" "can't" "don't" "shan't" "won't")
  "*Words whose roots can't be found by removing the \"n't\".
I believe that this list is exhaustive.")

(defvar love22-es-exceptions 
  '("james" "molasses" "rhodes" "ulysses" "yes")
  "*Words ending in \"es\" whose singulars cannot be found by removing the \"es\" or \"s\".")

(defvar love22-es-plural-suffixes
  '("ch" "sh" "th" "o" "ss" "x" "ay" "ey" "iy" "oy" "uy")
  "*Words ending with one of these suffixes add \"es\" to make the plural.")

(defvar love22-es-s-exceptions
  '("shoes" "horseshoes")
  "*Words ending in "\es\" that look like "\es\" was added to make the plural,
but actually only \"s\" was.  In other words, these words have some member of
love22-es-plural-suffixes immediately before the \"es\".")

(defvar love22-s-exceptions 
  '("as" "carlos" "Emacs" "has" "its" "plus" "towards" "was" "wumpus")
  "*Words ending in \"s\" whose singulars can't be found by removing the \"s\".
Words ending in \"es\" should appear in love22-es-exceptions, not here.")

(defvar love22-ous-exceptions
  '("bayous" "bijous" "cachous" "carcajous" "caribous" "chous" "congous"
    "froufrous" "ious" "kinkajous" "manitous" "marabous" "sous" "tinamous")
  "*Words ending in \"ous\" which are plural nouns, not adjectives.
I believe that this list is exhaustive.
\"loups-garous\" is omitted since two s's were added to make the plural.")

(defvar love22-ism-exceptions 
  '("schism")
  "*Words whose roots can't be found by removing the \"ism\".")
  
(defvar love22-ist-exceptions
  '("exist" "fist" "twist")
  "*Words whose roots can't be found by removing the \"ist\".")


;;; CONVERSION LIST

;;; These could go both directions, and possibly among homonyms (eg to => too)
;;; as well, but those seem less in the spirit of the thing.
;;; The inputs and results are lowercase for uniformity.
;;; None of the conversion inputs is a 22-word, else it shouldn't have been 
;;; here to begin with.  (This is not required, however.)
;;; I should make sure none of these has the same ABC chart value before and
;;; after, though the code works correctly (ie, uses the original value) 
;;; regardless.
(defvar love22-conversion-alist
  '(
    ;; letter homonyms
    ("be" . "B")
    ("see" . "C")
    ("sea" . "C")
    ("gee" . "G")
    ("aye" . "I")
    ("eye" . "I")
    ("jay" . "J")
    ("ell" . "L")
    ("oh" . "O")
    ("pea" . "P")
    ("pee" . "P")
    ("cue" . "Q")
    ("queue" . "Q")
    ("are" . "R")
    ("tee" . "T")
    ("tea" . "T")
    ("you" . "U")
    ("why" . "Y")
    ;; number homonyms
    ("won" . "1")
    ("to" . "2")
    ("too" . "2")
    ("for" . "4")
    ("fore" . "4")
    ("ate" . "8")
    ("nein" . "9")
    ;; numbers
    ("one" . "1")
    ("two" . "2")
    ("three" . "3")
    ("four" . "4")
    ("five" . "5")
    ("six" . "6")
    ("seven" . "7")
    ("eight" . "8")
    ("nine" . "9")
    ("ten" . "10")
    ;; synonyms
    ("can't" . "cannot")		;; cannot is a 22-word
    
    ;; How to deal with multi-word substitutions?  These could be especially
    ;; troublesome if only one of the two words is part (or all) of a 22-word.
    ;; ("won't" . "will not")
    ;; likewise with ("ain't" "can't" "don't" "shan't" "won't")
    ;; Don't want to just treat "will not" as a single word because of 22-hook. 
    
    )
  "*Alist of word conversions used to give phrases a 22 ABC chart value."
  )


;;; SYNTAX TABLE

(defvar love22-syntax-table nil
  "*Syntax table used by love22 to find word boundaries.")

;; What if the syntax table changes?  What if I change buffers or modes?
;; What if I invoke this from a buffer with a weird syntax table?
;; Solution:  use the text mode syntax table as a base and hope for the best.
(if (null love22-syntax-table)
    (progn
      (setq love22-syntax-table (copy-syntax-table text-mode-syntax-table))
      ;; Make - into inter-word punctuation.
      (modify-syntax-entry ?- "'" love22-syntax-table)
      ;; Make ' a word constituent.
      (modify-syntax-entry ?' "w" love22-syntax-table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Datatypes
;;;

;;; DESCRIPTION
;; A description represents a word unit (a word, a converted word, or the 
;; root of a word) via a vector of:
;;   ABC-value (a number)
;;   start (a point)
;;   length (nil if whole word, else number of characters in root)
;;   text (t if the text is what actually appears in the buffer)

(defmacro make-description (abc-value start length text)
  (list 'vector abc-value start length text))
(defmacro description-abc-value (desc)
  (list 'aref desc 0))
(defmacro description-start (desc)
  (list 'aref desc 1))
(defmacro description-length (desc)
  (list 'aref desc 2))
(defmacro description-text (desc)
  (list 'aref desc 3))
;; For use by mapcar
(defun description-text-fn (desc)
  (description-text desc))
;; If the length is nil, it's no root.  If the length is a number, it is.
(defmacro description-rootp (desc)
  (list 'description-length desc))

;;; TRACE
;; A trace is a view of the previous few words in the buffer.
;; Several traces may apply, if a recent word can be converted.
;; A trace is a list of descriptions, in reverse order (ie, most recent first).

(defvar love22-traces nil
  "A list of traces, most preferred first.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ABC Chart manipulation
;;;

(defun show-abc-chart ()
  "Display the ABC chart relating letters to their numeric values."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ " The ABC Chart\n\n")
    (princ "A B C D E F G H I\n")
    (princ "J K L M N O P Q R\n")
    (princ "S T U V W X Y Z  \n")
    (princ "1 2 3 4 5 6 7 8 9\n")
    (print-help-return-message)))

;; perhaps do this more elegantly.
(defvar abc-chart 
  (let ((chart (make-vector 256 0)))
    (setq ch ?1)
    (setq val 1)
    (while (<= ch ?9)
      (aset chart ch val)
      (setq val (1+ val))
      (setq ch (1+ ch)))
    (setq ch ?a)
    (setq val 1)
    (while (<= ch ?z)
      (aset chart ch val)
      (aset chart (upcase ch) val)
      (setq val (if (= val 9) 1 (1+ val)))
      (setq ch (1+ ch)))
    chart)
  "The ABC chart assigning numeric values to characters.")

(defmacro abc-chart-char (char)
  (` (aref abc-chart (, char))))
;; For use by mapcar
(defun abc-chart-char-fn (char)
  (abc-chart-char char))

;; This does NOT do clever substitutions.
(defun abc-chart-string (string)
  (apply '+ (mapcar 'abc-chart-char-fn string)))

;; This is adapted from ispell.el.
(defun love22-current-word (&optional noerr)
  "Returns a cons of (start . end) for the current word.
Errs if there is no current word, or returns nil if NOERR is true."
  (with-love22-syntax-table
   (save-excursion
     (if (not (looking-at "\\w"))
	 ;; Move backward for word if not already on one
	 (re-search-backward "\\w" (point-min) 'stay))
     ;; Move to start of word
     (re-search-backward "\\W" (point-min) 'stay)
     ;; Find start and end of word
     (if (re-search-forward "\\w+" nil t)
	 (cons (match-beginning 0) (match-end 0))
       (if (not noerr)
	   (error "No word to check."))))))

(defun abc-chart-word (&optional noerr terse)
  "Check word at or before point on the ABC chart.
If NOERR is non-nil, then doesn't err if there is no current word.
If TERSE is non-nil, the output is abbreviated."
  (interactive)
  (let ((region (love22-current-word noerr)))
    (if region
	(abc-chart-region (car region) (cdr region) terse))))

(defun abc-chart-region (start end &optional terse)
  "Print the sum of the ABC chart values of the characters in the region.
If TERSE is non-nil, the output is abbreviated."
  (interactive "r")
  (let ((region-string (buffer-substring start end)))
    (message (if terse
		 "%s = %d"
	       "%s = %d on the ABC chart.")
	     region-string (abc-chart-string region-string))))



(defun abc-chart-word-emphasize-maybe (&optional noerr terse)
  "Check word at or before point on the ABC chart.  If = 22, then emphasize it.
This needs to be interactive because it gets bound to keys."
  (interactive)
  (let ((region (love22-current-word noerr)))
    (if region
	(abc-chart-region-emphasize-maybe (car region) (cdr region) terse))))

;; 'Twould be nice if this did the hyphenization hacks, too, but it DOESN'T!
;; (and probably shouldn't, given its calling pattern -- too hard to undo).
(defun abc-chart-region-emphasize-maybe (start end &optional terse)
  "Print the sum of the ABC chart values of the characters in the region; if
= 22, then emphazise it."
  ;; This doesn't really need to be interactive.
  ;; (interactive "r")
  (let* ((region-string (buffer-substring start end))
	 (region-abc-value (abc-chart-string region-string)))
    (if (word-22-value-p region-abc-value)
	;; could also add quotes
	(upcase-region start end))
    (message (if terse
		 "%s = %d"
	       "%s = %d on the ABC chart.")
	     region-string (abc-chart-string region-string))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "Love22" a region or buffer
;;;

;;; Beware adding a hypen on the front or end of a hyphenized word, though 
;;; that isn't so great a tragedy.



;;; algorithm:
;; maintain a point before which nothing will be done.
;; maintain a list of lists of ABC values of the following words,
;; 	and the words themselves (perhaps in reverse order?), and points.
;;   * sublists in reverse order:  easier to look at bigger & bigger prefixes
;;   * but we shouldn't be looking at more than a couple of words that way, and
;;     the "correct" direction is more intuitive.
;;     can use mapcar to compute sums:  expensive but easy.  (no, hard to look
;;        at smaller prefixes first).
;; 
;; get next word, compute ABC value(s).
;;   * if any is 22 or multiple thereof, win
;;   * check each with prefixes of the lists; if any add up to 22 or mult, win.
;;   * prune the lists

(defun love22-region (start end)
  "Call 22-hook on the words in the region whose ABC chart values are 22."
  (interactive "r")
  
  ;; get rid of the buffer, if it's there.
  (debug-form
   (if (get-buffer "*debug*")
       (debug-form (kill-buffer (get-buffer "*debug*")))))

  (with-love22-syntax-table
   (save-excursion
     (save-restriction
       (narrow-to-region start end)
       (goto-char (point-min))
       
       (setq love22-traces nil)
       
       ;; while we can find the start of a new word
       (while (re-search-forward "\\<" nil t)
	 (setq word-start (point))
	 (forward-word 1)
	 (setq word-end (point))
	 (setq word (buffer-substring word-start word-end))
	 
	 (check-for-22 word word-start)
	 )
       ))))

(defun love22-buffer ()
  "Call 22-hook on the words in the buffer whose ABC chart values are 22.
If narrowing is in effect, only affects the accessible region."
  (interactive)
  (love22-region (point-min) (point-max)))

;; There are two distinct criteria:  one for individual words, and another
;; for sequences of words.  For instance, by default we let words have any
;;  multiple of 22, but avoid long sequences by prohibiting multi-word 
;; sequences from having such very large multiples.

(defun word-22-value-p (value)
  (and (zerop (mod value 22))
       (not (zerop value))))

(defun trace-22-value-p (value)
  (or (= 22 value) (= 44 value)))

(defun trace-22-too-large (value)
  "Returns t if the value is larger than any of the acceptable 22 values."
  (> value 44))


;; I should probably downcase the word first of all, because of suffix
;; checking later.  As it is, I'm doing that twice:  once for homonym
;; (conversion) checking and once for root checking.

(defun check-for-22 (word word-start)
  "Checks WORD (and combinations ending with WORD) for ABC value of 22.
Uses homonyms, checks for roots, searches backward through traces.
WORD-START is the start of the word; point is on the end of the word.
Calls 22-hook on any 22-word or sequence ending with word.
If none is found, updates love22-traces.
Doesn't return a useful value."
  
  (debug-form
   (prin1 "check-for-22:") (terpri)
   (prin1 " ") (prin1 word) (prin1 " ") (prin1 word-start) (terpri)
   (prin1 " ") (prin1 love22-traces) (terpri))

  ;; possibilities is a list of (continuep . description) pairs.
  ;; found-22 is a cons of (abc-value . trace)
  (let* ((possibilities nil)
	 (found-22
	  (catch 'found-22
	   
	   (let* ((permit-continuation
		   (not (or
			 ;; end of sentence
			 (looking-at sentence-end)
			 ;; two newlines before next word
			 (looking-at "\\W*\n\\W*\n")
			 ;; It would be nice to permit this if the whole quote
			 ;; or parenthesis were included, but not otherwise...
			 ;; end of a quote or parenthesis, or colon
			 (looking-at "\"\\|''\\|)\\|:")
			 ;; beginning of a quote or parenthesis
			 (looking-at ",?\\s *\\(\"\\|``\\|(\\)")
			 ;; Possibly prohibit coalescing after commas,
			 ;; but that doesn't seem like such a tragedy.
			 )))
		 
		  ;; ORIGINAL WORD
		  (word-abc-value (abc-chart-string word))
		  (word-description (make-description
				     word-abc-value word-start nil nil)))
	     (if (word-22-value-p word-abc-value)
		 (throw 'found-22 (cons word-abc-value (list word-description))))
	     (if (not (trace-22-too-large word-abc-value))
		 (setq possibilities (list (cons permit-continuation
						 word-description))))
	     
	     ;; CONVERSION
	     (let ((cword (cdr (assoc (downcase word) love22-conversion-alist))))
	       (if cword
		   (let* ((cword-abc-value (abc-chart-string cword))
			  (cword-description (make-description
					      cword-abc-value word-start nil 
					      cword)))
		     (if (word-22-value-p cword-abc-value)
			 (throw 'found-22 (cons cword-abc-value
						(list cword-description))))
		     (if (not (trace-22-too-large cword-abc-value))
			 (nconc possibilities
				(list (cons permit-continuation
					    cword-description))))))))

	   ;; ROOT
	   (let ((root (word-root word)))
	     (if root
		 (let* ((root-abc-value (abc-chart-string root))
			(root-description (make-description
					   root-abc-value word-start
					   (length root) nil)))
		   (if (word-22-value-p root-abc-value)
		       (throw 'found-22 (cons root-abc-value
					      (list root-description))))
		   ;; Perhaps don't allow coalescing with roots at all.
		   (if (not (trace-22-too-large root-abc-value))
		       (nconc possibilities
			      (list (cons nil root-description)))))))
	   
	   (debug-form (prin1 " final possibilities: ")
		       (prin1 possibilities) (terpri))

	   ;; now the word, its conversion, and its root are in possibilities
	   (let ((old-traces love22-traces))
	     (setq love22-traces nil)
	     
	     (while possibilities
	       (love22-coalesce (car possibilities) old-traces)
	       (setq possibilities (cdr possibilities))))

	   ;; No 22 was found (else we would have thrown out to 'found-22).

	   ;; ACTION:  I should now prune love22-traces of duplicates.
	   
	   ;; end of catch body; default is nil (no 22 found)
	   nil))
	  )
    ;; body of (let found-22 ...)
    (debug-form (prin1 " found-22 = ") (prin1 found-22) (terpri))
    (if found-22
	(progn (22-hook found-22)
	       (setq love22-traces nil)))))


(defun love22-coalesce (cont-desc old-traces)
  "Try to combine words in the traces into groups with 22-valued ABC values.
CONT-DESC is a (continuep . description) cons; TRACES is a list of traces."
  (debug-form
   (prin1 "love22-coalesce:") (terpri)
   (prin1 " ") (prin1 cont-desc) (terpri)
   (prin1 " ") (prin1 old-traces) (terpri))

  (let ((continuep (car cont-desc))
	(description (cdr cont-desc)))
    (if (null old-traces)
	;; There were no qualifying previous words.
	;; Start a new trace with only the current description.
	;; love22-traces might not be nil here if this is a transformation 
	;; and the original word has already been added.
	(if continuep
	    (setq love22-traces
		  (nconc love22-traces (list (list description)))))
      ;; There were previous words.
      (let (new-traces
	    this-trace
	    tt-remainder		; poor variable name choice
	    tt-sum)
	(while old-traces
	  ;; this-trace exists to be modified and returned (thrown to 'found-22).
	  ;; I should be able to do this more efficiently 
	  ;;   (eg only copy what's needed, not everything).
	  ;; This copy is because of the setcdr below, for truncating traces.
	  ;; (Alternately, I could always simply add to traces, and lose them
	  ;; only after a 22 or at the end of a sentence.  Hmmmm... seems fishy.)
	  (setq this-trace (cons description (copy-sequence (car old-traces)))
		tt-remainder (cons nil this-trace)
		tt-sum 0)
	  ;; tt-remainder lags behind those elements of this-trace actually
	  ;; examined, so this while loop's condition is "while this-trace has
	  ;; unexamined elements".  This won't bail out on the first time
	  ;; through because we wouldn't have gotten here if the ABC value of
	  ;; the first word were too large.
	  (while (cdr tt-remainder)
	    
	    (debug-form
	     (prin1 " tt-remainder = ") (prin1 tt-remainder) (terpri))

	    ;; Add in the next description's ABC value.
	    (setq tt-sum
		  (+ tt-sum (description-abc-value (car (cdr tt-remainder)))))
	    (if (trace-22-too-large tt-sum)
		;; If this value is too large, then we will never need to look 
		;; beyond the current value, so ditch everything after it.
		(setcdr tt-remainder nil)
	      (progn
		(setq tt-remainder (cdr tt-remainder))
		;; Now tt-remainder is the last description summed into tt-sum.
		(if (trace-22-value-p tt-sum)
		    (progn
		      ;; Delete everything not contributing to tt-sum.
		      (setcdr tt-remainder nil)
		      ;; Return the (modified) trace.
		      (throw 'found-22 (cons tt-sum this-trace))))))
	    ;; this was neither a 22-value nor too large; try again
	    )  ; end (while (cdr tt-remainder) ...)
	  ;; no 22-value found; save away this-trace
	  (if continuep
	      (setq new-traces (nconc new-traces (list this-trace))))
	  (setq old-traces (cdr old-traces))
	  )  ; end (while old-traces ...)

	(if continuep
	    (setq love22-traces (nconc love22-traces new-traces)))
	)  ; end (let new-traces ...)
      )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Word roots
;;;

;;; Finding roots may backfire if a root gets connected with something
;;; previous instead of the whole word getting connected with something later.
;;; That is, "FOO BARism baz" might be less desirable than foo BARISM BAZ".  Of
;;; course, BARISM is always checked before BARism; likewise with coalescing.

;;; Finding word roots seems like a hard problem.  I'm giving up on most of
;;; it for now.
;;; A future version could return a number of progressively smaller roots.

;;; ACTION:  This suffix checking is VERY inefficient.  Fix it later.

(defun word-root (word)
  "Returns a root for WORD, or nil if none is found.
Nasty heuristic."
  
  (setq word (downcase word))

  (setq suffix-characters
	(cond
	 ((and (suffixp word "ly")
	       ;; b: all "ble"
	       ;; f: last syllable is "fly"
	       ;; i: roots end in "y"
	       (not (memq (elt word (- (length word) 3)) '(?b ?f ?i)))
	       (not (string-member word love22-ly-exceptions)))
	  2)
	 ((and (suffixp word "n't")
	       (not (string-member word love22-nt-exceptions)))
	  3)
	 ((suffixp word "'s")
	  2)
	 ;; Maybe I don't need any silent-e check (eg consecutive consonants).
	 ((suffixp word "es")
	  (debug-form (prin1 "ES-check: ") (prin1 word)
		      (prin1 (string-member word love22-es-exceptions))
		      (terpri))
	  (if (string-member word love22-es-exceptions)
	      nil
	    ;; otherwise:
	    (let ((sans-es (substring word 0 (- (length word) 2))))
	      (if (suffixp sans-es "i")
		  nil
		(if (and (apply 'or-fn
				(mapcar
				 (function
				  (lambda (suffix) (suffixp sans-es suffix)))
				 love22-es-plural-suffixes))
			 (not (string-member word love22-es-s-exceptions)))
		    2
		  1)))))
	 ((and (suffixp word "s")
	       ;; avoid double-counting possessives
	       (not (suffixp word "'s"))
	       ;; i: root could end in "y"
	       ;; s: "mess", "watercress"
	       ;; v: root could end in "f" or "fe"
	       (not (memq (elt word (- (length word) 2)) '(?i ?s ?v)))
	       (not (string-member word love22-s-exceptions))
	       ;; bweare of "ous" adjectives
	       (or (not (suffixp word "ous"))
		   (string-member word love22-ous-exceptions)))
	  1)
	 ((and (suffixp word "ism")
	       (not (string-member word love22-ism-exceptions)))
	  3)
	 ((and (suffixp word "ist")
	       (not (string-member word love22-ist-exceptions)))
	  3)
	 ;;    ;; these might be too erratic for use.
	 ;;    ((suffixp word "ing")
	 ;;     ;; beware doubled letter:  travelling vs traveling (both OK)
	 ;;     ;; beware "baking"
	 ;;     (setq word-root (allbutlastthreechars word)))
	 ;;    ((past-form word)
	 ;;     ;;; for instance, "<doubled consonant>ed"
	 ;;     (foo))
	 ) ; end of cond
	)
  (debug-form (princ " suffix-chars: ") (prin1 suffix-characters) (terpri))
  (if suffix-characters
      (substring word 0 (- (length word) suffix-characters))))

(defun suffixp (word suffix)
  "Returns t if SUFFIX is a suffix of WORD, nil otherwise."
  (let ((suffix-len (length suffix))
	(word-len (length word)))
    (and (< suffix-len word-len)
	 (string-equal suffix (substring word (- word-len suffix-len))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 22-hook
;;;

;; What a mess!

;; ACTION: problem with redoing when there's an (= 44) there already which
;; isn't noticed or is coalesced with something else.

;; This should act intelligently depending on intervening text (eg newlines).

;;  "Can insert quotes, upcase, change spaces to hyphens, etc."

;; * upcase
;; * surround with "" (add extra space if already "")
;; * spaces to hyphens, if any substitutions done
;;      . be thoughtful about punctuation (eg don't, if colon or hyphen)
;;      . remember about newlines, reinsert elsewhere (need a global var)

;; work from end back to avoid screwing up points?
;; just use word-motion commands to find next word?  No, because of roots (?).
;; perhaps to deal with roots we need another slot in the description.
;; if the latter, beware roots getting messed up.
(defun 22-hook (abc-val-trace)
  "The argument is an (abc-value . trace) cons."
  (debug-form
   (prin1 "22-hook:") (terpri)
   (prin1 " ") (prin1 abc-val-trace) (terpri))

  (with-love22-syntax-table
   (let* ((abc-value (car abc-val-trace))
	  ;; Put 22-trace in forward order.
	  (22-trace (nreverse (cdr abc-val-trace)))
	  (convert-spaces
	   ;; at least two words in this trace, and some word was transformed.
	   (and (cdr 22-trace)
		(apply 'or-fn (mapcar 'description-text-fn 22-trace))))
	  ;; (region-start (description-start (car 22-trace)))
	  ;; (region-end (description-end (car (cdr (last 22-trace)))))
	  (missing-newline nil)
	  (trace-start (description-start (car 22-trace))))
     
     (debug-form 
      (prin1 " convert = ") (prin1 convert-spaces) (terpri))

     ;; Remember where I was.
     ;; Is this not very useful, in the case in which characters are inserted 
     ;; or deleted?  Perhaps the best thing is just to leave point at the end 
     ;; of the 22-region, or at the beginning of the first word after, or at 
     ;; the end of the last word even partially contained in it
     ;; (save-excursion ...)
     
     ;; go to beginning of region.
     (goto-char trace-start)
     
     ;; invariant:  at the beginning/end of this loop, I'm at the beginning
     ;; of the word corresponding to the first description.
     (while (cdr 22-trace)
       (if (description-text (car 22-trace))
	   (progn (kill-word 1)
		  (insert (upcase (description-text (car 22-trace)))))
	 (upcase-word 1))
       ;; (setq end-of-prev-word (point))
       ;; (re-search-forward "\\<")
       ;; (setq start-of-next-word (point))
       
       ;; Deal with interword stuff.
       (if convert-spaces 
	   (cond ((looking-at "\\s *\\<")
		  ;; only whitespace before beginning of next word, no
		  ;; punctuation:  convert to a hyphen with no surrounding
		  ;; space
		  (let ((end-of-prev-word (point)))
		    (re-search-forward "\\<")
		    (setq missing-newline 
			  (or missing-newline
			      (string-match "\n" (buffer-substring
						  end-of-prev-word
						  (point)))))
		    (delete-region end-of-prev-word (point))
		    (insert "-")))
		 ;; could have other cases here

		 ;; default -- leave punctuation as is.
		 (t
		  (re-search-forward "\\<")))
	 ;; Leave interword space alone, but move to beginning of next word.
	 ;; Here perhaps I should close up too-large spaces, etc.  Nah, that's 
	 ;; for the typist to do.
	 (re-search-forward "\\<"))
       

       (setq 22-trace (cdr 22-trace))
       )
     
     ;; Upcase last word, which may be a root.
     ;; This special code isn't necessary if we prohibit coalescing with roots.
     (let ((last-desc (car 22-trace)))
       (if (description-rootp last-desc)
	   (let ((word-start (point)))
	     (forward-char (description-length last-desc))
	     (debug-form
	      (prin1 " forward ") (prin1 (description-length last-desc))
	      (prin1 " from ") (prin1 (char-to-string (preceding-char)))
	      (prin1 (char-to-string (following-char))) (terpri))
	     (upcase-region word-start (point)))
	 (if (description-text (car 22-trace))
	     (progn (kill-word 1)
		    (insert (upcase (description-text (car 22-trace)))))
	   (upcase-word 1))))
     
     ;; Add surrounding quotes at trace-start and at point.
     ;; If the region is already surrounded by quotes, don't add any more.
     (if (not (quoted-region-p trace-start (point)))
       (let ((inserted-characters 0)
	     (trace-end (point)))
	 (goto-char trace-start)
	 (setq inserted-characters (insert-after-point
				    (if (TeX-mode-p) "``" "\"")))
	 (setq inserted-characters (+ inserted-characters
				      (add-quote-separation-maybe)))
	 (goto-char trace-end)
	 (forward-char inserted-characters)
	 (insert (if (TeX-mode-p) "''" "\""))))

     ;; Add sum
     (if (and (not (= 22 abc-value))
	      love22-explicit-sum)
	 (progn 
	   (if (not (looking-at "\\w"))
	       (insert " "))
	   ;; could use "n * 22" instead of "44" "66", etc.
	   (insert "(= " (int-to-string abc-value) ")")))
       
     ;; Add newline
     (if missing-newline 
	 (progn (insert "\n")
		(fixup-whitespace)))
     
     ;; Add quote separation
     (add-quote-separation-maybe)

     ;; Skip over the rest of word if this was a prefix.
     (if (description-rootp (car 22-trace))
	 (re-search-forward "\\>"))

     ;; if sum wasn't 22 (eg was 44), then point this out:  insert 
     ;; " (= 2*22)"
     
     )

   )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Love22 minor mode
;;;

;; problem:  minibuffer seems to use map which causes it to annoyingly
;; write over minibuffer input.  (Well, it goes away after a second.)
;;  Soln:  (1) fix this (2) put info in mode line

;; consider putting info in mode line instead.

;;; I should change the current syntax table into a full syntax table if
;;; it's currently sparse, else I'll spend too much time looking up characters
;;; in it (and describe-bindings will be ugly).

(defun love22-mode (arg) 
  "Minor mode to support writing of Love22 text.
Toggle love22-mode, or turn it on if optional ARG is positive.

Love22 mode causes the display of the ABC chart value value of the current
word, as well as emphasizing words whose value is a multiple of 22, if 
love22-emphasize is non-nil.  (This mode's emphasis functionality is
considerably less than that of love22-buffer, but it operates in real time."

  (interactive "P")
  (if love22-mode
      (love22-restore))
  (setq love22-mode
	(if (null arg) (not love22-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if love22-mode
      (love22-overlay))

  (set-buffer-modified-p (buffer-modified-p)))

;; Adapted from gin-mode
(if (boundp 'love22-mode)
    ()
  (setq minor-mode-alist (cons '(love22-mode " Love22") 
			       minor-mode-alist))
  (make-variable-buffer-local 'love22-mode)
  (set-default 'love22-mode nil)
  (fset 'old-kill-all-local-variables
	(symbol-function 'kill-all-local-variables))
  ;;   (make-variable-buffer-local 'gin-left-hang-indent-re)
  ;;   (make-variable-buffer-local 'gin-retain-indent-re)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Love22-new-command
;;;

;;; This is used to create new functions when the function cell definition
;;; is being modified or a wholly new command is being substituted into a
;;; keymap.

(defun lambda-list-argnames-from-iarg (iarg)
  "Returns a cons of (lambda-list . argnames), given the interactive spec."
  ;; Strip the leading asterisk, if any.
  ;; If the string is empty, this will err.  It deserves to err.
  (if (char-equal (elt iarg 0) ?*)
      (setq iarg (substring iarg 1)))
  (cond ((or (string-equal "p" iarg)
	     (string-equal "P" iarg))
	 (cons '(&optional n) '(n)))
	((string-equal "p\nP" iarg)
	 (cons '(a &optional b) '(a b)))
	((string-equal "Op" iarg)
	 (cons '(p-arg &optional P-arg)
	       '((if (or P-arg (not (= 1 p-arg))) p-arg))))
	(t
	 (error "Unrecognized interactive spec %s." iarg))))

;; This works when interpreted, but seems to be having some trouble
;; during byte-compilation.
(defmacro love22-new-command (int-arg old-command)
  "INT-ARG is the argument to interactive in OLD-COMMAND."
  ;; Why did this work before I had the eval??
  ;; Why does it work now??  (Well, it doesn't...)
  (let* ((oc (eval old-command))
	 (ia (eval int-arg))
	 (ll-an (lambda-list-argnames-from-iarg ia)))
    ;; This could be generalized.
    (if (string-equal ia "Op")
	(setq ia "p\nP"))
    (` (function 
	(lambda 
	  (, (car ll-an))
	  (, (concat "See " (symbol-name oc) " for more documentation."
		     "\nThis implementation also calls abc-chart-word."))
	  (interactive (, ia))
	  (, (cons oc (cdr ll-an)))
	  (abc-chart-word t t))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overlay and restore by modifying function cell binding
;;;

;; Which map contains these doesn't matter, since the function definition
;; is being directly changed.

;; ACTION:  Sometimes these don't seem to stick.
(defvar love22-cell-commands
  '(("p"
     previous-line next-line
     forward-word backward-word
     newline
     beginning-of-line end-of-line
     backward-sentence forward-sentence
     kill-word backward-kill-word
     )
    ("Op"
     scroll-up scroll-down
     )
    ("*P" 
     transpose-chars
     )
    ("*p\nP"
     delete-char delete-backward-char backward-delete-char-untabify
     )
    )
  "Commands whose definitions are modified by love22-mode.
This is a list of (interactive-arg . list-of-command-symbols) conses.

The keymap can remain unchanged; the new definition is used.  When love22-mode
is exited, the original definition is restored to the command.

The \"Op\" specification is special:  it's for \"p\" functions which should be
called with nil if there was no argument explicitly supplied.")

(defun love22-cell-overlay-commands (iarg-commands)
  "IARG-COMMANDS is a member of love22-cell-commands.
love22-cell-overlay-command is called on each of the commands."
  (let ((iarg (car iarg-commands)))
    (mapcar (function (lambda (command)
			(love22-cell-overlay-command iarg command)))
	    (cdr iarg-commands))))

(defun love22-cell-overlay-command (interactive-arg-cell command)
  "Adds to the function definition of symbol COMMAND a call to abc-chart-word."
  (let* ((name (symbol-name command))
	 (symbol-with-old (intern (concat "old-" name))))
    ;; (fset symbol-with-old (symbol-function command))
    (fset symbol-with-old (symbol-function command))
    
    ;; I build these functions anew each time the mode is invoked so that the 
    ;; most recent version is obtained (someone else might have been fiddling
    ;; with the function definitions, too); if love22-mode is invoked last
    ;; and turned off first, then this won't interfere with other such modes.
    
    (fset command (love22-new-command interactive-arg-cell symbol-with-old))))


;; ACTION:  This could check whether the defn has changed since it was set
;; by love22-mode.  If so, complain, or something.
;; This might require more space (eg storing the love22 definitions of the 
;; commands so there's something to check against).

(defun love22-cell-restore-commands (iarg-commands)
  "IARG-COMMANDS is a member of love22-cell-commands.
love22-cell-restore-command is called on each of the commands."
  (mapcar 'love22-cell-restore-command
	  (cdr iarg-commands)))

;; This doesn't have to worry about the interactive specification.
(defun love22-cell-restore-command (command)
  "Restores the original value of the function definition of symbol COMMAND."
  (fset command
	(symbol-function (intern (concat "old-" (symbol-name command))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overlay and restore by replacing in keymap
;;;

;; We can't just do
;; (fset 'old-self-insert-command (symbol-function 'self-insert-command))
;; (fset 'self-insert-command (symbol-function 'love22-self-insert-command))
;; as above, because Emacs special-cases when it sees self-insert-command
;; in a keymap.

(defvar love22-map-commands
  '(("p"
     self-insert-command
     forward-char backward-char
     )
    )
  "Commands replaced in the keymap by love22-mode.
This is a list of (interactive-arg . list-of-command-symbols) conses.

This is because when Emacs sees these commands, rather than looking up
their current definitions, it uses the original version coded in C.  Thus,
in order to use a new definition, the keymap must be changed.  When
love22-mode is exited, the original command is replaced in the keymap.

self-insert-command is an example of such a function, but it is also
treated somewhat specially.")

(defun love22-map-overlay-commands (iarg-commands)
  "IARG-COMMANDS is a member of love22-map-commands.
love22-map-overlay-command is called on each of the commands."
  (let ((iarg (car iarg-commands)))
    (mapcar (function (lambda (command)
			(love22-map-overlay-command iarg command)))
	    (cdr iarg-commands))))

(defun love22-map-overlay-command (interactive-arg-map overlaid-command)
  "Changes instances of COMMAND in the keymaps to the love22 version.

If the command is found in the global keymap, then the love22-symbol is
placed in the local keymap, shadowing it.  If the command is found in the
local keymap, then it is replaced by love22-local-symbol."

  (let* ((name (symbol-name overlaid-command))
	 (love22-symbol (intern (concat "love22-" name)))
	 (love22-local-symbol (intern (concat "love22-local-" name))))

    (fset love22-symbol (love22-new-command interactive-arg-map 
					    overlaid-command))
    (fset love22-local-symbol love22-symbol)
    
    (debug-form
     (with-output-to-temp-buffer
	 (concat "*" name " definition")
       (print love22-symbol)
       ;; (print (love22-new-command overlaid-command))
       (print (symbol-function love22-symbol))))
    
    ;; First, deal with command found in the local keymap.
    (substitute-key-definition overlaid-command
			       love22-local-symbol
			       (current-local-map))
    ;; Second, shadow instances of command visible in the global keymap.
    (mapcar (function (lambda (key)
			(if (not (local-key-binding key))
			    (local-set-key key love22-symbol))))
	    ;; This checks the global keymap only.
	    (where-is-internal overlaid-command))))

(defun love22-map-restore-commands (iarg-commands)
  "IARG-COMMANDS is a member of love22-map-commands.
love22-map-restore-command is called on each of the commands."
  (mapcar 'love22-map-restore-command
	  (cdr iarg-commands)))

(defun love22-map-restore-command (command)
  "Restores the original value of the function definition of symbol COMMAND.

When we undo the Love22 bindings, if the love22-local version is found in
the local keymap, the command replaces it there; otherwise, the local
keymap was originally empty (the command was found in the global keymap),
so the slot in the local keymap is simply unset and the key's binding will
henceforth be found in the global keymap, as desired."

  (let* ((name (symbol-name command))
	 (love22-symbol (intern (concat "love22-" name)))
	 (love22-local-symbol (intern (concat "love22-local-" name))))
    (mapcar 'local-unset-key
	    (where-is-internal love22-symbol
			       (current-local-map)))
    (substitute-key-definition 'love22-local-symbol
			       command
			       (current-local-map))))

;; Two distinct commands are substituted for self-insert-command; these
;; functions overlay and undo the emphasized versions in a fashion very
;; much like that above.

(defun love22-self-insert-emphasize-maybe (n)
  "Like self-insert, but also calls abc-chart-word-emphasize-maybe.
This needs to be interactive because it gets bound to keys."
  (interactive "p")
  (self-insert-command n)
  (if love22-emphasize
      (abc-chart-word-emphasize-maybe t t)
    (abc-chart-word t t)))

(defvar love22-local-self-insert-emphasize-maybe)
(fset 'love22-local-self-insert-emphasize-maybe
      'love22-self-insert-emphasize-maybe)

;; sei = self-insert-emphasize
(defun love22-sie-overlay ()
  "Substitute love22-self-insert-emphasize-maybe for self-insert-command."

  ;; Make punctuation possibly emphasize the preceding word;  maintain 
  ;; distinction between self-insert-command in local and global maps.
  (mapcar (function
	   (lambda (key)
	     (let ((key-binding (local-key-binding key)))
	       (if (eq key-binding 'self-insert-command)
		   (local-set-key key
				  'love22-local-self-insert-emphasize-maybe)
		 (if (and (not key-binding)
			  (eq (global-key-binding key) 'self-insert-command))
		     (local-set-key key
				    'love22-self-insert-emphasize-maybe))))))
	  '("." "!" "?" " " "," ";" ":" "-" "/"))
  )

(defun love22-sie-restore ()
  "Substitute self-insert-command for love22-self-insert-emphasize-maybe.
This undoes the effect of love22-sie-overlay."

  ;;   (mapcar 'local-unset-key
  ;; 	  (where-is-internal 'love22-self-insert-command
  ;; 			     (current-local-map)))
  (mapcar 'local-unset-key
	  (where-is-internal 'love22-self-insert-emphasize-maybe
			     (current-local-map)))
  ;;   (substitute-key-definition 'love22-local-self-insert-command
  ;; 			     'self-insert-command
  ;; 			     (current-local-map))
  (substitute-key-definition 'love22-local-self-insert-emphasize-maybe
			     'self-insert-command
			     (current-local-map)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Overlay and restore -- top-level functions
;;;

;; ACTION:  the old-*-binding variables don't seem to be doing their job;
;; \C-x= and \M-= stay bound to abc-chart-word and abc-chart-region.

(defvar old-c-x=-binding)
(defvar old-m-=-binding)
;; This should only be done once -- perhaps move up to the progn at the top.
(make-variable-buffer-local 'old-c-x=-binding)
(make-variable-buffer-local 'old-m-=-binding)

(defun love22-overlay-keybindings ()
  (setq old-c-x=-binding (local-key-binding "\C-x="))
  (setq old-m-=-binding (local-key-binding "\M-="))
  (local-set-key "\C-x=" 'abc-chart-word)
  (local-set-key "\M-=" 'abc-chart-region))

(defun love22-restore-keybindings ()
  (local-set-key "\C-x=" old-c-x=-binding)
  (local-set-key "\M-=" old-m-=-binding))

(defun love22-overlay ()
  "Substitutes new definitions for many commands.
Also, for each key bound to self-insert-command, places a binding in the local
keymap for love22-self-insert-command.
Because of this, love22 mode should be the last minor mode added and the
first removed."

  ;; If the local keymap is sparse, replace it with a full one.
  (make-local-keymap-full)

  ;; nonlocal changes
  (mapcar 'love22-cell-overlay-commands love22-cell-commands)
  ;; booby-trap kill-all-local-variables, called when the major mode changes
  (fset 'kill-all-local-variables
	(symbol-function 'love22-kill-all-local-variables))

  ;; local changes
  (love22-sie-overlay)
  (mapcar 'love22-map-overlay-commands love22-map-commands)
  (love22-overlay-keybindings))

(defun love22-restore-non-local ()
  "Fixes non-local everything that Love22 mode screwed up."
  (mapcar 'love22-cell-restore-commands love22-cell-commands)
  ;; Again, I do this every time because I'm paranoid the value might change.
  (fset 'kill-all-local-variables
	(symbol-function 'old-kill-all-local-variables)))

(defun love22-restore ()
  (love22-restore-non-local)
  ;; restore local changes
  (love22-sie-restore)
  (mapcar 'love22-map-restore-commands love22-map-commands)
  (love22-overlay-keybindings))

(defvar old-kill-all-local-variables nil
  "Keeps the true kill-all-local-variables function during Love22 mode.")

(defun love22-kill-all-local-variables ()
  (love22-restore-non-local)
  (kill-all-local-variables))

(defun make-local-keymap-full ()
  "If the local keymap is sparse, replace it with a full one."
  (let ((old-local-keymap (current-local-map)))
    (if (not (vectorp old-local-keymap))
	(progn
	  (use-local-map (make-keymap))
	  (if old-local-keymap
	      (mapcar (function
		       (lambda (char-binding)
			 ;; Breaking the abstraction barrier would be faster.
			 (local-set-key (char-to-string (car char-binding))
					(cdr char-binding))))
		      (cdr old-local-keymap)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;

;; In the standard 18.55 distribution, backquote has a bug.
;; I don't think this program needs this particular function, though...
;; (defun bq-splicequote (form)
;;   (setq tailmaker (list form (list 'quote tailmaker)))
;;   (setq state 'append))

(defmacro with-love22-syntax-table (&rest body)
  (` (let ((current-syntax (syntax-table)))
       (unwind-protect
	   (progn
	     (set-syntax-table love22-syntax-table)
	     (,@ body))
	 (set-syntax-table current-syntax)))))

(defvar love22-debugging nil
  "T if I'm debugging this, nil otherwise.
This runs very slowly while debugging is ongoing.")

(defmacro debug-form (&rest body)
  (if love22-debugging
      (append '(let ((standard-output (get-buffer-create "*debug*")))) body)))

;; Adapted from cl.el's member (which uses eql where I have string-equal).
(defun string-member (item list)
  "Find ITEM in LIST; return first link in LIST whose car is string= to ITEM."
  (let ((ptr list)
        (done nil)
        (result '()))
    (while (not (or done (null ptr)))
      (cond ((string-equal item (car ptr))
             (setq done t)
             (setq result ptr)))
      (setq ptr (cdr ptr)))
    result))

(defun or-fn (&rest args)
  "Like or, but can be an argument to apply, funcall, mapcar, etc."
  (if (null args)
      nil
    (or (car args) (apply 'or-fn (cdr args)))))

(defun insert-after-point (string)
  "Inserts the string after point.  Returns the length of the string."
  (let ((string-length (length string)))
    (insert string)
    (backward-char string-length)
    string-length))

(defun TeX-mode-p ()
  "Returns t if the major mode is a Tex editing mode, nil otherwise."
  (memq major-mode '(plain-TeX-mode 'LaTeX-mode)))

;; ACTION:  This should be more sophisticated:  e.g., in tex-mode, don't add
;; anything between { and { or } and }.  Perhaps use a variable settable like
;; the surrounding stuff (quotes vs. boldface...).
(defun add-quote-separation-maybe ()
  "Returns the number of characters inserted."
  (if (and (char-equal (preceding-char) (following-char))
	   (not (looking-at "\\s ")))
      ;; previous two chars are equal and not whitespace
      (if (TeX-mode-p)
	  (progn (insert "\\,") 2)
	(progn (insert " ") 1))
    0))

;; ACTION: problem with this when there's an (= 44) there already which
;; isn't noticed or is coalesced with something else.
(defun quoted-region-p (start end)
  "Returns t if the region is quoted, nil otherwise."
  ;; As it turns out, this always leaves point at end, where I want it, 
  ;; but it's best to be safe...
  (save-excursion
    (goto-char start)
    (if (TeX-mode-p)
	(and
	 (string-equal (buffer-substring (max (point-min) (- start 2)) start)
		       "``")
	 (progn (goto-char end)
		(looking-at "''"))
	 ;; ACTION:  internal quotes are matched
	 )
      (and 
       (not (bobp))
	 (char-equal (preceding-char) ?\")
	 (progn (goto-char end)
		(char-equal (following-char) ?\"))
	 ;; no internal quotation marks
	 (not (string-match "\"" (buffer-substring start end)))))))


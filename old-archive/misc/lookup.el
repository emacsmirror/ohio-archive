;;; lookup -- lookup word fragment in dictionary and replace with full word.

;; Author: Lawrence R. Dodd <dodd@roebling.poly.edu>
;;	Alon Albert <alon@milcse.rtsg.mot.com>
;; Maintainer: Lawrence R. Dodd <dodd@roebling.poly.edu>
;; Created: 1993 Sep 13
;; Version: 1.32
;; Keywords: extensions

;;; Copyright (C) 1993 Lawrence R. Dodd and Alon Albert

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Idea based on spell-dict.el by John W. Peterson but uses an improved
;;; algorithm.

;;; Alon Albert <alon@milcse.rtsg.mot.com> had the idea for in-the-buffer
;;; expansion similar lisp-complete-symbol (M-TAB) and also provided much of
;;; the code.

;;; Version:
;;;
;;; !Id: lookup.el,v 1.32 1993/09/27 18:22:08 dodd Exp !
;;; !Date: 1993/09/27 18:22:08 !
;;; !Revision: 1.32 !
;;;
;;; Available in /roebling.poly.edu:/pub/lookup.el

;;; LCD Archive Entry:
;;; lookup|Lawrence R. Dodd and Alon Albert|dodd@roebling.poly.edu|
;;; Lookup word fragment in dictionary and replace with full word.|
;;; 27-Sep-1993|1.32|~/misc/lookup.el.Z|

;;; Motivation:
;;;
;;; This is a feature missing from some spell checkers or is inconvenient to
;;; use in others.  It allows you to get a completion list from the system
;;; dictionary for a word fragment.  These examples assume you have bound M-x
;;; lookup-word to M-?.
;;;
;;;   Example 1:
;;;
;;;   The word `Saskatchewan' needs to be spelled.  The user may type `Sas'
;;;   and hit M-? and a completion list will be built using the shell command
;;;   `look' and displayed in the *Completions* buffer:
;;;
;;;        Possible completions are:
;;;        sash                               sashay
;;;        sashayed                           sashed
;;;        sashes                             sashimi
;;;        Saskatchewan                       Saskatoon
;;;        sass                               sassafras
;;;        sassier                            sassing
;;;        sasswood                           sassy
;;;
;;;   By viewing this list the user will hopefully be motivated to insert the
;;;   letter `k' after the `sas'.  When M-? is hit again the word `Saskat'
;;;   will be inserted in place of `sas' (note case) since this is a unique
;;;   substring completion.  The narrowed completion list can be viewed with
;;;   another M-?
;;;
;;;        Possible completions are:
;;;        Saskatchewan                       Saskatoon
;;;
;;;   Inserting the letter `c' and hitting M-? will narrow the completion
;;;   possibilities to just `Saskatchewan' and this will be inserted in the
;;;   buffer.
;;;
;;;   Example 2:
;;;
;;;   The user has typed `Sasaquane' and M-$ (ispell-word) gives no
;;;   "near-misses" in which case you back up to `Sas' and hit M-?
;;;   (lookup-word), find the correct word as above.  The word `Sasaquane'
;;;   will be replaced by the correct `Saskatchewan'.
;;;
;;;   Example 3:
;;;
;;;   Word fragments may also be used for the search.  The word `pneumonia'
;;;   needs to be spelled.  All the user can remember is the interior fragment
;;;   `mon' in which case she/he can do `C-u M-?' on `mon' and get a list of
;;;   all words containing the interior word fragment `mon'.  Typing `p' and
;;;   M-?  will narrow this list to all the words starting with `p' and
;;;   contains `mon' from which `pneumonia' can be found as the above example.

;;; Note:
;;;
;;; The version of ispell.el that comes with GNU Ispell 4.0 does have an `L'
;;; (regular expression look up) option for misspelled words.  Since this
;;; ispell.el feature can only be run after finding a misspelled words you can
;;; not do a lookup on `cat' for `catechetical' since `cat' is a correctly
;;; spelled word.  Furthermore, ispell.el does not return the entire list
;;; returned by `look' (for some reason).  I personally use both lookup.el and
;;; the `L' option of GNU Ispell's ispell.el and find they compliment each
;;; other.

;;; Installation:
;;;
;;; To use this package, simply put it in a file called "lookup.el" in a lisp
;;; directory known to Emacs (see the variable `load-path' for a list of such
;;; directories: M-x help-for-help v load-path [RET]), byte-compile it, and
;;; put the lines
;;;
;;;   (require 'lookup)
;;;   (define-key esc-map "?" 'lookup-word)
;;;
;;; in your ~/.emacs file or in the file default.el in the ../lisp directory.
;;; If you only want to load this when needed, then instead insert the lines
;;;
;;;   (autoload 'lookup-word "lookup"
;;;     "Lookup word fragment in dictionary and replace with full word." t)
;;;   (define-key esc-map "?" 'lookup-word)
;;;
;;; You may wish to bind this to something other than M-?.  The user-defined
;;; variables are
;;;
;;;  lookup-dict-file
;;;  lookup-regexp-ok
;;;
;;; to find out more about these variables, load this file, put your cursor at
;;; the end of any of the variable names, and hit C-h v [RET].  The only
;;; interactive function is (lookup-word).

;;; Algorithm:
;;;
;;; Uses call-process on the shell command `look' to find words in
;;; lookup-dict-file matching lookup-string or lookup-regexp storing the
;;; results in lookup-completions-alist.  It then parses the results for the
;;; completion.  It expands matches in the buffer and only rebuilds the
;;; lookup-completions-alist on a new call to lookup-word when the beginning
;;; word fragment has change.  Interior fragments are performed similarly with
;;; the exception that the entire fragment is initially removed from the
;;; buffer, no STRING is passed to the completion built-ins allowing all
;;; completions to be shown, and the location in the buffer is tested for
;;; future completion narrowing.
;;;
;;; Robust searches are done using a `look' with -r (regular expression)
;;; switch (e.g., the `look' that comes with GNU ispell).  This is controlled
;;; by the variable `lookup-regexp-ok'.  This helps in cases (for example)
;;; where `look ca' for some reason yields nothing but `look -r "^ca.*"'
;;; yields many many words.  [Can someone explain to me why `look ca' yields
;;; nothing while `look ad' yields many?  It seems like a bug in `look' to me
;;; since in both cases the word exists (i.e., both `ca' and `ad') but one
;;; gives a match and the other does not.  All versions of `look' seem to do
;;; this.  Strange, huh?]  Does expansion in current buffer instead of in
;;; mini-buffer similar to lisp-complete-symbol (M-TAB).


;;; FIX FOR mouse-choose-completion
;;; Alon Albert <alon@milcse.rtsg.mot.com> 
;;; 
;;; This will probably be a feature of GNU Emacs 19.20 
;;;  
;;; (defvar choose-execute-on-completion t
;;;   "If not nil then exucute command after completion")
;;; 
;;; 
;;; (defun choose-delete-max-match (string)
;;;   (let* ((len (min (length string) (1- (point))))
;;;         (string (substring string 0 len)))
;;;     (goto-char (- (point) len))
;;;     (while (and (> len 0) (null (looking-at string)))
;;;       (setq string (substring string 0 -1)
;;;             len (1- len))
;;;       (forward-char 1))
;;;     (delete-char len)))
;;; 
;;; (defun choose-choose-completion (event)
;;;   (interactive "e")
;;;   (let ((buffer (window-buffer))
;;;         choice)
;;;     (save-excursion
;;;       (set-buffer (window-buffer (posn-window (event-start event))))
;;;       (save-excursion
;;;         (goto-char (posn-point (event-start event)))
;;;         (skip-chars-backward "^ \t\n")
;;;         (let ((beg (point)))
;;;           (skip-chars-forward "^ \t\n")
;;;           (setq choice (buffer-substring beg (point))))))
;;;     (set-buffer buffer)
;;;       (choose-delete-max-match choice)
;;;       (insert choice)
;;;       (and (equal buffer (window-buffer (minibuffer-window)))
;;;            choose-execute-on-completion (exit-minibuffer))))
;;; 
;;; (define-key completion-list-mode-map [mouse-2] 'choose-choose-completion)


;;; Code:

;;;; User-defined variables.

(defvar lookup-dict-file "/usr/dict/words"
  "*Spelling dictionary file as string used by `lookup-word'.
Used to override default dictionary that `look' searches.  GNU ISpell's `look'
searches in \"${prefix}/lib/ispell/ispell.words\" by default.")

(defvar lookup-regexp-ok t
  "*t means `look' command can handle `-r' regular expression switch.
The version of `look' that comes with GNU ISpell has this feature.")

(defvar lookup-gnu-look-still-broken-p nil

  "*t if the version of GNU look is broken as follows. `look -dfr \"^ya\"
./foobar' returns nothing, while `look -dfr \"^ya.*\" ./foobar' returns the
word yacc, where ./foobar is a dictionary file containing the three lines
   .........
   y
   y's
   yacc
   .........

Both commands should return yacc.")


;;;; Internal variables.

;;; Possible completions for word
(defvar lookup-completions-alist nil)

;;; Last word processed by `lookup-word'
(defvar lookup-last-word nil)

;;; Buffer locals...

;;; Value of interior-frag in last call to lookup-word.
(defvar lookup-last-interior-p nil)
(make-variable-buffer-local 'lookup-last-interior-p)
(put 'lookup-last-interior-p 'permanent-local t)

;;; Buffer position in last call to lookup-word.
(defvar lookup-last-bow nil)
(make-variable-buffer-local 'lookup-last-bow)
(put 'lookup-last-bow 'permanent-local t)

;;;; Interactive function.
(defun lookup-word (&optional interior-frag)

  "Look up and complete words using letters at point to word beginning.
With optional argument INTERIOR-FRAG (C-u \\[lookup-word]) the word fragment is
assumed to be an interior word fragment in which case `lookup-regexp-ok'
should be to be t.  See also `lookup-dict-file'."

  (interactive "P")

  (let* ((completion-ignore-case t)

         ;; Get location of beginning of word fragment.
         (bow (save-excursion (skip-chars-backward "a-zA-Z'") (point)))

         ;; Get the string to look up.
         (lookup-string (buffer-substring bow (point)))

         ;; Get regexp for which we search and, if necessary, an interior word 
         ;; fragment.
         (lookup-regexp (if interior-frag
                            (concat "^.*" lookup-string ".*")
                          ;; Use fast binary search: no trailing `.*'.
                          (concat "^" lookup-string
                                  (if lookup-gnu-look-still-broken-p ".*"))))

         ;; We want all completions for case of interior fragments so set
         ;; prefix to an empty string.
         (prefix (if interior-frag "" lookup-string))
         completion)

    ;; Check for perfect completion already.  That is, maybe the user has hit
    ;; M-x lookup-word one too many times?
    (if (string= lookup-string "")
        (if (string= (concat lookup-last-word " ")
                     (buffer-substring
                      (save-excursion (forward-word -1) (point)) (point)))
            (error "Perfect match...still.  Please move on.")
          (error "No word fragment at point.")))

    ;; Create list of words in system dictionary starting with lookup-string.
    ;; If the new string for which we are searching contains the last word we
    ;; looked up we do not build a new list.  Also do not build if the last
    ;; call had a non-nil interior-word and the point has not moved.
    (if (and (not (and (equal lookup-last-bow bow)
                       lookup-last-interior-p))
             (or interior-frag
                 (null lookup-last-word)
                 (not (string-match (concat "^" lookup-last-word)
                                    lookup-string))))
        (setq lookup-completions-alist
              (lookup-build-list lookup-string lookup-regexp)))

    ;; Check for a completion of lookup-string in the list and store
    ;; lookup-string and other variables for the next call.
    (setq completion (try-completion prefix lookup-completions-alist)
          lookup-last-word lookup-string
          lookup-last-interior-p interior-frag
          lookup-last-bow bow)

    ;; Test the completion status.
    (cond

     ;; Guess is a perfect match.
     ((eq completion t)
      (insert " ")
      (message "Perfect match."))

     ;; No possibilities.
     ((null completion)
      (message "Can't find completion for \"%s\"" lookup-string)
      (beep))

     ;; Replace string fragment with matched common substring completion.
     ((and (not (string= completion ""))
           (not (string= completion lookup-string)))
      (search-backward lookup-string bow)
      (replace-match completion nil t) ; FIXEDCASE doesn't work? or LITERAL?
      (message "Proposed unique substring.  Repeat for completions list."))

     ;; String is a common substring completion already.  Make list.
     (t
      (message "Making completion list...")
      (if (string= completion "") (delete-region bow (point)))
      (let ((list (all-completions prefix lookup-completions-alist)))
        (with-output-to-temp-buffer " *Completions*"
          (display-completion-list list)))
      (message "Making completion list...done")))))

;;;; Internal Function.

;;; Build list of words from system dictionary starting with LOOKUP-STRING if
;;; lookup-regexp-ok is nil or LOOKUP-REGEXP if lookup-regexp-ok is t.
;;; Returns resulting alist.
(defun lookup-build-list (lookup-string lookup-regexp)
  (save-excursion
    (message "Building list...")
    (set-buffer (get-buffer-create " *lookup*"))
    (erase-buffer)

    (if lookup-regexp-ok
        (call-process "look" nil t nil "-dfr" lookup-regexp lookup-dict-file)
      (call-process "look" nil t nil "-df" lookup-string lookup-dict-file))

    ;;  Build list for completing-reading by storing each line in the
    ;;  alist lookup-list starting from the bottom.

    ;; I trashed my own similar method for doing this and use instead the
    ;; method used in `ispell-do-look' of ispell.el.  That way if someone
    ;; ever merges these two it will be easier. -lrd 9/15/93.

    (let (lookup-list)

      (goto-char (point-min))
      (while (not (= (point-min) (point-max)))
        (end-of-line)
        (setq lookup-list (cons (buffer-substring (point-min) (point))
                                lookup-list))
        (forward-line)
        (delete-region (point-min) (point)))

      ;; Clean.
      (erase-buffer)
      (message "Building list...done")

      ;; Make the list into an alist and return.
      (mapcar 'list (nreverse lookup-list)))))


;;; Provide this package.
(provide 'lookup)

;;; lookup.el ends here

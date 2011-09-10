;; @(#) tinysword.el -- search word under cursor: backward, forward

;; @(#) $Id: tinysword.el,v 1.10 1995/05/06 21:22:07 jaalto Release_5 jaalto $
;; @(#) $Keywords: emacs, tools, text, searching $
;; $KnownCompatibility: 18.57 , 19.28 $
;; $outlineRegexp: $
;; $bookMarkRegexp: $
;; $PackageInstallRe: '^[ \t]*;;+[*] ' $

;; This file is *NOT* part of GNU emacs

;;{{{ Documentation

;; This code is free software in terms of GNU Gen. pub. Lic. v.2 or later

;; Copyright (C)  1994,1995 Jari Aalto <jaalto@tre.tele.nokia.fi>
;; Author:        Jari Aalto <jaalto@tre.tele.nokia.fi>
;; Maintainer:    Jari Aalto <jaalto@tre.tele.nokia.fi>
;; FIrst Created: Nov 7th 1994
;; Version:       $Revision: 1.10 $
;; Sate:          $State: Release_5 $
;;
;; To get information on this program use ident(1) or M-x tisw-version
;; Look at the code with folding.el 1.7 , tinyfold.el 1.5

;; LCD Archive Entry:
;; tinysword|Jari Aalto|jaalto@tre.tele.nokia.fi|
;; emacs 18-19, search word under cursor: backward, forward|
;; 07-Nov-1994|1.10|~/misc/tinysword.el.Z|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Intallation:

;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file
;;
;;     (require 'tinysword)
;;
;; 19.xx user can use function keys easily, like this:
;; (global-set-key (quote [f2]) 'tisw-search-word)
;; (global-set-key (quote [S-f2])
;;     '(lambda () (interactive) (tisw-search-word 1)))

;;; Commentary:

;; RESERVED prefix FOR FUNCTIONS IN THIS FILE
;; ===========================================
;; - To avoid collisions to other modules I use "tisw-" in front of
;;   every function & variable. It stands for '(ti)ny (s)earch (w)ord
;; - variable names contain letters '-v-'.
;;
;; PREFACE
;; ===========================================
;; - In 7 Nov 1994 aep@world.std.com (Andrew E Page) posted interesting code
;;   by article name 'Script Example:  Search for next word', which I took
;;   a look. The idea of code was excellent, but it didn't work as expected at
;;   all. Gradually the idea was crystallized into this el, which now works
;;   with emacs 18 and 19.
;;

;;}}}
;;{{{ history

;; HISTORY OF CHANGES
;; ===========================================
;; 
;; May  6       1995    19.28   v1.8-1.10 [jaalto]      Release_5
;; - This had been working so well that hadn't looked it for a long
;;   time... When I finally wanted to change tisw-v-word-boundary-set
;;   variable so that it would include ':' for class names in C++, I
;;   was supriced? Not working any more? It turned out that I used
;;   defvar for composing tisw-word-boundary, so actually what
;;   happened was, that this package never saw the change....
;; - I changed them into macros, much better now :-/
;;
;; Nov 30        1994   19.28   v1.7    [jaalto]        Release_4
;; - Added hooks, added configurable charset, corrected *again*
;;   the search-word function...
;; - The packet polished: variables and functions
;;
;; Nov 26       1994    19.27   v.1.3   [jaalto]        Release_3
;; - Now supports 19.27 isearch C-s C-r
;; - Now REALLY grabs whole words. The prev versions worked incorrectly.
;;
;; Nov 21       1994    18.57   v1.2    [jaalto]        Release_2
;; - Added isearch support for 18.57. Now you can use C-s C-r after word has
;;   been grabbed. A good feature suggested by
;;   hochberg@LightStream.COM (Yigal Hochberg).
;;
;; Nov 7        1994    18.57   v1.1    [jaalto]        Release_1
;; - Andrew sent quite interesting piece of code which unfortunately
;;   had some errors and weaknesses --> it didn't work. I took
;;   a hobby to look at it and make new version. I think this is good
;;   piece of code which teaches various important facts for newcomers...
;; - The code above simplified the searching by using only 1 function
;;   for either direction.
;; - Have fun :-)

;;}}}

;;; Code:

(provide 'tinysword)

;;{{{ hooks

;;; .......................................................... &Hooks  ...

(defvar tisw-b-hooks nil
  "*Hook that is run at the BEG of search function.
You can set this to point to function that alters the value of
tisw-v-word-boundary-set eg. by looking at the file type.")

(defvar tisw-e-hooks nil
  "*Hook that is run at the end of search function <if> the word was grabbed
for searching. You can look at the variable tisw-v-stat to find out if
the search was successfull, you could also use tisw-v-direc to find
direction of the search.
")

(defvar tisw-ef-hooks nil
  "*Hook that is _always_ run at the END of search function. It doesn't
care about word grabbings or search failures. ef = end final.")

;;}}}
;;{{{ variables

;;; ............................................................. &var ...

(defconst tisw-version-id
  "$Id: tinysword.el,v 1.10 1995/05/06 21:22:07 jaalto Release_5 jaalto $"
  "Latest modification time and version number.")

(defun tisw-version ()
  (interavtive)
  (message tilib-version-id))


(defvar tisw-v-word-boundary-set "-:A-Za-z0-9_"
  "*Variable that determines which character are counted to
conform a single word. You might want to set this to something else
before doing search.")


(defvar tisw-v-direc nil
  "Tells direction of search. nil = fwd.")
(defvar tisw-v-stat nil
  "Status of word search. t = successfull")

;;}}}
;;{{{ macros

;;; ----------------------------------------------------------------------
;;;
(defmacro tisw-is-isearch19 ()
  "determines if we're in emacs that uses isearch version for 19.
It uses search-ring, not target-string as emacs 18 for storing
search value.
"
  (list 'boundp 'regexp-search-ring))


(defmacro tisw-word-boundary ()
  "Returns boundary regexp. Complement of word chars"
  (list 'concat "[^" 'tisw-v-word-boundary-set "]"))


(defmacro tisw-re-word ()
  "Word charset in RE form"
 (list 'concat "[" 'tisw-v-word-boundary-set "]"))

;;}}}

;;{{{ 19.xx isearch add

;;; ----------------------------------------------------------------------
;;;
(defun tisw-add-to-ring-isearch19 (isearch-string)
  "Adds search pattern to isearch ring in 19.xx
This code is directly taken from function isearch-done By Daniel LaLiberte.
"
  (if (> (length isearch-string) 0)
      ;; Update the ring data.
      (if isearch-regexp
          (if (or (null regexp-search-ring)
                  (not (string= isearch-string (car regexp-search-ring))))
              (progn
                (setq regexp-search-ring
                      (cons isearch-string regexp-search-ring))
                (if (> (length regexp-search-ring) regexp-search-ring-max)
                    (setcdr (nthcdr (1- search-ring-max) regexp-search-ring)
                            nil))))
        (if (or (null search-ring)
                (not (string= isearch-string (car search-ring))))
            (progn
              (setq search-ring (cons isearch-string search-ring))
              (if (> (length search-ring) search-ring-max)
                  (setcdr (nthcdr (1- search-ring-max) search-ring) nil))))))
  )

;;}}}
;;{{{ main

;;; ----------------------------------------------------------------------
;;;
(defun tisw-search-word (&optional direction)
  "Gets word under cursor and searches next occurrance.
If prefix argument is non-nil, the search will be headed backward

Before searching is done the tisw-hooks is thrown. This is usefull
is you want someone to dynamically change the search-word's idea of
the chars belonging to word. By setting tisw-v-word-boundary-set you
can set differents sets for text and lisp.  [In lisp the '-' is part of
word while in text it normally isn't].

NOTE:
   You cannot search 1 char words with this due to internal
   behaviour of search method and cursor positioning.
"
  (interactive "P")                     ;available as a command
  (let (pe
        pb                              ;point BEG END
        re-word-boundary
        re-word                         ;considered single word
        target-string
        blp elp                         ;beg end line points
        p
        re
        found
        )

    (setq tisw-direc direction) ;inform possible hook func

    ;;   Let the user set the word criteria
    (run-hooks tisw-b-hooks)
    (setq re-word-boundary (tisw-word-boundary))
    (setq re-word (tisw-re-word))       ;considered single word

    ;;
    ;; Note:  the first search goes backwards to find the start of the
    ;;        word, which is one character in front of the character
    ;;        found by the search.  Then we go forward to the end of
    ;;        word which is one character behind the character found by the
    ;;        search.

    (save-excursion
      (beginning-of-line) (setq blp (point))
      (end-of-line) (setq elp (point)))

    (save-excursion                     ;conceive original (point)
      (if (re-search-backward re-word-boundary blp t)
          (setq pb (1+ (point))))
      (if pb nil                        ;already found
        (setq p (point)) (beginning-of-line)
        (if (eq p (point))
            (setq re re-word)
          (setq re (concat re-word "+")))
        (if (re-search-forward re (1+ p) t)     ; word at the BEG
            (setq pb blp)))
      )

;;;    (d! re-word-boundary pb)

    (save-excursion
      (if (re-search-forward re-word-boundary elp t)
          (setq pe (1- (point))))
      (if pe nil                        ;already found
        (if (looking-at (concat re-word "+$"))  ; handle word at the END of ln
            (setq pe elp)))
      )

    (if (not (and pb pe))
        (message "Word not grabbed.");
      (setq target-string (buffer-substring pb pe))

      ;;   enable C-s and C-r to use the word, look isearch.el
      ;;   NOTE: this doesn't put the WORD regexp there...
      (if (null (tisw-is-isearch19))
          (setq search-last-string target-string) ;we're in 18.xx
        (tisw-add-to-ring-isearch19 target-string))

      ;; post a message saying what we're looking for
      (message "searching for \`%s\`" target-string)
      (setq re-word
            (concat
             "\\(^" target-string "\\|"
             re-word-boundary target-string "\\)" re-word-boundary
             ))
      (if direction                     ;choose direction
          (progn
            (setq found (re-search-backward re-word nil t))
            (if (null found)
                (message "No more words.")
              (forward-char 2)
              )         ;2 'cause of E-SPC
            )
        (setq found (re-search-forward re-word nil t))
        (if (null found)
            (message "No more words.")
          ;;   So that NEXT word will be grabbed, that's why 1 char words
          ;;   can't be found
          (backward-char 2))
        )
      ;; ---------- direction
      (setq tisw-stat found)            ;save status
      (run-hooks tisw-b-hooks)
      )
    ;; ---------------------- grabbed
    (run-hooks tisw-ef-hooks)
    ))

;;}}}

;; ---------------------- end of tinysword.el ----------------------------

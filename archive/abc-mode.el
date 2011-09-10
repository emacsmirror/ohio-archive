;;; abc-mode.el --- Major mode for editing ABC files

;; Copyright (C) 2000 by Michael Abraham Shulman

;; Emacs Lisp Archive Entry
;; Filename: abc-mode.el
;; Author: Michael Abraham Shulman <viritrilbia@users.sourceforge.net>
;; Version: 1.0
;; Keywords: languages, tools

;;{{{ GPL

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;}}}

;;; Commentary:

;;{{{ Loading ABC Mode

;; This file defines a major mode for editing files in the ABC music
;; format.  To load it, place this file in your `load-path' and add
;; the following line to your `.emacs' file:
;;
;;   (require 'abc-mode)
;;
;; ABC Mode can also be autoloaded by using this line instead:
;;
;;   (autoload 'abc-mode "abc-mode" "Major Mode for ABC files" t)
;;
;; You can then enter ABC Mode by typing `M-x abc-mode RET'.  To
;; automatically edit all files with the extension `.abc' in ABC Mode,
;; add the following line to your `.emacs' file:
;;
;;   (add-to-list 'auto-mode-alist '("\\.abc\\'" . abc-mode))
;;
;; For instructions on using ABC Mode, see the mode documentation.
;; You can access this, when in ABC Mode, by typing `C-h m'.

;;}}}

;;; Code:

(require 'cl)

;;{{{ Keymaps

(defvar abc-mode-map (make-sparse-keymap)
  "Keymap for ABC Mode.")

(define-key abc-mode-map "\C-c\C-c" #'abc-postscript-tune)
(define-key abc-mode-map "\C-c\C-b" #'abc-postscript-buffer)
(define-key abc-mode-map "\C-c\C-n" #'abc-postscript-region)
(define-key abc-mode-map "\C-c\C-p" #'abc-postscript-file)
(define-key abc-mode-map "\C-c\C-v" #'abc-view-postscript)

(define-key abc-mode-map "\C-c\C-r" #'abc-transpose-tunes)
(define-key abc-mode-map "\C-c\C-i" #'abc-insert-header)

(define-key abc-mode-map "\C-c\C-s" #'abc-search)

(define-key abc-mode-map "\C-\M-f" #'abc-forward-measure)
(define-key abc-mode-map "\C-\M-b" #'abc-backward-measure)
(define-key abc-mode-map "\C-c\C-g" #'abc-goto-tune)

(define-key abc-mode-map "\C-c\C-t" #'abc-jump-to-tune-body)
(defvar abc-header-map (make-sparse-keymap)
  "Keymap for the ABC jump-to-header-field commands.")
(if (featurep 'xemacs)
    (set-keymap-default-binding abc-header-map #'abc-jump-to-header)
  (define-key abc-header-map [t] #'abc-jump-to-header))
(define-key abc-mode-map "\C-c\C-f" abc-header-map)

;;}}}
;;{{{ Syntax Table

(defvar abc-mode-syntax-table (make-syntax-table)
  "Syntax table for ABC Mode.")

(defvar abc-mode-syntaxes
  `((?\| " ")                   ; separates measures
    (?\( "_")                   ; (slur), but also (3triplet.
    (?\) ")(")
    (?\[ "(]")                  ; [chord]
    (?\] ")[")
    (?\{ "(}")                  ; {ornament}
    (?\} "){")
    (?\" "\"")                  ; "guitar chord"
    (?\+ "\"")                  ; +guitar chord+ (deprecated)
    (?\% "<")                   ; % comment
    (10 ">")
    (12 ">")
    ;; I don't think this next section works.
    (?N "w1")                   ; notes, etc. are comments...
    (?H "w1")
    (?\: "_2")                  ; ...when followed by a colon
    ;; Consider groups of notes to be "symbols".
    ,@(mapcar #'(lambda (char) `(,char "_"))
              '(?< ?>           ; hornpipe rhythm, etc.
                   ?^ ?_ ?=     ; sharp/flat/natural
                   ?~           ; ornamentation
                   ?.           ; staccato
                   ?-           ; ties
                   ))
    )
  "Syntax entries to change from the default in ABC Mode.")
(mapc #'(lambda (pair)
          (modify-syntax-entry (car pair) (cadr pair)
                               abc-mode-syntax-table))
      abc-mode-syntaxes)

;;}}}
;;{{{ Font Lock

(defvar abc-measure-separator
  (regexp-opt '("|" ":|" "|:" ":||:" "::"))
  "Regexp that separates measures.")

(defvar abc-font-lock-keywords
  `(("^\\([ABCDFGIOSWwZ]\\):\\(.*\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-string-face))
    ("^\\([KLMPQRX]\\):\\(.*\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-builtin-face))
    ("^\\([HN]\\):\\(.*\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-comment-face))
    ("^\\([T]\\):\\(.*\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    ("[<>~.vu-]" . font-lock-constant-face)
    ("[=^_]" . font-lock-type-face)
    (,abc-measure-separator . font-lock-builtin-face)
    )
  "Font lock keywords for ABC Mode.")

;;}}}
;;{{{ ABC Mode Function

(defun abc-mode ()
  "Major mode for editing ABC music files.
Tune headers, comments, ornamentation marks, bar lines, accidentals,
and so on are highlighted with font-lock.  There are also a number of
commands that make editing and working with ABC files more convenient.
\\<abc-mode-map>
These commands `abc2ps' to create and view postscript output:

\\[abc-postscript-tune]	- Run `abc2ps' on the current tune.
\\[abc-postscript-buffer]	- Run `abc2ps' on the current buffer.
\\[abc-postscript-file]	- Run `abc2ps' on the current file.
\\[abc-postscript-region]	- Run `abc2ps' on the current region.
\\[abc-view-postscript]	- View the last postscript file created.

These commands move around in an ABC tune or file:

\\[abc-goto-tune]	- Go to a specified tune in the current file.
\\[abc-jump-to-tune-body]	- Jump to start of current tune body.
\\[abc-jump-to-header]
	- Jump to current tune's title \(T) field.  Analogous commands
	  exist for all header fields \(X, S, N, K, ...).  They create
	  the field if no such field exists.
\\[abc-forward-measure]	- Move forward one or more measures.
\\[abc-backward-measure]	- Move backward one or more measures.

This command transposes one tune or the entire buffer from one key
signature to another, if `abc2abc' is installed.

\\[abc-transpose-tunes]	- Transpose tunes with `abc2abc'.

This command inserts the header for a new tune, prompting for
important information such as the type, key, and title.

\\[abc-insert-header]	- Insert a header for a new tune at point.

Finally, this powerful command allows you to search one or more ABC
files using `abc2ps', view each matching tune's ABC source, and select
one or more of them to make into postscript output.

\\[abc-search-tunes]	- Search ABC file\(s) for tunes with `abc2ps'.

When you invoke this command, it allows you to edit the command line
and then runs `abc2ps' with output to the buffer `*abc2ps*'.  This
buffer is in a special version of `compilation-mode', so you can use
these keys:\\<compilation-mode-map>

\\[compile-goto-error]	- View the ABC source of the current tune.
\\[compile-mouse-goto-error]	- View the ABC source of the selected tune.

as well as these keys:\\<abc2ps-minor-mode-map>

\\[abc2ps-omit-tune]	- Remove the current tune from the display.
\\[abc2ps-mark-tune]	- Mark or unmark the current tune.
\\[abc2ps-postscript-tune]	- Make postscript from marked/current tune\(s).
\\[abc2ps-postscript-all]	- Make postscript from all displayed tunes.
\\[abc-view-postscript]	- View last made postscript file.
\\[abc2ps-next-tune]	- Move to next displayed tune.
\\[abc2ps-previous-tune]	- Move to previous displayed tune.
\\[bury-buffer]	- Bury the output buffer.
"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'abc-mode
        mode-name "ABC")
  (use-local-map abc-mode-map)
  (set-syntax-table abc-mode-syntax-table)
  (mapc
   #'(lambda (pair)
       (set (make-local-variable (car pair)) (cadr pair)))
   `((comment-column 0)
     (comment-start-skip "%+ *")
     (comment-start "% ")
     (comment-end "")
     (comment-multi-line nil)
     (comment-indent-function (lambda () comment-column))
     (adaptive-fill-regexp "\\w:")
     (adaptive-fill-first-line-regexp "\\w:")
     (imenu-generic-expression ((nil "T: *\\(.+\\)" 1)))
     (outline-regexp "T:")
     (font-lock-defaults
      (abc-font-lock-keywords
       nil                      ; do syntactic fontification
       nil                      ; do include case when searching
       nil                      ; syntax-alist
       backward-paragraph       ; syntax-begin
       ))
     ))
  (run-hooks 'abc-mode-hook))

(add-to-list 'auto-mode-alist '("\\.abc\\'" . abc-mode))

;;}}}
;;{{{ Tune Inspection

(defun abc-current-tune-field (field)
  "Return the value of a header FIELD for the current tune."
  (save-excursion
    (forward-paragraph -1)
    (re-search-forward (format "^%s: *\\(.+\\)" field)
                       (save-excursion (forward-paragraph) (point))
                       t)
    (or (match-string 1) "")))

(defun abc-title-to-filename (title)
  "Convert a tune TITLE to an appropriate postscript filename."
  (let ((words (split-string (downcase title) "[^a-z]+")))
    (when (string= (car words) "the")
      (setq words (cdr words)))
    (when (string= (car (last words)) "the")
      (setq words (butlast words 1)))
    (format "%s.ps" (mapconcat #'identity words "-"))))

;;}}}
;;{{{ Postscript Output

(defun abc-postscript-tune (filename)
  "Run `abc2ps' on the current tune to produce postscript output.
FILENAME is the output file name.  When called interactively, it is
prompted for with a suitable default supplied."
  (interactive
   (let ((file (abc-title-to-filename (abc-current-tune-field "T"))))
     (list
      (read-file-name (format "Output File (default %s): " file)
                      nil file nil))))
  (save-excursion
    (abc-postscript-region (progn (forward-paragraph -1) (point))
                           (progn (forward-paragraph 1) (point))
                           filename)))

(defun abc-postscript-buffer (filename)
  "Run `abc2ps' on the current buffer to produce postscript output.
FILENAME is the output file name.  When called interactively, it is
prompted for with a suitable default supplied."
  (interactive
   (let ((file (or buffer-file-name
                   (concat default-directory (buffer-name)))))
     (setq file
           (if (string-match "\\.abc\\'" file)
               (replace-match ".ps" t t file)
             (concat file ".ps")))
     (list
      (read-file-name (format "Output File (default %s): " file)
                      nil file nil))))
  (save-excursion
    (mark-paragraph)
    (abc-postscript-region (region-beginning) (region-end)
                           filename)))
  
(defun abc-postscript-region (start end filename)
  "Run `abc2ps' on the region to produce postscript output.
FILENAME is the output file name.  It is prompted for when called
interactively."
  (interactive "r\nFOutput File: ")
  (abc-postscript-string (buffer-substring start end) filename))

(defun abc-postscript-string (string filename)
  "Run `abc2ps' on STRING to produce FILENAME."
  ;; abc2ps doesn't accept stdin as an input file, so we need to make
  ;; a temporary file.
  (setq filename (expand-file-name filename))
  (let ((tempfile (format "%s/abc%s"
                          (or (getenv "TEMP") (getenv "TMP") "/tmp")
                          (abs (random)))))
    (with-temp-file tempfile
      (insert string))
    (shell-command
     (format "abc2ps -f %s -o -O %s"
             (shell-quote-argument tempfile)
             (shell-quote-argument filename)))
    (setq abc-last-made-postscript filename)
    (delete-file tempfile)))

(defun abc-postscript-file (selectors filename)
  "Run `abc2ps on the current file to produce postscript output.
Tunes are selected with SELECTORS which are quoted before being passed
to the shell.  See abc2ps(1) for more information.  The output file is
named FILENAME.  Both SELECTORS and FILENAME are prompted for
interactively."
  (interactive
   (let ((tune (abc-current-tune-field "X"))
         ;; TODO: Update this depending on selectors entered.
         (file (abc-title-to-filename (abc-current-tune-field "T"))))
     (save-some-buffers)        ; Do this first
     (list
      (read-string "Tune Selectors: " tune)
      (read-file-name (format "Output File (default %s): " file)
                      nil file nil))))
  (setq filename (expand-file-name filename))
  (setq abc-last-made-postscript filename)
  (shell-command
   (format "abc2ps %s -e %s -o -O %s"
           (shell-quote-argument buffer-file-name)
           (or (shell-quote-argument selectors) "")
           (shell-quote-argument filename))))

(defvar abc-last-made-postscript nil
  "Last postscript file created in this buffer.")
(make-variable-buffer-local 'abc-last-made-postscript)

(defun abc-view-postscript ()
  "View the last postscript file created for this buffer."
  (interactive)
  (if abc-last-made-postscript
      (shell-command
       (concat
        (read-string
         "Shell command: "
         (format "gv %s" (shell-quote-argument
                          abc-last-made-postscript)))
        " &"))
    (error "No postscript has been output for this buffer.")))

;;}}}
;;{{{ Transpose Tunes

(defun abc-transpose-tunes (arg semitones)
  "Transpose tunes in the current buffer using `abc2abc'.
Tune\(s) are transposed SEMITONES upward.  With no argument, transpose
the current tune.  With argument, transpose the entire buffer."
  (interactive "P\nnSemitones: ")
  (if arg
      ;; Transpose entire buffer
      (shell-command-on-region (point-min) (point-max)
                               (format "abc2abc - -t %s" semitones)
                               nil t)
    ;; Transpose current tune
    (mark-paragraph)
    (shell-command-on-region (region-beginning) (region-end)
                             (format "abc2abc - -t %s" semitones)
                             nil t)
    (deactivate-mark)))

;;}}}
;;{{{ Insert Tune Headers

(defvar abc-tune-types
  '(("reel" "4/4" "1/8")
    ("jig" "6/8" "1/8")
    ("slipjig" "9/8" "1/8")
    ("hornpipe" "4/4" "1/8")
    ("polka" "2/4" "1/8")
    ("waltz" "3/4" "1/4")
    )
  "The types of tunes that ABC Mode recognizes.
Elements look like \(TYPE METER LENGTH), where TYPE is the type's name
as a string, used for the `R' field, METER is its default meter, for
the `M' field, and LENGTH is its default note length, for the `L'
field.")

(defun abc-insert-header (type key title)
  "Insert a tune header at point.
TYPE should be a string such as `reel', `jig', `slipjig', etc.  See
`abc-tune-types' for valid values.  KEY should be an ABC-style key
signature, a string such as `D', `Gm', or `Amix'.  TITLE should be the
tune title, as a string."
  (interactive
   (list
    (completing-read "Tune Type (default jig): "
                     abc-tune-types nil t nil nil "jig")
    (read-string "Key (default D): " nil nil "D")
    (read-string "Title: ")))
  ;; Make sure we're separated from other tunes by 2 blank lines.
  (unless (eobp) (open-line 2))
  (unless (bobp) (newline 2))
  (delete-blank-lines)
  (unless (bobp) (newline 1))
  (unless (eobp) (open-line 1))
  ;; Get the tune info
  (let ((number (1+ (string-to-number
                     (or (abc-current-tune-field "X") "0"))))
        (data (assoc type abc-tune-types)))
    ;; Now insert the tune header
    (insert (format "X:%s\nT:%s\nM:%s\nL:%s\nR:%s\nZ:%s\nK:%s\n"
                    number title (cadr data) (caddr data) type
                    (user-full-name) key))))

;;}}}
;;{{{ Jump to Header/Body

(defvar abc-header-fields
  (nreverse (split-string "XTMLRABCDFGIOPQSZNWK" ""))
  "ABC tune header fields in reverse canonical order.")

(defun abc-jump-to-header (arg &optional field)
  "Jump to the current tune header FIELD, a one-character string.
Interactively, FIELD is specified by the last keypress.  If the field
is not present, insert it, unless ARG is negative, in which case
return nil.  Otherwise a true value is returned."
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    (upcase (char-to-string
             (event-basic-type
              (aref (this-command-keys)
                    (1- (length (this-command-keys)))))))))
  (let ((loc (point)))
    (forward-paragraph -1)
    (condition-case nil
        ;; Find the field if it already exists.
        (re-search-forward (format "^%s: *" field)
                           (save-excursion (forward-paragraph)
                                           (point)))
      (error
       (if (< arg 0)
           ;; If negative argument, don't insert; return nil.
           nil
         ;; Insert it in the conventional place.
         (cond
          ;; The first field goes first
          ((equal field (car (last abc-header-fields)))
           (insert (format "\n%s:" field)))
          ;; The last field goes last
          ((equal field (car abc-header-fields))
           (abc-jump-to-tune-body)
           (insert (format "%s:\n" field))
           (backward-char 1))
          ;; Otherwise, put it in order
          (t
           (goto-char loc)
           (let ((rest (cdr (member field abc-header-fields))))
             ;; Look for a header field that comes before it.
             (while (and rest (not (abc-jump-to-header -1 (car rest))))
               (setq rest (cdr rest))
               (goto-char loc))
             (if rest
                 ;; Found one!  Insert it here.
                 (progn
                   (forward-line 1)
                   (beginning-of-line)
                   (insert (format "%s:\n" field))
                   (backward-char 1))
               ;; Didn't find ANY!  So put this field first.
               (insert (format "\n%s:" field))))))
         t)))))

(defun abc-jump-to-tune-body ()
  "Jump to the start of the current actual tune."
  (interactive)
  (forward-paragraph -1)
  (forward-line 1)
  (beginning-of-line)
  (while (looking-at "[A-Z]:")
    (forward-line 1)))

;;}}}
;;{{{ Move Across Measures

(defun abc-forward-measure (arg)
  "Move forward one measure.  With ARG, do it that many times.
If ARG is less than zero, move backward."
  (interactive "p")
  (if (< arg 0)
      (re-search-forward abc-measure-separator nil nil arg)
    (re-search-backward abc-measure-separator nil nil (- 0 arg))))

(defun abc-backward-measure (arg)
  "Move backward one measure.  With ARG, do it that many times.
If ARG is less than zero, move forward."
  (interactive "p")
  (abc-forward-measure (- 0 arg)))

;;}}}
;;{{{ Go to a Specified Tune

(defun abc-goto-tune (tune)
  "Jump to a TUNE in the current file.
TUNE, prompted for interactively, may be a tune reference number or a
regular expression to search for in the title field \(T)."
  (interactive "sTune: ")
  (goto-char (point-min))
  (if (string-match "^[0-9]+$" tune)
      ;; It's a number
      (re-search-forward (format "^X: *%s" tune))
    ;; It's a regexp
    (re-search-forward (format "^T:.*%s" tune)))
  ;; Go to first line of the tune header
  (forward-paragraph -1)
  (forward-line 1))

;;}}}
;;{{{ Search ABC Files

(defalias 'abc-search #'abc2ps-search)

(defun abc2ps-search (&optional arg)
  "Search ABC file(s) for tunes.
By default, calls `abc2ps' to do the searching.  The user can edit the
command line that is provided by default and should supply an
appropriate tune selector at the end of it.  See abc2ps(1) for more
information on tune selectors.  Briefly, to find a tune with the
string `Ireland' in its title, the expression after the `-e' flag
should be `*Ireland*'.  Spaces, unfortunately, split selectors, so to
search for `Two Words', try `*Two*Words*'.  Case matters.

Without ARG, the default command line searches the current ABC file.
With ARG of 4+ (one `\\[universal-argument]'), it searches all ABC files in the current
directory.  With ARG of 16+ (two `\\[universal-argument]'s), it recurses subdirectories as
well.  When the current file is not in ABC Mode, no ARG searches the
current directory and an ARG of 4+ recurses subdirectories.

The output buffer is in Compilation Mode with some extra key bindings:

\\{abc2ps-minor-mode-map}\\<compilation-mode-map>\\[compile-goto-error]		Go to ABC source of current tune.
\\[compile-mouse-goto-error]		Go to ABC source of selected tune.
"
  (interactive "p")
  (or arg (setq arg 1))
  (or (eq major-mode 'abc-mode)
      (setq arg (* arg 4)))
  (require 'compile)
  (let ((buffer
         (compile-internal
          (read-string
           "Command: "
           (cond
            ((>= arg 16)
             "find . -iname '*.abc' -print0 | xargs -0 -e sh -c 'abc2ps -f $* -e '")
            ((>= arg 4)
             "abc2ps -f *.abc -e ")
            (t
             (format "abc2ps -f %s -e " (shell-quote-argument
                                         buffer-file-name)))))
          "No more matching tunes"
          "abc2ps"
          nil                   ; No custom parsing function
          abc2ps-match-regexp-alist
          nil                   ; Buffer is named *abc2ps*
          nil                   ; No enter-directory
          nil                   ; or leave-directory
          abc2ps-match-file-alist
          )))
    (save-excursion
      (set-buffer buffer)
      ;; This hook interface is also apparently undocumented.
      (make-local-hook 'compilation-filter-hook)
      (add-hook 'compilation-filter-hook #'abc2ps-filter-search nil t)
      (setq buffer-read-only t)
      ;; Get our keymap
      (setq abc2ps-minor-mode t)
      )))

(defvar abc2ps-filtered-pos nil)
(make-variable-buffer-local 'abc2ps-filtered-pos)

(defun abc2ps-filter-search ()
  "Remove files without any matching tunes."
  (save-match-data
    (save-excursion
      (goto-char (or abc2ps-filtered-pos (point-min)))
      (while (re-search-forward "^\\(\\S-.+:\n\\)\\S-" nil t)
        (replace-match "" nil nil nil 1))
      (setq abc2ps-filtered-pos (point-marker)))))

(defvar abc2ps-match-regexp-alist
  '((" +\\*?\\([0-9]+\\)" nil abc2ps-find-match))
  "Regexps to match a tune found by abc2ps.")

(defvar abc2ps-match-file-alist '(("\\(\\S-.+\\):" 1))
  "Regexps to match when abc2ps starts searching a new file.")

(defun abc2ps-find-match (filename column)
  "Given the match data for a tune, return an error descriptor.
This function is for the back-door interface to `compile-internal'
that is not \(well?) documented, wherein a function can be called to
compute the entries in `compilation-error-list' rather than simply
returning the line number as a matched subexpression.  We do this
because abc2ps outputs tune numbers rather than line numbers."
  ;; The call to this function is surrounded in `save-excursion'.
  (list (point-marker)
        filename
        (let ((tune (match-string 1)))
          (save-match-data
            (set-buffer (find-file-noselect (car filename)))
            (goto-char (point-min))
            (re-search-forward (format "^X: *%s" tune) nil t)
            (count-lines (point-min) (point))))
        nil))


;; This isn't a real minor mode, just a way to insinuate our keymap.
(defvar abc2ps-minor-mode nil)
(make-variable-buffer-local 'abc2ps-minor-mode)
(defvar abc2ps-minor-mode-map (make-sparse-keymap))
(mapc #'(lambda (pair)
          (define-key abc2ps-minor-mode-map (car pair) (cadr pair)))
      '(("o" abc2ps-omit-tune)
        ("m" abc2ps-mark-tune)
        ("u" abc2ps-unmark-tune)
        ("x" abc2ps-postscript-tune)
        ("a" abc2ps-postscript-all)
        ("v" abc-view-postscript)
        ("p" abc2ps-previous-tune)
        ("n" abc2ps-next-tune)
        ("q" bury-buffer)       ; No work?
        ))
(add-to-list 'minor-mode-map-alist
             (cons 'abc2ps-minor-mode abc2ps-minor-mode-map))

; (defun abc2ps-next-tune (arg)
;   "Move to the next displayed tune.
; With ARG, do it that many times.  With ARG < 0, move to previous
; tune\(s)."
;   (interactive "p")
;   (beginning-of-line)
;   (cond ((> arg 0)
;          (or (search-forward "\n " nil t arg)
;              (error "No more displayed tunes")))
;         ((< arg 0)
;          (or (re-search-backward "^ ." nil t (- 0 arg))
;              (error "No more displayed tunes"))))
;   (beginning-of-line))

; (defun abc2ps-previous-tune (arg)
;   "Move to the previous displayed tune.
; With ARG, do it that many times.  With ARG < 0, move to following
; tune\(s)."
;   (interactive "p")
;   (abc2ps-next-tune (- 0 arg)))

(defalias 'abc2ps-next-tune 'next-line)
(defalias 'abc2ps-previous-tune 'previous-line)

(defun abc2ps-omit-tune ()
  "Omit the tune on the current line.
If the current line is a file, omit that entire file."
  (interactive)
  (beginning-of-line)
  (labels ((delline ()
                    (delete-region
                     (point) (progn (forward-line 1) (point)))))
    (let ((inhibit-read-only t))
      (cond ((looking-at " ")
             ;; Single tune
             (delline)
             (and (looking-at "\\S-")
                  (progn (forward-line -1) (looking-at "\\S-"))
                  ;; Last tune in a file, so remove the file.
                  (delline)))
            ((looking-at "\\S-.+:$")
             ;; File line, so delete every tune in that file.
             (delline)
             (while (looking-at " ")
               (delline)))))))

(defvar abc2ps-mark-count 0)

(defun abc2ps-mark-tune (&optional arg)
  "Mark \(or unmark) the tune on the current line.
If ARG is negative, unmark rather than mark.  If the current line is a
file, operate on all displayed tunes in that file."
  (interactive "p")
  (save-excursion
    (let ((inhibit-read-only t)
          (inschar (if (and arg (< arg 0)) ?\ ?*)))
      (beginning-of-line)
      (cond ((looking-at " ")
             ;; Single tune
             (forward-char 1)
             (unless (eql (char-after (point)) inschar)
               (delete-char 1)
               (insert inschar)
               (incf abc2ps-mark-count
                     (if (eql inschar ?*) 1 -1))))
            ((looking-at "\\S-.+:$")
             ;; File
             (forward-line 1)
             (while (looking-at " ")
               (abc2ps-mark-tune arg)
               (forward-line 1)))))))

(defun abc2ps-unmark-tune ()
  "Unmark the tune on the current line.
If the current line is a file, unmark all tunes in that file."
  (interactive)
  (abc2ps-mark-tune -1))

(defvar abc2ps-tune-name nil
  "Title of the first tune selected in the current command.")
(make-variable-buffer-local 'abc2ps-tune-name)

(defun abc2ps-get-file-selectors (&optional mark)
  "Get the selector for tunes under the file on the current line.
If MARK is a marker, get it for only the tune with that marker at the
beginning of the line.  Otherwise, if MARK is non-nil, get it for only
the marked tunes, and if MARK is nil, get it for all tunes.  If no
tunes match, return the empty string.  If current line is not a file
line, return nil.  Leaves point at beginning of the next file line.

In addition, if `abc2ps-tune-name' is nil, set it to the title of the
first tune encountered."
  (beginning-of-line)
  (when (looking-at "^\\(\\S-.+\\):$")
    (let ((file (match-string 1)) (tunes ""))
      (forward-line 1)
      (beginning-of-line)
      (while (looking-at " ")
        (when (cond
               ((markerp mark) (= mark (point)))
               (mark (looking-at " \\*"))
               (t t))
          (looking-at " +\\*?\\([0-9]+\\)")
          (setq tunes (concat tunes "," (match-string 1)))
          (unless abc2ps-tune-name
            (looking-at "................... *\\(.+\\)$")
            (setq abc2ps-tune-name (match-string 1))))
        (forward-line 1))
      ;; If we found any tunes under this file, return the selector.
      (if (string= tunes "")
          ""
        (format " -f %s -e %s"
                (shell-quote-argument file)
                ;; Delete initial comma
                (substring tunes 1))))))

(defun abc2ps-postscript-tune ()
  "Make postscript output FILENAME of one or more matching tunes.
If one or more tunes are marked, make output of them.  Otherwise, if
the current line is a tune, make output of it, but if the current line
is a file, make output of all displayed tunes in that file."
  (interactive)
  (save-excursion
    (let ((tunes "") abc2ps-tune-name)
      (cond
       ((> abc2ps-mark-count 0)
        ;; There are marked tunes
        (save-excursion
          (goto-char (point-min))
          ;; Go to first file
          (re-search-forward "^\\S-.+:$" nil t)
          (let ((selectors (abc2ps-get-file-selectors t)))
            (while selectors
              (setq tunes (concat tunes selectors)
                    selectors (abc2ps-get-file-selectors t))))))
       ((progn (beginning-of-line) (looking-at " "))
        ;; On a tune line
        (setq tunes
              (abc2ps-get-file-selectors
               (prog1 (point-marker)
                 (re-search-backward "^\\S-.+:$")))))
       (t
        ;; Must be on a file line
        (setq tunes (abc2ps-get-file-selectors nil))))
    (setq abc2ps-tune-name (abc-title-to-filename abc2ps-tune-name))
    (shell-command
     (format "abc2ps %s -o -O %s"
             tunes
             (shell-quote-argument
              (setq abc-last-made-postscript
                    (expand-file-name
                     (read-file-name
                      (format "Output File (default %s): "
                              abc2ps-tune-name)
                      nil abc2ps-tune-name)))))))))

(defun abc2ps-postscript-all ()
  "Make postscript output of all displayed tunes."
  (interactive)
  (let ((tunes "") abc2ps-tune-name)
    (save-excursion
      (goto-char (point-min))
      ;; Go to first file
      (re-search-forward "^\\S-.+:$" nil t)
      (let ((selectors (abc2ps-get-file-selectors nil)))
        (while selectors
          (setq tunes (concat tunes selectors)
                selectors (abc2ps-get-file-selectors nil)))))
    (setq abc2ps-tune-name (abc-title-to-filename abc2ps-tune-name))
    (shell-command
     (format "abc2ps %s -o -O %s"
             tunes
             (shell-quote-argument
              (setq abc-last-made-postscript
                    (expand-file-name
                     (read-file-name
                      (format "Output File (default %s): "
                              abc2ps-tune-name)
                      nil abc2ps-tune-name))))))))

;;}}}

(provide 'abc-mode)

;;; abc-mode.el ends here

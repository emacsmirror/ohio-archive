;;; ansi-color.el -- translate ANSI into text-properties

;; Copyright (C) 1999, 2000  Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 2.3.0
;; Keywords: comm processes

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file provides a function that takes a string containing ANSI
;; escape sequences and tries to replace these with text-properties.
;;
;; I was unable to extract this functionality from term.el for another
;; program I wanted to extend (the MUSH client TinyTalk.el), so I had to
;; rewrite this.

;; The basic functions are:
;;
;; `ansi-color-apply' to colorize a string containing ANSI escape
;; sequences.
;;
;; `ansi-color-filter-apply' to filter ANSI escape sequences from a
;; string.
;;
;; `ansi-color-apply-on-region' to colorize a region containing ANSI
;; escape sequences.
;;
;; `ansi-color-filter-region' to filter ANSI escape sequences from a
;; region.

;; Instead of defining lots of new faces, this package uses
;; text-properties as described in the elisp manual
;; *Note (elisp)Special Properties::.

;;; Testing:

;; If you want to test the setup, evaluate the following fragment in a
;; buffer without font-lock-mode.  This doesn't work in buffers that
;; have font-lock-mode!
;;
;; (insert (ansi-color-apply "\033[1mbold\033[0m and \033[34mblue\033[0m, \033[1m\033[34mbold and blue\033[0m!!"))

;; Usage with TinyMush.el:

;; In order to install this with TinyMush.el, add the following to your
;; .emacs file:
;;
;; (setq tinymud-filter-line-hook 'my-ansi-color-filter)
;; (autoload 'ansi-color-apply "ansi-color" 
;;   "Translates ANSI color escape sequences into text-properties." t)
;; (defun my-ansi-color-filter (conn line)
;;   "Call `ansi-color-apply' and then processes things like `filter-line'."
;;   (setq line (ansi-color-apply line))
;;   (if (not (get-value conn 'trigger-disable))
;;       (progn
;; 	(check-triggers conn line
;; 			(get-value conn 'triggers))
;; 	(check-triggers conn line
;; 			(get-value (get-value conn 'world) 'triggers))
;; 	(check-triggers conn line
;; 			tinymud-global-triggers)))
;;   (display-line conn line)
;;   t)

;; Usage with shell-mode:

;; In order to enjoy the marvels of "ls --color=tty" you will have to
;; enter shell-mode using M-x shell, possibly disable font-lock-mode
;; using M-: (font-lock-mode 0), and add ansi-color-apply to
;; comint-preoutput-filter-functions using M-: (add-hook
;; 'comint-preoutput-filter-functions 'ansi-color-apply).

;;; Thanks

;; Georges Brun-Cottan <gbruncot@emc.com> for improving ansi-color.el
;; substantially by adding the code needed to cope with arbitrary chunks
;; of output and the filter functions.



;;; Code:

;; Customization

(defgroup ansi-colors nil
  "Translating ANSI escape sequences to text-properties.
This translation effectively colorizes strings and regions based upon
ANSI escape sequences embedded in the text."
  :version "20.7"
  :group 'processes)

(defcustom ansi-color-faces-vector
  [default bold  default default underline bold  default modeline]
  "Faces used for ANSI escape sequences determining a face.

ANSI escape sequences are sequences like this one: \033[1m, where 1
could be one of the following numbers: 0 (default), 1 (hilight, rendered
as bold), 4 (underline), 5 (flashing, rendered as bold), 7 (inverse,
rendered the same as the modeline).

This vector is used by `ansi-color-make-color-map' to create a color
map.  This color map is stored in the variable `ansi-color-map'."
  :type '(vector face face face face face face face face)
  :set 'ansi-color-map-update
  :initialize 'custom-initialize-default
  :group 'ansi-colors)

(defcustom ansi-color-names-vector
  ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"]
  "Colors used for ANSI escape sequences determining a face.

Used for sequences like this one: \033[31m, where 1 could be an index to a
foreground color (red, in this case), or \033[41m, where 1 could be an
index to a background color.

The default colors are: black, red, green, yellow, blue, magenta,
cyan, and white.

On a light background, I prefer: black, red, dark green, orange, blue,
magenta, turquoise, snow4

This vector is used by `ansi-color-make-color-map' to create a color
map.  This color map is stored in the variable `ansi-color-map'."
  :type '(vector string string string string string string string string)
  :set 'ansi-color-map-update
  :initialize 'custom-initialize-default
  :group 'ansi-colors)

(defconst ansi-color-regexp "\033\\[\\([0-9;]*\\)m")

;; Main functions


(defun ansi-color-filter-apply (s)
  "Filter out all escape ANSI sequences from S.

This function can be added to `comint-preoutput-filter-functions'."
  (while (string-match ansi-color-regexp s)
    (setq s (replace-match "" t t s)))
  s)


(defun ansi-color-filter-region (begin end)
  "Filter out all ANSI color escape sequences from region START END.

Returns the first point it is safe to start with.  Used to speedup
further processing.

Design to cope with arbitrary chunk of output such as the ones get by
comint-output-filter-functions, e.g.:

\(defvar last-context nil)
\(make-variable-buffer-local 'last-context)

\(defun filter-out-color-in-buffer (s)
  \(setq last-context
        \(ansi-color-filter-region
         \(if last-context
             last-context
           \(if (marker-position comint-last-output-start)
               \(marker-position comint-last-output-start)
             1))
         \(marker-position (process-mark (get-buffer-process (current-buffer)))) ))
  s)

\(add-hook 'comint-output-filter-functions 'filter-out-color-in-buffer)
"
  (let ((endm (copy-marker end)))
    (save-excursion
      (goto-char begin)
      (while (re-search-forward ansi-color-regexp endm t)
        (replace-match ""))
      (if (re-search-forward "\033" endm t)
          (match-beginning 0)
        (marker-position endm)))))


(defun ansi-color-apply (string)
  "Translates ANSI color escape sequences into text-properties.

Applies ANSI escape sequences setting foreground and background colors
to STRING and returns the result.  The colors used are given in
`ansi-color-faces-vector' and `ansi-color-names-vector'.

This function can be added to `comint-preoutput-filter-functions'."
  (let ((face)
	(start 0) (end) (escape)
	(result)
	(params))
    ;; find the next escape sequence
    (while (setq end (string-match ansi-color-regexp string start))
      ;; store escape sequence
      (setq escape (match-string 0 string))
      ;; colorize the old block from start to end using old face
      (if face
	  (put-text-property start end 'face face string))
      (setq result (concat result (substring string start end)))
      ;; create new face by applying all the parameters in the escape sequence
      (let ((i 0))
	(while (setq i (string-match "[013457][01234567]?[;m]" escape i))
	  (setq face (ansi-color-make-face face
					   (aref escape i)
					   (aref escape (1+ i))))
	  (setq i (match-end 0))))
      (setq start (+ end (length escape))))
    (concat result (substring string start))))


(defun ansi-color-apply-on-region (begin end &optional context)
  "Translates ANSI color escape sequences into text-properties.

Applies ANSI escape sequences setting foreground and background colors
to text in region. The colors used are given in
`ansi-color-faces-vector' and `ansi-color-names-vector'.
Returns a context than can be used to speedup further processing.
Context is a (begin (start . face)) list.

Design to cope with arbitrary chunk of output such as the ones get by
comint-output-filter-functions, e.g.:

\(defvar last-context nil)
\(make-variable-buffer-local 'last-context)

\(defun ansi-output-filter (s)
  \(setq last-context
        \(ansi-color-apply-on-region
         \(if last-context
             \(car last-context)
           \(if (marker-position comint-last-output-start)
               \(marker-position comint-last-output-start)
             1))
         \(process-mark (get-buffer-process (current-buffer)))
         last-context ))
  s)

\(add-hook 'comint-output-filter-functions 'ansi-output-filter)
"
  (let ((endm (copy-marker end))
        (face (if (and context (cdr context))
                  (cdr (cdr context))))
	(face-start (if (and context (cdr context))
                        (car (cdr context))))
        (next-safe-start begin)
        escape-sequence
        null-sequence
        stop )
    (save-excursion
      (goto-char begin)
      ;; find the next escape sequence
      (while (setq stop (re-search-forward ansi-color-regexp endm t))
        ;; store escape sequence
        (setq escape-sequence (match-string 1))
        (setq null-sequence (string-equal (match-string 1) ""))
        (setq next-safe-start (match-beginning 0))
        (if face
            (put-text-property face-start next-safe-start 'face face)) ; colorize
        (replace-match "")              ; delete the ANSI sequence
        (if null-sequence
            (setq face nil)
          (setq face-start next-safe-start)
          (setq face (ansi-color-get-face escape-sequence))))
      (setq next-safe-start
            (if (re-search-forward "\033" endm t)
                (match-beginning 0)
              (marker-position endm))))
    (cons next-safe-start
          (if face
              (cons face-start face))) ))

;; Helper functions

(defun ansi-color-make-color-map ()
  "Creates a vector of face definitions and returns it.

The index into the vector is an ANSI code.  See the documentation of
`ansi-color-map' for an example.

The face definitions are based upon the variables
`ansi-color-faces-vector' and `ansi-color-names-vector'."
  (let ((ansi-color-map (make-vector 50 nil))
        (index 0))
    ;; miscellaneous attributes
    (mapcar
     (function (lambda (e)
                 (aset ansi-color-map index e)
                 (setq index (1+ index)) ))
     ansi-color-faces-vector)

    ;; foreground attributes
    (setq index 30)
    (mapcar
     (function (lambda (e)
                 (aset ansi-color-map index
                       (cons 'foreground-color e))
                 (setq index (1+ index)) ))
     ansi-color-names-vector)

    ;; background attributes
    (setq index 40)
    (mapcar
     (function (lambda (e)
                 (aset ansi-color-map index
                       (cons 'background-color e))
                 (setq index (1+ index)) ))
     ansi-color-names-vector)
    ansi-color-map))

(defvar ansi-color-map (ansi-color-make-color-map)
  "A brand new color map suitable for ansi-color-get-face.

The value of this variable is usually constructed by
`ansi-color-make-color-map'.  The values in the array are such that the
numbers included in an ANSI escape sequence point to the correct
foreground or background colors.

Example: The sequence \033[34m specifies a blue foreground.  Therefore:
     (aref ansi-color-map 34)
          => \(foreground-color . \"blue\")")

(defun ansi-color-map-update (symbol value)
  "Update `ansi-color-map'.

Whenever the vectors used to construct `ansi-color-map' are changed,
this function is called.  Therefore this function is listed as the :set
property of `ansi-color-faces-vector' and `ansi-color-names-vector'."
  (set-default symbol value)
  (message "YOW")
  (setq ansi-color-map (ansi-color-make-color-map)))

(defun ansi-color-get-face-1 (ansi-code)
  "Get face definition from `ansi-color-map'.
ANSI-CODE is used as an index into the vector."
  (aref ansi-color-map ansi-code))

(defun ansi-color-get-face (escape-seq)
  "Create a new face by applying all the parameters in ESCAPE-SEQ.

ESCAPE-SEQ is an ANSI escape sequence such as \033[34m.  The ANSI code
34 is used by `ansi-color-get-face-1' to return a face definition."
  (let ((ansi-color-r "[013457][01234567]?")
        (i 0)
        f)
    (while (string-match ansi-color-r escape-seq i)
      (setq i (match-end 0))
      (add-to-list 'f
                   (ansi-color-get-face-1
                    (string-to-int (match-string 0 escape-seq) 10))))
    f))

(defun ansi-color-make-face (face param1 param2)
  "Return a face based on FACE and characters PARAM1 and PARAM2.

The face can be used in a call to `add-text-properties'.  The PARAM1 and
PARAM2 characters are the two numeric characters in ANSI escape
sequences between ?[ and ?m.  Unless the ANSI escape sequence specifies
a return to default face using PARAM1 ?0 and PARAM2 ?m (ie. \"\033[0m\"), the
properties specified by PARAM1 and PARAM2 are added to face."
  (cond ((= param1 ?0)
	 nil)
	((= param2 ?m)
	 (add-to-list 'face (aref ansi-color-faces-vector
				  (string-to-number (char-to-string param1)))))
	((= param1 ?3)
	 (add-to-list 'face (cons 'foreground-color
				  (aref ansi-color-names-vector
					(string-to-number (char-to-string param2))))))
	((= param1 ?4)
	 (add-to-list 'face (cons 'background-color
				  (aref ansi-color-names-vector
					(string-to-number (char-to-string param2))))))
	(t (add-to-list 'face (aref ansi-color-faces-vector
				  (string-to-number (char-to-string param1)))))))

(provide 'ansi-color)

;;; ansi-color.el ends here

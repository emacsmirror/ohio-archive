;;; find-func.el --- find function definitions from calls
;;
;; Copyright (C) 1994 Christian Moen
;;
;; Author: Christian Moen <christim@ifi.uio.no>
;; Created: 15 Jul 1994
;; Version: 1.0
;; Keywords: find function
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; LCD Archive Entry:
;; find-func|Christian Moen|christim@ifi.uio.no|
;; Find function definitions from calls|
;; 30-Jul-1994|1.0|~/misc/find-func.el.Z|
;;

;;; Purpose:
;;
;; This package provides an easy way to find function definitions
;; from corresponding function calls. Several languages are supported
;; and new ones are easily added. The ability to backtrack previous
;; function definition searches is included as well.
;;

;;; Installation:
;;
;; To use this package, be sure to have find-func.el in your emacs
;; lisp load path. If you put find-func.el in a directory called
;; ~/elisp, add the following form to your ~/.emacs:
;;
;; (setq load-path (append (cons (expand-file-name "~/elisp") load-path)))
;;
;; You need to add these forms to as well:
;;
;; (require 'find-func)
;; (define-key global-map "\C-cf" 'ffu-find-func)
;; (define-key global-map "\C-cb" 'ffu-backtrack)
;;
;; Alternatively:
;;
;; (autoload 'ffu-find-func "find-file" "Find function definition." t)
;; (autoload 'ffu-backtrack "find-file" "Backtrack to previous call." t)
;; (define-key global-map "\C-cf" 'ffu-find-func)
;; (define-key global-map "\C-cb" 'ffu-backtrack)
;;
;; Bind the keys to whatever suits you best.
;;

;;; History:
;;
;; - 1.0:
;;   Initial release.
;;

;;; Todo:
;;
;; - Apply a better backtracking strategy if the buffer backtraced
;;   to has changed.
;;
;; - Add find-func functionality to buffers not easily matched by
;;   BUF-REGEXP in `ffu-search-alist', like for various scripts.
;;
;; - If needed, support separate functions to parse grammars, like
;;   using major-mode's beginning-of-defun etc. in a search scheme.
;;
;; - Include `ffu-call-search-lines' in `ffu-search-alist'?
;;
;; - Support more languages
;;

;;; Feedback:
;;
;; Please send me bug reports, bug fixes, suggestions, extensions,
;; new or improved regular expressions, etc. Thanks.
;;

;;; Code:

(defvar ffu-verbose t
  "*If nil, prevent search status from being displayed.")

(defvar ffu-recenter t
  "*If non-nil, recenter point if function definition is found.")

(defvar ffu-call-search-lines 3
  "Number of lines after point to be searched for a function call.")

(defvar ffu-path nil
  "Used for backtracking. If a function definition is found,
buffer-name, function-name, `point' and `point-max' of the
current buffer is appended to this list.")

(defvar ffu-search-alist
  '(
    ("\\.pl$"  . ("[-A-Za-z][-A-Za-z0-9]* *"
		  "sub +"
		  "[-A-Za-z][-A-Za-z0-9]* *"
		  nil))
    ("\\.tcl$" . ("[-A-Za-z][-A-Za-z0-9]* *"
		  "proc +"
		  "[-A-Za-z][-A-Za-z0-9]* *"
		  nil))
    ("\\.sim$" . ("[-A-Za-z][-A-Za-z0-9]* *"
		  "\\(PROCEDURE\\|Procedure\\|procedure\\) +"
		  "[-A-Za-z][-A-Za-z0-9]* *"
		  nil))
    ("\\.el$\\|\\.l$\\|\\.lsp$\\|\\.lisp$\\|\\.ml$" .
                 ("[-A-Za-z][-A-Za-z0-9]* *"
		  "(defun +"
		  "[-A-Za-z][-A-Za-z0-9]* *"
		  nil))
    ("\\.c$\\|\\.C$\\|\\.CC$\\|\\.cc$\\|\\.cxx$\\|\\.cpp$" .
                 ("[A-Za-z_][A-Za-z0-9_]* *(\\(.\\|\n\\)*) *"
		  " *([]A-Za-z0-9_*, []*) *\n+ *{"
		  "[A-Za-z_][A-Za-z0-9_]*"
		  t))
    )
  "*Association list of buffer-name patterns vs. corresponding regular
expressions to match when searching for function definitions. Each
element has the form:

   (BUF-REGEXP . (FCALL-REGEXP FDEF-REGEXP NAMEX-REGEXP NAMEF))

When find-func searches for a function definition from an associated
function call, the buffer-name of the current buffer is matched
againt every BUF-REGEXP in the alist. If a match is found, the
associated FCALL-REGEXP is used to find a function call within
`ffu-call-search-lines' lines of point. If a match occurs, the
NAMEX-REGEXP is used to retrieve the function name. All buffers
whose names are matched by BUF-REGEXP are then matched againt
the concatenated regular expression of FDEF-REGEXP and the function
name matched by NAMEX-REGEXP. The order of the concatenation is
defined by NAMEF; if nil concat FDEF-REGEXP first.

See `ffu-call-search' and `ffu-def-search' for additional information.")

(make-variable-buffer-local 'ffu-path)

(defun ffu-find-func ()
  "Find a function definition from a function call."
  (interactive)
  (let ((search ffu-search-alist))
    (while search
      (if (string-match (nth 0 (car search)) (buffer-name))
	  (let* ((buf-regexp   (nth 0 (car search)))
		 (call-regexp  (nth 1 (car search)))
		 (def-regexp   (nth 2 (car search)))
		 (namex-regexp (nth 3 (car search)))
		 (name-first   (nth 4 (car search)))
		 (func-name (ffu-call-search call-regexp)))
	    (if func-name
		(progn
		  (string-match namex-regexp func-name)
		  (ffu-def-search buf-regexp def-regexp
				  (substring func-name
					     (match-beginning 0) (match-end 0))
				  name-first))
	      (ffu-message "No function call found."))))
      (setq search (cdr search)))))

(defun ffu-call-search (call-regexp)
  "Search for a function call.

The number of lines after point denoted by `ffu-call-search-lines',
is searched for a function call by matching the CALL-REGEXP regular
expression. If a match is found, return matching buffer substring.
Otherwise, return nil.

Arguments: (call-regexp)"
  (save-excursion
    (let ((begp (point)))
      (forward-line ffu-call-search-lines)
      (end-of-line)
      (let ((endp (point)))
	(goto-char begp)
	(if (re-search-forward call-regexp endp t)
	    (buffer-substring (match-beginning 0) (match-end 0)))))))

(defun ffu-def-search (buf-regexp def-regexp func-name name-first)
  "Search for a function definition.

Search buffer names matched by BUF-REGEXP for a function definition.
The buffers are searched for the concatenated regular expression of
DEF-REGEXP and FUNC-NAME. The order of the concatenation is defined
by NAME-FIRST.

If a function definition is found, switch to this buffer and move
point to the beginning of the definition.

Arguments: (buf-regexp def-regexp func-name name-first)"
  (let ((found nil)
	(buffer (buffer-name))
	(buffers (ffu-make-bufferlist buf-regexp))
	(oldp (point))
	(maxp (point-max)))
    (while buffers
      (set-buffer (car buffers))
      (let ((bufp (point)))
	(goto-char (point-min))
	(let ((defpos (re-search-forward (if name-first
					     (concat func-name def-regexp)
					   (concat def-regexp func-name))
					 (point-max) t)))
	  (if (not defpos)
	      (goto-char bufp)
	    (goto-char (match-beginning 0))
	    (switch-to-buffer (car buffers))
	    (if ffu-recenter
		(recenter))
	    (ffu-message (concat "Function definition found: " func-name))
	    (ffu-set-path buf-regexp
			  (append (list buffer func-name oldp maxp) ffu-path))
	    (setq buffers nil
		  found t))))
      (setq buffers (cdr buffers)))
    (if (not found)
	(ffu-message (concat "Function definition NOT not found: " func-name)))
    ))

(defun ffu-backtrack ()
  "Backtrack to previous function call.

Use the information in `ffu-path' and try to backtrack to the
previous function call. If the buffer size has changed since
`ffu-def-search' was called for this buffer, backtracking may
not be accurate. If the buffer was killed, backtracking isn't
possible.

If backtracking is possible, switch to appropriate buffer and
move point to function call."
  ;; fixme -- Apply better backtracking strategy if buffer changed
  (interactive)
  (let ((search ffu-search-alist))
    (while search
      (if (string-match (nth 0 (car search)) (buffer-name))
	  (if ffu-path
	      (let ((buffer (nth 0 ffu-path))
		    (func   (nth 1 ffu-path))
		    (pos    (nth 2 ffu-path))
		    (maxp   (nth 3 ffu-path)))
		(ffu-set-path (nth 0 (car search)) (nthcdr 4 ffu-path))
		(set-buffer buffer)
		(goto-char pos)
		(switch-to-buffer buffer)
		(if ffu-recenter
		    (recenter))
		(ffu-message (concat
			      (if (= (point-max) maxp)
				  "Backtracked to function call: "
				"Buffer has changed. Trying to backtrack: ")
			      func)))
	    (ffu-message "Unable to backtrack at the top level.")))
      (setq search (cdr search)))))

(defun ffu-set-path (name-regexp value)
  "Set buffer local variable `ffu-path' to VALUE in all buffers
matched by NAME-REGEXP.

Arguments: (name-regexp value)"
  (let ((buffer  (buffer-name))
	(buffers (ffu-make-bufferlist name-regexp)))
    (while buffers
      (set-buffer (car buffers))
      (setq ffu-path value
	    buffers (cdr buffers)))
    (set-buffer buffer)))

(defun ffu-make-bufferlist (name-regexp)
  "Return a list of buffer-names matched by NAME-REGEXP.

Arguments: (name-regexp)"
  (let ((match-buffers (list))
	(buffers (mapcar (function buffer-name) (buffer-list))))
    (while buffers
      (if (string-match name-regexp (car buffers))
	  (setq match-buffers (append (cons (car buffers) match-buffers))))
      (setq buffers (cdr buffers)))
    match-buffers))

(defun ffu-message (message-text)
  "Print MESSAGE-TEXT if `ffu-verbose' is non-nil.

Arguments (message-text)"
  (if ffu-verbose
      (message message-text)))

(provide 'find-func)

;;; find-func.el ends here

;; tex-complete.el

(defconst tex-complete-version
  "$Id: tex-complete.el,v 1.3 90/06/19 18:32:09 sk Exp $"
  "The RCS id of this version of tex-complete.")

(defun tex-insert-plain-command (CMD)
  "Insert a plain TeX command CMD with completing read, 
using the list in the variable tex-names."
  (interactive
   (progn
     (require 'tex-names)
     (list (completing-read
	    "Name of (plain) TeX command: " ; PROMPT
	    tex-names ; the TABLE
	    nil		; PREDICATE (nil: use whole TABLE)
	    nil		; REQUIRE-MATCH: no
	    nil		; no INITIAL-INPUT
	    ))))
  (insert  "\\" CMD))

(defun tex-insert-latex-command (CMD)
  "Insert a LaTeX command CMD with completing read,
using the variable latex-commands."
  (interactive
   (progn
     (require 'latex-names)
     (list (completing-read
	    "Name of LaTeX command: " ; PROMPT
	    latex-commands ; TABLE
	    nil		; PREDICATE (nil: use whole TABLE)
	    nil		; REQUIRE-MATCH
	    nil		; no INITIAL-INPUT
		      ))))
  (insert (concat "\\" CMD)))

;; this redefines cmutex.el's function
;; sk@sun4 14-Jun-1990 19:37
					
(defun latex-make-environment (name)
  "Make a latex NAME environment (begin and matching end).
NAME is read from the minibuffer with name completion,
using the list in the variable latex-all-environments."
  (interactive 
   (progn
     (require 'latex-names)
     (list (completing-read
	    "Name of LaTeX environment to make: " ; PROMPT
	    latex-all-environments ; TABLE
	    nil		; PREDICATE (nil: use whole TABLE)
	    nil		; REQUIRE-MATCH
	    nil		; no INITIAL-INPUT
	    ))))
  (insert "\\begin\{" name "}\n")
  (save-excursion (insert "\n\\end\{" name "}")))

(defun latex-insert-begin (environment)
  "Define a LaTeX environment, \\begin part only.
Later you can use \\[latex-close-block] to insert the matching \\end.

You are prompted for an environment, which is read from the minibuffer
with name completion, using the list in the variable latex-all-environments."
  (interactive 
   (progn
     (require 'latex-names)
     (list (completing-read
	    "Name of LaTeX environment to insert: " ; PROMPT
	    latex-all-environments	; TABLE
	    nil				; PREDICATE (nil: use whole TABLE)
	    nil				; REQUIRE-MATCH
	    nil				; no INITIAL-INPUT
	    ))))
  (insert "\\begin{" 
	  environment
	  "}\n"))

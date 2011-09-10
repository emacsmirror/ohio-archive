;; @(#) csh-mode2.el -- Editing tcsh, csh files

;; LCD Archive Entry:
;; csh-mode2|Jari Aalto|jaalto@tre.tele.nokia.fi|
;; csh-mode , Editing tcsh, csh files |
;; 25-May-1995|1.0|~/modes/csh-mode2.el.Z|


;;; Commentary:

;; ..................................................... &t-commentary ...

;; Original mail from the Author of csh-mode.el
;;
;; From: strong+@CMU.EDU (Thomas W. Strong, Jr.)
;;       mf4m+ts49@andrew.cmu.edu
;; Newsgroups: gnu.emacs.sources
;; Subject: csh-mode.el
;; Date: 15 Nov 92 18:29:54 GMT
;; Article-I.D.: andrew.of1mD2a00Vpd88mzs7
;;
;; Similar to the last one...  once again, comments and suggestions are
;; welcome.
;;
;; hacked out of forth.el from the tile-forth package

;; FOREWORD
;; ========================================
;; May 25 1995 [jaalto]
;; - Well, I should say something about this version 2 I uploaded to
;;   ohio. When I first loaded the csh-mode.el I was thrilled, but it
;;   soon turned out that it had very serious flaws. Beeing a novice
;;   lisp programmer back then I tried to correct some of the most
;;   worst things that kept me from using this mode: it simply didn't
;;   indent the statements right.
;; - Now I'm uploading this version 2 so that whoever uses the csh,
;;   may benefit changes I made long time ago. If you find any bugs or
;;   serious flaws from this second version, you can try dropping me
;;   mail. I don't promise to correct them, since I don't use this
;;   mode anymore, but because I hold myself responsible for the
;;   changes I made, I will try to correct them.
;; - I hereby declare that any serious CSH user who can program lisp
;;   can immediately take over this package and change the LCD entry
;;   to reflaect his name and contact address. Also any new uploads
;;   can be made with the same same name "csh-mode2", please let's not
;;   clutter ohio with "csh-mode3, csh-mode4.." files.
;; 
;; - The whole package should be rewritten by someone, because what I
;;   remember it was very difficult to correct or add features to this
;;   el.


;; CHANGE HISTORY
;; ........................................................ &t-history ...
;; [jaalto]	  Jari Aalto, <jaalto@tre.tele.nokia.fi>
;;
;; Feb 24. 1994 [jaalto]
;; 1) semicolons were not counted properly, so indent became to
;;    go wrong if line KEYWORD ended up with no space or ";"
;;    --> Now accepts semicolons at the front/beg of KEYWORD
;;    --> controlled by csh-skip-...  variables
;; 2) Comment handling. Keywords are not looked after the comment
;;    marker. This allows visual labeling of code:
;;    "end # ------------------ foreach section end -----------"
;; 3) Added handling of goto keyword. Special indentation
;;;
;; Jan 11 1995 [jaalto] Note
;; - When I programmed heavily in csh I used this mode all the time,
;;   and I think it was adequate.
;; - Since the I moved permanently to perl and very seldom do csh or sh
;;   stuff anymore. I also learned to program lisp and I eventually coded
;;   general block editing .el called tinyindent.el , which replaced
;;   all modes I used: csh, perl, awk, sh, ksh  even C/C++ if I had not
;;   them around.
;; - I hope someone will enjoy these changes I made. I don't plan to return
;;   this code any more. I think the whole indentation algorithm and
;;   code structure should be changed totally by someone...
;;   Just keep the ball rolling!


;;; Code:

;;; ....................................................... &variables ...

(defvar csh-indent-level 2
  "*Indentation of csh statements.")

(defvar csh-indent-level-goto 4
  "*Indentation of csh GOTO statements.")

(defvar csh-indent-level-exec csh-indent-level
  "Tells ++ or -- indent level. Set by functions to appropriate value.")

(defvar csh-goto-rex-beg "^[ \t]*[a-zA-Z_0-9]+:"
  "Pattern that is used for locating beginning of goto-label.")

(defvar csh-goto-rex-end "#.*__gend"
  "Patern that maches goto-end. In csh there is no command for
this, so suggest that you set up this variable by hand.
Normally this KEYWORD is placed into comment.")


(defvar csh-skip-charset-f " \t\n;"          ; forward
  "(MULTI LINE) This tells which characters are not significant when
determining words. In csh the ; is special terminator, so we ignore it too.
")

(defvar csh-skip-charset-f2 " \t;"          ; forward
  "(LINE) This tells which characters are not significant when determining
words. In csh the ; is special terminator, so we ignore it too.")


(defvar csh-skip-charset-nf "^ \t\n;"          ; (not "charset")
  "This tells which characters are not significant when determining
words. In csh the ; is special terminator, so we ignore it too.")


(defvar csh-mode-positives
  " foreach while else then switch default: case "
  "Contains all words which will cause the indent-level to be incremented
on the next line.
OBS! All words in csh-mode-positives must be surrounded by spaces.")

(defvar csh-mode-negatives
  " endif else breaksw endsw end "
  "Contains all words which will cause the indent-level to be decremented
on the current line.
OBS! All words in csh-mode-negatives must be surrounded by spaces.")

(defvar csh-mode-zeroes
  " #!/bin/csh "
  "Contains all words which causes the indent to go to zero")

(defvar csh-mode-abbrev-table nil
  "Abbrev table in use in csh-mode buffers.")

(define-abbrev-table 'csh-mode-abbrev-table ())

(defvar csh-mode-map nil
  "Keymap used in csh mode.")

(if (not csh-mode-map)
    (setq csh-mode-map (make-sparse-keymap)))

(define-key csh-mode-map "\t" 'csh-indent-command)
(define-key csh-mode-map "\C-m" 'reindent-then-newline-and-indent)

(defvar csh-mode-syntax-table nil
  "Syntax table in use in csh-mode buffers.")

(defun csh-mode-variables ()
  (setq local-abbrev-table csh-mode-abbrev-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'csh-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t))


;;; ----------------------------------------------------------------------
;;; The main program
;;;
(defun csh-mode ()
  "Major mode for editing csh scripts.
\\[csh-indent-command] properly indents subexpressions of multi-line
if, while, foreach, and switch statements, taking nesting into account.

Caveats:
------------------
The file must start with '#!/bin/csh' for the indentation to start properly.
Extra spaces should be inserted to make sure the indentation algorithm can
figure out what is a keyword, string, etc.  For example, write
    if ($foo == \"bar\")
not
    if($foo==\"bar\")
or later lines may get indented wrong.  (Many lines like this are also
illegal csh code, so this shouldn't cramp your style.)

Notes [jaalto]
------------------
- In csh there is no end-pair for goto label, so I had to supply my own.
- I really *perefer* goto to be intended too, because it so special case.

Goto label indentation is controlled by three variables.
	csh-indent-level-goto
	csh-goto-rex-beg
	csh-goto-rex-end    <-- change this for your own goto-end match

Now you can write code like this:
EXIT_OK:
   echo ok.
   exit 0	# __gend    <-- see csh-goto-rex-end

EXIT_BAD:
   echo 'yup?'
   exit 1	# __gend


KNOWN LIMITATIONS
--------------------
- The line parser routine isn't very smart. Comments are interpreted
  loosely. Taking care of all kind of quotings would be pain.
- Incidently this is not a big issue, because the line is searched
  for keywords only to ++ or -- the indentation. BUT BEWARE.

grep '#' file  # blaa-blaa-blaa
      ^
      This is interpreted as starting comment
      So forget the rest of line for keywords.

case here:
     grep '#' file; breaksw
           ^
           see? a big NO-NO. put break on separate line!!

TIPS
--------------------
- To indent whole code, issue C-h M-C-\

The variable csh-indent-level controls the amount of indentation.
\\{csh-mode-map}"
  (interactive)               ; ------------------ END DOC ----------------
  (kill-all-local-variables)
  (use-local-map csh-mode-map)
  (setq mode-name "csh")
  (setq major-mode 'csh-mode)
  (csh-mode-variables)
  (run-hooks 'csh-mode-hook))



;;; ----------------------------------------------------------------------
;;;
(defun csh-current-indentation ()
  (save-excursion
    (beginning-of-line)
    (back-to-indentation)
    (current-column)))

;;; ----------------------------------------------------------------------
;;;
(defun csh-delete-indentation ()
  (let
      ((b nil)
       (m nil))
    (save-excursion
      (beginning-of-line)
      (setq b (point))
      (back-to-indentation)
      (setq m (point)))
    (delete-region b m)))

;;; ----------------------------------------------------------------------
;;;
(defun csh-indent-line (&optional flag)
  "Correct indentation of the current csh line."
  (let
      ((x (csh-calculate-indent)))
    (csh-indent-to x)))

(defun csh-indent-command ()
  (interactive)
  (csh-indent-line t))

;;; ----------------------------------------------------------------------
;;;
(defun csh-indent-to (x)
  (let
      ((p nil))
    (setq p (- (current-column) (csh-current-indentation)))
    (csh-delete-indentation)
    (beginning-of-line)
    (indent-to x)
    (if (> p 0) (forward-char p))))



;;; ----------------------------------------------------------------------
;;; Calculate indent
;;; Putl all cases , keyword checkings here, which
;;; require next line to be UNindented
;;;
;;;
(defun csh-calculate-indent ()
  (let ((w1 nil)
	(indent 0)
	(centre 0))
    (save-excursion
      (beginning-of-line)
      (skip-chars-backward csh-skip-charset-f )   ; Go to previous line
      (beginning-of-line)
      (back-to-indentation)
      (setq indent (current-column))
      (setq centre indent)
      (setq indent (+ indent (csh-sum-line-indentation)))
;      (jmsg  (concat "INDENT NOW " (int-to-string indent)) 1)

      )

    ;;  Search for first word on the line
    ;;
    (save-excursion
      (beginning-of-line)
      (back-to-indentation)
      (let ((p (point)))
	(skip-chars-forward csh-skip-charset-nf )  ; "^ \t\n"
	(setq w1 (buffer-substring p (point)))))

    ;;  How the indent was ?
    (setq differ (- indent centre))

;;;    (jmsg  (concat " indent centre diff ex "
;;;	     (int-to-string indent) " "
;;;	     (int-to-string centre) " "
;;;	     (int-to-string differ) " "
;;;	     (int-to-string csh-indent-level-exec) " "
;;;	    "w1 // " w1) 1)



    ;;  Now determine how much to indent -- or ++
    ;;  This is set by (csh-sum-line-indentation)
    ;;
    (if (> differ 0) 	(setq indent (+ centre csh-indent-level-exec)))
    (if (< differ 0) 	(setq indent (- centre csh-indent-level-exec)))
    (if (< indent 0)    (setq indent 0))


    ;; now scan for every NEG keywoards, if hit then decrease
    ;;
    (setq match (string-match (regexp-quote (concat " " w1 ))
			      csh-mode-negatives))
    (setq minus (if (eq match nil) 0 csh-indent-level-exec))
    (setq indent (- indent minus))


    ;; check for goto label indent , Special case
    ;;
    (if (eq 1 (csh-is-end-gotolabel))
	(progn
	  (setq indent (- indent csh-indent-level-goto))))


    ;; Do we have ZERO condition ?
    ;;
    (if (string-match (regexp-quote (concat " " w1 " ")) csh-mode-zeroes)
	(setq indent 0))

    (if (< indent 0) (setq indent 0))

    indent))


;;; ----------------------------------------------------------------------
;;; Putl allcases , keyword checkings here, which
;;; require next line to be indented.
;;;
(defun csh-sum-line-indentation ()
  "Add up the positive and negative weights of all words on the current line."
  (let ((b (point))
	(e nil)
	(sum 0)
	(w nil)
	(t1 nil)
	(t2 nil)
	(first t)
	)
    (end-of-line) (setq e (point))
    (setq com-pos (csh-find-pos))
    (if (not (eq com-pos nil))        ; there is # comment on the line
	(progn
	  (setq e (+ b com-pos))             ; read till comment
	  ))

    (goto-char b)
    (while (< (point) e)
      (setq w (csh-next-word))
      (setq rex_plus  (regexp-quote (concat " " w " ")));
      (setq rex_minus (regexp-quote (concat " " w " ")))
      (setq t1 (string-match rex_plus  csh-mode-positives))
      (setq t2 (string-match rex_minus csh-mode-negatives))


      (if (and t1 t2)
	  (setq sum (+ sum csh-indent-level)))
      (if t1
	  (setq sum (+ sum csh-indent-level)))
      (if (and t2 (not first))
	  (setq sum (- sum csh-indent-level)))
      (skip-chars-forward csh-skip-charset-f2 )  ;  " \t"
      (setq first nil)) ; --while-----------------------------------

    ;;  set normal indent
    (setq csh-indent-level-exec csh-indent-level)

    ;; check special keyword GOTO , force next line indent
    ;;
    (setq special-indent (csh-is-beg-gotolabel))
    (if (eq 1 special-indent)
	(progn                     ; set indent level
	  (setq csh-indent-level-exec csh-indent-level-goto)
	  (setq sum (+ sum csh-indent-level-goto))))

;    (jmsg  (concat "SUM " (int-to-string sum)) 1)
    sum))



;;; ----------------------------------------------------------------------
;;;
(defun csh-next-word ()
  "Return the next csh-word. Skip anything enclosed in double quotes."
  (let ((w1 nil))
    (while (not w1)
      (skip-chars-forward csh-skip-charset-f )   ; " \t\n"
      (if (not (looking-at "\""))
	  (let ((p (point)))
	    (skip-chars-forward csh-skip-charset-nf ) ; "^ \t\n"
	    (setq w1 (buffer-substring p (point))))
	(skip-chars-forward "^\"")   ; "^\""
	(forward-char 1)))
    w1))



;;; ----------------------------------------------------------------------
;;; csh-is-com
;;; - Return 1 if line is plain comment
;;;
(defun csh-is-com ()
  (setq pos (csh-find-pos))
  
  (cond
   ;; Does the comment start from pos 0 ??
   ((eq pos 0)
    1)
   ;; No, partial comment or no comment at all
   (t 0))
  )



;;; ----------------------------------------------------------------------
;;; - Checks if line has goto label "  GO_HERE:"
;;;
(defun csh-is-end-gotolabel ()
  (setq pos (csh-find-pos 0 csh-goto-rex-end))
  (cond
   ;; match?
   ( (not (eq pos nil))
     1)
   ;; No
   (t 0))
  )    






;;; ----------------------------------------------------------------------
;;   Checks if line has comment and returns it's position
;;   rex  = matching regxp
;;   NOTE: position returned is "first match" or nil if no match
;;
;;   in csh:
;;   if ( $#variable )  # comment here  <-- see [^$] in rex
;;
;;
;;   mode: 0  return buffer  beg-match
;;         1  return buffer  end-match
;;         10 return line    beg-match
;;         11 return line    end-match

(defun csh-find-pos (&optional mode rex)
  (save-excursion
    (if (equal nil rex)		; set default values
	(setq rex "[ \t]*[^$]#"))
    (setq str                   ; read line
	  (buffer-substring (setq BUF (progn (beginning-of-line) (point)))
			    (progn (end-of-line) (point))))
    (setq pos (string-match rex str))
    (if (eq nil pos) 		; nothing matched
	nil
      (progn
	(setq MB (match-beginning 0))
	(setq ME (match-end 0))

	(cond
	 ;; Which values to return, according to mode
	 ((eq mode 0)
	  (+ BUF MB))
	 ((eq mode 1)
	  (+ BUF ME))
	 ;; ----------------- Now line positi<ons
	 ((eq mode 10)
	  MB)
	 ((eq mode 11)
	  ME)
	 ;;  Default mode here , match beginning
	 ((eq mode nil)
	  MB))))
  ))

;;; ----------------------------------------------------------------------
;; reads current line
;; D = debug only
;;
(defun Dread-line ()
  (save-excursion
   (setq str                   ; read line
	  (buffer-substring (setq BUF (progn (beginning-of-line) (point)))
			    (progn (end-of-line) (point))))
  ))


;;; ............................................................... &end ...

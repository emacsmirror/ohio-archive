;Article 517 of comp.emacs
;Path: ark1!uakari.primate.wisc.edu!ginosko!gem.mps.ohio-state.edu!tut.cis.ohio-state.edu!ucbvax!ucdavis!ccs015
;From: ccs015@mr-ranger.ucdavis.edu (Kambiz Aghaiepour)
;Newsgroups: comp.emacs
;Subject: Re: Pascal mode
;Message-ID: <CCS015.89Sep29192649@mr-ranger.ucdavis.edu>
;Date: 30 Sep 89 02:26:49 GMT
;References: <45ee7b24.1032a@hi-csc.UUCP>
;Sender: uucp@ucdavis.ucdavis.edu
;Reply-To: ccs015@bullwinkle.ucdavis.edu
;Distribution: na
;Organization: University of California, Davis
;Lines: 381
;In-Reply-To: foslien@hi-csc.UUCP's message of 29 Sep 89 17:59:00 GMT

; Pascal editing support package
; Author Mick Jordan for Modula-2
; amended Peter Robinson
; ported to GNU Michael Schmidt <michael@pbinfo.uucp>
; Modified by tom Perrine <Perrin@LOGICON.ARPA> (TEP)
; analogue for pascal by Vincent Broman <broman@bugs.nosc.mil>

(setq auto-mode-alist (cons (cons "\\.p$" 'pascal-mode) auto-mode-alist))
(setq auto-mode-alist (cons (cons "\\.h$" 'pascal-mode) auto-mode-alist))

;;; Added by TEP
(defvar pascal-mode-syntax-table nil
  "Syntax table in use in Pascal-mode buffers.")

(if pascal-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "." table)
    (modify-syntax-entry ?\{ "<" table)
    (modify-syntax-entry ?\} ">" table)
    (modify-syntax-entry ?\( "()1" table)
    (modify-syntax-entry ?\) ")(4" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?\& "." table)
    (modify-syntax-entry ?\| "." table)
    (modify-syntax-entry ?\$ "_" table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    (setq pascal-mode-syntax-table table)))

;;; Added by TEP
(defvar pascal-mode-map nil
  "Keymap used in Pascal mode.")

(if pascal-mode-map ()
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-i" 'pascal-tab)
    (define-key map "\C-m" 'pascal-newline)
    (define-key map "\C-cb" 'pascal-begin)
    (define-key map "\C-cc" 'pascal-case)
    (define-key map "\C-c\C-c" 'pascal-const)
    (define-key map "\C-ce" 'pascal-else)
    (define-key map "\C-cf" 'pascal-for)
    (define-key map "\C-c\C-f" 'pascal-function)
    (define-key map "\C-ch" 'pascal-header)
    (define-key map "\C-ci" 'pascal-if)
    (define-key map "\C-c\C-i" 'pascal-include)
    (define-key map "\C-c\C-p" 'pascal-procedure)
    (define-key map "\C-cp" 'pascal-program)
    (define-key map "\C-cr" 'pascal-repeat)
    (define-key map "\C-c\C-r" 'pascal-record)
    (define-key map "\C-c\C-t" 'pascal-type)
    (define-key map "\C-c\C-v" 'pascal-var)
    (define-key map "\C-cw" 'pascal-while)
    (define-key map "\C-c\C-w" 'pascal-with)
    (define-key map "\C-c*" 'pascal-star-display-comment)
    (define-key map "\C-c{" 'pascal-display-comment)
    (define-key map "\C-c}" 'pascal-inline-comment)
    (define-key map "\C-c(" 'pascal-paired-parens)
    (define-key map "\C-c[" 'pascal-paired-brackets)
    (define-key map "\C-ct" 'pascal-toggle)
    (define-key map "\C-cL" 'pascal-link)
    (define-key map "\C-cC" 'pascal-compile)
    (setq pascal-mode-map map)))

(defvar pascal-indent 4 "*This variable gives the indentation in Pascal-Mode")
  
(defun pascal-mode ()
"This is a mode intended to support program development in Pascal.
Most control constructs of Pascal can be created by typing
Control-C followed by the first character of the construct.

C-c p    program        C-c b    begin-end
C-c C-c  const          C-c c    case-do
C-c C-t  type           C-c t    toggle between .p-.h
C-c C-v  var            C-c {    enter matched braces
C-c C-r  record         C-c r    repeat-until
C-c C-w  with-do        C-c w    while-do
C-c C-i  #include       C-c i    if-then
C-c C-p  procedure      C-c e    else
C-c C-f  function       C-c f    for-do

\\{pascal-mode-map}

variable pascal-indent controls the number of spaces for each indentation."
  (interactive)
  (kill-all-local-variables)
  (use-local-map pascal-mode-map)
  (setq major-mode 'pascal-mode)
  (setq mode-name "Pascal")
  (make-local-variable 'comment-column)
  (setq comment-column 41)
  (make-local-variable 'end-comment-column)
  (setq end-comment-column 72)
  (set-syntax-table pascal-mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
;  (make-local-variable 'indent-line-function)
;  (setq indent-line-function 'c-indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "{\n")
  (make-local-variable 'comment-end)
  (setq comment-end "\n}")
  (make-local-variable 'comment-column)
  (setq comment-column 41)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'c-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (run-hooks 'pascal-mode-hook))

(defun pascal-newline ()
  "Start new line and indent to current tab stop."
  (interactive)
  (setq cc (current-indentation))
  (newline)
  (indent-to cc)
  )

(defun pascal-tab ()
  "Indent to next tab stop."
  (interactive)
  (indent-to (* (1+ (/ (current-indentation) pascal-indent)) pascal-indent)))

(defun pascal-begin ()
  "Insert a begin-end pair and indent for the line between."
  (interactive)
  (insert "begin")
  (pascal-newline)
  (pascal-newline)
  (insert "end;")
  (let ((comment (read-string "comment about block: ")))
    (cond ((not (string-equal comment "")) (insert " {" comment "}"))))
  (end-of-line 0))

(defun pascal-case ()
  "Build skeleton case statment, prompting for the <expression>."
  (interactive)
  (insert "case ")
  (let ((selector (read-string "selector-expr: ")))
    (progn
      (insert selector " of")
      (pascal-newline)
      (pascal-newline)
      (insert "end; {case " selector "}")))
  (end-of-line 0)
  (pascal-tab))

(defun pascal-else ()
  "Insert else keyword and indent for next line."
  (interactive)
  (insert "else")
  (pascal-newline)
  (pascal-tab))

(defun pascal-for ()
  "Build skeleton for loop statment, prompting for the loop parameters."
  (interactive)
  (insert "for ")
  (insert (read-string "init: ") " to ")
  (insert (read-string "limit: ") " do")
  (pascal-newline)
  (pascal-tab))

(defun pascal-header ()
  "Insert a comment block containing the module title, author, etc."
  (interactive)
  (insert "(*\n    Title: \t")
  (insert (read-string "Title: "))
  (insert "\n    Created:\t")
  (insert (current-time-string))
  (insert "\n    Author: \t")
  (insert (user-full-name))
  (insert (concat "\n\t\t<" (user-login-name) "@" (system-name) ">\n"))
  (insert "*)\n\n"))

(defun pascal-if ()
  "Insert skeleton if statment, prompting for a boolean-expression."
  (interactive)
  (insert "if ")
  (insert (read-string "condition: ") " then")
  (pascal-newline)
  (pascal-tab))

(defun pascal-program ()
  (interactive)
  (insert "program ")
  (let ((name (read-string "Program name: " )))
    (insert name " (input, output")
    (let ((arglist (read-string "other file vars: ")))
      (cond ((not (string-equal "" arglist)) (insert ", " arglist)))
      (insert ");"))
    (pascal-newline)
    (pascal-newline)
    (insert "begin")
    (pascal-newline)
    (pascal-newline)
    (insert "end. {")
    (insert name)
    (insert "}")
    (end-of-line 0)
    (pascal-tab)))

(defun pascal-procedure ()
  (interactive)
  (insert "procedure ")
  (let ((name (read-string "Name: " )))
    (insert name "(")
    (insert (read-string "argument list: ") ");")
    (pascal-newline)
    (pascal-newline)
    (pascal-tab)
    (insert "begin")
    (pascal-newline)
    (pascal-newline)
    (insert "end; {")
    (insert name)
    (insert "}")
    (end-of-line 0)
    (pascal-tab)))

(defun pascal-function ()
  (interactive)
  (insert "function ")
  (let ((name (read-string "name: " )))
    (insert name "(")
    (insert (read-string "argument list: ") "): ")
    (insert (read-string "result type: ") ";")
    (pascal-newline)
    (pascal-newline)
    (pascal-tab)
    (insert "begin")
    (pascal-newline)
    (pascal-newline)
    (insert "end; {")
    (insert name)
    (insert "}")
    (end-of-line 0)
    (pascal-tab)))

(defun pascal-with ()
  (interactive)
  (insert "with ")
  (insert (read-string "idents: "))
  (insert " do")
  (pascal-newline)
  (pascal-tab))

(defun pascal-record ()
  (interactive)
  (insert "record")
  (pascal-newline)
  (pascal-newline)
  (insert "end;")
  (let ((comment (read-string "comment about record: ")))
    (cond ((not (string-equal comment "")) (insert " {" comment "}"))))
  (end-of-line 0)
  (pascal-tab))

(defun pascal-type ()
  (interactive)
  (insert "type")
  (pascal-newline)
  (pascal-tab))

(defun pascal-const ()
  (interactive)
  (insert "const")
  (pascal-newline)
  (pascal-tab))

(defun pascal-repeat ()
  (interactive)
  (insert "repeat")
  (pascal-newline)
  (pascal-newline)
  (insert "until ")
  (insert (read-string "exit cond: ") ";")
  (end-of-line 0)
  (pascal-tab))

(defun pascal-var ()
  (interactive)
  (insert "var")
  (pascal-newline)
  (pascal-tab))

(defun pascal-while ()
  (interactive)
  (insert "while ")
  (insert (read-string "entry cond: "))
  (insert " do")
  (pascal-newline)
  (pascal-tab))

(defun pascal-include ()
  (interactive)
  (insert "\n#include \"")
  (insert (read-string "header file: "))
  (insert "\"")
  (pascal-newline)
  (pascal-newline))


(defun pascal-paired-parens ()
  (interactive)
  (insert "()")
  (backward-char))


(defun pascal-paired-brackets ()
  (interactive)
  (insert "[]")
  (backward-char))

(defun pascal-inline-comment ()
  (interactive)
  (insert "{}")
  (backward-char))

(defun pascal-display-comment ()
"puts comment delimiters around a blank line, making a display comment."
  (interactive)
  (insert "{\n\n}")
  (end-of-line 0))

(defun pascal-star-display-comment ()
"starts a UNIX-style display comment."
  (interactive)
  (insert "(*\n *\n *)")
  (end-of-line 0)
  (pascal-tab))

(defun pascal-compile ()
  (interactive)
  (setq modulename (buffer-name))
  (compile (concat "pc -c " modulename)))

(defun pascal-link ()
  (interactive)
  (compile "make"))

;UNUSED?
;(defun execute-monitor-command (command)
;  (let* ((shell shell-file-name)
;	 (csh (equal (file-name-nondirectory shell) "csh")))
;    (call-process shell nil t t "-cf" (concat "exec " command))))

(defun pascal-toggle ()
  "toggle between .p and .h files for the module."
  (interactive)
  (cond ((string-equal (substring (buffer-name) -2) ".h")
	 (find-file-other-window
	   (concat (substring (buffer-name) 0 -2) ".p")))
	((string-equal (substring (buffer-name) -2) ".p")
	 (find-file-other-window
	   (concat (substring (buffer-name) 0 -2)  ".h")))))



(put 'eval-expression 'disabled nil)
;--
;Kambiz Aghaiepour                   Internet :  ccs015@bullwinkle.ucdavis.edu
;Computing Services  -*-                 UUCP :  ucdavis!bullwinkle!ccs015, or
;University of California, Davis                 ucdavis!kaghaiepour
;Work : (916) 752-3994                 BITNET :  kaghaiepour@ucdavis.edu  

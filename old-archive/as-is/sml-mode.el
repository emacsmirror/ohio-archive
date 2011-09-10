;To: unix-emacs@bbn.com
;Date: 25 Jan 89 20:36:26 GMT
;From: Lars Bo Nielsen <mailrus!sharkey!atanasoff!deimos!ksuvax1!lbn@bbn.com>
;Subject: Major mode for editing and running (Standard) ML. Version 2.0.
;
;Here is the newest version of sml-mode, a major mode for editing and
;running (Standard) ML.
;Version 1.0 that was send to these newsgroups in December 88.
;
;Please let me know if you come up with any ideas, bugs, or fixes.
;
; Lars Bo Nielsen          | UUCP: ..!rutgers!ksuvax1!lbn
; lbn@ksuvax1.cis.ksu.edu  |   or: ..!{pyramid,ucsd}!ncr-sd!ncrwic!ksuvax1!lbn
;
;-----cut-----cut-----cut-----cut-----cut-----cut-----cut-----cut-----cut-----
;; sml-mode.el. Major mode for editing (Standard) ML.
;; Copyright (C) 1989, Free Software Foundation, Inc. and Lars Bo Nielsen. 

;; This file is not officially part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SML Mode. A major mode for editing and running SML. (Version 2.0)
;; =================================================================
;;
;; This is a mode for editing and running (Standard) ML (sml). It
;; features:
;;
;;      - Automatic indentation of sml code. With a number of
;;        constants to customize the indentation.
;;
;;      - Short cut insertion of commonly used structures, like
;;        "let in end", "signature xxx = sig end", etc.
;;
;;      - Short cut insertion of "|". Will automaticly determine if
;;        "|" is used after a "case", "fun" etc. If used after a "fun"
;;        automaticly insert the name of the function, used after
;;        case automaticly insert "=>" etc.
;;
;;      - Inferior shell running sml. No need to leave emacs, just
;;        keep right on editing while sml runs in another window.
;;
;;      - Automatic "use file" in inferior shell. Send regions of code
;;        to the sml program.
;;
;;      - Parsing of errors in the inferior shell. Like the next-error
;;        function used in c-mode (This will only work with SML of
;;        New Jersey).
;;
;;
;; 1. HOW TO USE THE SML-MODE
;; ==========================
;;
;; Here is a short introduction to the mode.
;;
;; 1.1 GETTING STARTED
;; -------------------
;;
;; If you are an experienced user of Emacs, just skip this section.
;;
;; To use the sml-mode, insert this in your "~/.emacs" file (Or ask your
;; emacs-administrator to help you.):
;;
;;    (setq auto-mode-alist (cons '("\\.sml$" . sml-mode) 
;;                           auto-mode-alist)) 
;;    (autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
;;
;; If you are using other extensions, like ".sig" for your files
;; containing signatures, just add one more line saying:
;;
;;    (setq auto-mode-alist (cons '("\\.sig$" . sml-mode) 
;;                           auto-mode-alist)) 
;;
;; Now every time a file with the extension `.sml' is found, it is
;; automatically started up in sml-mode.
;;
;; You will also have to specify the path to this file, so you will have
;; to add this as well:
;;
;;    (setq load-path (cons "/usr/me/emacs" load-path))
;;
;; where "/usr/me/emacs" is the directory where this fill is.
;;
;; You may also want to compile the this file (M-x byte-compile-file)
;; for speed.
;;
;; You are now ready to start using sml-mode. If you have tried other
;; language modes (like lisp-mode or C-mode), you should have no
;; problems. There are only a few extra functions in this mode.
;;
;; 1.2. EDITING COMMANDS.
;; ----------------------
;;
;; The following editing and inferior-shell commands can ONLY be issued
;; from within a buffer in sml-mode.
;;
;; M-| (sml-electric-pipe). 
;;     In the case you are typing in a case expression, a function with
;;     patterns etc., this function will give you some help. First of all
;;     it  will put the `|' on a line by itself. If it is used in the
;;     definition of a function it inserts the functions name, else it
;;     inserts `=>'. Just try it, you will like it.
;;
;; LFD (reindent-then-newline-and-indent).  
;;     This is probably the function you will be using the most (press
;;     CTRL while you press Return, press C-j or press Newline). It
;;     will reindent the line, then make a new line and perform a new
;;     indentation.
;;
;; TAB (sml-indent-line).  
;;     This function indents the current line.
;;
;; C-c TAB (sml-indent-region).
;;     This function indents the current region (be patient if the
;;     region is large).
;;
;; M-; (indent-for-comment).
;;     Like in other language modes, this command will give you a comment
;;     at the of the current line. The column where the comment starts is
;;     determined by the variable comment-column (default: 40).
;;    
;; C-c C-v (sml-mode-version). 
;;     Get the version of the sml-mode.
;;
;;
;; 1.3. COMMANDS RELATED TO THE INFERIOR SHELL
;; -------------------------------------------
;;
;; C-c C-s (sml-pop-to-shell).
;;     This command starts up an inferior shell running sml. If the shell
;;     is running, it will just pop up the shell window.
;;
;; C-c C-u (sml-save-buffer-use-file).
;;     This command will save the current buffer and send a "use file",
;;     where file is the file visited by the current buffer, to the
;;     inferior shell running sml.
;;
;; C-c C-f (sml-run-on-file).
;;     Will send a "use file" to the inferior shell running sml,
;;     prompting you for the file name.
;;    
;; C-c ' (sml-next-error). 
;;     This function parses the buffer of the inferior shell running sml,
;;     and finds the errors one by one. The cursor is positioned at the
;;     line of the file indicated by the error message, while the
;;     inferior shell buffer running sml will be positioned at the error
;;     message in another window. 
;;
;;     IMPORTANT: This function only knows the syntax of error messages
;;     ********** produced by SML of New Jersey. To have this function
;;     working with other implementations of sml you will have to
;;     rewrite it.
;;
;; C-c C-r (sml-send-region). 
;;     Will send region, from point to mark, to the inferior shell
;;     running sml.
;;
;; C-c C-c (sml-send-function). 
;;     Will send function to the inferior shell running sml. (This only
;;     sends the paragraph, so you might prefer the sml-send-region
;;     instead. Paragraphs are separated by blank lines only).
;;
;; C-c C-b (sml-send-buffer). 
;;     Will send whole buffer to inferior shell running sml.
;;
;;
;; 2. INDENTATION
;; ==============
;;
;; The indentation algorithm has been the hardest one to implement.
;;
;;         What is the standard way of indenting sml-code?
;;
;; The algorithm has its own view of the right way to indent code,
;; according to the constants you can set to control the mode.
;;
;; 2.1. CONSTANTS CONTROLLING THE INDENTATION
;; ------------------------------------------
;;
;; sml-indent-level (default 4)
;;     Indentation of blocks in sml.
;;
;; sml-pipe-indent (default -2)
;;     The amount of extra indentation (negative) of lines beginning
;;     with "|". 
;;
;; sml-case-indent (default nil)
;;     How to indent case-of expressions.
;;
;;     If t:   case expr              If nil:   case expr of
;;               of exp1 => ...                     exp1 => ...
;;                | exp2 => ...                   | exp2 => ...
;;
;;     The first seems to be the standard in NJ-SML. The second is the
;;     default.
;;
;; sml-nested-if-indent (default nil)
;;     If set to t, nested if-then-else expressions will have the
;;     indentation:
;;
;;                if exp1 then exp2
;;                else if exp3 then exp4
;;                else if exp5 then exp6
;;                     else exp7
;;
;;     If nil, they will be indented as:
;;
;;                if exp1 then exp2
;;                else if exp3 then exp4
;;                     else if exp5 then exp6
;;                          else exp7
;;
;;     With the "else" at the same column as the matching "if".
;;
;; sml-type-of-indent (default t)
;;     Determines how to indent `let', `struct' etc.
;;     
;;     If t:      fun foo bar = let
;;                                  val p = 4
;;                              in
;;                                  bar + p
;;                              end
;;
;;     If nil:    fun foo bar = let
;;                    val p = 4
;;                in
;;                    bar + p
;;                end
;;
;;     Will not have any effect if the starting keyword is first on the
;;     line.
;;
;; sml-electric-semi-mode (default t)
;;     If t, a ";" will reindent the current line, and make a newline. To
;;     override this (to type in a ";" without a newline) just type: C-q ;
;;
;; sml-paren-lookback (default 200)
;;     Determines how far back (in chars) the indentation algorithm
;;     should look for open parenthesis. High value means slow
;;     indentation algorithm. A value of 200 (being the equivalent of
;;     4-6 lines) should suffice most uses. (A value of nil, means do
;;     not look at all).
;;
;; To change these constants, See CUSTOMIZING YOUR SML-MODE below.
;;
;;
;; 3. INFERIOR SHELL.
;; ==================
;;
;; The mode for Standard ML also contains a mode for an inferior shell
;; running sml. The mode is the same as the shell-mode, with just one
;; extra command.
;;
;; 3.1. INFERIOR SHELL COMMANDS
;; ----------------------------
;;
;; C-c C-f (sml-run-on-file).  Send a `use file' to the process running
;; sml.
;;
;; 3.2. CONSTANTS CONTROLLING THE INFERIOR SHELL MODE
;; --------------------------------------------------
;;
;; Because sml is called differently on various machines, and the
;; sml-systems have their own command for reading in a file, a set of
;; constants controls the behavior of the inferior shell running sml (to
;; change these constants: See CUSTOMIZING YOUR SML-MODE below).
;;
;; sml-prog-name (default "sml").
;;     This constant is a string, containing the command to invoke
;;     Standard ML on your system. 
;;
;; sml-use-right-delim (default "\"")
;; sml-use-left-delim  (default "\"")
;;     The left and right delimiter used by your version of sml, for
;;     `use file-name'.
;;
;; sml-process-name (default "SML"). 
;;     The name of the process running sml. (This will be the name
;;     appearing on the mode line of the buffer)
;;
;; NOTE: The sml-mode functions: sml-send-buffer, sml-send-function and
;; sml-send-region, creates temporary files (I could not figure out how
;; to send large amounts of data to a process). These files will be
;; removed when you leave emacs.
;;
;;
;; 4. CUSTOMIZING YOUR SML-MODE
;; ============================
;;
;; If you have to change some of the constants, you will have to add a
;; `hook' to the sml-mode. Insert this in your "~/.emacs" file.
;;
;;    (setq sml-mode-hook 'my-sml-constants)
;;
;; Your function "my-sml-constants" will then be executed every time
;; "sml-mode" is invoked.  Now you only have to write the emacs-lisp
;; function "my-sml-constants", and put it in your "~/.emacs" file.
;;
;; Say you are running a version of sml that uses the syntax `use
;; ["file"]', is invoked by the command "OurSML" and you don't want the
;; indentation algorithm to indent according to open parenthesis, your
;; function should look like this:
;;
;;    (defun my-sml-constants ()
;;       (setq sml-prog-name "OurSML")
;;       (setq sml-use-left-delim "[\"")
;;       (setq sml-use-right-delim "\"]")
;;       (setq sml-paren-lookback nil))
;;
;; Other things could be added to this function. As an example:
;;
;;       (setq sml-paren-lookback 1000) ; Look back 1000 chars for open
;;                                      ;  parenthesis.
;;       (setq sml-case-indent t)       ; Select other type of case
;;                                      ;  expressions 
;;       (turn-on-auto-fill)            ; Turn on auto-fill
;;       (setq sml-indent-level 8)      ; change the indentation to 8
;;
;;       (define-key sml-mode-map "|"   ; Let "|" be electric,
;;           'sml-electric-pipe)        ;  like M-|
;;
;; The sml-shell also runs a `hook' (sml-shell-hook) when it is invoked.
;;
;;
;; 5. CHANGES FROM PREVIOUS VERSIONS
;; ================================
;;
;; For those of you who have been using sml-mode in an earlier version,
;; here is a compiled list of the changes made.
;;
;; 5.1 VERSION 1.0 TO 2.0
;; ----------------------
;;
;;   o The indentation algorithm has become better at determining the
;;     right indentation, and faster.
;;
;;   o A number of constants have been introduced to customize the mode
;;     to your needs.
;;
;;   o The indentation algorithm recognizes `else if' (nested if then
;;     else), but only if the variable "sml-nested-if-indent" has been
;;     set to t, the default is nil.
;;
;;   o By setting the variable sml-case-indent to either nil or t you now
;;     can have two different indentations of case-expr-of.
;;
;;   o The constant sml-type-of-indent now determines the way to indent
;;     blocks, if the starting keyword is not first on the line.
;;
;;   o The indentation algorithm now indents to same level as an open
;;     parenthesis if this was left open at a previous line (the variable
;;     sml-paren-lookback controls this action.)
;;
;;   o A couple of extra functions have been added:
;;         sml-next-error and sml-save-buffer-use-file.
;;
;;   o If temporary files are created, these files will be removed when
;;     emacs is killed.
;;
;;   o Thanks to Andy Norman (ange%anorman@hplabs.hp.com) you are no
;;     longer placed in the inferior shell when you send a `use file' to
;;     the inferior shell. The shell window will pop up, and the window
;;     will scroll. He also added a shell-prompt-pattern variable for the
;;     inferior sml shell mode. This will allow you to move the cursor
;;     backwards in the buffer, and re-execute lines that begin with this
;;     pattern (pattern is found in the variable sml-shell-prompt-pattern).
;;
;;
;; 6. THINGS TO DO
;; ===============
;;
;; And there are still things to do:
;;
;;   o Find a better way to send regions to the inferior shell (the command
;;     process-send-region does not work on larger regions, that is why the
;;     function sml-simulate-send-region uses temporary files).
;;
;;   o Have sml-next-error recognizing more than just SML of New Jersey
;;     syntax of error messages.
;;
;;   o The indentation algorithm still can be fooled. I don't know if it
;;     will ever be 100% right, as this means it will have to actually
;;     parse all of the buffer up to the actual line (this can get -very-
;;     slow).
;;
;;   o Add tags.
;;
;;
;;
;; Thanks to Andy Norman (ange%anorman@hplabs.hp.com) for helping me a
;; lot. Not only did he tell me about the bugs, he also included fixes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; AUTHOR
;;         Lars Bo Nielsen
;;         Aalborg University
;;         Computer Science Dept.
;;         9000 Aalborg
;;         Denmark
;;
;;         lbn@iesd.dk
;;         or: ...!mcvax!diku!iesd!lbn
;;         or: mcvax!diku!iesd!lbn@uunet.uu.net
;;
;; NOTE:   Current email-address (until May 1. 1989):
;;
;;         lbn@ksuvax1.cis.ksu.edu
;;         (..!rutgers!ksuvax1!lbn
;;          or:   ..!{pyramid,ucsd}!ncr-sd!ncrwic!ksuvax1!lbn)
;;
;;
;; Please let me know if you come up with any ideas, bugs, or fixes.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sml-mode-version-string
  "SML-MODE, Version 2.0. Jan 25 1989 - Lars Bo Nielsen.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONSTANTS CONTROLLING THE MODE.
;;;
;;; These are the constants you might want to change
;;; 

;; The amount of indentation of blocks
(defconst sml-indent-level 4 "*Indentation of blocks in sml.")

;; The amount of negative indentation of lines beginning with "|"
(defconst sml-pipe-indent -2
  "*Extra (negative) indentation for lines beginning with |.") ;

;; How do we indent case-of expressions.
(defconst sml-case-indent nil
  "*How to indent case-of expressions.
  If t:   case expr              If nil:   case expr of
            of exp1 => ...                     exp1 => ...
             | exp2 => ...                   | exp2 => ...
\nThe first seems to be the standard in NJ-SML. The second is the default.")

(defconst sml-nested-if-indent nil
  "*If set to t, nested if-then-else expression will have the same
indentation as:
                 if exp1 then exp2
                 else if exp3 then exp4
                 else if exp5 then exp6
                      else exp7")

(defconst sml-type-of-indent t
  "*How to indent `let' `struct' etc.
If t:
          fun foo bar = let
                           val p = 4
                        in
                           bar + p
                        end
If nil:
          fun foo bar = let
              val p = 4
          in
              bar + p
          end
\nWill not have any effect if the starting keyword is first on the line.")

(defconst sml-electric-semi-mode t
  "*If t, a `\;' will insert itself, reindent the line, and perform a newline.
If nil, just insert a `\;'. (To insert while t, do: C-q \;).")

;; How far should the indentation algorithm look to find open parenthesis 
(defconst sml-paren-lookback 200
  "*Determines how far back (in chars) the indentation algorithm
should look for open parenthesis. High value means slow indentation
algorithm. A value of 200 (being the equivalent of 4-6 lines) should
suffice most uses. (A value of nil, means do not look at all)")

;; The command used to start up the sml-program.
(defconst sml-prog-name "sml" "*Name of program to run as sml.")

;; The left delimmitter for `use file'
(defconst sml-use-left-delim "\""
  "*The left delimiter for the filename when using \"use\".
 To be set to `[\\\"' for Edinburgh SML, and `\\\"' for New Jersey SML.
 Correspondes to `sml-use-right-delim'.")

;; The right delimmitter for `use file'
(defconst sml-use-right-delim "\""
  "*The right delimiter for the filename when using \"use\".
 To be set to `\\\"]' for Edinburgh SML, and `\\\"' for New Jersey SML.
 Correspondes to `sml-use-left-delim'.")

;; A regular expression matching the prompt pattern in the inferior
;; shell
(defconst sml-shell-prompt-pattern "^[^\-=]*[\-=] *"
  "*The prompt pattern for the inferion shell running sml.")

;; The template used for temporary files, created when a region is
;; send to the inferior process running sml.
(defconst sml-tmp-template "/tmp/sml.tmp."
  "*Template for the temporary file, created by sml-simulate-send-region.")

;; The name of the process running sml (This will also be the name of
;; the buffer).
(defconst sml-process-name "SML" "*The name of the SML-process")

;;;
;;; END OF CONSTANTS CONTROLLING THE MODE.
;;;
;;; If you change anything below, you are on your own.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar sml-mode-syntax-table nil "The syntax table used in sml-mode.")

(defvar sml-mode-map nil "The mode map used in sml-mode.")

(defun sml-mode ()
  "Major mode for editing SML code.
Tab indents for SML code.
Comments are delimited with (* ... *).
Paragraphs are separated by blank lines only.
Delete converts tabs to spaces as it moves back.

Key bindings:
=============

\\[sml-indent-line]\t  Indent current line.
\\[reindent-then-newline-and-indent]\t  Reindent line, newline and indent.
\\[sml-indent-region]\t  Indent region.
\\[sml-electric-pipe]\t  Insert a \"|\". Insert function name, \"=>\" etc.
\\[sml-region]\t  Insert a common used structure.
\\[sml-pop-to-shell]\t  Pop to the sml window.
\\[sml-next-error]\t  Find the next error.
\\[sml-save-buffer-use-file]\t  Save the buffer, and send a \"use file\".
\\[sml-send-region]\t  Send region (point and mark) to sml.
\\[sml-run-on-file]\t  Send a \"use file\" to sml.
\\[sml-send-function]\t  Send function to sml.
\\[sml-send-buffer]\t  Send whole buffer to sml.
\\[sml-mode-version]\t  Get the version of sml-mode


Variables controlling the indentation
=====================================

sml-indent-level (default 4)
    The indentation of a block of code.

sml-pipe-indent (default -2)
    Extra indentation of a line starting with \"|\".

sml-case-indent (default nil)
    Determine the way to indent case-of expression.
       If t:   case expr              If nil:   case expr of
                 of exp1 => ...                     exp1 => ...
                  | exp2 => ...                   | exp2 => ...

    The first seems to be the standard in NJ-SML. The second is the default.

sml-nested-if-indent (default nil)
    If set to t, nested if-then-else expression will have the same
    indentation as:
                     if exp1 then exp2
                     else if exp3 then exp4
                     else if exp5 then exp6
                          else exp7

sml-type-of-indent (default t)
    How to indent `let' `struct' etc.
    If t:
              fun foo bar = let
                               val p = 4
                            in
                               bar + p
                            end
    If nil:
              fun foo bar = let
                  val p = 4
              in
                  bar + p
              end
    Will not have any effect if the starting keyword is first on the line.

sml-electric-semi-mode (default t)
     If t, a `\;' will reindent line, and perform a newline.

Mode map
========
\\{sml-mode-map}
Runs sml-mode-hook if non nil."
  (interactive)
  (kill-all-local-variables)
  (if sml-mode-map
      ()
    (setq sml-mode-map (make-sparse-keymap))
    (define-key sml-mode-map "\C-c'" 'sml-next-error)
    (define-key sml-mode-map "\C-c\C-v" 'sml-mode-version)
    (define-key sml-mode-map "\C-c\C-u" 'sml-save-buffer-use-file)
    (define-key sml-mode-map "\C-c\C-s" 'sml-pop-to-shell)
    (define-key sml-mode-map "\C-c\C-r" 'sml-send-region)
    (define-key sml-mode-map "\C-c\C-m" 'sml-region)
    (define-key sml-mode-map "\C-c\C-f" 'sml-run-on-file)
    (define-key sml-mode-map "\C-c\C-c" 'sml-send-function)
    (define-key sml-mode-map "\C-c\C-b" 'sml-send-buffer)
    (define-key sml-mode-map "\e|" 'sml-electric-pipe)
    (define-key sml-mode-map "\C-j" 'reindent-then-newline-and-indent)
    (define-key sml-mode-map "\177" 'backward-delete-char-untabify)
    (define-key sml-mode-map "\;" 'sml-electric-semi)
    (define-key sml-mode-map "\C-c\t" 'sml-indent-region)
    (define-key sml-mode-map "\t" 'sml-indent-line))
  (use-local-map sml-mode-map)
  (setq major-mode 'sml-mode)
  (setq mode-name "SML")
  (define-abbrev-table 'sml-mode-abbrev-table ())
  (setq local-abbrev-table sml-mode-abbrev-table)
  (if sml-mode-syntax-table
      ()
    (setq sml-mode-syntax-table (make-syntax-table))
    (modify-syntax-entry ?\( "()1" sml-mode-syntax-table)
    (modify-syntax-entry ?\) ")(4" sml-mode-syntax-table)
    (modify-syntax-entry ?\\ "\\" sml-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" sml-mode-syntax-table)
    ;; Special characters in sml-mode to be treated as normal
    ;; characters:
    (modify-syntax-entry ?_ "w" sml-mode-syntax-table)
    (modify-syntax-entry ?\' "w" sml-mode-syntax-table)
    )
  (set-syntax-table sml-mode-syntax-table)
  ;; A paragraph is seperated by blank lines (or ^L) only.
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^[\t ]*$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'sml-indent-line)
  (make-local-variable 'require-final-newline) ; Always put a new-line
  (setq require-final-newline t)	; in the end of file
  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'comment-column)
  (setq comment-column 39)		; Start of comment in this column
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+[ \t]?") ; This matches a start of comment
  (make-local-variable 'comment-indent-hook)
  (setq comment-indent-hook 'sml-comment-indent)
  ;;
  ;; Adding these will fool the matching of parens. I really don't
  ;; know why. It would be nice to have comments treated as
  ;; white-space
  ;; 
  ;; (make-local-variable 'parse-sexp-ignore-comments)
  ;; (setq parse-sexp-ignore-comments t)
  ;; 
  (run-hooks 'sml-mode-hook))		; Run the hook

(defconst sml-pipe-matchers-reg
  "\\bcase\\b\\|\\bfn\\b\\|\\bfun\\b\\|\\bhandle\\b\
\\|\\bdatatype\\b\\|\\babstype\\b\\|\\band\\b"
  "The keywords a `|' can follow.")

(defun sml-electric-pipe ()
  "Insert a \"|\". Depending on the context insert the name of
function, a \"=>\" etc."
  (interactive)
  (let ((here (point))
	(match (save-excursion
		 (sml-find-matching-starter sml-pipe-matchers-reg)
		 (point)))
	(tmp "  => ")
	(case-or-handle-exp t))
    (if (/= (save-excursion (beginning-of-line) (point))
	    (save-excursion (skip-chars-backward "\t ") (point)))
	(insert "\n"))
    (insert "|")
    (save-excursion
      (goto-char match)
      (cond
       ;; It was a function, insert the function name
       ((looking-at "fun\\b\\|and\\b")
	(setq tmp (concat " " (buffer-substring
			       (progn (forward-char 3)
				      (skip-chars-forward "\t\n ") (point))
			       (progn 
				 (forward-word 1)
				 (point)))
			  " "))
	(setq case-or-handle-exp nil))
       ;; It was a datatype, insert nothing
       ((looking-at "datatype\\b\\|abstype\\b")
	(setq tmp " ") (setq case-or-handle-exp nil))))
    (insert tmp)
    (sml-indent-line)
    (forward-char (1+ (length tmp)))
    (if case-or-handle-exp
	(forward-char -4))))

(defun sml-electric-semi ()
  "If sml-electric-semi-mode is t, indent the current line, and make a
newline."
  (interactive)
  (insert "\;")
  (if sml-electric-semi-mode
      (reindent-then-newline-and-indent)))

(defun sml-mode-version ()
  (interactive)
  (message sml-mode-version-string))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SHORT CUTS (sml-region)
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sml-region-alist
  '(("let") ("local") ("signature") ("structure") ("datatype") ("case"))
  "The list of regions to auto-insert.")

(defun sml-region ()
  "Interactive short-cut. Insert a common used structure in sml."
  (interactive)
  (let ((newline nil)			; Did we insert a newline
	(name (completing-read "Region to insert: (default let) "
			       sml-region-alist nil t nil)))
    ;; default is "let"
    (if (string= name "") (setq name "let"))
    ;; Insert a newline if point is not at empty line
    (if (save-excursion (beginning-of-line) (skip-chars-forward "\t ") (eolp))
	()
      (setq newline t)
      (insert "\n"))
    (condition-case ()
	(cond
	 ((string= name "let") (sml-let))
	 ((string= name "local") (sml-local))
	 ((string= name "structure") (sml-structure))
	 ((string= name "signature") (sml-signature))
	 ((string= name "case") (sml-case))
	 ((string= name "datatype") (sml-datatype)))
      (quit (if newline 
		(progn
		  (delete-char -1)
		  (beep)))))))

(defun sml-let () 
  "Insert a `let in end'."
  (interactive) (sml-let-local "let"))

(defun sml-local ()
  "Insert a `local in end'."
  (interactive) (sml-let-local "local"))

(defun sml-signature ()
  "Insert a `signature ??? = sig end', prompting for name."
  (interactive) (sml-structure-signature "signature"))

(defun sml-structure ()
  "Insert a `structure ??? = struct end', prompting for name."
  (interactive) (sml-structure-signature "structure"))

(defun sml-case ()
  "Insert a case, prompting for case-expresion."
  (interactive)
  (let (indent (expr (read-string "Case expr: ")))
    (insert (concat "case " expr))
    (sml-indent-line)
    (setq indent (current-indentation))
    (end-of-line)
    (if sml-case-indent
	(progn
	  (insert "\n")
	  (indent-to (+ 2 indent))
	  (insert "of "))
      (insert " of\n")
      (indent-to (+ indent sml-indent-level)))
    (save-excursion (insert " => "))))

(defun sml-let-local (starter)
  (let (indent)
    (insert starter)
    (sml-indent-line)
    (setq indent (current-indentation))
    (end-of-line)
    (insert "\n") (indent-to (+ sml-indent-level indent))
    (insert "\n") (indent-to indent)
    (insert "in\n") (indent-to (+ sml-indent-level indent))
    (insert "\n") (indent-to indent)
    (insert "end") (previous-line 3) (end-of-line)))
    
(defun sml-structure-signature (which)
  (let (indent
	(name (read-string (concat "Name of " which ": "))))
    (insert (concat which " " name " ="))
    (sml-indent-line)
    (setq indent (current-indentation))
    (end-of-line)
    (insert "\n") (indent-to (+ sml-indent-level indent))
    (insert (if (string= which "signature") "sig\n" "struct\n"))
    (indent-to (+ (* 2 sml-indent-level) indent))
    (insert "\n") (indent-to (+ sml-indent-level indent))
    (insert "end") (previous-line 1) (end-of-line)))

(defun sml-datatype ()
  "Insert a `datatype ??? =', prompting for name."
  (let (indent 
	(type (read-string (concat "Type of datatype (default none): ")))
	(name (read-string (concat "Name of datatype: "))))
    (insert (concat "datatype "
		    (if (string= type "") "" (concat type " "))
		    name " ="))
    (sml-indent-line)
    (setq indent (current-indentation))
    (end-of-line) (insert "\n") (indent-to (+ sml-indent-level indent))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PARSING ERROR MESSAGES (NOTE: works only with SML of New Jersey)
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sml-last-error 1 "Last position of error. Initially 1.")

(defun sml-next-error ()
  "Find the next error by passing the *SML* buffer.\n
NOTE: This function only knows about the syntax of errors generated by
SML of New Jersey, and will only work with this."
  (interactive)
  (let ((found t) (tmp-file nil) found-line found-file error-line tmp)
    (save-excursion
      (condition-case ()
	  (progn
	    (set-buffer (concat "*" sml-process-name "*" ))
	    (goto-char sml-last-error)
	    (re-search-forward "^.+\\(Error:\\|Warning:\\)")
	    (save-excursion
	      (beginning-of-line)
	      (if (looking-at sml-tmp-template)
		  (setq tmp-file t)))
	    (setq sml-last-error (point))
	    (beginning-of-line)
	    (setq error-line (point))
	    (search-forward ",")
	    (setq found-file (buffer-substring error-line (1- (point))))
	    (search-forward "line ")
	    (setq tmp (point))
	    (skip-chars-forward "[0-9]")
	    (setq found-line (string-to-int (buffer-substring tmp (point)))))
	(error (setq found nil))))
    (if found
	(progn
	  (set-window-start
	   (display-buffer (concat "*" sml-process-name "*")) error-line)
	  (if tmp-file
	      (let ((loop t) (n 0) (tmp-list sml-tmp-files-list))
		(while loop
		  (setq tmp (car tmp-list))
		  (if (string= (car tmp) found-file)
		      (setq loop nil)
		    (setq tmp-list (cdr tmp-list)))
		  (if (null tmp-list) (setq loop nil)))
		(if (null tmp)
		    (error "Temporary file not associated with buffer.")
		  (condition-case ()
		      (progn
			(switch-to-buffer (nth 1 tmp))
			(message
			 (concat "Error found in temporary file "
				 "(line number may not match)."))
			(goto-line (1- (+ found-line (nth 2 tmp)))))
		    (error (error "Sorry, buffer doesn't exist any more.")))))
	    (if (file-exists-p found-file)
		(progn
		  (condition-case ()
		      (progn
			(find-file found-file)
			(goto-line found-line))
		    (error ())))
	      (error (concat "File not found: " found-file)))))
      (if (= sml-last-error 1)
	  (message "No errors.")
	(if (y-or-n-p "No more errors. Reset error count: ")
	    (progn
	      (message (concat
			"You will have to type: "
			(substitute-command-keys "\\[sml-next-error]")
			" again, to get first error/warning."))
	      (setq sml-last-error 1))
	  (message ""))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INDENTATION
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sml-indent-region (begin end)
  "Indent region of sml code."
  (interactive "r")
  (message "Indenting region...")
  (save-excursion
    (goto-char end) (setq end (point-marker)) (goto-char begin)
    (while (< (point) end)
      (skip-chars-forward "\t\n ")
      (sml-indent-line)
      (end-of-line))
    (move-marker end nil))
  (message "Indenting region...done"))

(defun sml-indent-line ()
  "Indent current line of sml code."
  (interactive)
  (let ((indent (sml-calculate-indentation)))
    (if (/= (current-indentation) indent)
	(let ((beg (progn (beginning-of-line) (point))))
	  (skip-chars-forward "\t ")
	  (delete-region beg (point))
	  (indent-to indent))
      ;; If point is before indentation, move point to indentation
      (if (< (current-column) (current-indentation))
	  (skip-chars-forward "\t ")))))

(defconst sml-indent-starters-reg
  "abstraction\\b\\|abstype\\b\\|and\\b\\|case\\b\\|datatype\\b\
\\|else\\b\\|fun\\b\\|functor\\b\\|if\\b\
\\|in\\b\\|infix\\b\\|infixr\\b\\|let\\b\\|local\\b\
\\|nonfix\\b\\|of\\b\\|open\\b\\|sig\\b\\|signature\\b\
\\|struct\\b\\|structure\\b\\|then\\b\\|\\btype\\b\\|val\\b\
\\|while\\b\\|with\\b\\|withtype\\b"
  "The indentation starters. The next line, after one starting with
one of these, will be indented.")

(defconst sml-starters-reg
  "\\babstraction\\b\\|\\babstype\\b\\|\\bdatatype\\b\
\\|\\bexception\\b\\|\\bfun\\b\\|\\bfunctor\\b\\|\\blocal\\b\
\\|\\binfix\\b\\|\\binfixr\\b\
\\|\\bnonfix\\b\\|\\bopen\\b\\|\\bsignature\\b\\|\\bstructure\\b\
\\|\\btype\\b\\|\\bval\\b\\|\\bwithtype\\b"
  "The starters of new expressions.")

(defconst sml-end-starters-reg
  "\\blet\\b\\|\\blocal\\b\\|\\bsig\\b\\|\\bstruct\\b"
  "Matching reg-expression for the \"end\" keyword")

(defconst sml-starters-indent-after
  "let\\b\\|local\\b\\|struct\\b\\|in\\b\\|sig\\b\\|with\\b")

(defun sml-calculate-indentation ()
  (save-excursion
    (beginning-of-line)			; Go to first non whitespace
    (skip-chars-forward "\t ")		; on the line.
    (cond
     ;; Indentation for comments alone on a line, matches the
     ;; proper indentation of the next line. Search only for the
     ;; next "*)", not for the matching.
     ((looking-at "(\\*")
      (if (not (search-forward "*)" nil t))
	  (error "Comment not ended."))
      (skip-chars-forward "\n\t ")
      ;; If we are at eob, just indent 0
      (if (eobp) 0 (sml-calculate-indentation)))
     ;; Are we looking at a case expression ?
     ((looking-at "|.*\\(\\|\n.*\\)=>")
      (sml-skip-block)
      (sml-re-search-backward "=>")
      (beginning-of-line)
      (skip-chars-forward "\t ")
      (cond
       ((looking-at "|") (current-indentation))
       ((and sml-case-indent (looking-at "of\\b"))
	(1+ (current-indentation)))
       ((looking-at "fn\\b") (1+ (current-indentation)))
       ((looking-at "handle\\b") (+ (current-indentation) 5))
       (t (+ (current-indentation) sml-pipe-indent))))
     ((looking-at "and\\b")
      (if (sml-find-matching-starter sml-starters-reg)
	  (current-column)
	0))
     ((looking-at "in\\b")		; Match the beginning let/local
      (sml-find-match-indent "in" "\\bin\\b" "\\blocal\\b\\|\\blet\\b"))
     ((looking-at "end\\b")		; Match the beginning
      (sml-find-match-indent "end" "\\bend\\b" sml-end-starters-reg))
     ((and sml-nested-if-indent (looking-at "else[\t ]*if\\b"))
      (sml-re-search-backward "\\bif\\b\\|\\belse\\b")
      (current-indentation))
     ((looking-at "else\\b")		; Match the if
      (sml-find-match-indent "else" "\\belse\\b" "\\bif\\b" t))
     ((looking-at "then\\b")		; Match the if + extra indentation
      (+ (sml-find-match-indent "then" "\\bthen\\b" "\\bif\\b" t)
	 sml-indent-level))
     ((and sml-case-indent (looking-at "of\\b"))
      (sml-re-search-backward "\\bcase\\b")
      (+ (current-column) 2))
     ((looking-at sml-starters-reg)
      (let ((start (point)))
	(sml-backward-sexp)
	(if (and (looking-at sml-starters-indent-after)
		 (/= start (point)))
	    (+ (if sml-type-of-indent
		   (current-column)
		 (if (progn (beginning-of-line)
			    (skip-chars-forward "\t ")
			    (looking-at "|"))
		     (- (current-indentation) sml-pipe-indent)
		   (current-indentation)))
	       sml-indent-level)
	  (beginning-of-line)
	  (skip-chars-forward "\t ")
	  (if (and (looking-at sml-starters-indent-after)
		   (/= start (point)))
	      (+ (if sml-type-of-indent
		     (current-column)
		   (current-indentation))
		 sml-indent-level)
	    (goto-char start)
	    (if (sml-find-matching-starter sml-starters-reg)
		(current-column)
	      0)))))
     (t
      (let ((indent (sml-get-indent)))
	(cond
	 ((looking-at "|")
	  ;; Lets see if it is the follower of a function definition
	  (if (sml-find-matching-starter
	       "\\bfun\\b\\|\\bfn\\b\\|\\band\\b\\|\\bhandle\\b")
	      (cond
	       ((looking-at "fun\\b") (- (current-column) sml-pipe-indent))
	       ((looking-at "fn\\b") (1+ (current-column)))
	       ((looking-at "and\\b") (1+ (1+ (current-column))))
	       ((looking-at "handle\\b") (+ (current-column) 5)))
	    (+ indent sml-pipe-indent)))
	 (t
	  (if sml-paren-lookback	; Look for open parenthesis ?
	      (max indent (sml-get-paren-indent))
	    indent))))))))

(defun sml-get-indent ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-backward "\t\n; ")
    (if (looking-at ";") (sml-backward-sexp))
    (cond
     ((save-excursion (sml-backward-sexp) (looking-at "end\\b"))
      (- (current-indentation) sml-indent-level))
     (t
      ;; Go to the beginning of the line and place by first
      ;; non-whitespace, but pass over starting parenthesis
      (beginning-of-line)		
      (skip-chars-forward "\t (|")
      (let ((indent (current-column)))
	(cond
	 ;; Started val/fun/structure...
	 ((looking-at sml-indent-starters-reg) (+ indent sml-indent-level))
	 ;; Indent after "=>" pattern
	 ((looking-at ".*=>") (+ indent sml-indent-level))
	 ;; else keep the same indentation as previous line
	 (t indent)))))))

(defun sml-get-paren-indent ()
  (save-excursion
    (let ((levelpar 0)			; Level of "()"
          (levelcurl 0)                 ; Level of "{}"
          (levelsqr 0)                  ; Level of "[]"
          (backpoint (max (- (point) sml-paren-lookback) (point-min)))
          (loop t) (here (point)))
      (while (and (/= levelpar 1) (/= levelsqr 1) (/= levelcurl 1) loop)
	(if (re-search-backward "[][{}()]" backpoint t)
	    (if (not (sml-inside-comment-or-string-p))
		(cond
		 ((looking-at "(") (setq levelpar (1+ levelpar)))
		 ((looking-at ")") (setq levelpar (1- levelpar)))
		 ((looking-at "\\[") (setq levelsqr (1+ levelsqr)))
		 ((looking-at "\\]") (setq levelsqr (1- levelsqr)))
		 ((looking-at "{") (setq levelcurl (1+ levelcurl)))
		 ((looking-at "}") (setq levelcurl (1- levelcurl)))))
	  (setq loop nil)))
      (if loop
	  (1+ (current-column))
	0))))

(defun sml-inside-comment-or-string-p ()
  (let ((start (point)))
    (if (save-excursion
	  (condition-case ()
	      (progn
		(search-backward "(*")
		(search-forward "*)")
		(forward-char -1)	; A "*)" is not inside the comment
		(> (point) start))
	    (error nil)))
	t
      (let ((numb 0))
	(save-excursion
	  (save-restriction
	    (narrow-to-region (progn (beginning-of-line) (point)) start)
	    (condition-case ()
		(while t
		  (search-forward "\"")
		  (setq numb (1+ numb)))
	      (error (if (and (not (zerop numb))
			      (not (zerop (% numb 2))))
			 t nil)))))))))
		
(defun sml-skip-block ()
  (sml-backward-sexp)
  (cond 
   ;; If what we just passed was a comment, then go backward to
   ;; some code, as code is indented according to other code and
   ;; not according to comments.
   ((looking-at "(\\*")
    (skip-chars-backward "\t\n "))
   ;; Skip over let-in-end/local-in-end etc...
   ((looking-at "end\\b")
    (goto-char (sml-find-match-backward "end" "\\bend\\b"
					sml-end-starters-reg))
    (skip-chars-backward "\n\t "))
   ;; Here we will need to skip backwardd past if-then-else
   ;; and case-of expression. Please - tell me how !!
   ))

(defun sml-find-match-backward (unquoted-this this match &optional start)
  (save-excursion
    (let ((level 1) (here (point))
	  (pattern (concat this "\\|" match)))
      (if start (goto-char start))
      (while (not (zerop level))
	(if (sml-re-search-backward pattern)
	    (setq level (cond
			 ((looking-at this) (1+ level))
			 ((looking-at match) (1- level))))
	  ;; The right match couldn't be found
	  (error (concat "Unbalanced: " unquoted-this))))
      (point))))

(defun sml-find-match-indent (unquoted-this this match &optional indented)
  (save-excursion
    (goto-char (sml-find-match-backward unquoted-this this match))
    (if (or sml-type-of-indent indented)
	(current-column)
      (if (progn
	    (beginning-of-line)
	    (skip-chars-forward "\t ")
	    (looking-at "|"))
	  (- (current-indentation) sml-pipe-indent)
	(current-indentation)))))

(defun sml-find-matching-starter (regexp)
  (let ((start-let-point (sml-point-inside-let-etc))
	(start-up-list (sml-up-list))
	(found t))
    (if (sml-re-search-backward regexp)
	(progn
	  (condition-case ()
	      (while (or (/= start-up-list (sml-up-list))
			 (/= start-let-point (sml-point-inside-let-etc)))
		(re-search-backward regexp))
	    (error (setq found nil)))
	  found)
      nil)))

(defun sml-point-inside-let-etc ()
  (let ((last nil) (loop t) (found t) (start (point)))
    (save-excursion
      (while loop
	(condition-case ()
	    (progn
	      (re-search-forward "\\bend\\b")
	      (while (sml-inside-comment-or-string-p)
		(re-search-forward "\\bend\\b"))
	      (forward-char -3)
	      (setq last (sml-find-match-backward "end" "\\bend\\b"
						  sml-end-starters-reg last))
	      (if (< last start)
		  (setq loop nil)
		(forward-char 3)))
	  (error (progn (setq found nil) (setq loop nil)))))
      (if found
	  last
	0))))
		     
(defun sml-re-search-backward (regexpr)
  (let ((found t))
    (if (re-search-backward regexpr nil t)
	(progn
	  (condition-case ()
	      (while (sml-inside-comment-or-string-p)
		(re-search-backward regexpr))
	    (error (setq found nil)))
	  found)
      nil)))

(defun sml-up-list ()
  (save-excursion
    (condition-case ()
	(progn
	  (up-list 1)
	  (point))
      (error 0))))

(defun sml-backward-sexp ()
  (condition-case ()
      (progn
	(backward-sexp 1)
	(if (looking-at "(\\*")
	    (backward-sexp 1)))
    (error nil)))

(defun sml-comment-indent ()
  (if (looking-at "^(\\*")		; Existing comment at beginning
      0					; of line stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (1+ (max (current-column)		; Else indent at comment column
	       comment-column)))))	; except leave at least one space.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INFERIOR SHELL
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sml-shell-map nil "The mode map for sml-shell.")

(defun sml-shell ()
  "Inferior shell invoking SML.
It is not possible to have more than one shell running SML.
Like the shell mode with the additional command:

\\[sml-run-on-file]\t Runs sml on the file.
\\{sml-shell-map}
Variables controlling the mode:

sml-prog-name (default \"sml\")
    The string used to invoke the sml program.

sml-use-right-delim (default \"\\\"\")
sml-use-left-delim  (default \"\\\"\")
    The left and right delimiter used by your version of sml, for
    \"use file-name\".

sml-process-name (default \"SML\")
    The name of the process running sml.

sml-shell-prompt-pattern (default \"^[^\\-=]*[\\-=] *\")
    The prompt pattern.

Runs sml-shell-hook if not nil."
  (interactive)
  (if (not (process-status sml-process-name))
      (save-excursion			; Process is not running
	(message "Starting SML...")	; start up a new process
	(require 'shell)
	(set-buffer (make-shell sml-process-name sml-prog-name))
	(erase-buffer)			; Erase the buffer if a previous
	(if sml-shell-map		; process died in there
	    ()
	  (setq sml-shell-map (copy-sequence shell-mode-map))
	  (define-key sml-shell-map "\C-c\C-f" 'sml-run-on-file))
	(use-local-map sml-shell-map)
	(make-local-variable 'shell-prompt-pattern)
	(setq shell-prompt-pattern sml-shell-prompt-pattern)
	(setq major-mode 'sml-shell)
	(setq mode-name "SML Shell")
	(setq mode-line-format 
	      "-----Emacs: %17b   %M   %[(%m: %s)%]----%3p--%-")
	(set-process-filter (get-process sml-process-name) 'sml-process-filter)
	(message "Starting SML...done.")
	(run-hooks 'sml-shell-hook))))

(defun sml-process-filter (proc str)
  (let ((cur (current-buffer))
	(pop-up-windows t))
    (pop-to-buffer (concat "*" sml-process-name "*"))
    (goto-char (point-max))
    (insert str)
    (set-marker (process-mark proc) (point-max))
    (pop-to-buffer cur)))

(defun sml-pop-to-shell ()
  (interactive)
  (sml-shell)
  (pop-to-buffer (concat "*" sml-process-name "*")))

(defun sml-run-on-file (fil)
  (interactive "FRun SML on : ")
  (sml-shell)
  (save-some-buffers)
  (send-string sml-process-name
	       (concat "use " sml-use-left-delim (expand-file-name fil)
		       sml-use-right-delim ";\n")))

(defun sml-save-buffer-use-file ()
  "Save the buffer, and send a `use file' to the inferior shell
running SML."
  (interactive)
  (let (file)
    (if (setq file (buffer-file-name))	; Is the buffer associated
	(progn				; with file ?
	  (save-buffer)
	  (sml-shell)
	  (send-string sml-process-name
		       (concat "use " sml-use-left-delim
			       (expand-file-name file)
			       sml-use-right-delim ";\n")))
      (error "Buffer not associated with file."))))

(defvar sml-tmp-files-list nil
  "List of all temporary files created by sml-simulate-send-region.
Each element in the list is a list with the format:

      (\"tmp-filename\"  buffer  start-line)")

(defvar sml-simulate-send-region-called-p nil
  "Has sml-simulate-send-region been called previously.")

(defun sml-simulate-send-region (point1 point2)
  "Simulate send region. As send-region only can handle what ever the
system sets as the default, we have to make a temporary file.
Updates the list of temporary files (sml-tmp-files-list)."
  (let ((file (expand-file-name (make-temp-name sml-tmp-template))))
    ;; Remove temporary files when we leave emacs
    (if (not sml-simulate-send-region-called-p)
	(progn
	  (setq sml-old-kill-emacs-hook kill-emacs-hook)
	  (setq kill-emacs-hook 'sml-remove-tmp-files)
	  (setq sml-simulate-send-region-called-p t)))
    (save-excursion
      (goto-char point1)
      (setq sml-tmp-files-list
	    (cons (list file
			(current-buffer)
			(save-excursion	; Calculate line no.
			  (beginning-of-line)
			  (1+ (count-lines 1 (point)))))
		  sml-tmp-files-list)))
    (write-region point1 point2 file nil 'dummy)
    (sml-shell)
    (message "Using temporary file: %s" file)
    (send-string
     sml-process-name
     ;; string to send: use file;
     (concat "use " sml-use-left-delim file sml-use-right-delim ";\n"))))

(defvar sml-old-kill-emacs-hook nil
  "Old value of kill-emacs-hook")

(defun sml-remove-tmp-files ()
  "Remove the temporary files, created by sml-simulate-send-region, if
they still exist. Only files recorded in sml-tmp-files-list are removed."
  (message "Removing temporary files created by sml-mode...")
  (while sml-tmp-files-list
    (condition-case ()
 	(delete-file (car (car sml-tmp-files-list)))
      (error ()))
    (setq sml-tmp-files-list (cdr sml-tmp-files-list)))
  (message "Removing temporary files created by sml-mode...done.")
  (run-hooks 'sml-old-kill-emacs-hook))

(defun sml-send-region ()
  "Send region."
  (interactive)
  (let (start end)
    (save-excursion
      (setq end (point))
      (exchange-point-and-mark)
      (setq start (point)))
    (sml-simulate-send-region start end)))

(defun sml-send-function ()
  "Does NOT send the function, but the paragraph."
  (interactive)
  (let (start end)
    (save-excursion
      (condition-case ()
	  (progn
	    (backward-paragraph)
	    (setq start (point)))
	(error (setq start (point-min))))
      (condition-case ()
	  (progn
	    (forward-paragraph)
	    (setq end (point)))
	(error (setq end (point-max)))))
    (sml-simulate-send-region start end)))

(defun sml-send-buffer ()
  "Send the buffer."
  (interactive)
  (sml-simulate-send-region (point-min) (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; END OF SML-MODE
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;-- 
; Lars Bo Nielsen          | UUCP: ..!rutgers!ksuvax1!lbn
; lbn@ksuvax1.cis.ksu.edu  |   or: ..!{pyramid,ucsd}!ncr-sd!ncrwic!ksuvax1!lbn


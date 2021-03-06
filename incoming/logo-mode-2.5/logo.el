;;; logo.el -- Major mode for editing Logo source code

;; Copyright (C) 1998, 1999 by Hrvoje Blazevic <hrvoje.blazevic@ri.tel.hr>

;; Logo.el is free software distributed under the terms
;; of the GNU General Public License, version 2, or (at your option)
;; any later version.

;; Logo.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; This is version 2.5 completed on 26th January 2000, with
;; bits and pieces lifted from scheme.el , cmuscheme.el and comint.el

;; For more information about logo.el , comments, or bug reports, 
;; send e-mail to <hrvoje.blazevic@ri.tel.hr>
 

;;; Commentary:

;; Logo.el is written for Berkeley Logo, with no intention to make
;; it usable with other UN*X Logo interpreters (if there are any?). 
;;
;; Logo TERMINAL ACCESS issues
;; ===========================
;; 
;; The effects of these commands depend partially on terminal-mode
;; you run Inferior Logo process in.
;;
;; By default logo.el starts Logo process in setcursor mode. Out of
;; three available, this is the most "logo-centric" mode. All Berkeley
;; Logo terminal access commands are available, as well as all normal
;; emacs and comint mode editing goodies. However, the behavior of emacs
;; might look strange at times in this mode. This is a result of two
;; conflicting requirements.  On one side the need to make emacs buffer
;; behave like a standard dumb terminal, and on the other to allow most
;; of emacs editing niceties.  
;; The most visible differences are:
;; Backspace and Delete keys will only work if the point is within input
;; line (string) limits. That also means that if you haven't entered any
;; fresh input yet, they will not work. You got no business deleting text
;; all over setcursor buffer anyway!  Yanking marked text with mouse-2
;; works, but it *always* inserts at the beginning of input line, no
;; matter where you point with the mouse, overwriting any text found
;; there. Reason is simple - again, you got no business yanking text all
;; over setcursor buffer (except at input line).  C-k and C-y kill or
;; yank-back only the input line. C-y works the same way as mouse-2,
;; which means that if external X-selection exists, it will always use
;; that, and not the contents of the kill-ring.
;;
;; The remaining two modes, terminal and emacs, are here in case that
;; setcursor mode causes problems, or is not available. (Setcursor mode is
;; not available if you have set logo-system-type to 'safe or 'raw in
;; your .emacs file.)  Terminal and emacs modes are practically equal,
;; except that scrolling in emacs mode is to the middle of buffer window,
;; while in terminal mode you stay at the bottom, once you get there. The
;; access to previous screens of logo interaction (after doing CLEARTEXT)
;; is restricted in terminal mode, but not so in emacs mode. Use these
;; modes if you write Logo programs that do not use SETCURSOR an CURSOR
;; commands.
;; 
;; CLEARTEXT does the right thing (in all modes).
;; SETCURSOR works in Setcursor mode (does nothing in other two).
;; CURSOR outputs correctly only in Setcursor mode.
;; STANDOUT does the right thing (highlights) - in all modes.
;; SETMARGINS does the right thing (in all modes).
;;
;; ===============================
;;
;; The indenting style that I chose to implement here is trying to 
;; follow Brian Harwey's Computer Science Logo Style (Second Edition).
;;
;; The "Style" can be described as follows:
;;
;; 1 Everything is flush left -- except
;;
;; 2 Explicit continuation lines (after the line ending with `~')
;;   indent to the second word on previous line, or to same
;;   column as the previous line. Explicit continuation has a
;;   "sanity checker" -- 50th column, built in. This can be changed
;;   by changing `logo-max-indent' variable.
;;
;; 3 Implicit continuation lines, lines with pending `]', `)', or `}',
;;   indent in several different ways. The "BOOK" uses mostly lisp 
;;   style indentation on logo that does not have lisp syntactical 
;;   form. If you want to enforce this in your code use parenthesis,
;;   even if not needed. Ending the line with `[' starts something
;;   that can be described as C style -- or a very long line style.
;;
;;   The examples bellow are taken from CSLS:
;;
;;   Lisp style (almost)
;;
;;   to worldtree
;;   make "world tree "world ~
;;        (list (tree "France
;;                    (list (tree "Paris [])
;;                          (tree "Dijon [])
;;                          (tree "Avignon [])))
;;              (tree "China
;;                    (list ...
;;
;;   Very, very long line style
;;
;;   ifelse :end1 < (linenum :fib1) [
;;      print (sentence "INSERT :end1+1 (word (linenum :fib2) "- :end2))
;;   ] [ifelse :end2 < (linenum :fib2) [
;;      print (sentence "DELETE (word (linenum :fib1) "- :end1) :end2+1) 
;;   ] [
;;      print (sentence "CHANGE (word (linenum :fib1) "- :end1)
;;                              (word (linenum :fib2) "- :end2))
;;      make "dashes "true
;;   ]]
;;
;;   C style ?
;;
;;   for [i 1 :buttons] [
;;      setitem :i :f 0
;;      make "right 0
;;      for [j 0 :i-1] [
;;         make "left :right
;;         ...
;;      ]
;;      setitem :i :combs 1
;;   ]
;;
;;   Continued strings do not indent.
;;
;;   All of this is implemented, and works automatically. If (in
;;   certain situations) it doesn't, or you just hate it, then
;;   use `~'.
;;
;;; ==================================================================
;;
;;  Following lines should be added to your .emacs file:
;;
;; THIS IS A MUST! 
;; (setq auto-mode-alist
;;       (cons '("\\.lgo?\\'" . logo-mode)
;;             auto-mode-alist))
;; (autoload 'logo-mode "logo")
;;
;; If you choose to put logo.el in an unusual place 
;; (setq load-path
;;       (cons "/your-unusual-place"
;;             load-path))
;;
;; If your Logo binary is not called `logo'
;; (setq logo-binary-name "your-logo-binary")
;;
;; To change the size of emacs frame/window for logo inferior process:
;; (setq logo-term-columns 96)
;; (setq logo-term-rows 48)
;; where 96 and 48 is whatever makes you happy. If this is not set,
;; logo.el will default to 80 and 24.
;; Emacs edit buffer will resize to logo-term-columns, but will *not*
;; resize to logo-term-rows. This works properly only with ucblogo
;; versions greater than 4.6 .
;;
;; By default emacs will run newline and indent when RET key is hit in
;; logo (edit) buffer. LFD key (C-j) will run newline only. To change this
;; behavior:
;; (setq logo-standard-indent t)
;; This will reverse RET and LFD keys.
;;
;; Default Logo process mode is set to setcursor mode. To change this 
;; permanently set:
;; (setq logo-setcursor-start nil)
;; 
;; Default text input mode in setcursor mode is set to overwrite. To change
;; this permanently set:
;; (setq logo-setcursor-overwrite nil)
;;
;; By default, the old text, after recalling the previous input in
;; setcursor mode will be cleared to the end of the line. If that gets in
;; your way, set this:
;; (setq logo-setcursor-spaces-old n)
;; where n is from 0 to any number. However, spaces are never pushed beyond
;; end of the line.
;;
;; By default two spaces are pushed ahead of end of the new input in
;; setcursor mode. If that gets in your way, set this:
;; (setq logo-setcursor-spaces-new n)
;; where n is from 0 to any number. However, spaces are never pushed beyond
;; end of the line.
;;
;; If your logo helpfiles are not in /usr/local/lib/logo/helpfiles 
;; (setq logo-help-path "/your-help-path")
;;
;; If your contents file in helpfiles is not called HELPCONTENTS 
;; (setq logo-helpcontents "your-helpcontents")
;;
;; If your logo info files are not in /usr/local/info/ or in 
;; /usr/local/lib/logo/info
;; (setq logo-info-file "/your-info-path/ucblogo.info")
;;
;; If you have installed ucblogo info files translated into another language,
;; then set this:
;; (setq logo-info-file-trans "/your-info-path/file-name.info")
;; However, for this to work - somebody has to write translation first. To the
;; best of my knowledge only Croatian translation exists. Therefore by default
;; this is set to "/usr/local/info/ucbl-hr.info". If you change this to another
;; language you must also update the `Help' menu entry by setting this:
;; (setq logo-info-trans "De")
;; This "De" would be in case your translation is German, but you can use 
;; longer words as well. By default this is set to "Hr". 
;; If you don't care for this at all set
;; (setq logo-info-trans nil)
;; With this set menu entry for translated manual will disappear.
;;
;; Emacs expects to find logo-mode tutorial files (tutorial.lg &
;; tutor1.lg) in /usr/local/lib/logo/emacs directory. If you placed these
;; files somewhere else, then set this:
;; (setq logo-tutorial-path "/your-path/")
;;
;; By default emacs allowes changing Logo language standard with
;; Language menu. If you don't want to see that menu, set this:
;; (setq logo-load-language nil)
;;
;; If you don't want to use /tmp/e-logo and /tmp/DEBUG for the temporary 
;; transfer and tracing info files. Make sure you have writing permission 
;; in /your-path (However it is safer not to change this). 
;; (setq logo-temp-file "/your-path/your-transfer-file")
;; (setq logo-debug-file "/your-path/your-debug-file")
;; 
;; By default logo-mode starts with syntax highlighting enabled. If
;; you do not want that, set this:
;; (setq logo-syntax-highlight nil)
;;
;; Time to show highlighted parentheses matched region when moving
;; cursor over source code is by default set to 2 seconds. You can change
;; it to any value, as highlighting ceases as soon as you move the cursor
;; again.
;; (setq logo-matching-delay-time seconds)
;;
;; By default logo-mode starts with novice command management enabled, 
;; which limits user's access to some possibly dangerous commands.
;; If you are not a novice (meaning emacs - mostly), set this:
;; (setq logo-novice-management nil) 
;; 
;; Logo mode automatically sets the depth of setcursor input
;; history menu to safe value for the display resolution it is running
;; on. (20 entries for 640x480, 25 for 800x600 ...) To change this
;; set following: 
;; (setq logo-dynamic-menu-depth n) 
;; where n is any number. However note that if you use larger number, 
;; you may not see and use all menu entries.
;;
;; Do not set next three unless you get garbage on screen in logo
;; process buffer.
;; If default output filter does more damage than good, try this to
;; force `vt100' or `xterm' TERM settings. This should work with 
;; default filter.
;; (setq logo-system-type 'vt100)
;; or
;; (setq logo-system-type 'xterm)
;;
;; If leaving it alone, or `vt100/xterm' did not work, set this to get 
;; comint-mode default TERM settings (dumb or emacs)
;; (setq logo-system-type 'safe)
;;
;; If `safe' filters cause problems as well disable filtering with
;; (setq logo-system-type 'raw)
;;
;; This gives clean output with ucblogo4.2, but not with older versions.
;; 
;; The following should be set if your system is *very* slow, or heavily
;; overloaded at times:
;;
;; To change the maximum distance for checking unbalanced parentheses
;; (setq logo-unbalanced-distance 4096)
;; This should be reasonable value (default is 8192), but you can 
;; probably reduce it to 2048, if you do not intend to use *very*
;; long implicitly continued lines in your source code. Changing to 4096
;; should yield about 25% increase in speed for indenting and procedure 
;; sending operations.
;; 
;; For default filter timer problems;
;; (setq logo-filter-timer n)
;; `n' = 2 by default, which should work well for P133 or comparable system.
;; P6/200 works well with 1, but P60 needs 5. These figures are given just 
;; as an indication. If you suspect timing problems with default filter,
;; carry out the following test in setcursor mode:
;;
;; repeat 1000 [setcursor list random 75 random 24 type standout cursor]
;;
;; If you find more than one `?' after the loop has completed, you have to
;; increase `n'. This will *not* slow down your system's normal response.
;; What it will do is this: in case of *very* long standout (295 characters
;; or longer - a situation that Logo itself can't handle well) you will have
;; to wait `n' seconds to be notified, and to see the portion of output that 
;; Logo did print out. That's why I set default to 2 --- to avoid creating
;; the illusion of a hang-up system.
;;
;;
;;                       ===***===  
;;
;; The *BEST* way to start logo mode is opening one logo source file
;; and using Logo-start menu (on menu-bar) to fire up Logo. Of the 
;; three choices there (Run Logo Other Frame, Run Logo Other Window,
;; and Run Logo Same Window), the best is Other Frame, especially
;; if you intend to use setcursor mode. The worst (with setcursor)
;; is Other Window. The vertical size of window changes easily by
;; emacs own will, and Logo expects window of 80x24. 
;; If the size is not correct display will be garbled in setcursor mode.
;; 
;; "Logo source file" means file with extension `.lg' or `.lgo', or
;; a file that starts with magic characters: `;;;  -*- logo -*-'.
;;
;;                       ===***===
;;
;; If your logo library does not contain file `emacs.debug' (all versions
;; of ucblogo < 4.3) you will find the code at the and of this file. Copy 
;; it to the library directory under emacs.debug name (uncomment it first). 
;; Without this file Logo-debug menu in the process buffer will not work. 
;;
;;                       ===***===
;;
;; Basic features and functionality of logo.el, are about the same 
;; as cmuscheme.el, therefore no detailed instructions are necessary.
;; However, as Logo probably attracts younger users than Scheme, I have
;; included menu-bar equivalents for most of standard editing and 
;; process-communication commands.  
;;
;; Help system is very simple and low-tech. Mouse-3 in edit buffer
;; opens help file for the selected word. If help file for the word
;; does not exist, HELPCONTENTS file is visited. From HELPCONTENTS
;; user can select (mouse-3) and view as many help files as he wants.
;; To quit this cycle hit `q' in HELPCONTENS.
;;
;; Debug menu in Logo-process buffer, offers a simple mouse selection
;; for tracing and stepping. Mouse bindings are as in XFM manager.
;; Mouse-1 de-selects everything previously selected in category
;; (procedures, variables, properties) and selects chosen name.
;; Mouse-2 toggles selection for name.
;; Mouse-3 selects and quits debug buffer. `q' key also quits buffer
;; and sends the selected contents list to Logo.
;;
;; The following applies only to ucblogo version 4.3 or greater:
;; Names that were TRACED or STEPPED before the debug buffer opens, will
;; be highlighted and placed in parentheses. Highlighting will be turned
;; on/off with mouse action (to reflect user's choice). Parentheses (around
;; previously chosen names) will, however, remain. I choose to call this
;; a feature -- although some will probably call it laziness.
;; 
;;                        ===***===
;;
;; If you get garbage (or nothing) printed out when starting Logo, then 
;; add to your .emacs file (setq logo-system-type 'test). Start Logo, 
;; and in process buffer do: 
;;
;; M-x run-logo-filter-test
;;
;; This will send request to Logo to display all Terminal Access
;; codes. Copy that display into a file and e-mail it to me.
;; Even if you mess with filters on your own, I would like to
;; hear about it.
;; 
;;; ==================================================================


(require 'comint)
(require 'timer)
(require 'font-lock)

;;; code

;;; ============= Syntax Stuff ==============

(defvar logo-mode-syntax-table nil "")

(if (not logo-mode-syntax-table)
    (let ((i 0))
      (setq logo-mode-syntax-table (make-syntax-table))
      (set-syntax-table logo-mode-syntax-table)

      ;; Default is symbol constituent
      (while (< i 256)
	(modify-syntax-entry i "-   ")
	(setq i (1+ i)))

      ;; Standard word constituents
      (setq i ?0)
      (while (<= i ?9)
	(modify-syntax-entry i "w   ")
	(setq i (1+ i)))
      (setq i ?A)
      (while (<= i ?Z)
	(modify-syntax-entry i "w   ")
	(setq i (1+ i)))
      (setq i ?a)
      (while (<= i ?z)
	(modify-syntax-entry i "w   ")
	(setq i (1+ i)))
      
      ;; Logo specific word constituents
      (modify-syntax-entry ?. "w   ")
      (modify-syntax-entry ?: "w   ")
      (modify-syntax-entry ?? "w   ")
      (modify-syntax-entry ?` "w   ")
      (modify-syntax-entry ?_ "w   ")

      ;; The rest is to handle names in CSLS match program
      (modify-syntax-entry ?^ "w   ")
      (modify-syntax-entry ?# "w   ")
      (modify-syntax-entry ?@ "w   ")
      (modify-syntax-entry ?& "w   ")
      (modify-syntax-entry ?! "w   ")
      (modify-syntax-entry ?' "w   ")

      ;; Whitespace
      (modify-syntax-entry ?\t "    ")
      (modify-syntax-entry ?\n ">   ")
      (modify-syntax-entry ?\f "    ")
      (modify-syntax-entry ?\r "    ")
      (modify-syntax-entry ?\  "    ")

      ;; Symbol delimiters
      (modify-syntax-entry ?\( "()  ")
      (modify-syntax-entry ?\) ")(  ")
      (modify-syntax-entry ?{ "(}  ")
      (modify-syntax-entry ?} "){  ")
      (modify-syntax-entry ?[ "(]  ")
      (modify-syntax-entry ?] ")[  ")

      ;; Quotes
      (modify-syntax-entry ?\| "\"   ")

      ;; Comments
      (modify-syntax-entry ?\; "<   ")

      ;; Special characters
      (modify-syntax-entry ?, "_  p")
      (modify-syntax-entry ?\" "_  p")

      ;; Escape character
      (modify-syntax-entry ?\\ "\\   ")))

;;; ================ Abbreviation table ===============
;;; not much here

(defvar logo-mode-abbrev-table nil "")
(define-abbrev-table 'logo-mode-abbrev-table ())

;;; ================ Logo mode variables =================

(defvar dont-mess-with-logo-colors nil "Set highlighting for edit buffer")
(defvar logo-defun-start-rgx nil "Start of Logo definition common string")
(defvar logo-defun-start nil "Start of Logo definition")
(defvar logo-defun-end nil "End of Logo definition")
(defvar logo-defun-lock-start nil "Start of Logo definition - lock-mode")
(defvar logo-defun-lock-end nil "End of Logo definition - lock-mode")
(defvar logo-defun-lock-define nil "Start of Logo define or .defmacro")
(defvar logo-max-indent nil "Max indent column for ~ indenting")
(defvar logo-min-indent nil "Min indent column")
(defvar logo-xcontinuation-line nil "Explicit Logo continuation line")
(defvar logo-comment-line nil "Comment only line")
(defvar logo-unbalanced-distance nil "Search distance for end of sexp")
(defvar logo-instring-distance nil "Search distance string bounds")
(defvar logo-syntax-highlight t "Turn on syntax highlighting by default")
(defvar logo-novice-management t "Turn on disabling `dangerous' commands")
(defvar logo-flash-on-movement t "Turn on parens matching on cursor movement")
(defvar logo-lazy-lock-mode nil "Use standard font-lock as a default")

;; Pacifying the compiler
(defvar font-lock-mode-maximum-decoration t)
(defvar font-lock-use-default-fonts t)
(defvar font-lock-use-default-colors t)

(defun logo-mode-variables ()
  (set-syntax-table logo-mode-syntax-table)
  (setq local-abbrev-table logo-mode-abbrev-table)

  ;; Indenting stuff
  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-start "$\\|^^L")
  (setq paragraph-separate "[ \t]*$\\|^^L")
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'logo-indent-line)
  (setq indent-tabs-mode nil)
  (make-local-variable 'words-include-escapes)
  (setq words-include-escapes t)

  ;; Comments stuff
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+[ \t]*")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)

  ;; Emacs lisp eval depth limit &
  ;; limit on number of Lisp variable bindings & unwind-protects.
  ;; This is mostly for repeated Logo SETCURSOR & TYPE commands
  (setq max-lisp-eval-depth 400)
  (setq max-specpdl-size 800)

  ;; Syntax highlighting stuff
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(logo-font-lock-keywords nil t))
  (or dont-mess-with-logo-colors
      (condition-case nil
	  (progn
	    (make-face-bold 'bold-italic)
	    (set-face-foreground 'bold-italic "Blue")
	    (setq font-lock-mode-maximum-decoration t)
	    (setq font-lock-use-default-fonts nil)
	    (setq font-lock-use-default-colors nil)
	    (copy-face 'default 'font-lock-string-face)
	    (set-face-foreground 'font-lock-string-face "Sienna")
	    (copy-face 'italic 'font-lock-comment-face)
	    (set-face-foreground 'font-lock-comment-face "Red")
	    (copy-face 'bold 'font-lock-function-name-face)
	    (set-face-foreground 'font-lock-function-name-face "MediumBlue")
	    (copy-face 'bold 'font-lock-variable-name-face)
	    (set-face-foreground 'font-lock-variable-name-face "SteelBlue")
	    (copy-face 'bold 'font-lock-keyword-face)
	    (set-face-foreground 'font-lock-keyword-face "MidnightBlue")
	    (copy-face 'default 'font-lock-type-face)
	    (set-face-foreground 'font-lock-type-face "DarkOliveGreen")
	    (set-face-background 'region "LightCyan3")
	    (set-face-foreground 'modeline "red")
	    (set-face-background 'modeline "bisque"))
	(error nil)))
  ;; Logo mode frame colors
  (or dont-mess-with-logo-colors
      (condition-case nil
	  (setq default-frame-alist
		'(
		  ;; This will look nice if you set following in ~/.Xdefaults.
		  ;; emacs*Background: DarkSlateGray
		  ;; emacs*Foreground: Wheat
		  ;; emacs.pointerColor: Orchid
		  (cursor-color . "red")
		  (cursor-type . box)
		  (foreground-color . "black")
		  (background-color . "honeydew")))
	(error nil))))

(if (not logo-max-indent)
    (setq logo-max-indent 50))

(if (not logo-min-indent)
    (setq logo-min-indent 3))

;; Regex for matching the explicit continuation line.
;; Looking for a line that can consist of anything, as long as it does
;; have tilde at the end, followed by white spaces and end of line. It
;; must either have beginning of line, or can not have odd number of
;; backslashes, or newline directly in front of tilde.
(if (not logo-xcontinuation-line)
    (setq logo-xcontinuation-line
	  ".*\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*~[ \t]*$"))

;; Matching Logo procedure definition start - basic string.
;; Look for a word that starts Logo definition followed by at least
;; one space and a word constituent or a comment ending with ~ or
;; xcontinuation line.
(or logo-defun-start-rgx
    (setq
     logo-defun-start-rgx
     (concat
      "\\(to\\|.macro\\)\\([ \t]+\\w+\\|"
      "[ \t]*;[^~\n]*~[ \t]*$\\|"
      "[ \t]*~[ \t]*$\\)")))

;; Matching Logo procedure definition start.
;; Nothing but white spaces on a line before the basic string.
(or logo-defun-start
    (setq
     logo-defun-start
     (concat "^[ \t]*" logo-defun-start-rgx)))

;; Look for a line that starts with word END followed by zero or more spaces,
;; and end of line or comment start. This is more restrictive than Logo.
;; Logo will allow anything after END.
(or logo-defun-end
    (setq logo-defun-end
	  "^[ \t]*\\(end\\)[ \t]*\\($\\|;\\)"))

;; Font-lock mode - handling to and .macro - must be flush left
(or logo-defun-lock-start
    (setq
     logo-defun-lock-start
     (concat "^" logo-defun-start-rgx)))

;; Font-lock mode - handling end - must be flush left
(or logo-defun-lock-end
    (setq logo-defun-lock-end
	  "^\\(end\\)[ \t]*\\($\\|;\\)"))

;; Font-lock mode - handling define and .defmacro
(or logo-defun-lock-define
    (setq logo-defun-lock-define
	  (concat
	   "\\(^\\|[ \t]+\\)\\(define\\|.defmacro\\)\\([ \t]+\"?\\w+\\|"
	   "[ \t]*;[^~\n]*~[ \t]*$\\|"
	   "[ \t]*~[ \t]*$\\)")))

;; Defining how and when syntax highlighting happens
(setq font-lock-mode-maximum-decoration t)
(and logo-lazy-lock-mode
     (progn
       (defvar font-lock-support-mode 'lazy-lock-mode "")
       (defvar lazy-lock-defer-time 1 "")
       (defvar lazy-lock-defer-contextually t "")
       (defvar lazy-lock-stealth-time 3 ""))
     (fast-lock-mode 1))

(defvar logo-font-lock-keywords
  (list
   (cons "\\<:\\w+\\>" 'font-lock-variable-name-face)
   (list logo-defun-lock-define 2 'font-lock-keyword-face 'append)
   (list logo-defun-lock-define 3 'font-lock-function-name-face 'append)
   (list logo-defun-lock-start 1 'font-lock-keyword-face 'append)
   (list logo-defun-lock-start 2 'font-lock-function-name-face 'append)
   (list logo-defun-lock-end 1 'font-lock-keyword-face 'append))
  "Default expressions to highlight in Logo mode.")

;; Comment only line - for indenting purposes
(if (not logo-comment-line)
    (setq logo-comment-line
	  "^[ \t]*;+[^\n]*$"))

;; Limits the search for start of enclosing sexpr.
;; Change in .emacs if system response too slow.
(or logo-unbalanced-distance
    (setq logo-unbalanced-distance 8192))

(defvar logo-reindent-now nil
  "Do we reindent auto-magically after next character?")
(make-variable-buffer-local 'logo-reindent-now)

(defvar logo-buffer nil "Name of buffer running logo inferior process.")

(defvar logo-edit-buflist nil "List holding names of logo edit buffers")

(defvar logo-switch-buffer nil
  "Variable holding name of function for switching buffers.")
(if logo-switch-buffer
    nil
  (defalias 'logo-switch-buffer 'switch-to-buffer))

(defvar logo-frames nil "If t starting 2 frame operation.")

(defvar logo-binary-name nil "Logo binary name")
(if logo-binary-name
    nil
  (setq logo-binary-name "logo")) ;; Change this to your binary

;; This sets the temporary transfer file to /tmp/e-logoxxxxxx
;; Make sure you have writing permission in /tmp , or wherever you
;; change this to.
(defvar logo-temp-file nil "Name of load file.")
(if logo-temp-file
    nil
  (setq logo-temp-file
	(make-temp-name "/tmp/e-logo")))

(defvar logo-help-path nil "Logo helpfiles path.")
(if (not logo-help-path)
    (setq logo-help-path "/usr/local/lib/logo/helpfiles/"))

(defvar logo-helpcontents nil "Logo helpcontents file.")
(if (not logo-helpcontents)
    (setq logo-helpcontents "HELPCONTENTS"))

(defvar logo-tutorial-path "/usr/local/lib/logo/emacs/"
  "Logo-mode tutorial files")

(defvar logo-info-file nil "Variable holding full path to ucblogo info file.")
(or logo-info-file
    (setq logo-info-file "/usr/local/info/ucblogo.info"))

(defvar logo-info-trans "(Hr)" "Variable holding language name extension for 
info translation. Set by default to Croatian. To clear this menu enty set it
to nil.")

(defvar logo-info-file-trans nil "Variable holding full path to translated 
info file.")
(or logo-info-file-trans
    (setq logo-info-file-trans "/usr/local/info/ucbl-hr.info"))

(defvar logo-matching-delay-time 2 "Time to highlight parentheses matched 
region")
 
;; Try not to write programs with input prompts that look like
;; Logo REPL prompt -- `?' flush left with only one space following.
(defvar logo-busy-p nil "Is Logo idle or running a program?")

;; Width and height of emacs window for Logo inferior process are
;; set to 80 and 24, which is what Logo defaults to if it can not
;; get sizes from termcap. This is also vt100 default.
(defvar logo-term-columns nil "Width of terminal for setcursor mode.")
(defvar logo-term-rows nil "Height of terminal for setcursor mode.")
(or logo-term-columns
    (setq logo-term-columns 80))
(or logo-term-rows
    (setq logo-term-rows 24))

(defvar logo-mouse-pos nil "Mouse position.")

(defvar logo-standard-indent nil "By default RET runs newline & indent.")

(defvar logo-setcursor-start t "Setting setcursor as default term mode.")

(defvar logo-mode-hook nil
  "*Hook for customizing logo mode.")

(defun logo-initialize-hooks ()
  (make-local-hook 'view-mode-hook)
  (add-hook 'view-mode-hook 'logo-help-function)
  (add-hook 'view-mode-hook 'logo-view-exit-kludge)
  (make-local-hook 'post-command-hook)
  (setq post-command-hook nil)
  (add-hook 'post-command-hook 'logo-post-command-function)
  (add-hook 'kill-emacs-hook
	    (lambda ()
	      (or (not (file-readable-p logo-temp-file))
		  (delete-file logo-temp-file))))
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook
	    (lambda ()
	      (setq logo-edit-buflist
		    (delete (buffer-name)
			    logo-edit-buflist)))))

;;; ================ Key definitions & menus =================

(defun logo-mode-commands (map)

  ;; Next two could be dangerous
  (unwind-protect
      (progn
	;; Removing mule menu from global map
	(defvar logo-mode-mule-menu (copy-keymap mule-menu-keymap))
	(global-unset-key [menu-bar mule])))
  
  (unwind-protect
      (progn
	;; Removing tools menu from global map
	(defvar logo-mode-tools-menu (copy-keymap menu-bar-tools-menu))
	(global-unset-key [menu-bar tools])))

  (define-key map "\t" 'logo-indent-line)
  (if logo-standard-indent
      ;; gnu convention
      (define-key map "\C-j" 'newline-and-indent)
    ;; switching RET & LFD keys
    (define-key map "\C-j" 'newline)
    (define-key map "\C-m" 'newline-and-indent))
  ;; allegro cl
  (define-key map "\C-\M-q" 'logo-indent-definition)
  (define-key map "\M-;" 'logo-comment-region)
  ;; allegro cl
  (define-key map "\C-c;" 'logo-uncomment-region)
  ;; allegro cl
  (define-key map "\C-cl" 'logo-toggle-to-logo)
  (define-key map "\C-cb" 'logo-send-buffer)
  ;; cmuscheme48
  (define-key map "\C-cr" 'logo-send-region)
  (define-key map "\C-c\C-b" 'logo-send-buffer-and-go)
  ;; cmuscheme48
  (define-key map "\C-c\C-r" 'logo-send-region-and-go)
  ;; gnu convention
  (define-key map "\C-\M-x" 'logo-send-definition)
  ;; gnu convention (send sexp)
  (define-key map "\C-x\C-e" 'logo-send-instruction-line)
  (define-key map "\M-\t" 'logo-dabbrev-keywords)
  ;; this is just to have these keys present in mode description
  (define-key map "\M-/" 'logo-dabbrev-expand)
  (define-key map [?\e ?\^/] 'logo-dabbrev-completion)
  ;; mouse bindings
  (define-key map [down-mouse-2] 'mouse-set-point)
  (define-key map [down-mouse-3] 'logo-mouse-set-point)
  (define-key map [mouse-3] 'logo-help)
  
  ;; Menu bar
  ;; Logo-start menu
  (define-key map [menu-bar logo-start]
    (cons "Logo-start" (make-sparse-keymap "Logo-debug")))
  (define-key map [menu-bar logo-start run-logo-same-window]
    '("Run Logo Same Window" . run-logo-same-window))
  (define-key map [menu-bar logo-start run-logo-other-window]
    '("Run Logo Other Window" . run-logo-other-window))
  (define-key map [menu-bar logo-start run-logo-other-frame]
    '("Run Logo Other Frame" . run-logo-other-frame))
  
  ;; Logo-send menu
  (define-key map [menu-bar logo-send]
    (cons "Logo-send" (make-sparse-keymap "Logo-send")))
  (define-key map [menu-bar logo-send toggle-to-logo]
    '("Go to Logo" . toggle-to-logo))  
  (define-key map [menu-bar logo-send separator-send-1]
    '("--"))
  (define-key map [menu-bar logo-send logo-send-buffer]
    '("Send Buffer" . logo-send-buffer))
  (define-key map [menu-bar logo-send logo-send-region]
    '("Send Region" . logo-send-region))
  (define-key map [menu-bar logo-send logo-send-definition]
    '("Send Definition" . logo-send-definition))   
  (define-key map [menu-bar logo-send logo-send-instruction-line]
    '("Send Instruction" . logo-send-instruction-line))   
  (define-key map [menu-bar logo-send separator-send-2]
    '("--"))
  (define-key map [menu-bar logo-send logo-send-buffer-and-go]
    '("Send Buffer & Go" . logo-send-buffer-and-go))
  (define-key map [menu-bar logo-send logo-send-region-and-go]
    '("Send Region & Go" . logo-send-region-and-go))
  (define-key map [menu-bar logo-send logo-send-definition-and-go]
    '("Send Definition & Go" . logo-send-definition-and-go))   
  (define-key map [menu-bar logo-send logo-send-instruction-line-and-go]
    '("Send Instruction & Go" . logo-send-instruction-line-and-go))   

  ;; Logo-edit menu
  (define-key map [menu-bar logo-edit]
    (cons "Logo-edit" (make-sparse-keymap "Logo-edit")))
  (define-key map [menu-bar logo-edit logo-toggle-syntax-highlight]
    '("Toggle Syntax Highlighting" . logo-font-lock-mode))  
  (define-key map [menu-bar logo-edit logo-toggle-novice]
    '("Toggle Novice Management" . logo-toggle-novice))  
  (define-key map [menu-bar logo-edit separator-edit-1]
    '("--"))
  (define-key map [menu-bar logo-edit separator-double]
    '("--"))
  (define-key map [menu-bar logo-edit logo-uncomment-definition]
    '("Uncomment Definition" . logo-uncomment-definition))   
  (define-key map [menu-bar logo-edit logo-uncomment-region]
    '("Uncomment Region" . logo-uncomment-region))   
  (define-key map [menu-bar logo-edit logo-comment-definition]
    '("Comment Out Definition" . logo-comment-definition))   
  (define-key map [menu-bar logo-edit logo-comment-region]
    '("Comment Out Region" . logo-comment-region))   
  (define-key map [menu-bar logo-edit separator-edit-2]
    '("--"))
  (define-key map [menu-bar logo-edit logo-indent-buffer]
    '("Indent Buffer" . logo-indent-buffer)) 
  (define-key map [menu-bar logo-edit logo-indent-definition]
    '("Indent Definition" . logo-indent-definition))

  (unwind-protect
      (define-key map [menu-bar mule] (cons "Mule" logo-mode-mule-menu)))
  (unwind-protect
      (define-key map [menu-bar tools] (cons "Tools" logo-mode-tools-menu)))

  ;; Adding Logo help to emacs Help menu
  (and (memq 'help-menu menu-bar-final-items)
       (define-key global-map [menu-bar help-menu separator-l-help]
	 '("--"))
       (define-key global-map [menu-bar help-menu logo-help]
	 (cons "Logo-help" (make-sparse-keymap "Logo-help")))
       (define-key global-map [menu-bar help-menu logo-help logo-tutorial]
	 '("Tutorial" . logo-tutorial))
       (define-key global-map [menu-bar help-menu logo-help separator]
	 '("--"))
       ;; Include this if info translation set
       (if logo-info-trans
	   (define-key global-map [menu-bar help-menu logo-help logo-info-t]
	     (cons (concat "User Manual " logo-info-trans)
		   'logo-show-info-trans))
	 t)
       (define-key global-map [menu-bar help-menu logo-help logo-info]
	 '("User Manual" . logo-show-info))
       (define-key global-map [menu-bar help-menu logo-help logo-helpcontents]
	 '("Help Contents" . logo-show-helpcontents)))

    ;; Adding open new Logo file to emacs Files menu
  (define-key global-map [menu-bar files separator-logo-file]
    '("--"))
  (define-key global-map [menu-bar files logo-mode-files]
    (cons "Logo-mode Files" (make-sparse-keymap "Logo-mode-files")))
  (define-key global-map [menu-bar files logo-mode-files logo-mode-on]
    '("Convert to Logo-mode" . logo-mode-on))
  (define-key global-map [menu-bar files logo-mode-files logo-file]
    '("Open Logo File..." . logo-open-file)))

(defvar logo-mode-map nil)
(if (not logo-mode-map)
    (progn
      (setq logo-mode-map (make-sparse-keymap))
      (logo-mode-commands logo-mode-map)))

(setplist 'logo-tutorial
	  '(menu-enable
	    (and
	     (file-readable-p (concat logo-tutorial-path "tutorial.lg"))       
	     (equal major-mode 'logo-mode))))

(setplist 'logo-show-helpcontents
	  '(menu-enable
	    (and
	     (let ((helpcontents (get-buffer logo-helpcontents)))
	       (or (not helpcontents)
		   (not (get-buffer-window helpcontents t))))
	     (equal major-mode 'logo-mode))))

(setplist 'logo-show-info
	  '(menu-enable
	    (and
	     (let ((info-buf (get-buffer "*info*")))
	       (or (not info-buf)
		   (not (get-buffer-window info-buf t))))
	     (or (equal major-mode 'logo-mode)
		 (equal major-mode 'inferior-logo-mode)))))

(setplist 'logo-mode-on
	  '(menu-enable
	    (and
	     (equal major-mode 'fundamental-mode)
	     (not logo-debug-mode)
	     (not logo-helpcontents-mode)
	     (not logo-help-mode))))

(setplist 'make-frame-command
	  '(menu-enable
	     (equal major-mode 'logo-mode)))

(setplist 'logo-open-file
	  '(menu-enable
	     (equal major-mode 'logo-mode)))

(setplist 'logo-show-info-trans
	  '(menu-enable
	    (and
	     (let ((info-buf (get-buffer "*info*")))
	       (or (not info-buf)
		   (not (get-buffer-window info-buf t))))     
	     (or (file-readable-p logo-info-file-trans)
		 (file-readable-p
		  (concat logo-info-file-trans ".gz")))
	     (or (equal major-mode 'logo-mode)
		 (equal major-mode 'inferior-logo-mode)))))

(setplist 'run-logo-same-window
	  '(menu-enable (not (get-buffer-process logo-buffer))))
(setplist 'run-logo-other-window
	  '(menu-enable (not (get-buffer-process logo-buffer))))
(setplist 'run-logo-other-frame
	  '(menu-enable (not (get-buffer-process logo-buffer))))

(setplist 'logo-comment-region
	  '(menu-enable (mark)))
(setplist 'logo-uncomment-region
	  '(menu-enable (mark)))

(setplist 'logo-send-instruction-line-and-go
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not logo-busy-p)
		 (get-buffer-window "*logo*" t))))

(setplist 'logo-send-instruction-line
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not logo-busy-p))))

(setplist 'logo-send-definition-and-go
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not logo-busy-p)
		 (get-buffer-window "*logo*" t))))

(setplist 'logo-send-definition
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not logo-busy-p))))

(setplist 'logo-send-buffer-and-go
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not logo-busy-p)
		 (get-buffer-window "*logo*" t))))

(setplist 'logo-send-buffer
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not logo-busy-p))))

(setplist 'logo-send-region
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not logo-busy-p)
		 (mark))))

(setplist 'logo-send-region-and-go
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not logo-busy-p)
		 (mark)
		 (get-buffer-window "*logo*" t))))

(defalias 'toggle-to-logo 'logo-toggle-to-logo)
(put 'toggle-to-logo 'menu-enable '(get-buffer-process logo-buffer))
(defalias 'toggle-to-edit 'logo-toggle-to-logo)
(put 'toggle-to-edit 'menu-enable t)
(put 'toggle-to-logo 'menu-alias t)
(put 'toggle-to-edit 'menu-alias t)

;;; ======================= Novice package =======================

(defun logo-novice-commands (&optional enable)
  "Disabling `dangerous' commands."

  (let ((toggle (and (not enable) "\nYOU *PROBABLY* DON'T WANT THIS!\n\n"))
	(commands '( ;; List of emacs & comint commands to be disabled
		    
		    'kill-this-buffer
		    'make-frame-on-display
		    'ediff-files
		    'ediff-merge-files-with-ancestor
		    'ediff-merge-buffers
		    'ediff-merge-buffers-with-ancestor
		    'ediff-merge-directories
		    'ediff-merge-directories-with-ancestor
		    'ediff-merge-revisions
		    'ediff-merge-revisions-with-ancestor
		    'ediff-merge-directory-revisions
		    'ediff-merge-directory-revisions-with-ancestor
		    'ediff-patch-file
		    'ediff-patch-buffer
		    'ediff-documentation
		    'ediff-show-registry
		    'ediff-toggle-multiframe
		    'vc-register
		    'vc-directory
		    'gnus
		    'rmail
		    'compose-mail
		    'calendar
		    'grep
		    'compile
		    'gdb
		    'facemenu-set-read-only
		    'facemenu-set-invisible
		    'facemenu-set-intangible
		    'facemenu-remove-special
		    'set-justification-none
		    'set-justification-left
		    'set-justification-right
		    'set-justification-full
		    'set-justification-center
		    'increase-left-margin
		    'decrease-left-margin
		    'increase-right-margin
		    'decrease-right-margin
		    'facemenu-remove-face-props
		    'facemenu-remove-all
		    'list-text-properties-at
		    'list-faces-display
		    'list-colors-display
		    'find-tag
		    'toggle-input-method
		    'set-input-method
		    'set-buffer-file-coding-system
		    'universal-coding-system-argument
		    'set-selection-coding-system
		    'set-next-selection-coding-system
		    'view-hello-file
		    'toggle-global-lazy-font-lock-mode
		    'toggle-text-mode-auto-fill
		    'toggle-uniquify-buffer-names
		    'toggle-debug-on-error
		    'toggle-debug-on-quit
		    'comint-dynamic-complete
		    'comint-dynamic-complete-filename
		    'comint-replace-by-expanded-filename
		    
		    )))

    ;; Really -- there should be a `foreach' to do this!
    (mapcar (lambda (x)
	      (eval (list 'put x ''disabled toggle)))
	    commands)
    (message (concat "Novice command management "
		     (if enable
			 "Disabled!"
		       "Enabled!")))))

(and logo-novice-management
     (logo-novice-commands))

(defun logo-toggle-novice ()
  "Toggle novice commands management."
  (interactive)
  (if logo-novice-management
      (progn
	(setq logo-novice-management nil)
	(logo-novice-commands t))
    (setq logo-novice-management t)
    (logo-novice-commands)))

;;; ======================= dabbrev stuff =======================

(defvar dabbrev-search-these-buffers-only nil "")

;; Making sure that completion window is deleted
(define-key completion-list-mode-map "\C-m" 'logo-choose-completion)
(define-key completion-list-mode-map [mouse-2] 'logo-mouse-choose-completion)

(if (get-buffer " *logo-names*")
    nil
  (save-excursion
    (set-buffer (get-buffer-create " *logo-names*"))
    (insert-file-contents (concat logo-help-path logo-helpcontents))
    (toggle-read-only 1)
    (skip-chars-forward "^:")
    (beginning-of-line 2)
    (narrow-to-region (point) (point-max))))

(defun logo-dabbrev-setup ()
  "Setting variables for dynamic abbreviation expansion."
  (set (make-local-variable 'dabbrev-abbrev-skip-leading-regexp) "[:\"]")
  (set (make-local-variable 'dabbrev-case-fold-search) nil)
  (set (make-local-variable 'dabbrev-case-replace) nil)
  (set (make-local-variable 'dabbrev-check-all-buffers) nil)
  (set (make-local-variable 'dabbrev-search-these-buffers-only) nil)
  (set (make-local-variable 'dabbrev-select-buffers-function)
       (function (lambda nil (mapcar 'get-buffer logo-edit-buflist)))))

(defun logo-dabbrev-keywords ()
  "Try to expand the word at point, looking for completions in Logo 
HELPCONTENTS file. If not unique, show a window with all possible completions."
  (interactive)
  (unwind-protect
      (progn
	(setq dabbrev-search-these-buffers-only
	      (list (get-buffer " *logo-names*")))
	(dabbrev-completion))
    (setq dabbrev-search-these-buffers-only nil)))

(defun logo-mouse-choose-completion (event)
  "Delete completions window if not in `Logo other window' mode." 
  (interactive "@e")
  (mouse-choose-completion event)
  (or logo-switch-buffer
      (condition-case nil
	  (delete-completion-window)
	(error nil))))

(defun logo-choose-completion ()
  "Delete completions window if not in `Logo other window' mode." 
  (interactive)
  (choose-completion)
  (or logo-switch-buffer
      (condition-case nil
	  (delete-completion-window)
	(error nil))))

;; The next two are here just to show logo specific help in mode help.

(defun logo-dabbrev-expand (&optional dummy)
  "Expand the word at point, looking for completions in all logo-mode
buffers. Skip `\"' and `:' at the beginning of a word."
  (interactive)
  (dabbrev-expand dummy))

(defun logo-dabbrev-completion ()
  "Try to expand the word at point, looking for completions in all logo-mode
buffers. If not unique, show a window with all posible completions."
  (interactive)
  (dabbrev-completion))

;;; ======================= Tutorial =============================

(defun logo-tutorial ()
  "Start logo-mode tutorial."
  (interactive)
  (let ((tutorial (concat logo-tutorial-path "tutorial.lg"))
	(tutor1 (concat logo-tutorial-path "tutor1.lg")))
    (if (file-readable-p tutorial)
	(progn
	  (if (file-readable-p tutor1)
	      (progn
		;; Just copying tutor1.lg to ~/ directory
		(find-file tutor1)
		(write-file (substitute-in-file-name "$HOME"))
		(kill-buffer (current-buffer))))
	  (find-file tutorial)
	  (write-file (substitute-in-file-name "$HOME")))
      (error "Can't find tutorial.lg file -- see logo-tutorial-path variable"))))
 
;;; ======================= We start here ========================

;;;###autoload
(defun logo-mode ()
  "Major mode for editing Logo source code.

Important stuff to remember while writing Logo source in emacs logo mode:

You can leave blank lines inside Logo procedure definitions, and you do
not have to leave blank lines between two definitions. This last one is 
unwise, though -- you will speed things up if you do.

However, you can *not* leave blank lines inside Logo string constants. This 
will cause problems with indenting and procedure sending code.

Most of usual Lisp/Scheme mode commands are implemented here.

You can: 
Indent a line
Indent a procedure definition
Comment out a region
Uncomment a region
Send region to Logo (and go)
Send buffer to Logo (and go)
Send procedure to Logo (and go)
Send instruction line to Logo (and go)
Complete Logo name, based on names in HELPCONTENTS file.
Complete Logo name, based on names you used before.
Toggle between logo-edit and logo-process buffers 
       (with prefix toggles to older edit-buffers, if more than one exist)
Help for `procedure' is invoked by clicking mouse-3 on `procedure'
                                      
\\{logo-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (logo-mode-variables)
  (logo-mode-initialize)
  (logo-initialize-hooks)
  (run-hooks 'logo-mode-hook))

(defun logo-mode-initialize ()
  ;; Making sure that display is set the way Logo expects it!
  ;; Need one more to put 80 in one row
  ;; If frame height less than logo-term-rows, enlarging it!
  (set-frame-width (window-frame (selected-window))
		   (1+ logo-term-columns)
		   logo-term-columns)
  (setq pop-up-frame-alist
	(list (cons 'width (1+ logo-term-columns))
	      (cons 'height (+ logo-term-rows 2))))
  (let ((height (frame-height)))
    (or (>= height (+ logo-term-rows 2))
	(set-frame-height (window-frame (selected-window))
			  (+ logo-term-rows 2))))
  ;; Overriding any user settings
  ;; In Mandrake Linux 6.1 some joker decided to set transient-mode-mark
  ;; globally, and that is NOT a good idea!
  (setq transient-mark-mode nil)
  (setq truncate-lines nil)
  (use-local-map logo-mode-map)
  (setq major-mode 'logo-mode)
  (setq mode-name "Logo")
  (add-to-list 'logo-edit-buflist (buffer-name))
  (logo-dabbrev-setup)
  ;; Starting syntax highlighting
  (and logo-syntax-highlight
       (logo-font-lock-mode 1)))

(defun logo-mode-on ()
  "Changing buffer major mode to logo-mode, and inserting mode line."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (unwind-protect
	  (progn
	    (newline)
	    (goto-char (point-min))
	    (delete-blank-lines)
	    (insert-before-markers ";;; -*- logo -*- \n\n"))
	(logo-mode)))))

(defun logo-font-lock-mode (&optional arg)
  "Just making sure that font-lock-mode docstring doesn't show.
There must be a better way to do this."
  (interactive)
  (font-lock-mode arg)
  (setq minor-mode-alist (delete '(font-lock-mode nil) minor-mode-alist)))

;;; ================= Opening new edit frame ===================

;; Pacifying compiler
(defvar dired-mode-map nil)
(defun dired-mouse-find-file-other-window (e) nil)

(add-hook 'dired-load-hook
	  (lambda ()
	    (define-key
	      dired-mode-map
	      [mouse-2]
	      'logo-mouse-find-file-this-window))
	  "Rebinding mouse-2 to run find-file-this-window in dired.")

(defun logo-mouse-find-file-this-window (event)
  (interactive "@e")
  (let ((dired-window (get-buffer-window (current-buffer)))) 
    (dired-mouse-find-file-other-window event)
    ;; Kill dired window
    (delete-window dired-window)
    ;; This is for other-window mode only
    (and logo-switch-buffer
	 (logo-switch-buffer "*logo*")
	 (other-window 1)))
  ;; This is an odd-ball. Mouse pointer remains as a cross???
  (set-mouse-color (assoc "mouse-color" (frame-parameters))))

(defun logo-open-file ()
  "Opening another Logo file.
Using the same mode used to start Logo (same/other window, other frame),
if user did start Logo. If not, default to other frame." 
  (interactive)
  (or logo-buffer (setq logo-frames t))
  (and logo-frames
	 (select-frame (make-frame-command)))
  (dired default-directory))
	 
;;; ================= Starting process stuff ===================

(let ((logo-inferior-proc (get-process "logo")))
  (if (and (not logo-buffer)
	   logo-inferior-proc)
      (setq logo-buffer
	    (process-buffer logo-inferior-proc))))

(defun logo-proc ()
  (let ((proc (get-buffer-process logo-buffer)))
    (or proc
	(error "No current process -- start Logo first"))))

(defun run-logo-same-window ()
  "Start process Logo in the same window."
  (interactive)
  (setq logo-buffer nil)
  (setq logo-frames nil)
  (fset 'logo-switch-buffer 'switch-to-buffer)
  (setq logo-switch-buffer nil)
  ;; Don't start in setcursor mode
  (setq logo-setcursor-start nil)
  (run-logo))

(defun run-logo-other-window ()
  "Start process Logo in the second window."
  (interactive)
  (setq logo-buffer nil)
  (setq logo-frames nil)
  (fset 'logo-switch-buffer 'pop-to-buffer)
  (setq logo-switch-buffer t)
  ;; Don't start in setcursor mode
  (setq logo-setcursor-start nil)
  (run-logo))

(defun run-logo-other-frame ()
  "Start process Logo in the second frame."
  (interactive)
  (setq logo-buffer nil)
  (setq logo-frames t)
  (fset 'logo-switch-buffer 'pop-to-buffer)
  (setq logo-switch-buffer nil)
  (run-logo))

(defmacro logo-popup-frames (pred &rest subexprs)
  "Enable popping frames temporary."
  `(progn
     (setq pop-up-frames ,pred)
     ,@subexprs
     (setq pop-up-frames nil)))
							 
;;; ========================== Help code ==========================
;;
;;; Logo-helpcontents minor mode

;; This is here just to avoid compilation warnings - View mode kludge
(defvar view-mode nil)
(defvar view-overlay nil)
(defvar view-mode-auto-exit nil)
(defvar view-old-buffer-read-only nil)
(defvar view-exit-position nil)
(defvar view-exit-action nil)
(defvar view-return-here nil)
(defvar view-return-to-alist nil)
(defun view-exit nil)
(defun View-kill-and-leave nil)
(defun view-mode-exit (&optional a b c))


(defvar logo-helpcontents-mode nil
  "Mode variable for logo-helpcontents minor mode.")
(make-variable-buffer-local 'logo-helpcontents-mode)

(defvar logo-helpcontents-mode-map nil)
(if logo-helpcontents-mode-map
    nil
  (setq logo-helpcontents-mode-map (make-sparse-keymap))
  ;; Overriding `dangerous' keys in View-mode
  (define-key logo-helpcontents-mode-map "E" 'ignore)
  (define-key logo-helpcontents-mode-map "e" 'ignore)
  (define-key logo-helpcontents-mode-map "Q" 'ignore)
  (define-key logo-helpcontents-mode-map "c" 'ignore)
  (define-key logo-helpcontents-mode-map "C" 'ignore)
  ;; mouse bindings
  (define-key logo-helpcontents-mode-map [down-mouse-3] 'logo-mouse-set-point)
  (define-key logo-helpcontents-mode-map [mouse-3] 'logo-help)
  ;; the other two are here just to disable beeping
  (define-key logo-helpcontents-mode-map [down-mouse-1] 'logo-mouse-set-point)
  (define-key logo-helpcontents-mode-map [mouse-1] 'logo-help)
  (define-key logo-helpcontents-mode-map [down-mouse-2] 'logo-mouse-set-point)
  (define-key logo-helpcontents-mode-map [mouse-2] 'logo-help))

(defun logo-helpcontents-mode ()
  "Logo helpcontents minor mode.

Click with mouse (all 3 buttons work) on any Logo name in HELPCONTENTS
to choose help for that procedure.

Hit `q' to return to Logo edit buffer.

\\{logo-helpcontents-mode-map}"
  (interactive)
  (logo-helpcontents-mode-initialize)
  (setq logo-helpcontents-mode t))

(defun logo-helpcontents-mode-initialize ()
  (or (assq 'logo-helpcontents-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'logo-helpcontents-mode logo-helpcontents-mode-map)
		minor-mode-map-alist)))
  (if (not (assq 'logo-helpcontents-mode minor-mode-alist))
      (setq minor-mode-alist
	    (cons '(logo-helpcontents-mode " Logo-helpcontents")
		  minor-mode-alist)))
  ;; Logo specific word constituents
  (modify-syntax-entry ?. "w   ")
  (modify-syntax-entry ?: "w   ")
  (modify-syntax-entry ?? "w   ")
  (modify-syntax-entry ?` "w   "))


;;; Logo-help minor mode


(defvar logo-help-mode nil
  "Mode variable for Logo-help minor mode.")
(make-variable-buffer-local 'logo-help-mode)

(defvar logo-help-mode-map nil)
(if logo-help-mode-map
    nil
  (setq logo-help-mode-map (make-sparse-keymap))
  ;; Overriding `dangerous' keys in View-mode
  (define-key logo-help-mode-map "E" 'ignore)
  (define-key logo-help-mode-map "e" 'ignore)
  (define-key logo-help-mode-map "Q" 'ignore)
  (define-key logo-help-mode-map "c" 'ignore)
  (define-key logo-help-mode-map "C" 'ignore)
  ;; mouse bindings - actually, allowing only marking region with mouse-1
  (define-key logo-help-mode-map [down-mouse-3] 'logo-mouse-set-point)
  (define-key logo-help-mode-map [mouse-3] 'logo-mouse-set-point)
  (define-key logo-help-mode-map [down-mouse-1] 'mouse-drag-region)
  (define-key logo-help-mode-map [mouse-1] 'mouse-set-point)
  (define-key logo-help-mode-map [down-mouse-2] 'logo-mouse-set-point)
  (define-key logo-help-mode-map [mouse-2] 'logo-mouse-set-point))

(defun logo-help-mode ()
  "Logo help minor mode.

Hit `q' to return to Logo edit buffer, or to HELPCONTENTS buffer - if started
there.

Drag mouse-1 to mark region for copying into edit buffer.

\\{logo-help-mode-map}"
  (interactive)
  (logo-help-mode-initialize)
  (setq logo-help-mode t))

(defun logo-help-mode-initialize ()
  (or (assq 'logo-help-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'logo-help-mode logo-help-mode-map)
		minor-mode-map-alist)))
  (if (not (assq 'logo-help-mode minor-mode-alist))
      (setq minor-mode-alist
	    (cons '(logo-help-mode " Logo-help")
		  minor-mode-alist))))

(defun logo-mouse-set-point (event)
  "set the point with down-mouse-n."
  (interactive "@e")
  (mouse-set-point event)
  (setq logo-mouse-pos (point)))

(defun logo-help (event)
  "Pops a window with help-file for procedure at the point."
  (interactive "@e")
  (mouse-set-point event)
  (let ((word (current-word t)))
    (if (or (not word)
	    (not (= (point) logo-mouse-pos)))
	(message "Mouse-3 invokes Logo Help -- point to `name' and try again.")
      (let* ((selected
	      (downcase
	       (cond ((string-match "^\\." word)
		      (replace-match "d" t t word))
		     ((string-match "\\." word)
		      (replace-match "d" t t word))
		     ((string-match "^[\":]" word)
		      (replace-match "" t t word))
		     ((string-match "\?\\>" word)
		      (replace-match "p" t t word))
		     (t word))))
	     (path (concat logo-help-path selected))
	     (double (get-buffer (concat selected " Help"))))
	(cond (double
	       (logo-popup-frames
		logo-frames
		(logo-switch-buffer double)))
	      ((file-readable-p path) 
	       (unwind-protect
		   (progn
		     (remove-hook 'view-mode-hook 'logo-debug-function)
		     (view-file path))
		 (add-hook 'view-mode-hook 'logo-debug-function)))
	      (t ;; Help file doesn't exist
	       (message "File: %s not found -- check helpcontents." selected)
	       (sit-for 0.5)
	       (logo-show-helpcontents)))))))

(defun logo-show-helpcontents ()
  "Handling help request for misspelled procedure, or activated by Logo-help
menu from emacs Help."
  (interactive)
  (let ((helpcontents
	 (concat logo-help-path logo-helpcontents)))
    (if (file-readable-p helpcontents)
	(let ((double (get-buffer logo-helpcontents)))
	  (if (and double
		   (get-buffer-window double t))
	      (logo-popup-frames
	       logo-frames
	       (logo-switch-buffer double))
	    (unwind-protect
		(progn
		  (remove-hook 'view-mode-hook 'logo-debug-function)
		  (view-file
		   (concat logo-help-path logo-helpcontents)))
	      (add-hook 'view-mode-hook 'logo-debug-function))))
      (error "File: %s missing" helpcontents))))

(defun logo-help-function ()
  "If in HELPCONTENTS turn on Logo-helpcontents mode.
If in Logo help file buffer, turn on Logo-help mode."
  (let ((buffer (buffer-name)))
    (cond
     ((string= buffer logo-helpcontents)
      ;; In HELPCONTENTS - turn on Logo-helpcontents mode
      (logo-helpcontents-mode))
     ;; Do nothing - emacs help or completions buffer - making doubly sure
     ((string= buffer "*Help*"))
     ((string= buffer " *Completions*"))
     ;; If buffer found in helpfiles this must be Logo help file
     ((file-readable-p
       (concat logo-help-path buffer))
      (rename-buffer (concat buffer " Help"))
      (logo-help-mode)))))

(defun logo-view-exit-kludge ()
  "If emacs 19 overloading view-exit function. If 20, overloading only
high level exit functions, and calling low-level `view-mode-exit' with
doctored parameters. Reason for 20, is view-mode default choice of return
buffer. I do not want HELPCONTENTS buffer displayed in more than one window."
  (if (< emacs-major-version 20)
      (fset 'view-exit 'logo-view-exit-19)
    ;; All this is handled by rebinding keymap - just making doubly sure
    (fset 'View-quit-all 'logo-view-exit-20)
    (fset 'View-leave 'logo-view-exit-20)
    (fset 'View-exit-and-edit 'logo-view-exit-20)
    (fset 'View-quit 'logo-view-exit-20)
    (fset 'View-exit 'logo-view-exit-20)
    (fset 'View-kill-and-leave 'logo-view-exit-20)))

(defun logo-view-exit-20 ()
  "Doctoring return-alist contents of view-mode, and calling view-mode-exit."
  (interactive)
  (let* ((helpcontents (get-buffer logo-helpcontents))
	 (double (and helpcontents
		      (get-buffer-window helpcontents t)
		      (equal helpcontents
			     (nth 2 (car view-return-to-alist)))))
	 ;; If return buffer is again HELPCONTENTS, remove it from the alist
	 (return-alist (if double
			   (cdr view-return-to-alist)
			 view-return-to-alist)))
    (view-mode-exit return-alist 'kill-buffer t)))

(defun logo-view-exit-19 ()
  "Exit from view-mode fix for emacs 19 - do not die on killed buffer."
  (interactive)
  (setq view-mode nil)
  (and view-overlay (delete-overlay view-overlay))
  (force-mode-line-update)
  (cond (view-mode-auto-exit
	 (setq buffer-read-only view-old-buffer-read-only)
	 (setq view-mode-auto-exit nil)

	 (goto-char view-exit-position)
	 (set-marker view-exit-position nil)

	 ;; Now do something to the buffer that we were viewing
	 ;; (such as kill it).
	 (let ((viewed-buffer (current-buffer))
	       ;; I don't care what the action was - kill it!
	       (action 'kill-buffer))
	   (cond
	    ((and (buffer-live-p view-return-here)
		  (let* ((helpcontents (get-buffer logo-helpcontents))
			 (double (and helpcontents
				      (get-buffer-window helpcontents t)
				      (equal helpcontents view-return-here))))
		    ;; Do not allow returning to HELPCONTENTS if already
		    ;; visible in another frame 
		    (not double)))
	     (switch-to-buffer view-return-here))
	    (t (switch-to-buffer
		(logo-hidden-buffer logo-edit-buflist))))
	   (if action (funcall action viewed-buffer))))))

(defun logo-hidden-buffer (edit-buffers)
  "Return hidden edit buffer."
  (cond ((null edit-buffers) nil)
	((get-buffer-window (car edit-buffers) t)
	 (logo-hidden-buffer (cdr edit-buffers)))
	(t (car edit-buffers))))

(defun logo-show-info ()
  "Starting info mode on ucblogo.info version of usermanual."
  (interactive)
  (let ((info-gz (concat logo-info-file ".gz")))
    (cond ((file-readable-p "/usr/local/lib/logo/info/ucblogo.info")
	   ;; First trying default path
	   (info "xx/../usr/local/lib/logo/info/logo.info"))
	  ((file-readable-p logo-info-file)
	   ;; Forcing absolute path with `xx/../'. Why is this needed
	   ;; is beyond me?
	   (info (concat "xx/../" logo-info-file)))
	  ((file-readable-p info-gz)
	   (info (concat "xx/../" info-gz)))
	  (t (error "file: %s not found -- see variable `logo-info-file'"
		    logo-info-file)))))

(defun logo-show-info-trans ()
  "Starting info mode on translated ucblogo.info version of usermanual."
  (interactive)
  (let ((info-gz (concat logo-info-file-trans ".gz")))
    (cond ((file-readable-p logo-info-file-trans)
	   ;; Forcing absolute path with `xx/../'. Why is this needed
	   ;; is beyond me?
	   (info (concat "xx/../" logo-info-file-trans)))
	  ((file-readable-p info-gz)
	   (info (concat "xx/../" info-gz)))
	  (t (error "file: %s not found -- see variable `logo-info-file-trans'"
		    logo-info-file-trans)))))

;;; ========================== Indenting code ==========================

(defsubst logo-in-string-p (&optional def-start)
  "Is beginning of this line in string?"
  (let* ((line (point))
	 ;; Limit search to the beginning of enclosing paragraph or
	 ;; start of Logo definition
	 (search-limit (or def-start
			   (progn (backward-paragraph)
				  (point)))))
    (goto-char search-limit)
    (nth 3 (parse-partial-sexp (point) line))))

(defsubst logo-jump-to-next-word ()
  (let ((start (point))
	(end (progn (skip-syntax-forward "^<>") (point))))
    (goto-char start)
    (forward-word 1)
    (if (and (> (point) end)
	     (not (logo-in-string-p)))
	(goto-char start))
    (skip-syntax-forward " " end)))

(defun logo-indent-line (&optional no-instring def-start)
  "If explicit or implicit continuation line above,
then indent, otherwise flush left. First optional input must be t or nil;
t if you want to skip in-string checking for x-continuation lines. Second
optional input is limit for unbalanced brackets search."
  (interactive)
  (let ((indent (logo-indent-p no-instring def-start)))
    (save-excursion
      (beginning-of-line)
      ;; If in a string - do not remove leading spaces
      (or (logo-in-string-p def-start)
	  (delete-horizontal-space))
      (if indent
	  (indent-to indent)))
    (and (zerop (current-column))
	 indent
	 (forward-char (current-indentation)))))

(defsubst logo-unbalanced-p (&optional def-start)
  "Is this line unbalanced?"
  (save-restriction
    (condition-case nil
	(let* ((line (progn
		       (beginning-of-line) (point)))
	       ;; Limit search to the value of logo-unbalanced-distance
	       ;; characters before the beginning of this line
	       (search-limit (or def-start
				 (- line logo-unbalanced-distance))))
	  (narrow-to-region
	   (if (< search-limit (point-min))
	       (point-min)
	     ;; Make sure that search-limit is not in the middle of a comment
	     (goto-char search-limit)
	     (beginning-of-line)
	     (point))
	   line)
	  (goto-char line)
	  (up-list -1)
	  (point))
      (error nil))))

(defsubst logo-xcontinuation-p (&optional no-instring def-start)
  "Are we on a line that ends with `~' and is not part of Logo string?"
  (and
   (looking-at logo-xcontinuation-line)
   (or no-instring
       ;; Is end of this line part of a Logo string?
       (let ((pos (point)))
	 (end-of-line)
	 (prog1
	     (not (logo-in-string-p def-start))
	   (goto-char pos))))))

(defun logo-indent-p (&optional no-instring def-start)
  "Is this a line after ~ or after unmatched `[ ( {' ?
Strings that span several lines are not indenting."
  (save-excursion
    (beginning-of-line)
    (and (not (bobp))
	 (forward-line -1)
	 (or (and (logo-xcontinuation-p no-instring def-start)
		  (logo-xtab))
	     ;; Implicit indentation stuff
	     (logo-itab            
	      (point)
	      (progn
		(forward-line 1)
		(skip-syntax-forward " ")
		(following-char))
	      (logo-unbalanced-p def-start))))))

(defun logo-itab (prev-line next-char paren-pos)
  "Calculate implicit indent column."
  (if paren-pos
      (let ((open-char (following-char))
	    (indent
	     (cond ((looking-at "\\[[ \t]*$")
		    (logo-long-line prev-line next-char))
		   ((or (forward-char)
			(looking-at "[ \t]*\\s\("))
		    (progn
		      (skip-syntax-forward " ")
		      (current-column)))
		   ((logo-indent-to-paren next-char (point)))
		   (t (goto-char paren-pos)
		      (logo-jump-to-next-word)
		      (current-column)))))
	indent)))
			
(defun logo-long-line (prev-line next-char)
  "Start `long line' indent if next-char = ]."
  (cond ((char-equal next-char ?\])
	 (goto-char prev-line)
	 (max 0 (- (current-indentation) logo-min-indent)))
	((< next-char 14)
	 (setq logo-reindent-now t)
	 (current-indentation))
	(t (+ (current-indentation) logo-min-indent))))

(defun logo-indent-to-paren (next-char after-paren)
  "Indent to inner balanced parens, if any."
  (save-restriction
    (condition-case nil
	(progn
	  (narrow-to-region after-paren (progn
					  (end-of-line)
					  (point)))
	  (goto-char after-paren)
	  (down-list 1)
	  (widen)
	  (if (char-equal (preceding-char) ?\()
	      (progn
		(cond ((char-equal (char-syntax next-char) ?\()
		       (backward-char))
		      ((< next-char 14)
		       (setq logo-reindent-now t)
		       (goto-char after-paren))
		      (t (goto-char after-paren)
			 (logo-jump-to-next-word)))
		(current-column))
	    (goto-char after-paren)
	    (logo-jump-to-next-word)
	    (current-column)))
      (error nil))))

(defun logo-xtab ()
  "Calculate explicit indent - beginning of second word."
  (let ((indent (current-indentation)))
    (if (not (zerop indent))
	(min indent logo-max-indent)
      (logo-jump-to-next-word)
      (min (current-column) logo-max-indent))))

(defun logo-post-command-function ()
  "Re-indent after inserting first character, or flash matching opening 
parens after cursor movement commands."
  (and
   logo-reindent-now
   (cond ((eq this-command 'self-insert-command)
	  (and 
	   (char-equal (preceding-char) 32)
	   (message "Waiting for character other than space to re-indent."))
	  (setq logo-reindent-now nil)
	  (logo-indent-line))
	 ;; If anything else than following commands, cancel re-indenting
	 ((not (member 
		this-command
		'(logo-indent-line newline-and-indent overwrite-mode)))
	  (setq logo-reindent-now nil))))
  ;; Match opening parens when moving cursor horizontally/vertically
  (and
   logo-flash-on-movement
   (memq this-command
	 '(backward-char forward-char previous-line next-line
			 backward-delete-char-untabify))
   (memq (preceding-char) '(93 41 125))
   (unwind-protect
       ;; Bind temporarily matching delay to 2 seconds & face to bold
       (let ((blink-matching-delay logo-matching-delay-time)
	     (region-face 5))
	 (push-mark nil t)
	 ;; Show the region
	 ;; This is very cheap, but will have to do - for now.
	 (setq transient-mark-mode t)
	 (blink-matching-open))
     (pop-mark)
     ;; We're back to normal
     (setq transient-mark-mode nil))))

(defun logo-indent-definition ()
  "Indent Logo instruction or definition! First try to find boundaries of
definition above the point. If that fails indent paragraph.
Point must be inside or directly under definition!"
  (interactive)
  (save-excursion
    (let ((search-limit (and (logo-mark-definition)
			     (point))))
      (if (looking-at "[ \t]*$")
	  (forward-line 1))
      (while (and (<= (point) (mark))
		  (not (eobp)))
	(logo-indent-line nil search-limit)
	(forward-line 1))))
  (logo-indent-line)
  (and (zerop (current-column))
       (skip-syntax-forward " ")))

(defsubst logo-defun-start-p (&optional dummy)
  "Is this a start of Logo definition."
  (and (looking-at logo-defun-start)
       (not (logo-indent-p t))
       (not (logo-in-string-p))))

(defsubst logo-defun-end-p (&optional dummy)
  "Is this the end of Logo definition."
  (and (looking-at logo-defun-end)
       (not (logo-indent-p t))
       (not (logo-in-string-p))))

(defun logo-mark-definition ()
  "Finds the boundaries of Logo procedure definition. Marks the end and
leaves point at the beginning. Return value is t or nil"
  (beginning-of-line)
  ;; Up to the first nonempty line
  (while (and (not (bobp))
	      (looking-at "[ \t]*$"))
    (beginning-of-line 0))
  (let ((begin (point)))
    ;; Look for first boundary
    (while (and (not (bobp))
		(not (logo-defun-start-p))
		(not (logo-defun-end-p)))
      (beginning-of-line 0))
    (cond ((looking-at logo-defun-end)
	   ;; END found
	   (if (= (point) begin)
	       ;; OK - there was nothing below END
	       (progn
		 (end-of-line)
		 (push-mark nil t t)
		 (logo-mark-start begin))
	     ;; There was something below END - mark that
	     (beginning-of-line 2)
	     (let ((limit (point)))
	       (goto-char begin)
	       (mark-paragraph)
	       (and (< (point) limit)
		    (goto-char limit))
	       (message "No Logo definition at point -- grabbing paragraph.")
	       nil)))
	  ((looking-at logo-defun-start)
	   ;; TO or .MACRO found
	   (logo-mark-end (point) begin))
	  (t ;; This can only be bobp
	   (goto-char begin)
	   (mark-paragraph)
	   (if (zerop (- (mark) (point)))
	       (message "Nothing found.")
	     (message "No Logo definition at point -- grabbing paragraph."))
	   nil))))
	   
(defun logo-mark-start (begin)
  "After the mark is set at the end of definition, look for start."
  (beginning-of-line 0)
  (while (and (not (bobp))
	      (not (logo-defun-start-p))
	      (not (logo-defun-end-p)))
    (beginning-of-line 0))
  (cond ((looking-at logo-defun-start))
	((bobp)
	 (message "No Logo definition at point -- grabbing paragraph.")
	 (goto-char begin)
	 (mark-paragraph)
	 nil)
	(t ;; End again?
	 (message "No Logo definition at point -- grabbing paragraph.")
	 (goto-char begin)
	 (mark-paragraph)
	 nil)))

(defun logo-mark-end (defun-start begin)
  "Point is at the start of definition, look for end."
  (goto-char begin)
  (beginning-of-line 2)
  (while (and (not (eobp))
	      (not (logo-defun-end-p))
	      (not (logo-defun-start-p)))
    (beginning-of-line 2))
  (cond ((looking-at logo-defun-end)
	 (end-of-line)
	 (push-mark nil t t)
	 (goto-char defun-start)
	 t)
	((eobp)
	 (message "No Logo definition at point -- grabbing paragraph.")
	 (goto-char begin)
	 (mark-paragraph)
	 nil)
	(t ;; Start of Logo definition again?
	 (message "No Logo definition at point -- grabbing paragraph.")
	 (goto-char begin)
	 (mark-paragraph)
	 nil)))

(defun logo-indent-buffer (&optional beg end)
  "Indent the whole buffer - this is mostly for old Logo code & testing.
Also indents region - used for commenting."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((start (or beg (point-min)))
	    (stop (or end (point-max))))
	(goto-char start)
	(while (and (not (eobp))
		    (not (> (point) stop)))
	  (logo-indent-line)
	  (forward-line))))))

;;; ================= Commenting - Uncommenting =================

(defun logo-comment-region (beg end)
  "Comment out selected region."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (logo-comment-helper
     (lambda ()
       (if (char-equal (following-char) ?\;)
	   (insert-char ?\; 1)
	 (insert "; ")))
     (count-lines beg end))))

(defun logo-comment-helper (func lines)
  (if (zerop lines)
      nil
    (funcall func)
    (beginning-of-line 2)
    (logo-comment-helper func (1- lines))))

(defun logo-uncomment-region (beg end)
  "Uncomment selected region - one column only."
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (logo-comment-helper
     (lambda ()
       (if (char-equal (following-char) ?\;)
	   (delete-char 1)))
     (count-lines beg end))
    (logo-indent-buffer beg end)))

(defun logo-comment-definition ()
  "Comment out Logo definition, or maybe paragraph.
Point must be inside definition!"
  (interactive)
  (save-excursion
    (logo-mark-definition)
    (if (looking-at "[ \t]*$")
	(forward-line 1))
    (logo-comment-region (point) (mark))))

(defun logo-uncomment-definition ()
  "Uncomment Logo definition, or maybe paragraph.
Point must be inside definition!"
  (interactive)
  (save-excursion
    (logo-mark-definition)
    (if (looking-at "[ \t]*$")
	(forward-line 1))
    (if (char-equal (following-char) ?\;)
	(logo-uncomment-region (point) (mark))
      (message "Use Uncomment Region for that!"))))


;;; ================ Sending to Logo process ==================

(fset 'comint-send-string
      (lambda (proc string)
	  (setq logo-busy-p t)
	  (process-send-string proc string)))

(defun logo-send-buffer ()
  "Sends the contents of buffer to logo."
  (interactive)
  (logo-send-file (point-min) (point-max)))

(defun logo-send-buffer-and-go ()
  "Sends contents of buffer to logo and switches to logo process buffer."
  (interactive)
  (logo-send-file (point-min) (point-max))
  (and (get-buffer-window "*logo*" t)
       (logo-toggle-to-logo)))

(defun logo-send-string (string)
  "Strip newline and xcontinuation marks `~' from buffer string. 
This doesn't work for some bizarre situations like embedded newline in
continued string."   
  (comint-send-string
   (logo-proc)
   (concat "pr [] "
	   ;; avoiding multiple ? prompts
	   (logo-strip-newline
	    (logo-strip-xcont string))
	   "\n")))

(defun logo-strip-xcont (string)
  "Strip tilde and following newline if marking explicit continuation."
  (let ((xcont
	 (string-match "[^\\\\]\\(\\\\\\\\\\)*\\(~[ \t]*\\)\\(\n\\)" 
		       string)))
    (if xcont
	(or (and
	     ;; If match not in Logo string replace `~' with ""
	     (save-match-data
	       (zerop
		(% (logo-instring-count
		    (substring string 0 (1+ xcont))
		    0)
		   2)))
	     (logo-strip-xcont
	      (replace-match "" t t string 2)))
	    ;; If match in Logo string leave `~' & replace `\n' with " "
	    (logo-strip-xcont
	     (replace-match " " t t string 3)))
      string)))
	     
(defun logo-instring-count (string count)
  "Finding if end of string in Logo string. Actually only counting
number of bars before the end of string."
  (let ((bar (string-match "[^\\\\]\\(\\\\\\\\\\)*|" string)))
    (if bar
	(logo-instring-count (substring string (1+ bar)) (1+ count))
      count)))

(defun logo-send-region (start end)
  "Sends the contents of region to logo."
  (interactive "r")
  (logo-send-file start end))

(defun logo-send-region-and-go (start end)
  "Sends contents of region and switches to logo buffer."
  (interactive "r")
  (logo-send-file start end)
  (and (get-buffer-window "*logo*" t)
       (logo-toggle-to-logo)))

(defun logo-send-definition ()
  "Send procedure definition before point to logo."
  (interactive)
  (save-excursion
    (or (logo-mark-definition)
	(message "No Logo procedure at point -- sending paragraph."))
    (logo-send-file (point) (mark))))

(defun logo-send-definition-and-go ()
  "Send paragraph before point to Logo and go."
  (interactive)
  (logo-send-definition)
  (and (get-buffer-window "*logo*" t)
       (logo-toggle-to-logo)))

(defun logo-send-instruction-line ()
  "Send instruction line before point to Logo."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (and (not (bobp))
		(looking-at "[ \t]*$"))
      (beginning-of-line 0))
    (let* ((beg (point))
	   (start-line
	    (progn
	      (while (or (logo-indent-p t)
			 (logo-in-string-p))
		(beginning-of-line 0))
	      (if (or (looking-at logo-defun-start)
		      (looking-at logo-defun-end))
		  (error (concat
			  "Can not use `logo-send-instruction-line' "
			  "for defining logo procedure"))
		(point))))
	   (end-line
	    (progn
	      (goto-char beg)
	      (beginning-of-line 2)
	      (while (and (not (eobp))
			  (or (logo-indent-p t)
			      (logo-in-string-p)))
		(beginning-of-line 2))
	      (or (eobp)
		  (backward-char))
	      (point))))
      (condition-case nil
	  ;; Had a change of heart here. It is much easier to let Logo
	  ;; parse the string by loading it from a temporary file than
	  ;; having to parse it here, and pass it as a string.
	  ;; However the mechanism for parsing is still used for mouse-2
	  ;; yank in setcursor mode.
	  (prog1
	      (error)
	    (logo-send-string
	     (buffer-substring-no-properties start-line end-line)))
	(error (logo-send-file start-line end-line))))))

(defun logo-send-instruction-line-and-go ()
  "Send instruction line before point to Logo and go."
  (interactive)
  (logo-send-instruction-line)
  (and (get-buffer-window "*logo*" t)
       (logo-toggle-to-logo)))

(defun logo-send-file (start end &optional string)
  "Does the actual transfer via temporary file."
  (let ((contents (concat
		   "pr []\n"
		   (or string
		       (buffer-substring-no-properties start end))
		   "\n")))
    (condition-case nil
	(write-region contents t logo-temp-file nil 1)
      (error (error "Check write permission on %s file" logo-temp-file)))
    (comint-send-string (logo-proc) (concat
				     "load \""
				     logo-temp-file
				     "\n"))))

;;; =========================================================================
;;; ========================== Inferior process logo ========================
;;
;;  Logo process in a buffer. 
;;
;;  This is a customization of comint-mode (see comint.el)
;;
;;; =========================================================================

(defvar inferior-logo-mode-syntax-table nil "")

(if inferior-logo-mode-syntax-table
    nil
  (setq inferior-logo-mode-syntax-table (copy-syntax-table))
  (set-syntax-table inferior-logo-mode-syntax-table)
  (modify-syntax-entry ?\| "\"   ")
  (modify-syntax-entry ?\; "<   ")
  (modify-syntax-entry ?\" "_  p"))

(defvar logo-debug-file nil "Temporary file for logo contents output.")
(if (not logo-debug-file)
    (setq logo-debug-file
	  (make-temp-name "/tmp/DEBUG")))

(defvar logo-term-mode nil "Variable holding logo term mode.")
(defvar logo-setcursor-mode nil
  "Mode variable for logo-setcursor minor mode.")
(make-variable-buffer-local 'logo-setcursor-mode)

(defvar logo-setcursor-overwrite t 
  "Setting default behavior for setcursor mode.")

(defvar logo-input-marker nil "Marking end of input in Setcursor mode.")

(defvar logo-language 'standard "Controlling loading of .logo init file.")
(defvar logo-load-language t "Allow changes to Logo")

(defvar comint-input-sender nil
  "Variable holding function that sends input to Logo.")
(defvar logo-pad-function nil
  "variable holding function for padding.")

(make-variable-buffer-local 'mode-line-format)

(defun logo-crowded-mode-line (modeline)
  "Removing extra spaces from mode line."
  (cond ((null modeline) nil)
	((and (stringp (car modeline))
	      (string-match "[ ]+" (car modeline)))
	 (cons (replace-match " " t t (car modeline))
	       (logo-crowded-mode-line (cdr modeline))))
	(t (cons (car modeline)
		 (logo-crowded-mode-line (cdr modeline)))))) 

(defvar logo-ct-function nil
  "Variable holding CLEARTEXT function.")
(defun logo-startup-mode nil
  "Function defining default startup mode.")
(defvar logo-setcursor-function nil
  "Variable holding SETCURSOR function.")
(defvar working-comint-output-filter nil
  "Variable holding comint-output-filter function.")

(defun logo-inferior-mode-variables ()
  "Initialize logo-process variables."
  (setq mode-line-format (logo-crowded-mode-line mode-line-format))
  (setq logo-setcursor-mode nil)
  (setq indent-tabs-mode nil)
  ;; Preventing `next-line' command from inserting newline 
  (make-variable-buffer-local 'next-line-add-newlines)
  (setq next-line-add-newlines nil)
  (make-variable-buffer-local 'scroll-step)
  (setq comint-scroll-to-bottom-on-input nil)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-input-ignoredups t)
  ;; If this breaks input history, remove it. This is here for
  ;; setcursor mode only.
  (fset 'comint-previous-matching-input 'setcursor-previous-matching-input)
  (logo-dabbrev-setup)
  ;; Hook variables
  (make-local-hook 'comint-output-filter-functions)
  (setq comint-output-filter-functions nil)
  (add-hook 'comint-output-filter-functions 'logo-idle-function)
  (make-local-hook 'view-mode-hook)
  (add-hook 'view-mode-hook 'logo-debug-function)
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook
	    (lambda ()
	      ;; If this was debug buffer send debug request to Logo
	      (if (equal (buffer-name)
			 (file-name-nondirectory logo-debug-file))
		  (logo-send-debug))))
  (make-local-hook 'kill-buffer-query-functions)
  (setq kill-buffer-query-functions nil)
  (add-hook 'kill-buffer-query-functions
	    (lambda ()
	      ;; If killing Logo buffer ask first
	      (if (and (equal (buffer-name (current-buffer)) logo-buffer)
		       (get-buffer-process (current-buffer)))
		  (yes-or-no-p "You are killing Logo buffer? ")
		t))))
		  
(defun logo-inferior-mode-term ()
  "Setting emacs to behave like quasi Logo terminal."
  (interactive)
  (setq logo-setcursor-mode nil)
  (setq logo-input-marker nil)
  (setq comint-input-sender (function comint-simple-send))
  (setq logo-pad-function (function (lambda nil)))
  (setq logo-term-mode 'terminal)
  (setq mode-line-buffer-identification '("Terminal-mode: %6b"))
  (setq working-comint-output-filter (function comint-output-filter))
  (overwrite-mode -1)
  (add-hook 'comint-output-filter-functions
	       'comint-postoutput-scroll-to-bottom)
  (remove-hook 'comint-output-filter-functions 'logo-overwrite-function)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-scroll-show-maximum-output t)
  ;; Don't recenter while inserting input
  (setq scroll-step 1)
  (setq comint-get-old-input (function logo-get-old-input-default))
  (setq comint-eol-on-send t)
  (setq mouse-yank-at-point nil)
  (setq logo-setcursor-function (function (lambda (w x y z &optional p))))
  (setq logo-ct-function
	(function (lambda ()
		    (beginning-of-line)
		    (delete-region (point) (point-max))
		    (narrow-to-region (point) (point-max)))))
  (let ((proc (get-buffer-process logo-buffer)))
    (and proc
	 (comint-send-string proc "setmargins [0 0]\n"))))

(defun logo-inferior-mode-emacs ()
  "Emacs default behavior."
  (interactive)
  (setq logo-setcursor-mode nil)
  (setq logo-input-marker nil)
  (setq comint-input-sender (function comint-simple-send))
  (setq logo-pad-function (function (lambda nil)))
  (setq logo-term-mode 'emacs)
  (setq mode-line-buffer-identification '("Emacs-mode: %6b"))
  (setq working-comint-output-filter (function comint-output-filter))
  (overwrite-mode -1)
  (widen)
  (add-hook 'comint-output-filter-functions
	    'comint-postoutput-scroll-to-bottom)
  (remove-hook 'comint-output-filter-functions 'logo-overwrite-function)
  (setq comint-scroll-to-bottom-on-output t)
  ;; Recenter while inserting input
  (setq scroll-step 0)
  (setq comint-scroll-show-maximum-output nil)
  (setq comint-get-old-input (function logo-get-old-input-default))
  (setq comint-eol-on-send t)
  (setq mouse-yank-at-point nil)
  (setq logo-setcursor-function (function (lambda (w x y z &optional p))))
  (setq logo-ct-function
	(function (lambda ()
		    (beginning-of-line)
		    (delete-region (point) (point-max)))))
  (let ((proc (get-buffer-process logo-buffer)))
    (and proc
	 (comint-send-string proc "setmargins [0 0]\n"))))

(defvar inferior-logo-mode-hook nil
  "*Hook for customizing inferior logo mode.")

;;; ===================== Key and menu maps ========================

;; pacifying compiler with emacs 20.2
(defvar menu-bar-print-menu)

(defvar inferior-logo-mode-map nil)

(if inferior-logo-mode-map
    nil
  (setq inferior-logo-mode-map (copy-keymap comint-mode-map))

  ;; Restoring part of tools menu from global map
  (if (or (< emacs-major-version 20)
	  (< emacs-minor-version 3))
      nil
    ;; Restore print menu only if version > 20.2
    (unwind-protect
	(progn
	  (defvar logo-mode-print-menu (copy-keymap menu-bar-print-menu))
	  (define-key inferior-logo-mode-map [menu-bar print]
	    (cons "Print" logo-mode-print-menu)))))
  
  (define-key inferior-logo-mode-map "\C-cl" ;Use prefix for older buffers
    'logo-toggle-to-logo)
  ;; Disabling completion commands
  (define-key inferior-logo-mode-map "\M-\t" 'ignore)

  ;; Logo menu
  (define-key inferior-logo-mode-map [menu-bar logo-menu]
    (cons "Logo" (make-sparse-keymap "Logo")))

  (define-key inferior-logo-mode-map [menu-bar logo-menu toggle-to-edit]
    '("Go to Edit" . toggle-to-edit))
  (define-key inferior-logo-mode-map [menu-bar logo-menu separator]
    '("--"))

  ;; Logo Compiler menu
  (if (and logo-load-language
	   (let ((init-file (substitute-in-file-name "$HOME/.logo")))
	     (file-readable-p init-file)))
      (progn
	(define-key inferior-logo-mode-map [menu-bar logo-menu logo-compiler]
	  (cons "Compiler" (make-sparse-keymap "Compiler")))
	(define-key inferior-logo-mode-map 
	  [menu-bar logo-menu logo-compiler save-compiled]
	  '("Save compiled procedures ..." . logo-save-compiled))
	(define-key inferior-logo-mode-map 
	  [menu-bar logo-menu logo-compiler compiler]
	  '(".logo.compile  workspace" . logo-structure-compile))))

  ;; Logo language menu
  (if (and logo-load-language
	   (let ((init-file (substitute-in-file-name "$HOME/.logo")))
	     (file-readable-p init-file)))
      (progn
 	(define-key inferior-logo-mode-map [menu-bar logo-menu logo-language]
 	  (cons "Language" (make-sparse-keymap "logo-language")))
 	(define-key inferior-logo-mode-map 
 	  [menu-bar logo-menu logo-language logo-set-default]
 	  '("Set Startup Default" . logo-set-default-language))
	(define-key inferior-logo-mode-map
	  [menu-bar logo-menu logo-language separator]
	  '("--"))
 	(define-key inferior-logo-mode-map 
 	  [menu-bar logo-menu logo-language logo-dot-logo]
 	  '("Ucblogo + .logo" . logo-load-init-file))
 	(define-key inferior-logo-mode-map 
 	  [menu-bar logo-menu logo-language ucblogo-standard]
 	  '("Standard Ucblogo" . logo-remove-init-file))))

  ;; Terminal menu
  (define-key inferior-logo-mode-map [menu-bar logo-menu logo-term]
    (cons "Logo-term" (make-sparse-keymap "Logo-term")))
  (define-key inferior-logo-mode-map [menu-bar logo-menu logo-term setcursor]
    '("Setcursor" . logo-setcursor-mode))
  (define-key inferior-logo-mode-map [menu-bar logo-menu logo-term terminal]
    '("Terminal" . logo-inferior-mode-term))
  (define-key inferior-logo-mode-map [menu-bar logo-menu logo-term emacs]
    '("Emacs" . logo-inferior-mode-emacs))

  ;; Debug menu
  (define-key inferior-logo-mode-map [menu-bar logo-menu logo-debug]
    (cons "Logo-debug" (make-sparse-keymap "Logo-debug")))
  (define-key inferior-logo-mode-map
    [menu-bar logo-menu logo-debug logo-erase]
    '("Erase..." . logo-erase))
  (define-key inferior-logo-mode-map
    [menu-bar logo-menu logo-debug separator]
    '("--"))  
  (define-key inferior-logo-mode-map
    [menu-bar logo-menu logo-debug logo-unstep]
    '("Unstep all" . logo-unstep-all))
  (define-key inferior-logo-mode-map
    [menu-bar logo-menu logo-debug logo-untrace]
    '("Untrace all" . logo-untrace-all))
  (define-key inferior-logo-mode-map [menu-bar logo-menu logo-debug logo-step]
    '("Step..." . logo-step))
  (define-key inferior-logo-mode-map [menu-bar logo-menu logo-debug logo-trace]
    '("Trace..." . logo-trace))

  ;; Messing with comint menu-bar order - making sure Logo is last.
  (and (memq 'help-menu menu-bar-final-items)
       (setq menu-bar-final-items
	     (nconc
	      (delq 'help-menu menu-bar-final-items)
	      '(logo-menu help-menu)))))

(setplist 'logo-save-compiled
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not logo-busy-p)
		 (eq logo-language 'dot-logo))))

(setplist 'logo-structure-compile
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not logo-busy-p)
		 (eq logo-language 'dot-logo))))

(setplist 'logo-load-init-file
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not logo-busy-p)
		 (eq logo-language 'standard))))

(setplist 'logo-remove-init-file
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not logo-busy-p)
		 (eq logo-language 'dot-logo))))

(setplist 'logo-trace
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not logo-busy-p))))
	    
(setplist 'logo-step
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not logo-busy-p))))

(setplist 'logo-erase
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not logo-busy-p))))

(setplist 'logo-untrace-all
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not logo-busy-p))))

(setplist 'logo-unstep-all
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not logo-busy-p))))

(setplist 'logo-inferior-mode-term
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not (eq logo-term-mode 'terminal))
		 (not (member logo-system-type
			      '(safe raw test)))
		 (not logo-busy-p))))

(setplist 'logo-inferior-mode-emacs
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not (eq logo-term-mode 'emacs))
		 (not logo-busy-p))))

(setplist 'logo-setcursor-mode
	  '(menu-enable
	    (and (get-buffer-process logo-buffer)
		 (not (eq logo-term-mode 'setcursor))
		 (not (member logo-system-type
			      '(safe raw test)))
		 (not logo-busy-p))))

;; Can't use get-old-input in setcursor mode
(setplist 'comint-copy-old-input
 	  '(menu-enable (not logo-setcursor-mode)))
;; Disabling comint-kill-output in setcursor mode
(setplist 'comint-kill-output
 	  '(menu-enable (not logo-setcursor-mode)))
;; Disabling Complete menu in setcursor mode. Has no (or little)
;; value in Logo anyway.
(setplist 'comint-dynamic-complete
 	  '(menu-enable (not logo-setcursor-mode)))
(setplist 'comint-dynamic-complete-filename
  	  '(menu-enable (not logo-setcursor-mode)))
(setplist 'comint-dynamic-list-filename-completions
   	  '(menu-enable (not logo-setcursor-mode)))
(setplist 'comint-replace-by-expanded-filename
  	  '(menu-enable (not logo-setcursor-mode)))
;; Disabling replace operations in setcursor mode
(setplist 'query-replace
 	  '(menu-enable (not logo-setcursor-mode)))
(setplist 'query-replace-regexp
 	  '(menu-enable (not logo-setcursor-mode)))
;; Disabling Select and Paste in setcursor mode
(setplist 'menu-bar-select-yank
 	  '(menu-enable (not logo-setcursor-mode)))
;; Disabling List All Buffers in setcursor mode
(setplist 'list-buffers
 	  '(menu-enable (not logo-setcursor-mode)))
;; Disabling split-window-vertically in setcursor mode
(setplist 'split-window-vertically
 	  '(menu-enable (not logo-setcursor-mode)))

;; Disabling File menu entries in logo-inferior
(setplist 'find-file '(menu-enable (not (eq major-mode 'inferior-logo-mode)))) 
(setplist 'dired '(menu-enable (not (eq major-mode 'inferior-logo-mode)))) 
(setplist 'recover-session
	  '(menu-enable (not (eq major-mode 'inferior-logo-mode)))) 
(setplist 'insert-file
	  '(menu-enable (not (eq major-mode 'inferior-logo-mode)))) 
(setplist 'delete-frame
	  '(menu-enable (and (not (eq major-mode 'inferior-logo-mode))
			     (cddr (filtered-frame-list 'frame-live-p)))))
(setplist 'make-frame-on-display
	  '(menu-enable (not (eq major-mode 'inferior-logo-mode)))) 

;;; ====================== test code ==============================

; (define-key inferior-logo-mode-map "\M-\C-t" 'logo-test)

; (defun logo-test ()
;   (interactive)
;   (prin1 (cdr (cdr (filtered-frame-list 'frame-live-p)))))
    
;;; ====================== end test code ==========================

(defvar logo-system-type nil
  "Set this in .emacs if default filter does not work. 
Check the top of logo.el file for more detailed information.")

(defvar logo-filter-timer nil
  "Set this in .emacs if timing problems occur with default filter. 
Check the top of logo.el file for more detailed information.")

(defvar logo-standout-str nil "String matching STANDOUT code")
(defvar logo-setcursor-str nil "String matching SETCURSOR code")
(defvar logo-cleartext-str nil "String matching CLEARTEXT code")

(defvar logo-term-start nil "String matching start of TERM code")
(defvar logo-term-ct nil "String matching CLEARTEXT TERM code")
(defvar logo-term-all nil "String matching the rest of TERM code")

(defvar logo-code-kludge nil "Kludge for cut code in communications")
(defvar logo-term nil "TERM for logo process buffer.")

(defun logo-default-setup ()
  (cond ((eq logo-system-type 'safe)
	 ;; This is old -- safe code. Works for default comint terminals,
	 ;; dumb and emacs. Not as good as default filter. Can't handle
	 ;; SETCURSOR, and sometimes missing control characters -- if
	 ;; in the middle of split sequence.
	 (fset 'logo-startup-mode 'logo-inferior-mode-emacs) 
	 (if system-uses-terminfo
	     (progn
	       (setq logo-standout-str (concat "\\>" (char-to-string 1)))
	       (setq logo-setcursor-str
		     (concat (char-to-string 153)
			     (char-to-string 153)
			     (char-to-string 153)
			     (char-to-string 25)))
	       (setq logo-cleartext-str (char-to-string 1))
	       (fset 'logo-comint-output-filter 'logo-dumb-filter))
	   (setq logo-standout-str
		 (concat
		  "�ܿ�"
		  "\\([^��\]*\\)"
		  "����<ܿ�ܿ�"))
	   (setq logo-setcursor-str "`��ܿ�")
	   (setq logo-cleartext-str "����8ܿ�")
	   (fset 'logo-comint-output-filter 'logo-emacs-filter)))
	((eq logo-system-type 'raw)
	 ;; You have disabled filtering -- what you want is what you
	 ;; get! Raw output from Logo. This however, is not necessarily
	 ;; true with ucblogo4.2.
	 (fset 'logo-startup-mode 'logo-inferior-mode-emacs) 
	 (fset 'logo-comint-output-filter 'comint-output-filter))
	((eq logo-system-type 'vt100)
	 ;; Forcing `vt100' as TERM type, but using default filter
	 (logo-default-pars "vt100"))
	((eq logo-system-type 'xterm)
	 ;; Forcing `xterm' as TERM type, but using default filter
	 (logo-default-pars "xterm"))
	((eq logo-system-type 'test)
	 ;; Testing only --- use to grab TERM control codes
	 (setq logo-term (getenv "TERM"))
	 (fset 'logo-startup-mode 'logo-inferior-mode-emacs) 
	 (make-local-hook 'comint-mode-hook)
	 (add-hook 'comint-mode-hook 'logo-term-override)
	 (fset 'logo-comint-output-filter 'comint-output-filter))
	(t
	 ;; This is default code - overrides comint hardwired TERM
	 ;; and lets emacs decide what TERM to send to Logo.
	 (logo-default-pars (logo-term-switch (getenv "TERM"))))))

(defun logo-term-switch (term)
  "This, really isn't a free choice.
Apart from not being free - it doesn't work reliably. You will most
probably end with TERM set to xterm. The only other choice is vt100.
The real problem is with the window sizes. The size of emacs window 
that Logo inferior process is running in, and Logo's idea of its size
will probably differ. To fix this you will have to follow instructions
at the top of this file."
  (cond ((string= term "vt100") "vt100")
	(t "xterm")))

(defun logo-default-pars (term)
  "Setting parameters for filters.
Works for TERM types: linux, cons25, vt100 and xterm."
  (setq logo-term term)
  ;; This regex works for both vt100 and xterm
  (setq logo-term-ct
	    "\\(\\[H\\[2*J\\[\\([0-9]+\\);\\([0-9]+\\)H\\)\\|")
  (if logo-setcursor-start
      (fset 'logo-startup-mode 'logo-setcursor-mode)
    (fset 'logo-startup-mode 'logo-inferior-mode-term)) 
  (make-local-hook 'comint-mode-hook)
  (add-hook 'comint-mode-hook 'logo-term-override)
  (setq logo-term-start (char-to-string ?\^\[))
  (setq logo-term-all
	(concat logo-term-ct
		"\\(\\[\\([0-9]+\\);\\([0-9]+\\)H\\)\\|"
		"\\(\\[7m\\([^]*\\)\\[m\\)"))
  (fset 'logo-comint-output-filter 'logo-default-filter)
  (or logo-filter-timer
      (setq logo-filter-timer 2)))

(defun logo-term-override ()
  (fset 'comint-exec-1 'logo-comint-exec-1))

(defun logo-comint-exec-1 (name buffer command switches)
  "A bad hack of comint-exec-1 function.
I am overriding default `dumb' or `emacs' TERMs to get usable
terminal access codes from Logo -- mostly for setcursor minor mode."
    (let ((process-environment
	   (nconc 
	    (list (concat "TERM=" logo-term)
		  ;; This works only with ucblogo 4.7 or greater
		  (format "COLUMNS=%d" logo-term-columns)
		  (format "LINES=%d" logo-term-rows))
	    (if (getenv "EMACS") nil (list "EMACS=t"))
	    process-environment))
	  (default-directory
	    (if (file-directory-p default-directory)
		default-directory
	      "/")))
      (apply 'start-process name buffer command switches)))


(defun inferior-logo-mode ()
  "Major mode for interacting with an inferior Logo process.

The following commands are available:
\\{inferior-logo-mode-map}

Logo starts by default in setcursor mode. If you find this mode 
uncomfortable - and I would not be surprised if you do - switch to 
emacs or terminal modes with Logo-term menu."

  (interactive)
  (comint-mode)
  (setq comint-prompt-regexp "^[^\n]*\\(?\\|>\\|~\\|\|\\)+\\ +")
  (setq major-mode 'inferior-logo-mode)
  (setq mode-name "Inferior Logo")
  (setq mode-line-process '(": %s"))
  (use-local-map inferior-logo-mode-map)
  (set-syntax-table inferior-logo-mode-syntax-table)
  (logo-inferior-mode-variables)
  (set-process-filter (get-process "logo")
		      'logo-comint-output-filter)
  (logo-startup-mode)
  (run-hooks 'inferior-logo-mode-hook))

;;;###autoload
(defun run-logo ()
  (interactive)
  (setq pop-up-frames logo-frames)
  (logo-default-setup)
  (set-buffer (apply 'make-comint "logo" logo-binary-name nil nil))
  (inferior-logo-mode)
  (setq logo-buffer "*logo*")
  (logo-switch-buffer "*logo*")
  ;; Making sure that display is set the way Logo expects it!
  ;; Need one more to put 80 in one row
  (set-frame-width (window-frame (selected-window))
		   (1+ logo-term-columns)
		   logo-term-columns)
  ;; Maximize frame height if in two window operation
  (and logo-switch-buffer
       (let* ((pixels (x-display-pixel-height))
	      (usable-pix (if (> pixels 480)
			      (* 80 (/ pixels 100))
			    (- pixels 24)))
	      (lines (/ usable-pix (frame-char-height))))
	 (set-frame-height
	  (window-frame (selected-window))
	  ;; Make frame hight maximum safe size, or larger if user wants
	  ;; it that way, but don't make it larger than twice the standard
	  ;; user requested size
	  (max (min lines (* 2 (1+ logo-term-rows))) (1+ logo-term-rows)))))
  ;; If in two window operation stealing from edit window if need be
  ;; If frame is only 26 rows high, switching to one window operation
  (let ((height (window-height)))
    (or (>= height (1+ logo-term-rows))
	(enlarge-window (- (1+ logo-term-rows) height))))
  ;; Overriding any user settings
  (setq truncate-lines nil)
  (setq pop-up-frames nil)
  ;; Loading .logo - initialization file
  (and logo-load-language
       (eq logo-language 'dot-logo)
       (logo-load-init-file)))
  
;;;###autoload (add-hook 'same-window-buffer-names "*logo*")

;;; ===========================================================

(defun logo-load-init-file ()
  "Load .logo initialization file into Logo."
  (interactive)
  (let ((init-file (substitute-in-file-name "$HOME/.logo")))
    (and (file-readable-p init-file)
	 (setq logo-language 'dot-logo)
	 (comint-send-string (logo-proc)
			     (concat "load "
				     (logoize-path init-file)
				     "\n")))))

(defun logo-remove-init-file ()
  "Restore Berkeley Logo Standard language."
  (interactive)
  (setq logo-language 'standard)
  (comint-send-string (logo-proc)
		      "restore.standard erase \"restore.standard \n"))

(defun logo-set-default-language ()
  "Set default language level on startup."
  (interactive)
  (and (yes-or-no-p
	(concat "Do you want to make "
		(if (eq logo-language 'standard)
		    " Standard Ucblogo  "
		  " Ucblogo + .logo  ")
		"default startup language?"))
       (if (eq logo-language 'standard)
	   (logo-edit-dot-emacs
	    "^[ \t]*([ \t]*setq[ \t]+logo-language[ \t]+'dot-logo[ \t]*)")
	 (logo-edit-dot-emacs
	  "^[ \t;]*([ \t]*setq[ \t]+logo-language[ \t]+'dot-logo[ \t]*)"
	  "(setq logo-language 'dot-logo)"))))

(defun logo-edit-dot-emacs (string &optional replacement)
  "Edit .emacs to set default language level."
  (let ((dot-emacs (substitute-in-file-name "$HOME/.emacs")))
    (if (and (file-readable-p dot-emacs)
	     (file-writable-p dot-emacs))
	(progn
	  (find-file dot-emacs)
	  (goto-char (point-max))
	  (let ((start (re-search-backward string nil t)))
	    (cond
	     ;; 'standard is the default anyway
	     ((and (not start) (not replacement)) 'do-nothing)
	     ;; No previous references - setting 'dot-logo
	     ((not start)
	      (goto-char (point-max))
	      (newline)
	      (insert replacement))
	     ;; Setting 'standard -- just commenting out
	     ((not replacement) (insert ";"))
	     ;; Setting 'dot-logo
	     (t (delete-matching-lines string)
		(insert replacement))))
	  (save-buffer)
	  (kill-buffer (current-buffer)))
      (error "Check permisions on your .emacs file"))))

(defun logo-structure-compile ()
  "Start .logo.compiler on the workspace."
  (interactive)
  (comint-send-string (logo-proc)
		      "pr [] (.logo.compiler) \n"))

(defun logo-save-compiled ()
  "Save compiled procedures to a file."
  (interactive)
  (comint-send-string (logo-proc)
		      "(save.compiled) \n"))  

(defun logo-idle-function (output)
  "Resets busy flag if Logo idling."
  (if (string-match  "^[ \t]*\\?\\ \\'" output)
	(setq logo-busy-p nil)))

(defun logo-toggle-to-logo (&optional n)
  "Toggle from edit to Logo process buffer and vice-versa."
  (interactive "P")
  (if (equal (buffer-name) logo-buffer)
      (let ((ed-buffer (nth (1- (prefix-numeric-value n)) logo-edit-buflist)))
	(if ed-buffer
	      (logo-popup-frames
	       logo-frames
	       (logo-switch-buffer ed-buffer))
	  (error "No logo files opened")))
    (if logo-buffer
	(logo-popup-frames
	 logo-frames
	 (logo-switch-buffer logo-buffer))
      (error "No logo process buffer"))))

(defun logo-default-filter (proc output)
  "We are initializing filter -- just handling focus problems."
  (let* ((pbuff (process-buffer proc))
	 (obuff (current-buffer))
	 (pwindow (get-buffer-window pbuff 'visible)))
    (unwind-protect
	(progn
	  (if pwindow
	      (select-window pwindow)
	    (switch-to-buffer pbuff)
	    (select-window (get-buffer-window pbuff)))
	  ;; Sending to real filter 
	  (logo-default-help proc output))
      (let ((owin (get-buffer-window obuff 'visible)))
	(if owin
	    (select-window owin)
	  (switch-to-buffer obuff))))))
      
(defun logo-default-help (proc output)
  "Cleans logo output from terminal access garbage.
Uses codes sent by system's standard TERM. I hope this works on 
all systems, as it lets me handle SETCURSOR."
  (let* ((string (if (not logo-code-kludge)
		     output
		   ;; Tack any leftovers onto new batch of Logo output
		   (concat logo-code-kludge output)))
	 (beg (string-match logo-term-start string)))
    (if (not beg)
	;; Clean code - hand it to comint filter
	(funcall working-comint-output-filter proc string)
      (if (string-match logo-term-all string (1+ beg))
	  (progn
	    (setq logo-code-kludge nil)
	    (cond ((match-beginning 1) 
		   ;; CLEARTEXT & SETMARGINS
		   ;; Send anything before CT to comint
		   (funcall working-comint-output-filter
			    proc
			    (substring string 0 beg))
		   ;; Scroll to top
		   (set-window-start
		    (get-buffer-window logo-buffer) (point) t)
		   ;; If in terminal-mode narrow from top to buffer end
		   (funcall logo-ct-function)
		   ;; Do the SETCURSOR - this is a possible security hole
		   (logo-setcursor proc string 3 2)
		   ;; Handle the rest
		   (logo-default-help proc
				      (substring string (match-end 1))))
		  ((match-beginning 4)
		   ;; SETCURSOR
		   ;; Send anything before SETCURSOR to comint
		   (funcall working-comint-output-filter
			    proc
			    (substring string 0 beg))
		   ;; Do SETCURSOR if in setcursor-mode
		   (funcall logo-setcursor-function proc string 6 5 t)
		   ;; Handle the rest
		   (logo-default-help proc
				      (substring string (match-end 4))))
		  ((match-beginning 7)
		   ;; STANDOUT
		   (add-text-properties (match-beginning 8)
					(match-end 8)
					'(rear-nonsticky t face highlight)
					string)
		   (funcall working-comint-output-filter
			    proc
			    (concat (substring string 0 beg)
				    (substring string
					       (match-beginning 8)
					       (match-end 8))))
		   (logo-default-help proc
				      (substring string (match-end 7))))
		  (t
		   ;; How did we get here?
		   (error "Error in default filter"))))
	;; fixing process communication problems
	(if logo-code-kludge
	    ;; I'm letting it slide here.
	    ;; The only valid Logo code that can get through
	    ;; is a *very* long standout. Unfortunately Logo itself
	    ;; can't handle this properly.
	    (progn
	      ;; Show last Logo output
	      (if logo-busy-p
		  (funcall working-comint-output-filter proc logo-code-kludge)
		(funcall working-comint-output-filter proc string))
	      (setq logo-code-kludge nil)
	      (ding)
	      (message (concat
			"Unknown control code or *very* large standout - "
			"see `logo-system-type' variable.")))
	  (setq logo-code-kludge string)
	  ;; Trapping invalid control codes or *very* long standouts
	  ;; This prevents a stalemate, while Logo awaits for user input
	  ;; and logo.el awaits for the rest of unrecognized control
	  ;; sequence.
	  (run-with-idle-timer
	   ;; Wait n seconds for next batch of Logo output, after
	   ;; discovery of broken sequence. If on a very slow system,
	   ;; change logo-filter-timer to 3, or whatever works for you.
	   ;; Default value is 2.
	   logo-filter-timer nil
	   (function
	    (lambda ()
	      (and logo-code-kludge
		   ;; Ask Logo to send anything -- to force printing of
		   ;; the broken code waiting for next batch of Logo output.
		   (logo-send-command "wait 0"))))))))))

(defun logo-setcursor (proc string regex-col regex-row &optional narrow)
  "Setting cursor in comint process buffer.
There is a tradeoff with this - if you really need it then use it.
If you do not, switch to emacs or terminal mode - it is safer."
  (let* ((pbuff (process-buffer proc))
	 (pwindow (get-buffer-window pbuff)))
    (unwind-protect
	(progn
	  (goto-char (point-max))
	  (goto-char (window-start pwindow))
	  (and narrow
	       (narrow-to-region (point) (point-max)))
	  ;; Get the coordinates
	  (let ((col (1- (string-to-number
			  (substring string
				     (match-beginning regex-col)
				     (match-end regex-col)))))
		(row (1- (string-to-number
			  (substring string
				     (match-beginning regex-row)
				     (match-end regex-row))))))
	    ;; Move down row lines
	    (let ((lines (- row (vertical-motion row))))
	      ;; If not enough insert newlines
	      (insert-before-markers (make-string lines ?\n)))
	    ;; we're padding here if in setcursor mode
	    (funcall logo-pad-function)
	    ;; Set the column
	    (move-to-column col t)))
      ;; Update process mark
      (set-marker (process-mark proc) (point) pbuff))))

(defun logo-emacs-filter (proc output)
  "Cleans logo output from garbage created by CLEARTEXT & SETCURSOR.
Strips Logo output from STANDOUTs, then from SETCURSORs. Whatever is left
must be CLEARTEXTs or SETMARGINs. Works on `emacs' terminals!"
  (comint-output-filter
   proc
   (logo-filter-help proc
		     (get-buffer-window logo-buffer)
		      (logo-put-standout
		       (logo-remove-garbage output
					    logo-setcursor-str)))))

(defun logo-put-standout (string)
  "Highlight the standout string & strip the garbage."
  (if (string-match logo-standout-str string)
      (progn
	(put-text-property (match-beginning 1)
			   (match-end 1)
			   'face 'highlight
			   string)
	(logo-put-standout (replace-match "\\1" t nil string)))
    string))

(defun logo-filter-help (proc proc-window string)
  "If any CLEARTEXTs or SETMARGINs left, clear it and scroll to top."
  (if (string-match logo-cleartext-str string)
      (progn
	(comint-output-filter
	 proc
	 ;; Take output before CT, append newline and send it to comint
	 (concat (substring string 0 (match-beginning 0)) "\n"))
	;; Scroll old output off the screen
	(set-window-start proc-window (point) nil)
	(logo-filter-help proc
			  proc-window
			  (substring string (match-end 0))))
    string))

(defun logo-dumb-filter (proc output)
  "Same as logo-emacs-filter, but works for `dumb' terminals."
  (comint-output-filter
   proc
   (logo-filter-help proc
		     (get-buffer-window logo-buffer)
		     (logo-remove-garbage
		      (logo-remove-garbage output logo-standout-str)
		      logo-setcursor-str))))

(defun logo-remove-garbage (string garbage)
  "Remove STANDOUT and SETCURSOR garbage first."
  (if (string-match garbage string)
      (logo-remove-garbage (replace-match "" t t string) garbage)
    string))

;;; ======================== Logo setcursor mode ===================
;;
;;; Logo-setcursor minor mode

;; Only avoiding compilation warnings
(defvar comint-last-input-end nil)
(defvar comint-last-output-start)
(defvar comint-last-input-start)

(defvar logo-setcursor-spaces-old nil
  "Number of spaces to clear after old input. Nil clears to the end of line.")
(or (and logo-setcursor-spaces-old
	 ;; Let's be reasonable
	 (setq logo-setcursor-spaces-old
	       (min logo-setcursor-spaces-old (1- logo-term-columns))))
    (setq logo-setcursor-spaces-old (1- logo-term-columns)))

(defvar logo-setcursor-spaces-new nil
  "Number of spaces to clear after new input. Nil clears 2 spaces.")
(or (and logo-setcursor-spaces-new
	 ;; Let's be reasonable
	 (setq logo-setcursor-spaces-new
	       (min logo-setcursor-spaces-new (1- logo-term-columns))))
    (setq logo-setcursor-spaces-new 2))

;; Forcing yank at start of logo input
(make-variable-buffer-local 'mouse-yank-at-point)

(defvar logo-history-menu nil
  "Variable holding setcursor dynamic history menu.")

(defvar logo-setcursor-mode-map nil)
(if logo-setcursor-mode-map
    nil
  (setq logo-setcursor-mode-map (make-keymap))
  ;; Overloading self-insert-command to control user input
  (substitute-key-definition 'self-insert-command
 			     'logo-setcursor-self-insert
 			     logo-setcursor-mode-map
 			     (current-global-map))
  ;; Disabling TAB
  (define-key logo-setcursor-mode-map "\t" 'ignore)
  ;; Enter key - sending input, if any, to Logo
  (define-key logo-setcursor-mode-map "\C-m" 'logo-comint-send-input)
  ;; Not much use for re-center in setcursor mode, but you may need
  ;; to find input end, if stepping through input history
  (define-key logo-setcursor-mode-map "\C-l" 'logo-find-input-end)
  ;; Fixing long line problems after backspacing or deleting
  (define-key logo-setcursor-mode-map [backspace] 'logo-setcursor-backspace)
  ;; Do this only if user altered global binding of delete key to delete-char
  (if (equal (lookup-key (current-global-map) [delete]) 'delete-char)
      (define-key logo-setcursor-mode-map [delete] 'logo-setcursor-delete)
    (define-key logo-setcursor-mode-map [delete] 'logo-setcursor-backspace))
  ;; Disabling comint-delchar-or-maybe-eof function
  (define-key logo-setcursor-mode-map "\C-d" 'logo-setcursor-delete)
  ;; Overloading kill and yank
  (define-key logo-setcursor-mode-map "\C-k" 'logo-setcursor-kill)
  (define-key logo-setcursor-mode-map "\C-y" 'logo-setcursor-yank)
  (define-key logo-setcursor-mode-map "\C-@" 'logo-set-input-mark)
  ;; mouse bindings
  (define-key logo-setcursor-mode-map [mouse-1] 'logo-setcursor-mouse-1)
  (define-key logo-setcursor-mode-map [down-mouse-2] 'mouse-set-point)
  (define-key logo-setcursor-mode-map [mouse-2] 'logo-setcursor-yank)
  ;; disabling mouse-3
  (define-key logo-setcursor-mode-map [down-mouse-3] 'logo-mouse-set-point)
  (define-key logo-setcursor-mode-map [mouse-3] 'logo-mouse-set-point)
  ;; disabling comint-kill-output
  (define-key logo-setcursor-mode-map "\C-c\C-o" 'ignore)
  ;; disabling quarry-replace
  (define-key logo-setcursor-mode-map [?\e ?\^%] 'ignore)
  (define-key logo-setcursor-mode-map "\M-%" 'ignore)
  ;; disabling completions
  (define-key logo-setcursor-mode-map "\M-/" 'ignore)
  (define-key logo-setcursor-mode-map [?\e ?\^/] 'ignore)
  ;; redefining undo
  (define-key logo-setcursor-mode-map "\C-_" 'logo-setcursor-undo)
  (define-key logo-setcursor-mode-map [menu-bar edit undo]
    'logo-setcursor-undo)
  ;; redefining kill-region
  (define-key logo-setcursor-mode-map "\C-w" 'logo-setcursor-kill)
  (define-key logo-setcursor-mode-map [menu-bar edit cut]  
    'logo-setcursor-kill)
  ;; redefining paste
  (define-key logo-setcursor-mode-map [menu-bar edit paste]  
    'logo-setcursor-yank)  
  ;; redefining delete-region
  (define-key logo-setcursor-mode-map [menu-bar edit clear]  
    'logo-setcursor-kill)
  ;; disabling list-buffers
  (define-key logo-setcursor-mode-map "\C-x\C-b" 'ignore)
  ;; disabling split-window
  (define-key logo-setcursor-mode-map "\C-x2" 'ignore)  
  ;; disabling comint "List Input History"
  (define-key logo-setcursor-mode-map "\C-c\C-l" 'ignore)
  (define-key logo-setcursor-mode-map [menu-bar inout list-history]
    '("" . (lambda ()
 	     (interactive)
 	     (error "In setcursor mode use S-down-mouse-2 instead"))))

  ;; Pop-up menu for input history and file completion
  (defvar logo-popup-menu-map
    (make-sparse-keymap "Setcursor Menu"))
  (fset 'logo-dynamic-menu logo-popup-menu-map)
  
  (define-key logo-popup-menu-map [logo-select-history]
    '("List Input History" . logo-history-menu))
  (put 'logo-history-menu 'menu-enable '(cdr logo-history-menu))
  
  (define-key logo-setcursor-mode-map [S-down-mouse-2]
    'logo-dynamic-menu))

(defun logo-setcursor-mode ()
  "Setcursor minor mode - simulating two dimensional nonsequential display.

Key and mouse bindings are as in inferior-logo-mode, except
for C-l, which does not re-center. We do not want re-centering in
Setcursor mode. Instead it sends the point to the end of current
input, or to the process-mark \\=(point where command line starts\\=).

Emacs sets the end of current input marker automatically. If you want
to override this, set the logo-input-marker manually with C-@ .

DEL, Backspace, and C-d work only within the limits of command \\=(input\\=) line.
C-k, C-y and mouse-2 \\=(mouse paste\\=) work only on the whole command line.
No matter where your point is C-k will always kill input line, and C-y and
mouse-2 will always yank-back last X-selection or first entry from kill-ring
\\=(if X-selection does not exist\\=).

\\{logo-setcursor-mode-map}"

  (interactive)
  (logo-setcursor-mode-initialize)
  (logo-setcursor-variables)
  (setq mouse-yank-at-point t)
  (setq logo-setcursor-mode t)
  ;; Terminal and Emacs modes are not updating history menu.
  (logo-fill-history-menu (nthcdr 2 comint-input-ring))
  ;; Make sure there's no other windows in setcursor mode frame
  ;; unless we are in two windows mode
  (or logo-switch-buffer
      (delete-other-windows)))

(defun logo-setcursor-mode-initialize ()
  (or (assq 'logo-setcursor-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'logo-setcursor-mode logo-setcursor-mode-map)
		minor-mode-map-alist)))
  (if (not (assq 'logo-setcursor-mode minor-mode-alist))
      (setq minor-mode-alist
	    (cons '(logo-setcursor-mode " SC")
		  minor-mode-alist))))

(defun logo-setcursor-variables ()
  "Setting emacs to behave like Logo terminal."
  (setq logo-term-mode 'setcursor)
  (setq logo-input-marker (make-marker))
  (setq comint-input-sender (function setcursor-input-sender))
  (setq logo-pad-function
	(function (lambda nil (move-to-column logo-term-columns t))))
  (setq mode-line-buffer-identification '("Setcursor-mode: %6b"))
  (setq working-comint-output-filter
	(function setcursor-comint-output-filter))
  (setq comint-scroll-to-bottom-on-output nil)
  (and logo-setcursor-overwrite
       (overwrite-mode 1))
  (setq comint-scroll-show-maximum-output t)
  ;; Setting setcursor history menu
  (setq logo-history-menu (cons "History List" nil))
  (fset 'logo-history-menu (cons 'keymap logo-history-menu))
  (setq comint-get-old-input
	(function (lambda nil
		    (goto-char logo-input-marker)
		    (error "This is not available in setcursor mode"))))
  (setq comint-eol-on-send nil)
  (setq scroll-step 1)
  (setq logo-setcursor-function (function logo-setcursor))
  (remove-hook 'comint-output-filter-functions '
	       'comint-postoutput-scroll-to-bottom)
  (add-hook 'comint-output-filter-functions 'logo-overwrite-function)
  (setq logo-ct-function
	(function (lambda ()
		    (beginning-of-line)
		    (delete-region (point) (point-max))
		    (narrow-to-region (point) (point-max)))))
  (let ((proc (get-buffer-process logo-buffer)))
    (and proc
	 (comint-send-string proc "setmargins [0 0]\n"))))

(defun logo-setcursor-self-insert ()
  "A wraparound code for self-insert command. Displaying and trimming
text that user types in setcursor mode."
  (interactive)
  (if (or (< (point) (marker-position (process-mark (logo-proc))))
	  (> (point) (marker-position logo-input-marker)))
      (progn
	(message "This was not within command line  --  You have been moved!")
	(ding)
	;; sending point back to the command line
	(logo-find-input-end)))
  (let ((inside (< (point) (marker-position logo-input-marker)))
	(overwriting overwrite-mode))
    (save-excursion
      ;; Getting rid of all newlines in input.
      (while (and (skip-chars-forward "^\n")
		  (< (point) (marker-position logo-input-marker)))
      	(delete-char 1))
      ;; Padding the line - works only for first line
      (move-to-column logo-term-columns t))
    ;; Making sure we're in insert mode.
    (overwrite-mode -1)
    ;; Inserting dummy space first. Don't want parens maching to
    ;; occure just yet.
    (insert-char 32 1)
    ;; Return previous mode (insert/overwrite).
    (and overwriting
	 (overwrite-mode 1))
    ;; If inserted at marker position move marker manually. 
    (if (not inside)
	(set-marker logo-input-marker
		    (1+ (marker-position logo-input-marker))))
    (save-excursion
      (if (and inside overwriting)
	  (delete-char 1)
	(progn
	  (goto-char logo-input-marker)
	  (or (eobp)
	      ;; If not at end of line delete character.
	      (and (not (eolp))
		   (progn
		     (delete-char 1)
		     t))
	      (progn
		;; If at end of line, first pad this line.
		;; Then move to next and pad that as well.
		;; And finally delete 2 characters.
		(save-excursion
		  (move-to-column logo-term-columns t)
		  (beginning-of-line 2)
		  (move-to-column logo-term-columns t))
		(delete-char 2)))))
      ;; Inserting previously deleted newlines.
      (logo-insert-nl (- (progn (end-of-line) (point))
			 (progn (beginning-of-line) (point)))))
    ;; Making sure we'r in overwrite mode.
    (overwrite-mode 1)
    ;; Inserting character that user typed now. Overwriting dummy space.
    (and (char-equal (preceding-char) ?\n)
	 (backward-char))
    (backward-char)
    (self-insert-command 1)
    ;; If inserted at marker position move marker manually, again? 
    (if (not inside)
	(set-marker logo-input-marker
		    (1+ (marker-position logo-input-marker))))
    ;; Return previous mode (insert/overwrite).
    (or overwriting
	(overwrite-mode -1))
    ;; Push some spaces over the old text, to make new input readable
    (save-excursion
      (goto-char (marker-position logo-input-marker))
      (let ((spaces (min logo-setcursor-spaces-new
			 ;; Not beyond eobp
			 (- (point-max) (point))
			 (mod (- logo-term-columns
				 (mod (current-column)
				      logo-term-columns))
			      logo-term-columns))))
	(delete-char spaces)
	(insert-char 32 spaces))
      ;; Do not recenter window if inserted beyond last line
      (or (pos-visible-in-window-p
	   (point) (selected-window))
	  (recenter -1)))))

(defun logo-insert-nl (line-len)
  "Inserting newlines that were removed before using self-insert-command."
  (if (> line-len logo-term-columns)
      (progn
	(move-to-column logo-term-columns)
	(insert-before-markers "\n")
	(logo-insert-nl (- line-len logo-term-columns)))))

(defun logo-setcursor-backspace ()
  "Adjusting the `attitude' of backspace key in setcursor mode. Ignoring
newlines and padding at the end of logo input string to prevent shifting
of old text."
  (interactive)
  (let* ((pmark (marker-position (process-mark (logo-proc))))
	 (in-input
	  (and (> (point) pmark)
	       (<= (point) (marker-position logo-input-marker))))
	 (overwriting overwrite-mode))
    ;; Backspacing out of input string is not good for you. Anything
    ;; not good for you is illegal.
    (and (not in-input)
	 (error "Trying to backspace out of input string - hit C-l to find end of input"))
    ;; Let's dispense with emacs silly backspacing in overwrite mode.
    (overwrite-mode -1)
    (save-excursion
      ;; Getting rid of all newlines in input.
      (goto-char pmark)
      (while (and (skip-chars-forward "^\n")
		  (< (point) (marker-position logo-input-marker)))
	(delete-char 1)))
    (backward-delete-char-untabify 1)
    (save-excursion
      (goto-char logo-input-marker)
      (insert-char 32 1)
      ;; Inserting previously deleted newlines.
      (logo-insert-nl (- (progn (end-of-line) (point))
			 (progn (beginning-of-line) (point)))))
    ;; Restore the input mode
    (and overwriting
	 (overwrite-mode 1))))

(defun logo-setcursor-delete ()
  "Adjusting the `attitude' of delete key in setcursor mode. Ignoring
newlines and padding at the end of logo input string to prevent shifting
of old text."
  (interactive)
  (let* ((pmark (marker-position (process-mark (logo-proc))))
	 (in-input
	  (and (>= (point) pmark)
	       (< (point) (marker-position logo-input-marker)))))
    ;; Deleting out of input string is not good for you. Anything
    ;; not good for you is illegal.
    (and (not in-input)
	 (error "Trying to delete out of input string - hit C-l to find end of input"))
    (save-excursion
      ;; Getting rid of all newlines in input.
      (goto-char pmark)
      (while (and (skip-chars-forward "^\n")
		  (< (point) (marker-position logo-input-marker)))
	(delete-char 1)))
    ;; This is the one that actually deletes the character.
    (delete-char 1)
    (save-excursion
      (goto-char logo-input-marker)
      (insert-char 32 1)
      ;; Inserting previously deleted newlines.
      (logo-insert-nl (- (progn (end-of-line) (point))
			 (progn (beginning-of-line) (point)))))))

(defun logo-setcursor-yank ()
  "C-y & Mouse-2 yank back last entry from kill-ring. It is *always* placed
at the start of logo input, and overwrites as much old text as necessary."
  (interactive)
  (let* ((proc (logo-proc))
	 (oproc-mark (marker-position (process-mark proc))))
    (goto-char oproc-mark)
    ;; Sending last entry as Logo output, but stripping newlines first.
    (let ((yanked nil))
      (unwind-protect
	  (progn
	    (setcursor-comint-output-filter
	     proc
	     (let ((string
		    ;; Removing extra spaces
		    (logo-strip-comment
		     ;; Replacing newlines with spaces
		     (logo-strip-newline
		      ;; Stripping comment only lines
		      (logo-strip-comment
		       ;; Getting rid of xcontinuation ~
		       (logo-strip-xcont
			(prog1
			    ;; Checking for X-selection first
			    (current-kill 0)
			  ;; Kill old input, only if selection exists.
			  (logo-setcursor-kill)))
		       0)
		      "\n" " ")
		     ;; Look for more than 2 spaces followed by anything
		     ;; and a vertical bar or by anything.
		     0 "\\([ \t][ \t]+\\)\\(.*|\\|.*\\)" " ")))
	       ;; Strip trailing whitespaces
	       (if (string-match "[ \t]+\\'" string)
		   (substring (replace-match "" t t string) 0)
		 string)))
	    (setq yanked t))
	(set-marker (process-mark proc) oproc-mark)
	;; If yank succeeded move end of Logo input marker.
	(or (and yanked
		 (set-marker logo-input-marker (point)))
	    ;; Else set point to the end of old Logo input.
	    (goto-char logo-input-marker))))))

(defun logo-strip-comment (string start &optional regex fill)
  "Strip comment lines if not within pair of bars."
  (let* ((regexpr (or regex
		      "[ \t\n]*\\(\\(;+\\).*$\\)"))
	 (replacement (or fill ""))
	 (comment
	  (string-match regexpr string start)))
    (if comment
	;; If match not in Logo string replace it with ""
	(if (save-match-data
	      (zerop
	       (% (logo-instring-count
		   (substring string 0 (1+ comment))
		   0)
		  2)))
	    (logo-strip-comment (replace-match replacement t t string 1)
				start
				regexpr
				replacement)
	  ;; If match in Logo string leave it, but move start after `;'
	  (logo-strip-comment string
			      (match-end 2)
			      regexpr
			      replacement))
      string)))

(defun logo-setcursor-kill ()
  "C-k kills the input line in setcursor mode. Just overwrites with spaces."
  (interactive)
  (let* ((proc (logo-proc))
	 (oproc-mark (marker-position (process-mark proc)))
	 (logo-mark (marker-position logo-input-marker)))
    (goto-char oproc-mark)
    (let ((spaces
	   (length
	    (logo-strip-newline
	     (buffer-substring-no-properties oproc-mark logo-mark)))))
      (setcursor-comint-output-filter proc (make-string spaces 32))
      (set-marker (process-mark proc) oproc-mark)
      (goto-char oproc-mark)
      (set-marker logo-input-marker (point)))))

(defun logo-set-input-mark ()
  "Setting logo-input-marker at the end of Logo command line manually."
  (interactive)
  (set-marker logo-input-marker (point))
  (message "Logo input mark set."))

(defsubst logo-terminal-return ()
  "Return does not insert newline, except at eobp."
  (move-to-column logo-term-columns t)
  (if (eobp)
      ;; Drop down
      (insert-before-markers ?\n)
    (beginning-of-line 2))
  ;; Padding
  (move-to-column logo-term-columns t)
  (beginning-of-line)
  "")

(defun logo-comint-send-input () 
  "Send input to process. Overloading comint-send-input.
The reason is one hardwired newline, and grabbing the line up to the
logo-input-marker, and removing newlines from input sent to history -
in case user switches to terminal, or emacs mode later."
  (interactive)
  ;; This is just messing with keyboard, in case user changed it
  (and logo-setcursor-overwrite
       (overwrite-mode 1))
  ;; Remove any other windows, just in case user was messing with
  ;; commands that leave a split frame. Not if in two windows mode.
  (or logo-switch-buffer
      (delete-other-windows))
  ;; Note that the input string does not include its terminal newline.
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (error "Current buffer has no process")
	(let* ((pmark (process-mark proc))
	       (intxt (if (>= (point) (marker-position pmark))
			  (progn (if comint-eol-on-send (end-of-line)
				   ;; setcursor input marker
				   (goto-char logo-input-marker))
				 (buffer-substring pmark (point)))

			;; Setcursor kludge. Can't get-old-input, as
			;; input line may be broken into several lines.
			;; No way to keep track of that.
			(goto-char pmark)
			""))
	       (input (if (not (eq comint-input-autoexpand 'input))
			  ;; Just whatever is already there
			  intxt
			;; Expand and leave it visible in buffer
			(comint-replace-by-expanded-history t)
			(buffer-substring pmark (point))))
	       		
	       ;; Strip newlines from history - in case user changes to
	       ;; emacs or terminal mode later on
	       (history (logo-strip-newline
			 (if (not (eq comint-input-autoexpand 'history))
			     input
			   ;; This is messy 'cos ultimately the original
			   ;; functions used do insertion, rather than return
			   ;; strings.  We have to expand, then insert back.
			   (comint-replace-by-expanded-history t)
			   (let ((copy (buffer-substring pmark (point))))
			     (delete-region pmark (point))
			     (insert-before-markers input)
			     copy)))))
          (if comint-process-echoes
              (delete-region pmark (point))

	    ;; Logo kludge
	    ;; If we wrapped the line after the end of last input snip it
	    (save-excursion
	      (end-of-line)
	      (while (> (current-column) logo-term-columns)
		(let ((eol (point)))
		  (move-to-column logo-term-columns)
		  (delete-region (point) eol))))
	    (if (> (current-column) logo-term-columns)
		(save-restriction
		  (widen)
		  (logo-overwrite-help
		   (length input)
		   (save-excursion
		     (let ((oend (point)))
		       (beginning-of-line)
		       (- oend (point)))))))
	    ;; If not just do a soft newline + padding
	    (insert-before-markers (logo-terminal-return)))

	  (if (and (funcall comint-input-filter history)
		   (or (null comint-input-ignoredups)
		       (not (ring-p comint-input-ring))
		       (ring-empty-p comint-input-ring)
		       (not (string-equal (ring-ref comint-input-ring 0)
					  history))))
	      (progn
		;; Update setcursor history menu and history ring
		(logo-update-history-menu history)
		(ring-insert comint-input-ring history)))
	  
	  (run-hook-with-args 'comint-input-filter-functions
			      (concat input "\n"))
	  (setq comint-input-ring-index nil)
	  ;; Update the markers before we send the input
	  ;; in case we get output amidst sending the input.
	  (set-marker comint-last-input-start pmark)
	  (set-marker comint-last-input-end (point))
	  (set-marker (process-mark proc) (point))
	  (funcall comint-input-sender proc input)
	  (comint-output-filter proc "")))))

(defun setcursor-input-sender (proc input)
  "Remove newlines from input. This has to do with overwriting mechanism."
  (comint-simple-send proc (logo-strip-newline input)))

(defun logo-strip-newline (string &optional character replacement)
  "Can't have newlines in overwrite mode."
  (let ((char (or character "\n+"))
	(fill (or replacement "")))
    (if (string-match char string)
	(logo-strip-newline (substring (replace-match fill t t string) 0)
			    char fill)
      string)))

(defun setcursor-comint-output-filter (proc string)
  "Handling hard newline in Logo output. Send to comint up to a newline, 
do a soft newline, and repeat."
  (let ((oldbuf (current-buffer)))
    (save-match-data
      (setcursor-comint-helper proc string))
    (set-buffer oldbuf)))

(defun setcursor-comint-helper (proc output)
  (let ((procbuf (process-buffer proc)))
    (set-buffer procbuf)
    (let ((beg (string-match "\n" output)))
      (if (not beg)
	  ;; No newlines - hand it to comint
	  (comint-output-filter proc output)
	;; Send anything before newline to comint
	(comint-output-filter proc (substring output 0 beg))
	(insert-before-markers (logo-terminal-return))
	(set-marker (process-mark proc) (point) procbuf)
	(setcursor-comint-helper proc
				 (substring output (match-end 0)))))))
      
(defun logo-overwrite-function (output)
  "Deleting enough old text to simulate overwrite behavior.
At the same time overloading comint-postoutput-scroll-to-bottom."
  (let* ((selected (selected-window))
	 (current (current-buffer))
	 (process (get-buffer-process current))
	 (scroll comint-scroll-to-bottom-on-output))
    (unwind-protect
	(if process
	    (walk-windows
	     (function (lambda (window)
	       (if (eq (window-buffer window) current)
		   (progn
		     (select-window window)
		     ;; deleting text after newly inserted.
		     ;; Stopping before eob, or before eol,
		     ;; but only if not at column 80. If at col 80
		     ;; insert newline, and continue deleting next line
		     (save-restriction
		       (widen)
		       (goto-char (process-mark process))
		       (logo-overwrite-help
			(length output)
			(save-excursion
			  (let ((oend (point)))
			    (beginning-of-line)
			    (- oend (point))))))
		     (if (and (< (point) (process-mark process))
			      (or (eq scroll t) (eq scroll 'all)
				  ;; Maybe user wants point to jump to end.
				  (and (eq scroll 'this) (eq selected window))
				  (and (eq scroll 'others) (not (eq selected window)))
				  ;; If point was at the end, keep it at end.
				  (>= (point) comint-last-output-start)))
			 (goto-char (process-mark process)))
		     ;; Optionally scroll so that the text
		     ;; ends at the bottom of the window.
		     (if (and comint-scroll-show-maximum-output
			      (>= (point) (process-mark process)))
			 (save-excursion
			   (goto-char (point-max))
			   (recenter -1)))
		     ;; Setting point at process mark
		     (goto-char (process-mark process))
		     ;; Setting logo-input-mark
		     (set-marker logo-input-marker (point))
		     (select-window selected)))))
	     nil t))
      (set-buffer current))))

(defun logo-overwrite-help (deletions line-len)
  "This one is actually doing the deleting."
  (if (> line-len logo-term-columns)
      ;; Insert 1 newline at last column, and eat up following newline
      ;; This is assuming that padding has been carried out before
      (progn
	(save-excursion
	  (move-to-column logo-term-columns)
	  (insert-before-markers "\n")
	  (end-of-line)
	  (or (eobp) (delete-char 1)))
	(logo-overwrite-help deletions (- line-len logo-term-columns)))
    ;; Delete deletions, but stop before eol or eob
    (let ((output-end (point)))
      (delete-char (min deletions
			(save-excursion
			  (end-of-line)
			  (- (point) output-end))
			(- (point-max) output-end))))))

(defun setcursor-previous-matching-input (regexp arg)
  "Overloading comint-previous-matching-input, overwriting old text."
  (interactive (comint-regexp-arg "Previous input matching (regexp): "))
  (setq arg (comint-search-arg arg))
  (let ((pos (comint-previous-matching-input-string-position regexp arg)))
    ;; Has a match been found?
    (if (null pos)
	(error "Not found")
      (setq comint-input-ring-index pos)
      (message "History item: %d" (1+ pos))
      ;; If in overwrite mode - do not delete! Overwrite with spaces. We 
      ;; are making sure that the text after inserted input does not move.
      (if logo-setcursor-mode
	  (logo-setcursor-kill)
	(delete-region 
	 ;; Can't use kill-region as it sets this-command
	 (process-mark (get-buffer-process (current-buffer))) (point)))
      (let ((string (ring-ref comint-input-ring pos)))
	;; Just making sure that we are at the process mark
	(goto-char (process-mark (get-buffer-process (current-buffer))))
	(if (not logo-setcursor-mode)
	    (insert string)
	  ;; Setcursor mode
	  (let ((string-no-nl (logo-strip-newline string)))
	    (insert string-no-nl)
	    (set-marker logo-input-marker (point))
	    ;; Need to clear some old text after input end
	    ;; Default is clear to the end of line
	    (let ((spaces (min logo-setcursor-spaces-old
			       (mod (- logo-term-columns
				       (mod (current-column)
					    logo-term-columns))
				    logo-term-columns))))
	      (insert-char 32 spaces)
	      ;; prepare for overwriting after spaces
	      (save-excursion
		(save-restriction
		  (widen)
		  (logo-overwrite-help (+ (length string-no-nl) spaces)
				       (save-excursion
					 (let ((oend (point)))
					   (beginning-of-line)
					   (- oend (point)))))))))
	  (goto-char logo-input-marker))
	;; Do not recenter window if in setcursor or terminal mode
	(or (equal logo-term-mode 'emacs)
	    (or (pos-visible-in-window-p
		 (point) (selected-window))
		(recenter -1)))))))

(defun logo-find-input-end ()
  "Returns point to current input end -- end of Logo command line."
  (interactive)
  (goto-char logo-input-marker))

(defun logo-get-old-input-default ()
  "Replacing comint-get-old-input-default. 
Take the current line, discard any initial text matching
`comint-prompt-regexp', and strip trailing white-spaces."
  (save-excursion
    (beginning-of-line)
    (comint-skip-prompt)
    (let ((beg (point)))
      (end-of-line)
      (skip-syntax-backward "-")
      ;; In case we were on the empty line
      (if (< (point) beg)
	  ""
	(buffer-substring beg (point))))))

(defun logo-setcursor-undo ()
  "Undo in setcursor buffer, only if undoing the new input text."
  (interactive)
  (if (> logo-input-marker (marker-position (process-mark (logo-proc))))
      (undo)
    (message "Not active in setcursor mode.")))

;;; Setcursor dynamic menu

(defcustom logo-history-menu-length 48
  "*Maximum length to display in the history-menu."
  :type 'integer
  :group 'mouse)

(defcustom logo-dynamic-menu-depth
  (let ((pixels (x-display-pixel-height)))
    (cond ((> pixels 1023) (min 42 comint-input-ring-size))
	  ((> pixels 863) (min 36 comint-input-ring-size))
	  ((> pixels 767) (min 32 comint-input-ring-size))
	  ((> pixels 599) (min 25 comint-input-ring-size))
	  (t (min 20 comint-input-ring-size))))
  "*Maximum number of entries in the dynamic-menu."
  :type 'integer
  :group 'mouse)

(defun logo-update-history-menu (string)
  (let ((front (car (cdr logo-history-menu)))
	(menu-string (if (<= (length string) logo-history-menu-length)
			 string
		       (concat
			(substring string 0 (/ logo-history-menu-length 2))
			"..."
			(substring string (- (/ logo-history-menu-length 2)))))))
    ;; Don't let the menu string be all dashes
    ;; because that has a special meaning in a menu.
    (if (string-match "\\`-+\\'" menu-string)
	(setq menu-string (concat menu-string " ")))
    (setcdr logo-history-menu
	    (cons
	     (cons string (cons menu-string 'logo-menu-select-history))
	     (cdr logo-history-menu)))
    (if (> (length (cdr logo-history-menu)) logo-dynamic-menu-depth)
        (setcdr (nthcdr logo-dynamic-menu-depth logo-history-menu) nil))))

(put 'logo-menu-select-history 'apropos-inhibit t)

(defun logo-fill-history-menu (history)
  "Update input history menu when switching mode from emacs or terminal
to setcursor."
  (mapcar
   (lambda (input)
     (and input (logo-update-history-menu input))) history))

(defun logo-menu-select-history ()
  "Pop-up a menu with input history items."
  (interactive "*")
  (push-mark (point))
  (let ((string-no-nl (logo-strip-newline last-command-event)))
    ;; Instead of insert -- have to do all this
    (logo-setcursor-kill)
    ;; Just making sure that we are at the process mark
    (goto-char (process-mark (get-buffer-process (current-buffer))))
    (insert string-no-nl)
    (set-marker logo-input-marker (point))
    ;; Need to clear some old text after input end
    ;; Default is clear to the end of line
    (let ((spaces (min logo-setcursor-spaces-old
		       (mod (- logo-term-columns
			       (mod (current-column)
				    logo-term-columns))
			    logo-term-columns))))
      (insert-char 32 spaces)
      ;; prepare for overwriting after spaces
      (save-excursion
	(save-restriction
	  (widen)
	  (logo-overwrite-help (+ (length string-no-nl) spaces)
			       (save-excursion
				 (let ((oend (point)))
				   (beginning-of-line)
				   (- oend (point)))))))))
  (goto-char logo-input-marker)
  ;; Do not recenter window if in setcursor or terminal mode
  (or (equal logo-term-mode 'emacs)
      (or (pos-visible-in-window-p
	   (point) (selected-window))
	  (recenter -1))))

(defun logo-setcursor-mouse-1 (event)
  "Sending point to logo-input-marker after click with mouse-1.
Avoiding beeping after clicking to focus inferior-logo buffer."
  (interactive "@e")
  (mouse-set-point event)
  ;; Jump only if point is out of logo command line
  (if (or (< (point) (marker-position (process-mark (logo-proc))))
	  (> (point) (marker-position logo-input-marker)))
      (goto-char logo-input-marker)))

;;; ======================== Logo debugging ========================
;;
;;; Logo-debug minor mode

(defvar logo-procedures nil "part of logo contents list.")
(defvar logo-variables nil "part of logo contents list.")
(defvar logo-properties nil "part of logo contents list.")
(defvar logo-debug-string nil "mode line identifier.")
(defvar logo-debug-command nil "trace step or erase")

(defvar logo-debug-mode nil "Mode variable for logo-debug minor mode.")
(make-variable-buffer-local 'logo-debug-mode)

(defvar logo-debug-mode-map nil)
(if logo-debug-mode-map
    nil
  (setq logo-debug-mode-map (make-sparse-keymap))
  ;; Overriding `dangerous' keys in View-mode
  (define-key logo-help-mode-map "E" 'ignore)
  (define-key logo-help-mode-map "e" 'ignore)
  (define-key logo-help-mode-map "Q" 'ignore)
  (define-key logo-help-mode-map "c" 'ignore)
  (define-key logo-help-mode-map "C" 'ignore)  
  ;; mouse bindings
  (define-key logo-debug-mode-map [down-mouse-1] 'logo-mouse-set-point)
  (define-key logo-debug-mode-map [mouse-1] 'logo-xor-debug)
  (define-key logo-debug-mode-map [down-mouse-2] 'logo-mouse-set-point)
  (define-key logo-debug-mode-map [mouse-2] 'logo-or-debug)
  (define-key logo-debug-mode-map [down-mouse-3] 'logo-mouse-set-point)
  (define-key logo-debug-mode-map [mouse-3] 'logo-and-go-debug))

(defun logo-debug-mode ()
  "Logo debug minor mode.

Mouse bindings are as in XFM file-manager.

Mouse-1 De-selects everything previously selected in category
(procedures, variables, properties) and selects chosen name.
Mouse-2 toggles selection for name.
Mouse-3 selects and quits debug buffer. `q' key also quits buffer
and sends the selected contents list to Logo.

\\{logo-debug-mode-map}"
  (interactive)
  (logo-debug-mode-initialize)
  (setq logo-debug-mode t))

(defun logo-debug-mode-initialize ()
  (or (assq 'logo-debug-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'logo-debug-mode logo-debug-mode-map)
		minor-mode-map-alist)))
  (if (not (assq 'logo-debug-mode minor-mode-alist))
      (setq minor-mode-alist
	    (cons (list 'logo-debug-mode logo-debug-string)
		  minor-mode-alist))
    (setcdr (assq 'logo-debug-mode minor-mode-alist)
	    (list logo-debug-string))
    (prin1 minor-mode-alist))
  ;; Logo specific word constituents
  (modify-syntax-entry ?. "w   ")
  (modify-syntax-entry ?? "w   ")
  (modify-syntax-entry ?_ "w   ")
  (modify-syntax-entry ?: "w   ")
  (modify-syntax-entry ?, "w   ")
  ;; These are here to handle names in CSLS match program
  (modify-syntax-entry ?^ "w   ")
  (modify-syntax-entry ?# "w   ")
  (modify-syntax-entry ?@ "w   ")
  (modify-syntax-entry ?& "w   ")
  (modify-syntax-entry ?! "w   ")
  (modify-syntax-entry ?' "w   ")
  ;; These are for nut-cases that enjoy constructing names with `\'
  (modify-syntax-entry ?+ "w   ")
  (modify-syntax-entry ?- "w   ")
  (modify-syntax-entry ?* "w   ")
  (modify-syntax-entry ?/ "w   ")
  (modify-syntax-entry ?< "w   ")
  (modify-syntax-entry ?> "w   ")
  (modify-syntax-entry ?~ "w   "))

(defun logo-debug-function ()
  "Initializes debug mode."
  (and (equal (buffer-file-name) logo-debug-file)
       (logo-debug-mode)))

(defun logo-debug-select (command)
  "Show contents for selection."
  (if (not (file-writable-p logo-debug-file))
      (error "Check your write permission on %s" logo-debug-file))
  (setq logo-debug-command command)
  (logo-flush-contents)
  (logo-send-command
   (concat "emacs.debug "
	   (logoize-path logo-debug-file)
	   (cond ((string= "trace" command) " \"traced untrace contents")
		 ((string= "step" command) " \"stepped unstep contents")
		 ;; Just display contents -- for erase
		 (t " [[] [] []]"))))
  ;; I guess 5 seconds should do even on *very* slow machine
  (with-timeout
      (5 (error "Logo did not write to %s file" logo-debug-file))
    (while (or (not (file-readable-p logo-debug-file))
	       logo-busy-p)
      (sleep-for 0.3)))
  (unwind-protect
      (progn
	(remove-hook 'view-mode-hook 'logo-help-function)
	(view-file logo-debug-file)
	(setq buffer-read-only nil)
	(fill-region (point-min) (point-max) 'left)
	(setq buffer-read-only t)
	(logo-debug-overlay)
	(save-buffer 0)
	(delete-file logo-debug-file))
    (add-hook 'view-mode-hook 'logo-help-function)))

(defun logo-debug-overlay ()
  "Arrange overlays for debug buffer."
  (goto-char (point-min))
  (re-search-forward "PROCEDURES:")
  (overlay-put (make-overlay (match-beginning 0)
			       (match-end 0))
	       'face 'bold)
  (beginning-of-line 3)
  (let ((proc-beg (point)))
    (re-search-forward "VARIABLES:")
    (overlay-put (make-overlay (match-beginning 0)
			       (match-end 0))
		 'face 'bold)
    (end-of-line 0)
    (overlay-put (make-overlay proc-beg (point))
		 'category 'logo-procedures)
    (beginning-of-line 4)
    (let ((var-beg (point)))
      (re-search-forward "PROPERTIES:")
      (overlay-put (make-overlay (match-beginning 0)
				 (match-end 0))
		   'face 'bold)
      (end-of-line 0)
      (overlay-put (make-overlay var-beg (point))
		   'category 'logo-variables))
    (beginning-of-line 4)
    (overlay-put (make-overlay (point) (point-max))
		 'category 'logo-properties)
    (goto-char proc-beg)
    (logo-mark-debugged)
    (goto-char proc-beg)))

(defun logo-mark-debugged ()
  "Marking previously traced or stepped names."
  (while (and (skip-chars-forward "^(")
 	      (not (eobp)))
    (or (progn
	  (forward-char)
	  (skip-syntax-forward "\w" (point-max))
	  (if (looking-at ")")
	      (progn
		(add-to-list
		 (overlay-get (car (overlays-at (point))) 'category)
		 (current-word t))
		(logo-highlight 'highlight))))
 	(forward-char))))

(defun logo-trace ()
  "Select objects for tracing."
  (interactive)
  (setq logo-debug-string " Logo-TRACE")
  (logo-debug-select "trace"))

(defun logo-step ()
  "Select objects for stepping."
  (interactive)
  (setq logo-debug-string " Logo-STEP")
  (logo-debug-select "step"))

(defun logo-erase ()
  "Select objects for erasing from Logo workspace."
  (interactive)
  (setq logo-debug-string " Logo-ERASE")
  (logo-debug-select "erase"))

(defun logo-untrace-all ()
  "Untracing contents."
  (interactive)
  (logo-send-command "untrace contents"))

(defun logo-unstep-all ()
  "Unstepping contents."
  (interactive)
  (logo-send-command "unstep contents"))

(defun logo-send-command (command)
  "Send debug command to process Logo."
  (comint-send-string (logo-proc)
		      (concat command " pr []\n")))

(defun logoize-path (path)
  "Convert path to logo path."
  (concat "\"" (logoize-help path)))

(defun logoize-help (path)
  "Double the `/'"
  (if (string-match "\\(\\`\\|[^/]\\)\\(/\\)[^/]" path)
      (logoize-help (replace-match "//" t t path 2))
    path))

(defun logo-highlight (face)
  "Highlight debug selection."
  (save-excursion
    (let* ((beg (progn (skip-syntax-backward "\w") (point)))
	   (end (progn (skip-syntax-forward "\w") (point)))
	   (highlight (or (logo-overlaid-p (overlays-at beg) beg end)
			  (make-overlay beg end))))
      (overlay-put highlight 'face face))))

(defun logo-overlaid-p (overlays beg end)
  "Use existing overlay if created?"
  (cond ((null overlays) nil)
	((and (= (overlay-start (car overlays)) beg)
	      (= (overlay-end (car overlays)) end))
	 (car overlays))
	(t (logo-overlaid-p (cdr overlays) beg end))))

(defun logo-extinguish ()
  "Turn off highlighting for category."
  (save-excursion
    (mark-paragraph)
    (while (< (point) (mark))
      (forward-word 1)
      (if (cdr (overlays-at (1- (point))))
	  (logo-highlight 'default)))))

(defun logo-and-go-debug (event)
  "Close debug window and send names to logo."
  (interactive "@e")
  (mouse-set-point event)
  (let* ((word (current-word t))
	 (overlay (overlays-at (point)))
	 (category (and overlay
			(or (overlay-get (car overlay) 'category)
			    (condition-case nil
				(overlay-get (car (cdr overlay))
					     'category)
			      (error nil))))))
    (if (or (not word)
	    (not (= (point) logo-mouse-pos))
	    (not category))
	(message "Mouse-3 adds and sends to logo -- point to `name' and try again.")
      (add-to-list category word)
      (if (< emacs-major-version 20)
	  (view-exit)
	(View-kill-and-leave)))))

(defun logo-or-debug (event)
  "Add or remove `name' to appropriate list of names for debugging."
  (interactive "@e")
  (mouse-set-point event)
  (let* ((word (current-word t))
	 (overlay (overlays-at (point)))
	 (category (and overlay
			(or (overlay-get (car overlay) 'category)
			    (condition-case nil
				(overlay-get (car (cdr overlay))
					     'category)
			      (error nil))))))
    (if (or (not word)
	    (not (= (point) logo-mouse-pos))
	    (not category))
	(message "Mouse-2 reverses choice -- point to `name' and try again.")
      (if (member word (eval category))
	  (progn
	    (logo-highlight 'default)
	    (set category (delete word (eval category))))
	(logo-highlight 'highlight)
	(add-to-list category word)))))

(defun logo-xor-debug (event)
  "Remove all and add `name' to appropriate list of names for debugging."
  (interactive "@e")
  (mouse-set-point event)
  (let* ((word (current-word t))
	 (overlay (overlays-at (point)))
	 (category (and overlay
			(or (overlay-get (car overlay) 'category)
			    (condition-case nil
				(overlay-get (car (cdr overlay))
					     'category)
			      (error nil))))))
    (if (or (not word)
	    (not (= (point) logo-mouse-pos))
	    (not category))
	(message "Mouse-1 flushes all, then adds to list -- point to `name' and try again.")
      (logo-extinguish)
      (logo-highlight 'highlight)
      (logo-flush-contents category)
      (add-to-list category word))))

(defun logo-send-debug ()
  "Send debug request to logo process."
  (logo-send-command
   (concat logo-debug-command
	   "["
	   (logo-contents-to-string (list logo-procedures
					  logo-variables
					  logo-properties))
	   (if (string= logo-debug-command "erase")
	       " gc"
	     ""))))

(defun logo-flush-contents (&optional category)
  "Flushing three parts of contents list."
  (if category
      (set category nil)
    (setq logo-procedures nil)
    (setq logo-variables nil)
    (setq logo-properties nil)))

(defun logo-contents-to-string (contents)
  "convert contents list to string."
  (cond ((stringp contents) contents)
	((null contents) "]")
	(t (concat (if (listp (car contents))
		       "["
		     " ")
		   (logo-contents-to-string (car contents))
		   (logo-contents-to-string (cdr contents))))))

;;; ================== Test for system filters ====================

(defun run-logo-filter-test ()
  "Testing TERMINAL ACCESS strings for new system.
If you want logo.el updated, e-mail results of this
to <hrvoje.blazevic@ri.tel.hr>."
  (interactive)
  (logo-send-file
   nil nil
   (concat
    "pr []"
    "pr [******** cut here and e-mail to <hrvoje.blazevic@ri.tel.hr> ********]"
    "pr []"
    "pr [Terminal access test for: ]"
    "pr \"" (prin1-to-string system-type) "\n"
    "pr \"" (prin1-to-string (getenv "TERM")) "\n"
    "pr [] pr []"
    "pr [CLEARTEXT test:]"
    "CLEARTEXT pr [] pr []"
    "pr [SETCURSOR test:]"
    "SETCURSOR [1 1] pr [] pr []"
    "pr [STANDOUT test:]"
    "type STANDOUT \"XxX pr [] pr []"
    "pr [SETMARGINS test:]"
    "SETMARGINS [0 0] pr [] pr []"
    "pr [**************************** test end *****************************]"
    "\n")))

;;; ======================== end code =============================


(provide 'logo)


;;; =========== Add file emacs.debug to ucblogo library ===========
;;; ===========     if you have ucblogo-4.2 or older    ===========
;;
;;  ;;;  -*- logo -*-
;;
;;  to emacs.debug :file_elisp_code :elisp_dummy
;;  localmake "wr_elisp_code writer
;;  if namep "printwidthlimit [
;;     localmake "pw_elisp_code :printwidthlimit
;;     ern "printwidthlimit]
;;  openwrite :file_elisp_code
;;  setwrite :file_elisp_code
;;  bury [[] [wr_elisp_code pw_elisp_code file_elisp_code elisp_dummy] []]
;;  (foreach contents
;;           [PROCEDURES: VARIABLES: PROPERTIES:]
;;           [[names title]
;;            [pr :title pr [] pr :names pr []]])
;;  close :file_elisp_code
;;  setwrite :wr_elisp_code
;;  if namep "pw_elisp_code [make "printwidthlimit :pw_elisp_code]
;;  end
;;
;;  bury "emacs.debug
;;
;;; ===============================================================


;;; logo.el  ends here

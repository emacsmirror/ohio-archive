;; kumac-mode.el --- Kumac mode for GNU Emacs and GNU XEmacs

;; Copyright (C) 1994 David P. Morrison
;; Copyright (C) 1995, 1997 Ulrik Dickow

;; Authors: David P. Morrison <morrison@orph01.phy.ornl.gov>
;;                            <URL:http://uther1.phy.ornl.gov/~morrison/> and
;;          Ulrik Dickow <dickow@nbi.dk> <URL:http://www.nbi.dk/~dickow/>
;; Version: $Id: kumac-mode.el,v 2.4 1997/03/09 15:53:55 dickow Exp $

;; LCD Archive Entry:
;; kumac-mode|David P. Morrison|morrison@orph01.phy.ornl.gov|
;; Major mode for editing Kumac (CERN KUIP macro) files.|
;; 09-Mar-1997|2.4|~/modes/kumac-mode.el.gz|

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;; fortran.el, of which kumac-mode.el is a shameless derivative, is
;; available from hallc1.cebaf.gov, and as part of the standard Emacs
;; distribution. fortran.el was written by Michael D. Prange and is
;; currently maintained by Stephen A. Wood.

;;
;; Installation:
;; 
;; Put kumac-mode.el in your load path and byte-compile it.  You can ignore
;; all compilation warnings (about free variables and an unknown function).
;; Then add these lines to your ~/.emacs file (without the ";;" in front):
;;
;;   (autoload 'kumac-mode "kumac-mode" "Mode for editing KUMAC files." t)
;;   (setq auto-mode-alist
;;         (cons (cons "\\.kumac$" 'kumac-mode) auto-mode-alist))
;;
;; If you wish to enable the automatic expansion of the kumac-mode
;; abbreviations, also add the following lines to your .emacs file:
;;
;;   (add-hook 'kumac-mode-hook 
;;             (lambda () (abbrev-mode 1)))
;;
;; If you want highlighting of strings, comments and some keywords, you might
;;
;;   (add-hook 'kumac-mode-hook (lambda () (font-lock-mode 1)))
;;
;; However, you probably want font-lock for all modes supporting it,
;; highlighting as much as possible in each mode.  With Emacs 19.31 and later,
;; just put these two lines in your ~/.emacs instead:
;;
;;   (setq font-lock-maximum-decoration t)
;;   (global-font-lock-mode t)
;;
;; Lusers of Emacs <19.29 may find help at http://www.nbi.dk/~dickow/emacs/.

;;; This file may be used with GNU Emacs version 18.xx you replace
;;;   comment-indent-function               with comment-indent-hook
;;;   (setq unread-command-events (list c)) with (setq unread-command-char c)
;;; by commenting in/out the lines marked with "--Emacs 18--" & "--Emacs 19--".

;; Revision history:
;; 
;; $Log: kumac-mode.el,v $
;; Revision 2.4  1997/03/09 15:53:55  dickow
;; Cleaned up some comments.  Optimized string regexp slightly.
;; Added support for hilit19/hl319 (requested by <J.Holeczek@gsi.de>).
;;
;; Revision 2.3  1995/10/12 16:41:13  dickow
;; Added more junk to make font-lock work better with XEmacs.
;;
;; Revision 2.2  1995/10/12  15:52:45  dickow
;; Font-lock: added support for XEmacs and paved the way for Emacs 19.30.
;; RCS fully introduced.
;;
;; Revision 2.01 (aka 2.1) 1995/08/02 20:37:00 dickow
;; Fontification made more like 19.29 fortran.el's,
;; while remaining backwards compatible.
;;
;; Revision 2.0 1995/08/02 15:50:00 dickow
;; Added font-lock support.  Added/modified lots of comments.
;; Replaced two obsolete Emacs 18.xx variables with Emacs 19 ones.
;;
;; Revision 1.19 1994/02/17 02:33:38 dave
;; The last 1994 version.

(defconst kumac-mode-version "$Revision: 2.4 $")

;;; Code:

(defvar kumac-block-indent 2
  "*Extra indentation applied to all blocks.")

(defvar kumac-label-indent 0
  "*Indentation applied to labels.")

(defvar kumac-comment-indent-style 'fixed
  "*nil forces comment lines not to be touched,
'fixed makes fixed comment indentation to `kumac-comment-line-extra-indent'
columns beyond `kumac-minimum-statement-indent-fixed' (for
`indent-tabs-mode' of nil) or `kumac-minimum-statement-indent-tab' (for
`indent-tabs-mode' of t), and 'relative indents to current
Kumac indentation plus `kumac-comment-line-extra-indent'.")

(defvar kumac-comment-line-extra-indent 1
  "*Amount of extra indentation for text within full-line comments.")

(defvar comment-line-start nil
  "*Delimiter inserted to start new full-line comment.")

(defvar comment-line-start-skip nil
  "*Regexp to match the start of a full-line comment.")

(defvar label-start-skip nil
  "*Regexp to match the start of a label.")

(defvar kumac-minimum-statement-indent-fixed 1
  "*Minimum statement indentation for fixed format continuation style.")

;; Note that this is documented in the v18 manuals as being a string
;; of length one rather than a single character.
;; The code in this file accepts either format for compatibility.
(defvar kumac-comment-indent-char " "
  "*Single-character string inserted for Kumac comment indentation.
Normally a space.")

(defvar kumac-check-all-num-for-matching-do nil
  "*Non-nil causes all numbered lines to be treated as possible DO loop ends.")

(defvar kumac-comment-region "*$$$"
  "*String inserted by \\[kumac-comment-region]\
 at start of each line in region.")

(defvar kumac-startup-message t
  "*Non-nil displays a startup message when Kumac mode is first called.")

(defconst bug-kumac-mode "morrison@orph01.phy.ornl.gov"
  "Address for reporting Kumac mode bugs.")

(defvar kumac-mode-syntax-table nil
  "Syntax table in use in Kumac mode buffers.")

(if kumac-mode-syntax-table
    ()
  (setq kumac-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\; "w" kumac-mode-syntax-table)
  (modify-syntax-entry ?:  "w" kumac-mode-syntax-table)
  (modify-syntax-entry ?\r " " kumac-mode-syntax-table)
  (modify-syntax-entry ?+ "." kumac-mode-syntax-table)
  (modify-syntax-entry ?- "." kumac-mode-syntax-table)
  (modify-syntax-entry ?= "." kumac-mode-syntax-table)
  (modify-syntax-entry ?* "." kumac-mode-syntax-table)
  (modify-syntax-entry ?/ "." kumac-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" kumac-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" kumac-mode-syntax-table)
  (modify-syntax-entry ?\\ "/" kumac-mode-syntax-table)
  (modify-syntax-entry ?. "w" kumac-mode-syntax-table)
  (modify-syntax-entry ?_ "w" kumac-mode-syntax-table)
  (modify-syntax-entry ?\n ">" kumac-mode-syntax-table))

(defvar kumac-mode-map () 
  "Keymap used in Kumac mode.")
(if kumac-mode-map
    ()
  (setq kumac-mode-map (make-sparse-keymap))
  (define-key kumac-mode-map ";" 'kumac-abbrev-start)
  (define-key kumac-mode-map "\C-c;" 'kumac-comment-region)
  (define-key kumac-mode-map "\e\C-a" 'beginning-of-kumac-subprogram)
  (define-key kumac-mode-map "\e\C-e" 'end-of-kumac-subprogram)
  (define-key kumac-mode-map "\e;" 'kumac-indent-comment)
  (define-key kumac-mode-map "\e\C-h" 'mark-kumac-subprogram)
  (define-key kumac-mode-map "\e\n" 'kumac-split-line)
  (define-key kumac-mode-map "\n" 'kumac-reindent-then-newline-and-indent)
  (define-key kumac-mode-map "\r" 'kumac-reindent-then-newline-and-indent)
  (define-key kumac-mode-map "\e\C-q" 'kumac-indent-subprogram)
  (define-key kumac-mode-map "\C-c\C-c" 'kumac-insert-comis)
  (define-key kumac-mode-map "\C-c\C-p" 'kumac-previous-statement)
  (define-key kumac-mode-map "\C-c\C-n" 'kumac-next-statement)
  (define-key kumac-mode-map "*" 'kumac-electric-comment)
  (define-key kumac-mode-map "\t" 'kumac-indent-line))

(defvar kumac-mode-abbrev-table nil)
(if kumac-mode-abbrev-table
    ()
  (let ((ac abbrevs-changed))
    (define-abbrev-table 'kumac-mode-abbrev-table ())
    (define-abbrev kumac-mode-abbrev-table  ";c"   "case" nil)
    (define-abbrev kumac-mode-abbrev-table  ";cl"  "close" nil)
    (define-abbrev kumac-mode-abbrev-table  ";dw"  "do while" nil)
    (define-abbrev kumac-mode-abbrev-table  ";e"   "else" nil)
    (define-abbrev kumac-mode-abbrev-table  ";ec"  "endcase" nil)
    (define-abbrev kumac-mode-abbrev-table  ";ed"  "enddo" nil)
    (define-abbrev kumac-mode-abbrev-table  ";el"  "elseif" nil)
    (define-abbrev kumac-mode-abbrev-table  ";en"  "endif" nil)
    (define-abbrev kumac-mode-abbrev-table  ";ew"  "endwhile" nil)
    (define-abbrev kumac-mode-abbrev-table  ";ff"  "fortran/file" nil)
    (define-abbrev kumac-mode-abbrev-table  ";g"   "goto" nil)
    (define-abbrev kumac-mode-abbrev-table  ";hc"  "histogram/copy" nil)
    (define-abbrev kumac-mode-abbrev-table  ";hd"  "histogram/delete" nil)
    (define-abbrev kumac-mode-abbrev-table  ";hf"  "histogram/file" nil)
    (define-abbrev kumac-mode-abbrev-table  ";hp"  "histogram/plot" nil)
    (define-abbrev kumac-mode-abbrev-table  ";m"   "macro" nil)
    (define-abbrev kumac-mode-abbrev-table  ";me"  "message" nil)
    (define-abbrev kumac-mode-abbrev-table  ";nj"  "ntuple/proj" nil)
    (define-abbrev kumac-mode-abbrev-table  ";nl"  "ntuple/loop" nil)
    (define-abbrev kumac-mode-abbrev-table  ";np"  "ntuple/plot" nil)
    (define-abbrev kumac-mode-abbrev-table  ";rt"  "return" nil)
    (define-abbrev kumac-mode-abbrev-table  ";u"   "until" nil)
    (define-abbrev kumac-mode-abbrev-table  ";vc"  "vector/create" nil)
    (define-abbrev kumac-mode-abbrev-table  ";vd"  "vector/delete" nil)
    (define-abbrev kumac-mode-abbrev-table  ";vi"  "vector/input" nil)
    (define-abbrev kumac-mode-abbrev-table  ";vr"  "vector/read" nil)
    (setq abbrevs-changed ac)))

;; Font lock patterns by Ulrik Dickow <dickow@nbi.dk>.
;; The 3 first doc-strings stolen from Simon Marshall <simon@gnu.ai.mit.edu>.
;; There's still plenty of room for a third level of gaudiness.
;; No fancy Emacs 19.29+ features used.  This makes it easy to stay compatible
;; with older emacsen and with people using hilit19/hl319 (see next page).

(defconst kumac-font-lock-keywords-1
 '(;;
   ;; Highlight full-line comments. They start with "*" in the first column.
   ("^\\*.*" 0 font-lock-comment-face)
   ;; Highlight in-line (`|') comments.  Avoid |'s in strings.
   ;; Emacs 19.29+ can do this faster, but let's stick to the old way.
   ("^[^|'\n]*\\('[^'\n]*'[^|'\n]*\\)*\\(|.*\\)" 2 font-lock-comment-face)
   ;; Highlight strings.
   ("'[^'\n]*'" 0 font-lock-string-face)
   ;; Highlight macro names being defined.
   ("\\<macro[ \t]+\\([a-z][a-z0-9]*\\)" 1 font-lock-function-name-face)
   ;; Highlight these main keywords (in keyword face).
   "\\<\\(macro\\|return\\|exitm\\|exec\\|application\\|quit\\)\\>"
   ;; Highlight macro names being executed (let's be liberal with `#' :-).
   ("\\<exec[ \t]+\\([a-z][a-z0-9#]*\\)" 1 font-lock-function-name-face))
 "Subdued level highlighting for Kumac mode.")

(defconst kumac-font-lock-keywords-2
  (append
   kumac-font-lock-keywords-1
   (list 
    ;; Highlight control structure keywords in default keyword face.
    (concat "\\<\\("
	    "breakl\\|case\\|do\\|else\\(if\\)?\\|"
	    "end\\(case\\|do\\|for\\|if\\|while\\)\\|"
	    "for\\|goto\\|if\\|in\\|repeat\\|then\\|until\\|while"
	    "\\)\\>")
    ;; Highlight Fortran-like logicals: and or not lt le eq ne ge gt
    "\\.\\(and\\|eq\\|g[et]\\|l[et]\\|n\\(e\\|ot\\)\\|or\\)\\."
    ;; Highlight goto labels.
    '("\\<goto[ \t]+\\([a-z][a-z0-9]*\\)\\>" 1 font-lock-reference-face)
    ;; Highlight statement labels and case labels.
    '("^[ \t]*\\([a-z][a-z0-9]*:\\|([^)\n]*)\\)" 1 font-lock-reference-face)))
  "Gaudy level highlighting for Kumac mode.")

(defvar kumac-font-lock-keywords
  (if (and (boundp 'font-lock-use-maximal-decoration) ; XEmacs
	   (not font-lock-use-maximal-decoration))
      kumac-font-lock-keywords-1
    kumac-font-lock-keywords-2)
  "Default expressions to highlight in Kumac mode.")

;; font-lock compatibility with XEmacs/Lucid and older Emacsen (<19.29).
;;
(cond ((string-match "XEmacs\\|Lucid" (emacs-version))
       ;; Make sure `font-lock-reference-face' is defined.
       ;; Respect X resources (`make-face' uses them when they exist).
       (or (if (fboundp 'facep)
	       (facep 'font-lock-reference-face)
	     (memq 'font-lock-reference-face (face-list)))
	   (make-face 'font-lock-reference-face))
       (or (face-differs-from-default-p 'font-lock-reference-face)
	   (set-face-foreground 'font-lock-reference-face "violet")))
      ;; For Emacs <19.29 we fall back to the doc string face
      ;; (obsoleted by 19.29), if needed.
      ((and (not (boundp 'font-lock-reference-face))
	    (or (not (boundp 'emacs-version))
		(string-lessp emacs-version "19.28.90")))
       (defvar font-lock-reference-face 'font-lock-doc-string-face
	 "Face to use for references -- and various other things.")))

(defun kumac-hack-xemacs-syntax ()
  "Force XEmacs to ignore the syntax table when fontifying.
XEmacs no longer (19.13) understands `font-lock-no-comments'.
Thus we have to add this hack to `font-lock-mode-hook', unless we modify the
kumac syntax table permanently (remove all comment and string syntax from it).
Kumac's comment syntax is too complicated to be handled by a syntax table."
  (if (eq major-mode 'kumac-mode)
      (setq font-lock-use-syntax-tables nil)))

;; Some people still use the hilit19 and hl319 packages.
;; To make them happy, let's repeat all of the font-lock patterns and comments,
;; this time feeding the stuff into the `hilit-set-mode-patterns' function --
;; but only if the user has already loaded one of those packages.
;; The hilit packages should have a function for importing a simple font-lock
;; list (changing `font-lock-comment-face' into `comment' etc.).
;;
;; This support was requested & submitted by Jacek M. Holeczek, 05-Mar-1997.

(if (featurep 'hilit19)
    (hilit-set-mode-patterns
     'kumac-mode
     (list
      ;; Highlight full-line comments. They start with "*" in the first column.
      '("^\\*.*" nil comment)
      ;; Highlight in-line (`|') comments.  Avoid |'s in strings.
      '("^[^|'\n]*\\('[^'\n]*'[^|'\n]*\\)*\\(|.*\\)" 2 comment)
      ;; Highlight strings.
      '("'[^'\n]*'" nil string)
      ;; Highlight macro names being defined.
      '("\\<macro[ \t]+\\([a-z][a-z0-9]*\\)" 1 defun)
      ;; Highlight these main keywords (in keyword face).
      '("\\<\\(macro\\|return\\|exitm\\|exec\\|application\\|quit\\)\\>"
        nil keyword)
      ;; Highlight macro names being executed (let's be liberal with `#' :-).
      '("\\<exec[ \t]+\\([a-z][a-z0-9#]*\\)" 1 defun)
      ;; Highlight control structure keywords in default keyword face.
      (list (concat "\\<\\("
                    "breakl\\|case\\|do\\|else\\(if\\)?\\|"
                    "end\\(case\\|do\\|for\\|if\\|while\\)\\|"
                    "for\\|goto\\|if\\|in\\|repeat\\|then\\|until\\|while"
                    "\\)\\>") nil 'keyword)
      ;; Highlight Fortran-like logicals: and or not lt le eq ne ge gt
      '("\\.\\(and\\|eq\\|g[et]\\|l[et]\\|n\\(e\\|ot\\)\\|or\\)\\."
        nil keyword)
      ;; Highlight goto labels.
      '("\\<goto[ \t]+\\([a-z][a-z0-9]*\\)\\>" 1 label)
      ;; Highlight statement labels and case labels.
      '("^[ \t]*\\([a-z][a-z0-9]*:\\|([^)\n]*)\\)" 1 label))
     nil 'case-insensitive)
  nil)

(defun kumac-mode ()
  "Major mode for editing Kumac code.
\\[kumac-indent-line] indents the current Kumac line correctly. 

Type ;? or ;\\[help-command] to display a list of built-in\
 abbrevs for Kumac keywords.

Key definitions:
\\{kumac-mode-map}

Variables controlling indentation style and extra features:

 comment-start
    Normally `|' in Kumac mode for inline comments. 
 kumac-block-indent
    Extra indentation within all blocks.  (default 3)
 kumac-comment-line-extra-indent
    Amount of extra indentation for text within full-line comments. (default 0)
 kumac-comment-indent-style
    nil    means don't change indentation of text in full-line comments,
    fixed  means indent that text at `kumac-comment-line-extra-indent' beyond
           the value of `kumac-minimum-statement-indent-fixed' (for fixed
           format continuation style).
    relative  means indent at `kumac-comment-line-extra-indent' beyond the
 	      indentation for a line of code.
    (default 'fixed)
 kumac-comment-indent-char
    Single-character string to be inserted instead of space for
    full-line comment indentation.  (default \" \")
 kumac-minimum-statement-indent-fixed
    Minimum indentation for Kumac statements in fixed format mode. (def.6)
 kumac-comment-region
    String inserted by \\[kumac-comment-region] at start of each line in 
    region.  (default \"*$$$\")
 kumac-font-lock-keywords
    The default `font-lock-keywords' for \\[font-lock-mode] highlighting. 
 kumac-startup-message
    Set to nil to inhibit message first time Kumac mode is used.

Turning on Kumac mode calls the value of the variable `kumac-mode-hook'
with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (if kumac-startup-message
      (message "Emacs Kumac mode %s.  Bugs to %s"
	       kumac-mode-version bug-kumac-mode))
  (setq kumac-startup-message nil)
  (setq local-abbrev-table kumac-mode-abbrev-table)
  (set-syntax-table kumac-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'kumac-indent-line)
  (make-local-variable 'comment-indent-function) ; --Emacs 19--
  (setq comment-indent-function 'kumac-comment-hook) ; --Emacs 19--
;;  (make-local-variable 'comment-indent-hook) ; --Emacs 18--
;;  (setq comment-indent-hook 'kumac-comment-hook) ; --Emacs 18--
  (make-local-variable 'comment-line-start-skip)
  (setq comment-line-start-skip
	"^\*[ \t]*")
  (make-local-variable 'comment-line-start)
  (setq comment-line-start "*")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "|[ \t]*")
  (make-local-variable 'comment-start)
  (setq comment-start "| ")
  (make-local-variable 'label-start-skip)
  (setq label-start-skip "^[ \t]*[a-zA-Z]+[a-zA-Z0-9_]*:")
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'abbrev-all-caps)
  (setq abbrev-all-caps t)
  (setq fill-column 72) ; Automatically local
  (use-local-map kumac-mode-map)
  (setq mode-name "Kumac")
  (setq major-mode 'kumac-mode)
  (make-local-variable 'kumac-comment-line-extra-indent)
  (make-local-variable 'kumac-minimum-statement-indent-fixed)
  ;;
  ;; font-lock setup for various emacsen: XEmacs, Emacs 19.29+, Emacs <19.29.
  ;; By Ulrik Dickow <dickow@nbi.dk>.
  (cond	((string-match "XEmacs\\|Lucid" (emacs-version)) ; XEmacs/Lucid
	 (put major-mode 'font-lock-keywords-case-fold-search t)
	 ;; XEmacs (19.13, at least) is too "smart".  Outsmart it.
	 (add-hook 'font-lock-mode-hook 'kumac-hack-xemacs-syntax))
	 ;; XEmacs guesses the rest correctly.
	 ;; If any older XEmacsen don't, then tell me.
	 ;;
	((string-lessp "19.28.89" emacs-version) ; Emacs 19.29 and later
	 (make-local-variable 'font-lock-defaults)
	 (setq font-lock-defaults '(kumac-font-lock-keywords t t)))
	;;
	(t ; Emacs 19.28 and older
	 (make-local-variable 'font-lock-keywords-case-fold-search)
	 (make-local-variable 'font-lock-keywords)
	 (make-local-variable 'font-lock-no-comments)
	 (setq font-lock-keywords-case-fold-search t)
	 (setq font-lock-keywords kumac-font-lock-keywords)
	 (setq font-lock-no-comments t)))
  (run-hooks 'kumac-mode-hook))

(defun kumac-comment-hook ()
  (save-excursion
    (skip-chars-backward " \t")
    (max (+ 1 (current-column))
	 comment-column)))

(defun kumac-indent-label ()
  "Align label on current line."
  (interactive)
  (beginning-of-line)
  (if (looking-at label-start-skip)
      (delete-horizontal-space)))

(defun kumac-indent-comment ()
  "Align or create comment on current line.
Existing comments of all types are recognized and aligned.
If the line has no comment, a side-by-side comment is inserted and aligned
if the value of comment-start is not nil.
Otherwise, a separate-line comment is inserted, on this line
or on a new line inserted before this line if this line is not blank."
  (interactive)
  (beginning-of-line)
  ;; Recognize existing comments of either kind.
  (cond ((looking-at comment-line-start-skip)
	 (kumac-indent-to-column 3))
	((kumac-find-comment-start-skip) ; catches any inline comment and
					; leaves point after comment-start-skip
	 (if comment-start-skip
	     (progn (goto-char (match-beginning 0))
		    (if (not (= (current-column) (kumac-comment-hook)))
			(progn (delete-horizontal-space)
			       (indent-to (kumac-comment-hook)))))
	   (end-of-line)))        ; otherwise goto end of line or sth else?
	;; No existing comment.
	;; If side-by-side comments are defined, insert one,
	;; unless line is now blank.
	((and comment-start (not (looking-at "^[ \t]*$")))
	 (end-of-line)
	 (delete-horizontal-space)
	 (indent-to (kumac-comment-hook))
	 (insert comment-start))
	;; Else insert separate-line comment, making a new line if nec.
	(t
	 (if (looking-at "^[ \t]*$")
	     (delete-horizontal-space)
	   (beginning-of-line)
	   (insert "\n")
	   (forward-char -1))
	 (insert comment-line-start)
	 (insert-char (if (stringp kumac-comment-indent-char)
			  (aref kumac-comment-indent-char 0)
			kumac-comment-indent-char)
		      (- (calculate-kumac-indent) (current-column))))))

(defun kumac-comment-region (beg-region end-region arg)
  "Comments every line in the region.
Puts kumac-comment-region at the beginning of every line in the region. 
BEG-REGION and END-REGION are args which specify the region boundaries. 
With non-nil ARG, uncomments the region."
  (interactive "*r\nP")
  (let ((end-region-mark (make-marker)) (save-point (point-marker)))
    (set-marker end-region-mark end-region)
    (goto-char beg-region)
    (beginning-of-line)
    (if (not arg)			;comment the region
	(progn (insert kumac-comment-region)
	       (while (and  (= (forward-line 1) 0)
			    (< (point) end-region-mark))
		 (insert kumac-comment-region)))
      (let ((com (regexp-quote kumac-comment-region))) ;uncomment the region
	(if (looking-at com)
	    (delete-region (point) (match-end 0)))
	(while (and  (= (forward-line 1) 0)
		     (< (point) end-region-mark))
	  (if (looking-at com)
	      (delete-region (point) (match-end 0))))))
    (goto-char save-point)
    (set-marker end-region-mark nil)
    (set-marker save-point nil)))

(defun kumac-abbrev-start ()
  "Typing ;\\[help-command] or ;? lists all the Kumac abbrevs. 
Any other key combination is executed normally."
  (interactive)
  (let (c)
    (insert last-command-char)
    (if (or (= (setq c (read-char)) ??)	;insert char if not equal to `?'
	    (= c help-char))
	(kumac-abbrev-help)
;;      (setq unread-command-char c)))) ; --Emacs 18--
      (setq unread-command-events (list c))))) ; --Emacs 19--

(defun kumac-abbrev-help ()
  "List the currently defined abbrevs in Kumac mode."
  (interactive)
  (message "Listing abbrev table...")
  (display-buffer (kumac-prepare-abbrev-list-buffer))
  (message "Listing abbrev table...done"))

(defun kumac-prepare-abbrev-list-buffer ()
  (save-excursion
    (set-buffer (get-buffer-create "*Abbrevs*"))
    (erase-buffer)
    (insert-abbrev-table-description 'kumac-mode-abbrev-table t)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (edit-abbrevs-mode))
  (get-buffer-create "*Abbrevs*"))

(defun kumac-split-line ()
  "Break line at point and insert continuation marker and alignment."
  (interactive)
  (delete-horizontal-space)
  (if (save-excursion (beginning-of-line) (looking-at comment-line-start-skip))
      (insert "\n" comment-line-start " ")
      (insert "_\n "))
  (kumac-indent-line))		;when the cont string is C, c or *.

(defun delete-horizontal-regexp (chars)
  "Delete all characters in CHARS around point.
CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \."
  (interactive "*s")
  (skip-chars-backward chars)
  (delete-region (point) (progn (skip-chars-forward chars) (point))))

(defun kumac-electric-comment (arg)
  "Self insert, and if part of a Kumac comment line indent it automatically."
  (interactive "P")
  (let (comment-line)
    (save-excursion
      (narrow-to-region 
       (point) 
       (progn
	 (beginning-of-line)
	 (point)))
      (setq comment-line (or
			  (looking-at comment-line-start-skip)
			  (looking-at "\\s-*$")))
      (widen))
    (self-insert-command 1)
    (if comment-line
	(progn
	  (save-excursion
	    (beginning-of-line)
	    (delete-horizontal-space))
	  (kumac-indent-line)))))

(defun beginning-of-kumac-subprogram ()
  "Moves point to the beginning of the current Kumac subprogram."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line -1)
    (re-search-backward "^[ \t]*return\\b" nil 'move)
    (if (looking-at "^[ \t]*return\\b")
	(forward-line 1))))

(defun end-of-kumac-subprogram ()
  "Moves point to the end of the current Kumac subprogram."
  (interactive)
  (let ((case-fold-search t))
    (beginning-of-line 2)
    (re-search-forward "^[ \t]*return\\b" nil 'move)
    (forward-line 1)))

(defun mark-kumac-subprogram ()
  "Put mark at end of Kumac subprogram, point at beginning. 
The marks are pushed."
  (interactive)
  (end-of-kumac-subprogram)
  (push-mark (point))
  (beginning-of-kumac-subprogram))

(defun kumac-previous-indent-statement ()
  "Moves point to beginning of the previous Kumac statement.
Returns `first-statement' if that statement is the first
non-comment Kumac statement in the file, and nil otherwise."
  (interactive)
  (let (not-first-statement continue-test)
    (beginning-of-line)
    (while (and (setq not-first-statement (= (forward-line -1) 0))
		(or (looking-at comment-line-start-skip)
		    (looking-at label-start-skip)
		    (looking-at "[ \t]*$")
		    (looking-at (concat "[ \t]*"  comment-start-skip)))))
    (if (not not-first-statement)
 	'first-statement)))

(defun kumac-previous-statement ()
  "Moves point to beginning of the previous Kumac statement.
Returns `first-statement' if that statement is the first
non-comment Kumac statement in the file, and nil otherwise."
  (interactive)
  (let (not-first-statement continue-test)
    (beginning-of-line)
    (while (and (setq not-first-statement (= (forward-line -1) 0))
		(or (save-excursion
		      (and (= (forward-line -1) 0)
			   (looking-at ".*_[ \t]*$")))		
		    (looking-at comment-line-start-skip)
		    (looking-at "[ \t]*$")
		    (looking-at (concat "[ \t]*"  comment-start-skip)))))
    (if (not not-first-statement)
 	'first-statement)))

(defun kumac-next-statement ()
  "Moves point to beginning of the next Kumac statement.
Returns `last-statement' if that statement is the last
non-comment Kumac statement in the file, and nil otherwise."
  (interactive)
  (let (not-last-statement)
    (beginning-of-line)
    (while (and (setq not-last-statement
		      (and (= (forward-line 1) 0)
			   (not (eobp))))
 		(or (looking-at comment-line-start-skip)
 		    (looking-at "[ \t]*$")
 		    (looking-at (concat "[ \t]*"  comment-start-skip)))))
    (if (not not-last-statement)
 	'last-statement)))

(defun kumac-indent-line ()
  "Indents current Kumac line based on its contents and on previous lines."
  (interactive)
  (let ((cfi (calculate-kumac-indent)))
    (save-excursion
      (beginning-of-line)
      (if (not (= cfi (kumac-current-line-indentation)))
	  (kumac-indent-to-column cfi))
      (if (and (not (looking-at comment-line-start-skip))
	      (kumac-find-comment-start-skip))
	  (kumac-indent-comment))
      (if (looking-at label-start-skip)
	  (kumac-indent-label)))
    ;;    (beginning-of-line)
    ;; Never leave point in left margin.
    (if (< (current-column) cfi)
	(move-to-column cfi))))

(defun kumac-reindent-then-newline-and-indent ()
  "Reindent the current Kumac line, insert a newline and indent the newline.
An abbrev before point is expanded if `abbrev-mode' is non-nil."
  (interactive)
  (if abbrev-mode (expand-abbrev))
  (newline)
  (forward-line -1)
  (kumac-indent-line)
  (forward-line 1)
  (kumac-indent-line))

(defun kumac-indent-subprogram ()
  "Properly indents the Kumac subprogram which contains point."
  (interactive)
  (save-excursion
    (mark-kumac-subprogram)
    (message "Indenting subprogram...")
    (indent-region (point) (mark) nil))
  (message "Indenting subprogram...done."))

(defun calculate-kumac-indent ()
  "Calculates the Kumac indent column based on previous lines."
  (let (icol first-statement (case-fold-search t)
	     (kumac-minimum-statement-indent 
	      kumac-minimum-statement-indent-fixed)
	     prev-continuation-line
	     in-continuation-line)
    (save-excursion
      (save-excursion
	(kumac-previous-indent-statement)
	(setq prev-continuation-line (kumac-is-in-continuation-line-p)))
      (setq in-continuation-line (kumac-is-in-continuation-line-p))
;      (if prev-continuation-line
;	  (progn
;	    (message "Prev continuation Line")
;	    (sit-for 1)))
;      (if in-continuation-line
;	  (progn
;	    (message "In continuation Line")
;	    (sit-for 1)))
      (setq first-statement (kumac-previous-statement))
      (if first-statement
	  (setq icol kumac-minimum-statement-indent)
	(progn
	  (if (= (point) (point-min))
	      (setq icol kumac-minimum-statement-indent)
	    (setq icol (kumac-current-line-indentation)))
	  (skip-chars-forward " \t")
	  (cond ((looking-at "return\\b")
		 ;; Previous RETURN resets indent to minimum
		 (setq icol kumac-minimum-statement-indent))
		((looking-at "macro\\b")
		 (setq icol (+ icol kumac-block-indent)))
		(in-continuation-line
		 (setq icol (+ icol kumac-block-indent)))
		((looking-at "if.*then\\b")
		 (setq icol (+ icol kumac-block-indent)))
		((looking-at "\\(else\\|elseif\\)\\b")
		 (setq icol (+ icol kumac-block-indent)))
		((looking-at "do\\s-")
		 (setq icol (+ icol kumac-block-indent)))
		((looking-at "while\\s-")
		 (setq icol (+ icol kumac-block-indent)))
		((looking-at "for\\s-")
		 (setq icol (+ icol kumac-block-indent)))
		((looking-at "repeat\\b")
		 (setq icol (+ icol kumac-block-indent)))
		((looking-at "case\\b")
		 (setq icol (+ icol kumac-block-indent)))
		((looking-at label-start-skip)
		 (setq icol kumac-label-indent))))))
    (save-excursion
      (beginning-of-line)
      (cond ((looking-at "[ \t]*$"))
	    ((looking-at comment-line-start-skip)
	     (cond ((eq kumac-comment-indent-style 'relative)
		    (setq icol (+ icol kumac-comment-line-extra-indent)))
		   ((eq kumac-comment-indent-style 'fixed)
		    (setq icol (+ kumac-minimum-statement-indent
				  kumac-comment-line-extra-indent))))
	     (setq kumac-minimum-statement-indent 0))
	    (first-statement)
	    (t
	     (skip-chars-forward " \t")
	     (cond ((looking-at "endif\\b")
		    (setq icol (- icol kumac-block-indent)))
		   ((looking-at "endcase\\b")
		    (setq icol (- icol kumac-block-indent)))
		   ((looking-at "\\(else\\|elseif\\)\\b")
		    (setq icol (- icol kumac-block-indent)))
		   ((looking-at "endwhile\\b")
		    (setq icol (- icol kumac-block-indent)))
		   ((looking-at "enddo\\b")
		    (setq icol (- icol kumac-block-indent)))
		   ((looking-at "endfor\\b")
		    (setq icol (- icol kumac-block-indent)))
		   ((looking-at "until\\b")
		    (setq icol (- icol kumac-block-indent)))
		   ((looking-at "return\\b")
		    (setq icol (- icol kumac-block-indent)))
		   ))))
    (max kumac-minimum-statement-indent icol)))

(defun kumac-current-line-indentation ()
  "Indentation of current line. This is the column position of the 
first non-whitespace character. For comment lines, returns indentation 
of the first non-indentation text within the comment."
  (save-excursion
    (beginning-of-line)
    (if (looking-at comment-line-start-skip)
	(progn 
	  (goto-char (match-end 0))
	  (skip-chars-forward kumac-comment-indent-char)))
    (skip-chars-forward " \t")
    (current-column)))

(defun kumac-indent-to-column (col)
  "Indents current line with spaces to column COL."
  (save-excursion
    (beginning-of-line)
    (if (looking-at comment-line-start-skip)
	(if kumac-comment-indent-style
	    (let ((char (if (stringp kumac-comment-indent-char)
			    (aref kumac-comment-indent-char 0)
			  kumac-comment-indent-char)))
	      (goto-char (match-end 0))
	      (delete-horizontal-regexp (concat " \t" (char-to-string char)))
	      (insert-char char (- col (current-column)))))
      ;; Put bo<dy of statement where specified.
      (delete-horizontal-space)
      (indent-to col)
      ;; Indent any comment following code on the same line.
      (if (and comment-start-skip
	       (kumac-find-comment-start-skip))
	  (progn (goto-char (match-beginning 0))
		 (if (not (= (current-column) (kumac-comment-hook)))
		     (progn (delete-horizontal-space)
			    (indent-to (kumac-comment-hook)))))))))

(defun kumac-find-comment-start-skip ()
  "Move to past `comment-start-skip' found on current line.
Return t if `comment-start-skip' found, nil if not."
;;; In order to move point only if comment-start-skip is found,
;;; this one uses a lot of save-excursions.  Note that re-search-forward
;;; moves point even if comment-start-skip is inside a string-constant.
;;; Some code expects certain values for match-beginning and end
  (interactive)
  (let ((save-match-beginning) (save-match-end))
    (if (save-excursion 
	  (re-search-forward comment-start-skip
			     (save-excursion (end-of-line) (point)) t))
	(progn
	  (setq save-match-beginning (match-beginning 0))
	  (setq save-match-end (match-end 0))
	  (if (kumac-is-in-string-p (match-beginning 0))
	      (progn
		(save-excursion
		  (goto-char save-match-end)
		  (kumac-find-comment-start-skip)) ; recurse for rest of line
		)
	    (goto-char save-match-beginning)
	    (re-search-forward comment-start-skip
			       (save-excursion (end-of-line) (point)) t)
	    (goto-char (match-end 0))
	    t))
      nil)))

(defun kumac-is-in-continuation-line-p ()
  "Returns t is POS (a buffer position) is in a continuation line"
  (interactive)
  (let (not-first-statement 'nil)
    (save-excursion
    (beginning-of-line)
    (while (and (setq not-first-statement (= (forward-line -1) 0))
		(or (looking-at comment-line-start-skip)
		    (looking-at label-start-skip)
		    (looking-at "[ \t]*$")
		    (looking-at (concat "[ \t]*"  comment-start-skip)))))
    (if (not not-first-statement)
	(progn 
;	  (message "Not continuation line")
;	  (sit-for 1)
	  'nil)
      (if (looking-at ".*_[ \t]*$") 
	  (progn
;	    (message "Continuation line")
;	    (sit-for 1)
	    't)
	(progn
;	  (message "Not continuation line")
;	  (sit-for 1)
	  'nil))))))

(defun kumac-is-in-string-p (pos)
  "Return t if POS (a buffer position) is inside a standard Kumac string.
Kumac strings are delimeted by apostrophes (\').  Quote-Escape-sequences
(\\'), strings delimited by \" and detection of syntax-errors
(unbalanced quotes) are NOT supported."
;;; The algorithm is simple: start at point with value nil
;;; and toggle value at each quote found until end of line.
;;; The quote skip is hard-coded, maybe it's possible to change this
;;; and use something like 'string-constant-delimiter' (which
;;; doesn't exist yet) so this function can be used by other modes,
;;; but then one must pay attention to escape sequences, multi-line-constants
;;; and such things.
  (let ((is-in-kumac-string nil))
    (save-excursion   
      (goto-char pos)
      (if (not (kumac-previous-statement))
	  (kumac-next-statement))
      (while (< (point) pos)
	;; Make sure we don't count quotes in continuation column.
	(if (looking-at "^     ")
	    (goto-char (+ 1 (match-end 0)))
	  (if (and (not is-in-kumac-string)
		   (looking-at comment-start-skip))
	      (beginning-of-line 2)
	    (if (looking-at "'")
		(setq is-in-kumac-string (not is-in-kumac-string)))
	    (forward-char 1)))))
    is-in-kumac-string))

(or (assq 'kumac-tab-mode-string minor-mode-alist)
    (setq minor-mode-alist (cons
			    '(kumac-tab-mode-string
			      (indent-tabs-mode kumac-tab-mode-string))
			    minor-mode-alist)))

(defun kumac-insert-comis ()
  (interactive)
  (beginning-of-line)
  (insert "application COMIS quit")
  (kumac-indent-line)
  (end-of-line)
  (insert "\n")
  (insert "\n")
  (insert "quit")
  (kumac-indent-line)
  (forward-line -1) 
  (kumac-indent-line))


(provide 'kumac-mode)

;;; kumac-mode.el ends here

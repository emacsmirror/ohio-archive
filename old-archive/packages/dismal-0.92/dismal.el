;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              
;;;; File            : dismal.el
;;;; Authors         : David Fox, fox@cs.nyu.edu 
;;;;                   and Frank E. Ritter, ritter@cs.cmu.edu
;;;; Created On      : 31 Oct 1991.
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Sat Sep 25 16:02:30 1993
;;;; Update Count    : 835
;;;; 
;;;; PURPOSE
;;;;     DISMAL - Dis Mode Ain't Lotus.
;;;; 	Spreadsheet program for gnu-emacs.
;;;;
;;;; TABLE OF CONTENTS
;;;;	i.	Disclaimer
;;;;	ii.	Overview of how dismal-mode works
;;;;	iii.	What you must do to start up
;;;;	iv.	HISTORY
;;;; 	v.	Global user visible variables
;;;; 	vi.	Requires and loads and autoloads
;;;;	vii.	Data structures
;;;; 	viii.	Mandatory variables - must be set in/by the control file
;;;; 	ix.	System constants
;;;;	x.	Internal variables
;;;;	xi.	Preliminary macro(s)
;;;;	xii.	Known bugs
;;;;
;;;; 	I.	dismal-mode and startup code
;;;; 	II.	Other mode helpers & macros
;;;;	IIa.	Ruler code
;;;;	III.	Set up the keymaps
;;;;	IV.	Dismal versions of commands
;;;; 	V.	dismal-mark
;;;; 	VI.	Range and range-buffer functions
;;;;	VII.	Cell access and setting functions
;;;; 	VIII.	Changed movement functions
;;;;	IX.	Cell editing
;;;;	X.	Cell re-evaluation
;;;;	XI.	Cell evaluation
;;;;	XIIa.	Insertion - of rows and columns.
;;;;	XIIb.	Deletion - of rows, columns & ranges
;;;;	XIIc.	Insertion and Deletion - Cell reference updating
;;;;	XIII.	Cell dependencies
;;;;	XIVa.	File I/O - Reading and writing
;;;;    XIVb.	File I/O - Translation functions between Excel and Forms
;;;;	XIVc.	File I/O - Report functions
;;;;	XV.	Redrawing the screen
;;;;	XVI.	Cell formatting
;;;;	XVII.	Cell expression conversions
;;;;	XVIII.	Column formating commands
;;;;	XIXa.	Utility functions - Date functions
;;;;	XIXb.	Utility functions - List functions
;;;;	XIXc.	Utility functions - Math functions
;;;;	XIXd.	Utility functions - Misc
;;;;		(Debugging functions)
;;;;	N.	Final code
;;;;	N+1.	History 
;;;;
;;;; Copyright 1993, David Fox & Frank Ritter.
;;;; Bug testing (incidental) and some fixes by bob@gnu.ai.mit.edu 
;;;; and altmann@cs.cmu.edu b
;;;; 
;;;; Formated in a modified Milnes style, based on
;;;; Oman & Cook, Typographic style is more than cosmetic, CACM, 33, 506-520. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fixed other utils 15k float, 18k popper
;;;  63 k 19-Dec-91 -DF
;;; 104 k 11-Jan-92 -FER
;;; 135 k 25-Jan-92 -FER
;;; 147 k 20-Feb-92 -FER  (+ 127 8 7 5)
;;; 178 k 14-Mar-92 -FER => bytecomp 116k (137K with lots of macros)
;;; 192 k 3-Apr-92 -FER
;;; 195 k 8-Apr-92 -FER V.62
;;; 242 k 18-Jul-92 -FER v.81 (+ 197 7 1 11 2  5 10 9)
;;; 247 k 26-Aug-92 -FER v.82 (+ 202 7 1 11 2  5 10 9)


;;;
;;;	i.	Disclaimer
;;;

;;; This file works with GNU-Emacs.  GNU Emacs is distributed in the hope
;;; that it will be useful, but WITHOUT ANY WARRANTY.  No author or
;;; distributor accepts responsibility to anyone for the consequences of
;;; using it or for whether it serves any particular purpose or works at
;;; all, unless he says so in writing.  Refer to the GNU Emacs General
;;; Public License for full details.
;;; 
;;; Everyone is granted permission to copy, modify and redistribute GNU Emacs, 
;;; and this file and its associated files
;;; but only under the conditions described in the GNU Emacs General Public
;;; License.  A copy of this license is supposed to have been given to you
;;; along with GNU Emacs so you can know your rights and responsibilities.  If
;;; you don't have this copy, write to the Free Software Foundation, Inc., 675
;;; Mass Ave, Cambridge, MA 02139, USA.


;;;
;;;	ii.	Overview of how dismal-mode works
;;;

;;; Date: Thu, 19 Dec 91 17:58:44 -0500
;;; From: David Fox <fox@GRAPHICS.CS.NYU.EDU>
;;; To: Frank_Ritter@SHAMO.SOAR.CS.CMU.EDU
;;; Subject: dismal results
;;; 
;;; Dismal is something I've worked on for quite a while on and off.
;;; I haven't been working on it lately, I've been hoping someone
;;; would take over for me.  I'll give you a call, or you can call me:
;;; 
;;; Office: 212-998-3389
;;; Home, weekdays: 212-874-7382
;;; Home, weekends: 908-273-3667
;;; 
;;; Frank Ritter ++ 44 (602) 436 265  (h)  ++ 44 (602) 515 292 (w)
;;; now at the U. of Nottingham, England
;;; Ritter doubled it in size and scope, and is now hoping for someone like 
;;; Fox was hoping.

;; INSTRUCTIONS FOR PRELIMINARY VERSION:  Commands are similar to sc,
;; "=" to enter a cell value, etc.   You must enclose numbers with
;; decimal points in quotes!  You can use the floating point functions
;; f+, f- etc to build expresions, and you build them lisp-style (prefix),
;; not infix.  Use dismal-find-file to create or retrieve a spreadsheet.
;; more help is available from the menu (C-c C-m), from mode
;; help (C-h m), and from the currently brief manual that comes with dismal.
;; dismal-mode.doc, available on the menu under doc.

;; Discussion:  The spreadsheet is stored in the buffer local variable
;; dismal-matrix.  This is a two dimensional array where each element
;; contains a five-tuple:
;;
;;   exp - the expression whose value is to be displayed in the cell
;;   val - the most recent result of evaluating the expression
;;   dep - a list of the addresses of the cells that use this cell's value
;;   mrk - a field used by some of the algorithms for temporary marks
;;   fmt - a function that takes the value and returns a formatted string
;;
;; The expression is a s-expression that, when eval-ed, returns the
;; current value of the expression.  The other cells of the spreadsheet
;; can be referred to in this expression using the four cell reference
;; functions:  dismal-r-c-, dismal-rfc-, dismal-r-cf, dismal-rfcf.
;; These functions are produced when the user inputs cell references
;; of the form A1, A$1, A1$, and A$1$ respectively, where the meaning
;; is that the reference is "fixed" in the dimension the dollar sign
;; follows.  Thus if you insert a new row zero, the reference A1 will
;; become A2, but A1$ remains A1$.  The four functions take row and
;; column as arguments.
;;
;; When the value field is non-nil, it is used rather than eval-ing
;; the expression.  When a cell's expression changes a function is
;; called that recursively sets the value field of all its dependents
;; to nil.  Note that that value field need have no particular type,
;; as long as the format function can convert it to a string.
;;
;; The format function takes four arguments and returns a string:
;;      (format value width decimal extra)
;; Value is the value to be formatted.  Width is the total length
;; of the returned string.  Decimal is the number of characters to
;; follow the decimal point, if this is meaningful.  Finally, extra
;; is the total width of the empty cells to this cell's right.  When
;; a left justified string is formatted, it is allowed to overlap
;; any empty cells to its right.
;; Tiny fonts let you open a window on a big display as large as 220 col
;; by 100 rows.  This is hard to read though.


;;;
;;;	iii.	What you must do to start up
;;;
;;; Load the dismal-mode-defaults.el file in your .emacs or by hand.
;;;

;; Some notes on bytecompiling
;;   [.elC is with Zawinski bytecompiler]
;; 3:07 to enter 31x1071 dismal spreadsheet with .el
;; 3:00 to enter 31x1071 dismal spreadsheet with .elc
;; 3:00 to enter 31x1071 dismal spreadsheet with .elC
;; 3:00 to enter 31x1071 dismal spreadsheet with .elC w/ some inline
;; 2:33 to enter 31x1071 dismal spreadsheet with .elC w/ lots inline
;; 2:21 to enter 31x1071 dismal spreadsheet with .elC w/ lots^2 inline
;; 1:54 to enter 31x1071 dismal spreadsheet with .elC w/ lots^3 inline
;; 2:54 to enter 31x1071 dismal spreadsheet " " " w/ integer numbers


;;;
;;;	iv.	HISTORY
;;;
;;; Deprecated, see end of file.


;;;
;;; 	v.	Global user visible variables
;;;

(defvar dismal-use-popper t
  "*Use the popper package for temporary results.")

(defvar dismal-show-selected-ranges t
  "*Show the user the selected range when cutting or erasing.")

(defvar dismal-recursion-limit 9
  "*Maximum depth allowed when evaluating (perhaps) circular dependencies.")

(defvar dismal-iteration-limit 3 ;; 9 might be good in a released version
  "*Maximum number of iterations of update cycles.")

(defvar dismal-load-hook nil
  "*Hook variable run after dismal-mode is loaded.")

(defvar dismal-hooks nil
  "*Hook functions to be run upon entering dismal-mode.")

(defvar dismal-query-on-entry-p nil
  "*Ask for confirmation each time dismal-mode is called.  Normally
unimportant for normal users, who should have this set to nil.  In released
versions this should be set to nil.")

(defvar dismal-default-column-width 10
  "*Default width for columns.")
(make-variable-buffer-local 'dismal-default-column-width)

(defvar dismal-default-column-alignment 'default
 "*Default way to align a cell.")
(make-variable-buffer-local 'dismal-default-column-alignment)

(defvar dismal-default-column-decimal 2
 "*Default number of decimal digits for a cell.")
(make-variable-buffer-local 'dismal-default-column-decimal)

(defvar dismal-page-size 64
  "*Anticipated page size for printing.")
(make-variable-buffer-local 'dismal-default-column-decimal)

(defvar dismal-field-sep "\t"
  "*Default field separator character when working with other system
dump files (default TAB).")

(defvar dismal-auto-save-interval 1000
  "*Number of dismal movements between auto-saves.
Zero means disable autosaving.")
;; counts number of visit-cells

(defvar dismal-ruler-row -2
  "*The row to use to make a ruler on the top.")
(make-variable-buffer-local 'dismal-ruler-row)

(defvar dismal-show-ruler t
  "*If t (the default) show the ruler at the top.")
(make-variable-buffer-local 'dismal-show-ruler)

(defvar dismal-auto-update t
  "*If t (the default) automaticaly call update after a cell changes.
Setting this to nill can save significant amounts of time on large sheets.")
(make-variable-buffer-local 'dismal-auto-update)

;; We use enscript cause it works well here at CMU.   You will have to use
;; a local command probably, although enscript appears to standard unix...
;; -r is rotate                ;; -L is page length in lines
;; -G is gaudy                 ;; -c truncates long lines
;; -f is font                  ;; -pfile will print to a file

;; Common to a user, not a buffer
(defvar dismal-raw-print-command
        "enscript -r -c -G -fCourier7 -L%d "
  "*Format statement to make the local print command.  
Can take an argument of max display-width.")

(defvar dismal-print-command (format dismal-raw-print-command
                                     (+ 2 dismal-page-size))
  "Command used to print a file on local system.  Created from dismal-raw-print-command.")

(defvar dismal-middle-col nil
  "*The last col (coming from the left) that is grouped with the left hand side
columns when alighning.")
(make-variable-buffer-local 'dismal-middle-col)

(defvar dismal-normal-max-column-width 20
 "*The normal maximum column width.  Widths larger than this must be 
confirmed on entering.")

(defvar dismal-copy-to-dismal-binding "\C-c\M-c"
  "*Key to globally bind to copy-to-dismal.")

(defvar dis-codes-file (concat dismal-directory "/example-codes.txt")
  "*Default file to get codes from.")


;;;
;;;	vi.	Requires and loads
;;;

;; The Emacs Common-lisp look-alike package
;; Every site should have this.  Email us if you don't.
;; Put here because it doesn't fit in the make otherwise.
(require 'cl)

;; so popper won't clobber ^Z
(if (or (not (boundp 'dismal-use-popper)) dismal-use-popper)
    (setq popper-load-hook 
         (function (lambda ()
           ;; Define key bindings
           (define-key global-map "\C-c1" 'popper-bury-output)
           (define-key global-map "\C-cv" 'popper-scroll-output)
           (define-key global-map "\C-cg" 'popper-grow-output)
           (define-key global-map "\C-cb" 'popper-switch)))))
           ;; Make *Manual windows default to 10 lines

(if (or (not (boundp 'dismal-use-popper)) dismal-use-popper)
    (require 'popper))

(if (not (boundp 'exp-base))
    (load "float"))             ; float should (provide 'float)!

(require 'float-changes)

(require 'vectors)
(require 'heaps)
;; (require 'matrix)  ;; a column based matrix
(require 'rmatrix)
(require 'dismal-simple-menus)
(require 'soar-misc)
(require 'ritter-math)

(require 'dismal-mouse-x)

(require 'goto-manual)

(push dismal-directory doc-manual-homes)

;; (require 'dismal-metacolumn)
(autoload 'dismal-set-metacolumn
  "dismal-metacolumn"
  "Set the middle-column, which is used to create two meta-columns in 
the spreadsheet.")

(autoload 'dismal-insert-metacolumn-cells 
  "dismal-metacolumn"
  "Insert ARG cells in the metacolumn that COL (default, current-col) is in,
at ROW (default, current-row).")

(autoload 'dismal-insert-z-box 
  "dismal-metacolumn"
  "Insert ARG rows of cells on each side of dismal-middle-col,
starting at the rows of point and mark, which must be on opposite 
sides of the middle-col.")

(autoload 'dismal-align-metacolumns 
  "dismal-metacolumn"
  "Align the metacolumns so that point and mark are on the same line,
keeping other parts of the columns still aligned.")


;; (require 'insert-date)
;; now assume that we get our directory in load path
(autoload 'insert-time-string "insert-date"
	    "Inserts an Al-like time-stamp after point." t)
(autoload 'insert-current-time-string "insert-date"
	    "Inserts a full time-stamp after point." t)
(autoload 'insert-date-string "insert-date"
	  "Inserts the current date after point, in m-d-y format.  With prefix
argument, inserts the weekday first." t)
(global-set-key "\C-cd" 'insert-date-string)


;; can't require this (it has no provide), but we'll try
(autoload 'delete-extract-rectangle "rect")

(autoload 'dis-model-match "dismal-extensions"
  "Given a cell RANGE-LIST computes the percentage of colA matched
with something in colA-1.  Only counts stuff that is in order." t)

(autoload 'dis-model-match-op "dismal-extensions"
  "Given a cell RANGE-LIST computes the percentage of colA matched
with something in colA-2, and col A is an operator.  Only counts stuff
that is in order." t)

(autoload 'dis-auto-align-model  "auto-aligner"
  "Automatically align the two metacolumns in
the spreadsheet." t)

(autoload 'dis-model-match-op    "dismal-model-extensions"
   "Given a cell RANGE-LIST computes the percentage of colA matched
with something in colA-2, and col A is an operator.  Only counts stuff
that is in order." t)

(autoload 'dis-model-match "dismal-model-extensions"
  "Given a cell RANGE-LIST computes the percentage of colA matched
with something in colA-1.  Only counts stuff that is in order." t)

(autoload 'dis-initialize-operator-codes "semi-coder"
  "Initialize the dismal operator codes." t)

(autoload 'dis-load-op-codes "semi-coder"
 "Load operator codes into dismal.  UNION-OR-REPLACE can be either." t)

(autoload 'dis-op-code-segment "semi-coder"
  "Code a segment with an operator name." t)

;; this here could be modified to use 19's modified zawinski compiler
(if (fboundp 'proclaim-inline)
  (proclaim-inline
    ;; dismal-address-compare  ;; makes me nervous
    dis-addressp
    dis-cellp
    dismal-adjust-range-list
    dismal-cell-name
    ;; dismal-end-of-col-non-interactive ;interactive
    dismal-char-col-to-dismal-col
    dismal-cleanup-long-string
    dismal-column-alignment
    dismal-column-decimal
    dismal-column-width
    dismal-convert-number-to-colname
    dismal-del
    dismal-draw-column-label
    dismal-draw-row-label
    dismal-evaluate-cell
    dismal-evaluate-cellref
    dismal-execute-delayed-commands
    dismal-flat-format
    dismal-get-cell
    dismal-get-cell-alignment
    dismal-get-cell-exp 
    dismal-get-cell-val 
    dismal-get-cell-dep 
    dismal-get-cell-mrk 
    dismal-get-cell-fmt
    dismal-get-or-make-cell
    dismal-get-create-column-format
    dismal-get-column-format
    dismal-get-deps
    dismal-get-exp
    dismal-get-fmt
    dismal-get-mrk
    dismal-get-val
    dismal-goto-cell
    dismal-insert-blank-box
    dismal-insert-blank-col
    dismal-insert-blank-range
    dismal-insert-report-rulers
    dismal-invalidate-cell
    dismal-isearch-guts
    dismal-isearch-queryer
    dismal-jump-to-cell
    dismal-jump-to-cell-quietly
    dismal-make-print-file-name
    dismal-note-selected-range
    dismal-read-cell
    dismal-read-row
    dismal-redraw-cell
    dismal-redraw-row
    dismal-remove-row-label
    dismal-report-header
    dismal-query-replace-guts
    dismal-set-cell
    dismal-set-cell-exp 
    dismal-set-cell-val 
    dismal-set-cell-dep 
    dismal-set-cell-mrk 
    dismal-set-cell-fmt 
    dismal-set-deps
    dismal-set-exp
    dismal-set-fmt
    dismal-set-mark
    dismal-set-mrk
    dismal-set-val
    ; dismal-show-selected-range ; basically interactive
    dismal-string-to-range
    dismal-sum-column-widths
    dismal-visit-cell
    rangep
  ))


;;;
;;;	vii.	Data structures
;;;

;; Column format macros:

(defmacro col-format-width (f) (list 'aref f 0))
(defmacro col-format-decimal (f) (list 'aref f 1))
(defmacro col-format-alignment (f) (list 'aref f 2))

(defmacro set-col-format-width (f val) (list 'aset f 0 val))
(defmacro set-col-format-decimal (f val) (list 'aset f 1 val))
(defmacro set-col-format-alignment (f val) (list 'aset f 2 val))

;; Address accessor functions:  these are cons of row and col numbers
(defmacro dis-make-address (r c) (list 'cons r c))
(defmacro dis-address-row  (address) (list 'car address))
(defmacro dis-address-col  (address) (list 'cdr address))

(defun dis-addressp (arg)
   (and (consp arg)
        (numberp (car arg))
        (numberp (cdr arg))))

;; Cells: these can be changed by insertion other cells
;; Cell accessor functions:  these are cells that can be relative addresses
(defconst dismal-cell-types '(dismal-r-c- dismal-rfc- dismal-r-cf dismal-rfcf))

(defmacro dis-cell-row (cell) (list 'nth 1 cell))
(defmacro dis-cell-col (cell) (list 'nth 2 cell))

(defun dis-cellp (arg)
   (and (listp arg) 
        (= (length arg) 3)
        ;; could add tests here for valus of cell
        (memq (car arg) dismal-cell-types)))

;; Range accessor functions:
;; (setq a (make-range 2 3 4 5))
(defun make-range (start-row start-col end-row end-col)
   (setq end-row (min end-row dismal-max-row))
   (setq end-col (min end-col dismal-max-col))
   (` (dismal-range (dismal-r-c- (, start-row) (, start-col))
                    (dismal-r-c- (, end-row)
                                 (, end-col)))))

(defmacro range-1st-cell (range)  (list 'nth 1 range))
(defmacro range-2nd-cell (range)  (list 'nth 2 range))
(defmacro range-1st-row (range)   (list 'cadr (list 'range-1st-cell range)))
(defmacro range-1st-col (range)   (list 'caddr (list 'range-1st-cell range)))
(defmacro range-2nd-row (range)   (list 'cadr (list 'range-2nd-cell range)))
(defmacro range-2nd-col (range)   (list 'caddr (list 'range-2nd-cell range)))

(setq dismal-range 'dismal-range)

(defun rangep (arg)
  (and (listp arg)
       (eq (car arg) 'dismal-range)
       (= (length arg) 3)))

;; Range-buffer accessor functions:
(defmacro range-buffer-length (range-buffer) (list 'aref range-buffer 0))
(defmacro range-buffer-width (range-buffer) (list 'aref range-buffer 1))
(defmacro range-buffer-matrix (range-buffer) (list 'aref range-buffer 2))
(defmacro range-buffer-set-rows (rb rows) (list 'aset rb 0 rows))
(defmacro range-buffer-set-cols (rb cols) (list 'aset rb 1 cols))


;;;
;;; 	viii.	Mandatory variables - must be set in/by the control file
;;;

;; Variables that will be written out on save.
(defvar dismal-saved-variables
      '(dismal-auto-update
        dismal-default-column-format
        dismal-column-formats
        dismal-formula-cells
        dismal-max-row
        dismal-max-col
        dismal-middle-col
        dismal-page-size
        dismal-ruler-row
        dismal-show-ruler
        dismal-matrix))

;;
;; BUFFER-LOCAL VARIABLES: Variables saved in spreadsheet file.
;;

(defvar dismal-matrix nil
  "The elements of this matrix represents cells.  Each element is
a list with up to four elements:
        1. The expression (exp) associated with each cell.
        2. The result of the most recent evaluation (val) of this cell.
        3. A list of addresses (row-column pairs) of cells that depend
           on the value of this cell (dep).
        4. The current depth of recursive evaluations of this cell.
           This is saved in the data file for simplicity even though
           it is temporary data (mrk?)
        5. Format, specific format information pertaining to that cell only.")
(make-variable-buffer-local 'dismal-matrix)

(defvar dismal-default-column-format
  [dismal-default-column-width 2 default]
  "Columns corresponding to nil elements or elements beyond the end of
dismal-column-formats are considered to have this format.  An array that is
width, decimals shown, and justification (default, left, right, center).")

(aset dismal-default-column-format 0 dismal-default-column-width)
(aset dismal-default-column-format 1 dismal-default-column-decimal)
(aset dismal-default-column-format 2 dismal-default-column-alignment)
(make-variable-buffer-local 'dismal-default-column-format)

(defvar dismal-column-formats (vector-create nil)
  "A vector of the formats of the columns.  Each element is a list of
two numbers, the column width and the number of characters after the
decimal point.")
(make-variable-buffer-local 'dismal-column-formats)

(defvar dismal-formula-cells (vector-create nil)
  "A vector of the cells with real expressions in them.  Each element is a cons of
the row and column numbers.")
(make-variable-buffer-local 'dismal-formula-cells)

(defvar dismal-max-row 0
  "Number of the bottom-most row that contains a value.")
;; number of rows is 1+ this number
(make-variable-buffer-local 'dismal-max-row)

(defvar dismal-max-col 0
  "Equal to (1- (length dismal-expressions)), but used to help draw new 
column labels.")
;; number of rows is 1+ this number
(make-variable-buffer-local 'dismal-max-col)

(defvar dismal-middle-col-name nil
  "The name of the middle column, such as 'A' or 'L'.")
(make-variable-buffer-local 'dismal-middle-col-name)

(defvar dismal-buffer-auto-save-file-name nil)
(make-variable-buffer-local 'dismal-buffer-auto-save-file-name)

(defvar dismal-auto-save-counter 0)
(make-variable-buffer-local 'dismal-auto-save-counter)

(defvar dismal-delayed-commands nil)
(make-variable-buffer-local 'dismal-delayed-commands)

(defvar dismal-interactive-p t)
;; Set to nil when doing secret commands, that we don't want side effects from.
;; Not buffer local, if doing a command, must be accessable in all buffers.


;;;
;;; 	ix.	System Constants
;;;

(defconst dismal-version "0.92"
  "Version of dismal-mode implementation.")

(defconst dismal-cell-name-regexp "^\
\\([A-Z][A-Z]*\\|[a-z][a-z]*\\)\
\\(\\$\\|\\)\
\\([0-9][0-9]*\\)\
\\(\\$\\|\\)\
$"
  "Matches cell names, such as 'A1', 'b$2', 'C$3$', etc.")

(defconst dismal-cell-range-regexp "^\
\\([A-Z][A-Z]*\\|[a-z][a-z]*\\)\
\\(\\$\\|\\)\
\\([0-9][0-9]*\\)\
\\(\\$\\|\\)\
:\
\\([A-Z][A-Z]*\\|[a-z][a-z]*\\)\
\\(\\$\\|\\)\
\\([0-9][0-9]*\\)\
\\(\\$\\|\\)\
$"
  "Matches two cell names separated by a colon.")


;;;
;;; 	x.	Internal variables
;;;

(defvar dismal-ctl-x-map nil)
(defvar dismal-ctl-c-map nil)
(defvar dismal-delete-prefix-map nil)
(defvar dismal-insert-prefix-map nil)
(defvar dis-user-cell-functions nil "Functions the user can put in a cell.")

(defvar dismal-mode-line-format
 '("" mode-line-modified mode-line-buffer-identification "   " 
   global-mode-string
   " " dismal-current-cell " "
   (dismal-auto-update "AutoUp" "ManUp ")
   ;; doing this right might be hard, so leave it alone.
   " <"
   (dismal-middle-col dismal-middle-col-name)
   "]"
   "  %[(" mode-name minor-mode-alist "%n"
   mode-line-process ")%]----" (-3 . "%p") "-%-"))

(defvar dismal-map nil "")

(defvar dismal-row-label-lined nil
  "If t (default nil), put a | up at the end of row number.")
;; not kept around separately in all buffers
;; See Tufte, and Short papers @ chi '92 for why this is ok.

(defvar dismal-row-label-format
   (concat "%6d "
           (if dismal-row-label-lined "|" "")))
(make-variable-buffer-local 'dismal-row-label-format)

(defvar dismal-current-first-ruler-row nil
  "Where the ruler is currently displayed.")
(make-variable-buffer-local 'dismal-ruler-row)

(defvar dismal-ruler "") ; the actual string put up
(make-variable-buffer-local 'dismal-ruler)

(defvar dismal-buffer-using-minibuffer nil
  "Set when a buffer is doing a read.")
;; don't make local, so that minibuffer and current buffer can get back to it.
;; only bound with a let anyhow, so can't be clobbered...

(defconst dismal-blank-bag '(?\ ?\t))  ;took out ?:

(defvar dismal-first-data-line 3
  "Argument to goto-line that takes you to row 0.")
(make-variable-buffer-local 'dismal-default-column-width)

(defvar dismal-first-printed-column 8
  "Argument to move-to-column that takes you to the beginning of column A.")
(make-variable-buffer-local 'dismal-first-printed-column)

(defvar dismal-number-p 'floatp)
(defvar dismal-number-to-string 'float-to-string)
(defvar dismal-string-to-number 'string-to-float)

(defvar dismal-invalid-heap nil
  "Heap to hold addresses of cells that need re-evaluation.")
(make-variable-buffer-local 'dismal-invalid-heap)

(defvar dismal-invalid-heap-not nil
  "Backup heap to hold addresses of cells that need re-evaluation.")
(make-variable-buffer-local 'dismal-invalid-heap-not)

;; These two are used to let you note changes in a cycle.
;; dismal-invalid-heap is set to one of them
(defvar dismal-invalid-heapA nil
  "Heap to hold addresses of cells that need re-evaluation.")
(make-variable-buffer-local 'dismal-invalid-heapB)

(defvar dismal-invalid-heapB nil
  "Heap to hold addresses of cells that need re-evaluation.")
(make-variable-buffer-local 'dismal-invalid-heapA)

;;
;; BUFFER-LOCAL VARIABLES: Transient.
;;

(defvar dismal-setup nil
  "T if dismal-mode has been called on the file/buffer.") 
(make-variable-buffer-local 'dismal-setup)

(defvar dismal-current-col 0
  "Current column number, where first column (labelled `A') is number 0.")
(make-variable-buffer-local 'dismal-current-col)

(defvar dismal-current-row 0
  "Current row number.")
(make-variable-buffer-local 'dismal-current-row)

(defvar dismal-cell-buffer nil
  "The range expression copied by range selection commands is saved here,
including its address of origin.")
;; not local so it can be shared
;(make-variable-buffer-local 'dismal-cell-buffer)

(defvar dismal-range-buffer nil
  "The cells copied by range command for range pasting are saved here, 
along with its size.  Format: [rows-used cols-used matrix].")
;; By not making this local, we can cut and paste between sheets...
;;(make-variable-buffer-local 'dismal-range-buffer)

;; There must be a faster, cheaper way to do this, but I don't see it 
;; tonight: this sucker will get reset repeatedly.
(defvar dismal-current-cell "A0"
  "String indicating current cell.")
(make-variable-buffer-local 'dismal-current-cell)


;;;
;;;	xi.	Preliminary macro(s)
;;;

(defmacro my-message (&rest body)
 (` (and (boundp 'my-debug) my-debug
         (progn (message  (,@ body))
                (sit-for 2)))))

(defmacro mapc (function alist)
 (` (let ((blist (, alist)))
     (while blist
      (funcall (, function) (car blist))
      (setq blist (cdr blist))    ))))


;;;
;;;	xii.	Known bugs
;;;

;; CAUTION: I haven't made it safe from looping on circular definitions
;; yet.

;; For David:  
;; 17.  Implement evaluation loop counting [you should take a whack at this,
;;     it's outside my interests and talents (reletively)].
;;     [On DF's list]
;; * On updates, and adding, deleting rows/cols, we're are losing time
;;   big in dismal-erase-all-dependencies and dismal-record-all-dependencies.
;;   I've gotten the redraw routines partially beat, can you look at these?
;; Compatibility Notes: 
;; * column-format's are now arrays of 3, incompatible, but fixable in existing
;;   files by replacing "(" with "[", old files with (Num Num)'s in 
;;   dismal-column-formats should be replaced with [Num Num nil], and 
;;    within the dismal-matrix itself
;;    (query-replace "ff+" "dis-sum")
;;    (query-replace "ff*" "dis-product")
;;    (replace-string "dismal-format-string-left" "left")
;;    (replace-string "dismal-format-string-right" "right")
;;    (replace-string "dismal-format-number" "default")
;;    (replace-string "dismal-format-center" "center")
;;    (replace-string "dismal-current-column" "dismal-current-col")
;;    (replace-string "dismal-maximum-column" "dismal-max-col")
;;    (replace-string "dismal-maximum-row" "dismal-max-row")


;; Known bugs II  (some of these are spa-mode bugs included here for now)
;; To Do:
;; 2 (20) chaning a null cell to a values does not update formula depending on it
;; 1 (15) yank does not update cells that rely on the yanked into cell
;; 2 (25) Allow the row labels to be hid
;; 3 (20) make popper a user preference
;; 2 (30) takes 10 seconds to load.  Cut this in half with autoloads 
;;         (or single sourcing?)
;; 2 (?) bigger numbers, see number-size-bug.txt
;; 2 (?) tie to calc
;; 3 (20) redraw-visible area C-c C-m L
;; 3 (30) on delete col, offer to save contents
;; 2 (30) on paste cells, cleanup long strings
;; 2 (10) tidy n,p,f,b commands
;; 2 (15) when inserting a row, update the ruler row
;;        in general, do timing of routines
;;        load write5.dis, 8:18, unit, 8:43
;; 3 (15) on insert col, adjust dismal-middle-col
;; 2 (50) Compute average delay of slots, 
;;    eg. v9 matches 32 between two segmetns that match at 61 & 62
;; also get this error with 18.54: could not sbrk, return=1
;; ? (?)  Show state changes
;; ? (?)  support coding before matching
;; 3 (25) updating formula on row additions does it twice
;; ?      How to update ranges in formula upon cell insertion/deletion
;;     If insert-range (delete-range)
;; 3 (40) Check each fun in function-list
;;    If range of insertion after range of fun, do nothing
;;    If range of insertion includes range of fun, make range bigger
;;         setting row-end to  row-end + insert
;;    If range of insertion before range of fun, move range
;;         setting row-start to  row-start + insert, and for row-end
;;    Do for columns too
;; 3 (20) dis-max on a range
;; 3 (20) When printing, check for long line of blanks
;; 3 (20) General function to set a variable 
;; 3 (20) when printing a message about the cell contents, substitute
;;        %% for % in the string.
;; 3 (50) count number and types of match, i.e., a report in cells
;; 3 (30) add tally and report to dismal and menus
;; 3 (240) typing should put stuff into a cell, what's this = thing?
;;        have to move all keys off of a-zA-Z map
;;        if cell is filled, query for editing?
;;        18-Jul-92 -Bob@gnu
;;        could we just change jump-to-cell to end with an edit?
;; 3 (40) write autoalign function to take two cols, and line up
;; 3 (45) insert-range takes 30s to insert 2 cells on 30x550.  Make 25% faster
;; 3 (30) make delete-column to be a case of delete-range
;; 3 (35) can't put (+ a3 a4) in cell, must use f+
;; 3 (20) dismal-backward-filled-column breaks searching back when wrapping?
;; 3 (20) on printout, put ruler and top cell label (a, b, etc) up
;; 3 (10) better printout name on pprint
;; 3 ()   do printing of cells better by simply drawing R to L (?!)
;; 3 (40) How to do cleanup right:
;;       - Find farthest away dirty cell in direction you are cleaning
;;       - Walk back to current cell unmarking and cleaning up
;;       - Do the other direction 
;;       - if pointer is used (but not by you) or cell has value, redraw it
;; 3 (25:68) support search, C-s, so that it leaves you in the cell you 
;;        started from and so that it matches numbers
;; 4 (30) display cell expr's instead of val's, by setting a flag bob 19-Jul-92
;; 3 (15) yank text from kill-ring, ie, from another buffer into a cell, M-C-y
;; 3 (30) Can't open read-only files very well....
;; 3 (20) On yank, redraw rows with wide cells
;; 3 (40) support query replace for numbers
;; 3 (120:35+30+35+10) fix all the little commands suggested in the keymap
;; 3 (15) if end up in first window line, should pop away from display bug
;; 4 (40)  add a calculation dialog menu, to set iterations, etc.
;; 4 (?)  Add database facilities, of sorting and searching
;; 4 (?)  Consider "clicking" on a trace line and jumping to the trace? model?
;; 4 (?)  Consider using watch 0 and adding your own trace
;; 4 (180) hide rows
;; 4 (80) we can't split buffer in two, at least not for long.  Support
;;            two windows on same buffer, with different points
;;        - with window-buffer-local variables
;;        - using two different buffers with the same data structures
;; 4 (15) check for simple circular dependencies (does cell depend on itself)
;; 4 (600) Support Undo.
;; 4 (240) support commas in numbers
;;         13-Jul-92 -Bob@gnu
;;         print out with commas (cell format type)
;;         read in with commas (all the time)
;;         dump commas

;; 
;; Done:
;; 2 (15:45) Save-some-buffers will not save files the right way, need to redefine 
;;       save-some-buffers to do .dis special, 20-Aug-93 -FER
;; 1 (?) previous-pa.dis somehow blew up from 24k to 769k, with a big null row
;;       was a basic pointer problem in how we assinged cells.  fixed now Oct-92
;; copy on left mouse button fixed. 6-Nov-92 -fer
;; shell-command fixed (for some), but using new popper.
;; * And related to this, is updating cell refernces,  If you have 
;;   the code in hand, I would be happy to cut in a fucntion that knows how
;;   to update the contents of a range when it is pasted in, and that could
;;   apply when a rectangle is inserted or deleted.  12-Jul-93 -FER/EMA?
;; 3 (15) yank does not invalidate cells


;;;
;;; 	I.	dismal-mode and startup code
;;;

(defun dismal-mode (&optional no-query)
  "A major mode for editing SpreadSheets.  Version 0.85+
A command menu is available by typing C-c C-m (C-m is also RET).
All the numerical values in the spreadsheet are floating point numbers as
implemented in the float.el package in the standard GNU emacs distribution.
Therefore you can put arbitrary emacs expressions into the cells which use
the floating point functions f+, f-, f*, f/, abs, fabs, f%, f=, f>, f>=,
f<, f<=, f/=, fmin, fmax, fzerop, float-to-string.
 Special commands:\n\\{dismal-map}\n
 Special commands:\n\\{dismal-minibuffer-map}\n"
  (interactive)
  ;;(setq aa 1)
  (if (and (not dismal-setup)
           (or (not dismal-query-on-entry-p) no-query
               (y-or-n-p (format "Put %s into dismal-mode? " (buffer-name)))))
    (progn (kill-all-local-variables)
  ;;(setq aa 2)
      (use-local-map dismal-map)
      (setq mode-name "dismal")
      (setq major-mode 'dismal-mode)
      (auto-save-mode 0)
      (setq dismal-buffer-auto-save-file-name (make-auto-save-file-name))
      (setq truncate-lines t)
      (setq mode-line-format dismal-mode-line-format)
      (setq dismal-matrix (dismal-create-matrix)) ;muste be set in all buffers
      (setq dismal-invalid-heapA (heap-create 'dismal-address-compare))
      (setq dismal-invalid-heapB (heap-create 'dismal-address-compare))
      (setq dismal-invalid-heap dismal-invalid-heapA)
      (setq dismal-invalid-heap-not dismal-invalid-heapB)
      (setq dismal-range-buffer [0 0 0])
      (aset dismal-range-buffer 2 (dismal-create-matrix))
      (setq dismal-current-row 0   dismal-current-col 0)
      (setq dismal-auto-save-counter dismal-auto-save-interval)
      (eval-current-buffer)
  ;;(setq aa 4)
      (setq dismal-first-printed-column 
            (+ (1+ (log10 (max 10 dismal-max-row))) ; numbers
               1 ; space
               (if dismal-row-label-lined 1 0)))
      (setq dismal-row-label-format
                  (format "%%%dd %s" (1+ (log10 (max 1 dismal-max-row)))
                          (if dismal-row-label-lined "|" "")))
  ;;(setq aa 5)
      (erase-buffer)
      ;; have to do this explicetly
      (switch-to-buffer (current-buffer)) 
      (let ((dismal-show-ruler nil))
        (dismal-redraw nil))
      (setq dismal-middle-col-name
            (dismal-convert-number-to-colname dismal-middle-col))
      (dismal-make-ruler)
      (set-buffer-modified-p nil)
      (run-hooks 'dismal-hooks)
      (setq dismal-setup t)
      (beep t) (beep t) )))

(defun dismal-find-file (filename)
  "Edit file FILENAME in dismal-mode.
Switch to a buffer visiting file FILENAME, creating one if none exists."
  (interactive "FFilename: ")
  (let ((buffer (get-file-buffer filename)))
    (if (and buffer (buffer-name buffer))
        (pop-to-buffer buffer)
      (setq buffer (create-file-buffer filename)) ; Create a buffer to draw in
      (set-buffer buffer)                 ; Make it current
      (dismal-mode)                       ; Remember this kills local variables
      ;; probably dead code, delete 1 july 92, 29-May-92 -FER
      ;;(setq dismal-buffer-file-name filename)     ; Save the name of the file
      (setq buffer-file-name filename)    ;probably goes when redone
      (setq dismal-current-row 0          ; Reset the cursor
            dismal-current-col 0)
      (set-buffer-modified-p nil)
      (pop-to-buffer buffer))))


;;;
;;; 	II.	Other mode helpers & macros
;;;

(defun dismal-toggle-auto-update ()
  "Toggle whether or not the spreadsheet is updated on each cell entry."
  (interactive)
  (setq dismal-auto-update (not dismal-auto-update))
  (if dismal-auto-update
      (progn (message "Updating dismal-matrix")
             (dismal-update-matrix))
      (dismal-save-excursion
        (dismal-redraw-cell dismal-current-row dismal-current-col t))))

;; Jump to ROW, COLUMN and display the contents of the cell
;; in the status line.
(defun dismal-jump-to-cell (r c)
  (dismal-visit-cell r c)
  (setq dismal-current-row r)
  (setq dismal-current-col c))

(defun dismal-jump-to-cell-quietly (r c)
  "Jump to ROW, COLUMN but don't display the contents of the cell
in the status line."
  (dismal-goto-cell r c t)
  (setq dismal-current-row r)
  (setq dismal-current-col c))

(defmacro dismal-save-excursion-quietly (&rest body)
  (` (let (  ;; (dismal-show-ruler nil)
           (old-row dismal-current-row)
           (old-col dismal-current-col)
           (old-hscroll (window-hscroll))
           (old-window (selected-window)))
       (progn (,@ body))
       (dismal-jump-to-cell-quietly
                            (if (< old-row dismal-max-row)
                                old-row
                              dismal-max-row)
                            (if (< old-col dismal-max-col)
                                old-col
                              dismal-max-col))
       (set-window-hscroll old-window old-hscroll))))

(defmacro dismal-save-excursion (&rest body)
  (` (let ( ;; (dismal-show-ruler nil) ; autoshowing ruler is too slow
           (old-row dismal-current-row)
           (old-col dismal-current-col)
           (old-hscroll (window-hscroll))
           (old-window (selected-window)))
       (progn (,@ body))
       (dismal-jump-to-cell (if (< old-row dismal-max-row)
                                old-row
                              dismal-max-row)
                            (if (< old-col dismal-max-col)
                                old-col
                              dismal-max-col))
       (set-window-hscroll old-window old-hscroll))))

;; Drawing leaves you in the right spot, but on entry
;; find-file puts you at point=0, this moves us to cell 0.0
(defun dismal-find-file-hook ()
  (if (eq major-mode 'dismal-mode)
      (progn (forward-line 2)
             (move-to-column (+ dismal-first-printed-column -1
                                (dismal-column-width 0)))
             (dismal-display-current-cell-expr 0 0))))

(if (not (member 'dismal-find-file-hook find-file-hooks))
    (push 'dismal-find-file-hook find-file-hooks))

(defun dismal-set-first-printed-column ()
 (let* ((width (log10 (max 1 dismal-max-row)))
        (old-dismal-first-printed-column dismal-first-printed-column)
        (difference nil) )
 (setq dismal-first-printed-column
       (+ (1+ width) ; numbers
           1 ; a space
           (if dismal-row-label-lined 1 0)))
 (setq difference (- dismal-first-printed-column 
                     old-dismal-first-printed-column))
 (if (not (= 0 difference))
     (dismal-save-excursion 
        (setq dismal-row-label-format
              (format "%%%dd %s" (1+ (log10 (max 1 dismal-max-row)))
                      (if dismal-row-label-lined "|" "")))
        (if (> difference 0)
            (dismal-insert-blank-box (point-min)
                                     (+ dismal-first-data-line
                                        dismal-max-row) 1 " ")
          (let ((start (point-min))
                (end (save-excursion (goto-char (point-max))
                                     (beginning-of-line)
                                     (forward-char (- difference))
                                     (point))) )
            (kill-rectangle start end)) )))))

(defconst dismal-files
         '("/dismal-simple-menus" ;; put here first for a reason
                                     ;; this puts duplicates on with reloads
           "/dismal-metacolumn"
           "/float"                  "/popper"
           "/float-changes"          "/vectors"
           "/heaps"                  "/rmatrix"
           "/dismal-mouse-x"
           "/dismal-mode-defaults"   "/utilities/soar-misc"
           "/utilities/insert-date"  "/utilities/simple-menu"
           "/utilities/goto-manual"  "/dismal"))

(defun dismal-compile-n-load-dismal ()
  "Byte compile and load all the files dismal uses."
  (interactive)
  (dismal-compile-dismal)
  (mapc (function (lambda (x) (load x)))
        (cdr dismal-files)))

(defun dismal-compile-dismal (&optional directory files)
  "Byte compile all the files dismal uses."
  (interactive)
  (if (not files) (setq files dismal-files))
  (if (not directory) (setq directory dismal-directory))
  ;; load to get macros set correctly
  (mapc (function (lambda (x)
           (message "DCD-Loading new %s" x)
           (load (concat directory x ".el"))))
        files)
  (mapc (function (lambda (x) 
          (let ((compiled-version (concat directory x ".elc"))
                (plain-version (concat directory x ".el")) )
           (if (file-newer-than-file-p compiled-version plain-version)
               ;; you are set
               (progn (message "Not compiling %s" x) (sit-for 1))
             ;; else do compile
             (byte-compile-file plain-version)))))
        files)
  (beep t)  (beep t)
  (message "Done compiling dismal."))

(defmacro dismal-eval (object)
  ;; If object has no value, print it as a string.
  (` (if (or (stringp (, object)) (listp (, object)) 
             ;; put back in ;; 13-Jul-92 -FER, so qreplace can work on numbers
             (numberp (, object)) 
             (and (symbolp (, object)) (boundp (, object))))
         (eval (, object))
       (prin1-to-string (, object)))))
         

;; FUNCTIONS USED IN DEFVAR INITIALIZERS have to be defined before they
;; appear, since they are actually invoked when the file is loaded.
;;

(defun dismal-create-matrix ()
  (let ((m (matrix-create)))
    (vector-insert m 0 1)))

(defun dismal-address-compare (addr1 addr2)
  ;; Compare function for two addresses.
  (let ((rowdiff (- (dis-address-row addr1) (dis-address-row addr2))))
    (if (= rowdiff 0)
        (- (dis-address-col addr1) (dis-address-col addr2))
      rowdiff)))




;;;
;;;	IIa.	Ruler code
;;;

(defun dismal-draw-ruler (expected-current-row)
  ;; Uses expected-current-row, b/c it is draw before current-row is updated
  (sit-for 0)  ;this sit-for appears to be necssary, which is v. weird.
  (dismal-save-excursion
  (let ((new-ruler-row (- expected-current-row (current-line-in-window)))
        (buffer-originally-clean (not (buffer-modified-p))))
  (if (and dismal-show-ruler (>= new-ruler-row -1))
      (progn
       (save-excursion
         (let ((current-line (current-line-in-window)) )
         (forward-line (- current-line))
         (delete-region (point) (save-excursion (forward-line 1)
                                                (end-of-line) (point)))
         (insert dismal-ruler))   )
       (setq dismal-current-first-ruler-row
             (- expected-current-row (current-line-in-window))) ))
  (if buffer-originally-clean (set-buffer-modified-p nil)))))

(defun dismal-undraw-ruler-rows ()  ;(dismal-undraw-ruler-rows)
  (let ((buffer-originally-clean (not (buffer-modified-p))))
  (if (numberp dismal-current-first-ruler-row)
      (if (>= dismal-current-first-ruler-row 0)
          (dismal-save-excursion
             (dismal-goto-row dismal-current-first-ruler-row nil)
             (delete-region (point) (save-excursion (end-of-line) (point)))
             (forward-line 1)
             (delete-region (point) (save-excursion (end-of-line) (point)))
           (dismal-redraw-row dismal-current-first-ruler-row nil)
           (dismal-redraw-row (1+ dismal-current-first-ruler-row) nil)
           (setq dismal-current-first-ruler-row nil))
        (if (>= dismal-current-first-ruler-row -1)
            (dismal-save-excursion
               (dismal-goto-row dismal-current-first-ruler-row nil)
               (delete-region (point) (save-excursion (end-of-line) (point)))
               (dismal-goto-row (1+ dismal-current-first-ruler-row) nil)
               (delete-region (point) (save-excursion (end-of-line) (point)))
               (dismal-redraw-row (1+ dismal-current-first-ruler-row) nil)
               (setq dismal-current-first-ruler-row nil)
               (dismal-draw-column-labels)))))
  (if buffer-originally-clean (set-buffer-modified-p nil))))

(defun dismal-make-ruler ()
  (cond (dismal-ruler-row
         (save-excursion
           (goto-line (+ dismal-first-data-line dismal-ruler-row))
           (setq dismal-ruler
                 (buffer-substring (point) (progn (end-of-line) (point))))))
        (t (setq dismal-ruler "")) )
  ;; Add the dashed line below.
  (setq dismal-ruler
        (concat dismal-ruler
         (save-excursion
           (goto-line (- dismal-first-data-line 1))
           (buffer-substring (1- (point)) (progn (end-of-line) (point)))))))

(defun dismal-increment-ruler (start-row arg)
 ;; update the location of the ruler if necessary
 (if (and dismal-ruler-row
          (<= start-row dismal-ruler-row))
     (let ((dismal-show-ruler t))
       (dismal-undraw-ruler-rows)
       (setq dismal-ruler-row (+ dismal-ruler-row arg)) 
       (sit-for 0)
       (dismal-make-ruler)
       (dismal-draw-ruler dismal-current-row))     ) )

(defun dismal-update-ruler (arg)
 "Move ruler to top of screen.  If ARG is supplied, remakes the ruler."
 (interactive "p")
 (let ((dismal-show-ruler t))
   (dismal-save-excursion
     (dismal-undraw-ruler-rows)
     (if arg (dismal-make-ruler))
     (dismal-draw-ruler dismal-current-row))))

(defun dismal-set-ruler-rows (row-to-use)
  "Set the row to use a ruler and redraws it.  Set to -2 to get letters."
  (interactive (list (dismal-read-minibuffer
                          (format "Replace ruler row <%s> with: "
                                  dismal-ruler-row)
                           t (prin1-to-string dismal-current-row))))
  (dismal-save-excursion
  (setq dismal-ruler-row row-to-use)
  ;; go grab it
  (dismal-make-ruler)
  (dismal-undraw-ruler-rows)
  (dismal-draw-ruler dismal-current-row)))

(defun dismal-set-ruler (use-ruler)
  (interactive (list (dismal-read-minibuffer "Use ruler t/nil: " t
                                   (prin1-to-string dismal-show-ruler))))
  (let ((old-ruler dismal-show-ruler))
  (cond ((and old-ruler use-ruler) nil)
        ((and (not old-ruler) (not use-ruler)) nil)
        ((and old-ruler (not use-ruler))
         (dismal-undraw-ruler-rows)
         (setq dismal-show-ruler use-ruler))
        (t (setq dismal-show-ruler use-ruler)))))


;;;
;;;	III.	Set up the keymaps
;;;

;; This is used outside of dismal, so must rebind.
(cond 
  ((not (key-binding dismal-copy-to-dismal-binding))
   (global-set-key dismal-copy-to-dismal-binding 'copy-to-dismal))
  ((eq (key-binding dismal-copy-to-dismal-binding) 'copy-to-dismal))
  (t (message 
      "Change value of variable dismal-copy-to-dismal-binding, %s already used"
      (key-description dismal-copy-to-dismal-binding))
     (sit-for 4)))

(if dismal-map
    ()
  (setq dismal-map (make-keymap))
  (suppress-keymap dismal-map)

  (setq dismal-insert-prefix-map (make-sparse-keymap))
  (setq dismal-delete-prefix-map (make-sparse-keymap))
  (setq dismal-ctl-x-map (make-sparse-keymap))
  (setq dismal-ctl-c-map (make-sparse-keymap))

  ;; could del work appropriately?

  (define-key dismal-map "\t" 'dismal-forward-column)
  (define-key dismal-map "?" 'describe-mode)
  (define-key dismal-map "<" 'dismal-read-cell-leftjust)
  (define-key dismal-map ">" 'dismal-read-cell-rightjust)
  ;; (define-key dismal-map "\"" 'dismal-read-cell-string)
  ;; (define-key dismal-map "'" 'dismal-read-cell-string)
  (define-key dismal-map "=" 'dismal-read-cell-default)
  (define-key dismal-map "|" 'dismal-read-cell-center)
  (define-key dismal-map "\ " 'dismal-forward-column)
  (define-key dismal-map "c" 'dismal-copy-range)
  (define-key dismal-map "d" dismal-delete-prefix-map)
  (define-key dismal-map "dc" 'dismal-delete-column)
  (define-key dismal-map "dd" 'dismal-delete-range)
  (define-key dismal-map "dr" 'dismal-delete-row)
  (define-key dismal-map "d " 'dismal-delete-blank-rows)
  (define-key dismal-map "e" 'dismal-read-cell-plain)
  (define-key dismal-map "f" 'dismal-read-column-format)
  (define-key dismal-map "i" dismal-insert-prefix-map)
  (define-key dismal-map "ic" 'dismal-insert-column)
  (define-key dismal-map "ii" 'dismal-insert-range)
  (define-key dismal-map "iz" 'dismal-insert-z-box)
  (define-key dismal-map "i." 'dismal-insert-cells)
  (define-key dismal-map "ir" 'dismal-insert-row)
  (define-key dismal-map "j" 'dismal-jump)
  (define-key dismal-map "m" 'dismal-set-mark-command)
  (define-key dismal-map "n" 'dismal-next-filled-row-cell)
  (define-key dismal-map "p" 'dismal-previous-filled-row-cell)
  (define-key dismal-map "r" 'dismal-hard-redraw-row)
  (define-key dismal-map "v" 'dismal-paste-range)
  (define-key dismal-map "x" 'dismal-kill-range)
  (define-key dismal-map "z" 'dismal-redraw-range)

  ;; C-j newline-and-indent should goto work better
  ;; C-o should work appropriately
  ;; C-r should goback to search
  ;; C-z should work appropriately
  ;; C-x i should be rebound
  ;; C-x [ and C- ] (paging) should work appropriately
  ;; C-x > C-x < scroll right & left

  (define-key dismal-map "\C-?" 'dismal-backward-kill-cell) ;del
  (define-key dismal-map "\C-@" 'dismal-set-mark-command)
  (define-key dismal-map "\C-a" 'dismal-first-column)
  (define-key dismal-map "\C-b" 'dismal-backward-column)
  (define-key dismal-map "\C-c" dismal-ctl-c-map)
  (define-key dismal-map "\C-c\C-m" 'dismal-run-menu)
  (define-key dismal-map "\C-d" 'dismal-delete-cell)
  (define-key dismal-map "\C-e" 'dismal-end-of-row)
  (define-key dismal-map "\C-f" 'dismal-forward-column)
  (define-key dismal-map "\C-k" 'dismal-kill-line)
  ;; this appears to be too slow, leave as plain recenter
  ;(define-key dismal-map "\C-l" 'dismal-recenter)
  (define-key dismal-map "\C-m" 'dismal-forward-row)
  (define-key dismal-map "\C-n" 'dismal-forward-row)
  (define-key dismal-map "\C-o" 'dismal-open-line)
  (define-key dismal-map "\C-p" 'dismal-backward-row)
  (define-key dismal-map "\C-r" 'dismal-isearch-backwards)
  (define-key dismal-map "\C-s" 'dismal-isearch)
  (define-key dismal-map "\C-t" 'dismal-no-op)
  (define-key dismal-map "\C-q" 'dismal-quoted-insert)
  (define-key dismal-map "\C-w" 'dismal-kill-range)
  (define-key dismal-map "\C-v" 'dismal-scroll-up-in-place)
  (define-key dismal-map "\C-x" dismal-ctl-x-map)
  (define-key dismal-map "\C-xi" 'dismal-insert-file)
  (define-key dismal-map "\C-x\C-i" 'dismal-insert-file)
  (define-key dismal-map "\C-xr" 'dismal-update-ruler)
  ;; (define-key dismal-map "\C-xs" 'dismal-save-some-buffers)
  (define-key dismal-map "\C-x\C-s" 'dismal-save-file)
  (define-key dismal-map "\C-x\C-w" 'dismal-write-file)
  (define-key dismal-map "\C-x\C-x" 'dismal-exchange-point-and-mark)
  (define-key dismal-map "\C-x[" 'dismal-start-of-col)
  (define-key dismal-map "\C-x]" 'dismal-end-of-col)
  (define-key dismal-map "\C-x>" 'dismal-no-op) ; set-fill-prefix
  (define-key dismal-map "\C-y" 'dismal-paste-range)

  (define-key dismal-map "\C-c\M-\C-c" 'dis-op-code-segment)
  ;; M-a should work appropriately
  ;; M-m back-to-indentation should work appropriately
  ;; M-r replace-string should work appropriately
  ;; M-y yank-pop should work appropriately
  ;; M-z down-one-line 
  ;; M-del should work appropriately?

  (define-key dismal-map "\M-\C-?" 'dismal-backward-kill-cell) ;del
  (define-key dismal-map "\M-\ " 'dismal-backward-column)
  (define-key dismal-map "\M-<" 'dismal-beginning-of-buffer)
  (define-key dismal-map "\M->" 'dismal-end-of-buffer)
  (define-key dismal-map "\M-[" 'dismal-no-op)
  (define-key dismal-map "\M-]" 'dismal-no-op)
  (define-key dismal-map "\M-\t" 'dismal-backward-column)
  (define-key dismal-map "\M-a" 'dismal-no-op)
  (define-key dismal-map "\M-b" 'dismal-backward-filled-column)
  (define-key dismal-map "\M-c" 'dismal-capitalize-cell)
  (define-key dismal-map "\M-d" 'dismal-kill-cell)
  (define-key dismal-map "\M-e" 'dismal-last-column)
  (define-key dismal-map "\M-f" 'dismal-forward-filled-column)
  (define-key dismal-map "\M-g" 'dismal-no-op) ; fill-region
  (define-key dismal-map "\M-h" 'dismal-no-op) ; mark-paragraph
  (define-key dismal-map "\M-i" 'dismal-no-op) ; tab-to-tab-stop
  (define-key dismal-map "\M-j" 'dismal-align-metacolumns) ; fill-paragraph
  (define-key dismal-map "\M-k" 'dismal-no-op) ; kill-sent
  (define-key dismal-map "\M-l" 'dismal-downcase-cell)
  (define-key dismal-map "\M-n" 'dismal-next-filled-row-cell)
  (define-key dismal-map "\M-o" 'dismal-insert-range)
  (define-key dismal-map "\M-p" 'dismal-previous-filled-row-cell)
  (define-key dismal-map "\M-q" 'dismal-query-replace)
                                      ;; used to be replace-string
  (define-key dismal-map "\M-r" 'dismal-move-to-window-line)
  (define-key dismal-map "\M-t" 'dismal-no-op) ; used 2 be transpose-words
  (define-key dismal-map "\M-u" 'dismal-upcase-cell)
  (define-key dismal-map "\M-v" 'dismal-scroll-down-in-place)
  (define-key dismal-map "\M-w" 'dismal-copy-range)
  (define-key dismal-map "\M-=" 'dismal-debug-cell)
  (define-key dismal-map "\M-%" 'dismal-query-replace)
  (define-key dismal-map "\M-," 'dismal-no-op)

  ;; C-M-b, f, a, & e should work appropriately
  (define-key dismal-map "\M-\C-k" 'dismal-no-op) ;kill-sexp
  (define-key dismal-map "\M-\C-e" 'dismal-erase-range)
  (define-key dismal-map "\M-\C-m" 'dismal-backward-row)
  (define-key dismal-map "\M-\C-r" 'dismal-redraw)
  (define-key dismal-map "\M-\C-t" 'dismal-transpose-cells) ; used to be transpose-sexps
  (define-key dismal-map "\M-\C-u" 'dismal-update-matrix) ; used to be backward-up-list
  )

(defvar dismal-minibuffer-local-map nil "")
(if dismal-minibuffer-local-map
    ()
  (setq dismal-minibuffer-local-map (copy-keymap minibuffer-local-map))
  (define-key dismal-minibuffer-local-map "\C-j" 'dismal-exit-minibuffer-down)
  (define-key dismal-minibuffer-local-map "\C-m" 'exit-minibuffer)
  (define-key dismal-minibuffer-local-map "\C-n" 'dismal-exit-minibuffer-down)
  (define-key dismal-minibuffer-local-map "\C-p" 'dismal-exit-minibuffer-up)
  ;; (define-key dismal-minibuffer-local-map "\M-\C-b" 'dismal-exit-minibuffer-left)
  ;; (define-key dismal-minibuffer-local-map "\M-\C-f" 'dismal-exit-minibuffer-right)
  )

;; Support undo in the minibuffer since we use it so much
(buffer-enable-undo (window-buffer (minibuffer-window)))

(defun dismal-exit-minibuffer-down ()
  (interactive)
  (save-excursion (set-buffer dismal-buffer-using-minibuffer)
     (push '(dismal-forward-row 1) dismal-delayed-commands))
     (exit-minibuffer))

(defun dismal-exit-minibuffer-up ()
  (interactive)
  (save-excursion (set-buffer dismal-buffer-using-minibuffer)
    (push '(dismal-forward-row -1) dismal-delayed-commands))
  (exit-minibuffer))

;(defun dismal-exit-minibuffer-right ()
;  (interactive)
;  (save-excursion (set-buffer dismal-buffer-using-minibuffer)
;    (push '(dismal-forward-column 1) dismal-delayed-commands))
;  (exit-minibuffer))

;(defun dismal-exit-minibuffer-left ()
;  (interactive)
;  (save-excursion (set-buffer dismal-buffer-using-minibuffer)
;      (push '(dismal-backward-column 1) dismal-delayed-commands))
;  (exit-minibuffer))


;;;
;;;	IV.	Dismal versions of commands
;;;

(defun dismal-query-replace (from-string to-string)
  "Replace some occurrences of FROM-STRING with TO-STRING.
Currently only works for cells with string values.
As each match is found, the user must type a character saying
what to do with it.  For directions, type C-h at that time.

Preserves case in each replacement if  case-replace  and  case-fold-search
are non-nil and FROM-STRING has no uppercase letters.
Third arg DELIMITED (prefix arg if interactive) non-nil means replace
only matches surrounded by word boundaries."
;; Query replacing aa with bb.
;; 
;; Type Space or `y' to replace one match, Delete or `n' to skip to next,
;; ESC or `q' to exit, Period to replace one match and exit,
;; Comma to replace but not move point immediately,
;; C-r to enter recursive edit (ESC C-c to get out again),
;; C-w to delete match and recursive edit,
;; C-l to clear the screen, redisplay, and offer same replacement again,
;; ! to replace all remaining matches with no more questions,
;; ^ to move point back to previous match.
  (interactive
   (let ((f-string (dismal-convert-input-to-cellexpr 
                      (read-string "Dismal query replace: "))))
   (list f-string
         (dismal-convert-input-to-cellexpr
             (read-string (format "Dismal query replace %s with: " f-string))))))

  (dismal-set-mark dismal-current-row dismal-current-col)
  (let ((i dismal-current-row)
        (j dismal-current-col)
        (done nil) cell-value  match-start
        prompt-result
        (prompt (format "Dismal query replacing %s with %s:"
                        (dismal-convert-cellexpr-to-string from-string)
                        (dismal-convert-cellexpr-to-string to-string)) ))
   (while (and (not done) (<= i dismal-max-row))
     (while (and (not done) (<= j dismal-max-col))
       (setq cell-value (dismal-get-exp i j))
       ;;(message "Doing %s:%s with <<%s>> match: %s" i j cell-value
       ;;         (and (stringp cell-value)
       ;;              (setq match-start
       ;;                  (string-match from-string cell-value))))(sit-for 2)
       ;; search forward for a match
       (dismal-query-replace-guts)
       (setq j (1+ j)) ) ; end while
     (setq j 0)
     (setq i (1+ i)) ) ;end while
   (message "Finished.")
   (sit-for 1)))

(defun dismal-query-replace-guts ()
  (if (cond ((stringp cell-value)
             (setq match-start
                   (string-match from-string cell-value)))
            (t (equal cell-value from-string)))
      (progn (dismal-jump-to-cell i j)  ;; present match
        (message prompt)                ;; query for action
        (setq prompt-result (downcase (read-char)))
        (dismal-save-excursion
        (cond ((or (= prompt-result ?\ ) (= prompt-result ?y))
               (dismal-set-exp i j  ;; need to be careful about
               (dismal-set-val i j  ;; need to be careful about
                   (if (stringp from-string)
                       (concat (substring cell-value 0 match-start)
                               to-string
                               (substring cell-value (match-end 0)))
                      to-string)))
               (dismal-redraw-cell i j t))
              ;; skip on del (127) and n
              ((or (= prompt-result ?n) (= prompt-result 127)))
              ;; C-h goes here too to give help
              ((= prompt-result ?\h)
               (with-output-to-temp-buffer "*Help*"
                 (princ query-replace-help)))
              ;; quit on anything else
              (t (setq done t)))))))

(defun dismal-isearch-backwards (search-string)
  "Do incremental search backwards in dismal, sorta.  Not started."
  (interactive "cDis I-search backward: ")
  (message "We don't do isearch-backwards yet."))

(defun dismal-isearch (search-string)
  "Do incremental search forward in dismal, sorta.  Not complete.
As you type characters, they add to the search string and are found.
Type Delete to cancel characters from end of search string.
Type ESC to exit, leaving point at location found.
Type C-s to search again forward, C-r to search again backward.
Type C-w to yank word from buffer onto end of search string and search for it.
Type C-y to yank rest of line onto end of search string, etc.
Type C-q to quote control character to search for it.
Other control and meta characters terminate the search
 and are then executed normally.
The above special characters are mostly controlled by parameters;
 do M-x apropos on search-.*-char to find them.
C-g while searching or when search has failed
 cancels input back to what has been found successfully.
C-g when search is successful aborts and moves point to starting point."
  (interactive "cDismal I-search: ")
  (setq search-string (char-to-string search-string))
  (let ((i dismal-current-row)
        (j dismal-current-col)
        (saved-i dismal-current-row)
        (saved-j dismal-current-col)
        (done nil) 
        result
        (prompt (format "Dismal I-search: %s" search-string)) )
   (while (and (not (eq result 'aborted)) (not done) (<= i dismal-max-row))
     (while (and (not (eq result 'aborted))
                 (not done) (<= j dismal-max-col))
       ;; search forward for a match
       (setq result (dismal-isearch-guts))
       (setq j (1+ j)) ) ; end while
     (setq j 0)
     (setq i (1+ i)) ) ;end while
   (cond ((eq result 'aborted)
          (dismal-jump-to-cell saved-i saved-j))
         ((not done)  (beep) ;; leave this beep without a t
          (message "Failing Dismal I-search: %s" search-string)
          (dismal-isearch-queryer)
          (if (not done)
              (dismal-jump-to-cell saved-i saved-j)))
         (t (dismal-set-mark-command))) ))

(defun dismal-isearch-guts ()
 ;(message "starting isearch-guts with %s at %s %s" search-string i j)
 (let ( match-start
         (cell-value (dismal-get-val i j)))
  (if (and (stringp cell-value)
           (setq match-start (string-match search-string cell-value)))
      (progn (dismal-jump-to-cell i j)  ;; present match
        (message prompt)                ;; query for action
        (dismal-isearch-queryer)))))

(defun dismal-isearch-queryer ()
        (setq next-char (char-to-string (read-char)))
        (cond ((string-match "[a-zA-Z0-9!@#$%^&*]" next-char)
               (setq search-string (concat search-string next-char))
               (setq prompt (format "Dismal I-search: %s" search-string))
               ;(message "  in isearch-guts with %s match %s" search-string
               ;            (string-match search-string cell-value))
               (if (string-match search-string cell-value)
                   (dismal-isearch-guts)))
              ((string-match "[]" next-char))
              ((string-match "[]" next-char)
               (setq done t))
              ;; quit on anything else
              ((string-match "[]" next-char)
               'aborted)
              (t (call-interactively (key-binding next-char))
                 ;(my message "just did interactively call/")
                 (setq done t))))


;;;
;;; 	V.	dismal-mark
;;;

(defvar dismal-mark [nil nil])
(make-variable-buffer-local 'dismal-mark)

(defmacro dismal-mark-row () 
  '(let ((result (aref dismal-mark 0)))
     (if (numberp result) 
         result
         (error "Mark not set."))))

(defmacro dismal-mark-col () 
  '(let ((result (aref dismal-mark 1)))
     (if (numberp result) 
         result
         (error "Mark not set."))))
                              
;; (defmacro dismal-mark-col () '(aref dismal-mark 1))

(defun dismal-set-mark-command ()
  "Set mark in dismal buffers to cell where point is at."
  (interactive)
  (dismal-set-mark dismal-current-row dismal-current-col)
  (message "Dismal mark set."))

;; this should probably become a push-mark, but alas, no time/understanding...
(defun dismal-set-mark (row col)
  (aset dismal-mark 0  row)
  (aset dismal-mark 1  col))

(defun dismal-exchange-point-and-mark ()
  "Put the dismal mark where point is now, and point where mark is now."
  (interactive)
  (let ((temp-row (aref dismal-mark 0))
        (temp-col (aref dismal-mark 1)))
    (aset dismal-mark 0 dismal-current-row)
    (aset dismal-mark 1 dismal-current-col)
    (dismal-jump-to-cell temp-row temp-col))  )


;;;
;;; 	VI.	Range and range-buffer functions
;;;
;;; dismal ranges look like:
;;;   (setq ar  '(dismal-range (dismal-r-c- 1 0) (dismal-r-c- 3 2)))

;; (inspect (dismal-string-to-range "A1:c3"))
(defun dismal-string-to-range (rangename)
  ;; Convert string RANGENAME to an expression representing a cell range.
  (string-match dismal-cell-range-regexp rangename)
  (let* ((from (substring rangename (match-beginning 1) (match-end 4)))
         (to (substring rangename (match-beginning 5) (match-end 8))))
    (list 'dismal-range
          (dismal-convert-cellname from)
          (dismal-convert-cellname to))))

(defun dismal-copy-range ()
  "Copy a range into the mark buffer."
  (interactive)
  (dismal-select-range)
  (dismal-show-selected-range)
  (let ((start-row (range-1st-row dismal-cell-buffer))
        (start-col (range-1st-col dismal-cell-buffer))
        (end-row (range-2nd-row dismal-cell-buffer))
        (end-col (range-2nd-col dismal-cell-buffer))  )
  (dismal-note-selected-range "Copying range %s%d:%s%d")
  (matrix-copy start-row start-col end-row end-col
       0 0 dismal-matrix (range-buffer-matrix dismal-range-buffer))
  (aset dismal-range-buffer 0 (abs (- start-row end-row)))
  (aset dismal-range-buffer 1 (abs (- start-col end-col)))
  (dismal-note-selected-range "Copied range %s%d:%s%d")))


(defun dismal-kill-range ()
  "Cut a range (mark + point) into the mark buffer."
  (interactive)
  (dismal-save-excursion
    (dismal-select-range)
    (if dismal-show-selected-ranges 
       (progn (dismal-show-selected-range)
              (dismal-note-selected-range "Cutting range %s%s:%s%d...")))
    (let ((start-row (range-1st-row dismal-cell-buffer))
          (start-col (range-1st-col dismal-cell-buffer))
          (end-row (range-2nd-row dismal-cell-buffer))
          (end-col (range-2nd-col dismal-cell-buffer))
          (dismal-interactive-p nil)  )
    (matrix-copy start-row start-col end-row end-col
         0 0 dismal-matrix (range-buffer-matrix dismal-range-buffer))
    (aset dismal-range-buffer 0 (abs (- start-row end-row)))
    (aset dismal-range-buffer 1 (abs (- start-col end-col)))
    (matrix-funcall-rc
          (function (lambda (r c dummy)
              (dismal-set-cell r c nil nil)
              (dismal-cleanup-long-string r c)))
          start-row start-col end-row end-col dismal-matrix)
    (if dismal-auto-update (dismal-private-update-matrix))
    (dismal-redraw-range start-row end-row)
    (if dismal-show-selected-ranges
       (dismal-note-selected-range "Cut range %s%s:%s%d")))))

(defun dismal-erase-range ()
  "Erase a range without saving."
  ;; deliberately leaves saved range-buffer alone though...
  (interactive)
  (let ((old-range-buffer-r (aref dismal-range-buffer 0))
        (old-range-buffer-c (aref dismal-range-buffer 1)))
  (dismal-select-range)
  (if dismal-show-selected-ranges (dismal-show-selected-range))
  (let ((start-row (range-1st-row dismal-cell-buffer))
        (start-col (range-1st-col dismal-cell-buffer))
        (end-row (range-2nd-row dismal-cell-buffer))
        (end-col (range-2nd-col dismal-cell-buffer))  )
  (dismal-note-selected-range "Erasing range %s%d:%s%d")
  (matrix-funcall-rc 
         (function (lambda (r c dummy) (dismal-set-cell r c nil nil)))
         start-row start-col end-row end-col dismal-matrix)
  (if dismal-auto-update (dismal-private-update-matrix))
  (aset dismal-range-buffer 0 old-range-buffer-r)
  (aset dismal-range-buffer 1 old-range-buffer-c)
  (dismal-note-selected-range "Erased range %s%d:%s%d"))))

;; this still leaves a problem if block-block-block is in col 1,
;; and then aaa is pasted in th middle, block aaa block results
;; b/c the cleanup thingy when it cleansup, redraws the aaa (or ""),
;; and
(defun dismal-paste-range ()
  "Paste a range into the spreadsheet."
  (interactive)
  (if (not (numberp (aref dismal-range-buffer 0)))
      (error "No range selected."))
  (if dismal-show-selected-ranges
     (dismal-note-selected-range "Pasting range %s%d:%s%d"))
  (dismal-save-excursion
  (let* ((end-row (aref dismal-range-buffer 0))
         (end-col (aref dismal-range-buffer 1))
         (over-max-row (- (+ dismal-current-row end-row)
                          dismal-max-row)) )
  ;; this attempts to clean up long strings
  (matrix-funcall-rc
      (function (lambda (r c dummy)
         (dismal-set-exp r c "") ;or val should work here
         (dismal-set-val r c "") ;or val should work here
         (let ((old-mrk (dismal-get-mrk r c)))
           (if (and (consp old-mrk) old-mrk)
               (dismal-cleanup-long-string (car old-mrk) (cdr old-mrk)))
           (dismal-cleanup-long-string r c))))
      dismal-current-row dismal-current-col
      (+ dismal-current-row end-row) (+ dismal-current-col end-col)
      dismal-matrix)
  (matrix-funcall-rc    ;; do the actual copy  
      (function (lambda (r c cell)
         (let ((expr (dismal-get-cell-exp cell))
               (orgrow (+ r (range-1st-row  dismal-cell-buffer)))
               (orgcol (+ c (range-1st-col  dismal-cell-buffer))))
         (dismal-set-exp (+ r dismal-current-row) (+ c dismal-current-col)
                         (dismal-change-indices expr
                                          (- dismal-current-row orgrow)
                                          (- dismal-current-col orgcol)))
         (dismal-set-val (+ r dismal-current-row) (+ c dismal-current-col)
                         nil)
         (dismal-invalidate-cell (dis-make-address r c))
         (dismal-redraw-cell (+ r dismal-current-row)
                             (+ c dismal-current-col) t))))
      0 0 
      end-row end-col
      ;; (if (= 0 end-row) 0 (1- end-row)) (if (= 0 end-col) 0 (1- end-col))
      (range-buffer-matrix dismal-range-buffer))
  ;; (matrix-copy 0 0 end-row end-col
  ;;    dismal-current-row dismal-current-col
  ;;    (range-buffer-matrix dismal-range-buffer) dismal-matrix)
  (setq dismal-max-col (max dismal-max-col (+ dismal-current-col end-col)))
  (if (> over-max-row 0)
      (progn (dismal-set-first-printed-column)
             (setq dismal-max-row (+ dismal-current-row end-row))
             (dismal-add-row-labels-at-end (1+ over-max-row))))
  ;; cleanup if you are written on, or if you might have wrote on someone
  (matrix-funcall-rc
      (function (lambda (r c dummy) 
         (let ((old-mrk (dismal-get-mrk r c)))
           (if (and (consp old-mrk) old-mrk)
               (dismal-cleanup-long-string (car old-mrk) (cdr old-mrk)))
           (dismal-cleanup-long-string r c)
           (dismal-redraw-cell r c t))))
      dismal-current-row dismal-current-col
      (+ dismal-current-row end-row) (+ dismal-current-col end-col)
      dismal-matrix)
  (dismal-visit-cell dismal-current-row dismal-current-col)
  (aset dismal-mark 0 (+ dismal-current-row end-row))
  (aset dismal-mark 1 (+ dismal-current-col end-col))
  (if dismal-auto-update (dismal-private-update-matrix))
 )))

;; should be lableled make-range
;; should be done destructively
(defun dismal-select-range ()
  ;; Select a range, setting dismal-cell-buffer to hold the result.
  (if (not (aref dismal-mark 0)) (error "Mark not set.")
  (let ((start-row dismal-current-row)   (start-col dismal-current-col)
        (end-row (dismal-mark-row))   (end-col (dismal-mark-col))
        result)
  (if (> start-row end-row)
      (progn (setq result start-row)
        (setq start-row end-row)
        (setq end-row result)))
  (if (> start-col end-col)
      (progn (setq result start-col)
         (setq start-col end-col)
         (setq end-col result)))
  (setq result
        (` (dismal-range (dismal-r-c- (, start-row) (, start-col))
                         (dismal-r-c- (, end-row)
                                             (, end-col)))))
  (aset dismal-range-buffer 0 nil) ; set to nil, 'cause now nothing is there
  (aset dismal-range-buffer 1 nil)
  (setq dismal-cell-buffer result))))

;; dead code from select-range
;; (defvar dismal-default-select-range-prompt "Select ending cell")
;;  (interactive)
;;  (message (or prompt dismal-default-select-range-prompt)
;;           (substitute-command-keys " and exit with \\[exit-recursive-edit]."))
;;  ;; put in more guards here, see forms--setup-buffer-create
;;  (recursive-edit)
;;  (dismal-jump-to-cell start-row start-col)

(defconst dismal-nsr-prompt "Selected range from %s%d to %s%d")

;   (dismal-note-selected-range "Deleting %s%s:%s%d...")
(defun dismal-note-selected-range (&optional prompt)
  (if dismal-interactive-p
      (message (or prompt dismal-nsr-prompt)
        (dismal-convert-number-to-colname (range-1st-col dismal-cell-buffer))
        (range-1st-row dismal-cell-buffer)
        (dismal-convert-number-to-colname (range-2nd-col dismal-cell-buffer))
        (range-2nd-row dismal-cell-buffer))))

(defun dismal-show-selected-range ()
  (if (not (eq (first dismal-cell-buffer) 'dismal-range))
      (error "No range selected.")
    (let ((r1r (range-1st-row dismal-cell-buffer))
          (r1c (range-1st-col dismal-cell-buffer))
          (r2r (range-2nd-row dismal-cell-buffer))
          (r2c (range-2nd-col dismal-cell-buffer)) )
    (if (and (= r1r r2r) (= r1c r2c))
        nil
    (dismal-visit-cell (if (= dismal-current-row r1r) r2r r1r)
                       (if (= dismal-current-col r1c) r2c r1c))
    ;; could put scroll in here...
    (sit-for 1)
    (dismal-visit-cell dismal-current-row dismal-current-col)))))

;(defun dismal-generate-range-list (fromcell tocell)
;  "Return a list of the addresses a range refers to.  FROMCELL is
;a cell reference to the upper left corner of the range, and TOCELL
;is refers to the lower right corner.  Both are in the
;form (dismal-r-c- row col)."
;  ;; Scan cells backwards so it ends up forwards.
;  (let ((list nil)
;        (row (nth 1 tocell))
;        (row1 (nth 1 fromcell))
;        (col (nth 2 tocell))
;        (col1 (nth 2 fromcell)))
;    (while (>= col col1)
;      (while (>= row row1)
;        (setq list (cons (cons row (cons col nil)) list))
;        (setq row (1- row)))
;      (setq col (1- col))
;      (setq row (nth 1 tocell)))
;    list))

;(defun dismal-range (fromcell tocell)
;  "Return a list of the values of a range of cells.  FROMCELL and
;TOCELL are cell references, in the form (dismal-r-c- row col)."
;  (mapcar 'dismal-evaluate-cellref
;          (dismal-generate-range-list fromcell tocell)))

(defun dismal-range-is-rows-or-columns ()
  (dismal-select-range)
  (if (not (eq (first dismal-cell-buffer) 'dismal-range))
      (error "No range selected.")
    (let ((r1r (range-1st-row dismal-cell-buffer))
          (r1c (range-1st-col dismal-cell-buffer))
          (r2r (range-2nd-row dismal-cell-buffer))
          (r2c (range-2nd-col dismal-cell-buffer)) )
      (cond ( (= r1r r2r) 'rows)
            ( (= r1c r2c) 'columns)
            (t nil)))))


;;;
;;;	VII.	Cell access and setting functions
;;;

(defun dismal-transpose-cells ()
  "Like ESC t but applies to adjacent horizontal cells.
Flips the current cell and the one to its left."
  ;; if on left edge, moves one cell right
  ;; this is stupid about updating forumla references, etc.
  (interactive)
  (dismal-save-excursion
  (if (= 0 dismal-current-col)
      (setq dismal-current-col (1+ dismal-current-col)))
  (let* ((cell1 (dismal-get-or-make-cell dismal-current-row dismal-current-col))
         (cell2 (dismal-get-or-make-cell dismal-current-row (1+ dismal-current-col)))
         (exp (dismal-get-cell-exp cell1))
         (val (dismal-get-cell-val cell1))
         (dep (dismal-get-cell-dep cell1))
         (mrk (dismal-get-cell-mrk cell1))
         (fmt (dismal-get-cell-fmt cell1))  )
    ;; swap A and B
    (dismal-set-cell-exp cell1 (dismal-get-cell-exp cell2))
    (dismal-set-cell-val cell1 (dismal-get-cell-val cell2))
    (dismal-set-cell-dep cell1 (dismal-get-cell-dep cell2))
    (dismal-set-cell-mrk cell1 (dismal-get-cell-mrk cell2))
    (dismal-set-cell-fmt cell1 (dismal-get-cell-fmt cell2))
    ;; swap temp and B
    (dismal-set-cell-exp cell2 exp)
    (dismal-set-cell-val cell2 val)
    (dismal-set-cell-dep cell2 dep)
    (dismal-set-cell-mrk cell2 mrk)
    (dismal-set-cell-fmt cell2 fmt)
    (dismal-redraw-row dismal-current-row t)))
  (dismal-forward-column 1))

(defun dismal-get-create-column-format (colnum)
  (or (vector-ref dismal-column-formats dismal-current-col)
      (vector-set dismal-column-formats dismal-current-col
                  (vec-copy-sequence-r dismal-default-column-format))))

(defun dismal-set-column-alignment (colnum style)
  (aset (dismal-get-create-column-format colnum)
        2 style))

;; Get the value of a particular field in a cell


;; heavyweight, make cell exist
(defun dismal-get-or-make-cell (r c)
  (let ((cell (matrix-ref dismal-matrix r c)))
    (or cell
       (matrix-set dismal-matrix r c (setq cell (make-vector 5 nil))))))

;; lightweight, does not make cell exist
(defun dismal-get-cell (r c)
  (matrix-ref dismal-matrix r c))

(defun dismal-get-cell-exp (cell) (if (null cell) nil (aref cell 0)))
(defun dismal-get-cell-val (cell) (if (null cell) nil (aref cell 1)))
(defun dismal-get-cell-dep (cell) (if (null cell) nil (aref cell 2)))
(defun dismal-get-cell-mrk (cell) (if (null cell) nil (aref cell 3)))
(defun dismal-get-cell-fmt (cell) (if (null cell) nil (aref cell 4)))

;; Set the value of a particular field in cell
;; may be dangerous

(defun dismal-set-cell-exp (cell x) (if (or cell x) (aset cell 0 x)))
(defun dismal-set-cell-val (cell x) (if (or cell x) (aset cell 1 x)))
(defun dismal-set-cell-dep (cell x) (if (or cell x) (aset cell 2 x)))
(defun dismal-set-cell-mrk (cell x) (if (or cell x) (aset cell 3 x)))
(defun dismal-set-cell-fmt (cell x) (if (or cell x) (aset cell 4 x)))

;; Get the value of a field of the cell at r, c

(defun dismal-get-exp (r c)
  (dismal-get-cell-exp (matrix-ref dismal-matrix r c)))
(defun dismal-get-val (r c)
  (dismal-get-cell-val (matrix-ref dismal-matrix r c)))
(defun dismal-get-deps (r c)
  (dismal-get-cell-dep (matrix-ref dismal-matrix r c)))

;; 2-Mar-92 -FER old address (row col) based way
; (defun dismal-get-deps (a)              ; A is a row-col pair.
;  (dismal-get-cell-dep (matrix-ref dismal-matrix (nth 0 a) (nth 1 a))))

(defun dismal-get-mrk (r c)
  (dismal-get-cell-mrk (matrix-ref dismal-matrix r c)))
(defun dismal-get-fmt (r c)
  (dismal-get-cell-fmt (matrix-ref dismal-matrix r c)))

(defun dismal-set-exp (r c x)
  ;; Set the value of a field of the cell at r, c
  (dismal-set-cell-exp (dismal-get-or-make-cell r c) x))

(defun dismal-set-val (r c x)
  (dismal-set-cell-val (dismal-get-or-make-cell r c) x))

(defun dismal-set-deps (r c x)
  (dismal-set-cell-dep (dismal-get-or-make-cell r c) x))

;; old way, with address as a list
;(defun dismal-set-deps (a x)    ; A is a row-col pair
;  (let ((cell (matrix-ref dismal-matrix (nth 0 a) (nth 1 a))))
;    (if (null cell)
;        (matrix-set dismal-matrix (nth 0 a) (nth 1 a)
;                    (setq cell (make-vector 5 nil))))
;    (dismal-set-cell-dep cell x)))

(defun dismal-set-mrk (r c x)
  (dismal-set-cell-mrk (dismal-get-or-make-cell r c) x))

;; this really means alignment...16-Jan-92 -FER
(defun dismal-set-fmt (r c x)
  (dismal-set-cell-fmt (dismal-get-or-make-cell r c) x))


;;;
;;; 	VIII.	Changed movement functions
;;;

;; 2-8-93 - EMA: behaves just like move-to-window-line:
(defun dismal-move-to-window-line (arg)
  "Position point relative to dismal window.
With no argument, position at text at center of window.
An argument specifies screen line; zero means top of window,
negative means relative to bottom of window."
  (interactive "P")
  (let* ((distance-to-move
	  (cond ((null arg)		; go to middle row
                 (- (/ (window-height) 2) (current-line-in-window)))
	        ((minusp arg)		; displacement from bottom
                 (- (+ (1- (window-height)) arg) (current-line-in-window)))
                (t			; displacement from top
                  (- arg (current-line-in-window))))))
    (dismal-jump-to-cell (max 0 (+ dismal-current-row distance-to-move))
			 dismal-current-col)))

(defun dismal-scroll-in-place (arg)
  (let ((lines-from-top (current-line-in-window)))
  (dismal-undraw-ruler-rows)
  (let ((dismal-show-ruler nil))
    (dismal-visit-cell arg dismal-current-col)
    (recenter lines-from-top)
    (setq dismal-current-row arg))
  (dismal-draw-ruler arg)  ))

;; 2-8-93 - EMA fix: "or arg" to "if arg (car arg)", to allow C-u to
;;   specify the number of lines to scroll.
(defun dismal-scroll-up-in-place (arg)
  "Scroll cells of dismal window upward ARG lines or nearly a full screen if
no ARG.  When calling from a program, supply a number as argument or
nil.  Leaves point in same row and column of window [which seems wrong]."
  (interactive "P")
  (let* ((addition (if arg (car arg) (- (window-height) 2)))
         (scroll-to (+ dismal-current-row addition)))
    (dismal-scroll-in-place (min dismal-max-row scroll-to))
    (if (> scroll-to dismal-max-row)
	(error "End of buffer."))))

;; you could make the -2 here an arg for how much to bump up
(defun dismal-scroll-down-in-place (arg)
  "Scroll cells of dismal window down ARG lines or nearly a full screen if
no ARG.  When calling from a program, supply a number as argument or
nil.  Leaves point in same row and column of window [which seems wrong]."
  (interactive "P")
  (dismal-scroll-in-place (max 0 (- dismal-current-row (window-height) -2))))

(defun dismal-forward-column (cols)
  "Move forward COLS columns."
  (interactive "p")
  (dismal-move-columns cols)
  (dismal-visit-cell dismal-current-row dismal-current-col))

;; moves over hidden columns
(defun dismal-move-columns (arg)
 (let ((n (abs arg))
       (direction (signp arg)))
 (while (and (> n 0)
             (or (> dismal-current-col 0)
                 (plusp direction)))
   (setq dismal-current-col (+ dismal-current-col direction))
   (if (> (dismal-column-width dismal-current-col) 0)
       (setq n (1- n))) )) )

;  (cond
   ;((and (= dismal-current-col 0)  ; this doesn't work as nicely
   ;as I hoped...FER
   ;      (> dismal-current-row 0))
   ; (setq dismal-current-col dismal-max-col)
   ; (setq dismal-current-row (- dismal-current-row 1)))
;   (t (setq dismal-current-col (max 0 (- dismal-current-col cols)))))

(defun dismal-first-column ()
  "Move to first column."
  (interactive)
  (setq dismal-current-col 0)
  (dismal-visit-cell dismal-current-row dismal-current-col))

(defun dismal-last-column ()
  "Move to last column."
  (interactive)
  (setq dismal-current-col dismal-max-col)
  (dismal-visit-cell dismal-current-row dismal-current-col))

(defun dismal-end-of-row ()
  "Move to last column with a value."
  (interactive)
  (setq dismal-current-col dismal-max-col)
  (while (and (>= dismal-current-col 1)
              (not (dismal-get-exp dismal-current-row dismal-current-col)))
     (setq dismal-current-col (1- dismal-current-col)))
  (dismal-visit-cell dismal-current-row dismal-current-col))

(defun dismal-previous-filled-row-cell (rows)
  "Move upward ROWS filled row cells (i.e., skip empty cells on the way up)."
  (interactive "p")
  (dismal-next-filled-row-cell (- rows)))

(defun dismal-next-filled-row-cell (rows)
  "Move down ROWS filled row cells (i.e., skip empty cells on the way down)."
  (interactive "p")
  (let ((old-row dismal-current-row)
        (old-col dismal-current-col)
        (direction (signp rows))
        (number (abs rows))  )
  (while (and (> number 0) (dismal-find-next-fill-row direction))
     (setq number (1- number)))
  (if (> number 0)
      (progn (setq dismal-current-row old-row)
             (setq dismal-current-col old-col)))
  (dismal-visit-cell dismal-current-row dismal-current-col)))

;; increment must be +/-1
(defun dismal-find-next-fill-row (increment)
 (let ((start-row dismal-current-row))
 ;; initial move
 (cond ((and (plusp increment) (= dismal-current-row dismal-max-row))
        (setq dismal-current-row 0))
       ((and (minusp increment) (= dismal-current-row 0))
        (setq dismal-current-row dismal-max-row))
       (t (setq dismal-current-row (+ increment dismal-current-row))))
 (while (and (not (dismal-get-exp dismal-current-row dismal-current-col))
             (not (= start-row dismal-current-row)))
    (cond ((and (plusp increment) (= dismal-current-row dismal-max-row))
           (beep) ;; leave this beep without a t
           (and dismal-interactive-p 
                (progn (message "Wrapping around forwards...") (sit-for 1)))
           (setq dismal-current-row 0))
          ((and (minusp increment) (= dismal-current-row 0))
           (beep) 
           (and dismal-interactive-p
                (progn (message "Wrapping around backwards...") (sit-for 1)))
           (setq dismal-current-row dismal-max-row))
          (t (setq dismal-current-row (+ increment dismal-current-row)))))
 (if (= start-row dismal-current-row)
     (error "No (other) filled cell to move to in this column."))
 (dismal-get-exp dismal-current-row dismal-current-col)))

(defun dismal-backward-filled-column (cols)
  "Move backward COLS filled columns (i.e., skip empty columns)."
  (interactive "p")
  (dismal-forward-filled-column (- cols)))

(defun dismal-forward-filled-column (cols)
  "Move forward COLS filled columns (i.e., skip empty columns)."
  (interactive "p")
  (let ((old-row dismal-current-row)
        (old-col dismal-current-col)
        (direction (signp cols))
        (number (abs cols))  )
  (while (and (> number 0) (dismal-find-next-fill-column direction))
     (setq number (1- number)))
  (if (> number 0)
      (progn (setq dismal-current-row old-row)
             (setq dismal-current-col old-col)))
  (dismal-visit-cell dismal-current-row dismal-current-col)))

;; increment must be +/-1
(defun dismal-find-next-fill-column (increment)
 ;; initial move
 (cond ((or (and (minusp increment) (dismal-bobp))
            (and (plusp increment) (dismal-eobp)))
        nil)
       ((and (plusp increment) (= dismal-current-col dismal-max-col))
        (setq dismal-current-row (+ 1 dismal-current-row))
        (setq dismal-current-col 0))
       ((and (minusp increment) (= dismal-current-col 0))
        (setq dismal-current-row (+ -1 dismal-current-row))
        (setq dismal-current-col dismal-max-col))
       (t (setq dismal-current-col (+ increment dismal-current-col))))
 (while (and (not (and (dismal-get-exp dismal-current-row dismal-current-col)
                       (< 0 (col-format-width (dismal-get-column-format dismal-current-col)))))
             ;; not stuck at either end
             (not (or (and (minusp increment) (dismal-bobp))
                      (and (plusp increment) (dismal-eobp)))))
    (cond ((and (plusp increment) (= dismal-current-col dismal-max-col))
           (setq dismal-current-row (+ increment dismal-current-row))
           (setq dismal-current-col 0))
          ((and (minusp increment) (= dismal-current-col 0))
           (setq dismal-current-row (- dismal-current-row 1))
           (setq dismal-current-col dismal-max-col))
          (t (setq dismal-current-col (+ increment dismal-current-col)))))
 (dismal-get-exp dismal-current-row dismal-current-col))

(defun dismal-start-of-col ()
  "Move to first row in current column."
  (interactive)
  (setq dismal-current-row 0)
  (dismal-goto-cell dismal-current-row dismal-current-col t)
  (dismal-display-current-cell-expr dismal-current-row dismal-current-col))

(defun dismal-end-of-col ()
  "Move to last row with a value in current column."
  (interactive)
  (dismal-end-of-col-non-interactive)
  (dismal-display-current-cell-expr dismal-current-row dismal-current-col))

(defun dismal-end-of-col-non-interactive ()
  (setq dismal-current-row dismal-max-row)
  (while (and (not (dismal-get-exp dismal-current-row dismal-current-col))
              (> dismal-current-row 0))
     (setq dismal-current-row (1- dismal-current-row)))
  (dismal-goto-cell dismal-current-row dismal-current-col t))

(defun dismal-backward-column (cols)
  "Move backward COLS columns."
  (interactive "p")
  (dismal-move-columns (- cols))
  (dismal-visit-cell dismal-current-row dismal-current-col))

(defun dismal-forward-row (rows)
  "Move down ROWS rows."
  (interactive "p")
  (dismal-move-rows rows)
  (dismal-visit-cell dismal-current-row dismal-current-col))

(defun dismal-backward-row (rows)
  "Move up ROWS rows."
  (interactive "p")
  (dismal-move-rows (- rows))
  (dismal-visit-cell dismal-current-row dismal-current-col))

;; moves over hidden rows (none currently)
(defun dismal-move-rows (arg)
 (let ((n (abs arg))
       (direction (signp arg)))
 (while (and (> n 0)
             (or (> dismal-current-row 0)
                 (plusp direction)))
   (setq dismal-current-row (+ dismal-current-row direction))
   (setq n (1- n))) ))

(defun dismal-visit-cell (row column)
  ;; Move cursor to ROW, COLUMN and display the contents of the cell
  ;; in the status line.
  (if (not (= dismal-auto-save-counter 0))
      (progn (setq dismal-auto-save-counter (1- dismal-auto-save-counter))
         (if (= dismal-auto-save-counter 1)
             (dismal-do-auto-save))))
  (dismal-goto-cell row column t)
  (if dismal-interactive-p
     (dismal-display-current-cell-expr row column)))

(defun dismal-goto-cell (row column interactivep)
  ;; Move cursor to the end of the cell at ROW, COLUMN.
  ;; does not set dismal-current-row, etc.
  (dismal-goto-row row interactivep)
  (dismal-goto-column column))

(defun dismal-goto-row (row interactivep)
  ;; Move the cursor to the requested ROW.
  (let* ((current-window-row (current-line-in-window))
         (window-rows (1- (window-height)))
         ;; (put-rule-up-later nil)
         (raw-offset (- row dismal-current-row)) )
  ;; autoshowing ruler is too slow
  ;; (if (and dismal-show-ruler interactivep)
  ;;     (cond ((< (+ current-window-row raw-offset) 2)   ;scrolling up
  ;;            (dismal-undraw-ruler-rows)
  ;;            (setq put-rule-up-later t)
  ;;            (scroll-down-in-place (+ (- raw-offset) 2)) )
  ;;           ((>= (+ current-window-row raw-offset) window-rows) ;scrlng down
  ;;            (dismal-undraw-ruler-rows)
  ;;            (setq put-rule-up-later t))
  ;;           (t nil)))
  (let ((rows-missing (goto-line (+ row dismal-first-data-line))))
    (if (not (bolp)) (setq rows-missing (1+ rows-missing)))
    (open-line rows-missing)
    (forward-char rows-missing))
  ;; (if (and interactivep put-rule-up-later)
  ;;    (dismal-draw-ruler row))
  )  )

;(setq spot (list  interactivep current-window-row row window-rows raw-offset))
;(if interactivep
;   (progn (message "%s crow %s torow %s wrows %s raw-offset %s"
;             interactivep current-window-row row window-rows raw-offset)
;          (sit-for 2)))


(defun dismal-goto-column (column)
  ;; Move the cursor to the last character of COLUMN.
  (let* ((col (dismal-get-column-position (1+ column)))
         (width (dismal-column-width column))
         (original-modified-p (buffer-modified-p))
         (chars-missing (- col (move-to-column col))))
    (if (> chars-missing 0)
        (progn (insert-char ?\040 chars-missing)     ; Insert some blanks
          (end-of-line)
          (set-buffer-modified-p original-modified-p)))
    ;; this ins't quite right, but is pleasent n.t.l.
    ;; (setq aa (list 'last-col column 'cc (current-column)
    ;;               'hscroll (window-hscroll)
    ;; 'going2 (> (+ 3 (current-column)) (+ (window-hscroll) (window-width)))))
    (if (> (+ 3 (current-column)) (+ (window-hscroll) (window-width)))
        (scroll-left (- (current-column) -4
                        (+ (window-hscroll) (window-width))))
        (if (< (- (current-column) width dismal-first-printed-column)
               (window-hscroll))
            (scroll-right (+ width dismal-first-printed-column
                             (- (window-hscroll)
                                (current-column))))))
       ;; (set-window-hscroll) may also work
       ;; Set number columns WINDOW is scrolled f/ l. margin to NCOL.
    (backward-char 1) ))
;;  the number of columns by which WINDOW is scrolled from left margin.

(defun dismal-beginning-of-buffer ()
  "Move dismal point to the beginning of the buffer."
  (interactive)
  (dismal-jump-to-cell 0 0))

(defun dismal-end-of-buffer ()
  "Move dismal point to the end of the buffer."
  (interactive)
  (dismal-jump-to-cell dismal-max-row dismal-max-col))

(defun dismal-jump (&optional r c)
  "Jump to ROW, COLUMN and display the contents of the cell
in the status line."
  (interactive "P") ;"nRow to goto: \nnColumn to goto (0 is A): "
  (if (not r)
      (setq r (dismal-read-minibuffer "Row to goto: " nil dismal-current-row)))
                                      ;; (prin1-to-string dismal-current-row)
  (if (not c)
      (setq c (dismal-read-minibuffer "Column to goto: "  nil
                 (dismal-convert-number-to-colname dismal-current-col))))
  (cond ((numberp r))
        ((not r) (setq r dismal-current-row))
        ((not (numberp r))
         (setq r (dismal-convert-colname-to-number (prin1-to-string r)))))
  (cond ((numberp c))
        ((not c) (setq c dismal-current-row))
        ((and (not (numberp c)) (stringp c))
         (setq c (dismal-convert-colname-to-number c)))
        ((and (not (numberp c)) (not (stringp c)))
         (setq c (dismal-convert-colname-to-number (prin1-to-string c)))))
  (dismal-jump-to-cell r c))



;;;
;;;	IX.	Cell editing
;;;


;;;;;;;;;;;;;;;; copy-to-dismal.el ;;;;;;;;;;;;;;;;
;;;
;;; Robert J. Chassell    bob@gnu.ai.mit.edu
;;; 12 July 1992
;;;
;;; Change Log ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Version 0.03 
;;; 12 July 1992 - Updated to work with Dismal version 0.62
;;;
;;; Version 0.02 
;;; 18 Nov 1991 - Handle both long and short text strings, such as
;;; month names. Handle blank lines.    
;;;
;;; Version 0.01
;;; 14 Nov 1991 - Initial.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dismal-copy-column-separator " ")

;; does not keep point and mark clean in original buffer
;; does not keep point and mark clean in dismal buffer
(defun copy-to-dismal (dismal-buffer-name beg end) 

"Copy column specified by point and mark to buffer DISMAL-BUFFER-NAME
starting at its current cell.  Point and mark must be within or at the
beginning of a column of text, delimited by blanks.  The column must
contain words without spaces or valid dismal numbers, which may
contain decimal points but not commas.  The variable 
dismal-copy-column-seperator (default is space) is used to separate columns.

For example, in the following column, you would place point on 2 in
123 and mark on the 8 in 789 and only that middle column would be
copied to the spreadsheet.

Jan    123   555
Feb    456   666
Mar    789   777"

 (interactive "BDismal buffer name: \nr")
 (if (eq major-mode 'dismal)
     (error "You must start in a non-dismal buffer.")
 (goto-char beg)
 ;; Record current column
 (save-excursion
 (let ((start-col (current-column)))
   ;; Go to beginning of column
   (if (re-search-backward dismal-copy-column-separator
                       ;; "\\(^\\w\\|[ \t\n]+\\)"
                       (save-excursion (beginning-of-line) (point))
                       'just-move-to-beginning-on-fail)
        (forward-char 1))
   (setq col (current-column))
   (setq match-start (point))
   (while (< (point) end)
     (let ((item (progn
                      (if (re-search-forward dismal-copy-column-separator
                                         (save-excursion (end-of-line) (point))
                                         'just-move-to-end-on-fail)
                          (forward-char -1))
                      (buffer-substring match-start (point)))))
       (save-excursion
          ;; dismal has to be in a window to readraw
          (switch-to-buffer (get-buffer dismal-buffer-name))
          (dismal-read-cell-plain item)
          (dismal-forward-row 1))
        (forward-line 1)
        ;; Check if blank line
        (if (looking-at "[ \t]*$") (next-line 1))
        ;; Stay within the correct colum
        (move-to-column start-col)
        ;; Go to beginning of column
        (if (re-search-backward dismal-copy-column-separator
                       ;; "\\(^\\w\\|[ \t\n]+\\)"
                       (save-excursion (beginning-of-line) (point))
                       'just-move-to-beginning-on-fail)
            (forward-char 1))
        (setq col (current-column))
        (setq match-start (point))))))))


;; 14.  Allow entering of numbers and strings not in quotes?
;;   Yes, you have to put quotation marks around a number it has a decimal
;;   point and nonzero digits after the decimal, e.g. "3.1416".  Try it.
;;   This is because 3.1416 is not a lisp object, it is three lisp
;;   objects, and dismal-read-cell accepts its input as an s-expression.
;;   It may be possible to go through the input s-expression and find
;;   the dotted pairs of two numbers and convert them to floating point
;;   numbers (which are themselves dotted pairs of two numbers, but
;;   won't ever appear in the input.)  The function
;;   dismal-convert-cellexpr-to-string converts the cell's current value
;;   to a string so the use can edit its value.  If you change
;;   dismal-convert-input-to-cellexpr to handle numbers not in quotes,
;;   you must change dismal-convert-cellexpr-to-string so it doesn't
;;   put the numbers in quotes.
;; 

(defun dismal-delete-cell (arg &optional kill-flag)
  "Delete the following ARG cells (previous, with negative arg).
Does not save in kill range buffer unless kill-flag is set.
Interactively, ARG is the prefix arg, and KILLFLAG is set if
ARG was explicitly specified. dismal-kill-cell saves."
  (interactive "P")
  (dismal-save-excursion
  ;; (message "got arg %s with %s" arg (interactive-p)) (sit-for 2)
  (if (and arg (interactive-p))
      (setq kill-flag t))
  (setq arg (prefix-numeric-value arg))
  (dismal-set-mark dismal-current-row dismal-current-col)
  (dismal-forward-column (1- arg))
  (let ((dismal-show-selected-ranges nil))
  (if kill-flag
     (dismal-kill-range)
    (dismal-erase-range)))))

(defun dismal-kill-cell (arg)
  "Kill ARG cells backward."
  (interactive "p")
  (dismal-save-excursion
    (dismal-set-mark dismal-current-row dismal-current-col)
    (dismal-backward-column (1- arg))
    (let ((dismal-show-selected-ranges nil))
      (dismal-kill-range))))

(defun dismal-backward-kill-cell (arg)
  "Kill ARG cells backward."
  (interactive "p")
  (dismal-save-excursion
    (dismal-set-mark dismal-current-row dismal-current-col)
    (dismal-backward-column (1- arg))
    (let ((dismal-show-selected-ranges nil))
      (dismal-kill-range)))
  (dismal-backward-column 1))

(defun dismal-read-cell-center (sexp)
  "Read a cell value, convert it to internal format, and make that the
current cell's value."
  (interactive                          ; Bob's cell editing prompt
   (list (dismal-read-minibuffer "Enter expression (center): " t
           (dismal-convert-cellexpr-to-string
              (dismal-get-exp dismal-current-row dismal-current-col)))))
  (dismal-read-cell sexp 'center))

(defun dismal-read-cell-rightjust (sexp)
  "Read a right justified value into the current cell."
  (interactive                          ; Bob's cell editing prompt
   (list (dismal-read-minibuffer "Enter expression (right): " t
           (dismal-convert-cellexpr-to-string
               (dismal-get-exp dismal-current-row dismal-current-col)))))
  (dismal-read-cell sexp 'right))


(defun dismal-read-cell-default (sexp)
  "Read a default justified value into the current cell."
  (interactive                          ; Bob's cell editing prompt
   (list (dismal-read-minibuffer "Enter expression (default): " t
           (dismal-convert-cellexpr-to-string
               (dismal-get-exp dismal-current-row dismal-current-col)))))
  (dismal-read-cell sexp 'default))

(defun dismal-read-cell-leftjust (sexp)
  "Read a left justified value into the current cell."
  (interactive                          ; Bob's cell editing prompt
   (list (dismal-read-minibuffer "Enter expression (left): " t
           (dismal-convert-cellexpr-to-string
               (dismal-get-exp dismal-current-row dismal-current-col)))))
  (dismal-read-cell sexp 'left))

;; now redundant, other reads can handle strings.
;(defun dismal-read-cell-string (sexp)
;  "Read a left justified value into the current cell."
;  (interactive
;    (list
;  (let ((cell-exp (dismal-get-exp dismal-current-row dismal-current-col)))
;     (read-string "Enter string (no \"'s): "
;                     (if (stringp cell-exp) 
;                     cell-exp
;                     (dismal-convert-cellexpr-to-string cell-exp))))))
;  (dismal-read-cell sexp (dismal-get-fmt dismal-current-row 
;                                         dismal-current-col)))

(defun dismal-read-cell-plain (sexp)
  "Read a cell value, convert it to internal format, and make that the
current cell's value."
  (interactive                          ; Bob's cell editing prompt
   (list (dismal-read-minibuffer "Enter expression: " t
            (dismal-convert-cellexpr-to-string
                 (dismal-get-exp dismal-current-row dismal-current-col)))))
   (dismal-read-cell sexp (dismal-get-fmt dismal-current-row
                                          dismal-current-col)))

(defun dismal-read-cell (sexp alignment)
  ;;(dismal-save-excursion-quietly) used to be wrapped here
  ;; and save-excursion would not work on it's own?!
  (dismal-save-excursion
  (let ((old-point (point))
        (dismal-interactive-p nil))
     (dismal-set-cell dismal-current-row dismal-current-col
                      (dismal-convert-input-to-cellexpr sexp)
                      alignment)
  (if dismal-auto-update
      (dismal-private-update-matrix)
    (dismal-cleanup-long-string dismal-current-row dismal-current-col))
  (dismal-execute-delayed-commands)
  (dismal-display-current-cell-expr dismal-current-row dismal-current-col)
  (goto-char old-point)
  (dismal-hard-redraw-row-non-interactive))))

(defun dismal-execute-delayed-commands ()
  (while dismal-delayed-commands
    (eval (pop dismal-delayed-commands))))

; (dismal-read-minibuffer "gimme: " nil 34)
; (dismal-read-minibuffer "gimme: " t "34")
;   (dismal-convert-cellexpr-to-string (dismal-get-exp dismal-current-row dismal-current-col))

; (dismal-read-minibuffer "how are you" nil 'fine)
; (dismal-read-minibuffer "how are you" t "fine")

(defun dismal-read-minibuffer (prompt editable-default default)
 (if (not editable-default)
     (setq prompt (format "%s [%s]: " prompt default)))
 (if (not (stringp default)) (setq default (format "%s" default)))
 (let* ((minibuffer-local-map dismal-minibuffer-local-map)
        (dismal-buffer-using-minibuffer (current-buffer))
        (first-result (if editable-default
                          (read-string prompt default)
                          (read-string prompt))) )
  (cond ((string= "" first-result)
         (if editable-default
             nil
             default))
        ((formula-string-p first-result)
         (car (read-from-string first-result)))
        ((integer-stringp first-result) (car (read-from-string first-result)))
        ((float-stringp first-result) first-result)
        (t first-result)  )))

;; This is called by most of the other functions on this page.

(defun dismal-set-cell (row column sexp format)
  ;; Assign cell at position ROW, COLUMN the expression SEXP.  FORMAT is the
  ;; function (or nil for default) to use to format the value for display.
  (if (> row dismal-max-row)
      (while (> row dismal-max-row)
        (setq dismal-max-row (1+ dismal-max-row))
        (dismal-set-first-printed-column)
        (dismal-draw-row-label dismal-max-row)))
  (if (> column dismal-max-col)
      (while (> column dismal-max-col)
        (setq dismal-max-col (1+ dismal-max-col))
        (dismal-draw-column-label dismal-max-col)))
  (dismal-erase-dependencies row column (dismal-get-exp row column))
  (dismal-set-exp row column sexp)
  (dismal-set-val row column nil)
  (if format (dismal-set-fmt row column format))
  ;; if cell could have a formula, add it to the formula list (else remove it)
  ;; and set mark that it is a non-formula cell
  (if (dismal-possible-live-sexp sexp)
      (dismal-record-dependencies row column sexp)
    (vector-remove dismal-formula-cells (cons row column))
    (dismal-set-mrk row column nil))
  (dismal-invalidate-cell (dis-make-address row column))
  (set-buffer-modified-p t))

(defun dismal-capitalize-cell (arg)
  "Capitalize the current cell (or ARG cells), moving over if arg >1 (default).
This gives the cell(s) first character in upper case and the rest lower case."
  (interactive "p")
  (while (> arg 0)
    (dismal-save-excursion
    (let* ((cell-exp (dismal-get-exp dismal-current-row dismal-current-col))
           start)
    (if (stringp cell-exp)
        (progn (setq start (1+ (string-match "[^ ]" cell-exp) ))
          (dismal-set-exp dismal-current-row dismal-current-col
          (dismal-set-val dismal-current-row dismal-current-col
                          (concat (capitalize (substring cell-exp 0 start))
                                  (downcase (substring cell-exp start)))))))
    (dismal-redraw-cell dismal-current-row dismal-current-col t)))
    (if (>= arg 0) (dismal-forward-column 1))
    (setq arg (1- arg))  ))

(defun dismal-downcase-cell (arg)
  "Downcase the current cell (or ARG cells), moving over if arg >= 2.
This gives the cell(s) all lower case characters."
  (interactive "p")
  (while (> arg 0)
    ;; dismal-save-excursion
    (let ((cell-exp (dismal-get-exp dismal-current-row dismal-current-col)))
    (if (stringp cell-exp)
        (dismal-set-exp dismal-current-row dismal-current-col
        (dismal-set-val dismal-current-row dismal-current-col
                        (downcase cell-exp))))
    (dismal-redraw-cell dismal-current-row dismal-current-col t)
    ;; if you have dependencies, should update them here...
    (if (not (= arg 0)) (dismal-forward-column (signp arg)))
    (setq arg (1- arg))    )))

(defun dismal-upcase-cell (arg)
  "Upcase the current cell (or ARG cells), moving over if arg >= 1.
This gives the cell(s) characters all in upper case."
  (interactive "p")
  (while (> arg 0)
    (let ((cell-exp (dismal-get-exp dismal-current-row dismal-current-col)))
      (if (stringp cell-exp)
          (dismal-set-exp dismal-current-row dismal-current-col
          (dismal-set-val dismal-current-row dismal-current-col
                          (upcase cell-exp))))
    (dismal-redraw-cell dismal-current-row dismal-current-col t)
    ;; if you have dependencies, should update them here...
    (if (not (= arg 0)) (dismal-forward-column (signp arg)))
    (setq arg (1- arg))    )))


;;;
;;;	X.	Cell re-evaluation 
;;;

(defun dismal-update-matrix ()
  "Recalculate the dirty cells in the spreadsheet."
  (interactive)
  (dismal-save-excursion
  (if (not dismal-auto-update)
      (message "Updating the matrix..."))
  (dismal-private-update-matrix)
  (message "Updating the matrix...Finished.")))

(defun dismal-private-update-matrix ()
  ;; actually recalculate the cells in the invalid heap
  (let ((temp nil)
        (i 1))
    (while (and (<= i dismal-iteration-limit)
                (not (heap-empty dismal-invalid-heap)))
      (message "Starting to update cycle %s ..." i)
      (dismal-update-cycle)
      (setq i (1+ i))
      (setq temp dismal-invalid-heap)
      (setq dismal-invalid-heap dismal-invalid-heap-not)
      (setq dismal-invalid-heap-not temp))
    ;; check to see how long you did this...
    (if (not (heap-empty dismal-invalid-heap))
        (message "Update stopped due to exceeding max cycles of %s."
                 dismal-iteration-limit)
        (message "Updated %s times." (1- i)) )    ))

(defun dismal-update-cycle ()
  (let ((prev nil))
  (while (not (heap-empty dismal-invalid-heap))
    (let* ((addr (heap-deletemin dismal-invalid-heap))
           (r (dis-address-row addr))
           (c (dis-address-col addr))
           (new-val nil)
           (old-val (dismal-get-val r c)))
      ;(message "starting with old-val of %s:%s of %s" r c old-val)
      (if (equal addr prev)
          nil   
        (setq new-val (dismal-set-val r c (dismal-eval (dismal-get-exp r c))))
        ;;(message "updat'n %s, got [%s] had [%s] equal= %s" 
        ;;        addr new-val old-val (equal old-val new-val))  (sit-for 1)
        (if (not (equal old-val new-val))
            (let ((dismal-invalid-heap dismal-invalid-heap-not))
               (dismal-invalidate-cell addr)))
        (dismal-redraw-cell r c t))
      (setq prev addr)))))

(defun dismal-invalidate-cell (addr)
  ;; Mark the cell at ADDR invalid (if necessary) and (recursively) all cells 
  ;; that depend on it, by inserting their addresses into dismal-invalid-heap.
  (let ((r (dis-address-row addr))
        (c (dis-address-col addr)) )
  ;(message "invalidating %s %s" r c)
  ;; only invalidate cells that can be updated
  (if (vector-member dismal-formula-cells addr)
      (heap-insert dismal-invalid-heap addr)
     (dismal-redraw-cell r c t))
  ;;is this necessary? seems to lead to problems...
  ;(dismal-set-val row col nil)  
  (if (eq 'visited (dismal-get-mrk r c))
      ()
    (dismal-set-mrk r c 'visited)
    (dismal-map-apply 'dismal-invalidate-cell (dismal-get-deps r c))
    (dismal-set-mrk r c 0))))

(defun dismal-recalculate-matrix ()
  "Recalculate and redraw the whole matrix."
  (interactive)
  (matrix-map-rc (function (lambda (cell dummy)
                    (if (dismal-get-exp (car cell) (cadr cell))
                        (heap-insert dismal-invalid-heap (cons  (car cell) (cadr cell))))))
                 dismal-matrix)
  (dismal-update-matrix)
  (dismal-redraw))


;;;
;;;	XI.	Cell evaluation
;;;

(defun dismal-evaluate-cell (row column)
  ;; Look for a current value for this cell at ROW, COLUMN.  If it has
  ;; none evaluate the associated expression and return the result.
  (let ((value (dismal-get-val row column)))
    (if (null value)
        (let ((sexp (dismal-get-exp row column)))
          (if (not sexp)
              ()
;; This is where code to check recursion depth should go.  Be careful,
;; because the -mrk field is used by the invalidation code, which sets
;; it to 'visited and then resets it to nil when it is done.
;           (let ((recursion-depth (dismal-get-mrk row column)))
;             (if (>= recursion-depth dismal-recursion-limit)
;                 (message (concat "Recursion depth exceeded on "
;                                  (prin1-to-string (list row column))))
;               (dismal-set-mrk row column (1+ recursion-depth))
                (setq value (dismal-eval sexp))
;               (dismal-set-mrk row column (1- recursion-depth))))
            (dismal-set-val row column value))))
    value))

;; Note that these all do the same thing.  We distinguish between them
;; when we are inserting or deleting rows and columns.  At that time
;; we look at the content of the cell expressions and change the indices
;; depending on which of these functions is used.  See "CELL REFERENCES".
;; f stands for fixed.

(defun dismal-r-c- (row column) (dismal-evaluate-cellref (cons row column)))
(defun dismal-rfc- (row column) (dismal-evaluate-cellref (cons row column)))
(defun dismal-r-cf (row column) (dismal-evaluate-cellref (cons row column)))
(defun dismal-rfcf (row column) (dismal-evaluate-cellref (cons row column)))

(defun dismal-evaluate-cellref (addr)
  (let ((value (dismal-evaluate-cell (dis-address-row addr)
                                     (dis-address-col addr))))
    (if value value _f0)))


;;;
;;;	XIIa.	Insertion - of rows and columns.
;;;

(defun dismal-kill-line (arg)
  "Kill the rest of the current line;  [rest not implemented in dismal]
if no nonblanks there, kill thru newline.
With prefix argument, kill that many lines from point.
Negative arguments kill lines backward.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg."
  (interactive "P")
  (dismal-save-excursion
    (dismal-set-mark-command)
    (if (not arg)
        (progn (dismal-end-of-row)
               (dismal-kill-range))
       (error "Can't do that yet in dismal-kill-line."))))

(defun dismal-insert-range (arg)
  "Insert arg rows or cols of cells.  If mark=point, insert a cell.
If range is along a row, insert blank cells, moving other cells down.
If range is along a column, insert blank cells, moving other cells left.
If range is 2d, signal an error."
  (interactive "p")
  (dismal-save-excursion
    (dismal-select-range)
    (dismal-show-selected-range)
    (let ((start-row (range-1st-row dismal-cell-buffer))
          (start-col (range-1st-col dismal-cell-buffer))
          (end-row (range-2nd-row dismal-cell-buffer))
          (end-col (range-2nd-col dismal-cell-buffer))  )
    (dismal-insert-range-cells start-row start-col end-row end-col arg))))


;;; EMA changes  ;;; EMA changes  ;;; EMA changes  ;;; EMA changes  
;;; EMA changes  ;;; EMA changes  ;;; EMA changes  ;;; EMA changes  

;;; 3-7-93 - EMA - the next four functions fix a problem in which
;;; inserting and deleting rows throws off formulas.
;;; dismal-formula-cells, which contains the addresses of formulas
;;; with flexible references, was getting updated haphazardly.  the
;;; same sort of fix might be needed for columns.  changes are marked
;;; with "EMA".

;; 3-7-93 - EMA - didn't remove the call to dismal-change-row-references.
;;   see dismal-insert-range-cells.
(defun dismal-insert-row (nrow)
  "Insert NROW new rows, moving current row down."
  (interactive "p")
  (dismal-save-excursion
   (if dismal-interactive-p
       (message (if (= nrow 1) "Inserting %d row..." "Inserting %d rows...")
                nrow))
   ;; 3-7-93 - it seems necessary to do this first, rather
   ;; than in dismal-insert-range-cells:
   (dismal-change-row-references dismal-current-row nrow)
   (dismal-insert-range-cells dismal-current-row 0
                              dismal-current-row  dismal-max-col nrow)))

(defun dismal-insert-range-cells (start-row start-col end-row end-col arg)
  ;; We have to search the expressions for non-fixed column references
  ;; greater or equal to dismal-current-col and add ncol to them. 
  ;; (dismal-change-column-references start-row start-col end-row end-col arg)
  ;; (message "with %s %s %s %s" start-row start-col end-row end-col)
  ;; 3-7-93 - EMA - removed a call to dismal-change-column-references,
  ;;   because it duplicates the call in dismal-insert-row
  (let ((dismal-interactive-p nil)
        (cols-to-insert (1+ (- end-col start-col)) ))
    (dismal-jump-to-cell-quietly start-row  start-col)
    (cond ;; Insert just at a single spot
     ((and (= start-row end-row) (= start-col end-col))
      (setq dismal-max-row (+ dismal-max-row arg))
      (dismal-insert-cells arg (if (not (aref dismal-mark 0)) 'rows)))
     
        ;;; Special case: insert whole row
     ((and (= start-row end-row)
           (= start-col 0) (= end-col dismal-max-col))
      (setq dismal-max-row (+ dismal-max-row arg))
      (dismal-set-first-printed-column)
      (dismal-insert-blank-rows arg)
      (matrix-insert-nil-rows dismal-matrix start-row arg)
      (dismal-increment-ruler start-row arg)
      ;; 3-7-93 - EMA - this has already been done in dismal-insert-row:
      ;;(dismal-change-row-references dismal-current-row arg)
      )
     
        ;;; Insert partial row moving cells down
     ((= start-row end-row)
      (setq dismal-max-row (+ dismal-max-row arg))
      (while (<= start-col end-col)
        (dismal-jump-to-cell-quietly start-row start-col)
        (dismal-insert-column-cells-logical arg)
        (dismal-insert-column-cells-graphical arg)
        (setq start-col (1+ start-col)))
      ;; matrix will grow ncols*arg, hold to 1*arg amount
      (matrix-delete-rows dismal-matrix 
                          (1+ dismal-max-row) ;0 centered reference
                          (- (matrix-height dismal-matrix)
                             (1+ dismal-max-row)))
      (dismal-add-row-labels-at-end arg))
     
        ;;; Special case: insert whole column
     ((and (= start-col end-col) 
           (= start-row 0) (= end-row dismal-max-row))
      (setq dismal-max-col (+ dismal-max-col arg))
      (matrix-insert-nil-cols dismal-matrix start-col arg)
      (dismal-insert-blank-col arg)
      (dismal-change-column-references start-col arg)
      (vector-insert dismal-column-formats start-col arg)
      (while (<= start-row end-row)
        (if (or (and (>= start-col 1) ;avoid looking too far left
                     (consp (dismal-get-mrk start-row 
                                            (1- start-col)))
                     (not (dismal-get-exp start-row 
                                          (1- start-col))))
                (and (consp (dismal-get-mrk start-row 
                                            (1+ start-col)))
                     (not (dismal-get-exp start-row 
                                          (1+ start-col)))))
            (dismal-redraw-row start-row t))
        ;; should cleanup in here
        (setq start-row (1+ start-row))) )
     
     ;; Insert partial col moving cells left
     ((= start-col end-col)
      (setq dismal-max-col (+ dismal-max-col arg))
      (matrix-insert-nil-column-cells dismal-matrix start-row start-col 
                                      arg)
      (vector-insert dismal-column-formats start-col arg)
      (while (<= start-row end-row)
        (dismal-jump-to-cell-quietly start-row start-col)
        (dismal-insert-row-cells arg)
        (setq start-row (1+ start-row)))         )
     (t  (error "Must choose a row or col to do range insertion, not both.")))
    (dismal-erase-all-dependencies)
    (dismal-record-all-dependencies)))

(defun dismal-change-row-references (minrow number)
  ;; This function changes in all cells any non-fixed row 
  ;; references at or beyond MINROW by NUMBER.
  ;; 3-7-93 - EMA - added ADDR in call to *-reference,
  ;;   dis-addrs-to-update for it to push ADDR onto, and a mapcar to
  ;;   update the ADDRs in dismal-formula-cells.
  (let (dis-addrs-to-update)
    (vector-mapl (function (lambda (addr)
                             (let* ((r (dis-address-row addr))
                                    (c (dis-address-col addr))
                                    (cell (dismal-get-cell r c)) )
                               (dismal-set-cell-exp cell
                                                    (dismal-change-row-reference
                                                     (dismal-get-cell-exp cell)
                                                     minrow number addr)))))
                 dismal-formula-cells)
    (mapcar (function (lambda (old-new) ; have to watch for duplicates
                        (vector-remove dismal-formula-cells (car old-new))
                        (vector-push-unique dismal-formula-cells (car (cdr old-new)))))
            dis-addrs-to-update)))

(defun dismal-change-row-reference (expr minrow number addr)
  ;; This function changes the non-fixed row reference in the cell
  ;; EXPR to a row at or beyond MINROW by NUMBER.
  ;; 3-7-93 - EMA - added ADDR (an element of dismal-formula-cells)
  ;;   as a parameter, and added a push.
  (if expr
      (if (listp expr)
          (if (or (eq (car expr) 'dismal-r-c-)
                  (eq (car expr) 'dismal-r-cf))
              (let* ((row (car (cdr expr)))
                     (new-row (if (>= row minrow) (max 0 (+ row number)) row))
                     (r (car addr))
                     (new-r (if (>= r minrow) (max 0 (+ r number)) r))
                     (col (car (cdr (cdr expr)))))
                ;; (break)
                ;; 3-7-93 - EMA - push the old addr and the new addr
                ;; onto an update list, as (OLD . NEW):
                ;; dis-addrs-to-update is a bound variable at this point
                (push (list addr (cons new-r (cdr addr))) dis-addrs-to-update)
                ;;(setcar addr new-r)  ; this could generate duplicates
                (list (car expr) new-row col)) ; return value
            (cons (dismal-change-row-reference (car expr) minrow number addr)
                  (dismal-change-row-reference (cdr expr) minrow number addr)))
        expr)))


;;; EMA changes  ;;; EMA changes  ;;; EMA changes  ;;; EMA changes  end
;;; EMA changes  ;;; EMA changes  ;;; EMA changes  ;;; EMA changes  end

;; old FER version
;; (defun dismal-insert-row (nrow)
;;   "Insert NROW new rows, moving current row down."
;;   (interactive "p")
;;   (dismal-save-excursion
;;     (if dismal-interactive-p
;;         (message (if (= nrow 1) "Inserting %d row..." "Inserting %d rows...")
;;                  nrow))
;;     (dismal-change-row-references dismal-current-row nrow)
;;     (dismal-insert-range-cells dismal-current-row 0
;;                                dismal-current-row  dismal-max-col nrow)))


;; This function inserts the cells.  It is responsible for (a) updating 
;; max-col and row, (b) updating dependencies, (c) (not implemented yet)
;; updating row/col references

;; (dismal-insert-range-cells 0              dismal-current-col
;;                            dismal-max-row dismal-current-col 1)

;; (setq start-row 0)
;; (setq start-col dismal-current-col)
;; (setq end-row  dismal-max-row)
;; (setq end-col  dismal-current-col)

;; old FER version
;; (defun dismal-insert-range-cells (start-row start-col end-row end-col arg)
;;   ;; We have to search the expressions for non-fixed column references
;;   ;; greater or equal to dismal-current-col and add ncol to them. 
;;   ;; (dismal-change-column-references start-row start-col end-row end-col arg)
;;   ;; (message "with %s %s %s %s" start-row start-col end-row end-col)
;;   (let ((dismal-interactive-p nil)
;;         (cols-to-insert (1+ (- end-col start-col)) ))
;;   (dismal-jump-to-cell-quietly start-row  start-col)
;;   (cond ;; Insert just at a single spot
;;         ((and (= start-row end-row) (= start-col end-col))
;;          (setq dismal-max-row (+ dismal-max-row arg))
;;          (dismal-insert-cells arg))
;; 
;;         ;;; Special case: insert whole row
;;         ((and (= start-row end-row)
;;               (= start-col 0) (= end-col dismal-max-col))
;;          (setq dismal-max-row (+ dismal-max-row arg))
;;          (dismal-set-first-printed-column)
;;          (dismal-insert-blank-rows arg)
;;          (matrix-insert-nil-rows dismal-matrix start-row arg)
;;          (dismal-increment-ruler start-row arg)
;;          (dismal-change-row-references dismal-current-row arg))
;; 
;;         ;;; Insert partial row moving cells down
;;         ((= start-row end-row)
;;          (setq dismal-max-row (+ dismal-max-row arg))
;;          (while (<= start-col end-col)
;;            (dismal-jump-to-cell-quietly start-row start-col)
;;            (dismal-insert-column-cells-logical arg)
;;            (dismal-insert-column-cells-graphical arg)
;;            (setq start-col (1+ start-col)))
;;          ;; matrix will grow ncols*arg, hold to 1*arg amount
;;          (matrix-delete-rows dismal-matrix 
;;                              (1+ dismal-max-row) ;0 centered reference
;;                              (- (matrix-height dismal-matrix)
;;                                 (1+ dismal-max-row)))
;;          (dismal-add-row-labels-at-end arg))
;; 
;;         ;;; Special case: insert whole column
;;         ((and (= start-col end-col) 
;;               (= start-row 0) (= end-row dismal-max-row))
;;          (setq dismal-max-col (+ dismal-max-col arg))
;;          (matrix-insert-nil-cols dismal-matrix start-col arg)
;;          (dismal-insert-blank-col arg)
;;          (dismal-change-column-references start-col arg)
;;          (vector-insert dismal-column-formats start-col arg)
;;          (while (<= start-row end-row)
;;           (if (or (and (>= start-col 1) ;avoid looking too far left
;;                        (consp (dismal-get-mrk start-row 
;;                                               (1- start-col)))
;;                        (not (dismal-get-exp start-row 
;;                                             (1- start-col))))
;;                   (and (consp (dismal-get-mrk start-row 
;;                                               (1+ start-col)))
;;                        (not (dismal-get-exp start-row 
;;                                             (1+ start-col)))))
;;               (dismal-redraw-row start-row t))
;;            ;; should cleanup in here
;;            (setq start-row (1+ start-row))) )
;; 
;;         ;; Insert partial col moving cells left
;;         ((= start-col end-col)
;;          (setq dismal-max-col (+ dismal-max-col arg))
;;          (matrix-insert-nil-column-cells dismal-matrix start-row start-col 
;;                                          arg)
;;          (vector-insert dismal-column-formats start-col arg)
;;          (while (<= start-row end-row)
;;            (dismal-jump-to-cell-quietly start-row start-col)
;;            (dismal-insert-row-cells arg)
;;            (setq start-row (1+ start-row)))         )
;;       (t  (error "Must choose a row or col to do range insertion, not both.")))
;;   (dismal-erase-all-dependencies)
;;   (dismal-record-all-dependencies)))
;; 

(defun dismal-insert-column (ncol)
  "Insert NCOL new columns, moving current column to right."
  (interactive "p")
  (dismal-save-excursion
    (and dismal-interactive-p (message "Inserting %d column(s)..." ncol))
    (dismal-insert-range-cells 0                  dismal-current-col
                               dismal-max-row dismal-current-col ncol)
    (dismal-draw-column-labels)
    (dismal-make-ruler)
    (dismal-draw-ruler dismal-current-row)
    (dismal-display-current-cell-expr dismal-current-row dismal-current-col)))

;(defun dismal-insert-row (nrow)
;  "Insert NROW new rows, moving current row down."
;  (interactive "p")
;  (dismal-save-excursion
;  (message "Inserting %d row(s)..." nrow)
;  (dismal-change-row-references dismal-current-row nrow)
;  (setq dismal-max-row (+ dismal-max-row nrow))
;  (dismal-set-first-printed-column)
;  (matrix-insert-nil-rows dismal-matrix dismal-current-row nrow)
;  (dismal-erase-all-dependencies)
;  (dismal-record-all-dependencies)
;    (beginning-of-line)
;    (forward-line 1)
;    (open-line nrow)
;    (let (start saved-labels)
;      (forward-line 1)
;      (setq start (point))
;      (forward-line (- (- dismal-max-row nrow 1) dismal-current-row))
;      (forward-char dismal-first-printed-column)
;      (setq saved-labels (delete-extract-rectangle start (point)))
;      (goto-char start)
;      (forward-line (- nrow))
;      (insert-rectangle saved-labels)
;      ;; insert new lower lables here
;      (dismal-add-row-labels-at-end nrow)
;      ;; check for longer labels
;      )
;  (dismal-display-current-cell-expr dismal-current-row dismal-current-col)))


;; done after new max-row is set
(defun dismal-insert-blank-rows (nrow)
  ;; Insert NROW new rows, moving current row down.
  (dismal-save-excursion
    (beginning-of-line)
    (open-line nrow)
    (let (start saved-labels end)
      (forward-line 1)
      (setq start (point))
      (forward-line (- (- dismal-max-row nrow) dismal-current-row))
      (forward-char dismal-first-printed-column)
      (setq saved-labels (delete-extract-rectangle start (setq end (point))))
      (goto-char start)
      (forward-line (- nrow))
      (insert-rectangle saved-labels)
      ;; insert leading spaces
      (dismal-insert-blank-box end nrow dismal-first-printed-column " ")

      ;; insert new lower lables here
      (dismal-add-row-labels-at-end nrow)      )))

(defun dismal-insert-cells (arg &optional direction)
  "Insert some new cells into the spreadsheet."
  (interactive "p")
  (dismal-save-excursion
  (if (not direction)
      (setq direction (run-menu 'dismal-row-or-column-menu
                                 (dismal-range-is-rows-or-columns))))
  (if (eq direction 'rows)
      (progn (dismal-insert-column-cells-logical arg)
             (dismal-insert-column-cells-graphical arg)
             (dismal-add-row-labels-at-end arg))
    (dismal-insert-row-cells arg)  ))
  (dismal-display-current-cell-expr dismal-current-row dismal-current-col))

(defun dismal-insert-column-cells-logical (nrow)
  (matrix-insert-nil-column-cells dismal-matrix
        dismal-current-row dismal-current-col nrow))

(defun dismal-insert-column-cells-graphical (nrow)
  (dismal-save-excursion-quietly
  (let (cut-start saved-rect cc max-real-row)
    (forward-char (- 1 (dismal-column-width dismal-current-col)))
    (setq cut-start (point))
    (dismal-end-of-col-non-interactive)
    (setq max-real-row dismal-current-row)
    (if (not (> (point) cut-start))
        nil
      (forward-char 1)
      (setq saved-rect (delete-extract-rectangle cut-start (point)))
      (goto-char cut-start)
      (setq cc (current-column))
      (dismal-insert-blank-box (point) nrow
          (dismal-column-width dismal-current-col) " ")
      (forward-line nrow)
      (move-to-column cc)
      (insert-rectangle saved-rect)
      (goto-char cut-start))
  (dismal-set-first-printed-column) )))

;; this does not increment dismal-max-col or the dismal-column-formats, 
;; the caller must do so, because there may be many calls in a block
(defun dismal-insert-row-cells (ncol)
  (dismal-save-excursion-quietly
  (let ((old-mrk (dismal-get-mrk dismal-current-row dismal-current-col)))
    (matrix-insert-nil-row-cells dismal-matrix
          dismal-current-row dismal-current-col ncol)
    ;(dismal-erase-all-dependencies)  ;(dismal-record-all-dependencies)
    ;; insert some space
    (dismal-insert-blank-range dismal-current-row  dismal-current-col
             1 ncol nil)
    ;; cleanup the cell the dirtied you up
    (if (and (consp old-mrk) old-mrk)
        (dismal-cleanup-long-string (car old-mrk) (cdr old-mrk)))    )))

(defun dismal-cleanup-long-string (row col)
 ;; cleanup the mrks
 ;(my-short-message "cleaning up a long string")
 (let ((alignment (dismal-get-cell-alignment row col)))
   (cond ((eq  'right alignment)
          (dismal-cleanup-mrks row col -1))
         ((or (eq  'left alignment)
              (eq  'default alignment))
          (dismal-cleanup-mrks row col 1))
         ((eq  'center alignment)
          (dismal-cleanup-mrks row col -1)
          (dismal-cleanup-mrks row col 1)))
 ;; redraw
 (dismal-redraw-cell row col t)))

(defun dismal-cleanup-mrks (row col increment)
 (let ( (old-col col)
        (neighbor-mrk nil) (done nil) )
 (setq col (+ increment col))
 (while (and (not done) (>= col 0) (<= col dismal-max-col))
   (setq neighbor-mrk (dismal-get-mrk row col))
   ;(-message "cleaning up in %s %s" row col)
   (if (and neighbor-mrk
            (consp neighbor-mrk)
            (= (car neighbor-mrk) row)
            (= (cdr neighbor-mrk) old-col))
       (progn (dismal-set-mrk row col nil)
              (dismal-redraw-cell row col t))
     (setq done t))
   (setq col (+ increment col))  )))

(defun dismal-open-line (arg)
  "Insert a new row and leave point before it.
With arg, inserts that many newlines."
  (interactive "p")
  (let ((dismal-interactive-p nil))
    (dismal-forward-row 1))
  (dismal-insert-row arg) )

(defun dismal-insert-blank-col (ncols)
 (dismal-insert-blank-range 0 dismal-current-col 
             (+ 1 dismal-max-row) ncols nil))

(defun dismal-insert-blank-range (start-row start-col rows cols compute-width)
  ;; compute-width means insert blanks based on the actual col width
  (let ((i 0))
   (dismal-goto-cell start-row start-col nil)
   (while (< i rows)
     (forward-char (- 1 (dismal-column-width start-col)))
     (dismal-insert-n-times " "
        (if compute-width
            (dismal-sum-column-widths start-col cols)
          (* cols dismal-default-column-width)))
     (setq i (1+ i))
     (dismal-goto-cell (+ i start-row) start-col t))))


;;;
;;;	XIIb.	Deletion - of rows, columns & ranges
;;;

(defun dismal-erase-buffer ()
  ;; Delete the entire contents of the current dismal-buffer.
  ;; values are not saved, but matrix shell is.
  (dismal-beginning-of-buffer)
  (let ((row 0) (dismal-interactive-p nil))
    (while (<= row dismal-max-row)
      (dismal-end-of-row)
      (dismal-delete-cell (- (1+ dismal-current-col)) nil)
      (dismal-forward-row 1)
      (setq row (1+ row))    )  ))

(defun dismal-delete-blank-rows (start-row end-row)
 "Delete any blank rows from START-ROW to END-ROW."
 (interactive 
    (list (dismal-read-minibuffer "Delete blank rows starting at: " t
                   (format "%s" (min dismal-current-row (dismal-mark-row))))
          (dismal-read-minibuffer "Delete blank rows ending with: " t
                 (format "%s" (max dismal-current-row (dismal-mark-row))))))
 (setq start-row (max start-row 0)) ; a guard
 (dismal-save-excursion
 (while (> end-row start-row)
   (and dismal-interactive-p
        (message "Deleting blank rows (looking at %s on the way to %s)..."
                 end-row start-row))
   (let ((previous-interactive-p dismal-interactive-p)
         (dismal-interactive-p nil)
         block-start looking-for-block-end)
     ;; find next blank row
     (dismal-goto-row end-row nil)
     (setq dismal-current-row end-row)
     (dismal-end-of-row)
     (if (dismal-get-exp dismal-current-row dismal-current-col)
         nil
       (setq block-start end-row)
       (setq looking-for-block-end t)
       ;; find how far it goes back
       (while (and (>= end-row start-row) looking-for-block-end
                   (>= end-row 1))
         (setq end-row (1- end-row))
         (setq dismal-current-row end-row)
         (message "Deleting blank rows (blank at %s on the way to %s)..."
                  end-row start-row)
         (dismal-end-of-row)
         (if (dismal-get-exp dismal-current-row dismal-current-col)
             (setq looking-for-block-end nil)))
       (setq end-row (1+ end-row))
       ;; go there
       (dismal-goto-row end-row nil)
       (setq dismal-current-row end-row)
       ;; delete row(s)
       (and previous-interactive-p
            (message "Deleting block of %s blank row(s) starting at row %s..."
                      (1+ (- block-start end-row)) end-row))
       (dismal-delete-row (1+ (- block-start end-row))))
   (setq end-row (1- end-row))))
  (and dismal-interactive-p 
       (message "Deleting blank rows %s down to %s...Done" end-row start-row))))

(defun dismal-delete-range (direction)
  "Delete a the current range of cells.  If mark=point, delete just a cell.
If direction is rows, move cells up to fill.
If direction is columns, move cells left to fill."
  (interactive (list (run-menu 'dismal-row-or-column-menu
                               (dismal-range-is-rows-or-columns))))
  (dismal-save-excursion
  (dismal-select-range)
  (dismal-note-selected-range "Deleting %s%s:%s%d...")
  (let ((start-row (range-1st-row dismal-cell-buffer))
        (start-col (range-1st-col dismal-cell-buffer))
        (end-row (range-2nd-row dismal-cell-buffer))
        (end-col (range-2nd-col dismal-cell-buffer))
        (dismal-interactive-p nil)
        (dismal-show-selected-ranges nil))
  (dismal-delete-range-cells start-row start-col end-row end-col direction))))

;(dismal-delete-range-cells dismal-current-row 0
;                       (+ (1- nrow) dismal-current-row) dismal-max-col 'rows)

(defun dismal-delete-range-cells (start-row start-col 
                                  end-row end-col direction)
  (cond ;; special case: delete whole row
        ((and (= start-col 0) (= end-col dismal-max-col))
         (dismal-delete-column-cells start-row start-col end-row end-col)
         (dismal-increment-ruler start-row (- (1+ (- end-row start-row)))))

        ;; remove cells moving up
        ((eq direction 'rows) 
         (dismal-delete-column-cells start-row start-col end-row end-col))

        ;; special case: delete whole column
        ;; no duplication with dismal-delete-column, this is delete/clearing
        ;; not killing function
        ((and (= start-row 0) (= end-row dismal-max-row))
         (while (<= start-row end-row)
           (dismal-jump-to-cell-quietly start-row start-col)
           (dismal-delete-row-cells (1+ (- end-col start-col)))
           (setq start-row (1+ start-row))))

        ((eq direction 'columns)  ;; remove cells moving left
         (while (<= start-row end-row)
           (dismal-jump-to-cell-quietly start-row start-col)
           (dismal-delete-row-cells (1+ (- end-col start-col)))
           (setq start-row (1+ start-row)))   )
        (t  (error "Must choose row or col to do range insertion, not %s."
                    direction)))
  ;; have to have cell references update here too...
  (dismal-erase-all-dependencies)
  (dismal-record-all-dependencies)
)

(defun dismal-delete-column-cells (start-row start-col end-row end-col)
  (dismal-save-excursion
  (let (cut-start cut2-start saved-rect cc i (nrow (- end-row start-row)))
  ;; delete the dead rectangle; move up the live; put filler in.
    (dismal-jump-to-cell-quietly start-row start-col)
    (forward-char (- 1 (dismal-column-width dismal-current-col)))
    (setq cut-start (point))
    (setq cc (current-column))
    (dismal-jump-to-cell-quietly end-row end-col)
    (forward-char 1)
    (delete-extract-rectangle cut-start (point))
    (dismal-jump-to-cell-quietly end-row start-col)
    (dismal-forward-row 1)
    (forward-char (- 1 (dismal-column-width dismal-current-col)))
    (setq cut2-start (point))
    (dismal-visit-cell dismal-max-row end-col)
    (forward-char 1) 
    (setq saved-rect (delete-extract-rectangle cut2-start (point)))
    (goto-char cut-start)
    (insert-rectangle saved-rect)
    (forward-line 1)    (move-to-column cc)
    (dismal-insert-blank-box (point) (- dismal-max-row end-row)
          (dismal-column-width dismal-current-col) " ")
  ;; cleanup the matrix
  (setq i 0)
  (if (and (= start-col 0) (= end-col dismal-max-col))
      (matrix-delete-rows dismal-matrix start-row (1+ nrow))
    (while (<= (+ start-col i) end-col)
      (matrix-delete-column-cells dismal-matrix
            start-row (+ i start-col) (1+ nrow))
      (setq i (1+ i)))
    (matrix-funcall-rc
          (function (lambda (r c dummy) (dismal-cleanup-long-string r c)))
           start-row (max 0 (1- start-col))
           end-row (min dismal-max-col (1+ end-col)) dismal-matrix)))))

(defun dismal-delete-row-cells (ncol)
  (dismal-save-excursion
    (let (cut-start saved-rect cc)
    (matrix-delete-row-cells dismal-matrix
          dismal-current-row dismal-current-col ncol)
    (dismal-redraw-row dismal-current-row t)
    (dismal-display-current-cell-expr dismal-current-row dismal-current-col))))

;; doesn't redraw the changed cells if any
(defun dismal-delete-column (ncol)
  "Delete NCOL columns starting with the current column and moving right."
  (interactive "p")
  (let (del-start
        (dismal-interactive-p nil))
    (dismal-save-excursion
    (if (> (+ (1- ncol) dismal-current-col) dismal-max-col) ;you want to cut too much
        (progn (setq ncol (1+ (- dismal-max-col dismal-current-col)))
               (message (if (= ncol 1) "Can only delete %d column..."
                                       "Can only delete %d columns...")
                         ncol))
      (message (if (= ncol 1) "Deleting %d column..."
                              "Deleting %d columns...")
               ncol))
    (dismal-change-column-references dismal-current-col (- ncol))
    (matrix-delete-cols dismal-matrix dismal-current-col ncol)
    (setq dismal-max-col (- dismal-max-col ncol))
    (set-buffer-modified-p t)
      (dismal-goto-cell -2 dismal-current-col nil)
      (forward-char (- 1 (dismal-column-width dismal-current-col)))
      (setq del-start (point))
      (dismal-goto-cell dismal-max-row (+ (1- ncol)
                                              dismal-current-col) nil)
      (forward-char 1)
      (kill-rectangle del-start (point))
    (vector-delete dismal-column-formats dismal-current-col ncol)
    (dismal-draw-column-labels)
    (dismal-make-ruler)
    (dismal-draw-ruler dismal-current-row)))
    (dismal-display-current-cell-expr dismal-current-row dismal-current-col))

(defun dismal-delete-row (nrow)
  "Delete NROW rows, moving remaining rows up."
  (interactive "p")
  (if dismal-interactive-p (message "Deleting %d row(s)..." nrow))
  (dismal-save-excursion
    (let ((dismal-interactive-p nil))
    ;; don't delete more rows than you have
    (if (> dismal-current-row 0)
        (if (> (+ dismal-current-row nrow -1) dismal-max-row)
            (setq nrow (- dismal-max-row dismal-current-row)))
      (if (> (+ dismal-current-row nrow) dismal-max-row)
          (setq nrow (- dismal-max-row dismal-current-row))) )
      ;; (my-message "Delete-row: done with endtest.") ;1
    (dismal-change-row-references dismal-current-row (- nrow))
      ;; (my-message "Delete-row: done with  change-row-references.") ;6
    (dismal-delete-range-cells dismal-current-row 0
                       (+ (1- nrow) dismal-current-row) dismal-max-col 'rows)
      ;; (my-message "Delete-row: done with  delete-range-cells.") ;20
    (dismal-remove-row-labels-at-end nrow)
      ;; (my-message "Delete-row: done with  remove-row-labels-at-end.") ;22
    (setq dismal-max-row (max 0 (- dismal-max-row nrow)))
    (dismal-set-first-printed-column))))

; (dismal-delete-range-cells dismal-current-row 0 dismal-current-row dismal-max-col 'rows)

;;;
;;;	XIIb.	Insertion and Deletion - Cell reference updating
;;;
;;;  When rows or columns are inserted or deleted the cell references 
;;;  must be changed so non-fixed references still refer to the same cell 
;;;  in its new location.

(defun dismal-change-indices (expr numrow numcol)
  ;; Return a version of EXPR moved by NUMROW rows and NUMCOL columns.
  (dismal-change-column-reference 
     (dismal-change-row-reference-expr expr 0 numrow)
     0 numcol))

(defun dismal-change-column-references (mincol number)
  ;; This function changes any non-fixed column references in the cell
  ;; matrix to columns at or beyond MINCOL by NUMBER.
  (matrix-mapl
   (function (lambda (cell)
      (dismal-set-cell-exp cell
                           (dismal-change-column-reference 
                              (dismal-get-cell-exp cell) 
                               mincol number))))
   dismal-matrix))

;; old FER version
;; (defun dismal-change-row-references (minrow number)
;;   ;; This function changes in all cells any non-fixed row 
;;   ;; references at or beyond MINROW by NUMBER.
;;   (vector-mapl (function (lambda (addr)
;;                    (let* ((r (dis-address-row addr))
;;                           (c (dis-address-col addr))
;;                           (cell (dismal-get-cell r c)) )
;;                       (dismal-set-cell-exp cell
;;                             (dismal-change-row-reference
;;                                 (dismal-get-cell-exp cell)
;;                                 minrow number)))))
;;                dismal-formula-cells))
;;   (matrix-mapl
;;    '(lambda (cell)
;;       (dismal-set-cell-exp cell
;;                            (dismal-change-row-reference
;;                                (dismal-get-cell-exp cell)
;;                                minrow number)))
;;   dismal-matrix)


(defun dismal-change-column-reference (expr mincol number)
  ;; This function changes the non-fixed column reference in the cell
  ;; EXPR to a column at or beyond MINCOL by NUMBER.
  (if (null expr)
      ()
    (if (listp expr)
        (if (or (eq (car expr) 'dismal-r-c-)
                (eq (car expr) 'dismal-rfc-))
            (let ((col (car (cdr (cdr expr)))))
              (list (car expr)
                    (car (cdr expr))
                    (if (>= col mincol) (max 0 (+ col number)) col)))
          (cons (dismal-change-column-reference (car expr) mincol number)
                (dismal-change-column-reference (cdr expr) mincol number)))
      expr)))

;; old FER version
;; (defun dismal-change-row-reference (expr minrow number)
;;   ;; This function changes the non-fixed row reference in the cell
;;   ;; EXPR to a row at or beyond MINROW by NUMBER.
;;   (if expr
;;      (if (listp expr)
;;          (if (or (eq (car expr) 'dismal-r-c-)
;;                  (eq (car expr) 'dismal-r-cf))
;;              (let ((row (car (cdr expr))))
;;                (list (car expr)
;;                      (if (>= row minrow) (max 0 (+ row number)) row)
;;                      (car (cdr (cdr expr)))))
;;            (cons (dismal-change-row-reference (car expr) minrow number)
;;                  (dismal-change-row-reference (cdr expr) minrow number)))
;;           expr)))

(defun dismal-change-row-reference-expr (expr minrow number)
  ;; This function changes the non-fixed row references in the EXPR 
  ;; to a row at or beyond MINROW by NUMBER.
  (if expr
     (if (listp expr)
         (if (or (eq (car expr) 'dismal-r-c-)
                 (eq (car expr) 'dismal-r-cf))
             (let ((row (car (cdr expr))))
               (list (car expr)
                     (if (>= row minrow) (max 0 (+ row number)) row)
                     (car (cdr (cdr expr)))))
           (cons (dismal-change-row-reference-expr (car expr) minrow number)
                 (dismal-change-row-reference-expr (cdr expr) minrow number)))
          expr)))


;;;
;;;	XIII.	Cell dependencies
;;;

(defun dismal-possible-live-sexp (sexp)
   (and sexp
        (listp sexp)        ; a list, not a number or string
        (not (floatp sexp)) ; not a float
        (listp (cdr sexp))  ; not a cons cell
   ))
;      (or (null sexp)         ;; up and out immediately if these types,
;          (floatp sexp)
;          (not (listp sexp))) ;; b/c they have nothing to do


;; also cleaned up like erase-dependencies....2-Mar-92 -FER
(defun dismal-record-dependencies (row col sexp)
  ;; Inform cells (by recording the fact in their dep field) that ROW COL 
  ;; holds a SEXP that refers to them, so that if they change the cell in 
  ;; ROW COL can be recalculated.  If sexp ends up referencing a cell, 
  ;; then put it on the dismal-formula-cells vector.
  (if (not (dismal-possible-live-sexp sexp)) ; can be called recursively
      ()
    (let ((depaddr (dis-make-address row col)))
    (if (rangep sexp)
        (progn
          (vector-push-unique dismal-formula-cells depaddr)
          (dismal-do (function (lambda (row2 col2 dummy)
                         (dismal-set-deps row2 col2
                                          (cons depaddr
                                                (dismal-get-deps row2 col2)))))
                      sexp nil))
      (if (dis-cellp sexp)
          (let ((drow (dis-cell-row sexp)) 
                (dcol (dis-cell-col sexp)))
            (vector-push-unique dismal-formula-cells depaddr)
            (dismal-set-deps drow dcol
                   (cons depaddr (dismal-get-deps drow dcol))))
        ;; else recurse
        (dismal-record-dependencies row col (car sexp))
        (dismal-record-dependencies row col (cdr sexp)))))))


(defun dismal-erase-dependencies (row col sexp)
  ;; Remove any dependencies implied by the cell at DEPADDR whose
  ;; definition is SEXP.
  (if (not (dismal-possible-live-sexp sexp))
      (vector-remove dismal-formula-cells (cons row col))
    (if (listp sexp)
        (if (eq (car sexp) 'dismal-range)
            (dismal-do (function (lambda (row2 col2 dummy)
                            (dismal-set-deps row2 col2
                                 (dismal-del (dis-make-address row col)
                                             (dismal-get-deps row2 col2)))))
                        sexp nil)
          (if (memq (car sexp) dismal-cell-types)
              (dismal-set-deps row col (dismal-del (dis-make-address row col)
                                                   (dismal-get-deps row col)))
            (dismal-erase-dependencies row col (car sexp))
            (dismal-erase-dependencies row col (cdr sexp)))))))

;; 2-Mar-92 -FER slightly cluncky version
; (defun dismal-erase-dependencies (depaddr sexp)
;   "Remove any dependencies implied by the cell at DEPADDR whose
; definition is SEXP."
;   (if (null sexp)
;       ()
;     (if (listp sexp)
;         (if (eq (car sexp) 'dismal-range)
;             (let ((cells (dismal-generate-range-list (nth 1 (nth 1 sexp))
;                                                      (nth 1 (nth 2 sexp)))))
;               (dismal-map-apply '(lambda (addr)
;                        (dismal-set-deps addr
;                                         (dismal-del depaddr
;                                             (dismal-get-deps addr)))) cells))
;           (if (memq (car sexp) '(dismal-r-c- dismal-rfc-
;                                              dismal-r-cf dismal-rfcf))
;               (let ((addr (list (nth 1 sexp) (nth 2 sexp))))
;           (dismal-set-deps addr (dismal-del depaddr (dismal-get-deps addr))))
;             (dismal-erase-dependencies depaddr (car sexp))
;            (dismal-erase-dependencies depaddr (cdr sexp)))))))

(defun dismal-erase-all-dependencies ()
  ;(message "In dismal-erase-all-dependencies")
  (matrix-mapl (function (lambda (cell) (dismal-set-cell-dep cell nil))) dismal-matrix))

(defun dismal-record-all-dependencies ()
  ;(message "In dismal-record-all-dependencies")
  (vector-mapl (function (lambda (addr)
                   (let ((r (dis-address-row addr))
                         (c (dis-address-col addr)))
                     (dismal-record-dependencies r c (dismal-get-exp r c)))))
               dismal-formula-cells))
  ;; (matrix-map-rc '(lambda (addr cell)
  ;;                  (dismal-record-dependencies (first addr) (second addr)
  ;;                                              (dismal-get-cell-exp cell)))
  ;;               dismal-matrix)


;;;
;;;	XIVa.	File I/O - reading and writing
;;;

(defun dismal-write-buffer (filename)
  ;; Save the current spreadsheet in file FILENAME.
  (save-excursion
    (let ((real-buffer (current-buffer))
          (mode-name-to-write mode-name) ;; might be spa or such
          (backup-file-name (concat filename "~"))
          (require-final-newline nil) )
    (if (file-exists-p filename) (rename-file filename backup-file-name t))
    (set-buffer (get-buffer-create "*Dismal-saving-buffer*"))
    (erase-buffer)
    (buffer-flush-undo (current-buffer))
    (dismal-file-header mode-name-to-write)
    (insert "\n")
    (mapc (function (lambda (x)
             (let ((real-x (save-excursion (set-buffer real-buffer)
                                           (eval x))))
             (insert "(setq " (prin1-to-string x) " '"
                     (prin1-to-string real-x) ")\n"))))
          dismal-saved-variables)
    (write-file filename)
    (setq dismal-auto-save-counter dismal-auto-save-interval)
    (kill-buffer (current-buffer))))
  (setq buffer-file-name filename)
  (clear-visited-file-modtime))

(defun dismal-file-header (mode-name-to-write)
  (insert ";; -*- Mode: " mode-name-to-write " -*-")
  (insert "\n;; This file was produced for " (getenv "USER")
          " by dismal-mode (Vers " dismal-version ")"
          "\n;; This file written ")
  (insert-current-time-string)
  (insert "\n;; dismal-mode Copyright 1992, Fox & Ritter."
          "\n;; No user serviciable parts, but it's your data.\n"))

(defun dismal-save-file ()
  "Save the current spreadsheet."
  (interactive)
  (if (not (buffer-modified-p))
      (message "(No dismal changes need to be saved.)")
   (message "Saving %s..." buffer-file-name)
   (dismal-write-buffer buffer-file-name)
   (if (file-exists-p dismal-buffer-auto-save-file-name)
       (delete-file dismal-buffer-auto-save-file-name))
   (set-buffer-modified-p nil)
   (message "Wrote %s" buffer-file-name)))

(defun dismal-write-file (filename)
  "Save the current spreadsheet."
  (interactive "FSave to file: ")
  (message "Saving %s..." filename)
  (dismal-write-buffer filename)
  (if (equal (file-name-nondirectory filename) (buffer-name))
      nil
    (rename-buffer (file-name-nondirectory filename)))
  (setq default-directory (file-name-directory filename))
  (setq dismal-buffer-auto-save-file-name (make-auto-save-file-name))
  (set-buffer-modified-p nil)
  (message "Wrote %s" filename))

(defun dismal-do-auto-save ()
  (message "Auto-saving %s ..." (buffer-name)) (sit-for 2)
  (setq dismal-auto-save-counter dismal-auto-save-interval)
  (if (buffer-modified-p)
      (let ((old-buffer-file-name buffer-file-name))
        (dismal-write-buffer dismal-buffer-auto-save-file-name)
        (setq buffer-file-name old-buffer-file-name))))

;; 2-16-93 -EMA fix: "save-some-buffers" clobbers dismal file format, so
;; need a dismal-save-some-buffers.  ideally it would be more
;; sophisticated than this.

;; (defun dismal-save-some-buffers (&optional arg exiting)
;;  "Dings."
;;  (interactive "P")
;;  (message "Not implemented.  Use dismal-save-file.")
;;  (ding))

;; here may be Erik's fix:
(defun dismal-write-file-hook ()
   (if (or dismal-matrix (eq mode-name "dismal"))
       (dismal-save-file)))

;; Written to run in 18 & 19
(setq write-file-hooks
      (cons 'dismal-write-file-hook
            write-file-hooks))

(defun dismal-dump-range (filename &optional formulas-p)
  "Dump the current range to a tabbed file.  If FORMULAS-P is t, then write out
formulas as s-expressions.  Writes an extra tab if last field is empty for S."
  (interactive "FSave to file:")
  (if (file-exists-p filename)
      (error "%s already exists" filename))
  (if (interactive-p)
      (setq formulas-p (y-or-n-p "Write out a formula as a formula? ")))
  (dismal-select-range)
  (dismal-note-selected-range (format "Dumping %%s%%s:%%s%%d to %s" 
                                      (file-name-nondirectory filename)))
  (sit-for 1)
  (dismal-save-excursion
  (let ((start-row (range-1st-row dismal-cell-buffer))
        (start-col (range-1st-col dismal-cell-buffer))
        (end-row (range-2nd-row dismal-cell-buffer))
        (end-col (range-2nd-col dismal-cell-buffer))
        (dump-buffer (find-file-noselect filename))
        (old-buffer (current-buffer))
        (dm dismal-matrix)
        (dcf dismal-column-formats)
        (dismal-show-selected-ranges nil))
  (set-buffer dump-buffer)
  (let ((dismal-matrix dm)
        (dismal-column-formats dcf))
  (message "Dumping range...")
  (matrix-funcall-rc
     (function (lambda (r c cell)
        ;; (my-message "formulas-p is %s, exp is: %s" formulas-p
        ;;            (dismal-get-exp r c))
        (let* ((format (dismal-get-column-format c))
               (expression (dismal-get-cell-exp cell))
               (string-value (dismal-flat-format
                                (if (and formulas-p
                                         expression
                                         (formula-p expression))
                                    (dismal-get-cell-exp cell)
                                    (dismal-evaluate-cell r c))
                                (aref format 1))))
          (cond ((stringp string-value) (insert string-value))
                (string-value (insert (format "%s" string-value)))
                ;; insert a tab if at the end with no value for S
                ((= c end-col) (insert "\t")))
          (cond ((= c end-col) (insert "\n"))
                (t (insert "\t")))  )))
     start-row start-col end-row end-col dm))
  (write-file filename)
  (set-buffer old-buffer)
  (message "Range dumped to %s" filename))))
;; lowpost
;; (setq aaa
;;     (dismal-flat-format (dismal-get-exp 0 4) (dismal-get-column-format 0)))


;;;
;;;    XIVb.	File I/O - Translation functions between Excel and Forms
;;;

(defun dismal-insert-file (filename)
  "Insert contents of file FILENAME into buffer starting at the current cell.  
Fields (cells) are seperated by dismal-field-sep.
Cells are overwritten rather than pushed down.
Set mark after the inserted text."
  (interactive "FDismal Insert file: ")
  (if (get-file-buffer filename)
      (error "Insert file requires that the file not already be a buffer."))
  (let ((read-col dismal-current-col)
        (read-row dismal-current-row)
        last-read-col
        (dismal-interactive-p nil)
        (original-buffer (current-buffer)) )
  (save-excursion
    (find-file filename)
    (goto-char (point-min))
    (while (not (eobp))
       (message "Reading into row %d..." read-row)
       (setq last-read-col (dismal-read-row original-buffer read-row read-col))
       (setq read-row (+ 1 read-row))
       (forward-line 1))
    (kill-buffer (current-buffer)))
  (if dismal-auto-update 
      (progn 
         (message "Updating matrix...")
         (dismal-private-update-matrix)
         (message "Updating matrix...Finished.")))
  (dismal-set-mark (1- read-row) last-read-col))
  (dismal-visit-cell dismal-current-row dismal-current-col))

(defun dismal-read-row (original-buffer read-row read-col)
  ;; returns how far it got
  (let ((eol (save-excursion (end-of-line) (point)))
        (new-item nil)  (done nil)
        (start (point))  (end nil)   )
  (while (not done)
    (setq end (if (search-forward dismal-field-sep eol t)
                  (point)
                (setq done t)
                eol))
    ;;this strips leading blanks, which is hard on strings
    ;;(setq new-item(string-trim dismal-blank-bag(buffer-substring start end)))
    (setq new-item
          (buffer-substring start (if done end (1- end)))) ;don't read tabs
    (setq start end)  ;set up for next item
    (if (string= new-item "") (setq new-item nil))
    (save-excursion (set-buffer original-buffer)
      (dismal-set-cell read-row read-col
                      (dismal-convert-input-to-cellexpr new-item)
                      nil))
    (setq read-col (+ 1 read-col))   )
  (1- read-col)))

(defun dismal-insert-tabs ()
  (interactive)
  (while (not (eobp))
    (if (y-or-n-p "Tab this field?")
        (progn (forward-word 1) 
               (insert "\t")))
    (forward-line 1)))


;;;
;;;	XIVc.	File I/O - Report functions
;;;

(defvar dismal-report-display-buffer nil "Where dismal reports are dumped.")

(defun dismal-make-print-file-name (file-name buffer-name)
  (concat (file-name-directory file-name)
          (concat (substring buffer-name 0 
                             (string-match ".[^.]*$" buffer-name))
          ".dp")))

(defun dismal-print-report ()
 "Print out a copy of the current dismal sheet."
 ;; .dp stands for dismal printout
 (interactive)
 (if (not dismal-print-command)
     (error "You must first set up to print.")
 (save-excursion
 (save-window-excursion
 (let* ((funny-file-name (dismal-make-print-file-name buffer-file-name 
                                                      (buffer-name)))
        (dismal-interactive-p nil)
        (print-out-buffer (get-buffer-create funny-file-name)))
   (dismal-make-report print-out-buffer)
   (sit-for 0)
   (message "Printing...")
   (shell-command (format "%s %s" dismal-print-command funny-file-name))
   (kill-buffer print-out-buffer)
   (message "Printing...Done"))))))

(defun dismal-make-report (&optional rbuffer)
  "Print to RBUFFER a plain file all the visible cols of all the visible 
rows.  Must be called from a dismal buffer."
  (interactive)
  ;; set variables you need to use while in other buffer
  (let ((current-buffer (current-buffer))
        (current-buffer-file-name buffer-file-name)
        (current-buffer-name (buffer-name))
        (page-size dismal-page-size)
        (ruler dismal-ruler)
        (report-buffer (or rbuffer
                           (if dismal-report-display-buffer
                              dismal-report-display-buffer)
                           (get-buffer-create "*Dismal-Report*"))))
    (dismal-undraw-ruler-rows)
    (pop-to-buffer report-buffer)
    (if (not buffer-file-name)
        (setq buffer-file-name
              (dismal-make-print-file-name current-buffer-file-name
                                           current-buffer-name)))
    (setq truncate-lines t)
    (erase-buffer)
    (dismal-report-header current-buffer-file-name)
    (insert-buffer current-buffer)
    (pop-to-buffer current-buffer)
    (dismal-draw-ruler dismal-current-row)
    (pop-to-buffer report-buffer)
    (dismal-insert-report-rulers page-size ruler)
    (set-buffer-modified-p nil)
    (goto-char (point-max))
    (insert "\n")
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (write-file buffer-file-name)
    ;; (and dismal-interactive-p 
    ;;      (message "You must type C-x C-w to save this file."))
    ;; (sit-for 3)
    ))

(defun dismal-insert-report-rulers (page-size ruler)
  (goto-char (point-min))
  ;; no ruler on first page
  (forward-line  page-size)  ; you are starting on line 1
  (if (not (eobp)) (insert "" ruler "\n"))
  (while (not (eobp))
    (forward-line (- page-size 2)) ; 2 is size of ruler
    (if (not (eobp)) (insert "" ruler "\n"))))

(defun dismal-report-header (forms-file)
  (insert-current-time-string)
  (insert " - Dismal (" dismal-version ") report for user ")
  (insert (getenv "USER"))
  (insert "\nFor file " forms-file "\n\n")
  (insert (format "To print use  \"%s\"\n"
                  (format "%s %s" dismal-print-command buffer-file-name)))
  (insert "-------------------------------------------------------------\n\n"))

(defun dismal-print-setup ()
  (interactive)
  (call-interactively 'dismal-set-ruler)
  (let ((old-dismal-page-size dismal-page-size))
  (setq dismal-page-size (dismal-read-minibuffer "Printed page size: " t
                                    (prin1-to-string dismal-page-size)) )
  (if (not (= old-dismal-page-size dismal-page-size))
      (set-buffer-modified-p t))
  ;; add 2: for the ruler lines
  (setq dismal-print-command (format dismal-raw-print-command
                                     (+ 2 dismal-page-size)))))
(defun dismal-clean-printout ()
  "Strip the leading digits and header information."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (delete-region (point) (save-excursion (forward-line 8) (point)))
    ;; point of 4 is two digit numbe, could be smarter
    (kill-rectangle 4 (point-max))
    ;; now remove trailing whitespace
    (goto-char (point-min))
    (while (not (eobp))
       (end-of-line)
       (just-one-space)
       (forward-char -1)
       (delete-char 1)
       (forward-line 1))))

;; 2/93 EMA
(defun dismal-unpaginate ()
  "Unpaginates a dismal report.  Call from within the report buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ".*\n.*\n" nil t)
      (replace-match ""))))

;;;
;;;	XV.	Redrawing the screen
;;;

(defun dismal-redraw-range (&optional min-row max-row)
  "Redraw the current range between point and mark."
  (interactive)
  (dismal-save-excursion
  (let ((min-row (or min-row (min (dismal-mark-row) dismal-current-row)))
        (max-row (or max-row (max (dismal-mark-row) dismal-current-row))))
  (while (<= min-row max-row)
    (if dismal-interactive-p (message "Redrawing range, line %s" min-row))
    (dismal-jump-to-cell-quietly min-row 0)
    (dismal-hard-redraw-row-non-interactive)
    (setq min-row (1+ min-row))) )))

(defun dismal-quoted-insert ()
  "Insert a quoted char only after querying the user, insertion may 
mess up the display so that it is unparsable."
  (interactive)
  (if (y-or-n-p "Are you sure you want to insert a raw character? ")
      (progn (message "Character to insert: ")
             (call-interactively 'quoted-insert))))

(defun dismal-recenter (arg)
  "Center row in window and redisplay screen.  With ARG, put point on line ARG.
The desired position of point is always relative to the current window.
Just C-u as prefix means put point in the center of the screen.
No arg (i.e., it is nil) erases the entire screen and then
redraws with point in the center.  Adjusts somewhat for rulers."
  (interactive "P")
  (if (and dismal-show-ruler
           (numberp dismal-current-first-ruler-row)
           (numberp arg))
      (setq arg (+ arg 2)))
  (dismal-undraw-ruler-rows)
  (recenter arg)
  (dismal-forward-row 0)
  (dismal-draw-ruler dismal-current-row))

(defun dismal-display-current-cell-expr (row column)
  (let ((cell-name (dismal-cell-name row column)))
  (setq dismal-current-cell cell-name)
  (if dismal-interactive-p
      (message (concat cell-name
               ": "
               (dismal-convert-cellexpr-to-string
                  (dismal-get-exp row column))
;; The rest of this function is for debugging
;           ", val: "
;           (dismal-convert-cellexpr-to-string
;            (dismal-get-val dismal-current-row dismal-current-col))
;           ", dep: "
;           (prin1-to-string
;            (dismal-get-deps (list dismal-current-row dismal-current-col)))
;           ", rct: "
;           (prin1-to-string
;            (dismal-get-mrk dismal-current-row dismal-current-col))
            )))))

(defun dismal-redraw (hard-redraw)
  "Redraw all the cells in the spreadsheet. If HARD-REDRAW, clear lines first."
  (interactive "P")
  (if (and (interactive-p)
           (not hard-redraw))
      (setq hard-redraw
            (y-or-n-p "Do hard redraw (y), or fast(n)? (y/n) ")))
  (message "Redrawing spreadsheet...")
  ;; if cleanup worked right, this could go.
  (matrix-funcall-rc
          (function (lambda (r c dummy)
             (let ((mrk (dismal-get-mrk r c)))
             (if (and mrk (consp mrk))
                 (dismal-set-mrk r c nil)))))
           0 0 dismal-max-row dismal-max-col dismal-matrix)
  (let ((buffer-originally-clean (not (buffer-modified-p))))
  (dismal-save-excursion (erase-buffer)
    (dismal-draw-labels)
    (let ((rowno 0)
          (nrow dismal-max-row))
      (while (<= rowno nrow)
         (dismal-redraw-row rowno hard-redraw)
         (setq rowno (1+ rowno)))))
  (dismal-make-ruler)
  (dismal-draw-ruler dismal-current-row)
  (if buffer-originally-clean (set-buffer-modified-p nil))
  (message "Redrawing spreadsheet...done")))

(defun dismal-redraw-column (column)
  ;; Redraw all the cells in a column of the spreadsheet.
  (save-excursion
    (dismal-draw-column-label column)
    (let* ((rowno 0))
      (while (< rowno dismal-max-row)
        (progn (dismal-redraw-cell rowno column t)
          (setq rowno (1+ rowno)))))))

(defun dismal-hard-redraw-row-non-interactive ()
  (beginning-of-line)
  (delete-region (point) (save-excursion (end-of-line) (point)))
  (dismal-redraw-row dismal-current-row t))

(defun dismal-hard-redraw-row (number-of-rows)
  "Redraw the current row."
  (interactive "p")
  (let ((buffer-originally-clean (not (buffer-modified-p))))
    (if (> number-of-rows 1)
        (progn 
          (dismal-hard-redraw-row 1)
          (dismal-hard-redraw-row (1- number-of-rows)))
      (dismal-save-excursion
        (beginning-of-line)
        (delete-region (point) (save-excursion (end-of-line) (point)))
        (dismal-redraw-row dismal-current-row t))
      (dismal-forward-row 1))
    (if buffer-originally-clean (set-buffer-modified-p nil))))

(defun dismal-redraw-row (rowno reset-marks)
  (dismal-draw-row-label rowno)
  (let* ((row (vector-ref dismal-matrix rowno))
         (colno 0)
         (ncol (if row (max dismal-max-col (vector-length row)) 0)))
    (if dismal-interactive-p
        (message "Redrawing row %s of %s" rowno dismal-max-row))
    (if reset-marks
       (while (< colno ncol)
          (if (and (not (dismal-get-exp rowno colno))
                   (dismal-get-mrk rowno colno))
              (dismal-set-mrk rowno colno nil))
          (setq colno (1+ colno))))
    (setq colno 0)
    (while (< colno ncol)
       (progn (dismal-redraw-cell rowno colno t)
              (setq colno (1+ colno))))))

;; * collapse two funs
;; * on insert range,
;; * on redraw-column, do guys that write into it
;; * on insertion, if mrk is held, redraw

(defun dismal-redraw-cell (row column hard-update)
 ;; hard update means to put in blanks if value is nil
 ;; and reevluating
 ;; otherwise, saves time by not drawing blanks.
 ;;Redraw one cell.
 ;; don't do it if you are blank and not a hard-update,
 ;; don't do it if you are blank and you are used (cons in mrk)
 ;; don't do it if you are in a 0 width col
 (if hard-update (dismal-evaluate-cell row column))
 (if (let* ((not-exp (not (dismal-get-exp row column))))
       (and not-exp (or (not hard-update)
                        (consp (dismal-get-mrk row column)))))
     nil ;return
   (let* ((format (dismal-get-column-format column))
          (set-width (col-format-width format)))
   (if (= set-width 0)
       nil ;return
   (dismal-goto-cell row column nil)
   ;; set up for doing the write
   (save-excursion
   (let* ((alignment (dismal-get-cell-alignment row column))
          (delete-b-width set-width)
          (delete-f-width 0)
          (delete-width set-width)
          (leading-spaces 0)  (trailing-spaces 0)
          (cell-value (dismal-get-val row column))
          ;; probbably don't need full-eval 13-Jul-92 -FER
          ;;(cell-value (dismal-evaluate-cell row column))
          (string (dismal-flat-format cell-value (aref format 1)))
          (slength (if (stringp string) (length string) 0)))
   (cond ((< slength set-width)
          (cond ((eq 'default alignment)
                 (if (or (numberp cell-value) (floatp cell-value))
                     (setq leading-spaces (- set-width slength))
                   (setq trailing-spaces (- set-width slength))))
                ((eq 'right alignment)
                 (setq leading-spaces (- set-width slength)))
                ((eq 'left alignment)
                 (setq trailing-spaces (- set-width slength)))
                ((eq 'center alignment)
                 (let ((trim (- set-width slength)))
                 (setq leading-spaces (/ trim 2))
                 (setq trailing-spaces (- trim leading-spaces)) )) ))
         ((= slength set-width)
          (setq leading-spaces 0)
          (setq trailing-spaces 0))
         ((> slength set-width)
          (setq leading-spaces 0)
          (setq trailing-spaces 0)
          (cond 
            ((eq 'default alignment)
             (if (or (floatp cell-value) (numberp cell-value))
                 (if (> slength delete-width)
                     (setq string (make-string set-width ?*)))
               (setq delete-f-width
                     (dismal-find-format-space (- slength set-width) 'right
                                               row column))
               (setq string (substring string 0 (+ delete-f-width
                                                   delete-b-width)))))
            ((eq 'right alignment)
             (setq delete-b-width
                   (+ delete-b-width
                      (dismal-find-format-space (- slength set-width) 'left
                                                row column)))
             (setq string (substring string (- slength delete-f-width
                                               delete-b-width))) )
            ((eq 'left alignment)
             (setq delete-f-width
                   (dismal-find-format-space (- slength set-width) 'right
                                             row column))
             (setq string (substring string 0 (+ delete-f-width 
                                                 delete-b-width))))
            ((eq 'center alignment)
             (setq delete-f-width
                   (dismal-find-format-space (- slength set-width) 'right
                                             row column))
             (setq delete-b-width
                   (+ delete-b-width
                      (dismal-find-format-space (- slength set-width) 'left
                                                row column)))
             ;(my-message "  break" (+ asdf adsf))
             (let* ((trim (- slength delete-b-width delete-f-width))
                    (start (if (= trim 0) 
                               0
                               (max 0 (/ 2 trim))) ))
             (setq string (substring string start (- slength start))))))  ))

            ;;(message "string is [%s]>>%s<<" slength string)

     ;; do the write
     (forward-char 1)
     (delete-char (- delete-b-width))
     ;(setq spot (list delete-b-width (min delete-f-width
     ;                  (- (save-excursion (end-of-line) (point)) (point)))
     ;                 leading-spaces string trailing-spaces))
     ;; have to do this politely
     (delete-char (min delete-f-width 
                       (- (save-excursion (end-of-line) (point)) (point))))
     (insert-char ?\040 leading-spaces)
     (if string (insert string))
     (insert-char ?\040 trailing-spaces)
     ;; don't know where you are left in the window
     ))))))

(defun dismal-find-format-space (wished-for direction row col)
 ;(message "In find-space with %s %s %s %s" wished-for direction row col) 
 (let ((result 0)
       (original-col col)
       (done nil)
       (increment (if (eq direction 'right) 
                      1
                      -1)) )
    (setq col (+ increment col))
    (while (and (not done) (>= col 0))
       (let ((mrk (dismal-get-mrk row col)))
       (if (and ;; (not (dismal-evaluate-cell row col))
                (not (dismal-get-val row col))
                (or (not mrk)
                    (and (numberp mrk)
                         (= 0 mrk))
                    (and (listp mrk)
                         (= (car mrk) row)
                         (= (cdr mrk) original-col))) )
           (progn (setq result (+ result
                                  (aref (dismal-get-column-format col) 0)))
                  (dismal-set-mrk row col (cons row original-col))
                  (if (> col dismal-max-col) (setq dismal-max-col col))
                  (if (>= result wished-for)
                      (setq done t)
                   (setq col (+ increment col))))
         (setq done t))))
  ;(message "returning find-space %s %s" direction (min wished-for result))
  (min wished-for result)))

(defun dismal-resize-column (column old-width width)
  ;; Change the width of a column.
  (if dismal-interactive-p (message "Resizing column from %s to %s..." 
                                     old-width width))
  (dismal-save-excursion
    (let* ((rowno -2))
      (if (> old-width width) ; getting smaller
          (while (<= rowno dismal-max-row)
            (dismal-goto-cell rowno column nil)
            (backward-char (1- old-width)) ; Move to cell's left end
            (delete-char (- old-width width))
            (setq rowno (1+ rowno)))
        (if (< old-width width) ; getting larger
            (while (<= rowno dismal-max-row)
              (dismal-goto-cell rowno column nil)
              (if (= old-width 0) (forward-char 1))
              (insert-char ?\040 (- width old-width))
              (setq rowno (1+ rowno)))))))
  (dismal-make-ruler))

(defun dismal-draw-labels ()
  (dismal-draw-row-labels)
  (dismal-draw-column-labels)
  ;; (message "Labeling...Done")
  )

(defun dismal-draw-row-labels ()
  ;; (message "Labeling rows...")
  (dismal-goto-cell -1 0 nil)
  (dismal-set-first-printed-column)
  (beginning-of-line)
  (delete-char dismal-first-printed-column)
  (insert-char ?\040 (1- dismal-first-printed-column))
  (insert-char ?+ 1)
  ;; (message "Relabeling rows 0 to %s" dismal-max-row)
  (let ((rowno 0))
    (while (<= rowno dismal-max-row)
      (dismal-draw-row-label rowno)
      (setq rowno (1+ rowno))))
  (message "Relabeling rows...Finished."))

(defun dismal-draw-row-label (row)
  ;; Draw the label for ROW and put a vertical bar to its right.
  (dismal-goto-cell row 0 nil)
  (beginning-of-line)
  (delete-char dismal-first-printed-column)
  (insert (format dismal-row-label-format row)))

(defun dismal-add-row-labels-at-end (add)
  (while (>= add 0)
    (dismal-draw-row-label (- dismal-max-row add))
    (setq add (1- add)))  )

(defun dismal-remove-row-label (row)
  ;; Remove the label for line ROW, and the line itself
  (dismal-goto-cell row 0 nil)
  (beginning-of-line)
  ;(delete-char dismal-first-printed-column)
  (delete-region (1- (point)) (save-excursion (end-of-line) (point))))

(defun dismal-remove-row-labels-at-end (remove)
  (let ((i 0))
  (while (< i remove)
    (dismal-remove-row-label (- dismal-max-row i))
    (setq i (1+ i)))  ))

(defun dismal-draw-column-labels ()
  ;; makes assumptions about which line the labels go on.
  ;; (message "Relabeling columns...")
  (let ((colno 0)
        (numcol dismal-max-col)) ;; used to be (matrix-width
                                        ;; dismal-matrix) -FER
    ;; put on leading +
    (dismal-goto-cell -1 0 nil)
    (beginning-of-line)
    (delete-char dismal-first-printed-column)
    (insert-char ?\040 (1- dismal-first-printed-column))
    (insert-char ?+ 1)
    ;; do rest
    (while (<= colno numcol)
      (dismal-draw-column-label colno)
      (setq colno (1+ colno))))
  (delete-rectangle (point) 
                    (save-excursion (forward-line -1) (end-of-line) (point))))

(defun dismal-draw-column-label (column)
  ;; Draw and underline the label for COLUMN.
  (let ((label (dismal-convert-number-to-colname column))
        (width (dismal-column-width column)))
    (if (= width 0)
       nil
    (dismal-goto-cell -2 column nil)
    (backward-char (1- width))          ; Move to cell's left end
    (delete-char width)                 ; Delete what's there
    (insert-char ?\040 (/ (- width (length label)) 2))
    (insert label)
    (insert-char ?\040 (- width (+ (length label)
                                   (/ (- width (length label)) 2))))
    (dismal-goto-cell -1 column nil)
    (backward-char (1- width))
    (delete-char width)
    (insert-char ?- (1- width))
    (insert-char ?+ 1))))


;;;
;;;	XVI.	Cell formatting
;;;
;;; These functions can be replaced by the user to allow other sorts 
;;; of math packages.

;; (float-to-string _f1)

(defun dismal-flat-format (value decimal)
  ;; return a string in it's full glory
  (cond ( (numberp value) (int-to-string value))
        ( (floatp value) ;it's a number
          (dismal-flat-format-float-string (float-to-string value) decimal))
        (t value)))


;;;
;;;	XVII.	Cell expression conversions
;;;

; (dismal-convert-input-to-cellexpr sexp)
; (dismal-convert-input-to-cellexpr "23.3")
; (dismal-convert-input-to-cellexpr "Brown86")
; (dismal-convert-input-to-cellexpr " ")
; (dismal-convert-input-to-cellexpr ".")
; (dismal-convert-input-to-cellexpr "23")
; (dismal-convert-input-to-cellexpr "-")
; (dismal-convert-input-to-cellexpr "(dis-plus a23:b21)")
; (dismal-convert-input-to-cellexpr "(quote (6107956 . -18))")
;  (dismal-convert-input-to-cellexpr '(setq aaa (dis-sum e0:e2)))
;  (dismal-convert-input-to-cellexpr "(setq aaa (dis-sum e0:e2))")
; (dismal-convert-input-symbol 'aaa)
;(dismal-convert-input-to-cellexpr '(dis-count a1:a340))
; (dismal-convert-input-to-cellexpr "(dis-plus a23)")
;(dismal-convert-input-to-cellexpr "(dis-count-if-regexp-match B1:B3 \"B\\+$\")")

(defun dismal-convert-input-to-cellexpr (sexp)
  ;; Recursively replace symbols in SEXP that look like cell names with
  ;; expressions that access that cell.  Also replace any numbers with
  ;; equivalent floats.  Replace any strings that look like floats with
  ;; floats.  If in a setq use variables as defined, otherwise use the 
  ;; equivalent string
  (cond ((and sexp (listp sexp))
         (if (eq (car sexp) 'setq)
             (append (list 'setq (cadr sexp))
                   (dismal-convert-input-to-cellexpr (cddr sexp)))
             (cons (dismal-convert-input-to-cellexpr (car sexp))
                   (dismal-convert-input-to-cellexpr (cdr sexp)))))
        ((numberp sexp) sexp)
        ((or (and (stringp sexp)
                  (<= (length sexp) 6)  ; this allows ZZ9999
                  (string-match dismal-cell-name-regexp sexp 0))
             (symbolp sexp) )
         (dismal-convert-input-symbol sexp))
        ((integer-stringp sexp) (dismal-convert-string-to-integer sexp))
        ((float-stringp sexp)
         (dismal-convert-string-to-float sexp))
        ((formula-string-p sexp)
         (dismal-convert-input-to-cellexpr (car (read-from-string sexp))))
        (t sexp)))

; (dismal-convert-cellexpr-to-string (dismal-get-exp dismal-current-row dismal-current-col))
; (setq sexp (dismal-get-exp dismal-current-row dismal-current-col))
; (dismal-convert-cellexpr-to-string "%")
; (message (concat "how are you" "%%" ))

;; (setq astring "aaaa%aaaaa")
; (dismal-percentager "aaaa%aaaaa")
(defun dismal-percentager (astring)
 ;; returns any %'s doubled
 (setq match-start (string-match "%" astring))
 (if match-start
    (concat (substring astring 0 (1+ match-start)) "%"
            (dismal-percentager (substring astring (1+ match-start))))
    astring))

(defun dismal-convert-cellexpr-to-string (sexp)
 ;; Print the s-expression SEXP but convert floats, strings, and cell
 ;; references to their printed representations.
 (cond ((null sexp) "")
       ((stringp sexp) (dismal-percentager sexp))  ; makes % printable
       ((numberp sexp) (int-to-string sexp))
       ((dismal-float-expr-p sexp)
        ;;(concat "\"" (dismal-convert-cellexpr-to-string (nth 1 sexp)) "\"")
        (dismal-convert-cellexpr-to-string (nth 1 sexp)))
       ((apply dismal-number-p sexp nil)
        (apply dismal-number-to-string sexp nil))
       ;; trickyness here sets up printing ranges nicely??
       ;; has leading quote
       ((and (listp sexp) (listp (cdr sexp)) (rangep (cadr sexp)))  
        (concat 
         (dismal-convert-cellexpr-to-string (range-1st-cell (cadr sexp)))
         ":" (dismal-convert-cellexpr-to-string (range-2nd-cell (cadr sexp)))))
       ((and (listp sexp) (memq (car sexp) dismal-cell-types))
        (dismal-convert-cellref-to-cellname sexp))
       ((listp sexp)
        (concat "(" (dismal-convert-cellexprlist-to-string sexp) ")"))
       (t (prin1-to-string sexp))))

(defun dismal-recursive-convert-cellexpr-to-string (sexp)
  ;; Print the s-expression SEXP but convert floats, strings, and cell
  ;; references to their printed representations.
 (cond ((null sexp) "")
       ((stringp sexp) (prin1-to-string sexp)) ; big-change here
       ((numberp sexp) (int-to-string sexp))
       ((dismal-float-expr-p sexp)
        ;;(concat "\"" (dismal-convert-cellexpr-to-string (nth 1 sexp)) "\"")
        (dismal-convert-cellexpr-to-string (nth 1 sexp)))
       ((apply dismal-number-p sexp nil)
        (apply dismal-number-to-string sexp nil))
       ;; trickyness here sets up printing ranges nicely??
       ;; has leading quote
       ((and (listp sexp) (listp (cdr sexp)) (rangep (cadr sexp)))  
        (concat 
         (dismal-convert-cellexpr-to-string (range-1st-cell (cadr sexp)))
         ":" (dismal-convert-cellexpr-to-string (range-2nd-cell (cadr sexp)))))
       ((and (listp sexp) (memq (car sexp) dismal-cell-types))
        (dismal-convert-cellref-to-cellname sexp))
       ((listp sexp)
        (concat "(" (dismal-convert-cellexprlist-to-string sexp) ")"))
       (t (prin1-to-string sexp))))


; (dismal-convert-cellexpr-to-string '(ff+ '(dismal-range (dismal-r-c- 0 5) (dismal-r-c- 2 5))))
  ;(and (listp sexp)
  ;(message "Converting %s with subtest listp: %s quote: %s range: %s vaL: %s" sexp  (listp sexp)
  ;            (eq (car sexp) 'quote)
  ;            (eq (cadr sexp) 'dismal-range) (cadr sexp)))


(defun dismal-convert-cellexprlist-to-string (sexp)
  (concat (dismal-recursive-convert-cellexpr-to-string (car sexp))
          (if (null (cdr sexp))
              ""
            (concat " " (dismal-convert-cellexprlist-to-string (cdr sexp))))))

;(dismal-flat-format-float-string (float-to-string _f1) 2)
;(dismal-flat-format-float-string (float-to-string (f -1)) 2)
; (setq string (float-to-string (f -1)))
; (setq string "   -1.0000")
; (setq rightspace 2)

(defun dismal-flat-format-float-string (string rightspace)
  ;; Given the STRING returned by float-to-string, return a string formatted
  ;; according to the value of the  decimal in rightspace.
  ;; The SPACE locals refer to the space in the formatted string, the
  ;; START and END locals refer to positions in the argument STRING.
  ;; The DIGITS locals are equal to END - START.
  (string-match floating-point-regexp string) ;; sets up match
  (let* ((decimal (if (> rightspace 0) "." ""))
         (leftstart (match-beginning 1))
         (leftend (match-end 2))
         (rightstart (min (1+ (match-beginning 3)) (match-end 3)))
         (rightend (min (match-end 3) (+ rightstart rightspace)))
         (rightdigits (- rightend rightstart)))
      (concat (substring string leftstart leftend)
              decimal
              (substring string rightstart rightend)
              (make-string (- rightspace rightdigits) ?\040))))

;;
;; Functions to do conversions on cell names
;;

(defun dismal-convert-input-symbol (symbol)
  ;; Convert string NAME to a cell access expression if it refers to a cell,
  ;; and replace symbols that look like numbers with floats.
  (let ((name (if (stringp symbol) 
                  symbol
                  (symbol-name symbol))))
    ;; (my-message "symbol is %s" name)
    (cond ((string-match dismal-cell-range-regexp name 0)
           (list 'quote (dismal-string-to-range name)))
          ((string-match dismal-cell-name-regexp name 0)
           (dismal-convert-cellname name))
          ((or (boundp symbol) (fboundp symbol)) symbol)
          (t (prin1-to-string symbol)))))

(defun dismal-convert-cellname (cellname)
  ;; Convert string NAME to a cell access expression.
  (string-match dismal-cell-name-regexp cellname)
  (let* ((row-fixed (< (match-beginning 4) (match-end 4)))
         (col-fixed (< (match-beginning 2) (match-end 2))))
    (list (if (and row-fixed col-fixed) 
              'dismal-rfcf
            (if row-fixed 'dismal-rfc-
                (if col-fixed 'dismal-r-cf 'dismal-r-c-)))
          (string-to-int (extract-match cellname 3))
          (dismal-convert-colname-to-number (extract-match cellname 1)))))

(defun dismal-convert-cellref-to-cellname (cellref)
  (concat
   (dismal-convert-number-to-colname (nth 2 cellref))
   (if (or (eq (car cellref) 'dismal-r-cf) (eq (car cellref) 'dismal-rfcf))
       "$")
   (prin1-to-string (nth 1 cellref))
   (if (or (eq (car cellref) 'dismal-rfc-) (eq (car cellref) 'dismal-rfcf))
       "$")))

(defun dismal-convert-string-to-float (string)
  ;; Convert the string to a float if it looks like one.
  ;; We add a quote here so when we eval the expression the floats
  ;; are left as they are and passed unchanged as arguments.
  (if (string-match floating-point-regexp string 0)
      (list 'quote (apply dismal-string-to-number string nil))
    (car (read-from-string string))))

(defun dismal-convert-string-to-integer (sexp)
  ;; Convert a string to an integer.  ;; You assume it matches
  (car (read-from-string sexp)))

(defun dismal-cell-name (row column)
  (concat (dismal-convert-number-to-colname column) (int-to-string row)))

(defun dismal-convert-number-to-colname (column)
  ;; Convert a number to a column name string.  Maximum column is 26^2-1.
  ;; 0 -> `A', 25 -> `Z', 26 -> `AA', 51 -> `AZ', 52 -> `BA' ...
  (if column
     (concat (if (> column 25)
                 (char-to-string (1- (+ ?A (% (/ column 26) 26))))
                "")
             (char-to-string (+ ?A (% column 26))))
     "nil"))

; (dismal-convert-colname-to-number "a")
; (dismal-convert-colname-to-number 'a)
; (dismal-convert-colname-to-number 10)
; (dismal-convert-colname-to-number nil)
(defun dismal-convert-colname-to-number (name)
  ;; The inverse of dismal-convert-number-to-colname.
  (cond ((numberp name) name)
        ((and (not (stringp name)) (char-or-string-p name))
         (setq name (char-to-string name)))
        ((not name) nil)
        ((symbolp name) (setq name (prin1-to-string name))
         (dismal-convert-colname-to-number name))
        (t
  (let ((name-length (length name))
        (index 0)
        (column -1))
    (while (< index name-length)
      ;; !! Bob added `downcase' and changed ?A to ?a in following line:
      (setq column (+ (* (1+ column) 26) (- (aref (downcase name) index) ?a)))
      (setq index (1+ index)))
    column))))


;;;
;;;	XVIII.	Column formating commands
;;;

(defun dismal-set-alignment (range-or-col alignment-style)
  (interactive (list (run-menu 'dismal-range-or-col-menu 'col)
                     (run-menu 'dismal-alignment-style-menu "Default")))
  (message "Setting new alignment style...")
  (dismal-save-excursion
  (cond ((eq range-or-col 'column)
         (dismal-set-column-alignment dismal-current-col alignment-style)
         (dismal-redraw-column dismal-current-col))
        ((eq range-or-col 'range)
         (dismal-select-range)
         (dismal-show-selected-range)
         (dismal-note-selected-range "Aligning range %s%s:%s%d")
         (let ((start-row (range-1st-row dismal-cell-buffer))
               (start-col (range-1st-col dismal-cell-buffer))
               (end-row (range-2nd-row dismal-cell-buffer))
               (end-col (range-2nd-col dismal-cell-buffer))  )
          (matrix-funcall-rc (function (lambda (r c dummy)
                                (dismal-set-fmt r c alignment-style)))
                   start-row start-col end-row end-col dismal-matrix)
          (dismal-redraw-column dismal-current-col))
         (dismal-note-selected-range "Aligning range %s%s:%s%d...Done")  )
        (t (error "error in dismal-set-column-alignment")))))

(defvar dismal-set-width-prompt
   (format "Enter column width (default is %d): " dismal-default-column-width))

(defun dismal-read-column-format (width)
  "Read in the format of the current column and redraw the ruler."
  (interactive
    (list (read-minibuffer dismal-set-width-prompt
             (prin1-to-string (dismal-column-width dismal-current-col)))))
  (if (and (> width dismal-normal-max-column-width)
           (not (y-or-n-p (format "Do you really want a column %d wide? " 
                                  width))))
      (error "Not making a wide column."))
  (dismal-save-excursion
    (message "Redrawing column %s..." 
             (dismal-convert-number-to-colname dismal-current-col))
    (dismal-set-column-format dismal-current-col width
                  (dismal-column-decimal dismal-current-col)
                  (dismal-column-alignment dismal-current-col))
    (dismal-make-ruler)
    (dismal-draw-ruler dismal-current-row)
    (message "Redrawing column %s...Done" 
             (dismal-convert-number-to-colname dismal-current-col))))

;; used to use decimal
;(defun dismal-read-column-format (width decimal)
;  "Read in the format of the current column."
;  (interactive
;   (list (read-minibuffer dismal-set-width-prompt
;            (prin1-to-string (dismal-column-width dismal-current-col)))
;         (read-minibuffer "Enter decimal width: "
;            (prin1-to-string (dismal-column-decimal dismal-current-col)))))
;  (dismal-save-excursion
;    (message "Redrawing column %s..." 
;             (dismal-convert-number-to-colname dismal-current-col))
;    (dismal-set-column-format dismal-current-col width decimal
;                  (dismal-column-alignment dismal-current-col))
;    (dismal-make-ruler)
;    (dismal-draw-ruler dismal-current-row)
;    (message "Redrawing column %s...Done" 
;             (dismal-convert-number-to-colname dismal-current-col))))


; (set-col-format-width (dismal-get-column-format 1) 1)

(defun dismal-expand-cols-in-range (arg)
  "Make all columns with width=0 in range have width arg."
  (interactive "p")
  (dismal-select-range)
  (dismal-save-excursion
    (let ((start-row (range-1st-row dismal-cell-buffer))
          (start-col (range-1st-col dismal-cell-buffer))
          (end-row (range-2nd-row dismal-cell-buffer))
          (end-col (range-2nd-col dismal-cell-buffer))
          (expanded-a-col nil)  )
     (message "Expanding columns between %s and %s ..." 
           (dismal-convert-number-to-colname start-col)
           (dismal-convert-number-to-colname end-col))
     (while (<= start-col end-col)
        (let ((format (dismal-get-column-format start-col)))
        (if (= 0 (col-format-width format))
            (progn (setq expanded-a-col t)
                   (dismal-resize-column start-col 0 arg)
                   (set-col-format-width format arg)
                   (dismal-redraw-column start-col))))
        (setq start-col (1+ start-col)))
     (if expanded-a-col 
         (progn (dismal-draw-column-labels)
                (dismal-make-ruler) (dismal-draw-ruler dismal-current-row)))
     (message "Expanding columns...Done"))))

;(defun dismal-set-column-width (width)
;  "Set the width for the current column."
;  (interactive
;   (list (read-minibuffer dismal-set-width-prompt
;               (prin1-to-string (dismal-column-width dismal-current-col)))))
;  (if (and (> width dismal-normal-max-column-width)
;           (y-or-n-p (format "Do you really want a column %d wide? " width)))
;  (dismal-set-column-format dismal-current-col
;                            width
;                            (dismal-column-decimal dismal-current-col)
;                            (dismal-column-alignment dismal-current-col))))

(defun dismal-set-column-decimal (decimal)
  "Set the decimal format for the current column."
  (interactive
   (list (read-minibuffer "Enter decimal width: "
            (prin1-to-string (dismal-column-decimal dismal-current-col)))))
  (dismal-set-column-format dismal-current-col
                            (dismal-column-width dismal-current-col)
                            decimal
                            (dismal-column-alignment dismal-current-col)))

;(setq format (make-vector 5 nil))
; (setq decimal 0)
; (setq align 'center)

;; Do resize b4 changing dismal-column-formats so dismal-goto-cell still works
(defun dismal-set-column-format (column width decimal align)
  (let* ((format (dismal-get-create-column-format column))
         (old-width (aref format 0))
         (old-decimal (aref format 1))
         (old-align (aref format 2)) )
    (dismal-resize-column column old-width width)
    (aset format 0 width)
    (aset format 1 decimal)
    (aset format 2 align)
    (if (or (not (equal old-width width))
            (not (equal old-decimal decimal))
            (not (equal old-align align)))
        (dismal-redraw-column column))))

(defun dismal-get-column-format (column)
  ;; Compute the format of the given COLUMN from dismal-column-formats.
  (let ((format (vector-ref dismal-column-formats column)))
    (if format
        format
      dismal-default-column-format)))

;; clean this up using dismal-column-alignment
(defun dismal-get-cell-alignment (row column)
  ;; Compute the alignment of the given COLUMN from dismal-column-formats.
  (let* ((alignment0 (dismal-get-fmt row column))
         (format (vector-ref dismal-column-formats column))
         (alignment1 (if format (aref format 2))) )
    (or alignment0
        alignment1
        (aref dismal-default-column-format 2))))

(defun dismal-column-width (column)
  ;; Compute the width of the given COLUMN from dismal-column-formats.
  (aref (dismal-get-column-format column) 0))


(defun dismal-column-alignment (column)
  ;; may return nil
  (let ( (format (dismal-get-column-format dismal-current-col)) )
    (if format
        (aref format 2))))


(defun dismal-column-decimal (column)
  ;; Compute the decimal field width of the given COLUMN from 
  ;; dismal-column-formats.
  (aref (dismal-get-column-format column) 1))

(defun dismal-sum-column-widths (start-col cols)
  ;; compute the sum of widths of cols start-col to (start-col + cols)
   (let ((i 0) (results 0))
   (while (<= i cols)
      (setq results (+ results (dismal-column-width (+ start-col i))))
      (setq i (1+ i)))
   results))

(defun dismal-get-column-position (column)
  ;; Compute the position of the beginning the the given COLUMN.
  (if (= column 0)
      dismal-first-printed-column
    (+ (dismal-column-width (1- column))
       (dismal-get-column-position (1- column)))))


;;;
;;;	XIXa.	Utility functions - Date functions
;;;
;;; These should be made a little tighter and cleaner with conds and such,
;;; and pay head to common leap years. -FER

(defun current-line-in-window ()
  ;; taken from the gnu-emacs manual entry on count-lines, p. 377
  ;; so not necc. to add dismal- to front
  (+ (count-lines (window-start) (point))
     (if (= (current-column) 0) 1 0)
     -1))

(defun dismal-no-op (arg)
  (interactive "p")
  (error "%s is not defined for dismal-mode." (this-command-keys)))

(defun dismal-insert-blank-box (start-point rows cols text)
  ;; Starting at START-POINT insert ROW lines of COLS copys of TEXT.
  ;; The column is taken from that of START.
  ;; A rough inverse of this function is kill-rectangle.
 (save-excursion
 (let ((i 0) (cc nil))
      (goto-char start-point)
      (setq cc (current-column))
      (while (< i rows) 
        (dismal-insert-n-times text cols)
        (setq i (1+ i))
        (forward-line 1)
        (move-to-column cc)))))

(defun dismal-insert-n-times (item N)  ;(dismal-insert-n-times "a" t)
  ;; Insert ITEM (abs N) (t=1) times.
  (if (numberp N)
      (progn (setq N (abs N))
             (while (> N 0)
               (insert item)
               (setq N (- N 1))))
    (if N (insert item))))

(defun dis-current-date (&optional day-first)
  "Insert current date as string. If DAY-FIRST is t, do that."
  (let ((time-string (current-time-string)))
    (if day-first
        ;; insert day before date:
        (format "%s " (downcase (substring time-string 0 3)))
     ;; insert date:
     (if insert-date-with-month-namep
	 (format "%s-%s-%s"
		 (if (string-equal " " (substring time-string 8 9))
		     (substring time-string 9 10)
		   (substring time-string 8 10))
		 (substring time-string 4 7)
		 (substring time-string  -2 nil))
	 (format "%s-%s-%s"
		 (car (cdr (assoc (substring time-string 4 7) *date-table*)))
		 (if (string-equal " " (substring time-string 8 9))
		     (substring time-string 9 10)
		   (substring time-string 8 10))
		 (substring time-string  -2 nil))))))

(defun dismal-days-to-date (days)
  (let ((year (/ days 365))
        (yday (% days 365))
        (day 0)
        (month ""))
    (if (<= yday 31) (setq month "Jan")
      (setq yday (- yday 31))
      (if (<= yday 28) (setq month "Feb")
        (setq yday (- yday 28))
        (if (<= yday 31) (setq month "Mar")
          (setq yday (- yday 31))
          (if (<= yday 30) (setq month "Apr")
            (setq yday (- yday 30))
            (if (<= yday 31) (setq month "May")
              (setq yday (- yday 31))
              (if (<= yday 30) (setq month "Jun")
                (setq yday (- yday 30))
                (if (<= yday 31) (setq month "Jul")
                  (setq yday (- yday 31))
                  (if (<= yday 31) (setq month "Aug")
                    (setq yday (- yday 31))
                    (if (<= yday 30) (setq month "Sep")
                      (setq yday (- yday 30))
                      (if (<= yday 31) (setq month "Oct")
                        (setq yday (- yday 31))
                        (if (<= yday 30) (setq month "Nov")
                          (setq yday (- yday 30))
                          (if (<= yday 31) (setq month "Dec"))
                          (setq yday (- yday 31)))))))))))))
    (format "%02d-%s-%02d" yday month (+ year 70))))

(defun dis-date-to-days (date)
  "Return number of days between Jan 1, 1970 and DATE (a string)."
;; The date format is dd-mmm-yy.  Ignores leap years.
  (let* ((year (string-to-int (substring date 7 9)))
         (day (+ (string-to-int (substring date 0 2))
                 (* (- year 70) 365)))
         (month (substring date 3 6)) )
    (if (string= month "Jan")
        ()
      (setq day (+ day 31))
      (if (string= month "Feb")
          ()
        (setq day (+ day 28))
        (if (string= month "Mar")
            ()
          (setq day (+ day 31))
          (if (string= month "Apr")
            ()
            (setq day (+ day 30))
            (if (string= month "May")
                ()
              (setq day (+ day 31))
              (if (string= month "Jun")
                  ()
                (setq day (+ day 30))
                (if (string= month "Jul")
                    ()
                  (setq day (+ day 31))
                  (if (string= month "Aug")
                      ()
                    (setq day (+ day 31))
                    (if (string= month "Sep")
                        ()
                      (setq day (+ day 30))
                      (if (string= month "Oct")
                          ()
                        (setq day (+ day 31))
                        (if (string= month "Nov")
                            ()
                          (setq day (+ day 30))
                          (if (string= month "Dec") ; It has to be
                              ()
                            (setq day (+ day 31))))))))))))))
    day))


;;;
;;;	XIXb.	Utility functions - List functions
;;;

(defun dismal-map-apply (function list)
  (if (null list)
      ()
    (apply function (car list) nil)
    (dismal-map-apply function (cdr list))))

;; (setq aa '(1 2 1 3 4))
;; (dismal-del 1 aa)

;; this is a destructive versions I think 14-Mar-92 -FER
(defun dismal-del (elt alist)
  ;; Delete any elements equal to ELT in LIST.
  (let ((n nil)
        (copy-list nil))
    (while (eq (car alist) elt)
       (setq alist (cdr alist)))
    (setq n (length alist))
    (setq copy-list alist)
    (while (> n 0)
      (if (equal elt (cadr copy-list))
          (setcdr copy-list (cddr copy-list)))
      (pop copy-list)
      (setq n (1- n)) )   )
  alist)

;(defun dismal-del (elt list)
;  "Delete any elements 'equal' to ELT in LIST."
;  (if (null list)
;      nil
;    (if (listp list)
;        (if (equal elt (car list))
;            (dismal-del elt (cdr list))
;          (cons (car list) (dismal-del elt (cdr list)))))))

(defun dismal-float-expr-p (sexp)
  ;; Returns true if this is an expression of the form (quote float),
  ;; which is how numbers appear in expressions so they can be eval'ed.
  (and (listp sexp)
       (eq (car sexp) 'quote)
       (apply dismal-number-p (nth 1 sexp) nil)))


;;;
;;;	XIXc.	Utility functions - Math functions
;;;

(defvar dismal-last-fill-range-start 0)
(defvar dismal-last-fill-range-increment 1)

(defun dismal-fill-range (start-count increment)
  "Between point and mark, insert a range of numbers starting at START-COUNT."
  ;; someday you'll see this do decrements, etc.
 ;(interactive "nNumber to start counting from: \nnNumber to increment with: ")
  (interactive
    (list (read-minibuffer "Number to start counting from: "
             (prin1-to-string dismal-last-fill-range-start))
          (read-minibuffer "Number to increment with: "
             (prin1-to-string dismal-last-fill-range-increment))))
  (if (not start-count) dismal-last-fill-range-start)
  (setq dismal-last-fill-range-increment increment)
  (dismal-select-range)
  (dismal-save-excursion
    (let ((start-row (range-1st-row dismal-cell-buffer))
          (start-col (range-1st-col dismal-cell-buffer))
          (end-row (range-2nd-row dismal-cell-buffer))  )
    (dismal-jump-to-cell start-row start-col)
    (while (<= start-row end-row)
      (dismal-set-cell dismal-current-row dismal-current-col
                      start-count nil)
      (dismal-forward-row 1)
      (setq start-count (+ start-count increment))
      (setq start-row (1+ start-row)))
    (setq dismal-last-fill-range-start start-count) )))

;;  (dismal-adjust-range-list "l52:l500")

(defun dismal-adjust-range-list (range-list)
  (cond ((rangep range-list) range-list)
        ((stringp range-list) (dismal-string-to-range range-list))
        (t (message "Using current range from point and mark...")
           (dismal-select-range))))

; (dis-count '(dismal-range (dismal-r-c- 0 0) (dismal-r-c- 3 0)))
; (dis-count "a0:a3")

(setq dis-user-cell-functions
  '(dis-count dis-count-words-in-range
    dis-count-regexp-in-range
    dis-count-if-regexp-match
    dis-match-list

    dis-sum dis-product dis-div dis-plus

    dis-current-date dis-date-to-days
    f     ftrunc    fint
    fmin    fmax
    f+ f/ f* f-))

(defun dis-count (range-list)
  "Given a cell RANGE computes the count of filled cells."
  (interactive "P")
  (setq range-list (dismal-adjust-range-list range-list))
  (dismal-do (function (lambda (row col old-result)
                (setq result
                      (dismal-safe-count old-result (dismal-get-val row col)))))
             range-list 0))

(defun dis-count-words-in-range (range-list)
  "Count the words in RANGE"
  (interactive "P")
  (setq range-list (dismal-adjust-range-list range-list))
  (dis-count-regexp-in-range range-list "\\(\\w\\)+"))

(defun dis-count-regexp-in-range (range-list regexp)
  "Given a cell RANGE-LIST computes the number of times REGEXP is matched."
  (interactive "P")
  (setq range-list (dismal-adjust-range-list range-list))
  (dismal-do (function (lambda (row col old-result)
                  ;; (my-message "Got old-result of %s" old-result)
               (setq result
                     (+ old-result
                        (count-regexp-in-string regexp (dismal-get-val row col))))))
               range-list 0))

; (count-regexp-in-string "\\(\\w\\)+" "the odg-sat down." 0)
; (count-regexp-in-string "\\(\\w\\)+" "17-aug-92" 0)

(defun count-regexp-in-string (regexp string &optional start)
 (cond ((numberp string) 1)
       ((or (not string) (not (stringp string))) 0)
        (t (if (not (numberp start))
               (setq start 0))
           (let ((start (string-match regexp string start))
                 (end (match-end 0))
                 (real-end (length string)))
             (cond ((not start) 0)
                   ((>= end real-end) 1)
                   (t (1+ (count-regexp-in-string regexp 
                                                  string (+ 1 end)))))))))

(defun dis-count-if-regexp-match (range regexp)
  "Given a cell RANGE computes the number of cells that match REGEXP."
  (interactive "P")
  (setq range (dismal-adjust-range-list range))
  ;; dismal-do has a local result that it uses and returns on its own
  (dismal-do (function (lambda (row col old-val)
                (let ((val (dismal-get-val row col)))
                (if (and (stringp val)
                         (string-match regexp val))
                    (setq result (1+ old-val))))))
             range 0))

(defun dis-match-list (range regexps)
  "Given RANGE returns the cell references that match regexps in REGEXPs."
  (interactive "P")
  (setq range (dismal-adjust-range-list range))
  (let ((match-result nil))
  (dismal-do (function (lambda (row col old-val)
                (let ((val (dismal-get-val row col)))
                (if (and (stringp val)
                         (dis-string-match-regexps regexps val))
                    (setq match-result (push (cons row col) match-result))))))
             range 0)
  (reverse match-result)))


(defun dis-string-match-regexps (regexps val)
  ;; assumes that regexps is a list of regexps
  (cond ((null regexps) nil)
        ((stringp regexps)
         (string-match regexps val))
        (t (or (string-match (car regexps) val)
               (dis-string-match-regexps (cdr regexps) val)))))

;; old result comes first
(defun dismal-safe-count (arg1 arg2)
  (if arg2 (1+ arg1) arg1))

(defun dis-sum (range)
  "Given a cell RANGE computes the sum of filled cells."
  (interactive "P")
  (setq range (dismal-adjust-range-list range))
  (dismal-do (function (lambda (row col old-result)
                (let ((val (dismal-get-val row col)))
             ;(my-message "%s:%s Result is %s" row1 col1 
             ;           (if (floatp result) (float-to-string result) result))
                (if (or (numberp val) (floatp val))
                    (setq result (dis-simple-plus old-result val))))))
             range 0))

(defun dis-product (range)
  "Given a cell RANGE computes the product of filled cells."
  (interactive "P")
  (setq range (dismal-adjust-range-list range))
  (dismal-do (function (lambda (row col old-result)
                (setq result
                      (dismal-safe-f* old-result (dismal-get-val row col)))))
             range _f1))

(defun dismal-map (function first-value list)
  "Return the product of mapping function across list in a mapconcat fashion."
  (let ((result nil))
    (setq result (funcall function first-value (pop list)))
    (while list
       (setq result (funcall function result (pop list))))
    result))

(defun dismal-do (function arange initial-value)
  "Iteratively call FUNCTION on cells in ARANGE.  We bind
result to INITIAL-VALUE for your use, and return result."
  ;; can't be a macro, unless you keep the guard of 
  ;; dismal-max-row/col in somehow
 (let* ((from-cell (range-1st-cell arange))
        (to-cell (range-2nd-cell arange))
        (row1 (range-1st-row arange))
        (row2 (min dismal-max-row (range-2nd-row arange)))
        (col1 (range-1st-col arange))
        (col2 (min dismal-max-col (range-2nd-col arange)))
        (start-col col1)
        (result initial-value))
    (while (<= row1 row2)
      (while (<= col1 col2)
        (funcall function row1 col1 result)
        ;(my-message "%s:%s Result is %s" row1 col1 result)
        (setq col1 (1+ col1)))
      (setq row1 (1+ row1))
      (setq col1 start-col))
   result))

(defun dis-simple-plus (arg1 arg2)
  "A two arg version of plus that knows about floats and ints."
  (cond ((and (numberp arg1) (numberp arg2)) (+ arg1 arg2))
        ((and (floatp arg1) (numberp arg2))
         (f+ arg1 (f arg2)))
        ((and (floatp arg2) (numberp arg1))
         (f+ arg2 (f arg1)))
        ((and (floatp arg1) (floatp arg2))
         (f+ arg1 arg2))
        (t (error "Tried to add together %s and %s" arg1 arg2))))

(defun dis-plus (&rest args)
  "A safe version of plus that knows about floats, ints, cells and ranges."
  (let ((result 0))  
  (mapc
    (function (lambda (x)       ;; (my-message "Adding %s" x)
       (setq result
         (cond ((rangep x)
                (dis-simple-plus (dis-sum x) result))
               ((dis-addressp x)
                (dis-simple-plus result 
                   (dismal-get-val (dis-address-row x) (dis-address-col x))))
               ((stringp x) result)
               ((and (numberp x) (numberp result)) (+ x result))
               ((and (floatp x) (numberp result))
                (f+ x (f result)))
               ((and (floatp result) (numberp x))
                (f+ result (f x)))
               ((and (floatp x) (floatp result))
                (f+ x result))
               ((and (boundp x) (numberp (eval x)))
                (dis-simple-plus (eval x) result))
               (t (error "Tried to add together %s and %s" x result))))))
     args)
  result))

(defun dis-div (arg1 arg2)
  "A two arg version of divide that knows about floats and ints."
  (if (or (equal arg2 _f0) (and (numberp arg2) (= arg2 0)))
      (progn (ding) (ding) 
             (message "Dividing %s by %s given value na" arg1 arg2)
             "NA")
  (cond ((and (numberp arg1) (numberp arg2)) 
         (f/ (f arg1) (f arg2)))
        ((and (floatp arg1) (numberp arg2))
         (f/ arg1 (f arg2)))
        ((and (floatp arg2) (numberp arg1))
         (f/ (f arg1) arg2))
        ((and (floatp arg1) (floatp arg2))
         (f/ arg1 arg2))
        (t (error "Tried to dis-div %s and %s" arg1 arg2)))))

(defun dismal-safe-f* (arg1 arg2)
  "A safe version of f* that gives non-numbers the value of 1."
  (let ((real-arg1 (dismal-coerce-to-float arg1 _f1))
        (real-arg2 (dismal-coerce-to-float arg2 _f1)))
    (f* real-arg1 real-arg2)))

(defun dismal-coerce-to-float (arg default)
 (cond ((stringp arg) default)
       ((numberp arg) (f arg))
       ((floatp arg) arg)
       (t default)   ))


;;;
;;;	XIXd.	Utility functions - misc
;;;

;; (mapc '(lambda (x) (my-message "got %s" x)) '(1 2 3 34))

;; same as in simple-menu.el

;; (formula-p '(dis-count))
;; (formula-p '(34343 . 33))
;; (formula-p '(quote (34343 . 33)))
(defun formula-p (item)
  ;; (my-message "Calling formula-p on %s" item)
  (and (listp item)
       (not (eq (car item) 'quote))
       (not (floatp item))))

                                ; (formula-string-p "(gf* 34 34)")
 ; (formula-string-p "(dis-count-if-regexp-match B1:B3 \"B\\+$\")")
(defun formula-string-p (item)  ;(formula-string-p "(f* 34 34)")
  (and (stringp item)           ;(formula-string-p "(f/ (f 3) (f 3))")
       (string-match "^([a-zA-Z .0-9:$---/\"+=\\]*)$" item)
       (fboundp (car (car (read-from-string item))))))

; (dismal-char-col-to-dismal-col 50)
(defun dismal-char-col-to-dismal-col (char-col)
  (let ((i 0)
        (total dismal-first-printed-column))
    (while (> char-col (setq total 
                             (+ total (dismal-column-width i))))
      (setq i (1+ i)))
    i))

(defun dismal-bobp ()
  (and (= dismal-current-row 0)
       (= dismal-current-col 0)))

(defun dismal-eobp ()
  (and (= dismal-current-row dismal-max-row)
       (= dismal-current-col dismal-max-col)))

(defun dismal-debug-cell ()
  "Tell a developer more about a cell."
  (interactive)
  (let* ((cell (matrix-ref dismal-matrix dismal-current-row 
                          dismal-current-col))
         (val (dismal-get-cell-val cell)) )
    (message "%s-%s:[%s %s %s %s %s] C:%s MR:%s MC:%s RR:%s MC:%s"
             dismal-current-col
             (dismal-cell-name dismal-current-row dismal-current-col)
             (prin1-to-string (dismal-get-cell-exp cell))
             (if (floatp val)
                 (float-to-string val) (prin1-to-string val))
             (dismal-get-cell-dep cell)
             (dismal-get-cell-mrk cell)
             (dismal-get-cell-fmt cell)
             (dismal-get-column-format dismal-current-col)
             dismal-max-row dismal-max-col dismal-current-first-ruler-row
             dismal-middle-col   )))

;; force a move to column by adding spaces
(defun dismal-move-to-column (col)
  (dismal-insert-n-times " " (- col (move-to-column col))))


(defun dismal-show-functions ()
  "Show all the functions that dismal will let you use."
  (interactive)
  (let ((old-buffer (current-buffer)))
    (pop-to-buffer help-buffer)
    (erase-buffer)
    (insert "Available dismal functions:\n\n")
    (mapc (function (lambda (x)
             (insert (prin1-to-string x))
             (dismal-move-to-column (max (+ 2 (current-column))
                                    18))
             (insert (documentation x) "\n")))
          dis-user-cell-functions)
    (goto-char (point-min))  ))

;; on older version of popper was a macro.
(if dismal-use-popper
    (cond ((not (listp (symbol-function 'popper-wrap)))      ;; 19
           (popper-wrap 'dismal-show-functions help-buffer))
          ((eq 'lambda (car (symbol-function 'popper-wrap))) ;; 18 popper fun
           (popper-wrap 'dismal-show-functions help-buffer))
          (t                                                ;; 18 popper macro
            (popper-wrap dismal-show-functions help-buffer))))


;;;
;;;	N.	Final code
;;;

(if (not (featurep 'dismal))
    (progn
      (message "Type \"%s\" for the dismal %s command menu." 
               "C-c C-m" dismal-version)
      (sit-for 2)))

;; (fset 'remove-screen
;;   "nn nnnn             ")

(run-hooks 'dismal-load-hook)

;; Wait till end so you know it got loaded.
(provide 'dismal)


;;;
;;;	N+1.	History 
;;;
;;; 
;;; 30-Dec-93 - released 0.92, with improved makefile
;;; 8-Dec-93 -released 0.9 to the net
;;; 6-Aug-93 -FER 0.85 released so that it can compile with 18.59 and 
;;;   with 19.17 (but uses none of the 19 features). 
;;; V 0.8 13-jul-92 -FER added some code from Bob Chassell (bob@gnu.ai.mit.edu)
;;; V 0.61 19-Mar-92 -FER shipped down to David
;;; V 0.6   various       beat on a lot
;;; Dec 91 on  - FER being modified
;;; V 0.5  Dec-91 (?)     got version from David Fox


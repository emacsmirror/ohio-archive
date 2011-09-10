;;; multi-forms.el - Forms Mode with multiple records/page - A GNU Emacs Major Mode
;;; SCCS Status     : multi forms-mode	1.4.0
;;; Author          : Frank Ritter (based on Johan Vromans forms-mode)
;;; Created On      : 1989
;;; Last Modified By: Frank Ritter
;;; Last Modified On: Thu Jun 25 15:45:54 1992
;;; Update Count    : 571
;;; Status          : OK

;; LCD Archive Entry:
;; multiple forms-mode|Frank Ritter|ritter@cs.cmu.edu
;; |Mode to edit file of records, displaying multiple records per buffer
;; |92-6-25|1.4.0|~/?/multi-forms-mode.tar.Z


;;; Table of contents
;;;	i.	Disclaimer
;;;	ii.	Overview of how multi forms-mode works
;;;	iii.	HISTORY
;;; 	iv.	Global variables and constants
;;; 	v.	Mandatory variables - must be set in/by the control file
;;; 	vi.	Internal variables
;;;	vii.	Requires and loads
;;;	viii.	Known bugs
;;;	ix.	Installation notes
;;;
;;; 	I.	forms-mode and startup code
;;; 	II.	other mode helpers
;;;	III.	forms--make-record-filters
;;;	IV.	Set up and check local variables
;;; 	V.	Set up the format (printing out) stuff
;;; 	VI.	Set up the parsing (rereading records) routines
;;;	VII.	Set up the keymaps
;;; 	VIII.	Changed movement functions
;;;	IX.	Changed saving functions
;;;	X.	Translation functions between Excel and Forms
;;;	XI.	forms-self-insert & character insertion setup functions
;;;	XII.	Report functions
;;;	XIII.	Changes to "normal" commands
;;;	XIV.	Changes to picture-mode commands
;;;	XV.	Utility functions
;;;	XVI.	Debugging functions
;;;
;;; Original forms-mode version by Johan Vromans (jv@mh.nl).
;;; Revision and extensions by Frank Ritter (ritter@cs.cmu.edu).
;;; Copyright 1992 Frank Ritter (extensions past Johan's 1991 version).
;;; Multi-forms mode code is not necc. compatible with Johan's, do not load
;;; them into the same emacs.  Some forms-mode data files should be readable,
;;; however, under multi-forms. 
;;;


;;;
;;;	i.	Disclaimer
;;;
;;; This file is part of GNU Emacs.
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.

;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities. 
;;; If you don't have this copy, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;


;;;
;;;	ii.	Overview of how multi forms-mode works
;;; 
;;;
;;; === Naming conventions
;;;
;;; The names of all variables and functions start with 'form-'.
;;; Names that start with 'form--' are intended for internal mode use, and
;;; should *NOT* be touched by users.
;;;
;;; Most variables are buffer-local, to enable visiting several
;;; files simultaneously.
;;; Variable 'forms--mode-setup' is local to *ALL* buffers, for it 
;;; controls if forms-mode has been enabled in a buffer.
;;;
;;; === How it works ===
;;;
;;; Multi forms-mode is like Johan Vromans forms-mode, in that 
;;; it helps you create a record based way to view a file contents.
;;; The original Forms-mode lets you create a small file that
;;; contains information on how to display each record,
;;; including labels, and field sizes, and a pointer to the file
;;; that contained the actual records.  One could then step
;;; through the records, with one record displayed at a time.
;;; Multi-forms take this concept two steps further.  One step
;;; was to display more than one record per buffer.  Multi
;;; forms-mode displays all of them at once.  How many you see
;;; is limited by the window and font size.  The other step was
;;; to keep the formating commands and the data in the same
;;; file.  In each case, The records are separated by a newline,
;;; the fields are separated by a user-defined field separater
;;; (default: TAB).  When shown, the records are either
;;; transferred one at a time, or en mass to an emacs buffer and
;;; are presented using a user-defined form.
;;;
;;; Forms mode is a composite mode. It involves two files (and two
;;; buffers), or a single file (and two buffers).  In the two file version,
;;; the first file, called the control file, defines the name of the
;;; data file and the forms format.  This file buffer will be used to
;;; present the forms.  The second file holds the actual data.  The buffer
;;; of this file will be buried, for it is never accessed directly.
;;;
;;; In the single file version, the control data and then the data are in 
;;; the same file, separated by the forms-header-stop string 
;;; (default: ";**** end of forms header ****")
;;; A leading comment header is also allowed from the control data to the 
;;; forms-comment-header-stop separation string 
;;; (default: ";**** end of forms comment header ****")
;;; Forms mode is invoked for the two-file version by 
;;; using "forms-find-file control-file".  Alternativily 
;;; forms-find-file-other-window can be used.
;;; If forms-query-mode is t (the default), the user will be queried before 
;;; a file is put into forms-mode.  This allows you to edit a file in 
;;; single-file forms format.
;;;
;;; You may also visit the control file, and switch to forms mode by hand
;;; with M-x forms-mode.
;;;
;;; Automatic mode switching is supported, so you may use "find-file" with 
;;; single-file formated files if you specify "-*- forms -*-" in the first 
;;; line of the control/data file.  
;;;
;;; This file (forms.el) must also be loaded with a (load "forms") or 
;;; an (autoload 'forms-mode "forms-mode").
;;; 
;;; The control file (or single file) is visited, and its commands are 
;;; evaluated.  They should set at least the following variables:
;;;
;;;	forms-file		    [string] the name of the data file.
;;;                                 (omit in single file version.)
;;;
;;;	forms-format-list           [list]   formatting instructions.
;;;
;;;	forms-number-of-fields	    [integer]
;;;			The number of fields in each record.
;;;                                (now not explicitly needed)
;;;
;;; The forms-format-list should be a list, each element containing 
;;; information on how to display an item, which may or may not be a field
;;; from the data.  There are 8 fields, all required, in each element.
;;; All formats are relative, size fields only count if field is a number 
;;; (i.e, refers to the data)
;;;
;;;  - name (symbol) used in menus (or to be used in menus), to name this
;;;    element
;;;  - visiblep (t/nil), will this element be displayed? (currently ignored)
;;;
;;;  - label (string/nil) to print in front of field element
;;;  - field element.  It can be:
;;;    - a string, e.g. "hello" (which is inserted \"as is\"),
;;;    - an integer, denoting a field number. The contents of the field
;;;    are inserted at this point.  The first field is number one.
;;;    - an s-expression, e.g. (insert "text"). This s-expression is
;;;    dynamically evaluated and should return a string. It should *NOT*
;;;    have side-effects on the forms being constructed.
;;;    The fields of the current record are available to the function in
;;;    the variable forms-fields, they should *NOT* be modified.
;;;    - a lisp symbol, that must evaluate to one of the above.
;;;
;;;  - default value (s-expression).  Not currently used.
;;;
;;;  - newline-p (t/nil/N) number of newlines to put in front of label. t=1.
;;;
;;;  - field-size (number of chars or t).  Amount of maximum field element to
;;;    display.  If t, display to end of line.  Error (perhaps unsignaled)
;;;    to have to elements on same line with field-size t.
;;;  
;;;  - line-size (t/nil/N) number of lines field element is allowed to be.
;;;    t=1, nil=0.
;;;  Here's an example element: (timestamp t "TIME: " 1  "1"  nil  8  nil)

;;; Optional variables that may be set in the control file:
;;;
;;;	forms-field-sep				[string, default TAB]
;;;			The field separator used to separate the
;;;			fields in the data file. It may be a string.
;;;
;;;	forms-read-only				[bool, default nil]
;;;			't' means that the data file is visited read-only.
;;;			If no write access to the data file is
;;;			possible, read-only mode is enforced. 
;;;
;;;	forms-multi-line			[string, default "^K"]
;;;			If non-null the records of the data file may
;;;			contain fields that span multiple lines in
;;;			the form.
;;;			This variable denoted the separator character
;;;			to be used for this purpose. Upon display, all
;;;			occurrencies of this character are translated
;;;			to newlines. Upon storage they are translated
;;;			back to the separator.
;;;
;;;	forms-forms-scroll			[bool, default t]
;;;			If non-nil: redefine scroll-up/down to perform
;;;			forms-next/prev-field if in forms mode.
;;;
;;;	forms-forms-jump			[bool, default t]
;;;			If non-nil: redefine beginning/end-of-buffer
;;;			to performs forms-first/last-field if in
;;;			forms mode.
;;;
;;;	forms-new-record-filter			[symbol, no default]
;;;			If defined: this should be the name of a 
;;;			function that is called when a new
;;;			record is created. It can be used to fill in
;;;			the new record with default fields, for example.
;;;			Instead of the name of the function, it may
;;;			be the function itself.
;;;
;;;	forms-modified-record-filter		[symbol, no default]
;;;			If defined: this should be the name of a 
;;;			function that is called when a record has
;;;			been modified. It is called after the fields
;;;			are parsed. It can be used to register
;;;			modification dates, for example.
;;;			Instead of the name of the function, it may
;;;			be the function itself.
;;;
;;; 
;;; After evaluating the control file, its buffer is cleared and used for
;;; further processing.  The data file (as designated by "forms-file") is
;;; visited in a buffer (forms--file-buffer) that will not normally be
;;; shown.  Great malfunctioning may be expected if the data file/buffer
;;; is modified outside of this package while it's open under forms!
;;; 
;;;
;;; A record from the data file is transferred from the data file,
;;; split into fields (into forms--the-record-list), and displayed using
;;; the specs in forms-format-list.
;;; A format routine 'forms--format' is built upon startup to format 
;;; the records.
;;;
;;; When a form is changed the record is updated as soon as the form
;;; is left.  The contents of the form are parsed using forms-format-list,
;;; and the fields that are deduced from the form are modified
;;; (fields not shown on the forms retain their origional values).
;;; The newly formed record and replaces the contents of the
;;; old record in forms--file-buffer.
;;; A parse routine 'forms--parser' is built upon startup to parse
;;; the records.
;;;
;;; Two exit functions exist: forms-exit (which saves) and forms-exit-no-save
;;; (which doesn't).  However, if forms-exit-no-save is executed and the file
;;; buffer has been modified, emacs will ask the appropriate and regular 
;;; questions about saving the buffer.
;;;
;;; Other functions are:
;;;
;;;	paging (forward, backward) by record
;;;	jumping (first, last, any number)
;;;	searching
;;;	creating and deleting records
;;;	reverting a single form (NOT the entire file buffer)
;;;	switching edit <-> view mode v.v.
;;;	jumping from field to field
;;;     a menu of commands
;;;     print out a listing of the formated elements
;;;     edit associated comments and the command header on the fly
;;;     tally field counts (i.e., count the amounts of each type of field type 
;;;       for a given or queried field)
;;;
;;; As an documented side-effect: jumping to the last record in the
;;; file (using forms-last-record) will adjust forms--total-records if
;;; needed.
;;;
;;; Commands and keymaps:
;;;
;;; A local keymap 'forms-mode-map' is used in the forms buffer.
;;; As conventional, this map can be accessed with C-c prefix.
;;; In read-only mode, the C-c prefix is be omitted.
;;;
;;; Default bindings:
;;;
;;;	\C-c	forms-mode-map
;;;	TAB	forms-next-field
;;;	SPC 	forms-next-record
;;;	<	forms-first-record
;;;	>	forms-last-record
;;;	?	describe-mode
;;;	b	forms-prev-record
;;;	d	forms-delete-record
;;;	e	forms-edit-mode
;;;	i	forms-insert-record
;;;	j	forms-jump-record
;;;     m       forms-run-menu
;;;	n	forms-next-record
;;;	p	forms-prev-record
;;;	q	forms-exit
;;;	s	forms-search
;;;	v	forms-view-mode
;;;	x	forms-exit-no-save
;;;	DEL	forms-prev-record
;;;
;;; Buffer-local variables forms-forms-scroll and forms-forms-jump
;;; may be used to control how far forms-scroll-down/up scroll, to the bottom
;;; of a record or to the bottom of the whole list.
;;;
;;; A revert-file-hook is defined to revert a forms to original file contents.
;;;
;;; For convenience, TAB (and M-TAB) is bound to forms-next-field, so you
;;; don't need the C-c prefix for this command.
;;; See the example test.forms for an example of the single file format, and
;;; demo1.forms for the double file format.

;;; Example files should be included with this source code
;;; file.  if it is not, one may be obtained by emailing the
;;; author, ritter@cs.cmu.edu


;;;
;;;	iii.	HISTORY
;;;

;;; 25-Jun-92 - finally exported
;;; Feb 92?  Johan releases a later forms-mode. 
;;;     (multi-forms is not based on this version)
;;; Jan 92 -FER packaged for export; single file version
;;;   with protected keyword labels
;;; Dec-Aug 91 -FER  added support for single file version
;;;                  cleaned up keymaps
;;; 19-Jul-91 - FER  changed to (provide 'forms)
;;; 1-Jul-1991		Johan Vromans	
;;;    Normalized error messages.
;;; 30-Jun-1991		Johan Vromans	
;;;    Add support for forms-modified-record-filter.
;;;    Allow the filter functions to be the name of a function.
;;;    Fix: parse--format used forms--dynamic-text destructively.
;;;    Internally optimized the forms-format-list.
;;;    Added support for debugging.
;;;    Stripped duplicate documentation.
;;; 29-Jun-1991		Johan Vromans	
;;;    Add support for functions and lisp symbols in forms-format-list.
;;;    Add function forms-enumerate.

(provide 'forms)

;;; you must set this up upon installation:
(defvar forms-load-path
  "/afs/cs/project/soar/member/ritter/spa/spa-mode/new/forms/new"
  "*Directory where forms.el (and its associated files) live.")


;;;
;;; 	iv.	Global variables and constants
;;;
;;; As should be expected, user visible and setable variables have a leading
;;; * on their doc string.

(defconst forms-version "1.3.0"
  "*Version of forms-mode implementation.")

(defvar forms-forms-scrolls t
  "*If non-null: redefine scroll-up/down to be used with forms-mode.")

(defvar forms-forms-jumps t
  "*If non-null: redefine beginning/end-of-buffer commands to be used with 
forms-mode.")

(defvar forms-mode-hooks nil
  "*Hook functions to be run upon entering forms mode.")

(defvar forms-query-on-entry-p t
  "*Ask for confirmation each time forms-mode is called.  Particularly 
important when setting up single file versions.")

(defvar forms-go-to-beginning-on-jump nil
  "*If non-null (default is nil), when jumping to a new record, move
point to be at beginning of record.")

(defvar forms-mode-load-hook nil
  "*Hook variable run after forms-mode is loaded.")

(defvar forms-bad-count-fields nil
  "*If forms-count-fields, or forms-show-record fails, the arguments are 
placed here to help the user debug.")
;; not made local, so always inspectable


;;;
;;; 	v.	Mandatory variables - must be set in/by the control file
;;;

(defvar forms-auto-report-hook nil
  "*Hook to put commands on to generate auto-report.")

(defvar forms-file nil
  "*Name of the file holding the data.")

(defvar forms-format-list nil
  "*List of formatting specifications.")


;;;
;;; 	vi.	Optional variables with default values
;;;

(defvar forms-read-only nil
  "Read-only mode (defaults to the write access on the data file).")

(defvar forms-multi-line "\C-k"
  "Character to separate multi-line fields (default ^K).")

(defvar forms-forms-scroll t
  "Redefine scroll-up/down to perform forms-next/prev-record when in
 forms mode.")

(defvar forms-forms-jump t
  "Redefine beginning/end-of-buffer to perform forms-first/last-record
 when in forms mode.")

(defvar forms-header-stop ";**** end of forms header ****
"
  "*Indicates the end of the header; must begin with a semi-colon, can't be 
set by command, must be done in user's .emacs file or in the source code.")

(defvar forms-comment-header-stop ";**** end of forms comment header ****"
  "*Indicates the end of the comment header; should begin with a semi-colon,
can't be set by command, must be done in user's .emacs file or in the 
source code.")

(defvar forms-field-sep "\t"
  "*Field separator character (default TAB).")

(defvar forms-record-separator "----------------\n")

(defvar forms-mode-map nil
   "Global keymap for forms buffer in read/write mode.")

(defvar forms-mode-commands-map nil
   "Global keymap with commands for forms buffer in read only mode.")

(defvar forms-mode-hidden-buffer-map nil
   "Keymap for the hidden guy.")

(defvar forms--field-sep-char "\t"
  "Field separator turned into a real char (character).")


;;;
;;; 	vi.	Internal variables
;;;
;;;  Internal variables and functions start with "--".

(defvar forms--blank-bag '(?\ ?\t ?:))

(defvar forms-number-of-fields nil
  "Number of fields per record.")

(defvar forms-quick-help-writable nil "Initial help for writable files.")

(defvar forms-quick-help-read-only nil "Initial help for read only files.")

(defvar forms--commands-header-final-pos nil
  "Marker indicating end of header position.")

(defvar forms--comments-header-final-pos nil
  "Marker indicating end of comment header position.")

(defvar forms--comments-header-initial-pos nil
  "Marker indicating end of comment header position.")

(defvar forms--comments-header ""
  "Where the comment header is kept upon occasion.")

(defvar forms--commands-header ""
  "Where the command header is kept upon occasion.")

(defvar forms--all-in-one nil
  "Set to t when the file has its header and data in the same file.")

(defvar forms--display-buffer nil
  "Buffer that file is displayed in.")

(defvar forms--file-buffer nil
  "Buffer that holds the file data.")

(defvar forms--comments-buffer nil
  "Buffer that holds the file comments data.")

(defvar forms--commands-buffer nil
  "Buffer that holds the file header commands data.")

(defvar forms--total-records 0
  "Total number of records in the data file.")

(defvar forms--current-record 0
  "Number of the record currently on the screen.")

(defvar forms--markers nil
  "Field markers in the screen.")

;(defvar forms--number-of-markers 0
;  "Number of fields on screen.")

(defvar forms--the-record-list nil 
   "List of strings of the current record, as parsed from the file.")

(defvar forms--search-regexp nil
  "Last regexp used by forms-search.")

(defvar forms--format nil
  "Formatting routine.")

(defvar forms--parser nil
  "Forms parser routine.")

(defvar forms--mode-setup nil
  "Internal - keeps track of forms-mode being set-up.")
(make-variable-buffer-local 'forms--mode-setup)

(defvar forms--new-record-filter nil
  "Internal - set if a forms-new-record-filter has been defined.")

(defvar forms--modified-record-filter nil
  "Internal - set if a modified record filter has been defined.")

(defvar forms--dynamic-text nil
  "Internal - holds the dynamic text inserted between fields, ie, text
that might change or that gets computed each time.  Set in
make-format-elt, used in make-parse.")

(defvar forms-fields nil
  "List with fields of the current forms. First field has number 1.")

(defvar forms--column 1
  "Current column when set up by forms--find-row-and-column.")
(defvar forms--row 1
  "Current row when set up by forms--find-row-and-column.")

(defvar forms--char-insertable-function nil)

(defvar forms--advanced-column-keywords
      '("Verbalization: " "Soar-action: " "Comments: .  "))

(defvar forms--maxlength-advanced-column-keywords
      (apply 'max (mapcar 'length forms--advanced-column-keywords)))

(defvar forms--first-advanced-row 0 
  "The first row in the current form with variable line length fields.")
(defvar forms--first-advanced-column 0
  "The first column of the first row with variable line length fields.")

;;; modification to global variables


;;(setq old-sentence-end  "[.?!][]\"')}]*\\($\\|	\\|  \\)[ 	
;;]*")
(setq old-sentence-end sentence-end)
(setq sentence-end
      (format "\\(%s\\)\\|%s" old-sentence-end forms-record-separator))


;;;
;;;	vii.	Requires and loads
;;;

(setq load-path
      (cons (expand-file-name forms-load-path)
      (cons (expand-file-name (concat forms-load-path "/utilities"))
            load-path)))

(require 'simple-menu)
(require 'forms-simple-menus)
(require 'my-picture)
(require 'cl)
(require 'soar-misc)
(require 'insert-date)
(require 'byte-compile "bytecomp")
(require 'ritter-math)


;;;
;;;	viii.	Known bugs
;;;
;;; Fixes can be posted back to Frank.  No killer bugs known (although double-
;;; file version may no longer work).
;;;
;;; Bugs (N:M)  N is first estimate of time to do in min, 
;;;     
;;;     1 - must do   2 - should do   
;;;     3 - do if in area, would be a feature  4 - imaginable feature

;;; 1 (10) some way to move current record to top of window
;;; 2 (25) dabbrev-expand needs to be nice to fixed width fields
;;; 2 (50) support tallys with tests
;;; 2 (40) procedures that count, add to comments header somehow
;;; 2 (15) query if want tally report in generated listing report
;;; 2 (60) add automaticly rechecked tallies to the report
;;; 2 (20) add the comments to the report, with a query first
;;; 2 (30) should use default values
;;; 2 (20) forms-open-line should allow an open-line at the very end/very
;;;   beginning of a record 
;;; 2 (100) an autosave function should be created.  I have notes on this, and 
;;;         now do just this thing for another system like this.... FER
;;; 3 (30) should check if forms-format-list has duplicate labels in 
;;;   forms--process-format-list
;;; 3 (30) set up tally report to give %'s
;;; 3 (15) ^k in open advanced fields (ie comments) inserts extra spaces
;;; 3 opening read-only files is screwed up.
;;; 3 (10) just-one-space must also be fixed to work in fixed with areas
;;; 3 if field label is just spaces, put name in as option
;;; 3 think about how add/remove-fields works with parsing and formating
;;;   commands
;;; 
;;; 4 hot text for menu selections for given fields
;;; 4 hidden fields appear to get munged
;;; 4 No truncate of fields
;;; 4 write a function to clear a column


;;;
;;;	ix.	Installation notes
;;;

;;; How I export it:
;;;         tar crvlf multi-forms-mode.tar forms.el my-picture.el \
;;;              forms-simple-menus.el test.forms \
;;;              README demo1.forms \
;;;              ../original/forms.ti \
;;;              utilities/ritter-math.el utilities/simple-menu.el \
;;;              utilities/soar-misc.el utilities/insert-date.el
;;;   compress multi-forms-mode.tar
;;;
;;; How you can unpack it:  uncompress forms-mode.tar.Z
;;;                  tar xf forms-mode.tar
;;;                  set forms-load-path directory so it can load
;;;                       (see below)
;;;
;;; You should bytecompile all the files.
;;;
;;; demo1.forms shows an example file that uses a separate data file.
;;; test.forms shows an example file that includes its own records.
;;;
;;; One of the best ways to get going is to examine these files, and
;;; modify them to suit your needs.
;;;
;;; This file (forms.el) must also be loaded with a (load "forms") or 
;;; an (autoload 'forms-mode "forms-mode").


;;;
;;; 	I.	forms-mode
;;;
;;; This is not a typical or simple major mode.
;;; forms-mode takes an optional argument 'primary', which is used for 
;;; the initial set-up.  It forces the buffer into forms-mode, or resets 
;;; the mode is the buffer is already in forms-mode.  Normal use and 
;;; users will not see or need PRIMARY and should leave it nil.
;;;
;;; A global buffer-local variable 'forms--mode-setup' has the same effect
;;; but makes it possible to auto-invoke forms-mode using find-file.
;;;
;;; Note: although it seems logical to have (make-local-variable) executed
;;; where the variable is first needed, I (JV) deliberately placed all calls
;;; in the forms-mode function.
 
(defun forms-mode (&optional primary no-query)
  "Major mode to visit files in a field-structured manner using a form.
If PRIMARY is t, then entry is treated as first call and all initializations 
are run.  If NO-QUERY is t, then don't ask the user.
 Key map used when writeable:
 \\{forms-mode-map}
 Commands prefixed with C-c when writeable mode,
     and available directly in read-only mode):
 \\{forms-mode-commands-map}"
  ;; we also reset the point, which is ok on entry, but not good on repeated
  ;; calls.

  (interactive)				; no - 'primary' is not prefix arg
  (if (or (not forms-query-on-entry-p)
          no-query
          (y-or-n-p (format "Put %s into forms mode? " (buffer-name))))
      (forms--mode-helper primary)))

(defun forms--mode-helper (&optional primary)
 ;; Primary set-up: evaluate buffer and check if the mandatory
 ;; variables have been set.
 (message "Starting forms-mode up...")
 (if (or primary (not forms--mode-setup))
     (progn
       (forms--make-local-variables)
       (goto-char (point-min)) ; in case it was opened first
       (if (not (= 1 (forms--set-commands-header-final-pos)))
           (setq forms--all-in-one t))
       ;; eval the buffer, should set variables
       ;; this way it is backwardly compatible ;-fer
       (setq forms--display-buffer (current-buffer))
       (if forms--all-in-one
           (forms--setup-all-in-one)
           (progn (eval-current-buffer)
                  ;; find the data file
                  (setq forms--file-buffer (find-file-noselect forms-file)) ) )

	;; prepare the display buffer for further processing
      	;; and clean it
        (setq buffer-read-only nil)
        (erase-buffer)

        (forms--initialize-functions)
	;; prevent accidental overwrite of the control file and auto-save
	(setq buffer-file-name nil)
         ))
  (message "Starting forms-mode up....")
  (if forms-read-only
      (use-local-map forms-mode-commands-map)
      (use-local-map forms-mode-map))
  (forms--set-minor-mode)

  ;; count the number of records, and set see if it may be modified
  (setq forms--total-records
        (save-excursion
	    (set-buffer forms--file-buffer)
            (set-buffer-modified-p nil)
	    (bury-buffer (current-buffer))
            (if buffer-read-only
                (setq forms-read-only t))
	    (count-lines (point-min) (point-max))))

  ;; set the major mode indicator
  (setq major-mode 'forms-mode)
  (setq mode-name "Forms")
  (message "Starting forms-mode up.....")
  ;; setup the first (or current) record to show, but only on entry
  (if (not forms--mode-setup)
      (progn
         (forms--draw-all-records forms--display-buffer)
         (goto-char (point-min))
         (set-buffer-modified-p nil)
         (forms--set-mode-line)
         (auto-fill-mode -1))) ;make sure auto-fill is off

  ;; run user customising &  be helpful
  (run-hooks 'forms-mode-hooks)
  (forms-quick-help)
  (setq forms--mode-setup t))
;; end forms--mode-helper

(defun forms--initialize-functions ()
  ;; check if the mandatory variables make sense.
  (forms--check-local-variables)
  ;; validate and process forms-format-list
  (forms--process-format-list)
  ;; build the formatter and parser
  (forms--make-char-insertable-function)
  (forms--make-format)
  (forms--make-parser)
  (forms--make-forms-query-list)

  ;; check if record filters are defined
  (forms--make-record-filters))

(defun forms--setup-all-in-one ()
 (save-excursion
   ;; set up the display buffer
   (setq forms-read-only (if buffer-read-only t nil))
   (setq forms--commands-header 
         (buffer-substring 1 forms--commands-header-final-pos))
   ;; set up the file-buffer
   (setq forms--file-buffer-name (concat "*forms-" (buffer-name) "*"))
   (setq forms--file-buffer (get-buffer-create forms--file-buffer-name))
   (forms--call-commands)
   (forms--make-file-buffer)
   (erase-buffer)))

(defun forms--make-file-buffer ()
  "Make the file-buffer (back buffer) for the current forms file."
  (save-excursion
  (let ((temp-forms-body (buffer-substring (point-min) (point-max)))
        (original-file-name (buffer-file-name (current-buffer))) )
    (setq forms-file original-file-name)
    (set-buffer forms--file-buffer)
    (make-local-variable 'forms--commands-header-final-pos)
    (make-local-variable 'forms--comments-header-final-pos)
    (make-local-variable 'forms--comments-header-initial-pos)

    (setq forms--commands-header-final-pos (make-marker))
    (setq forms--comments-header-final-pos (make-marker))
    (setq forms--comments-header-initial-pos (make-marker))
    (use-local-map forms-mode-hidden-buffer-map)
    (setq buffer-auto-save-file-name (concat original-file-name "~"))
    (erase-buffer)
    (insert-before-markers temp-forms-body)
    (goto-char (point-min))
    (forms--set-commands-header-final-pos)
    (forms--call-commands)
    (forms--parse-comments)
    (narrow-to-region (+ 1 forms--comments-header-final-pos
                         (length forms-comment-header-stop))
                      (point-max))
    (goto-char (point-min)))))

(defun forms--call-commands ()
  "Calls the commands in buffer, and sets up the comment string."
  ;; called on each of the main buffer and the back buffer
  (narrow-to-region 1 forms--commands-header-final-pos) ; 1 is point-min
  (eval-current-buffer)
  (widen))

(defun forms--set-commands-header-final-pos ()
 ;; sets the forms--commands-header-final-pos as a marker
 (set-marker forms--commands-header-final-pos  ;this is bolp of command
    (save-excursion (search-forward forms-header-stop nil t)
                    (- (point) (length forms-header-stop)))))

(defun forms--parse-comments ()
  (set-marker forms--comments-header-initial-pos
        (+ forms--commands-header-final-pos (length forms-header-stop)))
  (set-marker forms--comments-header-final-pos
        (save-excursion (search-forward forms-comment-header-stop nil t)
                        (beginning-of-line)
                        (point) ))
  (setq forms--comments-header
        (buffer-substring forms--comments-header-initial-pos
                          forms--comments-header-final-pos)))

(defun forms--make-forms-query-list ()
  (setq forms--format-query-list nil)
  (mapcar '(lambda (x)
             (let ((field (format-item-field x))
                   (new-label (string-trim forms--blank-bag
                                           (format-item-label x))))
               (if (numberp field)
                   (push (cons new-label field)
                         forms--format-query-list))))
          forms-format-list))

(defun forms--auto-save-file-name ()
  (concat default-directory (buffer-name)))


;;;
;;; 	II.	other mode helpers
;;;
;;; One could just use the %% as an indicator.  Rarely do you really look at
;;; files you can't touch, and in those cases, you'll find out quickly enough.

(defun forms--set-minor-mode ()
  (setq minor-mode-alist (if forms-read-only " View" nil)))

(defun forms-view-mode ()
  "Visit buffer read-only."
  (interactive)
  (if forms-read-only
      nil
    (setq forms-read-only t)
    (forms-mode nil t)
    (forms-quick-help))  )

(defun forms-edit-mode ()
  "Make form suitable for editing, if possible."
  (interactive)
  (let ((read-only forms-read-only) )
    (if (save-excursion (set-buffer forms--file-buffer)
                        buffer-read-only)
	(progn (setq forms-read-only t)
               (message "No write access to \"%s\"" forms-file)
               (beep))
      (setq forms-read-only nil))
    (if (equal read-only forms-read-only)
	nil
      (forms-mode nil t))))

(defun forms-revert-record (&optional arg noconfirm)
 "Reverts current form to un-modified."
 (interactive "P")
 (if (or noconfirm
         (yes-or-no-p 
              "Revert current record (not whole buffer) to unmodified? "))
     (progn (set-buffer-modified-p nil)
            (forms-jump-record forms--current-record))))

(defvar forms--comments-recursive-edit nil) ;make local later

(defun forms-edit-comments-header ()
  "Edit and view the comments of the buffer."
  (interactive)
  (forms--set-mode-line)
  (if forms--comments-recursive-edit
      (switch-to-buffer-other-window forms--comments-recursive-edit)
  (let ((comment-header (save-excursion
                          (set-buffer forms--file-buffer)
                          forms--comments-header))
        (old-buffer (current-buffer)) 
        new-buffer)
    (save-excursion
    (save-window-excursion		; Prepare buffer
      (setq new-buffer
        (forms--setup-buffer-create 'forms--comments-buffer
              "*forms-comments-" comment-header "COMMENTS" (buffer-name)))
      (message (substitute-command-keys
		"Type replacement and exit with \\[exit-recursive-edit]"))
      (save-excursion (set-buffer old-buffer)
                      (setq forms--comments-recursive-edit new-buffer))
      (recursive-edit)
      (set-buffer-modified-p nil)
      (save-excursion (set-buffer old-buffer) 
                      (setq forms--comments-recursive-edit nil))
      ;; Wait for return from recursive edit
      (setq forms--comments-header (buffer-substring (point-min) (point-max)))
      (delete-windows-on forms--comments-buffer)))
    (bury-buffer forms--comments-buffer)
    ;; stuff the new comment into the file
    (save-excursion
      (set-buffer forms--file-buffer)
      (widen)
      ;; may have to do some fancy dancing to keep markers separated & correct
      (delete-region forms--comments-header-initial-pos
                     forms--comments-header-final-pos)
      (goto-char forms--comments-header-initial-pos)
      (insert forms--comments-header)
      (set-marker forms--comments-header-final-pos
            (+ forms--comments-header-initial-pos
               (length forms--comments-header)))
      (narrow-to-region
        (+ 1 forms--comments-header-final-pos
           (length forms-comment-header-stop))
        (point-max)))
    (forms--set-mode-line)  )))

;; select and set up a buffer to edit
(defun forms--setup-buffer-create
 (buffer-symbol-name extension-string contents window-label old-buffer)
 (let* ((new-buffer-name (concat extension-string (buffer-name) "*"))
        (new-buffer (get-buffer new-buffer-name)) )
 (if (bufferp new-buffer)
     (progn   )
   (setq new-buffer
         (set buffer-symbol-name (get-buffer-create new-buffer-name)))
   (set-buffer new-buffer)
   (use-local-map forms-mode-hidden-buffer-map)
   (erase-buffer)
   (insert-before-markers contents)
   (set-buffer-modified-p nil)
   (goto-char (point-min))
   (setq mode-line-format
         (forms--make-edit-subtext-mode-line-format window-label old-buffer)))
 (if (not (get-buffer-window new-buffer))
     (progn (switch-to-buffer-other-window new-buffer)
            (shrink-window -3))
   (switch-to-buffer new-buffer))
 new-buffer))


(defvar forms--commands-recursive-edit nil) ;make local later

;;; main buffer is write protected during commands edit
(defun forms-edit-commands-header ()
  "Edit and view the commands-header of the buffer."
  (interactive)
  (forms--set-mode-line)
  (if forms--commands-recursive-edit
      (switch-to-buffer-other-window forms--commands-recursive-edit)
  (forms--update-all)  ; save all stuff to back buffer here....
  (let ((old-buffer (current-buffer))
        must-rebuild new-buffer header new-header-stuff
        (old-forms-format-list forms-format-list)
        (old-buffer-read-only buffer-read-only))
    (if (not old-buffer-read-only) (toggle-read-only)) 
    (save-excursion
    (save-window-excursion		; Prepare buffer
      (setq new-buffer
        (forms--setup-buffer-create 'forms--commands-buffer
           "*forms-commands-" forms--commands-header "COMMANDS" (buffer-name)))
      (message (substitute-command-keys
		"Type replacement and exit with \\[exit-recursive-edit]"))
      (save-excursion (set-buffer old-buffer)
                      (setq forms--commands-recursive-edit new-buffer))
      (recursive-edit)
      ;; Wait for return from recursive edit
      (if (buffer-modified-p)
          (progn
            (setq new-header-stuff t)
            (set-buffer-modified-p nil)
            (setq header (buffer-substring (point-min) (point-max)))
            ;; do a test eval & let it bomb here if wouldn't work
           (eval-current-buffer)  
           (setq must-rebuild 
                 (not (equal old-forms-format-list forms-format-list))))
         (setq must-rebuild nil))
      (save-excursion (set-buffer old-buffer) 
                      (setq forms--commands-recursive-edit nil))
      (delete-windows-on forms--commands-buffer)
      (bury-buffer forms--commands-buffer)))
    (if (not old-buffer-read-only) (toggle-read-only))
    (if new-header-stuff
        (progn
           (forms--save-and-eval-new-header)
           (if must-rebuild
              (forms--rebuild-display-buffer)
              (message "No major changes, you get off easy.")))
      (message "No changes at all, you get very easy.")))))


(defun forms--save-and-eval-new-header ()
  ;; starting in display-buffer
  (save-excursion
    (set-buffer forms--file-buffer)
    (widen)
    (delete-region 1 forms--commands-header-final-pos)
    (goto-char 1) ;1 is (point-min)
    (setq forms--commands-header header)
    (insert-before-markers forms--commands-header)
    ;; this should be set automagically now
    ;;(setq forms--commands-header-final-pos (length forms--commands-header))
    (forms--call-commands)    ;(forms--parse-comments)
    (narrow-to-region
      (+ 1 forms--comments-header-final-pos (length forms-comment-header-stop))
      (point-max)) )
  (forms--set-mode-line)
  (save-excursion
    (goto-char 1)
    (setq forms--commands-header header)
    (insert-before-markers forms--commands-header)
    ;; (setq forms--commands-header-final-pos (length forms--commands-header))
    (forms--call-commands)
    (delete-region 1 forms--commands-header-final-pos)) ;used to be 1+
    (set-buffer-modified-p nil)  )

(defun forms--rebuild-display-buffer ()
  ;; vars are bound by calling function, used for clarity for now...
  (let ((start-rec forms--current-record)
        (start-row forms--row))
    (message "Significant changes, must rebuild display with new commands...")
    (bury-buffer forms--commands-buffer)
    (erase-buffer)
    (forms--initialize-functions)    
    (forms--draw-all-records forms--display-buffer)
    (forms-jump-record start-rec)
    (forward-line (- 1 start-row))
    ;do choice-parse?
    (forms--initialize-functions)
    (message "Rebuilding file with new commands...Finished.")
    (set-buffer-modified-p nil)
    (forms--set-mode-line)))

(defun forms-view-auto-report-header ()
  "View the auto-report-header of the buffer."
 (interactive)
 (let (start save-buffer
       (old-current-record forms--current-record)
       (header forms--commands-header)
       (display-buffer
        (get-buffer-create (concat "*forms-auto-report-" (buffer-name) "*"))) )
    (setq start (point))		; Save current location
    (save-window-excursion		; Prepare buffer
      (setq save-buffer (buffer-name))
      (switch-to-buffer-other-window display-buffer)
      (erase-buffer)
      (run-hooks 'forms-auto-report-hook)
      (goto-char (point-min))
      (set-buffer-modified-p nil)
    ;; stuff the new report into the file?
  )))

  ;; note good cog eng here, we make the command to get out always visible
  ;; also simplified the mode line considerably

;(forms--edit-subtext-mode-line-format 'commands 'buffer-name)
(defun forms--make-edit-subtext-mode-line-format (type buffer-name)
 "Set up the mode-line for a header section of buffer."
  (list
     mode-line-modified
    (format "{%s} of %s" type buffer-name)
    global-mode-string
    (substitute-command-keys
        " %[(\\[exit-recursive-edit] to exit, \\[describe-mode] for help)%]")
    "---"
   '(-3 . "%p")  "-%-"))


;;;
;;;	III.	forms--make-record-filters
;;;

(defun forms--make-record-filters ()
  "Check if record filters are defined and define them iff necc."
  (setq forms--new-record-filter 
        (cond ((fboundp 'forms-new-record-filter)
               (symbol-function 'forms-new-record-filter))
              ((and (boundp 'forms-new-record-filter)
                    (fboundp forms-new-record-filter))
               forms-new-record-filter)))
  (fmakunbound 'forms-new-record-filter)
  (make-local-variable 'forms--modified-record-filter)
  (setq forms--modified-record-filter 
        (cond ((fboundp 'forms-modified-record-filter)
               (symbol-function 'forms-modified-record-filter))
              ((and (boundp 'forms-modified-record-filter)
                    (fboundp forms-modified-record-filter))
               forms-modified-record-filter)))
  (fmakunbound 'forms-modified-record-filter)  )


;;;
;;;	IV.	Set up and check local variables
;;;

(defun forms--check-local-variables ()
  "Check if the mandatory variables make sense."
  (or forms-file (error "'forms-file' has not been set"))
  (or (stringp forms-field-sep)
      (error "'forms-field-sep' is not a string"))
  (setq forms--field-sep-char (string-to-char forms-field-sep))
  (or forms-number-of-fields
      (setq forms-number-of-fields
            (save-excursion (set-buffer forms--file-buffer)
                 (forms--set-number-of-fields)))
      (error "'forms-number-of-fields' has not been set"))
  (or (> forms-number-of-fields 0)
      (error "'forms-number-of-fields' must be > 0"))
  (if forms-multi-line
      (if (and (stringp forms-multi-line)
               (eq (length forms-multi-line) 1))
          (if (string= forms-multi-line forms-field-sep)
              (error "'forms-multi-line' is equal to 'forms-field-sep'"))
          (error "'forms-multi-line' must be nil or a one-character string")))
  )    


(defun forms--make-local-variables ()
  "Make all the local variables that forms-mode needs."
  (kill-all-local-variables)
  ;; make mandatory variables
  (make-local-variable 'forms-file)
  (make-local-variable 'forms-number-of-fields)
  (setq forms-number-of-fields nil)
  (make-local-variable 'forms-format-list)
  (make-local-variable 'forms--all-in-one)
  (make-local-variable 'forms-auto-report-hook)

  ;; make optional variables
  (make-local-variable 'forms-field-sep)
  (make-local-variable 'forms--field-sep-char)
  (make-local-variable 'forms-read-only)
  (make-local-variable 'forms-multi-line)
  (make-local-variable 'forms-forms-scroll)
  (make-local-variable 'forms-forms-jump)
  ;; set up the markers
  (make-local-variable 'forms--commands-header-final-pos)
  (make-local-variable 'forms--comments-header-final-pos)
  (make-local-variable 'forms--comments-header-initial-pos)

  (setq forms--commands-header-final-pos (make-marker))
  (setq forms--comments-header-final-pos (make-marker))
  (setq forms--comments-header-initial-pos (make-marker))

  (make-local-variable 'forms--commands-header)
  (make-local-variable 'forms--comments-header)
  (make-local-variable 'forms--commands-recursive-edit)
  (make-local-variable 'forms--comments-recursive-edit)
                        
  ;; record-filter stuff
  (make-local-variable 'forms--new-record-filter)
  (fmakunbound 'forms-new-record-filter)

  ;; make local variables; second batch from forms-mode
  ;; these are the working stack
  (make-local-variable 'forms--file-buffer)
  (make-local-variable 'forms--comments-buffer)
  (make-local-variable 'forms--commands-buffer)
  (make-local-variable 'forms--total-records)
  (make-local-variable 'forms--current-record)
  (make-local-variable 'forms--the-record-list)
  (make-local-variable 'forms--search-rexexp)

  ;; dynamic text support
  (make-local-variable 'forms--dynamic-text)
  (make-local-variable 'forms-fields)

  ;; We have our own (partial) revert function - use it
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'forms-revert-record)

  ;; variables for format processing
;  (make-local-variable 'forms--number-of-markers)
  (make-local-variable 'forms--markers)

  (make-local-variable 'forms--format)
  (make-local-variable 'forms--parser)

  ;; new stuff, mostly for new insertion functions
  (make-local-variable 'forms--display-buffer)
  (make-local-variable 'forms--column)
  (make-local-variable 'forms--row)
  (make-local-variable 'forms--char-insertable-function)
  (make-local-variable 'forms--advanced-column-keywords)
  (make-local-variable 'forms--first-advanced-row)
  (make-local-variable 'forms--first-advanced-column)
  (make-local-variable 'forms--maxlength-advanced-column-keywords)
)


;;;
;;; 	V.	Set up the format (printing out ) stuff
;;;
;;; Validates forms-format-list.
;;;
;;; Sets forms--number-of-markers and forms--markers.

(defun forms--process-format-list ()
  "Validate forms-format-list and set some global variables."
  (forms--debug "forms-forms-list before 1st pass:\n" 'forms-format-list)
  ;; it must be non-nil
  (or forms-format-list (error "'forms-format-list' has not been set"))
  ;; it must be a list ...
  (or (listp forms-format-list) (error "'forms-format-list' is not a list"))
  ;; it must be made out of lists
  (if forms--all-in-one
      (mapcar '(lambda (x) 
                 (if (not (listp x))
                     (error "forms-format-list sublist >>%s<< is not a list." 
                             x)))
               forms-format-list))
  ;; check for duplicate labels here:

  ;(setq forms--number-of-markers 0)

  (let ((the-list forms-format-list)	; the list of format elements
	(this-item 0)			; element in list
	(field-num 0))			; highest field number 

   (setq forms-format-list nil)	; gonna rebuild

   (while the-list
     (let* ((el-list (car-safe the-list))
            (el (format-item-field el-list))
            (rem (cdr-safe the-list)))

      ;; if it is a symbol, eval it first
      (if (and (symbolp el) (boundp el))
          (setq el (eval el)))
      (cond
	((stringp el))			; string is OK
	((numberp el)
	 (if (or (<= el 0)
	         (> el forms-number-of-fields))
	     (error "Forms error: field number %d out of range 1..%d"
	            el forms-number-of-fields))
	 ;(setq forms--number-of-markers (1+ forms--number-of-markers))
	 (if (> el field-num)
	     (setq field-num el)))
	((and (listp el)
              (not el))  ;it's nil
         (setq el ""))
	((listp el)     ; try s-exp
	 (or (fboundp (car-safe el))
	     (error "Forms error: not a function: %s"
	            (prin1-to-string (car-safe el)))))
	(t
	  (error "Invalid element in 'forms-format-list': %s"
		 (prin1-to-string el))))

      ;; advance to next element of the list
      (forms--replace-format-item 3 el-list el) ;this changes el
      (setq the-list rem)
      (setq forms-format-list
	    (append forms-format-list (list el-list))))))

  (forms--debug "forms-forms-list after 1st pass:\n" 'forms-format-list)

  ;; concat adjacent strings
  (setq forms-format-list (forms--concat-adjacent forms-format-list))

  (forms--debug "forms-forms-list after 2nd pass:\n")
		;'forms-format-list 'forms--number-of-markers

  ;(setq forms--markers (make-vector forms--number-of-markers nil))
  )
  ;; end - forms--process-format-list

;;;
;;; Build the format routine from forms-format-list.
;;;
;;; The format routine (forms--format) will look like
;;; 
;;; (lambda (arg)
;;;   (setq forms--dynamic-text nil)
;;;   ;;  "text: "
;;;   (insert "text: ")
;;;   ;;  6
;;;   (aset forms--markers 0 (point-marker))
;;;   (insert (elt arg 5))
;;;   ;;  "\nmore text: "
;;;   (insert "\nmore text: ")
;;;   ;;  (tocol 40)
;;;   (let ((the-dyntext (tocol 40)))
;;;     (insert the-dyntext)
;;;     (setq forms--dynamic-text (append forms--dynamic-text
;;;					  (list the-dyntext))))
;;;   ;;  9
;;;   (aset forms--markers 1 (point-marker))
;;;   (insert (elt arg 8))
;;;
;;;   ... )
;;; 

(defun forms--make-format ()
  "Generate format function for forms."
  (setq ff  (setq forms--format (forms--format-maker forms-format-list)))
  (setq forms--format (byte-compile-lambda forms--format))
  (forms--debug 'forms--format))

(defun forms--format-maker (the-format-list)
  "Returns the formating function for forms."
  (let ((the-marker 0))
    (` (lambda (arg)
	 (setq forms--dynamic-text nil)
         (forms--insert-line-number forms--current-record)
	 (,@ (apply 'append
		    (mapcar 'forms--make-format-elt the-format-list)))))))

(defun forms--insert-line-number (i)
  (insert (format "%d " i))
  (insert forms-record-separator))


;; arg gets bound in these guys' run time environment
(defun forms--make-format-elt (item-list)
 (let ((el (format-item-field item-list))
       (label (format-item-label item-list))
       (name (format-item-name item-list))
       (field-size (format-item-field-size item-list))
       (newline-p (format-item-newline-p item-list))
       (line-size (format-item-line-size item-list))   )
  (cond
   ((stringp el)
    (` ( (if (forms--item-list-visible-p '(, name))
             (progn
              (insert-n-times "\n" (, newline-p))
              (insert (, label))
              (if nil ' string)
              (forms--insert (, el) (, field-size) (, line-size))))
            )))
   ((numberp el)
    (prog1
      (` ( (if (forms--item-list-visible-p '(, name))
               (progn
                (insert-n-times "\n" (, newline-p))
                (insert (, label))
                (if forms--markers
                    (aset forms--markers (, the-marker) (point-marker)))
                (forms--insert (elt arg (, (1- el)))
                               (, field-size) (, line-size))))))
      (setq the-marker (1+ the-marker))))
   ((listp el)
    (let ((the-dyntext (eval el)))
      (` ((if (forms--item-list-visible-p '(, name))
              (progn
                (insert-n-times "\n" (, newline-p))
                (insert (, label))
                (forms--insert (, the-dyntext) (, field-size) (, line-size))
                (setq forms--dynamic-text (append forms--dynamic-text
                                                  (list (, the-dyntext)))) )))
      )))  )))

(defun forms--insert (el field-size line-size)
  "Insert record ELement based on FIELD-SIZE and LINE-SIZE."
  (let ((e-length (length el))
        (eline-size el)
        (start (point))
        eline-length)
    (if (and (numberp field-size) (> e-length field-size))
        (setq el (substring el 0 field-size)))
    (insert el)
    (if (and (numberp field-size) (< e-length field-size))
        (insert-n-times " " (- field-size e-length)))
    (setq eline-length (count-lines start (point)))
    (if (and (numberp line-size) (< eline-length line-size))
        (insert-n-times "\n" (- line-size eline-length)))  ))

(defun forms--concat-adjacent (the-list)
  "Concatenate adjacent strings in the-list and return the resulting list."
  (if (consp the-list)
      (let ((the-rest (forms--concat-adjacent (cdr the-list))))
	(if (and (stringp (car the-list)) (stringp (car the-rest)))
	    (cons (concat (car the-list) (car the-rest))
		  (cdr the-rest))
	    (cons (car the-list) the-rest)))
      the-list))


;;;
;;; 	VI.	Set up the parsing (rereading records) routines
;;;
;;; Generate parse routine from forms-format-list.  They parse for 
;;; form correctness, and also put the findings into a vector called
;;; the-recordv.
;;;
;;; The parse routine (forms--parser) will look like (give or take
;;; a few " "'s. [this is an unrevised comment -FER]
;;; 
;;; (lambda nil
;;;   (let (here)
;;;     (goto-char (point-min)))
;;; 
;;;	;;  "text: "
;;;     (if (not (looking-at "text: "))
;;; 	    (error "Parse error: cannot find \"text: \""))
;;;     (forward-char 6)	; past "text: "
;;;     ;;  6
;;;	;;  "\nmore text: "
;;;     (setq here (point))
;;;     (if (not (search-forward "\nmore text: " nil t nil))
;;; 	    (error "Parse error: cannot find \"\\nmore text: \""))
;;;     (aset the-recordv 5 (buffer-substring here (- (point) 12)))
;;;
;;;	;;  (tocol 40)
;;;	(let ((the-dyntext (car-safe forms--dynamic-text)))
;;;	  (if (not (looking-at (regexp-quote the-dyntext)))
;;;	      (error "Parse error: not looking at \"%s\"" the-dyntext))
;;;	  (forward-char (length the-dyntext))
;;;	  (setq forms--dynamic-text (cdr-safe forms--dynamic-text)))
;;;     ... 
;;;     ;; final flush (due to terminator sentinel, see below)
;;;	(aset the-recordv 7 (buffer-substring (point) (point-max)))))
;;; 

(defun forms--make-parser ()
  "Generate parser function for forms."
  (setq forms--parser (forms--parser-maker forms-format-list))
  ;; boy, this sure doesn't look like it saves us much...
  (setq forms--parser (byte-compile-lambda forms--parser))
  (forms--debug 'forms--parser))

(defun forms--parser-maker (the-format-list)
  "Returns the parser function for forms."
  (let ((the-field nil)  ; temp variable storing the last field parsed
	(seen-text nil)
	the--format-list
        (last-field-size nil))
    ;; last-field-size is set to the previous clause's field size, used to catch
    ;; errors.
    ;; add a terminator sentinel
    ;(setq the--format-list (append the-format-list (list nil)))
    ;; not longer needed?
    (setq the--format-list the-format-list)
    (` (lambda nil
	 (let (here max-record-point)
           (setq max-record-point
                 (or (save-excursion
                       (if (search-forward forms-record-separator nil t)
                           (progn (forward-line -1) (forward-char -1)
                                  (point))))
                     (point-max)))
           (search-backward forms-record-separator nil t)
           (search-forward forms-record-separator nil t)
	 (,@ (apply 'append
		    (mapcar 'forms--make-parser-elt the--format-list))))))))


(defun forms--make-parser-elt (item-list)
 (let ((el (format-item-field item-list))
       (name (format-item-name item-list))
       (label (format-item-label item-list))
       (field-size (format-item-field-size item-list))
       (newline-p (format-item-newline-p item-list))
       (line-size (format-item-line-size item-list))        )
 (if (and (not newline-p) (eq last-field-size t))
     (error
   "Must have 't' new-line after fullsize field [clause=%s  l=%s]."
             name label))
 (setq last-field-size field-size)
 (cond
  ((stringp el)
   (` ((if (forms--item-list-visible-p '(, name))
           (progn
       (if (, newline-p) (progn (forward-line) (beginning-of-line)))
       (if (and (, label)
                (not (search-forward (, label) max-record-point t nil)))
           (error "Parse error: cannot find \"%s\" [in #%s point=%s]" 
                  (+ 1 i) (, label) (point) ))
       (if (and (not (string= (, el) ""))
                (not (search-forward (, el) max-record-point t nil)))
           (error "Parse error: cannot find \"%s\" [in #%s point=%s]" 
                  (, el) (+ 1 i) (point))))
       ) )))
  ((numberp el)
   (` ((if (forms--item-list-visible-p '(, name))
           (progn
       (if (, newline-p) (progn (forward-line) (beginning-of-line)))
       (if (and (, label)
                (not (forms--search-forward (, label) max-record-point t nil)))
	   (error "Parse error: cannot find \"%s\" [in #%s point=%s]" 
                  (, label) (+ 1 i) (point)))
       (setq here (point))
       (forms--elt-parse (, field-size) (, line-size) '(, name))
       (aset the-recordv (, (1- el))
             (buffer-substring here (point)))))
       ) ))
  ((null el)
   (` ((if (forms--item-list-visible-p '(, name))
           (progn
       (if (, newline-p) (progn (forward-line) (beginning-of-line)))
       (if (and (, label)
                (not (search-forward (, label) max-record-point t nil)))
	   (error "Parse error: cannot find \"%s\" [in #%s point=%s]" 
                  (, label) (+ 1 i) (point)))))
      ) ) )
  ((listp el)
   (` ((if (forms--item-list-visible-p '(, name))
           (progn
       (if (, newline-p) (progn (forward-line) (beginning-of-line)))
       (if (and (, label)
                (not (search-forward (, label) max-record-point t nil)))
           (error "Parse error: cannot find \"%s\" [in #%s point=%s]" 
                  (, label) (+ 1 i) (point)))
       (setq here (point))
       (let ((the-dyntext (car-safe forms--dynamic-text)))
         (if (not (looking-at (regexp-quote the-dyntext)))
             (error "Parse error: not looking at \"%s\" [in #%s point=%s]" 
                    the-dyntext (+ 1 i) (point)))
         (forward-char (length the-dyntext))
         (setq forms--dynamic-text (cdr-safe forms--dynamic-text)))))
      ) ))
  )))

(defun forms--elt-parse (field-size line-size name)
 "Set the point at the end of the forms size."
 ;; field-size can be # or t (end of line)
 ;; line-size can be #, nil or t (till next known label), or next record sep
 (let (search-item)
 (if (not line-size)
     (if (numberp field-size)
         (forms--elt-parse-forward-char field-size)
       (end-of-line))
   ;; else if line-size
   (if (numberp line-size)
       (progn (forward-line line-size)
              (end-of-line))
     (if (setq search-item (format-item-label (forms--next-visible-item name)))
         (if (search-forward search-item max-record-point 'to-end)
             (progn (beginning-of-line)
                    (backward-char 1)))
       (setq search-item forms-record-separator)
       (if (search-forward search-item (point-max) 'to-end)
           (progn (forward-line -1)
                  (forward-char -1))))))))

(defun forms--elt-parse-forward-char (field-size)
  "Forward field-size chars or to end of line, whichever comes first."
  (let ( (line-end (save-excursion (end-of-line) (point))) )
  (forward-char field-size)
  (if (> (point) line-end)
      (goto-char line-end))
))

(defun forms--parse-form ()
  "Parse contents of form into list of strings."
  ;; The contents of the form are parsed, and a new list of strings
  ;; is constructed.
  ;; A vector with the strings from the original record is 
  ;; constructed, which is updated with the new contents. Therefore
  ;; fields that were not in the form are not modified.
  ;; Finally, the vector is transformed into a list for further processing.

  (let (the-recordv)
    ;; build the vector
    (setq the-recordv (vconcat forms--the-record-list))
    ;; parse the form and update the vector
    (let ((forms--dynamic-text forms--dynamic-text))
      (funcall forms--parser))
    (if forms--modified-record-filter
	;; As a service to the user, we add a zeroth element so she
	;; can use the same indices as in the forms definition.
	(let ((the-fields (vconcat [nil] the-recordv)))
	  (setq the-fields (funcall forms--modified-record-filter the-fields))
	  (cdr (append the-fields nil)))
      ;; transform to a list and return
      (append the-recordv nil))))


;;;
;;;	VII.	Set up the keymaps
;;;
;;; Set the keymaps used in this mode.

(defvar forms-prefix "\C-c" "Key that forms-mode commands hang off of.")

(defun forms--defkey (keymap key command)
  "Define KEYMAP forms-prefix+KEY as command."
  ;; if form-prefix has been bound to nil, then just set the key as is...
  (let ((prefix-map (if forms-prefix
                        (lookup-key keymap forms-prefix)
                      keymap)) )
    (if (not (keymapp prefix-map))
	(setq prefix-map
	      (define-key keymap forms-prefix (make-sparse-keymap))))
    (define-key prefix-map key command)))

(defun forms--mode-commands (map)
  "Fill MAP with all forms commands."
  ;; this is used by both keymaps; commands are prefixed with forms-prefix
  ;; (often C-c)
  (forms--defkey map "2"    'forms-duplicate-record)
  (forms--defkey map "\t"   'forms-next-field)
  (forms--defkey map " "    'forms-next-record)
  (forms--defkey map "b"    'forms-prev-record)
  (forms--defkey map "\C-c" 'forms-edit-comments-header)
  (forms--defkey map "d"    'forms-delete-record)
  (forms--defkey map "e"    'forms-edit-mode)
  (forms--defkey map "\C-h" 'forms-edit-commands-header)
  (forms--defkey map "i"    'forms-insert-record)
  (forms--defkey map "j"    'forms-jump-record)
  (forms--defkey map "m"    'forms-run-menu)
  (forms--defkey map "\C-m" 'forms-run-menu)
  (forms--defkey map "n"    'forms-next-record)
  (forms--defkey map "p"    'forms-prev-record)
  (forms--defkey map "q"    'forms-exit)
  (forms--defkey map "s"    'forms-search)
  (forms--defkey map "v"    'forms-view-mode)
  (forms--defkey map "x"    'forms-exit-no-save)
  (forms--defkey map "<"    'forms-end-of-buffer)
  (forms--defkey map ">"    'forms-beginning-of-buffer)
  (forms--defkey map "?"    'describe-mode)
  ;; del seems like a bad thing to encourage people to hit
  ;(forms--defkey map "\177" 'forms-prev-record)  ; \177 is ^?
  )

(defun forms--new-command-mappings (map)
  ;; these guys are always on the top map, and represent overides of global
  ;; bindings
  (define-key map "\M-\C-a" 'forms-beginning-of-form)
  (define-key map "\M-\C-e" 'forms-end-of-form)
  (define-key map "\M-v" 'forms-scroll-down)
  (define-key map "\C-v" 'forms-scroll-up)
  (define-key map "\C-_" 'forms-undo)
  (define-key map "\M-<" 'forms-first-record)
  (define-key map "\M->" 'forms-last-record)
  (define-key map "\C-xk" 'forms-exit)  ;replaces kill buffer
  (define-key map "\C-x\C-s" 'forms-save-buffer)
  (define-key map "\C-x\C-w" 'forms-write-buffer)
)

(if forms-mode-commands-map ;this is the read-only guy
    nil
    (let ((forms-prefix nil))
     (setq forms-mode-commands-map (make-sparse-keymap))
     (forms--mode-commands forms-mode-commands-map)
     (forms--new-command-mappings forms-mode-commands-map))
   ;; now set the C-c characters
   (forms--mode-commands forms-mode-commands-map))

(if forms-mode-hidden-buffer-map  ; this is the guy on the hidden buffer
    nil
    (setq forms-mode-hidden-buffer-map  (make-sparse-keymap))
    (define-key forms-mode-hidden-buffer-map "\C-xk" 'no-op)
)

;; this is the read/write map
(if forms-mode-map
    nil
  (setq forms-mode-map (make-keymap))
  (forms--mode-commands forms-mode-map)
  (forms--new-command-mappings forms-mode-map)
  (let ((i ?\ ))
    (while (< i ?\177)
      (aset forms-mode-map i 'forms-self-insert)
      (setq i (1+ i))))
  (define-key forms-mode-map "\C-b" 'backward-char)
  (define-key forms-mode-map "\C-d" 'forms-clear-column)
  (define-key forms-mode-map "\177" 'forms-backward-clear-column) ;DEL
  (define-key forms-mode-map "\C-j" 'picture-duplicate-line)
  (define-key forms-mode-map "\C-k" 'forms-kill-line)   ;ok-fer
  (define-key forms-mode-map "\C-m" 'forms-newline)     ;ok-fer
  (define-key forms-mode-map "\C-o" 'forms-open-line)   ;ok-fer
  (define-key forms-mode-map "\C-q" 'forms-quoted-insert) ;ok-fer
  (define-key forms-mode-map "\C-w" 'forms-kill-region) ;ok-fer
  (define-key forms-mode-map "\C-y" 'forms-yank)        ;ok-fer
  (define-key forms-mode-map "\ed"  'forms-kill-word)   ;ok-fer
  (define-key forms-mode-map "\t"   'forms-next-field)  ;ok-fer
  (define-key forms-mode-map "\e\t" 'forms-prev-field)  ;ok-fer
  (define-key forms-mode-map "\e\177"  'forms-backward-kill-word)  ;ok-fer
  (define-key forms-mode-map "\ey" 'forms-yank-pop)
  ;; these are probably bad keybindings
  (define-key forms-mode-map "\C-c\C-k" 'picture-clear-rectangle)
  (define-key forms-mode-map "\C-c\C-w" 'picture-clear-rectangle-to-register)
  (define-key forms-mode-map "\C-c\C-y" 'picture-yank-rectangle)
  (define-key forms-mode-map "\C-c\C-x" 'picture-yank-rectangle-from-register)
  (define-key forms-mode-map "\C-c\C-f" 'picture-motion)
  (define-key forms-mode-map "\C-c\C-b" 'picture-motion-reverse)
  ;(define-key forms-mode-map "\C-c<" 'picture-movement-left)
  ;(define-key forms-mode-map "\C-c>" 'picture-movement-right)
  (define-key forms-mode-map "\C-c^" 'picture-movement-up)
  (define-key forms-mode-map "\C-c." 'picture-movement-down)
  (define-key forms-mode-map "\C-c`" 'picture-movement-nw)
  (define-key forms-mode-map "\C-c'" 'picture-movement-ne)
  (define-key forms-mode-map "\C-c/" 'picture-movement-sw)
  (define-key forms-mode-map "\C-c\\" 'picture-movement-se)
)


(let ((old-map (current-local-map)))
  (if forms-quick-help-writable
      nil
    (use-local-map forms-mode-map)
    (setq forms-quick-help-writable
          (substitute-command-keys (concat
            "\\[forms-run-menu]:menu"
            "  \\[describe-mode]:help"
            "  \\[forms-next-record]:next"
            " \\[forms-prev-record]:prev"
            " \\[forms-first-record]:1st"
            " \\[forms-last-record]:last"
            " \\[forms-exit]:exit")))
    (use-local-map forms-mode-commands-map)
    (setq forms-quick-help-read-only
          (substitute-command-keys (concat
            "\\[forms-run-menu]:menu"
            "  \\[describe-mode]:help"
            "  \\[forms-next-record]:next"
            " \\[forms-prev-record]:prev"
            " \\[forms-first-record]:1st"
            " \\[forms-last-record]:last"
            " \\[forms-exit]:exit"))))
   (use-local-map old-map))

;; A bug in the current Emacs release prevents a keymap
;; that is buffer-local from being used by 'describe-mode'.
;; Hence we'll leave it global.
;; -fer I think I see, some forms may want different keybindings.
;; you can set this up on the way in, putting the changes in each file.
;; with static keymaps, you can do this with a hook function


;;;
;;; 	VIII.	Changed movement functions
;;;
;;; Emacs (as of 18.55) lacks the functionality of buffer-local
;;; funtions.  We no longer save the original meaning of some handy
;;; functions, and replace them with a wrapper.
;;; This is best done with a keymap.

(defun forms-backward-kill-word (&optional arg)
  "Kill characters backward until encountering the end of a word.
With argument, do this that ARG times.  Preserves forms column fill when
appropriate."
  (interactive "p")
  (forms-kill-word (- arg)))

(defun forms-kill-word (arg)
  "Kill characters forward until encountering the end of a word.
With argument, tries to do this ARG times.  Preserves forms column fill when
appropriate."
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (forms--find-row-and-column)
  (let* ((arg-sign (if (> arg 0) 1 -1))
         (here (point))
         (there (save-excursion (forward-word arg-sign) (point)))
         (max-there (forms--char-writable-region here there)) )
    (if (numberp max-there)
        (progn (kill-region here max-there)
           (if (and forms--first-advanced-row
                    (< forms--row forms--first-advanced-row))
               (insert-n-times " " (- max-there here))))
        (forms--next-writable arg-sign)
        (if (minusp arg-sign) (forward-char 1))
        (error ""))
    (goto-char there)
    (setq arg (+ arg (- arg-sign)))
    ;; recurse on rest
    (if (not (= arg 0))
        (forms-kill-word arg))  ))

(defun forms-scroll-down (&optional arg) 
  (interactive "P")
  (if (and forms--mode-setup
           forms-forms-scroll)
      (forms-prev-record arg)
   (forms--scroll-down arg)))

(defun forms-scroll-up (&optional arg) 
  (interactive "P")
  (if (and forms--mode-setup
           forms-forms-scroll)
     (forms-next-record arg)
     (forms--scroll-up arg)))

(defun forms-beginning-of-buffer ()
  (interactive)
  (if (and forms--mode-setup
           forms-forms-jump)
      (forms-first-record)
      (forms--beginning-of-buffer)))

(defun forms-end-of-buffer ()
  (interactive)
  (if (and forms--mode-setup
           forms-forms-jump)
      (forms-last-record)
      (forms--end-of-buffer)))

(defun forms-next-record (arg)
  "Advance to the ARGth following record."
  (interactive "P")
  (forms--find-current-record-number)
  (if (not (forms-jump-record (+ forms--current-record
                                 (prefix-numeric-value arg)) t t))
      (if (or forms-read-only
              (not (y-or-n-p "Insert a new record at end? ")))
          (error "At end of records.")
        (forms-insert-record t))))

(defun forms-prev-record (arg)
  "Advance to the ARGth previous record."
  (interactive "P")
  (forms--find-current-record-number)
  (if (not (forms-jump-record (- forms--current-record
                                 (prefix-numeric-value arg)) t t))
      (if (or forms-read-only
              (not (y-or-n-p "Insert a new record at beginning? ")))
          (error "At beginning of records.")
        (forms-insert-record nil))))


(defun forms-jump-record (arg &optional relative non-interactive)
  "Jump to a given record."
  (interactive "NRecord number: ")
  ;; verify that the record number is within range
  ;; for old-version, see forms.02.el
  (if (or (> arg forms--total-records) (<= arg 0))
      (if non-interactive
          nil ;return nil
         (progn (beep)
             (message "Record number %d out of range 1..%d"
                      arg forms--total-records)))
    ;; this could be made smarter and faster...
    (goto-char (point-min))
    (if (search-forward forms-record-separator nil nil arg)
        (progn (recenter 1)
               (forms--set-mode-line)
               t))))

(defun forms-beginning-of-form (arg)
  "Moves point to beginning of current form."
  (interactive "p")
 (let ((there (point)))
  (forward-line 1)
  (search-backward forms-record-separator nil nil arg)
  (beginning-of-line)
  (if (and (= there (point))
           (not (bobp)))
     (progn (forward-line -1) (forms-beginning-of-form 1)))
 ))

(defun forms-end-of-form (arg)
  "Moves point to end of current form.  "
  (interactive "p")
 (let ((there (point)))
  (end-of-line)
  (if (search-forward forms-record-separator nil t arg)
      (progn (forward-line -2)
         (end-of-line)
         (if (and (= there (point)) (not (eobp)))
             (progn (forward-line 1) (forms-end-of-form 1))))
      (end-of-buffer)
      ;; records always have a trailing CR
      (forward-char -1))  ))


(defun forms-first-record ()
  "Jump to first record."
  (interactive)
  (forms-jump-record 1))

(defun forms-last-record ()
  "Jump to last record. As a side effect: re-calculates the number
 of records in the data file."
  (interactive)
  (let ((numrec (save-excursion
                  (set-buffer forms--file-buffer)
                  (count-lines (point-min) (point-max)))))
    (if (= numrec forms--total-records)
	nil
      (beep)
      (setq forms--total-records numrec)
      (message "Number of records reset to %d." forms--total-records)))
  (forms-jump-record forms--total-records))


(defun forms-prev-field (arg)
  "Jump to ARG-th previous field."
  (interactive "p")
  (forms-next-field (- arg)))

;(defun forms-next-field (arg)
;  "Jump to ARG-th next field."
;  (interactive "p")
;  (let ((i 0)   (cnt 0)
;        (here   (point))
;        there)
;    (if (zerop arg)
;        (setq cnt 1)
;      (setq cnt (+ cnt arg)))
;    (if (catch 'done
;          (while (< i forms--number-of-markers)
;            (if (or (null (setq there (aref forms--markers i)))
;                    (<= there here))
;                nil
;              (if (<= (setq cnt (1- cnt)) 0)
;                  (progn
;                    (goto-char there)
;                    (throw 'done t))))
;            (setq i (1+ i))))
;        nil
;      (goto-char (aref forms--markers 0)))))

(defun forms-next-field (arg)
  "Jump to ARG-th next field."
  (interactive "p")
  (forms--find-row-and-column)
  (let ((signp (if (< 0 arg) 1 -1))
        (bwritable (if (bobp) nil
                       (save-excursion (forward-char -1)
                                   (funcall forms--char-writable-function)))))
    ;(message "going %s with sign %s bw %s" arg signp bwritable)
    (cond ((plusp signp)
           (forms--next-unwritable signp)
           (forms--next-writable signp))
          ((and bwritable (minusp signp))
           (forward-char -1)
           (forms--next-unwritable signp)
           (forward-char 1))
          ((minusp signp)
           (forms--next-unwritable signp)
           (forms--next-writable signp)
           (forms--next-unwritable signp)
           (forward-char 1)))
    (cond ((or (= arg 1) (= arg -1)))
          (t (forms-next-field (- arg signp))))    ))

(defun forms--next-unwritable (arg)
  (while (funcall forms--char-writable-function)
    (forward-char arg)))

(defun forms--next-writable (arg)
  (while (not (funcall forms--char-writable-function))
    (forward-char arg)))

(defun forms-open-line (arg)
 "Insert an empty line after the point if you are in the variable
length field region of a record."
 (interactive "p")
 (forms--find-row-and-column)
 (if (or (and (funcall forms--char-writable-function)     ;standard test
              forms--first-advanced-row
              (>= forms--row forms--first-advanced-row))
         (and (= 0 (current-column))                      ;bolp in safe region
              forms--first-advanced-row
              (> forms--row forms--first-advanced-row)))
     (open-line arg)
   (error "Can't insert a newline in the middle of labels.")))

(defun forms-quoted-insert ()
  "Insert a quoted char only after querying the user, insertion may 
mess up the display so that it is unparsable."
  (interactive)
  (if (y-or-n-p "Are you sure you want to insert a raw character? ")
      (progn (message "Character to insert: ")
      (call-interactively 'quoted-insert))))


;;;
;;;	IX.	Changed saving functions
;;;

(defun forms-revert-buffer (&optional ask-auto-save ask-confirm)
  (interactive "p")
  (let ((revert-buffer-function nil)
        (old-current-record forms--current-record)
        (buffer-file-name forms-file))
    (revert-buffer ask-auto-save ask-confirm)
    (set-buffer-modified-p nil)
    (forms-jump-record old-current-record))
  (forms--set-mode-line))

(defun forms-save-buffer (&optional arg)
  (interactive "p")
  (forms--update-all)
  (save-excursion
    (if forms--all-in-one
        (progn 
        (let ((file-name forms-file) )
          (set-buffer forms--file-buffer)
          (write-file file-name)
          (setq buffer-file-name nil)
          (setq buffer-auto-save-file-name
                (concat file-name "~"))  ))
      (progn
          (set-buffer forms--file-buffer)
          (save-buffer arg)))  )
  (set-buffer-modified-p nil)  
  (forms--set-mode-line))

(defun forms-write-buffer (&optional arg)
 "Write out a forms buffer, prompting for FILENAME."
  (interactive "F")
  (save-excursion
    (if forms--all-in-one
        (progn
          (set-buffer forms--file-buffer)
          (let ((old-name (buffer-name)))
           ;; this has to be a write to a file
           (write-file arg)
           (rename-buffer old-name)
           (setq buffer-auto-save-file-name
                 (concat forms-file "~"))
           (setq buffer-file-name nil) ))
      (progn (set-buffer forms--file-buffer)
             (write-file arg))  ))
  (setq forms-file arg)
  (rename-buffer (file-name-nondirectory forms-file))
  (forms--set-mode-line)    )

(defun forms--exit (query &optional save)
 "Exit forms-mode and delete a buffer.  If QUERY is t,
ask user to save iff buffer is modified.  Iff save is t, also ask."
 (catch 'forms--exit
 (let ((backup-buf (buffer-name forms--file-buffer))
       (b-name (buffer-name (current-buffer))) )
   (if (and query  (not (buffer-modified-p forms--file-buffer))
            (not (y-or-n-p
                    (format "Do you really want to kill forms-mode file %s? "
                            b-name))))
       (error "Ok, going back to forms-mode."))
   (if (and query (or (buffer-modified-p forms--file-buffer)
                      (buffer-modified-p nil)))
       (if (not (yes-or-no-p
                 (format "Forms-mode buffer %s modified; kill anyway? "                          b-name)))
           (throw 'forms--exit nil)))
   (forms--kill-buffer forms--file-buffer)
   (forms--kill-buffer forms--commands-buffer)
   (forms--kill-buffer forms--comments-buffer)
   (forms--kill-buffer (current-buffer))
   (message " "))))

(defun forms--kill-buffer (abuffer)
  (if (bufferp abuffer)
      (save-excursion (set-buffer abuffer)
                      (delete-auto-save-file-if-necessary)
                      (kill-buffer abuffer))))

(defun forms-find-file (fn)
  "Visit file FN in forms mode."
  (interactive "fForms file: ")
  (find-file-read-only fn)
  (or forms--mode-setup (forms-mode t t)))

(defun forms-find-file-other-window (fn)
  "Visit file FN in form mode in other window"
  (interactive "fFbrowse file in other window: ")
  (find-file-other-window fn)
  (eval-current-buffer)
  (or forms--mode-setup (forms-mode t t)))

(defun forms-exit (no-query)
  "Normal exit. Modified buffers are automatically saved if NO-QUERY is t.
Otherwise user is asked."
  (interactive "P")
  (forms--exit (not no-query) t))

(defun forms-exit-no-save (no-query)
  "Exit without saving buffers.  Modified buffers are not saved if NO-QUERY
is t. Otherwise user is asked."
  (interactive "P")
  (forms--exit (not no-query) nil))


;;;
;;;	X.	Translation functions between Excel and Forms
;;;

(defun forms-excel-to-forms-format (old-char new-char)
 "Changes the current buffer from an Excel file saved as text only,
to a file ready to be read into forms-mode."
 (interactive "cOLD forms-field-sep character (taken raw): 
cNEW forms-field-sep character (taken raw): ")
 (let ((old-mode-line-format mode-line-format)  comment-end  comment-start)
 (unwind-protect
  (progn (goto-char (point-min))
    (insert ";; -*- mode: forms -*-\n\n")
    (setq forms-field-sep (char-to-string new-char))
    (insert (format "(setq forms-field-sep \"%s\")\n\n"
                    (char-to-string new-char)))
    (insert-file (concat forms-load-path "/forms--example-header"))
    (exchange-point-and-mark)
    (insert forms-header-stop)
    (setq comment-start (point))
    (message (setq mode-line-format (substitute-command-keys
"Move point to very first char after comments, then type \\[exit-recursive-edit]")))
    (recursive-edit)      
    (setq comment-end (point))
    (subst-char-in-region comment-start comment-end old-char ?  t)
    (insert forms-comment-header-stop)
    (insert "\n")
    (message "Replacing tabs with %s" new-char)
    (subst-char-in-region comment-end (point-max) old-char new-char t)
    (message "Finished with translation to Soar/PA mode format."))
  (setq mode-line-format old-mode-line-format))  ))

(defun forms-record-lengths ()
  "Count the record length of the currently exposed region. (It assumes 
you have narrowed to only the record region, or are working with a raw file."
  (interactive)
  (setq forms-frl-results nil)
  (forms-do-to-all-raw-records "Counting fields"
      (push (forms-count-fields) forms-frl-results))
  (setq forms-frl-results (nreverse forms-frl-results))
  (if (= (apply 'min forms-frl-results)
         (apply 'max forms-frl-results))
      (message "All records %s long." (first forms-frl-results))
    (message 
       "Not all records %s long.  From %s to %s.  Check forms-frl-results."
        (first forms-frl-results) (apply 'min forms-frl-results) 
        (apply 'max forms-frl-results))))

(defun forms-trim-records (n)
  "Trim a line in forms back buffer format to be N fields long."
  (interactive "P")
  (if (not n)
      (setq n
            (read-from-minibuffer "Trim to field number: "
                (format "%s" forms-number-of-fields) nil t)))
  (setq forms-number-of-fields n)
  (save-excursion
  (let ((eol (save-excursion (end-of-line) (point))) )
    (beginning-of-line)
    (if (not (search-forward forms-field-sep eol t n))
        (progn (forward-line)
               (message "No trimming happened on this well shaped line."))
    (forward-char (- (length forms-field-sep)))
    (delete-region (point) eol))))
  (forward-line))



;;;
;;;	XI.	forms-self-insert & character insertion setup functions
;;;

(defun forms-newline (arg)
  "Insert ARG newlines if you are in the variable length records and
point is not over a label, else move down a line."
  (interactive "p")
  (forms--find-row-and-column)
  (if (and forms--first-advanced-row
           (< forms--row forms--first-advanced-row))
      (forward-line arg)
    (if (funcall forms--char-writable-function)
        (newline arg)
      (forward-line arg))  ))

(defun forms-self-insert (arg)
  "Insert this character in place of character previously at the cursor, if
it's ok.  The cursor then moves in the direction you previously specified
with the commands picture-movement-right, picture-movement-up, etc.
Do \\[command-apropos]  picture-movement  to see those commands."
  (interactive "p")
  (if (funcall forms--char-writable-function)
      (while (> arg 0)
        (forms--char-insert last-input-char)
        (setq arg (- arg 1)) )
    (progn (forward-char) (beep))
           ;(error "In a label or protected field.")
  ))

(defun forms--char-insert (input-char)
  (if (char-equal last-input-char forms--field-sep-char)
      (error "You should not insert the forms-field-sep character: %s."
             forms-field-sep))
  (if (or (and forms--first-advanced-row 
               (> forms--row forms--first-advanced-row))
          (and forms--first-advanced-row
               (= forms--row forms--first-advanced-row)
               (>= forms--column forms--first-advanced-column)))
      (insert input-char)
    (move-to-column-force (1+ (current-column)))
    (delete-char -1)
    (insert input-char)
    (forward-char -1)
    (picture-move)))

(defun forms-kill-line (arg)
  "Clear out rest of line if characters are writable;
if not ok, take what you can get.  If at end of line, remove the newline.
Cleared-out text goes into the kill ring, as do newlines that are advanced 
over.  With argument, clear out (and save in kill ring) that many lines."
  (interactive "P")
  (let (max-kill)
  (save-excursion
  (if arg
      (progn (setq arg (prefix-numeric-value arg))
             (if (setq max-kill
                       (forms--char-writable-region (point)
                           (save-excursion (forward-line arg) (point))))
                 (kill-region (point) max-kill)))
    ;; else no arg
    (if (looking-at "[ \t]*$")  ;blank line
        (if (setq max-kill
                  (forms--char-writable-region (point)
                      (save-excursion (forward-line 1) (point))))
            (progn ; check for label on following line
              (if (not (forms--char-writable-region max-kill (1+ max-kill)))
                  (setq max-kill (- max-kill 1)))
              (kill-region (point) max-kill))
          (error "Can't delete labels."))
      ;; else not at end of line
      (if (setq max-kill (forms--char-writable-region (point)
                              (save-excursion (end-of-line) (point))))
          (progn (kill-region (point) max-kill)
              (if (not (= max-kill (- (point)
                                      (save-excursion (end-of-line) (point)))))
                 (insert-n-times " " (- max-kill (point)))))
        (error "Can't delete labels.")))
    t))))

(defun forms--char-writable-region (start end)
  ;; return max point, of chars that are writable
  ;; this is directed unlike most region commands
  (save-excursion
  (let ((increment (if (> start end) -1 1)))
    (if (> start end) (setq start (- start 1)))
    (goto-char start)
    (while (and (not (= (point) end))
                (funcall forms--char-writable-function))
      (forward-char increment))
    (if (= (point) start)
        nil
      (if (not (funcall forms--char-writable-function))
          (if (looking-at "^")  ;new lines are verboten if not writable
              (- (point) increment)
            (point))
        (point))      ))))

(defun forms-backward-clear-column (arg)
 "Clear out ARG columns before point, moving back over them."
 ;; evolved from clear-column
 (interactive "p")
 (let* ((increment (if (> arg 0) -1 1))
        (cc (current-column))
        (target-col (+ cc (- arg))))
 (cond ((and (= cc 0) (not (funcall forms--char-writable-function)))
        (if (save-excursion (forward-char -1)
               (and (= 0 (current-column))
                    (funcall forms--char-writable-function)))
            (delete-char -1)
          (forward-char -1)
          (error "Can't delete a leading label.")) )
        ((and (= cc 0) (funcall forms--char-writable-function))
         (delete-char -1))
        (t ;(message "increment %d" increment) (sit-for 2)
           (while (if (> 0 increment)
                      (not (<= (current-column) target-col))
                      (not (>= (current-column) target-col)))
           (if (> 0 increment) (forward-char -1))
           (if (not (funcall forms--char-writable-function))
               (error "Deleting not allowed in a label, column %d." 
                      (current-column))
            (delete-char 1)
            (forms--find-row-and-column)
            (if (and forms--first-advanced-row
                     (>= forms--row forms--first-advanced-row)
                     (> forms--column forms--first-advanced-column))
                (if (not (eobp)) (forward-char 1))
                (insert " "))
            (if (and (not (eobp)) (> 0 increment)) (forward-char -1)) )) ))))


(defun forms-clear-column (arg)
  "Clear out ARG columns after point without moving."
  (interactive "p")
  (save-excursion
   (let* ((increment (if (> arg 0) 1 -1))
          (cc (current-column))
          (target-col (+ cc arg)))
    (cond ( (and (= cc 0) (not (funcall forms--char-writable-function)))
            (if (save-excursion (forward-char -1) 
                   (and (= 0 (current-column))
                        (funcall forms--char-writable-function)))
                (delete-char -1)
              (progn (forward-char -1)
                     (error "Can't delete a label.")) ))
          ( (and (= cc 0) (funcall forms--char-writable-function))
            (delete-char -1))
        (t (setq cc (current-column))
     (while (not (= cc target-col))
        (if (> 0 increment)
            (forward-char -1))
        (if (not (funcall forms--char-writable-function))
            (error "Deleting not allowed in a label, column %d."
                   (current-column))
         (delete-char 1)
         (if (and forms--first-advanced-row
                  (< forms--row forms--first-advanced-row))
             (insert " "))
        (if (> 0 increment)
            (forward-char -1)) )
        (setq cc (+ cc increment)) )        )))))

      ;; count-lines seem wrong here, often returning 1 when on the
      ;; same line
      ;(message "got %s for %d" forms--row-and-column here)
(defun forms--find-row-and-column ()
  (save-excursion
    (setq forms--column (current-column))
    (let* ((here (if (eobp)
                     (point)
                     (+ 1 (point))))
           (start (progn (forward-line) ;in case you're on it
                         (search-backward forms-record-separator nil t)
                         (beginning-of-line)
                         (point))))
      (setq forms--row (if (= 2 here)
                           1
                         (count-lines start here)))
  )))

;(forms--find-current-record-number)
(defun forms--find-current-record-number ()
 (save-excursion
   (let* ( (start (progn (forward-line 1)
                         (search-backward forms-record-separator nil t)
                         (beginning-of-line)
                         (point)))
           (end (progn (search-forward " " nil t) (point))) )
    (setq forms--current-record
          (car (read-from-string (buffer-substring start end)))))))

(defun forms-yank (arg)
 "Doesn't use ARG, as with real yank, yet anyhow....should be:
With just c-u as argument, same but put point in front (and mark at end).
With argument n, reinsert the nth most recently killed stretch of killed text."
  (interactive "p")
  (if (not (funcall forms--char-writable-function))
      (error "Can't yank here."))
  (push-mark (point))
  (setq kill-ring-yank-pointer kill-ring)
  (forms--yank-inserter (car kill-ring-yank-pointer)))

(defun forms--yank-inserter (astring)
  (let  ((i 0)
         (string-size (length astring))  )
    (while (and (< i string-size)
                (funcall forms--char-writable-function))
      (forms--char-insert (substring astring i (+ 1 i)))
      (setq i (1+ i))      )))

(defun forms-yank-pop ()
"Replace just-yanked stretch of killed-text with a different stretch.
This command is allowed only immediately after a  forms-yank or a forms-yank-pop.
At such a time, the region contains a stretch of reinserted
previously-killed text.  yank-pop  deletes that text and inserts in its
place a different stretch of killed text.

With no argument, the previous kill is inserted.
With argument n, the n'th previous kill is inserted.
If n is negative, this is a more recent kill.

The sequence of kills wraps around, so that after the oldest one
comes the newest one."
  (interactive)
  (if (not (or (eq last-command 'forms-yank-pop)
               (eq last-command 'forms-yank)))
      (error "Previous command was not a forms-yank")
    (delete-region (point) (mark))
    (setq kill-ring-yank-pointer (cdr kill-ring-yank-pointer))
    (forms--yank-inserter (car kill-ring-yank-pointer))) 
)

(defun forms-kill-region (start end)
  "Kill between point and mark preserving labels.
This is the primitive for forms programs to kill text (as opposed to deleting
it).  See kill-region (which this is modeled on) for more information."
  (interactive "r")
  (let (max-kill)
  (save-excursion
     (forms--find-row-and-column)
     (setq max-kill (forms--char-writable-region start end))
     (if max-kill
         (kill-region start max-kill))
     (if (and forms--first-advanced-row
              (< forms--row forms--first-advanced-row))
         (progn (goto-char start)
                (insert-n-times " " (- max-kill start)))))))


        ;(forms--make-char-insertable-function)
(defun forms--make-char-insertable-function ()
  "Generate predicate for character insertion."
  (setq forms--advanced-column-keywords nil)
  (setq forms--first-advanced-row nil)
  (setq forms--first-advanced-column nil)
  (setq forms--char-writable-function 
        (forms--char-writable-maker forms-format-list))
  (setq forms--maxlength-advanced-column-keywords
      (apply 'forms--max (mapcar 'length forms--advanced-column-keywords))) )

(defun forms--char-writable-maker (the--format-list)
 "Returns the parser function for forms."
 (let ((the-dynamic-text forms--dynamic-text)
       char-insert-and-list
       (row 2) (column 0) )
   (` (lambda nil
        (let (here)
         (forms--find-row-and-column)
         (or (,@ (apply 'append
                        (mapcar 'forms--make-char-insertable-elt
                                the--format-list)))
             (, (if char-insert-and-list
                    (` (and (,@ char-insert-and-list)))
                    nil))
;             (and (, (if char-insert-and-list t nil))
;                  (,@ char-insert-and-list))
          )) ))))
           
;; will have to be called after a field has changed visibility
(defun forms--make-char-insertable-elt (item-list)
  ;; we also use the dynamic bindings of: the-dynamic-text
  ;; and char-insert-and-list, row, column
 (let ((el (format-item-field item-list))
       (name (format-item-name item-list))       
       (label (format-item-label item-list))
       (field-size (format-item-field-size item-list))
       (newline-p (format-item-newline-p item-list))
       (line-size (format-item-line-size item-list))
       old-column)
   ;(message "in make-elt with %s and el %s" name el) (sit-for 1)
  (if (and forms--first-advanced-row (string= "" label))
      (error "Can't have a null label in advanced rows, name is %s" name))
  (if (forms--item-list-visible-p name)
  (cond
   ((or (stringp el) (null el))
    (if forms--first-advanced-row
        (error "Error in field %s. No fixed fields after line-size=t" name))
    (if newline-p
        (progn (setq column 0)
               (setq row (+ row (forms--t-or-number-to-number newline-p)))))
    (if label (setq column (+ column (length label))))
    (cond ((null el))
          ((stringp el)
           (if (null field-size)
               (setq column (+ column (length el)))
             (setq column (+ column field-size))))
          (t (setq column (+ column field-size))) )
    nil)
   ((numberp el)
    (if newline-p (progn (setq column 0)
                 (setq row (+ row (forms--t-or-number-to-number newline-p)))))
    (if label (setq column (+ column (length label))))
    (setq old-column column)
    (if (and (eq t line-size) (not forms--first-advanced-row))
        (progn (setq forms--first-advanced-row row)
               (setq forms--first-advanced-column column)))
        ;; if you are the 1st advanced row or if not in advanced rows yet
    (if (numberp field-size) (setq column (+ column field-size)))
    (cond ( (or (eq forms--first-advanced-row row)
                (not forms--first-advanced-row))  ;spot21
            (cond ((or (not line-size)
                       (and (numberp line-size) (= 1 line-size)))
                   (if (eq field-size t)
                       (` ((and (= forms--row (, row))
                                (>= forms--column (, old-column)))))
                     (` ((and (= forms--row (, row))
                              (>= forms--column (, old-column))
                              (< forms--column (, column)))))))
                  ((eq line-size t)
                   (push label forms--advanced-column-keywords)
                   (push (` (forms--char-insert-not-in-label (, label) 
                                                             (, row)))
                         char-insert-and-list)
                   (` ((and (= forms--row (, row))
                            (>= forms--column (, old-column))))))
                  ((and (numberp line-size) (> line-size 1))
                   (` ((or (and (= forms--row (, row))
                                (>= forms--column (, old-column)))
                           (,@ (forms--make-ok-rows row (- line-size 1))))))
                   )))
          ( forms--first-advanced-row
            (push label forms--advanced-column-keywords)
            (push (` (forms--char-insert-not-in-label (, label) (, row)))
                  char-insert-and-list)
            nil)    ))
   ((listp el)
    (if forms--first-advanced-row
        (error "Error in field %s. No fixed fields after line-size=t" name))
    (if newline-p
        (progn (setq column 0)
           (setq row (+ row (forms--t-or-number-to-number newline-p)))))
    (if label (setq column (+ column (length label))))
    (setq column (+ column (length (pop the-dynamic-text))))
    nil)   ))))

    ;(push (list 'first-row forms--first-advanced-row
     ;           'row row 'line-size line-size 'forms--row forms--row
      ;          'forms--col forms--column 'old-col old-column 
       ;         'field-size field-size)
        ;  aa)

;; I believe this is dead, wrong code from spot21 above
;(and (not forms--first-advanced-row)
;                     (or (not line-size)
;                         (and (numberp line-size)
;                              (= line-size 1) )))

(defun forms--char-insert-not-in-label (the-string first-row)
  ;; has two fast exits, then real test
  (and (and forms--first-advanced-row 
            (> forms--row forms--first-advanced-row))
       (if (>= forms--row first-row)
           (if (> forms--column forms--maxlength-advanced-column-keywords)
               t
             (save-excursion
               (beginning-of-line)
               (if (and (looking-at the-string)
                        (< forms--column (length the-string)))
                   nil
                 t)))
         t)))

(defun forms--t-or-number-to-number (item)
  (if (numberp item)  item  1))

(defun forms--make-ok-rows (row counter)
  (let ((aresults nil))
    (while (> counter 0)
      (push (list '= 'forms--row (+ row counter)) aresults)
      (setq counter (- counter 1)))
    aresults))



;;;
;;;	XII.	Report functions
;;;

(defun forms-make-report (&optional rbuffer)
 "Print to RBUFFER a plain file all the visible fields of all the visible 
records.  Must be called from a forms-file-buffer."
  (interactive)
  ;; set variables you need to use while in other buffer
  (let ((current-buffer (current-buffer))
        (current-buffer-file forms-file)
        (report-buffer (or rbuffer   
                           (if (and (boundp 'report-display-buffer)
                                    report-display-buffer)
                               report-display-buffer)
                           (get-buffer-create "*Forms-Report*"))) )
    (pop-to-buffer report-buffer)
    (erase-buffer)
    (forms--report-header current-buffer-file)
    (insert-buffer current-buffer)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (message "Type C-x C-w to save this file.")))

(defun forms--draw-all-records (draw-buffer)
  ;; assumes it starts in a forms-mode buffer
  (let ( (i 1)
         (file-buffer forms--file-buffer)
         (f-format forms--format)
         (seperator forms-field-sep)  
         (f-file forms-file)
         (total forms--total-records)
         (nof forms-number-of-fields) )
    (set-buffer draw-buffer)
    (setq forms--format f-format)
    (setq forms-field-sep seperator)
    (setq forms-number-of-fields nof)
    (setq forms-file f-file)
    (while (<= i total)
      (message "Drawing %s of %s" i total)
      (setq forms--current-record i) ;; used by insert-reco
      (forms--insert-record
       (save-excursion
          (set-buffer file-buffer)
          (forms--goto-line i)
          (forms--get-record)))
      (setq i (1+ i)) )))

(defun forms--report-header (forms-file)
  (insert-current-time-string)
  (insert (format " - Forms (%s) report for user " forms-version))
  (insert (getenv "USER") "\n")
  (insert "For file " forms-file "\n\n")
  (insert "-------------------------------------------------------------\n"))


;; make these be local later
(defvar forms--tally-fields-results nil
  "Where the forms--tally-fields-results are stored.")
(defvar  forms-report-buffer-name "*Forms-Report*"
  "Name of buffer where reports go.")


(defun forms-tally-fields (&optional field-number-or-name rbuffer)
  "Count the types in FIELD-NUMBER-OR-NAME (removing leading and trailing
spaces first), and print them and their counts out in RBUFFER."
  (interactive "P")
  (if (and (interactive-p)
           (buffer-modified-p)
           (y-or-n-p "Would you like to checkpoint first?"))
      (forms--update-all))
  (forms--set-mode-line)
  ;; rbuffer gets set to passed in arg, report-display-buffer if in a report
  ;; calling command, or makes its own
  (let ((report-buffer (or rbuffer
                           (if (and (boundp 'report-display-buffer)
                                    report-display-buffer)
                               report-display-buffer)
                           (get-buffer-create forms-report-buffer-name)))
        label counter (f-f forms-file)
        max-count-column max-field-column  (i 1)
        field-name)
    ;; set up the field-name and field-number, either passed a
    ;; numbered field, a name (so get the number), or have to query user
    (cond ((numberp field-number-or-name)
           (setq field-number field-number-or-name)
           (setq field-name (format " Number %d" field-number)))
          ((stringp field-number-or-name)
           (setq field-name field-number-or-name)
           (setq field-number (cdr (assoc forms--format-query-list)))
           (if (not field-number)
               (error "In forms-tally-fields with %s and %s"
                             field-number-or-name rbuffer)))
          ((not  field-number-or-name)
           (let ((pair (assoc (completing-read
                                "Field to summarize (name with autocomplete): "
                                               forms--format-query-list nil t)
                              forms--format-query-list)))
             (setq field-number (cdr pair))
             (setq field-name (car pair))))
          (t (error "In forms-tally-fields with %s and %s"
                    field-number-or-name rbuffer)))
  (setq forms--tally-fields-results nil)
  ;; compute field
  (while (<= i forms--total-records)
    (setq label (forms--field i field-number))
    (message "Tallying %s of %s" i forms--total-records)
    (setq label (string-trim forms--blank-bag label))
    (setq counter (assoc label forms--tally-fields-results))
    (if counter
        nil
      (push (cons label 0) forms--tally-fields-results)
      (setq counter (first forms--tally-fields-results)))
    (rplacd counter (+ 1 (cdr counter)))
    (setq i (1+ i)))
  ;; sort the list to print it, high to low
  (setq forms--tally-fields-results
        (sort forms--tally-fields-results
              '(lambda (x y) (> (cdr x) (cdr y)))))
  (save-excursion
    (set-buffer report-buffer)
    (goto-char (point-max))
    (forms--report-header f-f)
    (setq max-field-column
          (apply 'forms--max (mapcar '(lambda (x) (length (car x)))
                                     forms--tally-fields-results)))
    (setq max-count-column
          (apply 'forms--max (mapcar '(lambda (x) (cdr x))
                                     forms--tally-fields-results)))
    (setq max-field-column (+ 3 max-field-column))
    (insert (format "Tallying field #%s (%s)\n\n" field-number field-name))
    (insert "VALUE")
    (setq max-field-column (max max-field-column 7)) ; room for value
    (move-to-column-force max-field-column)
    (insert "FREQUENCY\n")
   (mapcar '(lambda (x)
              (if (string= "" (car x))
                 (insert (format "\"\""))
                (insert (format "%s   " (car x) )))
              (move-to-column-force max-field-column)
              (insert (format (format "%%%ss\n" (+ 1 (log10 max-count-column)))
                              (cdr x))))
            forms--tally-fields-results))
    (pop-to-buffer report-buffer)  ))

;(popper-wrap forms-tally-fields forms-report-buffer-name)


;;;
;;;	XIII.	Changes to "normal" commands
;;;

(defun forms-undo (amount)
  (interactive "p")
  (undo amount)
  (message "Undo %s!  But back buffer may have irreversable changes." amount))


;;;
;;;	XIV.	Changes to picture-mode commands
;;;



;;;
;;;	XV.	Utility functions
;;;

;; this looks like a real gnu bug
(defun forms--re-search-forward (regexp bound on-fail repeat)
  (if (not (= 0 repeat))
     (re-search-forward regexp bound on-fail repeat)))

(defun forms-quick-help ()
  "Initial help."
  (interactive)
  ;; this sets up the values of forms-quick-help-* only after the keymaps are set up
  (if forms-read-only
      (message forms-quick-help-read-only)
         ;"SPC:next   DEL:prev   <:first   >:last   ?:help   q:exit"
    (message forms-quick-help-writable
     ;"C-c n:next  C-c p:prev  C-c <:first  C-c >:last  C-c ?:help  C-c q:exit"
         )
    (sit-for 1)))

(defun forms--max (&rest numbers)
  (if (and numbers (not (equal numbers '(nil))))
      (apply 'max numbers)
      0))

(defmacro forms-do-to-all-raw-records (label &rest body)
  ;; go to the start of the records, for either real files, or about to be ones
  (` 
   (let ( (i 1) 
          (remember-old-point (point)))
    (goto-char (point-min))
    (or (if (search-forward forms-comment-header-stop nil t)
            (progn (forward-char 1) t))
        (goto-char (point-min)))
    (while (not (eobp))
      (,@ body)
      (message "%s %s" (, label) i)
      (setq i (+ 1 i))
      (forward-line 1))
    (goto-char remember-old-point)      )))

;(interactive "NField number to add: ")
(defun forms-add-field (field-num)
  (interactive 
  (let ((default (or forms-number-of-fields
                     (if forms--file-buffer
                         (save-excursion (set-buffer forms--file-buffer)
                            (forms--set-number-of-fields))
                         (save-excursion (forms--set-number-of-fields))))))
    (list (car (read-from-string (read-input
          (format "Add before field number [1 to %s(%s to add to end)]: "
                 default (+ 1 default)) ))))))
 "Add a field from all records in the current forms-mode buffer before 
current FIELD-NUM objects.  To insert a new first field, use 1; to 
append put one before 1 + forms-number-of-fields."
  (let (number-of-fields
        (switcharoo (or (eq major-mode 'forms-mode)
                        (eq major-mode 'spa-mode))) )
  (save-excursion
    (forms--do-field-mod-tests field-num 1 (+ 1 forms-number-of-fields))
    (setq number-of-fields  forms-number-of-fields)
    (if switcharoo (set-buffer forms--file-buffer))
  (forms-do-to-all-raw-records "Adding to"
    (cond ((= field-num 1)
           (beginning-of-line)
           (insert forms-field-sep)  )
          ((= field-num (+ 1 number-of-fields))
           (end-of-line)
           (insert forms-field-sep) )
          (t (forms--re-search-forward forms-field-sep nil nil (- field-num 1))
            (insert forms-field-sep) ))))
  (setq forms-number-of-fields (+ 1 forms-number-of-fields))
  (if switcharoo  (forms--set-mode-line))
  (message "Don't forget to update the format-list.")  ))

(defun forms-remove-field (field-num)
  "Remove a field from all records in the current forms-mode buffer."
  (interactive 
   (let ((default (or forms-number-of-fields
                      (if forms--file-buffer
                          (save-excursion (set-buffer forms--file-buffer)
                             (forms--set-number-of-fields))
                        (forms--set-number-of-fields)))))
     (list (car (read-from-string (read-input
	 (format "Field number to remove [1 to %s]: " default) ))))))
  (let (number-of-fields 
        (switcharoo (or (eq major-mode 'forms-mode)
                        (eq major-mode 'spa-mode))))
  (save-excursion 
    (forms--do-field-mod-tests field-num 1 forms-number-of-fields)
    (setq number-of-fields forms-number-of-fields)
    (if switcharoo (set-buffer forms--file-buffer))
  (forms-do-to-all-raw-records "Removing from"
    (cond ((= field-num 1)
           (beginning-of-line)
           (delete-region (point)
              (save-excursion (re-search-forward forms-field-sep nil nil 1)
                (point)))  )
          ((= field-num number-of-fields)
           (end-of-line)
           (delete-region (point)
              (save-excursion (re-search-backward forms-field-sep nil nil 1)
                (point)))    )
          (t (forms--re-search-forward forms-field-sep nil nil (- field-num 1))
             (delete-region (point)
                (save-excursion (re-search-forward forms-field-sep nil nil 1)
                  (point))) ))  ))
  (if switcharoo (forms--set-mode-line))
  ;; probably should reset format and parser here
  (setq forms-number-of-fields (- forms-number-of-fields 1))))
          
(defmacro forms--do-field-mod-tests (arg min max)
 (` 
  (progn
  (if (not forms-field-sep)
      (error "Need to set forms-field-sep variable for this buffer."))
  (if (not forms-number-of-fields) (forms--set-number-of-fields))
  (if (or (< (, arg) (, min))
          (if (, max)
              (> (, arg) (, max))))
      (if (not (y-or-n-p (format 
                           "Field-num %s out of range %s...%s, Do anyway?" 
                           (, arg) (, min) (, max))))
          (error "Field-num %s out of range %s...%s" (, arg) (, min) (, max)))
))))

(defun forms--set-number-of-fields ()
  (save-excursion (goto-char (point-max))
     (forward-line -2)
     (setq forms-number-of-fields (forms-count-fields))))

(defun forms-count-fields ()
  (interactive)
  (save-excursion
     (beginning-of-line)
     (let ((fcf-result (+ 1 (count-regexp (or forms-field-sep "#")
                                (save-excursion (end-of-line) (point))))))
       (if (interactive-p)
          (progn (message "%s fields found with %s seperator." fcf-result
                          (or forms-field-sep "#"))
                 fcf-result)
          fcf-result))))

(defun forms--renumber-buffer ()
  "Renumber each segment, from 1 to forms--total-records."
  (let ( (i 1) (renumber-old-point (point)))
    (goto-char (point-min))
    (while (<= i forms--total-records)
      (message "Renumbering %s of %d" i forms--total-records)
      (delete-region (point) (save-excursion (end-of-line) (+ 1 (point))))
      (forms--insert-line-number i)
      (setq i (+ 1 i))
      (if (<= i forms--total-records)
         (progn (search-forward forms-record-separator nil nil 1)
                (forward-line -1))))
    (goto-char renumber-old-point)      ))

(defun forms--search-forward (astring arg2 arg3 arg4)
  (if (string= "" astring)
      t
    (search-forward astring arg2 arg3 arg4)))

(defun forms--set-mode-line ()
  "Update the mode line, particularly to show that the backup file is dirty."
  (forms--find-current-record-number)
  (setq mode-line-process
	(concat " " forms--current-record "/" forms--total-records
                (if (buffer-modified-p forms--file-buffer) "*" ""))))

(defun forms-insert-blank-field (n)
 "Insert a blank field after N items."
 ;; need to take car of last field
 (beginning-of-line)
 (forms--re-search-forward forms-field-sep (point-max) t n)
 (insert forms-field-sep))

(defun forms--item-list-visible-p (name)
  "Return t if field NAME is visible."
  (let ((item (assoc name forms-format-list)) )
    (if item
        (format-item-visible-p item))))

(defun forms--next-visible-item (name)
  "Return next visible item after NAME."
  (let* ((items (cdr-assoc name forms-format-list))
         (next-item (car (cdr items))) )
    (if next-item
        (if (format-item-visible-p next-item)
            next-item
          (forms--next-visible-item (format-item-name next-item))))))

(defun cdr-assoc (name list)
  (cond ((null list) nil)
        ((equal name (car (car list)))
         list)
        (t (cdr-assoc name (cdr list)))))

(defun insert-n-times (item N)  ;(insert-n-times "a" t)
  "Insert ITEM (abs N) times."
  (if (numberp N)
      (progn
        (setq N (abs N))
        (while (> N 0)
          (insert item)
          (setq N (- N 1))))
    (if N
        (insert item))))

(defun forms--replace-format-item (position item value)
  (setq sub-item (nthcdr position item))
  (rplaca sub-item value)
  item)

;; These are accessor functions into format-items on the format-list
(defun format-item-name (x) (nth 0 x))
(defun format-item-visible-p (x) (nth 1 x))
(defun format-item-label (x) (nth 2 x))
(defun format-item-field (x) (nth 3 x))
(defun format-item-default (x) (nth 4 x))
(defun format-item-newline-p (x) (nth 5 x))
(defun format-item-field-size (x) (nth 6 x))
(defun format-item-line-size (x) (nth 7 x))

(defun forms--trans (subj arg rep)
  "Replace in SUBJ all characters in ARG with character REP. ARG and REP should
be single-char strings."
  (let ((i 0)
	(x (length subj))
	(re (regexp-quote arg))
	(k (string-to-char rep)))
    (while (setq i (string-match re subj i))
      (aset subj i k)
      (setq i (1+ i)))))


(defun forms--field (record field-num)   ;(forms--field 1 3)
 "Returns the contents of RECORD corresponding to FIELD-NUM (a number) field."
 (let (start end
       (field-sep forms-field-sep)
       (number-of-fields forms-number-of-fields))
 (save-excursion
   (set-buffer forms--file-buffer)
   (forms--goto-line record)
   (forms--re-search-forward field-sep nil nil (- field-num 1))
   (setq start (point))
   (if (= field-num number-of-fields)
       (end-of-line)
     (re-search-forward forms-field-sep nil nil 1)
     (forward-char -1))
   (setq end (point))
   (buffer-substring start end)  )))

(defun forms--get-record ()
  "Fetch the current record from the file buffer."
  ;; This function is executed in the context of the forms--file-buffer.
  (or (bolp) (beginning-of-line nil))
  (let ((here (point)))
    (prog2 (end-of-line)
           (buffer-substring here (point))
           (goto-char here))))

(defun forms--show-record (the-record)
  "Format THE-RECORD according to forms-format-list,
 and display it in the current buffer."
  (forms--find-row-and-column)
  (let ((old-row forms--row)
        (old-column forms--column))
  ;; split the-record
  (let (the-result           ;this let goes away sooner
	(start-pos 0)
	found-pos
	(field-sep-length (length forms-field-sep)))
    (if forms-multi-line
	(forms--trans the-record forms-multi-line "\n"))
    ;; add an extra separator (makes splitting easy)
    (setq the-record (concat the-record forms-field-sep))
    (while (setq found-pos (string-match forms-field-sep the-record start-pos))
      (let ((ent (substring the-record start-pos found-pos)))
	(setq the-result (append the-result (list ent)))
	(setq start-pos (+ field-sep-length found-pos))))
    (setq forms--the-record-list the-result))

  (setq buffer-read-only nil)
  (erase-buffer)
  ;; verify the number of fields, extend forms--the-record-list if needed
  (if (= (length forms--the-record-list) forms-number-of-fields)
      nil
    (beep)
    (message "Record has %d fields instead of %d."
	     (length forms--the-record-list) forms-number-of-fields)
    (setq forms-bad-count-fields 
          (list 'bad-field-count forms-bad-count-fields the-record))
    (sit-for 1)
    (if (< (length forms--the-record-list) forms-number-of-fields)
	(setq forms--the-record-list 
	      (append forms--the-record-list
		      (make-list 
		       (- forms-number-of-fields 
			  (length forms--the-record-list))
		       "")))))

  ;; call the formatter function
  (setq forms-fields (append (list nil) forms--the-record-list nil))
  (funcall forms--format forms--the-record-list)

  ;; prepare
  (set-buffer-modified-p nil)
  (setq buffer-read-only forms-read-only)
  (forms--set-mode-line)

  ;; this should be field based
  (if forms-go-to-beginning-on-jump
      (goto-char (point-min))
    (goto-char (point-min))
    (forward-line (- old-row 1))
    (forward-char old-column))
))

;; note that this is not the interactive version
(defun forms--insert-record (the-record)
  "Format THE-RECORD according to forms-format-list,
and insert it in the current buffer."
  ;; split the-record
  (let (the-result	(start-pos 0)
	found-pos
	(field-sep-length (length forms-field-sep)))
    (forms--trans the-record forms-multi-line "\n")
    ;; add an extra separator (makes splitting easy)
    (setq the-record (concat the-record forms-field-sep))
    (while (setq found-pos (string-match forms-field-sep the-record start-pos))
      (let ((ent (substring the-record start-pos found-pos)))
	(setq the-result (append the-result (list ent)))
	(setq start-pos (+ field-sep-length found-pos))))
    (setq forms--the-record-list the-result))
  (setq buffer-read-only nil)
  ;; verify the number of fields, extend forms--the-record-list if needed
  (if (= (length forms--the-record-list) forms-number-of-fields)
      nil
    (beep)
    (message "Record has %d fields instead of %d."
	     (length forms--the-record-list) forms-number-of-fields)
    (setq forms-bad-count-fields 
          (list 'bad-field-count i forms-bad-count-fields the-record))
    (sit-for 1)
    (if (< (length forms--the-record-list) forms-number-of-fields)
	(setq forms--the-record-list 
	      (append forms--the-record-list
		      (make-list 
		       (- forms-number-of-fields 
			  (length forms--the-record-list))
		       "")))))
  ;; call the formatter function
  (setq forms-fields (append (list nil) forms--the-record-list nil))
  (funcall forms--format forms--the-record-list)
  (insert "\n"))

(defun forms--update ()
  "Update current record with contents of form.  As a side effect: sets
forms--the-record-list."
  (if forms-read-only
      (progn
	(message "Read-only buffer!")
	(beep))

    (let (the-record)
      ;; build new record
      (setq forms--the-record-list (forms--parse-form))
      (setq the-record
	    (mapconcat 'identity forms--the-record-list forms-field-sep))

      ;; handle multi-line fields, if allowed
      (if forms-multi-line (forms--trans the-record "\n" forms-multi-line))

      ;; a final sanity check before updating
      (if (string-match "\n" the-record)
	  (progn (beep)
            (message "Multi-line fields in this record - update refused!"))
	(save-excursion
          ;; in multi-records per page, this will have to be found
	  (set-buffer forms--file-buffer)
	  ;; Insert something before kill-line is called. See kill-line
	  ;; doc. Bugfix provided by Ignatios Souvatzis.
          ;; shouldn't use kill-line, which pushes onto kill-ring
          ;; bug-fix fix provided by FER
	  ;(insert "*")
	  (beginning-of-line)
	  ;(kill-line nil)
          (delete-region (point) 
              (save-excursion (end-of-line)
                              (point)))
	  (insert the-record)
	  (beginning-of-line))))))

(defun forms--update-all ()
 "Update all records with contents of form buffer."
 (save-excursion
 (if forms-read-only
     (progn (message "Read-only buffer!") (beep))
   ;; else
   (if (buffer-modified-p)
   (let (the-record (i 0))
     (goto-char (point-min)) (forward-line 1)
     (save-excursion (set-buffer forms--file-buffer)
                     (goto-char (point-min)))
     (while (< i forms--total-records)
       (message "Checkpointing %s of %s" (+ i 1) forms--total-records)
       ;; build new record
       (setq forms--the-record-list (forms--parse-form))
       (setq the-record
             (mapconcat 'identity forms--the-record-list forms-field-sep))
       ;; handle multi-line fields, if allowed & needed
       (if forms-multi-line (forms--trans the-record "\n" forms-multi-line))
       ;; a final sanity check before updating
       (if (string-match "\n" the-record)
           (progn (message 
                      "Multi-line fields in this record - update refused!")
                  (beep))
         (save-excursion
         ;; in multi-records per page, this will have to be found
         (set-buffer forms--file-buffer)
	 ;; Insert something before kill-line is called. See kill-line
	 ;; doc. Bugfix provided by Ignatios Souvatzis.
         ;; shouldn't use kill-line, which pushes onto kill-ring
         ;; bug-fix fix provided by FER
	 (beginning-of-line)
         (delete-region (point) 
             (save-excursion (end-of-line)
                             (point)))
	 (insert the-record)
	 (forward-line 1)))
       (forward-line 2)
       (setq i (+ 1 i)))
       (set-buffer-modified-p nil))
   (message "No checkpointing necessary.")  ))))


;; Sample:
;; (defun my-new-record-filter (the-fields)
;;   ;; numbers are relative to 1
;;   (aset the-fields 4 (current-time-string))
;;   (aset the-fields 6 (user-login-name))
;;   the-list)
;; (setq forms-new-record-filter 'my-new-record-filter)

;; note: this is the interactive version, and it creates a new record.
;; The below doc is not true, but for documentary purposes only
(defun forms-insert-record (arg)
  "Create a new record before the current one. With ARG: store the
record after the current one.
If a function forms-new-record-filter is defined, or forms-new-record-filter
contains the name of a function, it is called to fill (some of) the fields
with default values."
  (interactive "P")
  (let ((ln (if arg (1+ forms--current-record) forms--current-record))
        the-list the-record)
    (forms--set-mode-line)
    (if forms--new-record-filter
	;; As a service to the user, we add a zeroth element so she
	;; can use the same indices as in the forms definition.
	(let ((the-fields (make-vector (1+ forms-number-of-fields) "")))
	  (setq the-fields (funcall forms--new-record-filter the-fields))
	  (setq the-list (cdr (append the-fields nil))))
      (setq the-list (make-list forms-number-of-fields "")))
    (setq the-record (mapconcat 'identity the-list forms-field-sep))
    (save-excursion (set-buffer forms--file-buffer)
                    (forms--goto-line ln)
                    (open-line 1)
                    (insert the-record)
                    (beginning-of-line))
    (setq forms--current-record ln)
  (setq forms--total-records (1+ forms--total-records))
  (if (not arg)
      (forms-beginning-of-form 1)
      (forms-end-of-form 1))
  (forms--insert-record the-record)
  (forms--renumber-buffer)
  (forms--set-mode-line)    ))

(defun forms-duplicate-record ()
  "Create a duplicate record of the current one."
  (interactive)
  (forms--find-current-record-number) ;update forms--current-record
  (let ((ln forms--current-record)
        the-record)
    (message "Duplicating record %s." ln)
    (save-excursion (set-buffer forms--file-buffer)
                    (forms--goto-line ln)
                    (setq the-record (forms--get-record))
                    (open-line 1)
                    (insert the-record)
                    (beginning-of-line))
    (setq forms--total-records (1+ forms--total-records))
    (forms-beginning-of-form 1)    
    (forms--insert-record the-record)
    (forms--renumber-buffer)
    (forms--set-mode-line)
    (message "Leaving in the middle of the two duplicated records.")    ))

(defun forms--goto-line (N)
  (let (narrow-count wide-count)
    (save-restriction
      (setq narrow-count
            (count-lines (point-min) (point-max)))
      (widen)
      (setq wide-count
            (count-lines (point-min) (point-max)))
      (goto-line (+ (- wide-count narrow-count)
                    N))      )))

(defun forms-delete-record (arg)
  "Deletes a record. With ARG, don't ask."
  (interactive "P")
  (forms--set-mode-line)
  (if (or arg (y-or-n-p "Really delete this record? "))
      (let ((ln forms--current-record)
            (old-point (point)))
	(save-excursion
            (set-buffer forms--file-buffer)
            (forms--goto-line ln)
            (beginning-of-line)
            (delete-region (point)
                   (save-excursion 
                      (end-of-line)
                      (+ 1 (point)))))
        (delete-region ; in printed out buffer, save redrawing
              (save-excursion (forms-beginning-of-form 1) (point))
              (save-excursion (forms-end-of-form 1) (+ 1 (point))))
	(setq forms--total-records (1- forms--total-records))
	(if (> forms--current-record forms--total-records)
	    (setq forms--current-record forms--total-records))
        ;; this could be optimized
        (forms--renumber-buffer)
        (message "Record deleted.")
        (goto-char old-point)
        (forms--set-mode-line))
     (message "Record not deleted.")))

(defun forms-search (regexp)
  "Search REGEXP in file buffer."
  (interactive 
   (list (read-string (concat "Search for" 
				  (if forms--search-regexp
				   (concat " ("
					   forms--search-regexp
					   ")"))
				  ": "))))
  (forms--set-mode-line)
  (if (equal "" regexp)
      (setq regexp forms--search-regexp))
  (let (the-line the-record here
		 (fld-sep forms-field-sep))
    (if (save-excursion
	  (set-buffer forms--file-buffer)
	  (setq here (point))
	  (end-of-line)
	  (if (null (re-search-forward regexp nil t))
	      (progn
		(goto-char here)
		(message (concat "\"" regexp "\" not found."))
		nil)
	    (setq the-record (forms--get-record))
	    (setq the-line (1+ (count-lines (point-min) (point))))))
	(progn (setq forms--current-record the-line)
	  (forms--show-record the-record)
	  (re-search-forward regexp nil t))))
  (forms--set-mode-line)
  (setq forms--search-regexp regexp))

(defun count-regexp (regexp limit)
  (save-excursion
    (let ((result 0))
      (while (re-search-forward regexp limit t)
         (setq result (+ 1 result)))
      result)))
;(count-regexp "e" (point-max))

(defun forms-enumerate (the-fields)
  "Take a quoted list of symbols, and set their values to the numbers
1, 2 and so on. Returns the higest number.

Usage: (setq forms-number-of-fields
             (forms-enumerate
              '(field1 field2 field2 ...)))"

  (let ((the-index 0))
    (while the-fields
      (setq the-index (1+ the-index))
      (let ((el (car-safe the-fields)))
	(setq the-fields (cdr-safe the-fields))
	(set el the-index)))
    the-index))


;;;
;;;	XVI.	Debugging functions
;;;

(defvar forms--debug nil
  "Enables forms-mode debugging if not nil.")
;; no leading *, not a user variable but a developer variable
;; if it really is a user variable, better to put it at top of file.

(defun forms--debug (&rest args)
  "Internal - debugging routine"
  (if forms--debug
      (let ((ret nil))
	(while args
	  (let ((el (car-safe args)))
	    (setq args (cdr-safe args))
	    (if (stringp el)
		(setq ret (concat ret el))
	      (setq ret (concat ret (prin1-to-string el) " = "))
	      (if (boundp el)
		  (let ((vel (eval el)))
		    (setq ret (concat ret (prin1-to-string vel) "\n")))
		(setq ret (concat ret "<unbound>" "\n")))
	      (if (fboundp el)
		  (setq ret (concat ret (prin1-to-string (symbol-function el)) 
				    "\n"))))))
	(save-excursion
	  (set-buffer (get-buffer-create "*forms-mode debug*"))
	  (goto-char (point-max))
	  (insert ret)))))


;;;
;;;	N.	Final code
;;;

(run-hooks 'forms-mode-load-hook)

;;;
;;;  dead code worth saving
;;; 



;;; Local Variables:
;;; eval: (headers)
;;; eval: (setq comment-start ";;; ")
;;; End:

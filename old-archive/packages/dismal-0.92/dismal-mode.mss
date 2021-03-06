@device(postscript)
@style[font=TimesRoman, size = 11]
@define(SmallerFont, size = 10)
@make(manual)
@include(/afs/andrew.cmu.edu/usr24/fr07/psy/mss/inits.mss)
@tabset(3.5 inches)

@begin(titlepage)
@begin(comment)
Title page based on /afs/cs/project/soar/5.2/emacs/soar/new/notes/soar-mode.mss
 -- TFMcG 24-Jan-92
@end(comment)
@begin(titlebox)
@Heading(Dismal: A spreadsheet for GNU-emacs)
@center[For GNU release 18.5X]

Frank E. Ritter@+(*) and David Fox@+(**)

ritter@@psych.nott.ac.uk    fox@@cs.nyu.edu
    (or @cs.cmu.edu)

@center[@value(date)]
@blankspace(3 lines)

@+(*)Psychology Department 
AI Group and CREDIT
U. of Nottingham
Nottingham, England
NG7 2RD

@+(**)Dept. of Computer Science 
New York University
New York, New York
@end(titlebox)

@begin(Abstract) This is a manual for dismal (Dis' Mode Ain't lotus),
a major mode in GNU-emacs that implements a spreadsheet.  It includes
how to load, run, and use dismal and dismal spreadsheets.  Dismal
provides the basic spreadsheet functions.  Because it is based on
GNU-emacs it offers several relatively novel features for a
spreadsheet.  It is designed to be keystroke driven rather than
mouse/menu driven (although it can be menu driven).  It is extensible,
so that users can write their own commands and functions, for example,
to allow a function cell to write to several nearby cells.  A ruler
can be put up that reflects the semantics of column names past the
ones automatically provided as letters.

@end(abstract)

@copyrightnotice(F. E. Ritter & D. Fox)

@begin(researchcredit)

This work was sponsored in part by a training grant from the Air Force
Office of Scientific Research, Bolling AFB, DC; and in part by the
Avionics Laboratory, Wright Research and Development Center,
Aeronautical Systems Division (AFSC), U. S. Air Force,
Wright-Patterson AFB, OH 45433-6543 under Contract F33615-90-C-1465,
ARPA Order No. 7597.  It is currently supported by the Economic and
Social Science Research Council's Centre for Research in Development,
Instruction and Training.

The views and conclusions contained in this document are those of the
authors and should not be interpreted as representing the official
policies, either expressed or implied, of the U.S. or U.K.
Governments.

@end(researchcredit)
@end(titlepage)

@set(page=0)
@newpage

@apaheading3(Distribution Notes)

Separate documents are available for the companion pieces of software
mentioned here, soar-mode and taql-mode, which modify GNU-Emacs to
more directly support programming in the Soar cognitive modeling
language though a structured editor with useful commands such as
automatic production loading and match set displays.

This piece of software is copy lefted as per the Free Software
Foundation's standard agreement.  Other pieces the Developmental Soar
Interface is placed in the public domain.  You are free to copy it as
you wish.  The Developmental Soar Interface and all of its parts: Soar
in X (SX), taql-mode, and soar-mode, are made available AS IS (like
Soar itself), and U. of Nottingham, Carnegie-Mellon University, the
University of Michigan, and its developers, make no warranty about the
software or its performance.  Please contact soar-bugs@@cs.cmu.edu for
more information or to report problems.

Some of the supporting software comes with different copyright conditions.
Soar-mode and taql-mode use several utility programs that are protected under
the Free Software Foundation's Copyleft agreement.

@newpage

@chapter(Basics)
@blankspace(1 line)

Dismal (DIS' Mode Ain't Lotus) is a spreadsheet implemented in
GNU-emacs.  Where you would move around through text in a normal
buffer, in a dismal buffer you move through cells.  Dismal files are
saved with the same keybindings as other files (although with
different implementations), and a plain text and a tabbed output
format are also available. Formulae can be entered like in other
spreadsheets.

There are several major differences from other spreadsheets because
dismal lives within GNU-Emacs.  First of all, it is free.  It was
developed by Fox and Ritter as part of their thesis work, and is
offered as is.  It is also copylefted, meaning that you are provided
with the full source code, and agree to provide free copies to others.
This also means that it is not fully supported.  Several people have
passed back comments, bug reports and bug fixes (most notably Erik
Altmann and Robert J. Chassell), and these improvements have greatly
added to dismal.

Being implemented in GNU-Emacs offers several technical differences.
The main disadvantages are that it is not as fast as a commercially
supported stand alone spreadsheet.  On the other hand, it exists in a
text editor and what it can't support, the underlying editor can.
Where possible, we have made GNU's text manipulation commands and
keybindings work appropriately for spreadsheet cells.  For example,
C-w, a command to cut and copy (kill in emacs-speak) a region of text,
is now a commnad to cut and copy a range of cells.

It is also quite flexible.  The GNU-Emacs' lisp interpreter is
immediately available, so extensions and modifications can be realized
fairly simply.

There are no set limits to the size of spreadsheets that you can use,
but there are practical limits.  Generally, 500 rows x 40 columns is
too much (20,000 cells); but 400 x 24 is managable (9600 cells).
There are over 20 known bugs or limits to the features.  These are
documented in the front of the source code.

@section(Terms)

@ux(Mark.)  Mark, which used to be a text position, is now the marked
cell.  Typing "m", C-SPACE, or C-@ will set the mark.
C-x C-x (Exchange mark and point) works now with cell positions.

@ux(Point.)  Point is the current cell.

@ux(Ranges.)  The idea a text range in GNU-Emacs is modified and used in
dismal as the region of cells between point and mark.  When the variable
@i(dismal-show-selected-ranges) is t, the user is shown the selected range
when cutting or erasing.  When the user kills a range, it is saved in
dismal's kill buffer.  This buffer can be yanked and inserted in the
initial buffer, or other dismal buffers as well..

@ux(Menu.)  When the user types C-c C-m a simple menu appears on the
bottom of the Emacs display in what is called the message line.  Users
can type the first letter of a menu item to choose that item.  There
may be multiple levels, and users can type ahead.  Typing "?" or SPACE
will popup a help screen providing more details on the menu items and
their keybindings (if any).  After a user selects an item and it has
been executed, its keybinding will be displayed in the message window
as well.  This supports migration to using the keybindings instead of
the menus.   There are two conventions used to indicate information
about menu items.  When a command ends in a period, that item will
request further information.  When an item ends with a slash, that
item is another menu.

@ux(Ruler.) Dismal supports putting up a ruler at the top of the window
indicating the contents of columns.  The default ruler is the letters heading
the columns, followed by dashes and crosses (+----+--+---+) indicating the
column borders.  Users can change this by selecting the menu @i(Options: Set:
RulerRow).  Advanced users can set the variable @i(dismal-ruler-row).  If a
ruler is not desired, it can be turned off by selecting the menu @i(Options:
Set: 2Ruler), or by setting the variable dismal-show-ruler.  Ruler settings
are remembered across sessions.

The ruler gets redrawn at the top of the screen with C-x r.  If an
argument is passed to C-x r (e.g., C-u C-x r), the ruler is remade,
and any changes in the ruler rule will appear in the ruler.

@ux(Mode line.) This is the inverse video line that appears at the
bottom of each window.  In dismal it displays the file you are
editing, the current cell, the status of the cell updating algorithm
(automatically if AutoUp is displayed, or upon request if ManUp is
displayed), the mode (in this case dismal), the metacolumn (the <
column-name ] indicates that the left column goes from the @i(middle
column) all the way to the right, and includes the middle-column, but
no more), the mode (dismal in parenthesis), and the percentage of the
file displayed.  For example,

--**- bob.dis              A6 AutoUp <]  (dismal)---All----------------

@section(Editing a cell)

You can put numbers (integer and floating point), strings, and formula
into cells.  In each case you type on of the commands below, and then
enter the value, and then hit return.

<               dismal-read-cell-leftjust
=               dismal-read-cell-default
>               dismal-read-cell-rightjust
e               dismal-read-cell-plain
|               dismal-read-cell-center

If you want to modify or delete a cell's contents, these commands are
available. 

DEL             dismal-backward-kill-cell
C-d             dismal-delete-cell (doesn't save contents in kill buffer)
ESC c           dismal-capitalize-cell
ESC d           dismal-kill-cell
ESC l           dismal-downcase-cell
ESC u           dismal-upcase-cell
ESC DEL         dismal-backward-kill-cell
ESC C-t         dismal-transpose-cells


@section(Editing the spreadsheet)

In addition to modifying a single cell, you can also modify the
spreadsheat's topology as a whole by inserting and deleteing ranges of
cells, such as rows or columns.  All of these commands should behave
nicely and in a way analogous to their text counterparts.

C-k             dismal-kill-line
C-o             dismal-open-line
C-w             dismal-kill-range
C-y             dismal-paste-range
c               dismal-copy-range
d SPC           dismal-delete-blank-rows
d r             dismal-delete-row
d d             dismal-delete-range
d c             dismal-delete-column

i r             dismal-insert-row
i .             dismal-insert-cells
i z             dismal-insert-z-box
i i             dismal-insert-range
i c             dismal-insert-column

v               dismal-paste-range
x               dismal-kill-range
C-x TAB         dismal-insert-file
C-x i           dismal-insert-file (assumes tabbed input)
ESC C-e         dismal-erase-range
ESC w           dismal-copy-range
ESC o           dismal-insert-range

@section(Moving around)

Most of the commands you know and love from plain emacs work in a
corresponding way here.  "C-c" is used to preface most dismal-mode
specific commands.

C-a             dismal-first-column
C-b             dismal-backward-column
C-e             dismal-end-of-row
C-f             dismal-forward-column
TAB             dismal-forward-column
RET .. C-n      dismal-forward-row
C-p             dismal-backward-row
C-v             dismal-scroll-up-in-place
SPC             dismal-forward-column
j               dismal-jump (prompts for where)
n               dismal-next-filled-row-cell
p               dismal-previous-filled-row-cell
C-x ]           dismal-end-of-col
C-x [           dismal-start-of-col
ESC RET         dismal-backward-row
ESC v           dismal-scroll-down-in-place
ESC r           dismal-move-to-window-line
ESC p           dismal-previous-filled-row-cell
ESC n           dismal-next-filled-row-cell
ESC f           dismal-forward-filled-column
ESC e           dismal-last-column
ESC b           dismal-backward-filled-column
ESC TAB         dismal-backward-column
ESC >           dismal-end-of-buffer
ESC <           dismal-beginning-of-buffer
ESC SPC         dismal-backward-column

@section(Other Commands)

C-r             dismal-isearch-backwards
C-s             dismal-isearch
?               describe-mode
r               dismal-hard-redraw-row
z               dismal-redraw-range
C-c RET         dismal-run-menu
C-x C-x         dismal-exchange-point-and-mark

C-x C-w         dismal-write-file
C-x C-s         dismal-save-file
C-x s           dismal-save-some-buffers

C-x r           dismal-update-ruler
ESC C-u         dismal-update-matrix
ESC C-r         dismal-redraw
ESC q           dismal-query-replace

@section(Number representations)

The current representation of numbers is inadequate for many users.
We are investigating using the calc package, or the data types that
will be available in GNU Emacs version 19.

@ux(Integer numbers.)  Currently we use Emacs Lisp integers as the
basis for the integers.  They are limited on most machines to -2@+(12)
to 2@+(12) - 1.  Some machines will have a larger exponent, but you
will still be limited to a fixed integer range.

@ux(Floating pointer numbers.)  We currently are using Rosenblatt's
float.el package to represent floating point numbers.  These numbers
are actually made up out of Emacs Lisp integers, so they too suffer a
limited precision (and range, but at 10@+(2@+[12]) you don't notice so
much).

@ux(Imaginary numbers.)  Are only imaginary at this point.  If calc is
cut in, then they become more feasible.

@section(Installation) 

There will be two installations, one for the site and one for each user.

@subsection(Site installation)

Dismal is faster in byte compiled elisp.  It is faster yet in
"bytecomp" byte compiled elisp.  Bytecomp is an improved bytecompiler
available from archive.cis.ohio-state.edu in
/pub/gnu/emacs/elisp-archive/packages/bytecomp.tar.Z.  The files are
designed to take advantage of this compiler.  Intalling it takes about
20 minutes.  If you can read emacs documentation and know what a
compiler is, you should be able to do this.

The font paths in dismal-simple-menu.el defining where the X display
fonts live will have to be updated for your site.  If you don't know
where, you will not be able to change the display font size, and
attempts to do this will results in error messages (but no crashes).

The value of dismal-directory in dismal-mode-defaults.el must be
updated for your site.

Once dismal has been set up, simple users only have to add to their
.emacs file the following command:

(load "... where dismal lives .../dismal-mode-defaults.el")


@section(User settable variables)

There are several variables that influence dismal's behavior on a
global level.  Users can set these variables while running (with M-x
set-variable) or by inserting a setq in their .emacs file.
These are the variables you might want to set.

@begin(format)
(defvar dismal-hooks nil
  "*Hook functions to be run upon entering dismal-mode.")

(defvar dismal-load-hook nil
  "*Hook variable run after dismal-mode is loaded.")

(defvar dismal-recursion-limit 9
  "*Maximum depth allowed when evaluating (perhaps) circular dependencies.")

(defvar dismal-query-on-entry-p t
  "*Ask for confirmation each time dismal-mode is called.  Normally
unimportant for normal users, who should have this set to nil.  In released
versions this should be set to nil.")

(defvar dismal-default-column-width 10
  "*Default width for columns.")

(defvar dismal-default-column-alignment 'default
 "*Default way to align a cell.")

(defvar dismal-default-column-decimal 2
 "*Default number of decimal digits for a cell.")

(defvar dismal-auto-save-interval 300
  "*Number of dismal keyboard input characters (?) between auto-saves.
Zero means disable autosaving.")

(defvar dismal-field-sep "\t"
  "*Default field separator character when working with other system
dump files (default TAB).")

(defvar dismal-page-size 66
  "*Anticipated page size for printing.")
;; 66 works well for 8.5"x11" with a 10 (?) pt. font.
@end(format)

@chapter(Using other spreadsheet program's data) 

You can transfer data between dismal and other spreadsheats in both
directions.  To pass data to dismal, write
out a tabbed file with the other program.  Read it
in with dismal-insert-file, bound to C-x C-i (similarly bound to
insert-file).  

To pass data from dismal, use the dismal-dump-range command (M-X
dismal-dump-range) to dump a range to a tabbed file.  If you want to
write out the whole file this way, move to the beginning of the file
(M-<), mark the first cell (M-SPACE), move to the end of the file
(M->), and then either select this command on the menu (C-c C-m I/O
RDump), or call it by hand as noted above.  You can also get a plain
text version through the Fprint (file print) command on the same menu.

@chapter(Creating and using a sheet) 

@ux(Creating a spreadsheet) After you have installed the code, and set
up your .emacs, you can create new spredsheets just as you would
create another file that lives in Emacs: (a) you can create a new
dismal file by typeing C-x C-f (find-file) new-file-name.dis, and
dismal-mode will get loaded (if necessary) and create an initial blank
spreadsheet. (b) You can also create a dismal spreadsheet by calling
dismal-mode in an existing blank buffer, but this is best lest to
people debugging dismal.

@section(Moving around)

You can move around in a dismal buffer in a similar way to a regular
buffer.  There are certain items that work differently.  The ruler
will reappear when you scroll off the page with
scroll-up/down-in-place (C-v, M-v).  If you scroll down with next-line
(C-n) or similar commands, the ruler is not refreshed.  Not refreshing
the ruler speeds travel through the spreadsheet.  The ruler can be
brought back by recentering the buffer (C-l).

If your emacs is compiled with the X window options, clicking a left
mouse will move you to the pointed at cell.

@section(Entering functions as cell values)

There are still two types of number used in the dismal spreadsheet,
integers and floats.  They both can be corced into each other with the
functions f and fint.  Most functions starting with 'dis-' should be
able to translate for you on the fly, but the functions that begin
with f require that their input be floating points numbers (and will
cause an error otherwise).

You can put (f+ NUM1 NUM2) as the value of a cell.  f+ does not take more than
two arguments. (Is there a good way around this?  Could we make it a macro?)

You can also use the following lisp functions to compute a cell's
value.  The format for all of them is a lisp S-expressions.  A simple
example would be "(dis-count a0:b23)".  More complicated expressions
(preserving type, integer or float) can also be built up.  Several
exmaples are: (dis-div A19 (dis-sum A19:B19)) and 
(dis-div (dis-sum A0:A3) (f 34)).

@begin(itemize)
dis-count -- Given a cell RANGE computes the count of filled cells.

dis-count-if-regexp-match -- Given a cell RANGE computes the number of cells 
that match REGEXP.

dis-sum -- Given a cell RANGE computes the sum of filled cells.

dis-product -- Given a cell RANGE computes the product of filled cells.

dis-current-date -- Insert current date as string. If DAY-FIRST is t, do that.

dis-date-to-days -- Return number of days between Jan 1, 1970 and DATE 
(a string).

f -- Convert the integer argument to floating point, like a C cast operator.

ftrunc -- Truncate the fractional part of a floating point number.

fint -- Convert the floating point number to integer, with truncation, 
like a C cast operator.

fmin -- Returns the minimum of two floating point numbers.

fmax -- Returns the maximum of two floating point numbers.

f+ -- Returns the sum of two floating point numbers.

f/ -- Returns the quotient of two floating point numbers.

f* -- Returns the product of two floating point numbers.

f- -- Returns the difference of two floating point numbers.

@end(itemize)

The forumula are actually evaluated as lisp s-expressions, so you may also set
variables within the formula.  The variables must be declared or set before
they can be used, so all uses of them must come after they are set (initial
evaluation is done in a right to left, top to bottom order order).
For example, (setq multiplier 34).

@chapter(Editing)

The character "%" can be entered as "%".  It is stored as %% because %
is difficult to print out on its own.  In the future this burr should
be hidden from the user, but at present the user must be careful to
only enter a single % when reediting cell contents.

@chapter(Formatting)

Dismal supports printing out cell contents in several ways.
The user can set the number of decimal columns to print out,
the alignment (justification) of the cell contents (flush right, flush
left, centered, and the default of numbers right and strings left),
the cell width, and the display fonts.  All these commands are
available on the main menu (C-c C-m) under Format.  

You can also modify the justification a single cell by editing it with
"<", ">", and "|".  These keystroke commands allow you to edit a cell,
and then set the cell's justification to be left, right, or centered.
The default way of editing a cell, "e" or "=", leave the cell's
justification alone.

If the ruler changes because the cell contents change in the row that
makes it up, the user can redraw the ruler by using the command in the
Main: Format menu (also by typing C-x r).

@chapter(Analyzing and calcuating a worksheet)

Once the dismal-mode-default.el file has been loaded, you can create a
worksheet by simply opening a file (C-x C-f) that ends in ".dis".  You
can edit cells by typing "e" or "=".

@section(Creating a series of dates or numbers)

There are commands to manipulate numbers as dates.  dis-current-date
provides the current date as a string.  dis-date-to-days returns the
number of days between Jan 1, 1970 and DATE (a string with format
dd-mmm-yy), ingnoring leap years.

@section(Controlling calculation)

You can do this.  The dismal defaults provide for automatic updates of
cells and their dependencies whenever a cell changes through
insertion, deletion or editing.  On large spreadsheets with lots of
cells, this can take a long time.  Manual updating can be invoked
through the (Main: Options: Set: AutoUpdate) menu.  Manual updating
can then be done through the menu (Main: Commands: Update) upon
request only.

@chapter(Databases)

In general you can't do databases like Excel supports, yet.  But there
is one command that may be directly useful to you that is part of
GNU-Emacs, and you could write futher ones.

list-matching-lines is a function that shows all lines following point
containing a match for the REGEXP (which it prompts for).  The list of
matching lines is shown in the *Occur* buffer, which is erased each time the
function is called.

The variable list-matching-lines-default-context-lines denotes the
default number of context lines to include around a line matched by
list-matching-lines.

If you copy the contents of a dismal buffer into a scratch buffer, you
can manipulate the resulting buffer with M-x
delete-non-matching-lines, which deletes all lines except those
containing matches for REGEXP, and and delete-matching-lines, which
does the opposite.

@chapter(Creating reports and printing)

Files are dumped into <file-name>.dp as text files augmented with extra copies
of the ruler, either a row you've set, or the A B C crap.  This file can be
printed on a line printer, or automatically from within dismal.  We suggest
using the enscript program (the default) to convert it to postscript and print
it, but you could set dismal-raw-print-command to be lpr if you wish.

dismal-clean-printout is a function that can be called interactively
(M-x dismal-clean-printout) to cleanup a .dp file.  It removes the
leading headers, alphabetic column labels, and the leading two digit
number (this should be fixed).

@chapter(Designing and Writing macros)

You can do this directly.  At the end of the dismal loading process,
the functions on the dismal-load-hook are called.  If you wish to
load additions, you can put them in a file, and put a lambda
expression to load them there.  For example,

(setq dismal-load-hook 
   '(lambda () (load "/afs/nott/usr/ritter/my-dismal-file.el")))

@section(Using references)

You can do this.

@section(Auxillary commands)

@i(copy-to-dismal) 

Copies a column specified by point and mark to buffer DISMAL-BUFFER-NAME
starting at its current cell.  Point and mark must be within or at the
beginning of a column of text, delimited by blanks.  The column must
contain words without spaces or valid dismal numbers, which may
contain decimal points but not commas.  The variable 
dismal-copy-column-seperator (default is space) is used to separate columns.

For example, in the following column, you would place point on 2 in
123 and mark on the 8 in 789 and only that middle column would be
copied to the spreadsheet.

@begin(verbatim)
Jan    123   555
Feb    456   666
Mar    789   777
@end(verbatim)

@subsection(Model-based manipulation)

There are a set of commands to make use of the two metacolumns by
treating one of them as a series of model predictions, and the other a
series of subject actions.  

The function dis-initialize-operator-codes will help you
initialize the dismal with operator codes.  Once these are set up, you
can insert them into a cell by typing C-c C-M-c, which will give you a
menu of codes that can be inserted.

After codes have been inserted, they can be aligned with objects in
the other metacolumn.

To do this, first turn auto-update off (this speeds up the process
considerably).  Set up middle-column that defines the two
metacolumns.  This can be done as a menu action.  Set up the pairs of
regular expresions that define objects that are equivalent across the
two columns.  These should be put in dis-paired-regexps.
Here's an example

;; "Pairs of equivalent regexps that match valid pred & obs codes"
(setq spa-paired-regexps  
      '( ("^C,C$" . "O: double-click-button")
         ("^M(" . "O: move-mouse")
         ("^C$" . "O: click-button")
         ("^C(" . "O: click-button")
         ("^D$" . "O: press-button")
         ("^U$" . "O: release-button")))

You can then call the function dis-auto-align-model, and following the
prompts provided there, align the two metacolumns.
It will ask you for the columns to compare from each metacolumn, and
which row to start and end with.

Once the alignment has been completed, there are several other
functions you can call to see how it went:

dis-model-match-op, when given a cell RANGE-LIST, computes the
percentage of colA matched with something in colA-2, and col A is an
operator (it begins with "O: ").  It only counts stuff that is in
order.

dis-model-match, when given a cell RANGE-LIST, computes the percentage
of colA matched with something in colA-1.  Only counts stuff that is
in order.

@chapter(Known bugs and interactions)

BIGGEST WARNING: Don't change the mode
When you change a dismal buffer into another mode, you reset all its
local variables.  In doing so, you delete the underlying data
structures, and are, in a word, completely hosed --- You will not be
able to correctly save that buffer ever again, nor will modifications
that you make be implemented in the underlying data structures.  For
example, this can happen if you accidently or on purpose change into
text-mode or fundamental mode.  I would turn off the ability to change
the mode if I could, but I don't see how.  This is in general
dangerous, for while you can save the buffer's textual contents, you
aren't saving the fundamental aspects of the speadsheet.  You won't
know that this has happened until you try to open and edit the file.

There is no true undo facility (undo will undo the drawing operations,
but not the underlying changes to the spreadsheet).  We therefore
suggest that you use a conservative number for the autosave variable
value dismal-auto-save-interval.  This number represents the number of
cell movements between autosaves.  Its default is 1000.

Large (20x500) Dismal files have tendency to crash GNU-Emacs 18.54 on
loading or after working for a while.  This behavior has not been
observed in 18.57.

Dismal accepts cell references up to XXXXNN or XXNNNN.  This means
that cell values like John89 will be parsed as a cell reference rather
than text, like a citation.  You can get around this by putting such
references in as quoted strings ('"Eb1"' instead of 'Eb1'), or by
putting a space or other character on the front of such strings ('
Eb1' instead of ' Eb1').

You can read in and write out formulas as S-expressions.  This also
means that if you have cell items like: "(and this is a comment in
parenthesis.)", you may have problems.  Dismal will attempt to read it
as a function call of "and".  If you run into this problem, you can
avoid it by making sure that each cell in parenthesis does not begin
with a valid function.  One could also modify dismal to use a flag
while reading in files that sez "don't attempt to read in functions".

If the dependencies get out of hand, which some users report happens,
M-x dismal-fix-dependencies will clean them up.

@chapter(Keybindings)

These are all the keybindings as noted in the online help (C-c C-h; or
menu Main: Help).

C-@             dismal-set-mark-command
C-a             dismal-first-column
C-b             dismal-backward-column
C-c             Prefix Command
C-d             dismal-delete-cell
C-e             dismal-end-of-row
C-f             dismal-forward-column
TAB             dismal-forward-column
C-k             dismal-kill-line
RET .. C-n      dismal-forward-row
C-o             dismal-open-line
C-p             dismal-backward-row
C-q             dismal-quoted-insert
C-r             dismal-isearch-backwards
C-s             dismal-isearch
C-t             dismal-no-op
C-v             dismal-scroll-up-in-place
C-w             dismal-kill-range
C-x             Prefix Command
C-y             dismal-paste-range
ESC             Prefix Command
SPC             dismal-forward-column
-               negative-argument
0 .. 9          digit-argument
<               dismal-read-cell-leftjust
=               dismal-read-cell-default
>               dismal-read-cell-rightjust
?               describe-mode
c               dismal-copy-range
d               Prefix Command
e               dismal-read-cell-plain
f               dismal-read-column-format
i               Prefix Command
j               dismal-jump
m               dismal-set-mark-command
n               dismal-next-filled-row-cell
p               dismal-previous-filled-row-cell
r               dismal-hard-redraw-row
v               dismal-paste-range
x               dismal-kill-range
z               dismal-redraw-range
|               dismal-read-cell-center
DEL             dismal-backward-kill-cell

C-c RET         dismal-run-menu

C-x >           dismal-no-op
C-x ]           dismal-end-of-col
C-x [           dismal-start-of-col
C-x C-x         dismal-exchange-point-and-mark
C-x C-w         dismal-write-file
C-x C-s         dismal-save-file
C-x s           dismal-save-some-buffers
C-x r           dismal-update-ruler
C-x TAB         dismal-insert-file
C-x i           dismal-insert-file

ESC C-u         dismal-update-matrix
ESC C-t         dismal-transpose-cells
ESC C-r         dismal-redraw
ESC RET         dismal-backward-row
ESC C-e         dismal-erase-range
ESC C-k         dismal-no-op
ESC ,           dismal-no-op
ESC %           dismal-query-replace
ESC =           dismal-debug-cell
ESC w           dismal-copy-range
ESC v           dismal-scroll-down-in-place
ESC u           dismal-upcase-cell
ESC t           dismal-no-op
ESC r           dismal-move-to-window-line
ESC q           dismal-query-replace
ESC p           dismal-previous-filled-row-cell
ESC o           dismal-insert-range
ESC n           dismal-next-filled-row-cell
ESC l           dismal-downcase-cell
ESC k           dismal-no-op
ESC j           dismal-align-metacolumns
ESC i           dismal-no-op
ESC h           dismal-no-op
ESC g           dismal-no-op
ESC f           dismal-forward-filled-column
ESC e           dismal-last-column
ESC d           dismal-kill-cell
ESC c           dismal-capitalize-cell
ESC b           dismal-backward-filled-column
ESC a           dismal-no-op
ESC TAB         dismal-backward-column
ESC ]           dismal-no-op
ESC [           dismal-no-op
ESC >           dismal-end-of-buffer
ESC <           dismal-beginning-of-buffer
ESC SPC         dismal-backward-column
ESC DEL         dismal-backward-kill-cell

d SPC           dismal-delete-blank-rows
d r             dismal-delete-row
d d             dismal-delete-range
d c             dismal-delete-column

i r             dismal-insert-row
i .             dismal-insert-cells
i z             dismal-insert-z-box
i i             dismal-insert-range
i c             dismal-insert-column


 Special commands:

Uses keymap "dismal-minibuffer-map", which is not currently defined.

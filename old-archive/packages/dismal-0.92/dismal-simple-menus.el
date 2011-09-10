;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;              
;;;; File            : dismal-simple-menus.el
;;;; Author          : Frank Ritter
;;;; Created On      : Mon Jan  6 21:19:01 1992
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Mon Oct 11 19:24:06 1993
;;;; Update Count    : 129
;;;; 
;;;; PURPOSE
;;;; 	Describe the simple-menus in dismal-mode.
;;;; TABLE OF CONTENTS
;;;;
;;;;	i.	Requires and provides
;;;;
;;;;	I.	Main menu
;;;;	II.	File menu and children
;;;;	III.	Edit menu and children
;;;;	IV.	Move menu and children
;;;;	V.	Commands menu
;;;;	VI.	Format menus
;;;;	VII.	Options menu
;;;;	VIII.	Set variables menu
;;;;	IX.	Model menus 
;;;;	X.	Misc. menus used outside of main menu
;;;; 
;;;; Copyright 1992, Frank Ritter.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Status          : Unknown, Use with caution!
;;;; HISTORY
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;;	i.	Requires and provides
;;;

(require 'simple-menu)
(provide 'dismal-simple-menus)


;;;
;;;	I.	Main menu and driver
;;;

(defun dismal-run-menu ()
  "Provide a menu of commands for dismal."
  (interactive)
  (run-menu 'dismal-menu))

(def-menu 'dismal-menu
  "Dismal" ;main prompt
  "This menu allows you to select all command options in dismal."
   ;123456789012345
 '(("Help            About dismal." describe-mode)
   ("IO/             Do dismal file I/O commands." dismal-file-menu)
   ("Edit/           Edit the spreadsheet." dismal-edit-menu)
   ("Go/             Move around." dismal-move-menu)
   ("Commands/       Special commands." dismal-commands-menu)
   ("Format/         Set up the format of cells, columns, and the sheet." 
                     dismal-format-menu)
   ("Doc/            Read the dismal documentation."
                     (goto-manual "dismal-mode.doc" 'text-mode))
   ("Options/        Miscellaneous commands." dismal-options-menu)
   ("Model/          Model based manipulations and actions." dismal-model-menu)
;; ("1Reload        Reload & compile dismal.  Used mostly/only by Frank"
;;                  dismal-compile-n-load-dismal)
;; ("2load.el       Reload dismal.el.  Used mostly/only by Frank"
;;                   (progn (if (file-exists-p "dismal.elc")
;;                              (delete-file "dismal.elc"))
;;                    (load "dismal.el")))
;; ("3load.elc      Reload dismal.elc.  Used mostly/only by Frank"
;;                    (load "dismal.elc"))
))


;;;
;;;	II.	File menu and children
;;;

(def-menu 'dismal-file-menu
  "Dis I/O"
  "" ;help prompt
 '(("New.         Open a new file." dismal-find-file)
   ("Open.        Open a dismal file." find-file)
   ("Save         Save a file." dismal-save-file)
   ("Write.       Write file to new name" dismal-write-file)
   ("Insert.      Insert a file starting at current cell." dismal-insert-file)
   ("2Prin.       Setup to print." dismal-print-setup)
   ("FPrin.       Print a copy to a file." dismal-make-report)
   ("PPrin.       Print a copy to a printer. (M-x print-buffer sorta works)."
                  dismal-print-report)
   ("RDump.       Dump a range to a tabbed file." dismal-dump-range)
   ("Unpage       Strip page breaks from a report." dismal-unpaginate)
   ("Quit         Quit dismal-mode on a file." kill-buffer)
))


;;;
;;;	III.	Edit menu and children
;;;

(def-menu 'dismal-edit-menu
  "Dis Edit"
  "" ;help prompt
 '(("Undo*        Undo the previous command." dismal-no-op)
   ("XKill        Kill (cut) a range." dismal-kill-range)
   ("2Copy.       Copy a range."  dismal-copy-range)
   ("Yank         Yank (paste) the range kill buffer." dismal-paste-range)
   ("Erase        Erase w/out saving a range." dismal-erase-range)
   ("Set/         Set a cell." dismal-set-cell-parameters-menu)
   ("Insert/      Insert new cells/columns/rows." dismal-insert-menu)
   ("Delete/      Delete cells/columns/rows." dismal-delete-menu)
   ("Modify/      Modify a cell." dismal-modify-cell-menu)
))

(def-menu 'dismal-modify-cell-menu
  "Dis modify"
  ""
 '((">          Edit a cell and set format to right justified." 
    dismal-read-cell-rightjust)
   ("<          Edit a cell and set format to left justified." 
    dismal-read-cell-leftjust)
   ("=          Edit a cell, set format to default (#'s right, strings left)." 
    dismal-read-cell-leftjust)
   ("|          Edit a cell, set format to centered." 
    dismal-read-cell-center)
   ("e          Edit a cell and don't change format."
    dismal-read-cell-plain)))


(def-menu 'dismal-insert-menu 
  "Dis insertable items"
  "" ;help prompt
 '(("Row           Insert a row." dismal-insert-row)
   ("Column        Insert a column." dismal-insert-column)
   ("Marked-range  Insert cells based on marked range." dismal-insert-range)
   ("Z-box         Insert Z shaped box of cells based on marked range." 
                   dismal-insert-z-box)
))

(def-menu 'dismal-delete-menu
  "Dis deleteable items"
  "" ;help prompt
 '(("Row           Delete a row." dismal-delete-row)
   ("Column        Delete a column." dismal-delete-column)
   ("Marked-range  Delete cells based on marked range." dismal-delete-range)
))

(def-menu 'dismal-set-cell-parameters-menu
  "Dis cell options"
  "" ;help prompt
 '(("Center.      Set a cell center justified." dismal-read-center)
   ("General.     Set a cell justified same as its column." dismal-read-cell)
   ("Left.        Set a cell left justified." dismal-read-leftjust)
   ("Right.       Set a cell right justified." dismal-read-rightjust)
))


;;;
;;;	IV.	Move menu and children
;;;

(def-menu 'dismal-move-menu
  "Dis Move"
  "" ;help prompt
 '(("Column/     Move to a column." dismal-move-col-menu)
   ("Row.        Move to a row." dismal-move-row-column)
   ("<--         Scroll buffer left (moving window to right)." scroll-left)
   ("-->         Scroll buffer right (moving window to left)." scroll-right)
   ("Begin       First cell." dismal-beginning-of-buffer)
   ("End         Last cell." dismal-end-of-buffer)
   ("Jump.       Jump to prompted for cell." dismal-jump)
))

(def-menu 'dismal-move-col-menu
  "Dis Column movements"
  "Move columns: " ;help prompt
 '(("1st         First column." dismal-first-column)
   ("Back        Back a column." dismal-backward-column)
   ("Last        Last column."   dismal-last-column)
   ("Forward     Forward a column." dismal-forward-column)
))

(def-menu 'dismal-move-row-menu
  "Dis row movements"
  "Move rows" ;help prompt
 '(("1st         First row." dismal-first-row)
   ("Back        Back a row." dismal-backward-row)
   ("Last        Last row."   dismal-last-row)
   ("Forward     Forward a row." dismal-forward-row)
))


;;;
;;;	V.	Commands menu
;;;

(def-menu 'dismal-commands-menu
  "DisCom"
  "" ;help prompt
 '(("Align        Align metacolumns based on mark and point." dismal-align-metacolumns)
   ("Cp2dis       Copy column of space delimited numbers or words from another 
                  buffer to dismal." copy-to-dismal)
   ;; ("Depend-clean Clean up the dependencies." dismal-fix-dependencies)
   ("Redraw       Redraw the display from scratch." dismal-redraw)
   ("Expand       Expand the columns of width 0 in range to be of width 1."
                  dismal-expand-cols-in-range)
   ("FillRng      Fill the range with incrementing numbers." dismal-fill-range)
   ("ShowFns      List the available functions for cells." dismal-show-functions)
   ("Update       Update the matrix." dismal-update-matrix)
   ("Hupdate      Hard update, recalculate the whole matrix (quite slow)."
                  dismal-recalculate-matrix)
   ("QueryR       Query-replace for Dismal." dismal-query-replace)
   ("DeBlank      Delete all blank rows in given range." 
                  dismal-delete-blank-rows)
))


;;;
;;;	VI.	Format menus
;;;

(def-menu 'dismal-format-menu
  "Dis Format"
  "Formating" ;help prompt
 '(("Number.     Set format for numbers." dismal-set-column-decimal)
   ("Align.      Set alignment for range or column." 
                   dismal-set-alignment)
   ("Width.      Set width for column." dismal-read-column-format)
   ("Fonts/      Set the font for the sheet." dismal-set-font)
   ("UpdateR     Redraw the ruler." dismal-update-ruler)
))

(defun dismal-set-font ()
   (interactive)
   (x-set-font (run-menu 'dismal-font-menu "Small")))

;; get fonts from xlsfonts

(def-menu 'dismal-font-menu
  "Dis Fonts"
  "Font use for the whole sheet (actually all buffers):" ;help prompt
 '( ;;("VTiny       A very tiny font, 4x13." "4x8")
   ("Tiny        A tiny font, 5x8." "5x8")
   ("4Small      A very short font, 6x9." "6x9")
   ("3Small      An even slightly shorter font, 6x10." "6x10")
   ("2Small      A slightly shorter font, 6x12." "6x12")
   ("Small       Probably your normal font, 6x13." "6x13")
   ("Medium      A slightly big font, 8x13." "8x13")
   ("Big         A big font, 9x15."          "9x15")
   ("Huge        A really big font, 10x20." "10x20")
))


;;;
;;;	VII.	Options menu
;;;

(def-menu 'dismal-options-menu
  "Dis Options"
  "" ;help prompt
 '(("Set            Set dismal global user variables." dismal-set-variables-menu)
   ("A-Redraw       Redraw the whole display." dismal-redraw)
   ("C-redraw       Redraw the current column."
                    (dismal-save-excursion
                      (dismal-redraw-column dismal-current-col)))
   ("R-redraw       Redraw the current row."
                    dismal-hard-redraw-row)
   ("Uler-redraw    Redraw the ruler." dismal-update-ruler)
   ("Z-range        Redraw the current range." dismal-redraw-range)
))



;;;
;;;	VIII.	Set variables menu
;;;

(def-menu 'dismal-set-variables-menu
  "Dis Set variables"
  "" ;help prompt
 '(("RulerRow      Set the ruler's row." dismal-set-ruler-rows)
   ("2Ruler        Toggle showing the ruler." dismal-set-ruler)
   ("Auto-update   Toggle auto-updating." dismal-toggle-auto-update)
   ("Middle-col    Set the last col, that is grouped with the LHS when aligning."
                   dismal-set-metacolumn)
))


;;;
;;;	IX.	Model menus 
;;;

(def-menu 'dismal-model-menu
   "DisMod"
   ""
 '(("Codes/       Setup, use, and save operator codes." dis-codes-menu)
   ("Stats/       Various stats that can be created." dis-stats-menu)
   ("Utils/       Other things to do in Soar/PA mode." dis-code-utils-menu)))

(def-menu 'dis-codes-menu
  "DisCodes"
  "" ;help prompt
 '(("Save         Write dismal operator codes out to a file."  dis-save-op-codes)
   ("Code.        Code a segment with an operator name."  dis-op-code-segment)
   ("Load.        Load operator codes into dismal." dis-load-op-codes)
   ("Init         Initialize operator codes, taking them from a Soar process if possible."
     dis-initialize-operator-codes)
))

(def-menu 'dis-code-utils-menu
  "DisCode utils"
  "" ;help prompt
 '(("AutoAlign.   Auto align the two meta columns." dis-auto-align-model)
   ("2AutoAlign.  Align columns as set by the previous run of dis-auto-align-model."
                  dis-align-columns)
))

(def-menu 'dis-stats-menu
  "DisCode stats"
  "" ;help prompt
 '(("Stats*       Print out stats (not defined yet)." dismal-no-op)
   ("Count*       Count codes in range (not defined yet)." dismal-no-op)
))


;;;
;;;	X.	Misc. menus used outside of main menu
;;;


(def-menu 'dismal-row-or-column-menu
  "Move Rows (vertically) or Columns (horizontally)" ;main prompt
  "This menu allows you to select how to insert new cell(s)."
 '(("Rows        Move the cells vertically adding or removing rows." 'rows)
   ("Columns     Move the cells horizontally adding or removing columns." 
                 'columns)
   ("H           Move the cells horizontally adding or removing columns." 
                 'columns)
   ("V           Move the cells vertically adding or removing rows." 'rows)
))

(def-menu 'dismal-range-or-col-menu
  "Use selected range (r) or column (c)" 
  "This menu allows you to select to operate on a range or the current column."
 '(("Range         Apply command to the current range." 'range)
   ("Column        Apply command to the current column." 'column)))

(def-menu 'dismal-alignment-style-menu
  "Select alignment style"
  "This menu allows you to select the alignment style."
 '(("Left         Flush left." 'left)
   ("Center       Centered." 'center)
   ("Default      Text flush left & numbers flush right." 
                    'default)
   ("Right        Flush right." 'right))
)

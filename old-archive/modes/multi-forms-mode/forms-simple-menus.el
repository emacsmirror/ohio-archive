;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : forms-simple-menus.el
;;;; Author          : Frank Ritter
;;;; Created On      : Tue Sep 10 12:01:48 1991
;;;; Last Modified By: Frank Ritter
;;;; Last Modified On: Fri Jan  3 17:56:00 1992
;;;; Update Count    : 39
;;;; 
;;;; PURPOSE
;;;; 	Menus for forms-mode
;;;; TABLE OF CONTENTS
;;;; 	|>Contents of this module<|
;;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'simple-menu)
(provide 'forms-simple-menus)

(defun forms-run-menu ()
  "Provide a menu of commands for forms-mode."
  (interactive)
  (run-menu 'forms-menu))

(def-menu  'forms-menu
  "Forms-mode commands" ;main prompt
  "The menu key allows you to select various forms command options"
   ;123456789012345
 '(
   ("File          File manipulation commands." forms-file-command-menu)
   ("Movement      Movement commands." forms-movement-command-menu)
   ("Commands      Commands for forms-mode, and changing file parameters."
                     forms-commands-menu)
   ("Analyses      Analysis programs internal to the mode."
                     forms-analysis-menu)
   ("Reload        Reload forms.  Used mostly/only by Frank"
                     (progn (byte-compile-file "forms.el")
                        (load "forms.elc")))
   ("EReload       Reload forms.el.  Used mostly/only by Frank"
                    (progn (if (file-exists-p "forms.elc")
                               (delete-file "forms.elc"))
                     (load "forms.el")))
))

(def-menu  'forms-file-command-menu
  "" ;main prompt
  "The menu key allows you to select various forms file commands" ;help prompt
 '(
   ("Open            Open new or old forms-mode file." forms-find-file)
   ("Save            Save a forms-mode file."   forms-save-buffer)
   ("Write           Save a forms-mode file to another file name."
        forms-write-buffer)
   ("View            Put file in forms view only submode." forms-view-mode)
   ("Edit            Put file in forms edit submode." forms-edit-mode)
   ("RRevert         Revert current RECORD to original form."
                      forms-revert-record)
   ("BRevert         Revert current BUFFER to original form."
                      forms-revert-buffer)
   ("Comments        Edit the comment header." forms-edit-comments-header)
   ("Header          Edit the command header." forms-edit-commands-header)
   ("AutoRep         View the autoreport." forms-view-auto-report-header)
   ("Quit            Quit forms-mode and get rid of buffer."
                      forms-exit)
))

(def-menu 'forms-commands-menu
  "" ;main prompt
  "The menu key allows you to select various command options" ;help prompt
 '(
   ("Help            Short quick help." forms-quick-help)
   ("Full-help       Full mode help." describe-mode)
   ("Renumber        Renumber number field in segments.")
   ("Hedit           Edit the form header bits.")
   ("Front           Edit the leading comments.")
   ("ToggleV         Toggle making invisible segments visible (or not).")
   ("Combine         Combine two segments together.")
   ("2Dupl           Split a segment in two through duplicating it."
                     forms-duplicate-record)
   ("Del             Delete a record" forms-delete-record)
   ("Insert          Insert a record" forms-insert-record)
))

(def-menu 'forms-movement-command-menu
  "" ;main prompt
  "This menu allows you to select various forms movement commands" ;help prompt
 '(
   ("Up        Scroll up."   forms-scroll-up)
   ("Down      Scroll down."   forms-scroll-down)
   ("<         Beginning of records."   forms-first-record)
   (">         End of records."   forms-last-record)
   ("Tab       Next field." forms-next-field)
   ("Next      Next record." forms-next-record)
   ("Back      Previous record." forms-prev-record)
   ("Jump      Jump to a record." forms-jump-record)
   ("Search    Search. " forms-search)
))

(def-menu 'forms-analysis-menu
  ""
  "Analysis programs internal to forms-mode."
 '(
   ("Tally      Tally the types of fields in a queried for field." 
                forms-tally-fields)
   ("Picture    Save a picture of the forms window." forms-picture-menu)
))
  
(def-menu 'forms-picture-menu
  "" ;main prompt
  "The menu key allows you to select various command options" ;help prompt
 '(
   ("Buffer            Picture of the entire buffer." forms-make-report)
   ("Window            Picture what's in the current window.")
))


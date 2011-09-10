;;; modify.el - simple code for maintaining modification tags in files like RCS

;; Author: Lawrence R. Dodd <dodd@roebling.poly.edu>
;; Maintainer: Lawrence R. Dodd <dodd@roebling.poly.edu>
;; Created: 28 May 1992
;; Version: 1.60
;; Keywords: extensions

;; Copyright (C) 1993 Lawrence R. Dodd
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

(defconst modify-version (substring "!Revision: 1.60 !" 11 -2) 
  "revision number of modify.el - maintains a modification tag in files.  type
M-x modify-submit-report to send a bug report.  available via anonymous ftp
in:

   /roebling.poly.edu:/pub/modify.el
   /archive.cis.ohio-state.edu:/pub/gnu/emacs/elisp-archive/misc/modify.el.Z")

;; LCD Archive Entry:
;; modify|Lawrence R. Dodd|dodd@roebling.poly.edu|
;; Simple code for maintaining modification tags in files like RCS.|
;; 16-Jun-1993|1.60|~/misc/modify.el.Z|

;;; Based on code posted by merlyn@iWarp.intel.com (Randal L. Schwartz)

;;; !Modified: Wed Jun 16 14:26:13 EDT 1993 by dodd !
;;; !Id: modify.el,v 1.60 1993/06/16 18:27:05 dodd Exp !
;;; !Revision: 1.60 ! 

;;;   Lawrence R. Dodd <dodd@roebling.poly.edu>
;;;   Department of Chemical Engineering
;;;   Polymer Research Institute
;;;   Polytechnic University 
;;;   Brooklyn, New York, 11201 USA

;;; BUG REPORTS: 
;;; 
;;; just type M-x modify-submit-report to generate a bug report template
;;; (requires Barry Warsaw's reporter.el available at roebling.poly.edu)


;;; INSTALLATION AND USAGE: 
;;;  
;;; Save as `modify.el' in a directory where emacs can find it. Stick 
;;; 
;;;                   (require 'modify) 
;;;                   
;;; in your ~/.emacs or the site initialization file.  Next set the variable
;;; `modify-update' to a value of t, nil, or 'ask for each major mode,
;;; including the default-major-mode, in their respective mode-hooks -- or
;;; make it a default by editing the value in this file or sticking _one_ of
;;; the following in your ~/.emacs file.
;;;  
;;;      (setq-default modify-update t)    ; update, never query
;;;      (setq-default modify-update nil)  ; never update
;;;      (setq-default modify-update 'ask) ; query (1st time only)
;;;  
;;; This variable has a unique value for each buffer it is visiting.  Do the
;;; same for `modify-insert-tag' and `modify-insert-offset.' 
;;; 
;;; The complete list of user-defined variables is:
;;;  
;;;       modify-tag
;;;       modify-tag-regexp
;;;       modify-maximum-point
;;;       modify-date-switches
;;;       modify-date-switches-repeat
;;;  
;;; the user-defined buffer-local variables are
;;;  
;;;       modify-update
;;;       modify-insert-tag
;;;       modify-insert-offset
;;; 
;;; to find out more about these variables, load this file, put your cursor at 
;;; the end of any of the above lines, and hit C-h v [RET].
;;;  
;;; the interactive function is
;;; 
;;;       (modify-insert-tag)
;;;  
;;; to find out more about this function, load this file, put your cursor
;;; inside the `()' of the above line, and hit C-h f [RET].

;;; RCS CAVEAT: 
;;; 
;;; Obviously, modifications to a file that has been checked out using RCS
;;; will cause the modified tags to be updated.  Those that use of Sebastian
;;; Kremer's rcs.el and GNU diff may wish to define the value of the variable
;;; `rcsdiff-switches' to ignore the differences due to the modified tag.
;;; This can be done as follows
;;;  
;;; (setq rcsdiff-switches (list (concat "-I\\$" modify-tag-regexp ":?.*\\$")))
;;;  
;;; note:
;;; 
;;;    diff -I REGEXP ignores changes that match REGEXP
;;; 
;;;    Thus, the regular expression `"\\$" modify-tag-regexp ":?.*\\$"'
;;;    matches occurrences of the modify.el tag (expanded or unexpanded) and
;;;    suppresses these differences.  For context or unified differences,
;;;    place a `c' or a `u' switch in front of the `I' switch.  The `list' in
;;;    the above definition allows the command line to be edited; this may be
;;;    removed if the user desires.

;;; DESCRIPTION: 
;;;  
;;; Once installed, this code will update a time stamp string, like the one in
;;; the header of this file, in a manner similar to the way RCS updates its
;;; tags.
;;;  
;;; If you stick the string "$\Modified$" anywhere near the top of a file,
;;; this code will replace it with "$\Modified: time_saved by user_name $"
;;; when the filed is first saved.  [Note `\Modified' is really the contents
;;; of the variable `modify-tag' which defaults to the string "Modified."]  It
;;; will then update the time and user name each subsequent saving of the
;;; file.  It will update all occurrences up to `modify-maximum-point' beyond
;;; the last update or the beginning of the buffer.
;;; 
;;; If `modify-date-switches' is non-nil then the string is passed as
;;; switches to the UNIX `date' command.  Thus, for most date commands, if set
;;; to "-u" modify.el will use Universal Coordinated Time or GMT.  Just like
;;; RCS!  You may also set it to the null string "" in which case the result
;;; of the command `date' will be used (i.e., containing the local time zone).
;;; The variable `modify-date-switches-repeat' is used for all tags after the
;;; first one.
;;; 
;;; Finally, if the variables `modify-insert-tag' and `modify-update' are t,
;;; and the $\Modified$ tag does not exist in the header of the file, then
;;; this code will make the top line (plus the value of
;;; `modify-insert-offset') of the file $\Modified: current_time by user_name
;;; $ which, if necessary, is prefixed and postfixed by the correct comment
;;; characters for the current major mode.


;;; Code:

;;; user defined variables 
(defvar modify-tag "Modified"
  "*string representing label of tag to be updated.  The modified string will
be \"$modify-tag: time by user $\".  See also `modify-tag-regexp.'")

(defvar modify-tag-regexp nil
    "*regexp matching any label of tag to be replaced.  defaults to
the result of \(regexp-quote modify-tag\).")

(or modify-tag-regexp
    (setq modify-tag-regexp (regexp-quote modify-tag)))

;;; buffer locals 
(defvar modify-insert-tag nil
  "*t says to attempt inserting tag in buffer if not found.  buffer local.  if
this is t and `modify-update' is t then tag automatically inserted.  if
`modify-update' is non-nil, but not t, and this variable is non-nil then ask
the user once and only once whether to insert a modification tag at the top of
the file.  if this variable and/or `modify-update' is nil then insertion will
never be attempted.")
(make-variable-buffer-local 'modify-insert-tag)
(put 'modify-insert-tag 'permanent-local t) ; for v19 Emacs
(put 'modify-insert-tag 'preserved t)       ; for kill-fix.el

(defvar modify-update nil
  "*t says buffer should have its modification tag automatically updated.  nil
says buffer will not have its modification tag updated.  if non-nil but not t,
then ask user once and only once whether to update modify tag.  buffer
local.")
(make-variable-buffer-local 'modify-update)
(put 'modify-update 'permanent-local t) ; for v19 Emacs
(put 'modify-update 'preserved t)       ; for kill-fix.el

(defvar modify-insert-offset 0
  "*number of lines from top of buffer that modify tag should be inserted.
see also `modify-insert-tag' and `modify-insert-tag'. buffer local.

set to more than one in mode hooks for files that have a shell pathname such
as `#!/bin/sh' on the first line.  For example, define this to be some
positive integer in `csh-mode-hook' for csh-mode.el, in `perl-mode-hook' for
perl.el, and in the function `awk-mode' for awk-mode.el.")
(make-variable-buffer-local 'modify-insert-offset)
(put 'modify-insert-offset 'permanent-local t) ; for v19 Emacs
(put 'modify-insert-offset 'preserved t)       ; for kill-fix.el

;;; switches for Unix `date' command

(defvar modify-date-switches nil
  "*string of switches passed to local Unix date command for first tag found.
nil says use the emacs lisp function `current-time-string.'  See also
`modify-date-switches-repeat.'

For example, with a value of \"-u\" modify.el would use Universal Coordinated
Time \(i.e., Greenwhich Mean Time\) in its time stamp (just like RCS)

             ;;; $\\Modified: Sat Feb 27 17:10:12 GMT 1993 by dodd $

Please note `\\Modified' is really the contents of the variable `modify-tag'
which defaults to the string \"Modified\".  If you use GNU date then you may
set this or `modify-date-switches-repeat' to \"+%c\"")

(defvar modify-date-switches-repeat modify-date-switches

  "*string of switches passed to local Unix date command for second tag.
set to the value of `modify-date-switches' by default.  nil says use the emacs
lisp function.  See also the variable `modify-date-switches.'

This variables exists so that it can be set to something like \"+%c\" for use
with the GNU version of date (or just plain \"\" for all versions of date) to
show local time, with timezone, on any repeat substitutions.  This allows
scenarios such as this inside files

             ;;; $\\Modified: Sat Feb 27 17:10:12 GMT 1993 by dodd $
             ;;; $\\Modified: Sat Feb 27 12:10:12 EST 1993 by dodd $ 

showing the modified time in _both_ GMT and local time.  An unexpected feature
of this is that their difference tells you how far you are from Greenwich,
England!

Please note `\\Modified' is really the contents of the variable `modify-tag'
which defaults to the string \"Modified\".")

(defvar modify-maximum-point 2000
  "*Integer for maximum number of characters from buffer top to search for tag.
the default is 2000 characters.")


;;; the work horse 
(defun modify-tag-hook ()

  "Update modify tag while writing out a modified file.  Installed by
modify.el and intended to be used as one of the `write-file-hooks' values.

Does _not_ write out file.  The return value is nil unconditionally."
  
  ;; if the variable is nil then we will exit
  (if modify-update
         
      (let (limit originally_locked case-fold-search

                  ;; define regular expression search string
                  ;; will match both expanded or unexpanded tag
                  (search-string
                   (concat
                    ;; from leading $ to end of modify-tag-regexp
                    "\\(\\$" modify-tag-regexp "\\)"
                    ;; may or may not be expanded, i.e., `:' may not exist
                    "\\(:?\\)"
                    ;; everything up to final $
                    "[^\n\\$]*"
                    ;; the final $
                    "\\(\\$\\)"))

                  ;; define replacement string
                  ;; will create expanded tag
                  (replacement-string
                   (concat
                    ;; from leading $ to end of modify-tag-regexp plus `: '
                    "\\1: "
                    ;; date and user
                    (modify-current-time-string)
                    " by " (user-login-name)
                    ;; the final $
                    " \\3")))

        ;; IF WE CAN LOCATE IT...
        (if (and
             
             ;; can we locate the tag? - meat of package 
             ;; search is case-sensitive
             
             (save-excursion

               ;; set the limit of our search to be the user-defined variable 
               ;; or the entire buffer, whichever is smaller
               (setq limit (min modify-maximum-point (point-max)))

               (goto-char (point-min))

               ;; can we find the tag?
               (re-search-forward search-string limit t))
             
             ;; we found that tag, now if `modify-update' is not explicitly
             ;; equal to t, then we must ask
             (or (eq t modify-update)
                 ;; reset value of variable
                 (setq modify-update (y-or-n-p "Replace Modify Tag? ")))
             )
            
            ;; ...THEN REPLACE IT
            (save-excursion

              ;; make buffer writable if necessary
              (setq originally_locked (if buffer-read-only (toggle-read-only)))

              ;; make the replacement 
              (replace-match replacement-string t)

              ;; need to redefine `limit' in case the point is beyond old value
              (setq limit (min (+ (point) modify-maximum-point) (point-max))) 

              ;; now repeat until `limit' is reached

              ;; define local value of `modify-date-switches'
              (let* ((modify-date-switches modify-date-switches-repeat)
                     (replacement-string
                      (concat "\\1: " (modify-current-time-string) " by "
                              (user-login-name) " \\3")))
                
                ;; search, replace, and redefine `limit'
                (while (re-search-forward search-string limit t)
                  (replace-match replacement-string t)
                  (setq limit (min (+ (point) modify-maximum-point)
                                   (point-max)))))

              ;; if the buffer was originally read-only, then make it so again
              (if originally_locked (toggle-read-only)))

          ;; ...ELSE SHOULD WE INSERT IT?

          ;; Scenarios

          ;; if either `modify-update' or `modify-insert-tag' are set to nil
          ;; then insertion will _not_ be done.

          ;; if, and only if, both `modify-update' and `modify-insert-tag' are
          ;; set to t then insertion be done automatically.

          ;; if either `modify-update' or `modify-insert-tag' is set to 'ask
          ;; (and the other is t) then user will be queried and both will be
          ;; set to the reply (i.e., to either t or nil).

          (if (and

               ;; 1. check for a non-nil value -- could be t or 'ask
               modify-update

               ;; 2. check for a non-nil value -- could be t or 'ask
               (if (listp modify-insert-tag)
                   (and (memq major-mode modify-insert-tag)
                        (setq modify-insert-tag t))
                 modify-insert-tag)

               ;; 3. check if both have a t value
               (or (and (eq t modify-insert-tag)
                        (eq t modify-update))

                   ;; one (or both) is 'ask 
                   ;; query user and set value of both variables to result
                   (setq modify-update
                         (setq modify-insert-tag
                               (y-or-n-p "Insert Modify Tag? ")))))

              ;; insert tag
              (modify-insert-tag nil)))))

  ;; force nil return value - we do _not_ write out the file
  nil)


(defun modify-current-time-string ()

  "Return current time as a human-readable string.  Reduces to
`current-time-string' if `modify-date-switches' is nil, otherwise passes that
string as switches to Unix date command."

  (if modify-date-switches

      ;; switches are non-nil use Unix date
      (let (string temp-buffer)
        
        (save-excursion
          
          ;; create a clean temporary buffer 
          (setq temp-buffer (get-buffer-create "*modify current time output*"))
          (set-buffer temp-buffer)
          (erase-buffer)
          
          ;; call the date with switches and stick the output in buffer
          ;; allow for the special case of a null string since this confuses 
          ;; most versions of `date'

          (if (string= modify-date-switches "")
              (call-process "date" nil t nil)
            (call-process "date" nil t nil modify-date-switches))
          
          ;; define string as everything in the buffer -- except the newline 
          (setq string (buffer-substring 1 (1- (point-max))))
          
          ;; remove temporary buffer
          (kill-buffer temp-buffer))
        
        ;; return string
        string)
    
    ;; use elisp routine to return string
    (current-time-string)))


(defun modify-insert-tag (arg)

  "Insert modify tag at top of buffer plus `modify-insert-offset' lines down.
called by `modify-tag-hook' but also may be called interactively.  If argument
ARG is non-nil then place modify tag at point in current buffer, ignoring the
value of `modify-insert-offset.'

Regardless of the value of `modify-insert-offset,' this routine will never
place a modify tag before a line that begins with `#!' this is done in order
to avoid screwing up shell scripts."

  (interactive "P")

  ;; make readable if necessary
  (let ((originally_locked (if buffer-read-only (toggle-read-only))))
    
    (save-excursion

      ;; no argument? go to top of buffer
      (if (not arg) (progn (goto-char (point-min))
                           ;; skip forward 0 or more lines
                           (forward-line modify-insert-offset)))

      ;; are we still looking at a `#!' or `%!'?  if so, move forward
      (if (looking-at "^[#%]?!") (forward-line 1))

      ;; insert modify tag
      (insert-string
       (concat comment-start "$" modify-tag ": "
               (modify-current-time-string) " by " (user-login-name)
               " $" comment-end "\n")))
    
    ;; if the buffer was originally read-only, then make it so again
    (if originally_locked (toggle-read-only))))

;;; prepend to write-file-hooks - checking first whether or not it is already
;;; there

(if (fboundp 'add-hook)

    ;; use v19's add-hook
    (add-hook 'write-file-hooks 'modify-tag-hook)

  ;; Contributed by Ken Laprade <laprade@trantor.harris-atd.com>
  ;; Really should use some sort of add-hook - 16 Feb 93 - KCL
  (or (and (listp write-file-hooks) (not (eq (car write-file-hooks) 'lambda)))
      (setq write-file-hooks (list write-file-hooks)))

  (or (memq 'modify-tag-hook write-file-hooks)
      (setq write-file-hooks (append '(modify-tag-hook) write-file-hooks))))


;;;; BUG REPORTS

;;; this section is provided for reports.
;;; using Barry A. Warsaw's reporter.el

(defconst modify-help-address "dodd@roebling.poly.edu"
  "Address(es) accepting submission of reports on modify.el.")

(defconst modify-maintainer "Larry"
  "First name(s) of people accepting submission of reports on modify.el.")

(defconst modify-file "modify.el"
  "Name of file containing emacs lisp code.")

(defconst modify-variable-list
  (list 'modify-update
        'modify-tag
        'modify-tag-regexp
        'modify-maximum-point
        'modify-date-switches
        'modify-date-switches-repeat)
  "list of variables to be appended to reports sent by `modify-submit-report.'")

(defconst modify-version-date (substring "!Date: 1993/06/16 18:27:05 !" 7 -2)
  "revision date of modify.el")

(defun modify-submit-report ()

  "submit via reporter.el a bug report on program.  send report on
`modify-file' version `modify-version,' to `modify-maintainer' at address
`modify-help-address' listing variables `modify-variable-list' in the
message."

  (interactive)

  (require 'reporter)

  (reporter-submit-bug-report
   modify-help-address                     ; address
   (concat modify-file " "                 ; pkgname
           modify-version " of "
           modify-version-date)            
   modify-variable-list                    ; varlist
   nil nil                                 ; pre-hooks and post-hooks
   (concat "Yo, " modify-maintainer "!"))) ; salutation


;;; provide package
(provide 'modify)

;;; modify.el ends here

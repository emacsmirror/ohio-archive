;;; mode-line.el --- code for including abbreviated file paths in mode line

;; Author: Lawrence R. Dodd <dodd@roebling.poly.edu>
;; Maintainer: Lawrence R. Dodd <dodd@roebling.poly.edu>
;; Created: prettymodeln.el on 13 Sep 1987, mode-line.el on 21 Jun 1992
;; Version: 2.94
;; Keywords: extensions

;; Copyright (C) 1992-94 Lawrence R. Dodd
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; mode-line|Lawrence R. Dodd|dodd@roebling.poly.edu|
;; code for including abbreviated file paths in mode line|
;; 15-Jul-1994|2.94|~/misc/mode-line.el.Z|

;;; AVAILABLE: 
;;; 
;;; via anonymous ftp in
;;;/roebling.poly.edu:/pub/mode-line.el
;;; to archive.cis.ohio-state.edu in 
;;; /archive.cis.ohio-state.edu:/pub/gnu/emacs/elisp-archive/misc/mode-line.el.Z

;;; BUG REPORTS: 
;;; 
;;; just type M-x mode-line-submit-report to generate a bug report template
;;; (requires Barry Warsaw's reporter.el available at roebling.poly.edu)

;;; MAINTAINER OF mode-line.el:
;;;  
;;; Lawrence R. Dodd       <dodd@roebling.poly.edu>
;;; Chemical Engineering
;;; Polytechnic University 
;;; Brooklyn, New York

;;; CONTRIBUTORS TO mode-line.el:
;;;
;;; Lawrence R. Dodd
;;; dodd@roebling.poly.edu
;;;
;;; Robert McLay 
;;; mclay@cfdlab.ae.utexas.edu
;;; (for much beta-testing and many good suggestions)
;;;
;;; Crys Rides (a.k.a., James C. Ghering) 
;;; crys@cave.tcp.com
;;; (for suggesting and testing of view-mode support)
;;;
;;; Vladimir G. Ivanovic 
;;; vladimir@Eng.Sun.COM 
;;; (for beta-testing with Lucid (v19) emacs)
;;; 
;;; Ed Rapoport
;;; rapoport@camax.com 
;;; (for suggesting Tree Dired support)

;;; HISTORY:
;;; 
;;; Derived from prettymodeln.el.  That file was checked in as version 2.1 of
;;; mode-line.el.  This is a cleaned, debugged, and more robust version of
;;; that original code containing more features and documentation. I would
;;; have named this prettymodeln++.el but that is too many letters...and
;;; besides I hack Fortran.
;;;
;;; AUTHOR OF prettymodeln.el: 
;;; 
;;; Andy Gaynor (a.k.a., Silver)
;;; gaynor@paul.rutgers.edu ...!rutgers!paul.rutgers.edu!gaynor
;;; 
;;;                               _   /|  Splthlt...
;;;                    Ahckthph!  \`o_@'
;;;                                 (_)
;;;                                  U   Ptooey!
;;;
;;; Created: 13 Sep 87 18:34:59 GMT

;;; POSSIBLE REQUIREMENT:
;;;
;;; kill-fix.el - (for v18 of Emacs only)
;;;
;;;   available via anonymous ftp to archive.cis.ohio-state.edu [128.146.8.52]
;;;   in /pub/gnu/emacs/elisp-archive/as-is/kill-fix.el.Z.
;;;
;;;   There is one small bug in mode-line.el that occurs when the major mode
;;;   of a buffer is changed.  Changing the major mode incorrectly resets the
;;;   buffer identification used in the mode line to the plain buffer display.
;;;   This bug is corrected easily and transparently in v19 of GNU Emacs (or
;;;   in Lucid Emacs). However, if v18 of Emacs is being used, then
;;;   mode-line.el will try to use Joe Wells' kill-fix.el.  However, if
;;;   the file isn't loaded nothing tragic will happen. The bug will simply 
;;;   not be fixed.

;;; INSTALLATION/USAGE:
;;;
;;;   o  save as mode-line.el in the load-path of GNU emacs 
;;;   o  optional: get kill-fix.el if you are using v18 of Emacs (see above)
;;;   o  stick this in your ~/.emacs:
;;;
;;;                (require 'mode-line) 
;;;   
;;;   o  use C-c C-t to scroll through different mode lines manually
;;;      (with an argument it will scroll through them automatically)
;;;   o  user may wish to change value of `mode-line-abbrev-dired'

;;; ADVANCED USAGE:
;;;
;;;   o  same as above but also stick something _like_ the following inside 
;;;      your ~/.emacs:
;;; 
;;;   (setq mode-line-abbreviation-alist
;;;         (list 
;;;          (cons  (concat "^" (expand-file-name "~") 
;;;                         "/" "special/")  "special:")
;;;          (cons  (concat "^" (expand-file-name "~") "/")  "~/")
;;;          '("^/dodd@roebling.poly.edu:/home/dodd/" . "Roebling:")
;;;          '("^/joe@\\([a-zA-Z0-9.]*\\).\\(edu\\|gov\\):/home/joe/" . "\\1:")
;;;          '("^.*/\\([^/]*/\\)" . "\\1")))  
;;; 
;;; The explanation of above is as follows. If I am editing a file called
;;; `filename' this list of associations will be attempted in this order: if
;;; the full path to `filename' is
;;; 
;;; (1) `/myhomedirectory/special/filename' display as `special:filename' 
;;;      (this is useful for much used sub-sub-directories)
;;; (2) `/myhomedirectory/filename' display as `~/filename' 
;;;      (this eliminates those long paths to your home directory)
;;; (3) `/user@machine.edu:/anything/filename' display as `Machine:filename' 
;;;     (this is _extremely_ useful with ange-ftp)
;;; (4) `/user@regexp.edu:/anything/filename' display as `regexp:filename' 
;;;     (this is the same as above but attempts to use a regular expression)
;;; (5) `/snafu/barfoo/filename' display as `barfoo:filename' 
;;;     (this shows just the current directory and is done for any path that
;;;     does not match one of the above)

;;; SEE ALSO: 
;;; 
;;; User-defined variables are:
;;; 
;;;        mode-line-abbreviation-alist
;;;        mode-line-abbrev-dired
;;;  
;;; to find out more about these variables, load this file, put your cursor at 
;;; the end of any of the above lines, and hit C-h v [RET].  See also the 
;;; function `(mode-line-toggle-display)'


;;; MOTIVATION:
;;; 
;;; Buffer names in the mode line are not very informative. When files with
;;; the same name are being visited in different directories the mode line
;;; shows names like "Makefile," "Makefile<2>," "Makefile<3>," and so on.  The
;;; zeroth order correction is to use the file name and directory in the mode
;;; line.  However, long file names with full directory paths (for example
;;; /u2/luser/foobar/bletch/src/0.1/foobar/Makefile) in the mode line are a
;;; pain in the ass.  They suck up the whole mode line, and are a strain on
;;; the eyes to differentiate. We would like to display things like
;;; "foobar/Makefile," "barfoo/Makefile," "conserve/Makefile," and so on in
;;; the mode line.
;;;
;;; You will find here a mode line formatting scheme that is fairly nice.  It
;;; displays the buffer name if the buffer is not associated with a file.
;;; Otherwise, it displays the file name, but only after abbreviating it as
;;; per a list of abbreviations that you provide.

;;; LOGIC: 
;;; 
;;; Set up the mode line by making mode-line-buffer-identification local to
;;; every buffer.  Various hooks will abbreviate the buffer-file-name to
;;; something a little easier to read.
;;; 
;;;   `buffer-file-name' == the original long file name   
;;;   `mode-line-abbreviation-alist' == list of abbreviations
;;;   `mode-line-abbreviate-buffer-identification' == what hooks call      
;;;   `mode-line-replace-regexp-in-alist' == means of abbreviation

;;; TO DO:
;;; 
;;; Use v19's function `abbreviate-file-name' and the variable
;;; `directory-abbrev-alist' in place of `mode-line-abbreviation-alist' and
;;; `mode-line-abbreviate-buffer-identification.'  I know this can be done...I
;;; am just too lazy to do it.


;;; KNOWN BUGS WITH SUGGESTED PATCHES:
;;; 
;;;   1. Sebastian Kremer's Tree Dired (v 6.0, available via anonymous ftp
;;;   from ftp.uni-koeln.de[134.95.80.1]:/pub/gnu/emacs/diredall.tar.Z) is an
;;;   improvement over the distribution dired for reasons too numerous to
;;;   mention. One major improvement is that Kremer's Dired correctly renames
;;;   any buffers visiting a file that is renamed using dired-mode.
;;; 
;;;   Unfortunately, it uses `set-visited-file-name' in the function
;;;   `dired-rename-file', which does not use `write-hooks'. The result being
;;;   that while the buffer is renamed, the mode line is not updated properly
;;;   after a dired-do-move (key r).  The patch is to force a call to
;;;   `mode-line-abbreviate-buffer-identification' after the call to
;;;   `set-visited-file-name' in the function `dired-rename-file':
;;;
;;;      (defun dired-rename-file (from to ok-flag)
;;;                                ...
;;;                        [material not shown]
;;;                                ...
;;;              (let ((modflag (buffer-modified-p)))
;;;                (set-visited-file-name to)   ; kills write-file-hooks
;;;     +          ;; for mode-line.el
;;;     +          (and (memq 'mode-line-abbreviate-buffer-identification
;;;     +                     write-file-hooks)
;;;     +                 (mode-line-abbreviate-buffer-identification)) 
;;;                (set-buffer-modified-p modflag))))
;;;                                ...
;;;                        [material not shown]
;;;                                ... 
;;; 
;;;   this patch is also available at /roebling.poly.edu in 
;;;   /pub/mode-line-dired-6.0.patch
;;;  
;;;   2. If you are using Dired from GNU Emacs 19, then this patch is needed 
;;;   to `dired-rename-file' of the file dired-aux.el (very similar to above):
;;;  
;;;      (defun dired-rename-file (from to ok-flag)
;;;                                ...
;;;                        [material not shown]
;;;                                ... 
;;;  
;;;               (let ((modflag (buffer-modified-p)))
;;;                 (set-visited-file-name to)
;;;     +           ;; for mode-line.el
;;;     +           (and (memq 'mode-line-abbreviate-buffer-identification
;;;     +                      write-file-hooks)
;;;     +                (mode-line-abbreviate-buffer-identification))
;;;                 (set-buffer-modified-p modflag))))
;;;                                ...
;;;                        [material not shown]
;;;                                ... 
;;;  
;;;   3. Dave Gillespie's live-find-file.el does not invoke a major mode so
;;;   the mode line is not set properly. This patch will fix it. 
;;;   A call to `mode-line-abbreviate-buffer-identification' is needed 
;;;  
;;;      (defun live-find-file (filename)
;;;                                ...
;;;                        [material not shown]
;;;                                ... 
;;;          (set-buffer-modified-p nil)
;;;          (setq buffer-read-only t)
;;;      +   ;; for mode-line.el
;;;      +   (and (memq 'mode-line-abbreviate-buffer-identification
;;;      +             write-file-hooks)
;;;      +       (mode-line-abbreviate-buffer-identification)) 
;;;          (goto-char (point-max))
;;;          (setq default-directory (file-name-directory filename))
;;;                                ...
;;;                        [material not shown]
;;;                                ... 
;;;  
;;;   this patch is also available at /roebling.poly.edu in 
;;;   /pub/mode-line-live.patch


;;; First, we need to load the default `view' package (be it view.el or
;;; new-view.el) so that `view-hook' will be defined when we append to it
;;; below.

(require 'view)

;;; GENERAL DISPLAY STUFF:

;;; This makes the mode line display the day, date, time of day, and average
;;; number of processes. The increment for time update is 30 seconds, also
;;; `Mail' appears if there is any unread mail.  Users may wish to comment
;;; this stuff out.

(display-time)
(setq display-time-interval 30)
(setq display-time-day-and-date t)

;;; Customize mode-line-format and its constituents. 

;;; Make sure you use mode-line-buffer-identification to identify the buffer
;;; in your mode-line-format.  This variable must be buffer-local (if it is
;;; not already).

;;; Note that mode-line-buffer-identification must be used to identify the
;;; buffer.

(make-variable-buffer-local 'mode-line-buffer-identification)
(put 'mode-line-buffer-identification 'permanent-local t) ; v19
(put 'mode-line-buffer-identification 'preserved t)       ; kill-fix
(setq-default mode-line-buffer-identification '("%b"))

;; create a new buffer-local variable to keep track of the current state of 
;; the mode line for use by mode-line-toggle-display.

(make-variable-buffer-local 'mode-line-state)
(put 'mode-line-state 'permanent-local t) ; v19
(put 'mode-line-state 'preserved t)       ; kill-fix

(defvar mode-line-state "buffer-name"
  "A buffer-local variable to keep track of the current state of the mode line
for use by mode-line-toggle-display.")
(put 'mode-line-state 'permanent-local t) ; v19
(put 'mode-line-state 'preserved t)       ; kill-fix
(setq-default mode-line-state "buffer-name")

;;; now the define the organization of the mode-line-format

(defvar line-number-mode nil) ; not defined in v18...

(setq-default mode-line-format
  (list
    (purecopy "--")
    'mode-line-modified
    (purecopy " ")
    'mode-line-buffer-identification
    (purecopy " %[(")
    (purecopy '(-13 . mode-name)) ; Truncate to 13 chars.  Can get too long.
    'mode-line-process
    'minor-mode-alist
    (purecopy "%n")
    (purecopy " ")
    (purecopy '(line-number-mode "L%l ")) ; line numbers!
    (purecopy '(-3 . "%p")) ; make string at most 3 chars: `Top', `Bot', or `nn%' - LRD
    (purecopy ")%] ")
    'global-mode-string
    (purecopy " %-")))

;;; A big thankyou to Robert McLay (mclay@cfdlab.ae.utexas.edu) for help with
;;; the following - LRD.

;;; Form home directory with a leading `^' and trailing `/' so if your home
;;; directory is /home/machine/user-name then home-dir is
;;; `^/home/machine/user-name/' (without the quotes) The leading `^' is need
;;; to match the leading end of the string.

;;; (originally was not a user option because it was missing the `*' - LRD)

(defvar mode-line-abbreviation-alist
      (list 
       (cons  (concat "^" (expand-file-name "~") "/")  "~/")
       ;; you probably want to stick special stuff here
       '("^~/.*/\\([^/]*/\\)" . "~/.../\\1")
       '("^/.*/\\([^/]*/\\)" . "\\1")
       )

  "*Alist of embedded filename patterns versus the corresponding desired
abbreviations. Each element is of the form (<regexp> . <to-string>).

The package mode-line.el goes down this alist looking for matches to regular
expression <regexp> in the full pathname of the file and replaces it with
<to-string>.  This is then repeated for all <regexp> in the list.  This fact
can be exploited when forming the regular expressions.  However, since the
searching and replacing is done top-down, special cases should be put at the
head of the list.  The user is strongly encouraged to modify this variable to
suit her or his own tastes.

Examples:

  Let's say that the user often plays with the files in the directory
  /u2/luser/foobar/bletch.  In this case the user may want to replace
  leading instances of this path with just `bletch' or `bletch:' To do
  this stick the association into the alist

     (\"^/u2/luser/foobar/bletch\" . \"bletch\")

  Another useful association (which requires the power of regular
  expressions) is to display only the last directory in the path if no
  other special case applies.  This is done with the following
  association

     (\"^/.*/\\\\([^/]*/\\\\)\" . \"\\\\1\")

  Finally, one can also abbreviate those long filenames that result
  when using ange-ftp

     (\"^/emily@roebling.poly.edu:/columbia/heights/emily/\" . \"Roebling:\")

Default:

     (cons  (concat \"^\" (expand-file-name \"~\") \"/\")  \"~/\")
     (\"^~/.*/\\\\([^/]*/\\\\)\" . \"~/...\\\\1\")
     (\"^/.*/\\\\([^/]*/\\\\)\" . \"\\\\1\")

  this will

      (1) remove the home directory path and replace it with `~/'

          example: /home/dodd/foobar.el --> ~/foobar.el

          dired buffer example: /home/dodd --> ~/

      (2) display only the last sub-directory in the abbreviated
          home path

          example: /home/dodd/lisp/foobar.el --> ~/lisp/foobar.el
                   /home/dodd/lisp/misc/foobar.el --> ~/.../misc/foobar.el

          dired buffer example: /home/dodd/Mail/John --> ~/.../John
                                /home/dodd/Mail/John/1992 --> ~/.../John/1992

      (3) display the last directory if no other special case
          applies

          example: /usr/local/gnu/src/gzip/foobar.el --> gzip/foobar.el

          dired buffer example: /usr/local/gnu/src/gzip --> src/gzip
")

(defvar mode-line-abbrev-dired t
  "*A value of `t' means that mode-line.el will abbreviate directory paths in
Tree Dired buffers via its `dired-after-readin-hook.'  Otherwise, not done.
Default: t
")

;;;; DEFUNS

;;; the function that makes the substitutions - this is the work-horse

(defun mode-line-replace-regexp-in-alist (string replacement-alist)

   "Given a string STRING, replace *each* instance of <regexp> (cars of elements
in REPLACEMENT-ALIST) with <to-string> (cdrs of elements in REPLACEMENT-ALIST)
and return the new string. The above is different from simply replacing the
first match in the alist and then leaving. This is why a temporary buffer is
used."

   (save-excursion
       
     (let 

         ;; VARLIST - we need to generate a unique name for temporary buffer
         ;; (originally just used `!@#$%^&*' which, believe or not, might not be
         ;; unique - LRD)

         ((temp-buffer (get-buffer-create (make-temp-name "!@#$%^&*")))
          (temp-alist replacement-alist) ; don't mess with incoming alist
          (new-string)) ; this is the value to be returned 

       ;; create temporary buffer
       (set-buffer temp-buffer)
       
       ;; insert incoming string (name of filename with full path name)
       (insert string)
       
       ;; we want to make sure the temporary buffer is killed
       (unwind-protect
           
           ;; BODY
           (progn
             
             ;; walk down `temp-alist', removing as we go, until it is empty
             (while temp-alist
               
               ;; go to beginning of temporary buffer
               (goto-char (point-min))
               
               ;; search the temporary buffer for every occurrence of the
               ;; regular expression stored in `(car (car temp-alist))' and
               ;; replace it with the one stored in `(cdr (car temp-alist))'
               ;; (code originally used replace-regexp - LRD)
               
               (while (re-search-forward (car (car temp-alist)) nil t)
                 (replace-match (cdr (car temp-alist)) t))
               
               ;; decrement temp-alist and restart while-loop
               (setq temp-alist (cdr temp-alist)))
             
             ;; set return string to what remains in the temporary buffer
             (setq new-string (buffer-string)))
         
         ;; CLEAN UP - no matter what happens, remove the temporary buffer
         (kill-buffer temp-buffer))
     
     ;; return value of converted string
     new-string)))

;;; function that creates the abbreviated identification and is called by the
;;; various hooks (originally returned non-nil values - LRD)

(defun mode-line-abbreviate-buffer-identification ()

  "Abbreviates mode-line-buffer-identification locally using the function
mode-line-replace-regexp-in-alist and the alist mode-line-abbreviation-alist.
This function will return nil always. This is needed for view-mode since it
will call this function even if it is not visiting a file and its return value
needs to be predictable (as opposed to garbage). A nil return is also needed
for the write-file-hooks."

  (if buffer-file-name
      (progn
        (setq mode-line-buffer-identification
              (list
               (mode-line-replace-regexp-in-alist buffer-file-name
                                               mode-line-abbreviation-alist)))
        (setq mode-line-state "abbreviated name"))
    ;; an attempt at Tree Dired support - instead of the buffer-file-name we 
    ;; pass the default-directory sans the last slash
    (if (and mode-line-abbrev-dired (eq major-mode 'dired-mode))
      (progn
        (setq mode-line-buffer-identification
              (list (mode-line-replace-regexp-in-alist
                     ;; special case of home directory
                     (if (string= default-directory (expand-file-name "~/"))
                         ;; pass the whole thing, otherwise looks funny
                         default-directory
                       ;; remove final slash
                       (substring default-directory 0 -1))
                     mode-line-abbreviation-alist)))
        (setq mode-line-state "abbreviated name"))
    (setq mode-line-state "buffer-name")))
  ;; always return nil
  nil)

;;;; HOOKS

;;; Add mode-line-abbreviate-buffer-identification to find-file-hooks,
;;; write-file-hooks, and view-hook but only if it has not been added already
;;; (originally overwrote find-file-hooks - LRD).

;;; add to Tree Dired's `dired-after-readin-hook' this allows dired buffers to 
;;; contain abbreviated paths in the mode line too

(if (fboundp 'add-hook)
    ;; use v19's add-hook
    (progn
      (add-hook 'find-file-hooks 'mode-line-abbreviate-buffer-identification)
      (add-hook 'view-hook 'mode-line-abbreviate-buffer-identification)
      (add-hook 'write-file-hooks 'mode-line-abbreviate-buffer-identification)
      (add-hook 'after-save-hook 'mode-line-abbreviate-buffer-identification)
      (add-hook 'dired-after-readin-hook
                'mode-line-abbreviate-buffer-identification))

  ;; check by hand
  (or (memq 'mode-line-abbreviate-buffer-identification find-file-hooks)
      (setq find-file-hooks
            (append '(mode-line-abbreviate-buffer-identification)
                    find-file-hooks)))
  (or (memq 'mode-line-abbreviate-buffer-identification view-hook)
      (setq view-hook
            (append '(mode-line-abbreviate-buffer-identification) 
                    view-hook)))
  (or (memq 'mode-line-abbreviate-buffer-identification write-file-hooks)
      (setq write-file-hooks 
            (append '(mode-line-abbreviate-buffer-identification)
                    write-file-hooks)))
  (defvar dired-after-readin-hook nil) ; may not be defined yet...
  (or (memq 'mode-line-abbreviate-buffer-identification
            dired-after-readin-hook)
      (setq dired-after-readin-hook
            (cons 'mode-line-abbreviate-buffer-identification
                  dired-after-readin-hook))))

;;;; TOGGLE 

(define-key global-map "\C-c\C-t" 'mode-line-toggle-display)

(defun mode-line-toggle-display (arg)

  "Cycles the buffer descriptor currently being displayed in modeline. If
current display is abbreviated, then display with full path.  If full path is
currently displayed, then display abbreviated.  With argument will scroll
through displays automatically.
Bound to C-c C-t"

  (interactive "P")

  ;; check to see if we should toggle at all 
  (if (or buffer-file-name
          (and mode-line-abbrev-dired (eq major-mode 'dired-mode)))
      
      (if arg

          ;; scroll display
          (progn (mode-line-toggle-display nil)
                 (sit-for 1)
                 (message " ")
                 (mode-line-toggle-display nil))

        ;; change display 
        (progn

          (if (string= mode-line-state "abbreviated name")
              
              ;; already abbreviated
              (setq mode-line-buffer-identification
                    (if buffer-file-name
                        ;; file
                        (buffer-file-name)
                      ;; dired
                      (substring default-directory 0 -1))
                    mode-line-state "full path name")
            
            ;; not already abbreviated
            (mode-line-abbreviate-buffer-identification)
            (setq mode-line-state "abbreviated name"))
            
          ;; force redisplay of mode line
          (message "%s of buffer %s" mode-line-state (buffer-name))
          (set-buffer-modified-p (buffer-modified-p))))))

;;;; BUG REPORTS

;;; this section is provided for reports.
;;; using Barry A. Warsaw's reporter.el

(defconst mode-line-version "2.94"
  "Revision number of mode-line.el -- includes abbreviated paths in mode
line. type M-x mode-line-submit-report to send bug report.  available via
anonymous ftp in:

 /roebling.poly.edu:/pub/lisp/mode-line.el.gz
 /archive.cis.ohio-state.edu:/pub/gnu/emacs/elisp-archive/misc/mode-line.el.Z")

(defconst mode-line-help-address "dodd@roebling.poly.edu"
  "Address accepting submission of reports on mode-line.el.")

(defconst mode-line-maintainer "Larry"
  "First name of person accepting submission of reports on mode-line.el.")

(defconst mode-line-file "mode-line.el"
  "Name of file containing emacs lisp code.")

(defconst mode-line-variable-list
  (list 'mode-line-abbreviation-alist)
  "List of variables whose values are to be appended to reports on mode-line
sent using mode-line-submit-report.")

(defun mode-line-submit-report ()

  "Submit via reporter.el a report on mode-line-file version
mode-line-version, to mode-line-maintainer at address mode-line-help-address
listing variables mode-line-variable-list in the message."

  (interactive)

  (require 'reporter)

  (reporter-submit-bug-report
   mode-line-help-address                        ; address
   (concat mode-line-file " " mode-line-version) ; pkgname
   mode-line-variable-list                       ; varlist
   nil nil                                       ; pre-hooks and post-hooks
   (concat "Yo, " mode-line-maintainer "!")))     ; salutation

;;;; provide the package

(provide 'mode-line)

;;; mode-line.el ends here

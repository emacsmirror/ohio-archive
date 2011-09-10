;;; which-function.el -- Print current function in mode line
;;; Copyright (C) 1994 Alexander Rezinsky <alexr@msil.sps.mot.com>
;;;
;;; Author:   Alex Rezinsky <alexr@msil.sps.mot.com>
;;; Created:  3 Jule 1994
;;; Version:  1.3
;;; Keywords: mode-line imenu
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; COMMENTARY
;;; ----------
;;; This package prints name of function where  your current point is
;;; located in mode line. It assumes that you work with imenu package
;;; and imenu--index-alist is up to date.
;;;
;;; NOTE that imenu.el package is a part of emacs 19.25 distribution.

;;; INSTALLATION
;;; ------------
;;;   Put this file in your load-path and insert the following in .emacs
;;;  
;;; (setq .........   - customization of package, see
;;;                     "Variables for customization" section.
;;; ....
;;; (require 'which-function)
;;; (which-func-mode)

;;; KNOWN BUGS
;;; ----------
;;; Really this package shows not  "function where the current  point
;;; is located now", but "nearest   function which defined above  the
;;; current point". So if your current point is  located after end of
;;; function  FOO but before   begin  of function  BAR,  FOO  will be
;;; displayed in mode line.

;;; LCD Archive Entry:
;;; which-function|Alex Rezinsky|alexr@msil.sps.mot.com|
;;; Dynamically print current function in mode line|
;;; 03-Jul-1994|1.3|~/misc/which-function.el.Z|

;;; TODO LIST
;;; ---------
;;;     1. Dependence from imenu  package should be removed. Separate
;;; function determination mechanism should  be used to determine end
;;; of functions as well as begin of functions.
;;;     2. This package  should be realized with  the help of overlay
;;; properties   instead of  imenu--index-alist  variable  (thanks to
;;; RMS).

;;; HISTORY
;;; -------
;;; v1.3 Jule 3  1994 Alex Rezinsky
;;;   permanent-local property has been added to variable
;;;   which-func-mode. Now this mode doesn't dissapear even
;;;   if you change mode dynamically.
;;; v1.2 June 27 1994 Alex Rezinsky
;;;   Global variable names changed to be more consistent, 
;;;   some customization variables added.
;;; v1.1 June 26 1994 Alex Rezinsky
;;;   Nested indexes bug fixed.
;;;   Thanks to Peter Eisenhauer
;;; v1.0 June 23 1994 Alex Rezinsky
;;;   First release.

;;; THANKS TO
;;; ---------
;;; Per Abrahamsen   <abraham@iesd.auc.dk>
;;;     Some ideas (inserting  in mode-line,  using of post-command  hook
;;;     and toggling this  mode) have  been   borrowed from  his  package
;;;     column.el
;;; Peter Eisenhauer <pipe@fzi.de>
;;;     Bug fixing in case nested indexes.
;;; Richard Stallman <rms@gnu.ai.mit.edu>
;;;     Ideas how to improve this package (not realized yet)
;;; Terry Tateyama   <ttt@ursa0.cs.utah.edu>
;;;     Suggestion to use find-file-hooks for first imenu
;;;     index building.

;;; Variables for customization
;;; ---------------------------
;;;  
(defvar which-func-unknown "???"
  "This is printed in mode line if current funcion is unknown.")

(defvar which-func-modes 
  '(emacs-lisp-mode c-mode c++-mode perl-mode makefile-mode sh-mode)
  "List  of modes  for  which   current function recognition   is
enabled. For any  other mode it is disabled.  If this is equal to
'ANY then current function recognition is enabled in any mode.")
;;; (defvar which-func-modes 'ANY)   ; - for all modes

(defvar which-func-amodes '(emacs-lisp-mode c-mode c++-mode)
  "List   of   modes   for   which    index  creating    function
(imenu-create-index-function  from  imenu package) will be called
automatically by find-file-hooks if you  enter to file with  this
mode fisrt time.  Setting of this  variable to  nil disables this
feature.")

(defvar which-func-maxout 100000
  "First automatic call of imenu-create-index-function function
is disabled in buffers larger than this.  If this variable zero -
it always is enabled.")

(defvar which-func-to-minor-modes t
  "If non-nil display current  function name in minor modes area,
otherwise  display this name in  mode line after which-func-after
item (old style).")

(defvar which-func-format '("- " which-func-current " ")
  "Format for displaying the function in the mode line.")

(defvar which-func-after '(-3 . "%p")
  "Display function after this element in the mode line. Relevant
only if which-func-to-minor-modes is nil.")


;;; Code, nothing to customize below here
;;; -------------------------------------
;;;
(if (not (featurep 'imenu)) (require 'imenu))  ; Require imenu if it not loaded
(defvar which-func-current  which-func-unknown)
(defvar which-func-previous which-func-unknown)
(make-variable-buffer-local 'which-func-current)
(make-variable-buffer-local 'which-func-previous)

(defvar which-func-mode-global nil
  "Show current function in mode line is globally enabled if non-nil.")

(defvar which-func-mode nil
  "Show  current  function  in mode  line   is locally enabled if
non-nil. Make sense only if which-func-mode-global is non-nil")
(make-variable-buffer-local 'which-func-mode)
(put 'which-func-mode 'permanent-local t)

;; Entry for function name in mode line.
(defconst which-func-entry
  (list 'which-func-mode (cons "" which-func-format)))

(if which-func-to-minor-modes

    ;; Add which-func-format to minor modes list in the mode line.
    (or (member which-func-entry minor-mode-alist)
        (setq minor-mode-alist
              (cons which-func-entry minor-mode-alist)))

  ;; Add which-func-format to mode-line-format after which-func-after.
  (or (member which-func-entry mode-line-format)
      (let ((entry (member which-func-after mode-line-format)))
        (setcdr entry (cons which-func-entry (cdr entry)))))
)

(add-hook 'find-file-hooks 'which-func-ff-hook t)

(defun which-func-ff-hook ()
  " File find hook for which-function package. It calls
imenu-create-index-function function to create a
imenu--index-alist list, if it is necessary."
  (if (or (eq which-func-modes 'ANY) (member major-mode which-func-modes))
      (setq which-func-mode which-func-mode-global)
    (setq which-func-mode nil)
  )
  (if (and which-func-mode
           (member major-mode which-func-amodes)
           (or (< buffer-saved-size which-func-maxout)
               (= which-func-maxout 0)
           )
      )
      (setq imenu--index-alist
	    (save-excursion (funcall imenu-create-index-function))
      )
  )
)

(defun which-func-update ()
  ;; Update the string containing the current function.
  (condition-case info
    (progn
      (if (not (setq which-func-current (which-function)))
          (setq which-func-current which-func-unknown))
      (if (not (string= which-func-current which-func-previous))
        (progn
          (force-mode-line-update)
          (setq which-func-previous which-func-current)
        )
      )
    )
    (error
     (ding)
     (remove-hook 'post-command-hook 'which-func-update)
     (which-func-mode -1)   ; Function mode off
     (message "which-func-update error: %s" info)
    )
  )
)

(defun which-func-mode (&optional arg)
  "Toggle display function mode.  With  prefix arg, turn  display
function mode on iff arg is positive and off otherwise.

When  display  function  mode  is  on, the  current  function  is
displayed in the mode line."
  (interactive "P")
  (if (or (and (null arg) which-func-mode-global)
          (<= (prefix-numeric-value arg) 0))
      ;; Turn it off
      (if which-func-mode-global
          (progn
            (remove-hook 'post-command-hook 'which-func-update)
            (setq which-func-mode-global nil)
            (setq which-func-mode nil)
            (force-mode-line-update)
          )
      )
    ;;Turn it on
    (if which-func-mode-global
        ()
      (add-hook 'post-command-hook 'which-func-update)
      (setq which-func-mode-global t)
      (if (or (eq which-func-modes 'ANY) (member major-mode which-func-modes))
          (setq which-func-mode t)
        (setq which-func-mode nil)
      )
    )
  )
)

(defun which-function ()
  "Returns function (imenu  index)  where  is current point.   If
imenu--index-alist not  exists  or if it  is empty  or if current
point is located before first function, returns nil"
  (and
   (boundp 'imenu--index-alist)
   imenu--index-alist
   (let ((pair (car-safe imenu--index-alist))
         (rest (cdr-safe imenu--index-alist))
         (name nil))
     (while (and pair (or (not (numberp (cdr pair))) (> (point) (cdr pair))))
       (setq name (car pair))
       (setq pair (car-safe rest))
       (setq rest (cdr-safe rest))
     )
     name
   )
  )
)

(provide 'which-function)

;; which-function ends here

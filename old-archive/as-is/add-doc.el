;To: unix-emacs@bbn.com
;Date: 25 Apr 89 20:05:51 GMT
;From: Mike Heavin <littlei!omepd!mgh@uunet.uu.net>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: Function for customizing mode descriptions
;Organization: BiiN Information Systems, Hillsboro, Oregon
;Source-Info:  From (or Sender) name not authenticated.
;
;Here is a function for adding documentation to the default mode description.
;
;I developed this because I made some major modifications to the some of the
;major modes and wanted to document them in the mode description, but I did
;not want to change the default mode description.
;
;This function appends custom information to the end of the mode description
;text when a describe-mode command is executed.  This should enable you to 
;document any changes or additions you might make  in a mode hook.
;
;Enjoy.
;
;Mike Heavin
;
;P.S.  this is my first attempt at posting code to the net; hopefully it will  
;      work.
;
;------------------------- cut here --------------------------------
;; Function to add documentation to mode descriptions
;; Copyright (C) 1988, 1989 Free Software Foundation, Inc.

;; This file is not officially part of GNU Emacs, but is being donated
;; to the Free Software Foundation.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; Author: Mike Heavin
;; Last changed: April 25, 1989 by mgh (Mike Heavin) on intelob
;; 

;; --------------- start of add-to-mode-documentation --------
;; define function to add documentation to mode description

(defun add-to-mode-documentation (this-mode-hook-function-name)
"Appends user documentation to the end of the mode description.
Normally, this would contain documentation about user enhancements/changes
to the mode.

Execute this function in the mode hook function and pass the name of the
hook function as a string.  For example:

   (defun setup-foo-mode ()
   \"custom stuff to be loaded when foo mode is invoked\"
      (add-to-documentation \"setup-foo-mode\") ; add above to mode description
      (local-set-key ...) ; define some keys locally
      (setq case-fold-search nil) ; turn off case-senstive searches
      (abbrev-mode 1) ; turn on abbrev mode
   )

where you have set the following:

   (setq foo-mode-hook 'setup-foo-mode)


When you ask for a mode description, the default description for foo mode is
displayed followed by the setup-foo-mode function description.

With the above example, a invoking describe-mode produces:

   <foo mode description stuff>

   LOCAL MODE MODIFCATIONS

   custom stuff to be loaded when foo mode is invoked  
"
(setq major-mode
    (list
     'lambda
     nil
     (concat (documentation major-mode) "\n\nLOCAL MODE MODIFICATIONS\n\n"
	(documentation this-mode-hook-function-name))
     (list
	'funcall
	major-mode)))
)
;--
;----------------******************---------------------
;Mike Heavin
;
;These ramblings do not necessarily reflect the views of my employer, my spouse,
;my friends, or any other sentient being.  They are mine alone; I am my own
;worst enemy.
;----------------******************---------------------
;

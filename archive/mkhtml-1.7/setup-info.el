;;; `setup-info.el' --- Definitions to be loaded *before* `info.el'.
;; 
;; Author: D. ADAMS
;; Maintainer: D. ADAMS
;; Copyright (C) 1999-2001, Drew Adams, all rights reserved.
;; Created: Wed Apr 14 10:37:15 1999
;; Version: $Id $
;;   Last modified by: 
;;   Last modified on: Wed Jan  3 09:45:13 2001
;;   Update count: 101
;; Keywords: help, docs
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;    Definitions to be loaded *before* `info.el'.
;;
;;
;; NOTE: The faces defined here look best on a medium-dark background.
;;       Try, for example, setting the background to "LightSteelBlue"
;;       in your `~/.emacs' file: You can do this is via
;;       `special-display-buffer-names':
;;
;;         (setq special-display-buffer-names
;;               (cons '("*info*" (background-color . "LightSteelBlue"))
;;                     special-display-buffer-names))
;;
;;       You can alternatively change the background value of
;;       `special-display-frame-alist' and set
;;       `special-display-regexps' to something matching "*info*":
;;
;;         (setq special-display-frame-alist 
;;               (cons '(background-color . "LightSteelBlue")
;;                     special-display-frame-alist))
;;         (setq special-display-regexps '("[ ]?[*][^*]+[*]"))
;;
;;
;;  ***** NOTE: The following user options (variables) defined in
;;              `info.el' have been REDEFINED HERE:
;;
;;  `Info-fontify-maximum-menu-size', `Info-title-face-alist'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; RCS $Log: setup-info.el,v $
;; RCS Revision 1.3  2001/01/03 17:45:15  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.2  2001/01/03 01:08:38  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.1  2000/09/14 17:23:53  dadams
;; RCS Initial revision
;; RCS
; Revision 1.2  1999/04/14  11:26:35  dadams
; Added defvar: Info-fontify-maximum-menu-size (100000, not 30000).
;
; Revision 1.1  1999/04/14  10:44:36  dadams
; Initial revision
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'def-face-const)


(provide 'setup-info)

;;;;;;;;;;;;;;;;;;;;


;;; Faces.  See NOTE at beginning of this file regarding frame background color.
(unless (boundp 'red-on-lightgray-face)
  (define-face-const "Red" "LightGray"))
(unless (boundp 'firebrick-on-lightgray-face)
  (define-face-const "Firebrick" "LightGray"))
(unless (boundp 'darkmagenta-on-lightgray-face)
  (define-face-const "DarkMagenta" "LightGray"))

;;;###autoload
(defvar Info-title-face-alist
  '((?* red-on-lightgray-face bold underline)
    (?= firebrick-on-lightgray-face bold-italic underline)
    (?- darkmagenta-on-lightgray-face italic underline))
  "*Alist or list of faces to use to replace underlined titles.
The alist key is the character the title would otherwise be
underlined with (?*, ?= or ?-).")


;;;###autoload
(defvar Info-fontify-maximum-menu-size 100000
  "*Maximum size of menu to fontify if `Info-fontify' is non-nil.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `setup-info.el' ends here

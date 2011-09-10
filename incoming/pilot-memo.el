;;; pilot-memo.el - write the current buffer out to a PalmOS device as a memo.
;;
;; Copyright (C) 1999 Andrew J Cosgriff
;; http://polydistortion.net/sw/emacs-lisp/
;;
;; Author: Andrew J Cosgriff <ajc@bing.wattle.id.au>
;; Created: Wed Aug 18 22:31:35 1999
;; Version: $Id: pilot-memo.el,v 1.12 1999/12/03 02:47:53 ajc Exp $
;; Keywords: palm pilot memo
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Inspired by David Maslen :)
;;
;; Usage : M-x save-buffer-to-pilot
;;
;; It prompts for the title of the memo and the category to store it
;; in.
;;
;; In the Gnus Summary buffer, you can also 
;; M-x gnus-summary-save-article-pilot
;; to save the current article to your PalmOS device.
;;
;; You need the `pilot-link' software installed, as this code simply
;; runs the external `install-memo' program to do all the hard work.
;;
;; `pilot-link' source can be found at
;; ftp://ryeham.ee.ryerson.ca/pub/PalmOS/ or, if you're running one of
;; the more popular Linux distributions, there may be an installable
;; package available as part of the distribution....
;;
;;; TO DO:
;;
;; - maybe also add a w3 hook to pick a reasonable default for the
;;   title ?
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; History:
;;
;; 1.1 - Initial version
;;
;; 1.5 - variables are customizable
;;       - allow setting of category to put it in (at customize-time,
;;         anyway)
;;       - Gnus binding - M-x gnus-summary-save-article-pilot
;;         (what to bind this to ?)
;;
;; 1.10 - include patch from Rob Riepel :
;;         - changes default category to "Unfiled" (since most folks
;;           probably don't have an "Emacs" category)
;;         - changes the default device to "/dev/pilot" (since the
;;           pilot-link stuff recommends making that a link to the
;;           real pilot device and as such has a better chance of
;;           being there than "/dev/ttyS0")
;;         - adds history variable for the title and category prompts
;;         - changed "memo title" to just "Title"
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst pilot-memo-version (substring "$Revision: 1.12 $" 11 -2)
  "Version of pilot-memo.")

;;
;;; User Customizable Variables:
;;
(defgroup pilot-memo nil
  "Upload emacs buffers to your Palm device.")

(defcustom pilot-memo-category "Unfiled"
  "Default category to upload memos into."
  :type 'string
  :group 'pilot-memo)

(defcustom pilot-memo-install-memo-program "install-memo"
  "Program to run that will install the memo onto your Palm device."
  :type 'string
  :group 'pilot-memo)

(defcustom pilot-memo-device (or (getenv "PILOTPORT") "/dev/pilot")
  "Device name for the serial port to which your Palm's hotsync cable is connected."
  :type 'string
  :group 'pilot-memo)

;;
;;; Other Variables:
;;
(defvar pilot-memo-title-history nil)
(defvar pilot-memo-category-history (list pilot-memo-category))

;;
;;; Code:
;;

;;
;; Yick.  Maybe there's a better way of doing this ?
;; (it's here for Emacs/XEmacs compatibility...)
;;
(defun pilot-get-temp-dir ()
  (cond ((boundp 'temp-directory)
	 temp-directory)
	((boundp 'temporary-file-directory)
	 temporary-file-directory)
	(t ".")))

(defun save-buffer-to-pilot (title category)
 "Save the current buffer to a memo on your Palm device.
TITLE is the memo title to be used on the Palm device, which defaults
to the buffer name.
CATEGORY is the memo category to place it in on the Palm device."
  (interactive
   (list
    (read-from-minibuffer
     "Title: " (buffer-name) nil nil 'pilot-memo-title-history)
    (read-from-minibuffer
     "Category: " (nth 0 pilot-memo-category-history) nil nil
     (cons 'pilot-memo-category-history 1))))
  (let ((pilot-memo-buffer (get-buffer-create "*pilot-memo*"))
	(pilot-memo-filename (concat (pilot-get-temp-dir) "/pilot-memo.txt"))
	(pilot-memo-out-buffer (get-buffer-create "*Shell Command Output*")))
    (save-excursion
      (copy-region-as-kill (point-min) (point-max))
      (set-buffer pilot-memo-buffer)
      (erase-buffer)
      (insert title "\n")
      (yank)
      (delete-windows-on pilot-memo-buffer)
      (write-file pilot-memo-filename nil))
    (kill-buffer pilot-memo-buffer)
    (message "Press the hotsync button...")
    (shell-command
     (concat pilot-memo-install-memo-program
	     " -q -c \"" category "\" -p "
	     pilot-memo-device " " pilot-memo-filename))
    (delete-file pilot-memo-filename)))

(defun gnus-summary-save-article-pilot (&optional arg)
  "Save the current article to a memo on your Palm device.
If N is a positive number, save the N next articles.
If N is a negative number, save the N previous articles.
If N is nil and any articles have been marked with the process mark,
save those articles instead."
  (interactive "P")
  (let ((gnus-default-article-saver 'gnus-summary-save-in-pilot-memo))
    (gnus-summary-save-article arg)))

(defun gnus-summary-save-in-pilot-memo (&optional filename)
  "Save this article to a memo on your Palm device.
Optional argument FILENAME specifies file name."
  (gnus-eval-in-buffer-window gnus-save-article-buffer
    (save-buffer-to-pilot 
     (read-from-minibuffer
      "Title: " (mail-header-subject gnus-current-headers) nil nil 'pilot-memo-title-history)
     (read-from-minibuffer
      "Category: " (nth 0 pilot-memo-category-history) nil nil 
      (cons 'pilot-memo-category-history 1)))))

;;; pilot-memo.el ends here

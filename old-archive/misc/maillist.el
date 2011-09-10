;;  maillist.el
;;  mailing list utilities for GNU Emacs.
;;
;;  written by Lee Short (short@asf.com)
;;  Copyright (C) 1992 Lee Short.
;;  last mod: 20 November, 1992

;; LCD Archive Entry:
;; maillist|Lee Short|short@asf.com|
;; Mailing list utilities.|
;; 92-11-20||~/misc/maillist.el.Z|

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation.  

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a copy of the GNU General Public License write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


(defvar  auto-cc-on-maillist-reply nil
"This variable determines if the user will be automatically CCed on replies
to the author of a mailing list message.  This will not work properly unless 
you also have rmail-reply-cc.el" )

(defvar  auto-cc-on-maillist-followup nil
"This variable determines if the user will be automatically CCed on followups
to a mailing list message.  This will not work properly unless 
you also have rmail-reply-cc.el" )

(defvar  auto-cc-on-nonlist-reply nil
"This variable determines if the user will be automatically CCed on replies
to a mail message.  This will not work properly unless 
you also have rmail-reply-cc.el" )

; the following defvar should be uncommented if rmail-reply-cc.el is not used
;
; (defvar  my-mail-address "foo@bar.com" 
; "The user's email address as it will be automaticlly inserted on CC: 
; lines.")

; the following defvar should be uncommented if rmail-reply-cc.el is not used
;
; (defvar  auto-cc nil 
; "The value of this variable determines if the user will be automatically 
; CC'd on mail messages.  )


(defconst maillists 
       '( ( "Reply-to: DRS@utxvm.cc.utexas.edu" "~/drs.rmail" ) 
          ( "Reply-To: Dead Runners Mind <DRM@DARTCMS1.BITNET>" "~/drs.rmail" )
        )
 "A list which describes the user's mailing lists in a format usable by 
maillist.el.  Each entry in the list is itself a list.  The first entry in 
each sublist is an identifying string in the text of messages from the 
mailing list, the second is the name of the rmail file in which to save 
messages from the mailing list.  Any message in which the identifying 
string is found is assumed to be a message from the mailing list.  Thus, 
a forwarded message from the mailing list is likely to be flagged as 
coming from the list."
)


(defun file-to-maillist ()
"Saves the current rmail message to the rmail file for its mailing list, as 
specified in the constant maillists."
;;  searches the message for a match with any of the identifying strings 
;;  given in maillists.  As soon as it finds a match, it outputs the message
;;  to the mailing list's rmail file and deletes it from the current rmail
;;  file
   (interactive)
   (rmail-output-to-rmail-file 
      (let  ( (current-list maillists) 
              (rmail-filename nil)  )
          (while  (and (car current-list) (not rmail-filename) )
              (if (my-string-find (car (car current-list) ) )
                  (setq rmail-filename (car (cdr ( car current-list) ) ) )  )
              (setq current-list (cdr current-list) )
          )
          rmail-filename
      )
   )
   (rmail-delete-forward)
)


(defun reply-to-maillist-message ()
"Initiates a reply to the author of a mailing list article."
   (let  ( (from nil) )
      (interactive)
      (delete-other-windows)
      (setq auto-cc auto-cc-on-maillist-reply)
      (rmail-reply t)		;; you are now in the mail buffer

      (beginning-of-buffer)
      (word-search-forward "To: " nil t)
      (kill-line)               ;; waste the old "to" field
      (insert ": ")

      (other-window 1)          ;; you are now in the rmail buffer
      (setq from (mail-fetch-field "from") )      ;; find the author's name

      (other-window 1)		;; you are now in the mail buffer
      (insert from)          ;; insert the name of the author

      (end-of-buffer)
      (newline)
   )
)


(defun my-rmail-followup  ()
"Initiates a followup to the mailing list in reply to a mailing list article."
   (interactive)
   (delete-other-windows)
   (setq auto-cc auto-cc-on-maillist-followup)
   (rmail-reply t)
)

(defun my-string-find  (string-arg)
"Searches for a string in the current buffer.  Returns t if it is found, 
nil otherwise."
   (set-mark-command nil)          ;;  store point
   (beginning-of-buffer)
   (if  (word-search-forward string-arg nil t)
       (progn
          (set-mark-command t)          ;;  restore point
          t
       )
       (progn
          (set-mark-command t)          ;;  restore point
          nil
       )
   )
)

(defun maillist-p ()
"Determines if the current rmail message is in one of the mailing lists as 
defined by the constant maillists."
   (let  ( (current-list maillists) 
           (found nil)  )
       (while  (and (car current-list) (not found) )
           (if (my-string-find (car (car current-list) ) )
               (setq found t)  )
           (setq current-list (cdr current-list) )
       )
       found
   )
)

(defun my-rmail-reply  ()
"Sends a reply to the author of an rmail message, if the message is either a 
regular message, or a message from one of the mailing lists defined by the 
variable maillists.  Any other mailing list message will likely result in 
a reply to the mailing list, rather than the author."  
    (interactive)
    (if (maillist-p)
        (reply-to-maillist-message)
        (progn
           (setq auto-cc auto-cc-on-nonlist-reply)
           (rmail-reply nil)
        )
    )
)

;;; ISWITCHB.EL --- switch between buffers using substrings

;; Copyright (C) 1996 Stephen Eglen

;; Author: Stephen Eglen <stephene@cogs.susx.ac.uk>
;; Maintainer: Stephen Eglen <stephene@cogs.susx.ac.uk>
;; Created: 15 Dec 1996
;; $Revision: 1.13 $
;; Keywords: extensions
;; location: http://www.cogs.susx.ac.uk/users/stephene/emacs

 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; LCD Archive Entry:
;; iswitchb|Stephen Eglen|<stephene@cogs.susx.ac.uk>|
;; switch between buffers using substrings|
;; 30-Dec-1996|1.13|~/packages/iswitchb.el|


;;; Installation:

;; To load the package, do
;; (require 'iswitchb)
;; To get the functions in this package bound to keys, do
;; (iswitchb-default-keybindings)
;; 
;; Has been tested on Emacs 19.34 and XEmacs 19.14.

;;; Commentary:

;; As you type in a substring, the list of buffers currently matching
;; the substring are displayed as you type.  The list is ordered so
;; that the most recent buffers visited come at the start of the list.
;; The buffer at the start of the list will be the one visited when
;; you press return.  By typing more of the substring, the list is
;; narrowed down so that gradually the buffer you want will be at the
;; top of the list.  Alternatively, you can use C-s an C-r to rotate
;; buffer names in the list until the one you want is at the top of
;; the list.  Completion is also available so that you can see what is
;; common to all of the matching buffers as you type.

;; This code is similar to a couple of other packages.  Michael R Cook
;; <mcook@cognex.com wrote a similar buffer switching package, but
;; does exact matching rather than substring matching on buffer names.
;; I also modified a couple of functions from icomplete.el to provide
;; the completion feedback in the minibuffer.

;;; Example 

;;If I have two buffers called "123456" and "123", with "123456" the
;;most recent, when I use iswitchb, I first of all get presented with
;;the default buffer (xxx) to switch to:
;;
;;       iswitch  {default xxx} 
;;
;; If I then press 2:
;;       iswitch 2[3]{123456,123}
;;
;; The list in {} are the matching buffers, most recent first (buffers
;; visible in the current frame are put at the end of the list by
;; default).  At any time I can select the item at the head of the
;; list by pressing RET.  I can also bring the put the first element
;; at the end of the list by pressing C-s, or put the last element at
;; the head of the list by pressing C-r.  The item in [] indicates
;; what can be added to my input by pressing TAB.  In this case, I
;; will get "3" added to my input.  So, press TAB: 
;;	 iswitch 23{123456,123}
;;
;; At this point, I still have two matching buffers.
;; If I want the first buffer in the list, I simply press RET.  If I
;; wanted the second in the list, I could press C-s to move it to the
;; top of the list and then RET to select it.
;;
;;However, If I type 4, I only have one match left:
;;       iswitch 234[123456] [Matched]
;;
;;Since there is only one matching buffer left, it is given in [] and we
;;see the text [Matched] afterwards.  I can now press TAB or RET to go
;;to that buffer.
;;
;; If however, I now type "a":
;;       iswitch 234a [No match]
;; There are no matching buffers.  If I press RET or TAB, I can be
;; prompted to create a new buffer called "234a".
;;
;; Of course, where this function comes in really useful is when you
;; can specify the buffer using only a few keystrokes.  In the above
;; example, the quickest way to get to the "123456" buffer would be
;; just to type 4 and then RET (assuming there isnt any newer buffer
;; with 4 in its name).


;;
;;  See the doc string of iswitchb for full keybindings and features.
;;  (describe-function 'iswitchb)

;;; Customisation

;; See the User Variables section below for easy ways to change the
;; functionality of the program.
;; To modify the keybindings, use the hook provided.  For example:
;;(add-hook 'iswitchb-define-mode-map-hook
;;	  'iswitchb-my-keys)
;;
;;(defun iswitchb-my-keys ()
;;  "Add my keybings for iswitchb."
;;  (define-key iswitchb-mode-map " " 'iswitchb-next-match)
;;  )
;;
;; Seeing all the matching buffers.
;; If you have many matching buffers, they may not all fit onto one
;; line of the minibuffer.  In this case, you should use rsz-mini
;; (resize-minibuffer-mode).  You can also limit iswitchb so that it
;; only shows a certain number of lines -- see the documentation for
;; `iswitchb-minibuffer-setup-hook'.



;;; Comparison with iswitch-buffer

;; This package is a rewrite of iswitch-buffer, using the minibuffer
;; rather than the echo area.  The advantages of using the minibuffer
;; are several:
;; o minibuffer has more powerful editing facilities
;; o doesnt interfere with other packages that use the echo area
;; o *Messages* buffer doesnt get filled up with all of the messages that
;;   go to the modeline
;; o cursor is in the minibuffer, which somehow looks right.
;; o minibuffer can be resized dynamically to show all the possible matching
;;   buffers rather than just the first line's worth (using rsz-mini).
;;
;; Disadvantages:
;; o cant change the prompt to indicate status of searching (eg whether
;;   regexp searching is currently on).


;;; Acknowledgements

;; Thanks to Jari Aalto <jari.aalto@poboxes.com> for help with the
;; first version of this package, iswitch-buffer.  Thanks also to many
;; others for testing earlier versions.

;;; Code:

(defconst iswitchb-version (substring "$Revision: 1.13 $" 11 -2)
  "$Id: iswitchb.el,v 1.13 1996/12/30 22:29:09 stephene Exp $

Report bugs to: Stephen Eglen <stephene@cogs.susx.ac.uk>")


;;; User Variables
;;
;; These are some things you might want to change.

(defvar iswitchb-case case-fold-search
  "*Non-nil if searching of buffer names should ignore case.")

(defvar iswitchb-buffer-ignore
  '("^ ")
  "*List of regexps or functions matching buffer names to ignore.  For
example, traditional behavior is not to list buffers whose names begin
with a space, for which the regexp is \"^ \".  See the source file for
example functions that filter buffernames.")

;;; Examples for setting the value of iswitchb-buffer-ignore
;(defun -c-mode (name)
;  "Ignore all c mode buffers -- example function for iswitchb."
;  (save-excursion
;    (set-buffer name)
;    (string-match "^C$" mode-name)))

;(setq iswitchb-buffer-ignore '("^ " ignore-c-mode))
;(setq iswitchb-buffer-ignore '("^ " "\\.c$" "\\.h$"))


(defvar iswitchb-default-method  'always-frame
    "*How to switch to new buffer when using iswitchb.
Possible values:
`samewindow'	Show new buffer in same window
`otherwindow'	Show new buffer in another window (same frame)
`otherframe'	Show new buffer in another frame
`maybe-frame'	If a buffer is visible in another frame, prompt to ask if you
		you want to see the buffer in the same window of the current
  		frame or in the other frame.
`always-frame'   If a buffer is visible in another frame, raise that
		frame.  Otherwise, visit the buffer in the same window.")

(defvar iswitchb-regexp nil
  "*Non-nil means that iswitchb will do regexp matching.  Value can be
toggled within iswitchb.")

(defvar iswitchb-newbuffer t
  "*Non-nil means create new buffer if no buffer matches substring.
See also `iswitchb-prompt-newbuffer'.")

(defvar iswitchb-prompt-newbuffer t
  "*Non-nil means prompt user to confirm before creating new buffer.
See also `iswitchb-newbuffer'.")

(defvar iswitchb-define-mode-map-hook  nil
  "*Hook to define keys in `iswitchb-mode-map' for extra keybindings.")

;;; THINGS TO DO / BUGS

;; In Xemacs, the default buffer is not shown the first time you enter
; the minibuffer, but if you type a char and then delete a char, the
; default appears.  The first time we enter the minibuffer in XEmacs,
; the default msg is not displayed, presumably because the hook is not
; being called.  I have put in a temporary hack therefore at the
; bottom of this file.  
;
; There is also a problem with the backspace key in XEmacs, so I have
; bound it to the normal backward-delete-char.

;; iswitch-buffer features Not yet implemented:
;  C-f Quit iswitch and drop into find-file

;; Changing the list of buffers.
; By default, the list of current buffers is most recent first, oldest
; last, with the exception that the buffers visible in the current
; frame are put at the end of the list.  Maybe other rules could be
; added to further sort the list of buffers (eg putting INBOX SUMMARY
; after INBOX).  Maybe a hook could be added for users to add their
; code to further sort iswitchb-buflist?

;; Do we need the variable iswitchb-use-mycompletion?


;;; Internal Variables
(defvar iswitchb-minibuffer-setup-hook nil
  "*Iswitchb-specific customization of minibuffer setup.

This hook is run during minibuffer setup iff iswitchb will be active.
It is intended for use in customizing iswitchb for interoperation
with other packages.  For instance:

  \(add-hook 'iswitchb-minibuffer-setup-hook 
	    \(function
	     \(lambda ()
	       \(make-local-variable 'resize-minibuffer-window-max-height)
	       \(setq resize-minibuffer-window-max-height 3))))

will constrain rsz-mini to a maximum minibuffer height of 3 lines when
iswitchb is running.  Copied from icomplete-minibuffer-setup-hook")

(defvar iswitchb-eoinput 1
  "Point where minibuffer input ends and completion info begins.
Copied from icomplete-eoinput.")
(make-variable-buffer-local 'iswitchb-eoinput)


(defvar iswitchb-buflist nil
  "Stores the current list of buffers that will be searched through.
The list is ordered, so that the most recent buffers come first,
although by default, the buffers visible in the current frame are put
at the end of the list.  Created by `iswitchb-make-buflist'.")

;; todo -- is this necessary?

(defvar iswitchb-use-mycompletion nil
  "Non-nil means use iswitchb completion feedback.  Should only be set
to t by iswitchb functions, so that it doesnt interfere with other
minibuffer usage.")

(defvar iswitchb-method nil
  "Stores the method for viewing the selected buffer.  Its value is
one of `samewindow', `otherwindow', `otherframe', `maybe-frame' or
`always-frame`.  See `iswitchb-default-method' for details of
values.")

(defvar iswitchb-change-word-sub nil 
  "Private variable used by `iswitchb-word-matching-substring'.")


(defvar iswitchb-common-match-string  nil
  "Stores the string that is common to all matching buffers.")


(defvar iswitchb-rescan nil
  "Non-nil means we need to regenerate the list of matching buffers.")

(defvar iswitchb-text nil
  "Stores the users string as it is typed in.")

(defvar iswitchb-matches nil
  "List of buffers currenly matching `iswitchb-text'.")

(defvar iswitchb-default-buffer nil
  "Default buffer to switch to.")

(defvar iswitchb-mode-map nil
  "Keymap for iswitchb.")

(defvar  iswitchb-history nil
  "History of buffers selected using iswitchb.")

(defvar iswitchb-exit nil 
  "Flag to monitor how iswitchb exits.  If equal to `takeprompt', we
use the prompt as the buffer name to be selected.")

(defvar iswitchb-buffer-ignore-orig nil
  "Stores original value of `iswitchb-buffer-ignore'.")

;;; FUNCTIONS


;;; ISWITCHB KEYMAP 
(defun iswitchb-define-mode-map ()
  "Set up the keymap for iswitchb."
  (interactive)
  (let (map)
    ;; generated every time so that it can inheret new functions.
    ;;(or iswitchb-mode-map

    (setq map (copy-keymap minibuffer-local-map))
	
    (define-key map "\C-s" 'iswitchb-next-match)
    (define-key map "\C-r" 'iswitchb-prev-match)
    (define-key map "\t" 'iswitchb-complete)
    (define-key map "\C-j" 'iswitchb-select-buffer-text)
    (define-key map "\C-t" 'iswitchb-toggle-regexp)
    (define-key map "\C-a" 'iswitchb-toggle-ignore)
    (define-key map "\C-c" 'iswitchb-toggle-case)
    (setq iswitchb-mode-map map)
    (run-hooks 'iswitchb-define-mode-map-hook)
    ))
  


;;; MAIN FUNCTION
(defun iswitchb ()
  "Switch to buffer matching a substring.
As you type in a string, all of the buffers matching the string are
displayed.  When you have found the buffer you want, it can then be
selected.  As you type, most keys have their normal keybindings,
except for the following:
\\<iswitchb-mode-map>

RET Select the buffer at the front of the list of matches.  If the
list is emptty, possibly prompt to create new buffer.

\\[iswitchb-select-buffer-text] Select the current prompt as the buffer.
If no buffer is found, prompt for a new one.

\\[iswitchb-next-match] Put the first element at the end of the list.
\\[iswitchb-prev-match] Put the last element at the start of the list.
\\[iswitchb-complete] Complete a common suffix to the current string that 
matches all buffers.  If there is only one match, select that buffer.
\\[iswitchb-toggle-regexp] Toggle rexep searching.
\\[iswitchb-toggle-ignore] Toggle ignoring certain buffers (see \
`iswitchb-buffer-ignore')
\\[iswitchb-toggle-case] Toggle case-sensitive searching of buffer names.
"
  (interactive)
  (let
      (
       prompt
       buf-sel
       iswitchb-final-text
       (icomplete-mode nil) ;; prevent icomplete starting up
       (minibuffer-local-completion-map minibuffer-local-completion-map))
    
    (iswitchb-define-mode-map)
    (setq minibuffer-local-completion-map iswitchb-mode-map)
    
    (setq iswitchb-exit nil)
    (setq iswitchb-rescan t)
    (setq iswitchb-text "")
    (setq iswitchb-matches nil)
    (setq iswitchb-default-buffer (buffer-name (other-buffer)))
    (setq prompt (format "iswitch "))
    (iswitchb-make-buflist)

    ;; prompt the user for the buffer name
    (setq iswitchb-final-text (completing-read prompt
					       ;;nil
					       '(("dummy".1))
					       ;;("2".2)  ("3".3))
					       nil nil
					       nil;init string
					       'iswitchb-history))
    
    ;; Choose the buffer name: either the text typed in, or the head
    ;; of the list of matches
    (if (or 
	 (eq iswitchb-exit 'takeprompt)
	 (null iswitchb-matches))
	(setq buf-sel iswitchb-final-text)
      ;; else take head of list
      (setq buf-sel (car iswitchb-matches)))
    
    ;; Or possibly choose the default buffer
    (if  (equal iswitchb-final-text "")
	(setq buf-sel iswitchb-default-buffer))
    
    ;; View the buffer
    (message "go to buf %s" buf-sel)
    
    (if (get-buffer buf-sel)
	;; buffer exists, so view it and then exit
	(iswitchb-visit-buffer buf-sel)
      ;; else buffer doesnt exist
      (iswitchb-possible-new-buffer buf-sel))
    
    ))


;;; COMPLETION CODE

(defun iswitchb-set-common-completion  ()
  "Find common completion of `iswitchb-text' in `iswitchb-matches'.  The
result is stored in `iswitchb-common-match-string'."

  (let* (val)
    (setq  iswitchb-common-match-string nil)
    (if (and iswitchb-matches
             (stringp iswitchb-text)
             (> (length iswitchb-text) 0))
        (if (setq val (iswitchb-find-common-substring
                       iswitchb-matches iswitchb-text))
            (setq iswitchb-common-match-string val)))
    val
    ))


(defun iswitchb-complete ()
  "Try and complete the current pattern amongst the buffer names."
  (interactive)
  (let (res)
    (cond ((not  iswitchb-matches)

	   (message "No buffer completions.")
	   (sit-for 0.3)
	   )
	  
	  ((eq 1 (length iswitchb-matches))
	   ;; only one choice, so select it.
	   (exit-minibuffer))
	  
	  (t
	   ;; else there could be some completions
	   
	   (setq res (iswitchb-find-common-substring
		      iswitchb-matches iswitchb-text))
	   (if (not (memq res '(t nil)))
	       ;; found something to complete, so put it in the minibuff.
	       (progn
		 (setq iswitchb-rescan nil)
		 (delete-region (point-min) (point))
		 (insert  res)))
	   )
	  )))



;;; TOGGLE FUNCTIONS

(defun iswitchb-toggle-case ()
  "Toggle the value of `iswitchb-case'."
  (interactive)
  (setq iswitchb-case (not iswitchb-case))
  ;; ask for list to be regenerated.
  (setq iswitchb-rescan t)
  )

(defun iswitchb-toggle-regexp ()
  "Toggle the value of `iswitchb-regexp'."
  (interactive)
  (setq iswitchb-regexp (not iswitchb-regexp))
  ;; ask for list to be regenerated.
  (setq iswitchb-rescan t)
  )


(defun iswitchb-toggle-ignore ()
  "Toggle ignoring buffers specified with `iswitchb-buffer-ignore'."
  (interactive)
  (if iswitchb-buffer-ignore
      (progn
        (setq iswitchb-buffer-ignore-orig iswitchb-buffer-ignore)
        (setq iswitchb-buffer-ignore nil)
        )
    ;; else
    (setq iswitchb-buffer-ignore iswitchb-buffer-ignore-orig)
    )
  ;; ask for list to be regenerated.
  (setq iswitchb-rescan t)
  )


(defun iswitchb-select-buffer-text ()
  "Select the buffer named by the prompt.  If no buffer exactly
matching the prompt exists, a new one is possibly created."
  (interactive)
  (setq iswitchb-exit 'takeprompt)
  (exit-minibuffer))


(defun iswitchb-next-match () 
  "Put first element of `iswitchb-matches' at the end of the list."
  (interactive)
  (let ((tmp  (car iswitchb-matches)))
    (setq iswitchb-matches (cdr iswitchb-matches))
    (setq iswitchb-matches (append iswitchb-matches (list tmp)))
    (setq iswitchb-rescan nil)
    ))

(defun iswitchb-prev-match () 
  "Put last element of `iswitchb-matches' at the front of the list."
  (interactive)
  (setq iswitchb-matches (iswitchb-rotate-list iswitchb-matches))
  (setq iswitchb-rescan nil)
  )




;;; CREATE LIST OF ALL CURRENT BUFFERS

(defun iswitchb-make-buflist ()
  "Set `iswitchb-buflist' to the current list of buffers.  Buffers
that are currently visible are put at the end of the list."

  (setq iswitchb-buflist 
	(let (buflist
	      iswitchb-current-buffers)
	  (setq iswitchb-current-buffers (iswitchb-get-buffers-in-frame))
	  (setq buflist (mapcar 'buffer-name (buffer-list)))
	  (mapcar 'iswitchb-to-end iswitchb-current-buffers)
	  buflist)))


(defun iswitchb-to-end (elem)
  "Move ELEM to the end of BUFLIST."
  (setq buflist (delq elem buflist))
  ;;(message "removing %s" elem)
  (setq buflist (append buflist (list elem))))
		    



(defun iswitchb-get-buffers-in-frame ()
  "Return the list of buffers that are visible in the current frame."
  (let ((iswitchb-bufs-in-frame nil))
    
    (walk-windows 'iswitchb-get-bufname)
    iswitchb-bufs-in-frame))


(defun iswitchb-get-bufname (win)
  "Used by `iswitchb-get-buffers-in-frame' to walk through all windows."
  (setq iswitchb-bufs-in-frame
	(cons (buffer-name (window-buffer win))
	      iswitchb-bufs-in-frame)))


;;; FIND MATCHING BUFFERS

(defun iswitchb-set-matches ()
  "Set `iswitchb-matches' to the list of buffers matching prompt."

  (if iswitchb-rescan
      (setq iswitchb-matches
	    (let* ((buflist iswitchb-buflist)
		   )
	      (if (> (length iswitchb-text) 0)
		  (iswitchb-get-matched-buffers iswitchb-text iswitchb-regexp
						buflist)
		;; else no text, no matches
		nil)))))

(defun iswitchb-get-matched-buffers
  (regexp &optional string-format buffer-list)
  "Return matched buffers.  If STRING-FORMAT is non-nil, consider
REGEXP as string.  BUFFER-LIST can be list of buffers or list of
strings."
  
  (let* ((case-fold-search  iswitchb-case)
	 ;; need reverse since we are building up list backwards
	 (list              (reverse buffer-list))
         (do-string          (stringp (car list)))
         name
         ret
         )
    (mapcar
     (function
      (lambda (x)

        (if do-string
            (setq name x)               ;We already have the name
          (setq name (buffer-name x)))

        (cond
         ((and (or (and string-format (string-match regexp name))
                   (and (null string-format)
                        (string-match (regexp-quote regexp) name)))
               (not (iswitchb-ignore-buffername-p name)))
          (setq ret (cons name ret))
          ))))
     list)
    ret
    ))




(defun iswitchb-ignore-buffername-p (bufname)
  "Return t if the buffer BUFNAME should be ignored."
  (let ((data       (match-data))
        (re-list    iswitchb-buffer-ignore)
        ignorep
        nextstr
        )
    (while re-list
      (setq nextstr (car re-list))
      (cond
       ((stringp nextstr)
        (if (string-match nextstr bufname)
            (progn
              (setq ignorep t)
              (setq re-list nil))))
       ((fboundp nextstr)
        (if (funcall nextstr bufname)
            (progn
              (setq ignorep t)
              (setq re-list nil))
          ))
       )
      (setq re-list (cdr re-list)))
    (store-match-data data)

    ;; return the result
    ignorep)
  )



(defun iswitchb-word-matching-substring (word)
  "Return part of WORD before 1st match to `iswitchb-change-word-sub'.
If `iswitchb-change-word-sub' cannot be found in WORD, return nil."
  (let ((case-fold-search iswitchb-case)) 
    (let ((m (string-match iswitchb-change-word-sub word)))
      (if m
          (substring word m)
        ;; else no match
        nil))))






(defun iswitchb-find-common-substring (lis subs)
  "Return common string following SUBS in each element of LIS."
  (let (res
        alist
        iswitchb-change-word-sub
        )
    (setq iswitchb-change-word-sub
          (if iswitchb-regexp
              subs
            (regexp-quote subs)))
    (setq res (mapcar 'iswitchb-word-matching-substring lis))
    (setq res (delq nil res)) ;; remove any nil elements (shouldnt happen)
    (setq alist (mapcar 'iswitchb-makealist res)) ;; could use an  OBARRAY

    ;; try-completion returns t if there is an exact match.
    (let ((completion-ignore-case iswitchb-case))

    (try-completion subs alist)   
    )))


(defun iswitchb-makealist (res)
  "Return dotted pair (RES . 1)."
  (cons res 1))

;; from Wayne Mesard <wmesard@esd.sgi.com>
(defun iswitchb-rotate-list (lis)
  "Destructively removes the last element from LIS.
Return the modified list with the last element prepended to it."
  (if (<= (length lis) 1)
      lis
    (let ((las lis)
          (prev lis))
      (while (consp (cdr las))
        (setq prev las
              las (cdr las)))
      (setcdr prev nil)
      (cons (car las) lis))
    ))


;;; VISIT CHOSEN BUFFER
(defun iswitchb-visit-buffer (buffer)
  "Visit buffer named BUFFER according to `iswitchb-method'."
  (let* (win)
    (cond
     ((eq iswitchb-method 'samewindow)
      (switch-to-buffer buffer))

     ((memq iswitchb-method '(always-frame maybe-frame))
      (cond
       ((and (setq win (iswitchb-window-buffer-p buffer))
	     (or (eq iswitchb-method 'always-frame)
		 (y-or-n-p "Jump to frame? ")))
	(raise-frame (select-frame (window-frame win)))
	(select-window win)
	)
       (t
	;;  No buffer in other frames...
	(switch-to-buffer buffer)
	)))



     ((eq iswitchb-method 'otherwindow)
      (switch-to-buffer-other-window buffer))

     ((eq iswitchb-method 'otherframe)
      (switch-to-buffer-other-frame buffer))
     )
    ))

(defun iswitchb-possible-new-buffer (buf)
  "Possibly create and visit a new buffer called BUF."

  (let ((newbufcreated))
    (if (and iswitchb-newbuffer
	     (or
	      (not iswitchb-prompt-newbuffer)
	      
	      (and iswitchb-prompt-newbuffer
		   (y-or-n-p
		    (format
		     "No buffer matching `%s', create one? "
		     buf)))))
	;; then create a new buffer
	(progn
	  (setq newbufcreated (get-buffer-create buf))
	  (if (fboundp 'set-buffer-major-mode)
	      (set-buffer-major-mode newbufcreated))
	  (iswitchb-visit-buffer newbufcreated))
      ;; else wont create new buffer
      (message (format "no buffer matching `%s'" buf))
      )))

(defun iswitchb-window-buffer-p  (buffer)
  "Return window pointer if BUFFER is visible in another frame.  If
BUFFER is visible in the current frame, return nil."

  (interactive)

  (let ((blist (iswitchb-get-buffers-in-frame)))
    ;;If the buffer is visible in current frame, return nil
    (if (memq buffer blist)
	nil
      ;;  maybe in other frame...
      (get-buffer-window buffer 'visible)
      )))

;;; KEYBINDINGS AND TOP LEVEL FUNCTIONS.
(defun iswitchb-default-keybindings ()
  "Set up default keybindings for iswitchb.  
Call this function to override the normal bindings."
  (interactive)
  (global-set-key "b" 'iswitchb-buffer)
  (global-set-key "4b" 'iswitchb-buffer-other-window)
  (global-set-key "5b" 'iswitchb-buffer-other-frame))



;;;###autoload
(defun iswitchb-buffer ()
  "Switch to another buffer.

The buffer name is selected interactively by typing a substring.  The
buffer is displayed according to `iswitchb-default-method' -- the
default is to show it in the same window, unless it is already visible
in another frame.  For details of keybindings, do `C-h f
iswitchb-mode'."

  (interactive)
  (setq iswitchb-method iswitchb-default-method)
  (iswitchb-entry))


;;;###autoload
(defun iswitchb-buffer-other-window ()
  "Switch to another buffer and show it in another window.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `C-h f iswitchb-mode'."
  (interactive)
  (setq iswitchb-method 'otherwindow)
  (iswitchb-entry))



;;;###autoload
(defun iswitchb-buffer-other-frame ()
  "Switch to another buffer and show it in another frame.
The buffer name is selected interactively by typing a substring.
For details of keybindings, do `C-h f iswitchb-mode'."
  (interactive)
  (setq iswitchb-method 'otherframe)
  (iswitchb-entry))



(defun iswitchb-entry ()
  "Simply fall into iswitchb -- the main function."
  (interactive)
  (iswitchb))





;;; XEMACS HACK FOR SHOWING DEFAULT BUFFER

;; The first time we enter the minibuffer, Emacs puts up the default
;; buffer to switch to, but XEmacs doesnt -- presumably there is a
;; subtle difference in the two, either in icomplete or somewhere
;; else.  The default is shown for both whenever we delete all of our
;; text though, indicating its just a problem the first time we enter
;; the function.  To solve this, we use another entry hook for emacs
;; to show the default the first time we enter the minibuffer.

(defun iswitchb-init-Xemacs-trick ()
  "Display default buffer when first entering minibuffer.  This is a
hack for XEmacs, and should really be handled by iswitchb-exhibit."
  (if (iswitchb-entryfn-p)
      (progn
	(iswitchb-show-default-buffer)
	(goto-char (point-min)))))

;; add this hook for Xemacs only.
(if (string-match "XEmacs" (emacs-version))
    (add-hook 'iswitchb-minibuffer-setup-hook 
	      'iswitchb-init-Xemacs-trick))


;;; XEMACS / BACKSPACE key
;; For some reason, if the backspace key is pressed in xemacs, the
;; line gets confused, so I've added a simple key definition to make
;; backspace act like the normal delete key.

(defun iswitchb-xemacs-backspacekey ()
  "Bind backspace to `backward-delete-char'."
  (define-key iswitchb-mode-map '[backspace] 'backward-delete-char))


(if (string-match "XEmacs" (emacs-version))
    (add-hook 'iswitchb-define-mode-map-hook 
	      'iswitchb-xemacs-backspacekey))



;;; ICOMPLETE TYPE CODE

(defun iswitchb-exhibit ()
  "Find matching buffers and display them in the minibuffer.
Copied from `icomplete-exhibit' with two changes:
1. It prints a default buffer name when there is no text yet entered.
2. It calls my completion routine rather than the standard completion."

  (if iswitchb-use-mycompletion
      (let ((contents (buffer-substring (point-min)(point-max)))
	    (buffer-undo-list t))
	(save-excursion
	  (goto-char (point-max))
                                        ; Register the end of input, so we
                                        ; know where the extra stuff
                                        ; (match-status info) begins:
	  (if (not (boundp 'iswitchb-eoinput))
	      ;; In case it got wiped out by major mode business:
	      (make-local-variable 'iswitchb-eoinput))
	  (setq iswitchb-eoinput (point))
	  ;; Update the list of matches
	  (setq iswitchb-text contents)
	  (iswitchb-set-matches)
	  (setq iswitchb-rescan t)
	  (iswitchb-set-common-completion)

	  ;; Insert the match-status information:
	  (if (> (point-max) 1)
	      (insert-string
	       (iswitchb-completions 
		contents
		minibuffer-completion-table
		minibuffer-completion-predicate
		(not minibuffer-completion-confirm)))
	    ;; else put in default
	    (iswitchb-show-default-buffer))
	  ))))

(defun iswitchb-show-default-buffer ()
  "Insert the default buffer to switch to."
  (insert (format " {default %s}" iswitchb-default-buffer)))

(defun iswitchb-completions
  (name candidates predicate require-match)
  "Return the string that is displayed after the user's text.
Modified from `icomplete-completions'."
  
  (let ((comps iswitchb-matches)
                                        ; "-determined" - only one candidate
        (open-bracket-determined (if require-match "(" "["))
        (close-bracket-determined (if require-match ")" "]"))
                                        ;"-prospects" - more than one candidate
        (open-bracket-prospects "{")
        (close-bracket-prospects "}")
        )

    (cond ((null comps) (format " %sNo match%s"
				open-bracket-determined
				close-bracket-determined))

	  ((null (cdr comps))		;one match
	   (concat (if (and (> (length (car comps))
			       (length name)))
		       (concat open-bracket-determined
			       ;; when there is one match, show the 
			       ;; matching buffer name in full
			       (car comps)
			       close-bracket-determined)
		     "")
		   " [Matched]"))
	  (t				;multiple matches
	   (let* (
		  ;;(most (try-completion name candidates predicate))
		  (most nil)
		  (most-len (length most))
		  most-is-exact
		  (alternatives
		   (apply
		    (function concat)
		    (cdr (apply
			  (function nconc)
			  (mapcar '(lambda (com)
				     (if (= (length com) most-len)
					 ;; Most is one exact match,
					 ;; note that and leave out
					 ;; for later indication:
					 (progn
					   (setq most-is-exact t)
					   ())
				       (list ","
					     (substring com
							most-len))))
				  comps))))))
	     (concat

	      ;; put in common completion item -- what you get by
	      ;; pressing tab
	      (if (> (length iswitchb-common-match-string) (length name))
		  (concat open-bracket-determined
			  (substring iswitchb-common-match-string 
				     (length name))
			  close-bracket-determined)
		)
	      ;; end of partial matches...

	      ;; think this bit can be ignored.
	      (and (> most-len (length name))
		   (concat open-bracket-determined
			   (substring most (length name))
			   close-bracket-determined))
	      
	      ;; list all alternatives
	      open-bracket-prospects
	      (if most-is-exact
		  (concat "," alternatives)
		alternatives)
	      close-bracket-prospects)))
	  )))

(defun iswitchb-minibuffer-setup ()
  "Set up minibuffer for iswitchb.  Copied from
`icomplete-minibuffer-setup-hook'."
  (if (iswitchb-entryfn-p)
      (progn

	(make-local-variable 'iswitchb-use-mycompletion)
	(setq iswitchb-use-mycompletion t)
	(make-local-hook 'pre-command-hook)
	(add-hook 'pre-command-hook
		  'iswitchb-pre-command
		  nil t)
	(make-local-hook 'post-command-hook)
	(add-hook 'post-command-hook
		  'iswitchb-post-command
		  nil t)
	
	(run-hooks 'iswitchb-minibuffer-setup-hook) 
	)
    ))


(defun iswitchb-pre-command ()
  "Run before command in iswitchb."
  (iswitchb-tidy))


(defun iswitchb-post-command ()
  "Run after command in iswitchb."
  (iswitchb-exhibit)
  )



(defun iswitchb-tidy ()
  "Remove completions display, if any, prior to new user input.
Copied from `icomplete-tidy'."

  (if (and (boundp 'iswitchb-eoinput)
	   iswitchb-eoinput)
      
      (if (> iswitchb-eoinput (point-max))
	  ;; Oops, got rug pulled out from under us - reinit:
	  (setq iswitchb-eoinput (point-max))
	(let ((buffer-undo-list buffer-undo-list )) ; prevent entry
	  (delete-region iswitchb-eoinput (point-max))))
    
    ;; Reestablish the local variable 'cause minibuffer-setup is weird:
    (make-local-variable 'iswitchb-eoinput)
    (setq iswitchb-eoinput 1)))


(defun iswitchb-entryfn-p ()
  "Return non-nil if `this-command' shows we are using iswitchb-buffer."
  (member (symbol-name this-command)
	  '("iswitchb-buffer"
	    "iswitchb-buffer-other-frame"
	    "iswitchb-buffer-other-window")))
  


;;; HOOKS
(add-hook 'minibuffer-setup-hook 'iswitchb-minibuffer-setup)

(provide 'iswitchb)

;;; ISWITCHB.EL ends here
 

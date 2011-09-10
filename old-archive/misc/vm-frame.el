;;
;; vm-frame.el: Support for FSF GNU Emacs 19 Frames for VM (View Mail)
;;
;; Copyright (C) 1993  Paul D. Smith <psmith@wellfleet.com>
;;
;; This package is written for VM versions 5.38 to 5.40: earlier
;; versions won't completely work.
;;
;; Later versions of VM are supported by wim-vm, available from the same
;; place you got this file.  Note that win-vm is just a renamed version
;; of this package (Kyle wanted me to not use the vm-* namespace), still
;; maintained, authored, etc. by me.  win-vm supports VM 5.46 and above.
;;
;; This package has only been tested with FSF Emacs 19.16 and above.
;; Lower versions of FSF Emacs definitely won't work.  It won't work
;; with Lucid Emacs either; get Jamie Zawinski's vm-lucid.el instead.
;;
;; VM (View Mail) is a very cool mail handling package for GNU Emacs.
;; VM is written and maintained by Kyle E. Jones <kyle@wonderworks.com>.
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
;; You didn't receive a copy of the GNU General Public License along
;; with this program; so, write to the Free Software Foundation, Inc.,
;; 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; LCD Archive Entry:
;;; vm-frame|Paul D. Smith|psmith@wellfleet.com|
;;; Support for multiple FSF GNU Emacs 19 frames with VM and VM-mail.|
;;; 14-Jan-1994|1.8|~/misc/vm-frame.el.Z|
;;
;; HISTORY:
;;   1.08 - 14 Jan 1994 psmith
;;   1.07 - 07 Jan 1994 psmith
;;   1.06 - 16 Dec 1993 psmith
;;   1.05 - 15 Dec 1993 psmith
;;   1.04 - 20 Aug 1993 psmith
;;   1.03 - 13 Jul 1993 psmith
;;   1.02 - 07 Jul 1993 psmith
;;   1.01 - 24 Jun 1993 psmith
;;   1.00:  Created: 22 June 1993 by Paul D. Smith <psmith@wellfleet.com>
;;
;;;----------------------------------------------------------------------------
;;
;; INSTALLATION:
;;
;;   Replace the current VM autoloads in your .emacs with this:
;;
;;      (let ((my-vm-pkg (if window-system "vm-frame" "vm")))
;;        (autoload 'vm my-vm-pkg "Read and send mail with View Mail." t)
;;        (autoload 'vm-mode my-vm-pkg "Read and send mail with View Mail." t)
;;        (autoload 'vm-mail my-vm-pkg "Send mail with View Mail." t)
;;        (autoload 'vm-visit-folder my-vm-pkg))
;;
;;   and put this elisp file somewhere on your load-path.
;;
;;   Be SURE to remove all current VM autoloads, requires, etc.!  If you
;;   don't, weird things might happen!
;;
;;   NOTE: If you're using (setq mail-setup-hook ...) to insert personal
;;   hooks into mail-setup-hook instead of (add-hook 'mail-setup-hook ...),
;;   you must either change your .emacs to use add-hook (yay!) or add a
;;   call to (vm-frame-mail-setup) to your personal mail-setup-hook
;;   function (boo!)
;;
;;;----------------------------------------------------------------------------
;;
;; USAGE:
;;
;; General:
;; --------
;;
;;  Check the user-settable variables section below for configuration
;;  options, including control of when frames are popped up (no frames,
;;  for sending mail only, one frame for all of VM, 
;; Reading Mail:
;; -------------
;;
;;  Just use VM normally.  When you first invoke VM, a new Emacs frame
;;  will appear to hold it.  There's one frame for all of VM; visit
;;  folders, etc. in that buffer.
;;
;;  Don't quit VM when you're done, iconify the frame instead.  Only
;;  quit VM when you want to quit your Emacs.  Note you might want to
;;  get into the habit of saving your INBOX occasionally, just in case.
;;  And, the "i" key has been rebound to call vm-frame-iconify, which
;;  saves then iconifies the VM frame; a handy way to "put away" your VM
;;  frame.
;;
;;  When you get new mail or want to use VM again, you can enter VM as
;;  before (M-x vm or whatever); if the VM frame already exists it
;;  will be made visible and raised to the top, and the primary inbox
;;  will be visited.
;;
;;  Quitting out of the INBOX folder causes the VM frame to be deleted.
;;  Quitting out of other folders *doesn't*.
;;
;;  Almost all common VM functions can be accessed through the new
;;  pull-down menus:
;;
;;    Visit     Similar to Emacs' "Buffers" menu: shows a list of VM
;;              folders you can visit.
;;
;;    Folder    Folder-specific commands
;;
;;    Message   Message-specific commands
;;
;;    Send      Commands for sending mail (Reply, Forward, etc.)
;;
;; Mouse Support:
;; --------------
;;
;;  The following mouse commands are supported in VM Summary windows:
;;
;;      mouse-2:        Select the message clicked on.
;;      double-mouse-2: Select and read the message clicked on.
;;
;;      mouse-3:        Select the message and show a pop-up menu of
;;                      commands you can run on that message.
;;
;;  If the message buffer is visible already, then if you have
;;  vm-preview-lines set to non-nil, mouse-2 will show you the message
;;  preview and double-mouse-2 will show the entire message.  If
;;  vm-preview-lines is set to nil, then mouse-2 and double-mouse-2 will
;;  be equivalent once the message buffer is visible.
;;
;;  I'm actively searching for mouse keybindings for VM Summary mode; if
;;  you have a suggestion for more bindings for mouse keys, please let
;;  me know!
;;
;; Sending Mail:
;; -------------
;;
;;  When sending mail, use the normal VM commands for replies,
;;  followups, forwards, etc.  Use vm-mail to send original mail.
;;
;;  All mail messages invoked through VM will appear in their own
;;  frame.  The title of the frame will be the same as the name of the
;;  VM mail buffer.
;;
;;  To send the message, select "Send Message" from the "Mail" menu, or
;;  use C-c C-c.  This sends the message, buries the message buffer, and
;;  deletes the message frame.  Sending without deleting ("Send, Keep
;;  Editing" from the "Mail" menu, or C-c C-s) doesn't delete the frame.
;;
;;  To kill a message without sending it, select "Kill Message" from
;;  the "Mail" menu, or use C-x k.  This is usually kill-buffer; in
;;  VM-mail buffers it's rebound to confirm, then kill the buffer *and*
;;  delete the message frame.
;;
;;;----------------------------------------------------------------------------
;;
;; History/Acknowledgments:
;;
;;;----------------------------------------------------------------------------
;;
;; 1.08:
;;
;; * Removed unneeded function vm-frame-mark-message.
;;
;; * Fixed vm-frame-choose-folder to properly handle the case where
;;   vm-folder-directory is nil.  Also Uwe Bonnes
;;   <bon@LTE.E-TECHNIK.uni-erlangen.de> reported a problem which turned
;;   out to bite people who's vm-primary-inbox wasn't in their
;;   vm-folder-directory, and had subdirectories.  Basically reworked
;;   the whole vm-frame-choose-folder algorithm.
;;
;; * By request of Greg Thompson <gregt@porsche.visix.COM>, modified the
;;   vm advice so that if the VM frame already exists it just
;;   de-iconifies it, then runs vm-get-new-mail, rather than calling vm.
;;   Fixed this to work with multiple spool file scenarios as well.
;;
;; 1.07:
;;
;; * Put back the Buffers pull-down menu even in VM.
;;
;; * Added a check to handle empty folder directories more gracefully.
;;
;; * Fixed a bug where the Messages -> Save in Folder... pull-down
;;   didn't handle subdirectories correctly, like Visit does.
;;
;; * Corny de Souza <cdesouza@isoit154.bbn.hp.com> wanted a way to tell
;;   if folders in the Visit, etc. pull-downs were really subdirectories
;;   or not, so I added a postfix of "..." to all folders in the list
;;   which are subdirectories.
;;
;; 1.06:
;;
;; * From Uwe Bonnes <bon@LTE.E-TECHNIK.uni-erlangen.de>: Fixed "Reload
;;   Resources" menu item.
;;
;; * Siddarth Subramanian <siddarth@cs.utexas.edu> and others wanted the
;;   ability to create a new frame for each folder visited, so I added
;;   that.
;;
;;   I removed vm-frame-no-frame, and substituted vm-frame-style.  Set
;;   it to 'none for no new frames for VM; 'single (the default) for one
;;   frame for all VM folders, and 'all for a new frame for every VM
;;   folder.  Note doesn't create a new frame for virtual folders--I'm
;;   having trouble getting that to work :(
;;
;; * Added override of Kyle's new vm-summary-mode-map in 5.38; it's
;;   broken for what I need (it's a copy of vm-mode-map instead of a
;;   submap of vm-mode-map).
;;
;; * Added Folder menu options to sort the folder's presentation, and to
;;   physically sort it.
;;
;; 1.05:
;;
;; * Kyle added a bunch of hook functions to VM 5.36!  Hooray!
;;   Unfortunately none of the ones I need.  Boo.  Oh well; Kevin
;;   Broadey <KevinB@bartley.demon.co.uk> suggested looking into
;;   "advice", so I rewrote vm-frame to use advice instead of bogus
;;   fsets.
;;
;; * Removed the 'vm-frame function: just call vm directly.  VM 5.36
;;   seems to have fixed the problem which required this hack.
;;
;; * VM 5.36 removed the grouping functions, so I took out that menu
;;   item.
;;
;; * Changed the mouse-1 binding to mouse-2 to conform to a developing
;;   Emacs convention where mouse-2 is used to select from a list.  Also
;;   bound double-mouse-2 to show the message if it's not already shown.
;;   mouse-1 is left unmodified (allowing selection, etc.)
;;
;; * <ada@unison.com> suggested the idea for vm-frame-iconify.
;;
;; * Colin Owen Rafferty <craffert@lehman.com> suggested creating the
;;   pop-up menu for mouse-3.  He got the idea from vm-lucid.el by Jamie
;;   Zawinski <jwz@lucid.com>.
;;
;; 1.04:
;;
;; * This version is written for Emacs 19.17 and above; it may not work
;;   completely correctly on earlier versions of Emacs.
;;
;; * Graham Gough <graham@computer-science.manchester.ac.uk> keeps his
;;   VM folders in subdirectories of vm-folder-directory, and sent
;;   sample code for having the "Visit" menu pop up submenus if the
;;   selected folder is actually a directory.
;;
;; * Made vm-frame-mail-send-and-exit more bulletproof vis a vis
;;   deleting the frame: it should never delete any frame except the one
;;   it creates for mail.
;;
;; * Cengiz Alaettinoglu <ca@cs.umd.edu> contributed the basic code for
;;   adding the ability to have multiple panes for the "Visit" folders
;;   pulldown menu.
;;
;; 1.03:
;;
;; * Fixed mail-mode stuff so it peacefully coexists with the new
;;   mail-mode menu bars in Emacs 19.16.
;;
;; * Make the primary inbox always be first in the folders list
;;
;; * Fencepost the fset calls so it's safe to load vm-frame multiple
;;   times (I hope...)
;;
;; * Mike Chace <mikec@praxis.co.uk> suggested displaying the primary
;;   inbox in the Visit menu even if it isn't in vm-folder-directory.
;;
;; 1.02:
;;
;; * Created some initial keybindings for mouse clicks, along with helper
;;   functions.
;;
;; * Michael Kutzner <futzi@uni-paderborn.de> suggested frame creation
;;   and printer command configurability variables.
;;
;; * Chris Moore <Chris.Moore@src.bae.co.uk> suggested using (require 'vm),
;;   a fix for vm-frame-mail-kill in non-VM mail buffers, and a request
;;   that (vm) visit the primary inbox, which led me to develop the
;;   vm-frame function as an interactive alternative to (vm).
;;
;; * abraham@research.att.com (Per Abrahamsen) posted a generic fix to
;;   the menu bar items beeping if you posted them without selecting
;;   anything; I added his fix into my menu bar items.
;;
;; 1.00: Created.
;;
;;;----------------------------------------------------------------------------

;;;----------------------------------------------------------------------------
;;
;;			    Customize these:
;;
;;;----------------------------------------------------------------------------

;; This is a list of Frame parameters used to create the VM frame.
;; Modify it to whatever you like; try something like:
;;
;;  (setq vm-frame-alist '((top . -1) (left . -1)))
;;
(defvar vm-frame-alist nil
  "*Alist of frame parameters used for creating the VM frame.")


;; This is a list of Frame parameters for the newly created mail frames.
;; Modify it to whatever you like; try something like:
;;
;;  (setq vm-frame-mail-alist '((height . 40) (top . -1) (left . 0)))
;;
(defvar vm-frame-mail-alist nil
  "*Alist of frame parameters used for creating Mail frames.")


;; Set this to nil if you don't want to be asked if you want to quit VM
;;
(defvar vm-frame-confirm-quit t
  "*If t, asks if you want to quit the VM frame.  If nil, doesn't ask.")


;; Control how/if new frames are created; if you want cool pull-down
;; menus but don't want cool new frames, set these appropriately in your
;; .emacs
;;
(defvar vm-frame-style 'single
  "*Controls if and when new frames are created for VM folders.

If 'none, no frames are created for VM folders.
If 'single, one frame is created to hold all VM folders.
If 'all, a frame is created for each folder visited.")

(defvar vm-frame-no-mail-frame nil
  "*If t, doesn't create a new frame for outgoing VM mail messages.
If nil, makes a new frame.")


;; If you want more/less folders to show up in each pane of the "Visit"
;; folders menu, set this.  Setting to nil causes all folders to appear
;; in one pane.
;;
(defvar vm-frame-max-folders-per-pane 25 
  "*Number of folders to display in each pane of the Visit menu.")


;; How to print a mail message (note! only used by the "print" pull-down
;; menu)
;;
(defvar vm-frame-print-command lpr-command
  "*The command used to print VM mail messages.")


;; A few hooks--since I'm bummed at Kyle for not adding any to VM, I'll
;; try to be nice and add a few myself :)
;;
(defvar vm-frame-exit-folder-hook nil
  "Hooks to be called before a folder is exited.")

(defvar vm-frame-quit-vm-hook nil
  "Hooks to be called before VM is exited.
Note vm-frame-exit-folder-hook is also called first for the current folder.")


;; You might want to change this: this key runs a function which kills
;; the current mail buffer.  Normally I just used C-x k, but now we
;; have to delete the frame too, so... voila!  New function.
;;
(defun vm-frame-mail-setup ()
  (define-key mail-mode-map "\C-xk"   'vm-frame-mail-kill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;----------------------------------------------------------------------------
;;
;;		    Nothing to customize below here.
;;
;;;----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; What are we?
;;
(defconst vm-frame-version-number "1.08"
  "vm-frame's version number.")


;;;----------------------------------------------------------------------------
;;
;;		       Support for the VM frame.
;;
;;;----------------------------------------------------------------------------

;; Define vm-summary-mode-map so VM won't define it and we can set it to
;; be a submap of vm-mode-map later.
;;
(defvar vm-summary-mode-map nil
  "Keymap for VM Summary mode.  Inherits from vm-mode-map.")


;; We need to load this stuff first
;;
(require 'advice)
(require 'vm)


;; Holds VM's frame list (if it exists)
;;
(defvar vm-frame-frames nil)


;; vm-visit-folder calls vm to do most of its work; this variable is set
;; by my vm-visit-folder advice to be whatever frame info was discovered
;; by it, so my vm advice can use it.
;;
(defvar vm-frame-folder-info nil)


;; Find the matching frame for this folder; nil if no matching frame.
;;
(defun vm-frame-find-frame (folder)
  (if (eq vm-frame-style 'none)
      nil
    (assoc folder vm-frame-frames)))


;; Create and/or select the frame for this folder.  This function takes
;; a folder name, and creates a frame with that name if one doesn't
;; exist, or raises the frame if it does exist.
;;
;; It returns a structure like: (<NEW> . (<NAME> . <FRAME ID>))
;; where:
;;  NEW is t if this is a newly-created frame, or nil if it existed
;;      already.
;;  NAME is a text string containing the name of the frame.
;;  FRAME ID is the elisp frame object for the frame.
;;
(defun vm-frame-select-frame (folder)
  "Create and/or select and raise the VM frame for the VM folder FOLDER.
Default is the primary inbox."
  (interactive)
  (if (eq vm-frame-style 'none)
      nil
    (let* ((fname (expand-file-name
		   (if (or (not folder) (eq vm-frame-style 'single))
		       vm-primary-inbox
		     (if (bufferp folder)
			 (buffer-file-name folder)
		       folder))))
	   (frame (vm-frame-find-frame fname))
	   (new-frame (not (frame-live-p (cdr-safe frame)))))
      (if new-frame
	  (let ((fp (select-frame
		     (make-frame
		      (append vm-frame-alist
			      (list (cons 'name
					  (if (eq vm-frame-style 'single)
					      "VM"
					    (concat "VM: "
						    (file-name-nondirectory
						     fname))))))))))
	    (if frame
		(setcdr frame fp)
	      (setq vm-frame-frames
		    (append (list (cons fname fp)) vm-frame-frames))
	      (setq frame (car vm-frame-frames))))
	(select-frame (cdr frame))
	(make-frame-visible)
	(raise-frame (cdr frame)))
      (cons new-frame frame))))


;; Add some advice to entering VM: if the frame doesn't exist we create
;; it and process the folder, otherwise we just bring the frame to the
;; foreground and check for new mail.
;;
(defadvice vm (around vm-frame-vm first activate)
  "VM is started inside its own frame (vm-frame)."
  (let* ((fdata (or vm-frame-folder-info
		    (vm-frame-select-frame (ad-get-arg 0))))
	 (frame (cdr fdata)))
    (if (car fdata)
	ad-do-it
      (vm-get-new-mail))
    (and frame (setcar frame (if (eq vm-frame-style 'single)
				 (expand-file-name vm-primary-inbox)
			       (buffer-file-name vm-mail-buffer))))))

(defadvice vm-visit-folder (around vm-frame-vm-visit-folder first activate)
  "VM is started inside its own frame (vm-frame)."
  (let ((vm-frame-folder-info (vm-frame-select-frame (ad-get-arg 0))))
    ad-do-it))


;; Since we're creating a new frame we need to delete it when we quit.
;; However, since we're only creating one frame for all VM folders, we
;; only want to delete it when we quit the last folder.
;;
;; We don't quite accomplish that here; instead we delete the frame
;; whenever we quit from the INBOX buffer.  Oh well.
;;
(defadvice vm-quit (around vm-frame-vm-quit first activate)
  "Delete the VM frame if we're quitting from the user's primary inbox.
Also run vm-frame-exit-folder-hook (vm-frame)."
  (run-hooks 'vm-frame-exit-folder-hook)
  (let* ((fname (buffer-file-name vm-mail-buffer))
	 (main-p (string= fname (expand-file-name vm-primary-inbox)))
	 (frame (vm-frame-find-frame fname)))
    (if (and main-p
	     vm-frame-confirm-quit
	     (not (y-or-n-p "Do you really want to quit VM? ")))
	(error "Aborted"))
    ad-do-it
    (and main-p (run-hooks 'vm-frame-quit-vm-hook))
    (cond ((frame-live-p (cdr-safe frame))
	   (delete-frame (cdr frame))
	   (setcdr frame nil)))))


(defun vm-frame-iconify ()
  "Saves the current folder and iconifies the current frame."
  (interactive)
  (vm-save-folder)
  (and (frame-live-p (selected-frame))
       (iconify-frame (selected-frame))))

;; If we're in summary mode, this function determines the message number
;; of the message header point is on, if any.  Returns nil if none, or
;; the message number as an integer.
;;
(defun vm-frame-message-num ()
  (save-excursion
    (beginning-of-line)
    (and (eq major-mode 'vm-summary-mode)
	 (looking-at "\\(->\\)? *\\([0-9]+\\) ")
	 (string-to-int (buffer-substring (match-beginning 2)
					  (match-end 2))))))


;; If point is on a valid message number in the summary mode, goto that
;; message.  Basically, we set point then pretend
;; vm-follow-summary-cursor is t and let VM do the work.
;;
(defun vm-frame-goto-message (event)
  "Goto the message clicked on with the mouse."
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (let ((vm-follow-summary-cursor t))
    (vm-follow-summary-cursor)))


;; If the message buffer isn't visible, make it visible.
;;
(defun vm-frame-view-message ()
  "View the current message if it's not already visible."
  (interactive)
  (or (get-buffer-window vm-mail-buffer) (vm-scroll-forward)))


;; Make a vm-summary mode keymap and rebind the mouse keys
;;
(define-key vm-mode-map "i" 'vm-frame-iconify)
(define-key vm-mode-map [C-down-mouse-1] 'vm-frame-menu-bar-folders)

(setq vm-summary-mode-map (cons 'keymap vm-mode-map))

(define-key vm-summary-mode-map [mouse-2] 'vm-frame-goto-message)
(define-key vm-summary-mode-map [double-mouse-2] 'vm-frame-view-message)
(define-key vm-summary-mode-map [down-mouse-3] 'vm-frame-message-menu)


;; Set up the new menus
;;
(define-key vm-mode-map [menu-bar] (make-sparse-keymap "vm-menu-bar"))

(define-key vm-mode-map [menu-bar file] 'undefined)
(define-key vm-mode-map [menu-bar edit] 'undefined)

(define-key vm-mode-map [menu-bar send]
  (cons "Send" (make-sparse-keymap "Send")))
(define-key vm-mode-map [menu-bar message]
  (cons "Message" (make-sparse-keymap "Message")))
(define-key vm-mode-map [menu-bar folder]
  (cons "Folder" (make-sparse-keymap "Folder")))
(define-key vm-mode-map [menu-bar visit]
  '("Visit" . vm-frame-menu-bar-folders))

;; Folder menu
;;
(define-key vm-mode-map [menu-bar folder exit-no-save]
  '("Exit, no Save" . vm-quit-no-change))
(define-key vm-mode-map [menu-bar folder exit-folder]
  '("Exit Folder" . vm-quit))
(define-key vm-mode-map [menu-bar folder reload]
  '("Reload Resources" . vm-load-init-file))
(define-key vm-mode-map [menu-bar folder expunge]
  '("Expunge Folder" . vm-expunge-folder))
(define-key vm-mode-map [menu-bar folder sort-physically]
  '("Sort Physically By..." . vm-frame-sort-physically))
(define-key vm-mode-map [menu-bar folder sort]
  '("Sort Folder By..." . vm-frame-sort))
(define-key vm-mode-map [menu-bar folder save-iconify]
  '("Save & Iconify" . vm-frame-iconify))
(define-key vm-mode-map [menu-bar folder save]
  '("Save Folder" . vm-save-folder))
(define-key vm-mode-map [menu-bar folder summary]
  '("Summarize Folder" . vm-summarize))
(define-key vm-mode-map [menu-bar folder get]
  '("Get New Mail" . vm-get-new-mail))


;; Message menu
;;
(define-key vm-mode-map [menu-bar message burst]
  '("Burst Digest" . vm-burst-digest))
(define-key vm-mode-map [menu-bar message pipe]
  '("Pipe to..." . vm-pipe-message-to-command))
(define-key vm-mode-map [menu-bar message print]
  '("Print" . vm-frame-print))
(define-key vm-mode-map [menu-bar message command-mark]
  '("Use Marked" . vm-next-command-uses-marks))
(define-key vm-mode-map [menu-bar message unmark-all]
  '("Unmark All" . vm-clear-all-marks))
(define-key vm-mode-map [menu-bar message mark-all]
  '("Mark All" . vm-mark-all-messages))
(define-key vm-mode-map [menu-bar message unmark]
  '("Unmark" . vm-unmark-message))
(define-key vm-mode-map [menu-bar message mark]
  '("Mark" . vm-mark-message))
(define-key vm-mode-map [menu-bar message undelete]
  '("Un-Delete" . vm-undelete-message))
(define-key vm-mode-map [menu-bar message delete]
  '("Delete" . vm-delete-message))
(define-key vm-mode-map [menu-bar message toggle]
  '("Toggle Headers" . vm-expose-hidden-headers))
(define-key vm-mode-map [menu-bar message write]
  '("Write to File..." . vm-save-message-sans-headers))
(define-key vm-mode-map [menu-bar message save]
  '("Save in Folder..." . vm-frame-save-in-folder))

;; Send menu
;;
(define-key vm-mode-map [menu-bar send send-digest]
  '("Send Digest" . vm-send-digest))
(define-key vm-mode-map [menu-bar send resend]
  '("Resend Bounced" . vm-resend-bounced-message))
(define-key vm-mode-map [menu-bar send forward]
  '("Forward Message" . vm-forward-message))
(define-key vm-mode-map [menu-bar send followup-text]
  '("Followup with Text" . vm-followup-include-text))
(define-key vm-mode-map [menu-bar send followup]
  '("Followup to All" . vm-followup))
(define-key vm-mode-map [menu-bar send reply-text]
  '("Reply with Text" . vm-reply-include-text))
(define-key vm-mode-map [menu-bar send reply]
  '("Reply to Sender" . vm-reply))
(define-key vm-mode-map [menu-bar send mail]
  '("Send Message" . vm-mail))

;; Help menu
;;
(define-key vm-mode-map [menu-bar help vm-warranty]
  '("VM Warranty" . vm-show-no-warranty))
(define-key vm-mode-map [menu-bar help vm-copy]
  '("Copying VM" . vm-show-copying-restrictions))
(define-key vm-mode-map [menu-bar help vm-help]
  '("Help on VM" . vm-frame-help))

(defun vm-frame-help (event)
  "Displays VM help."
  (interactive "e")
  (let ((major-mode 'vm-mode))
    (describe-mode)))


;; Message pop-up menu
;;
(defun vm-frame-message-menu (event)
  "Select the message pointed to by the mouse and show a pop-up menu of commands."
  (interactive "e")
  (vm-frame-goto-message event)
  (let ((op (x-popup-menu event
			  '(""
			    ("Message"
			     ("Reply to Sender" . vm-reply)
			     ("Reply with Text" . vm-reply-include-text)
			     ("Followup to All" . vm-followup)
			     ("Followup with Text" . vm-followup-include-text)
			     ("Forward Message" . vm-forward-message)
			     ("Resend Bounced" . vm-resend-bounced-message)
			     ("Save in Folder..." . vm-frame-save-in-folder)
			     ("Write to File..." . vm-save-message-sans-headers)
			     ("Toggle Headers" . vm-expose-hidden-headers)
			     ("Delete" . vm-delete-message)
			     ("Un-Delete" . vm-undelete-message)
			     ("Mark" . vm-mark-message)
			     ("Unmark" . vm-unmark-message)
			     ("Print" . vm-frame-print)
			     ("Pipe to..." . vm-pipe-message-to-command)
			     ("Burst Digest" . vm-burst-digest))))))
    (and op (call-interactively op))))


;; Folders menu
;;
(defun vm-frame-choose-folder (event)
  "Pop up a menu of VM folders for selection with the mouse.
Returns the folder that you select."
  (interactive "e")
  (let* ((inbox (file-name-nondirectory vm-primary-inbox))
	 (head (and (string= vm-folder-directory
			     (expand-file-name
			      (file-name-directory vm-primary-inbox)))
		    (cons (cons inbox vm-primary-inbox) nil)))
	 (folders (directory-files vm-folder-directory nil "[^~]$"))
	 menu)
    (setq menu
	  (cons "Folder Menu"
		(let ((tail folders)
		      (exclude-re (concat "^\\(#.*\\|\\.\\.?\\|" inbox "\\)$"))
		      (i 1)
		      (panes nil))
		  (while tail
		    (let ((elt (car tail)))
		      (if (not (string-match exclude-re elt))
			  (progn
			    (setq head
				  (cons (cons
					 (if (file-directory-p 
					      (concat vm-folder-directory elt))
					     (concat elt "/")
					   elt)
					 elt) head)
				  i (1+ i))
			    (if (and vm-frame-max-folders-per-pane
				     (= i vm-frame-max-folders-per-pane))
				(setq panes (cons (cons "Select Folder" 
							(reverse head))
						  panes)
				      head nil
				      i 0)))))
		    (setq tail (cdr tail)))
		  (if head 
		      (setq panes (cons 
				   (cons "Select Folder" 
					 (reverse head)) panes)))
		  (nreverse panes))))
    (if (cdr menu)
	(let ((choice (x-popup-menu (if (listp event)
					event
				      (cons '(0 0) (selected-frame)))
				    menu)))
	  (and choice (expand-file-name choice vm-folder-directory)))
      (message "Empty folder directory!")
      nil)))

(defun vm-frame-get-folder (event fdir)
  (let ((folder
	 (let ((vm-folder-directory
		(file-name-as-directory
		 (expand-file-name
		  (or fdir vm-folder-directory default-directory)))))
	   (vm-frame-choose-folder event))))
    (if folder
	(if (file-accessible-directory-p folder)
	    (vm-frame-get-folder event (file-name-as-directory folder))
	  folder)
      nil)))

(defun vm-frame-menu-bar-folders (event)
  "Pop up a menu of VM folders for selection with the mouse.
This visits the folder that you select."
  (interactive "e")
  (let ((folder (vm-frame-get-folder event nil)))
    (if folder
	(vm-visit-folder folder))))

(defun vm-frame-save-in-folder (event)
  "Save message in a VM folder.  Pop up a menu of VM folders for selection."
  (interactive "e")
  (let ((folder (vm-frame-get-folder event nil)))
    (if folder
	(vm-save-message folder))))

;; Choose a sorting option
;;
(defun vm-frame-choose-sort (event)
  (let (menu)
    (setq menu
          (list "Sorting Menu"
                (cons "Select Sort Style"
                      (let ((tail vm-supported-sort-keys)
                            head)
                        (while tail
                          (let ((elt (car tail)))
                            (setq head (cons (cons elt elt) head)))
                          (setq tail (cdr tail)))
                        (reverse head)))))
    (x-popup-menu (if (listp event) event
		    (cons '(0 0) (selected-frame)))
		  menu)))

(defun vm-frame-sort (event)
  "Change the presentation order of messages in VM folders.
Doesn't change the physical ordering of the messages."
  (interactive "e")
  (let ((style (vm-frame-choose-sort event)))
    (and style (vm-sort-messages style nil))))

(defun vm-frame-sort-physically (event)
  "Change the physical ordering of messages in VM folders."
  (interactive "e")
  (let ((style (vm-frame-choose-sort event)))
    (and style (vm-sort-messages style t))))

;; Quick printing option
;;
(defun vm-frame-print (event)
  "Print a message to the default printer."
  (interactive "e")
  (vm-pipe-message-to-command vm-frame-print-command nil))


;;;----------------------------------------------------------------------------
;;
;;		       Support for vm-mail frames
;;
;;;----------------------------------------------------------------------------
;;
;; Sets up having all mail sending through VM (replies, followups,
;; forwards, and new mail sent with "vm-mail") to be created in a new
;; frame.
;;


;; This hook adds the "kill" keybinding
;;
(add-hook 'mail-setup-hook 'vm-frame-mail-setup)


;; So we know whether to kill the frame when vm-frame-mail-kill is run.
;;
(defvar vm-frame-mail-frame nil)
(make-variable-buffer-local 'vm-frame-mail-frame)


;; Put new mail messages into a new frame.
;;
(defadvice vm-mail-internal (around vm-frame-vm-mail-internal first activate)
  (if (not vm-frame-no-mail-frame)
      (select-frame (make-frame
		     (append vm-frame-mail-alist
			     (list (cons 'name (or (ad-get-arg 0)
						   "*VM-mail*")))))))
  ad-do-it
  (if (not vm-frame-no-mail-frame)
      	(setq vm-frame-mail-frame (selected-frame))))

;; Call vm-mail-send-and-exit, then punt the mail frame.
;;
(defadvice vm-mail-send-and-exit (around vm-frame-vm-mail-send-and-exit
					 first activate)
  "Deletes VM mail frame after sending (vm-frame)."
  (let ((cframe (selected-frame))
	(vframe vm-frame-mail-frame))
    (setq vm-frame-mail-frame nil)
    ad-do-it
    (if (or (eq vframe cframe)
	    (and (frame-live-p vframe)
		 (y-or-n-p "Delete mail frame too? ")))
	(delete-frame vframe))))


;; This function kills the current message without sending.  There's no
;; real counterpart in current VM or mail; everyone just killed the
;; buffer.  That don' work no more, because we have to trash the frame
;; too.
;;
(defun vm-frame-mail-kill (arg)
  (interactive "P")
  (if (eq vm-frame-mail-frame (selected-frame))
      (if (or arg (yes-or-no-p "Kill this mail message? "))
	  (let ((cframe (selected-frame)))
	    (kill-buffer (current-buffer))
	    (delete-frame cframe)))
    (call-interactively 'kill-buffer)))


;; Set up the new menus
;;
(let ((mail-map (lookup-key mail-mode-map [menu-bar mail])))
  (if mail-map
      (progn
	(define-key-after mail-map [kill]
	  '("Kill Message" . vm-frame-mail-kill) 'send-stay)
	(define-key mail-map [send]
	  '("Send Message" . vm-mail-send-and-exit))
	(define-key mail-map [send-stay]
	  '("Send, Keep Editing" . vm-mail-send)))
    (define-key mail-mode-map [menu-bar mail]
      (cons "Mail" (make-sparse-keymap "Mail")))
    (define-key mail-mode-map [menu-bar mail signature]
      '("Insert Signature" . mail-signature))
    (define-key mail-mode-map [menu-bar mail kill]
      '("Kill Message" . vm-frame-mail-kill))
    (define-key mail-mode-map [menu-bar mail send-stay]
      '("Send, Keep Editing" . vm-mail-send))
    (define-key mail-mode-map [menu-bar mail send]
      '("Send Message" . vm-mail-send-and-exit))))

;;; Provide ourselves:

(provide 'vm-frame)

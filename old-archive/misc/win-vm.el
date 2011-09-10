;; win-vm.el: Support for GNU Emacs 19 Frames/Menus/Mouse for VM (View Mail)
;;
;; Copyright (C) 1993, 1994  Paul D. Smith <psmith@wellfleet.com>
;;
;; This package is written for VM version 5.46 and above: earlier
;; versions aren't guaranteed to completely work.  Versions below 5.41
;; will especially have problems.
;;
;; This package has only been tested with GNU Emacs 19.22 and above.
;; Versions of GNU Emacs pre-19.16 definitely won't work.  It won't work
;; with Lucid Emacs either; get Jamie Zawinski's vm-lucid.el instead.
;;
;; VM (View Mail) is a very cool mail handling package for GNU Emacs.
;; VM is written and maintained by Kyle E. Jones <kyle@wonderworks.com>.
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
;; You didn't receive a copy of the GNU General Public License along
;; with this program; so, write to the Free Software Foundation, Inc.,
;; 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; LCD Archive Entry:
;;; win-vm|Paul D. Smith|psmith@wellfleet.com|
;;; Support for GNU Emacs 19 frames, menus, mouse with VM and VM-mail.|
;;; 06-Jun-1994|1.13|~/misc/win-vm.el.Z|
;;
;; HISTORY:
;;   1.13 - 06 Jun 1994 psmith
;;   1.12 - 20 May 1994 psmith
;;   1.11 - 07 Apr 1994 psmith
;;   1.10 - 19 Jan 1994 psmith
;;   1.09 - 17 Jan 1994 psmith
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
;;      (let ((my-vm-pkg
;;             (if (not window-system)
;;                 "vm"
;;               (define-key menu-bar-file-menu [rmail] '("Read Mail" . vm))
;;               (define-key-after menu-bar-file-menu [smail]
;;                                 '("Send Mail" . vm-mail) 'rmail)
;;               "win-vm")))
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
;;;----------------------------------------------------------------------------
;;
;; USAGE:
;;
;; General:
;; --------
;;
;;  Check the user-settable variables section below for configuration
;;  options, including control of when frames are popped up (no frames,
;;  for sending mail only, one frame for all of VM, one frame per
;;  folder), what the frames look like, how the folders pop-up is
;;  segmented into panes, whether to warp the mouse, etc.
;;
;; Reading Mail:
;; -------------
;;
;;  Just use VM normally.  When you first invoke VM, a new Emacs frame
;;  will appear to hold it.  There's one frame for all of VM; visit
;;  folders, etc. in that buffer.  Alternatively you can set
;;  win-vm-style to 'all, and win-vm will pop up a new frame for every
;;  VM folder.
;;
;;  Don't quit VM when you're done, iconify the frame instead.  Only
;;  quit VM when you want to quit your Emacs.  Note you might want to
;;  get into the habit of saving your INBOX occasionally, just in case.
;;
;;  The "i" key has been bound to call win-vm-iconify, which saves the
;;  current folder then iconifies the VM frame; a handy way to "put
;;  away" your VM frame(s).
;;
;;  When you get new mail or want to use VM again, you can enter VM as
;;  before (M-x vm or whatever); if the VM frame already exists it
;;  will be made visible and raised to the top, and the primary inbox
;;  will be visited.
;;
;;  When using win-vm-style 'single (the default), quitting out of the
;;  INBOX folder causes the VM frame to be deleted.  Quitting out of
;;  other folders *doesn't*.  When using win-vm-style 'all, quitting out
;;  of a folder deletes its frame.
;;
;;  Almost all common VM functions can be accessed through the new
;;  pull-down menus:
;;
;;    Visit     Similar to Emacs' "Buffers" menu: shows a list of VM
;;              folders you can visit.
;;
;;    Virtual   Similar to "Visit", but for virtual folders.
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
;;  The following mouse commands are supported in all VM windows:
;;
;;      C-mouse-1:      Pop up a list of folders (same as the Visit menu
;;                      bar option above).
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
;;  To kill a message without sending it, select "Cancel" from
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
;; I've cut out most of this, except for the recent changes.  Kudos and
;; thanks go to the following folks, all of whom suggested useful new
;; features and/or contributed some elisp to help them along (in
;; alphabetical order by first name):
;;
;;  Alan J. DeWeerd <deweerd@wisnuf.physics.wisc.edu>
;;  Bill Trost <trost@cloud.rain.com>
;;  Cengiz Alaettinoglu <ca@cs.umd.edu>
;;  Chris Moore <Chris.Moore@src.bae.co.uk>
;;  Colin Owen Rafferty <craffert@lehman.com>
;;  Corny de Souza <cdesouza@isoit154.bbn.hp.com>
;;  Dean Andrews <ada@unison.com>
;;  Dirk H. Hohndel <hohndel@aib.com>
;;  Gardner Cohen <gec@Mti.Sgi.Com>
;;  Graham Gough <graham@computer-science.manchester.ac.uk>
;;  Greg Thompson <gregt@porsche.visix.COM>
;;  Gregory Neil Shapiro <gshapiro@WPI.EDU>
;;  Jamie Zawinski <jwz@lucid.com>.
;;  Kevin Broadey <KevinB@bartley.demon.co.uk>
;;  Michael Kutzner <futzi@uni-paderborn.de>
;;  Mike Chace <mikec@praxis.co.uk>
;;  Per Abrahamsen <abraham@research.att.com>
;;  Siddarth Subramanian <siddarth@cs.utexas.edu>
;;  Uwe Bonnes <bon@LTE.E-TECHNIK.uni-erlangen.de>
;;
;; Changes from 1.12 to 1.13:
;;
;;  - Added support for virtual folders.  Dirk H. Hohndel
;;    <hohndel@aib.com> provided some sample vm-virtual-folder-alist
;;    values for me to test with, and beta-tested.
;;
;;    There's a new menu bar item, "Virtual", which contains a pull-down
;;    list of all the virtual folders you have defined in
;;    vm-virtual-folders-alist; if you don't have any the menu doesn't
;;    appear.
;;
;;    If win-vm-style is set to 'all, then visiting a virtual folder
;;    will put it into a new frame just like visiting a regular folder.
;;
;;  - Merged the "Kill Message" menu item win-vm used to use when
;;    sending mail with the standard "Cancel" menu item provided by
;;    Emacs, so there aren't two options.
;;
;;  - Added a few handy items to the menus.  Please let me know if I'm
;;    missing useful functions there.
;;
;;  - Gregory Neil Shapiro <gshapiro@WPI.EDU> sent in elisp to implement
;;    win-vm-warp-mouse-on-raise.  Set this to "t" if you want win-vm to
;;    warp the mouse cursor to the middle of any new frame that's
;;    created or raised (both folder and mail frames).
;;
;;  - Greg Thompson <gregt@porsche.visix.COM> asked for
;;    win-vm-expunge-when-iconify.  By default, iconifying now saves the
;;    folder but doesn't expunge it, to conform to VM's default
;;    behaviour.  If you set this variable to t iconifying will also
;;    expunge.
;;
;; Changes from 1.11 to 1.12:
;;
;;  - New installation instructions to make sure the "Read Mail" menu
;;    item under the File popup in Emacs 19.23 gets populated correctly.
;;
;;  - Tweaked various menus to conform to the new Emacs syntax; "..."
;;    means more info is needed and will be obtained from the
;;    minibuffer; ">" means another submenu will be popped up.
;;
;;  - Added the primary inbox to all folder lists, so you can easily and
;;    quickly get back to it if you've gone skipping down into
;;    subdirectories.
;;
;;  - Added a ".." field to folder lists, so users can back up through
;;    directory hierarchies as well as going down.
;;
;;  - Added "*New*" option to folder lists, so users can create new
;;    folders as well as add to existing ones.
;;
;;;----------------------------------------------------------------------------

;;;----------------------------------------------------------------------------
;;
;;                          Customize these:
;;
;;;----------------------------------------------------------------------------

;; This is a list of Frame parameters used to create the VM frame.
;; Modify it to whatever you like; try something like:
;;
;;  (setq win-vm-alist '((top . -1) (left . -1)))
;;
;; Alternatively, you can set this to point to a function; if so,
;; whenever a new frame is to be created this function will be called
;; with the buffer name string as its only argument.  The return value
;; of the function should be a list of frame parameters to be used to
;; create the VM frame.  Here's a simple example:
;;
;;  (setq win-vm-alist
;;        (function
;;         (lambda (bufname)
;;           (if (string= bufname (file-name-nondirectory vm-primary-inbox))
;;               '((top . -1) (left . -1))
;;             '((top . -10) (left . -10))))))
;;
;;
(defvar win-vm-alist nil
  "*Alist of frame parameters used for creating the VM frame.")


;; This is a list of Frame parameters for the newly created mail frames.
;; Modify it to whatever you like; try something like:
;;
;;  (setq win-vm-mail-alist '((height . 40) (top . -1) (left . 0)))
;;
;; Alternatively, you can set this to point to a function; this works
;; as in win-vm-alist, above.
;;
(defvar win-vm-mail-alist nil
  "*Alist of frame parameters used for creating Mail frames.")


;; Set this to nil if you don't want to be asked if you want to quit VM
;;
(defvar win-vm-confirm-quit t
  "*If t, asks if you want to quit the VM frame.  If nil, doesn't ask.")

;; Set this to nil if you don't want to be asked if you want to kill
;; your mail buffers.
;;
(defvar win-vm-confirm-mail-kill t
  "*If t, asks for confirmation before killing mail buffers.
If nil, just do it.")


;; Set this to t if you want win-vm-iconify to expunge your folders when
;; iconifying them; by default it just saves.
;;
(defvar win-vm-expunge-when-iconify nil
  "*If nil, does not expunge before iconifying.  If t, expunges first.")


;; Control how/if new frames are created; if you want cool pull-down
;; menus but don't want cool new frames, set these appropriately in your
;; ~/.emacs.
;;
(defvar win-vm-style 'single
  "*Controls if and when new frames are created for VM folders.

If 'none, no frames are created for VM folders.
If 'single, one frame is created to hold all VM folders.
If 'all, a frame is created for each folder visited.")

(defvar win-vm-no-mail-frame nil
  "*If t, doesn't create a new frame for outgoing VM mail messages.
If nil, makes a new frame.")


;; Control whether mouse pointer is moved into frame when a new frame is
;; created or raised.
;;
(defvar win-vm-warp-mouse-on-raise nil
  "*If t, the pointer will be moved to the center of the raised mail frame.
This assures that focus is given to the raised frame.
If nil, the pointer isn't moved")


;; If you want more/less folders to show up in each pane of the "Visit"
;; folders menu, set this.  Setting to nil causes all folders to appear
;; in one pane.
;;
(defvar win-vm-max-folders-per-pane 25
  "*Number of folders to display in each pane of the Visit menu.")


;; How to print a mail message (note! only used by the "print" pull-down
;; menu)
;;
(defvar win-vm-print-command lpr-command
  "*The command used to print VM mail messages.")


;; This variable allows you to customize the operations which appear in
;; the message menu (mouse-3)
;;
(defvar win-vm-message-menu-list
  '("Message"
    ("View Message" . win-vm-view-message)
    ("Reply to Sender" . vm-reply)
    ("Reply with Text" . vm-reply-include-text)
    ("Followup to All" . vm-followup)
    ("Followup with Text" . vm-followup-include-text)
    ("Forward Message" . vm-forward-message)
    ("Resend Message" . vm-resend-message)
    ("Resend Bounced" . vm-resend-bounced-message)
    ("Save in Folder >" . win-vm-save-in-folder)
    ("Write to File..." . vm-save-message-sans-headers)
    ("Toggle Headers" . vm-expose-hidden-headers)
    ("Delete" . vm-delete-message)
    ("Un-Delete" . vm-undelete-message)
    ("Mark" . vm-mark-message)
    ("Unmark" . vm-unmark-message)
    ("Print" . win-vm-print)
    ("Pipe to..." . vm-pipe-message-to-command)
    ("Burst Digest" . vm-burst-digest))
  "List of items in the message popup menu--can be customized if you like.")


;; This lets you customize the title of the message menu (mouse-3); see
;; the docs for vm-summary-format for the syntax.
;;
(defvar win-vm-message-menu-format "(%n: %.20s)"
  "Format of the message popup menu; see vm-summary-format for format info.")


;; If this is non-nil, and you put
;;
;;  (add-hook 'win-vm-iconify-hook 'win-vm-bury)
;;
;; into your .emacs or .vm file, then any buffers whose major mode
;; matches this regexp will be buried whenever you iconify VM.  The
;; feature this tries to provide really should be provided more
;; generically in Emacs itself, but I haven't gotten around to coding it
;; up :)
;;
(defvar win-vm-bury-buffers-regexp "\\(VM\\|Mail\\)"
  "Regexp of buffer modes to bury upon exiting or iconifying VM")


;; A few hooks--since I'm bummed at Kyle for not adding any to VM, I'll
;; try to be nice and add a few myself :)
;;
(defvar win-vm-iconify-hook nil
  "Hooks to be called by win-vm-iconify.
Called after the folder is saved but before it's iconified.")

(defvar win-vm-quit-vm-hook nil
  "Hooks to be called before VM is exited.
Note win-vm-exit-folder-hook is also called first for the current folder.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;----------------------------------------------------------------------------
;;
;;                  Nothing to customize below here.
;;
;;;----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; What are we?
;;
(defconst win-vm-version (substring "$Revision: 1.13 $" 11 -2)
  "$Id: win-vm.el,v 1.13 1994/06/06 23:42:24 psmith Exp $")


;;;----------------------------------------------------------------------------
;;
;;                     Support for the VM frame.
;;
;;;----------------------------------------------------------------------------

;; Define vm-summary-mode-map so VM won't define it and we can set it to
;; be a submap of vm-mode-map later.  Ditto for vm-mail-mode-map.
;;
;; If Kyle ever decides to do this himself in VM, this won't be
;; necessary anymore.
;;
(defvar vm-summary-mode-map nil
  "Keymap for VM Summary mode.  Inherits from vm-mode-map.")

(defvar vm-mail-mode-map nil
  "Keymap for VM Mail mode.  Inherits from mail-mode-map.")


;; Load required packages.
;;
(require 'advice)
(require 'vm)


;; Holds VM's frame list (if it exists)
;;
(defvar win-vm-frames nil)


;; vm-visit-folder calls vm to do most of its work; this variable is set
;; by my vm-visit-folder advice to be whatever frame info was discovered
;; by it, so my vm advice can use it.
;;
(defvar win-vm-folder-info nil)


;; Find the matching frame for this folder; nil if no matching frame.
;;
(defun win-vm-find-frame (folder)
  (if (eq win-vm-style 'none)
      nil
    (assoc folder win-vm-frames)))


;; Check a variable to see if it's a function; if so invoke it and
;; return its result; if not return its contents.
;;
(defun win-vm-resolve (var arg)
  (if (or (and (symbolp var) (fboundp var))
          (and (listp var) (eq (car var) 'lambda)))
      (funcall var arg)
    var))


;; If configured for it, warp the mouse to the center of the currently
;; selected frame.
;;
(defun win-vm-warp-mouse ()
  (if win-vm-warp-mouse-on-raise
      (progn
	(set-mouse-position (selected-frame)
			    (/ (frame-width) 2)
			    (/ (frame-height) 2))
	(unfocus-frame))))


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
(defun win-vm-select-frame (folder &optional virtual)
  "Create and/or select and raise the VM frame for the VM folder FOLDER.
Default is the primary inbox.  If VIRTUAL is t the folder is virtual."
  (interactive)
  (if (eq win-vm-style 'none)
      '(t . nil)
    (let* ((vframe (and virtual (eq win-vm-style 'all)))
	   (fname (if vframe
		      (concat "(" folder ")")
		    (expand-file-name
		     (cond ((or (not folder) (eq win-vm-style 'single))
			    vm-primary-inbox)
			   ((bufferp folder)
			    (or (buffer-file-name folder) vm-primary-inbox))
			   (t folder)))))
	   (ftitle (if vframe fname (file-name-nondirectory fname)))
	   (frame (win-vm-find-frame fname))
	   (new-frame (not (frame-live-p (cdr-safe frame)))))
      (if new-frame
          (let ((fp (select-frame
                     (make-frame
                      (append (win-vm-resolve win-vm-alist ftitle)
                              (list
			       (cons 'name
				     (if (eq win-vm-style 'single)
					 "VM"
				       (concat "VM: " ftitle)))))))))
            (win-vm-warp-mouse)
            (if frame
                (setcdr frame fp)
              (setq win-vm-frames
                    (append (list (cons fname fp)) win-vm-frames))
              (setq frame (car win-vm-frames))))
        (select-frame (cdr frame))
        (make-frame-visible)
        (raise-frame (cdr frame))
        (win-vm-warp-mouse))
      (cons (or (eq win-vm-style 'single) new-frame) frame))))


;; Add some advice to entering VM: if the frame doesn't exist we create
;; it and process the folder, otherwise we just bring the frame to the
;; foreground and check for new mail.
;;
(defadvice vm (around win-vm-vm first activate)
  "VM is started inside its own frame (win-vm)."
  (if (interactive-p)
      (let* ((fdata (or win-vm-folder-info
                        (win-vm-select-frame (ad-get-arg 0))))
             (frame (cdr fdata)))
        (if (car fdata)
            ad-do-it
          (vm-get-new-mail))
        (and frame (setcar frame (if (eq win-vm-style 'single)
                                     (expand-file-name vm-primary-inbox)
                                   (buffer-file-name vm-mail-buffer)))))
    ad-do-it))


(defadvice vm-visit-folder (around win-vm-vm-visit-folder first activate)
  "VM is started inside its own frame (win-vm)."
  (let ((win-vm-folder-info (win-vm-select-frame (ad-get-arg 0))))
    ad-do-it))


(defadvice vm-visit-virtual-folder (around win-vm-vm-visit-virtual-folder
					   first activate)
  "VM virtual folders are started inside their own frame (win-vm)."
  (let ((win-vm-folder-info (win-vm-select-frame (ad-get-arg 0) t)))
    ad-do-it))


;; Since we're creating a new frame we need to delete it when we quit.
;; However, since we're only creating one frame for all VM folders, we
;; only want to delete it when we quit the last folder.
;;
;; We don't quite accomplish that here; instead we delete the frame
;; whenever we quit from the INBOX buffer.  Oh well.
;;
(let ((symbol (symbol-function 'vm-quit)))
  (and (listp symbol) (eq (car symbol) 'autoload) (load "vm-folder")))

(defadvice vm-quit (around win-vm-vm-quit first activate)
  "Delete the VM frame if we're quitting from the user's primary inbox.
Also run win-vm-exit-folder-hook (win-vm)."
  (run-hooks 'win-vm-exit-folder-hook)
  (let* ((fname (or (buffer-file-name vm-mail-buffer)
		    (buffer-name vm-mail-buffer)))
         (main-p (string= fname (expand-file-name vm-primary-inbox)))
         (frame (win-vm-find-frame fname)))
    (if (and main-p
             win-vm-confirm-quit
             (not (y-or-n-p "Do you really want to quit VM? ")))
        (error "Aborted"))
    ad-do-it
    (and main-p (run-hooks 'win-vm-quit-vm-hook))
    (cond ((frame-live-p (cdr-safe frame))
           (delete-frame (cdr frame))
           (setcdr frame nil)))))


(defun win-vm-iconify ()
  "Saves the current folder and iconifies the current frame."
  (interactive)
  (if win-vm-expunge-when-iconify
      (vm-save-and-expunge-folder)
    (vm-save-folder))
  (run-hooks 'win-vm-iconify-hook)
  (and (frame-live-p (selected-frame))
       (iconify-frame (selected-frame))))


;; If point is on a valid message number in the summary mode, goto that
;; message.  Basically, we set point then pretend
;; vm-follow-summary-cursor is t and let VM do the work.
;;
(defun win-vm-goto-message (event)
  "Goto the message clicked on with the mouse."
  (interactive "e")
  (mouse-set-point event)
  (beginning-of-line)
  (let ((vm-follow-summary-cursor t))
    (vm-follow-summary-cursor)))


;; If the message buffer isn't visible, make it visible.
;;
(defun win-vm-view-message ()
  "View the current message if it's not already visible."
  (interactive)
  (or (get-buffer-window vm-mail-buffer t)
      (let ((this-command 'vm-scroll-forward))
        (vm-scroll-forward))))


;; Buries all buffers whose major mode matches
;; win-vm-bury-buffers-regexp (if it's non-nil).
;;
(defun win-vm-bury ()
  (if win-vm-bury-buffers-regexp
      (let ((list (buffer-list)))
        (while list
          (set-buffer (buffer-name (car list)))
          (if (string-match win-vm-bury-buffers-regexp mode-name)
              (bury-buffer (car list)))
          (setq list (cdr list))))))


;; Make a vm-summary mode keymap and rebind the mouse keys
;;
(define-key vm-mode-map "i" 'win-vm-iconify)
(define-key vm-mode-map [C-down-mouse-1] 'win-vm-menu-bar-folders)

(setq vm-summary-mode-map (cons 'keymap vm-mode-map))

(define-key vm-summary-mode-map [mouse-2] 'win-vm-goto-message)
(define-key vm-summary-mode-map [double-mouse-2] 'win-vm-view-message)
(define-key vm-summary-mode-map [down-mouse-3] 'win-vm-message-menu)


;; Change the default File menu mail options to use VM
;;
(define-key menu-bar-file-menu [rmail] '("Read Mail" . vm))
(define-key-after menu-bar-file-menu [smail] '("Send Mail" . vm-mail) 'rmail)

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
(define-key vm-mode-map [menu-bar virtual]
  '("Virtual" . win-vm-menu-bar-virtual-folders))
(define-key vm-mode-map [menu-bar visit]
  '("Visit" . win-vm-menu-bar-folders))

(put 'win-vm-menu-bar-virtual-folders 'menu-enable 'vm-virtual-folder-alist)

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
  '("Sort Physically By >" . win-vm-sort-physically))
(define-key vm-mode-map [menu-bar folder sort]
  '("Sort Folder By >" . win-vm-sort))
(define-key vm-mode-map [menu-bar folder save-iconify]
  '("Save & Iconify" . win-vm-iconify))
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
  '("Print" . win-vm-print))
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
  '("Save in Folder >" . win-vm-save-in-folder))

;; Send menu
;;
(define-key vm-mode-map [menu-bar send send-digest]
  '("Send Digest" . vm-send-digest))
(define-key vm-mode-map [menu-bar send resend]
  '("Resend Bounced" . vm-resend-bounced-message))
(define-key vm-mode-map [menu-bar send resend]
  '("Resend Message" . vm-resend-message))
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
  '("Help on VM" . win-vm-help))

(defun win-vm-help (event)
  "Displays VM help."
  (interactive "e")
  (let ((major-mode 'vm-mode))
    (describe-mode)))


;; Message pop-up menu
;;
(defun win-vm-message-menu (event)
  "Select the message pointed to by the mouse and show a pop-up menu of commands."
  (interactive "e")
  (win-vm-goto-message event)
  (sit-for 0)
  (let ((op (x-popup-menu event
                          (list ""
                                (cons
                                 (save-excursion
                                   (vm-select-folder-buffer)
                                   (vm-sprintf 'win-vm-message-menu-format
                                               (car vm-message-pointer)))
                                 (cdr win-vm-message-menu-list))))))
    (and op (call-interactively op))))


;; Virtual Folders menu: returns nil of no folder was selected or the
;; name of the folder selected.
;;
(defun win-vm-choose-virtual-folder (event)
  "Pop up a menu of VM virtual folders for selection with the mouse.
Returns the folder that you select, or nil if no folder was selected."
  (interactive "e")
  (let (menu)
    (setq menu
          (cons "Virtual Folder Menu"
                (let ((tail vm-virtual-folder-alist) (i 1) (panes nil) head)
                  (while tail
                    (let ((elt (car (car tail))))
		      (setq head (cons (cons elt elt) head)
			    i (1+ i))
		      (if (and win-vm-max-folders-per-pane
			       (= i win-vm-max-folders-per-pane))
			  (setq panes (cons (cons "Virtual Folder" 
						  (reverse head))
					    panes)
				head nil
				i 0)))
                    (setq tail (cdr tail)))
                  (if head
                      (setq panes (cons
                                   (cons "Virtual Folder"
                                         (reverse head)) panes)))
                  (nreverse panes))))
    (if (cdr menu)
        (x-popup-menu (if (listp event)
			  event
			(cons '(0 0) (selected-frame)))
		      menu)
      (message "No virtual folders have been defined!")
      nil)))

(defun win-vm-menu-bar-virtual-folders (event)
  "Pop up a menu of VM virtual folders for selection with the mouse.
This visits the virtual folder that you select."
  (interactive "e")
  (let ((folder (win-vm-choose-virtual-folder event))
        (this-command 'vm-visit-virtual-folder))
    (if folder
	(vm-visit-virtual-folder folder))))


;; Folders menu: returns nil of no folder was selected, t if the user
;; wants a new folder, or the name of the folder selected.
;;
(defun win-vm-choose-folder (event)
  "Pop up a menu of VM folders for selection with the mouse.
Returns the folder that you select, nil if no folder was selected, and t
if the new folder token was selected."
  (interactive "e")
  (let* ((inbox (file-name-nondirectory vm-primary-inbox))
         (exp-inbox (expand-file-name vm-primary-inbox))
         (head (list (cons inbox exp-inbox)
                     (cons "*New*" "/")
                     (cons ".." "..")))
         (folders (directory-files vm-folder-directory nil "[^~]$"))
         menu)
    (setq menu
          (cons "Folder Menu"
                (let ((tail folders)
                      (exclude-re
                       (concat "^\\(#.*\\|\\.\\.?"
                               (and (string= (file-name-directory exp-inbox)
                                             vm-folder-directory)
                                    (concat "\\|" inbox))
                               "\\)$"))
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
                            (if (and win-vm-max-folders-per-pane
                                     (= i win-vm-max-folders-per-pane))
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
          (cond ((string= choice "/"))
                (choice (expand-file-name choice vm-folder-directory))))
      (message "Empty folder directory!")
      nil)))

(defun win-vm-get-folder (event fdir)
  (let ((folder
         (let ((vm-folder-directory
                (file-name-as-directory
                 (expand-file-name
                  (or fdir vm-folder-directory default-directory)))))
           (win-vm-choose-folder event))))
    (if (stringp folder)
        (if (file-accessible-directory-p folder)
            (win-vm-get-folder event (file-name-as-directory folder))
          folder)
      folder)))

(defun win-vm-menu-bar-folders (event)
  "Pop up a menu of VM folders for selection with the mouse.
This visits the folder that you select."
  (interactive "e")
  (let ((folder (win-vm-get-folder event nil))
        (this-command 'vm-visit-folder))
    (cond ((stringp folder) (vm-visit-folder folder))
          (folder (call-interactively 'vm-visit-folder)))))

(defun win-vm-save-in-folder (event)
  "Save message in a VM folder.  Pop up a menu of VM folders for selection."
  (interactive "e")
  (let ((folder (win-vm-get-folder event nil))
        (this-command 'vm-save-message))
    (cond ((stringp folder) (vm-save-message folder))
          (folder (call-interactively 'vm-save-message)))))

;; Choose a sorting option
;;
(defun win-vm-choose-sort (event)
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

(defun win-vm-sort (event)
  "Change the presentation order of messages in VM folders.
Doesn't change the physical ordering of the messages."
  (interactive "e")
  (let ((style (win-vm-choose-sort event))
        (this-command 'vm-sort-messages))
    (and style (vm-sort-messages style nil))))

(defun win-vm-sort-physically (event)
  "Change the physical ordering of messages in VM folders."
  (interactive "e")
  (let ((style (win-vm-choose-sort event))
        (this-command 'vm-sort-messages))
    (and style (vm-sort-messages style t))))

;; Quick printing option
;;
(defun win-vm-print (event)
  "Print a message to the default printer."
  (interactive "e")
  (vm-pipe-message-to-command win-vm-print-command nil))


;;;----------------------------------------------------------------------------
;;
;;                     Support for vm-mail frames
;;
;;;----------------------------------------------------------------------------
;;
;; Sets up having all mail sending through VM (replies, followups,
;; forwards, and new mail sent with "vm-mail") to be created in a new
;; frame.
;;

;; Load sendmail to define mail-mode-map.
;;
(require 'sendmail)


;; So we know whether to kill the frame when win-vm-mail-kill is run.
;;
(defvar win-vm-mail-frame nil)
(make-variable-buffer-local 'win-vm-mail-frame)


;; Check to see if vm-reply has been loaded already; if not then load it.
;;
(let ((symbol (symbol-function 'vm-mail-send-and-exit)))
  (and (listp symbol) (eq (car symbol) 'autoload) (load "vm-reply")))

;; Put new mail messages into a new frame.
;;
(defadvice vm-mail-internal (around win-vm-vm-mail-internal first activate)
  (if (not win-vm-no-mail-frame)
      (let ((bname (or (ad-get-arg 0) "*VM-mail*")))
        (select-frame (make-frame
                       (append (win-vm-resolve win-vm-mail-alist bname)
                               (list (cons 'name bname)))))
	(win-vm-warp-mouse)))
  ad-do-it
  (if (not win-vm-no-mail-frame)
      (setq win-vm-mail-frame (selected-frame))))

;; Call vm-mail-send-and-exit, then punt the mail frame.
;;
(defadvice vm-mail-send-and-exit (around win-vm-vm-mail-send-and-exit
                                         first activate)
  "Deletes VM mail frame after sending (win-vm)."
  (let ((cframe (selected-frame))
        (vframe win-vm-mail-frame))
    (setq win-vm-mail-frame nil)
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
(defun win-vm-mail-kill (arg)
  (interactive "P")
  (if (eq win-vm-mail-frame (selected-frame))
      (if (or arg (not win-vm-confirm-mail-kill)
              (yes-or-no-p "Kill this mail message? "))
          (let ((cframe (selected-frame)))
            (kill-buffer (current-buffer))
            (delete-frame cframe)))
    (call-interactively 'kill-buffer)))


;; Set up the new menus
;;
(setq vm-mail-mode-map (cons 'keymap mail-mode-map))

(define-key vm-mail-mode-map "\C-c\C-v" vm-mode-map)
(define-key vm-mail-mode-map "\C-c\C-y" 'vm-yank-message)
(define-key vm-mail-mode-map "\C-c\C-s" 'vm-mail-send)
(define-key vm-mail-mode-map "\C-c\C-c" 'vm-mail-send-and-exit)

;; You might want to change this: this key runs a function which kills
;; the current mail buffer.  Normally I just used C-x k, but now we
;; have to delete the frame too, so... voila!  New function.
;;
(define-key vm-mail-mode-map "\C-xk"    'win-vm-mail-kill)

(let ((mail-map (lookup-key vm-mail-mode-map [menu-bar mail])))
  (if (keymapp mail-map)
      (progn
        (define-key mail-map [cancel]
          '("Cancel" . win-vm-mail-kill))
        (define-key mail-map [send]
          '("Send Message" . vm-mail-send-and-exit))
        (define-key mail-map [send-stay]
          '("Send, Keep Editing" . vm-mail-send)))
    (define-key vm-mail-mode-map [menu-bar mail]
      (cons "Mail" (make-sparse-keymap "Mail")))
    (define-key vm-mail-mode-map [menu-bar mail signature]
      '("Insert Signature" . mail-signature))
    (define-key vm-mail-mode-map [menu-bar mail cancel]
      '("Cancel" . win-vm-mail-kill))
    (define-key vm-mail-mode-map [menu-bar mail send-stay]
      '("Send, Keep Editing" . vm-mail-send))
    (define-key vm-mail-mode-map [menu-bar mail send]
      '("Send Message" . vm-mail-send-and-exit))))


;;; Provide ourselves:

(provide 'win-vm)

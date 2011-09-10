;;; info+.el --- Extensions to `info.el'.
;; 
;; Author: D. ADAMS
;; Maintainer: D. ADAMS
;; Copyright (C) 1996-2001, Drew Adams, all rights reserved.
;; Created: Tue Sep 12 16:30:11 1995
;; Version: $Id: info+.el,v 1.6 2001/01/03 17:38:49 dadams Exp $
;;   Last modified by: 
;;   Last modified on: Wed Jan  3 09:38:47 2001
;;   Update count: 1892
;; Keywords: help, docs, internal
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;;    Extensions to `info.el'.
;;
;;  New functions defined here: 
;;
;;    `dir-info', `emacs-info', `emacs-lisp-info',
;;    `Info-display-node-default-header',
;;    `Info-display-node-time-header', `Info-merge-subnodes'.
;;
;;  New user options (variables) defined here:
;;
;;    `Info-display-node-header-fn', `info-file-face',
;;    `info-fontify-quotations-p', `info-fontify-strings-p',
;;    `info-menu-face', `info-msg-face', `info-node-face',
;;    `info-quoted-name-face', `info-string-face',
;;    `Info-subtree-separator', `info-xref-face'.
;;
;;  Other variables defined here:
;;
;;    `Info-merged-map', `menu-bar-info-menu'.
;;
;;
;;  ***** NOTE: The following standard functions defined in `info.el'
;;              have been REDEFINED HERE:
;;
;;  `Info-find-emacs-command-nodes' - Added in-progress message.
;;  `Info-find-node' - 1. Uses other-window.
;;                     2. `shrink-frame-to-fit' buffer if `Info-fit-frame-p'.
;;  `Info-fontify-node' - 
;;     1. File name in face `info-file-face'.
;;     2. Node names in face `info-node-face'.
;;     3. Menu items in face `info-menu-face'.
;;     4. Only 5th and 9th menu items have their `*' colored.
;;     5. Notes in face `info-xref-face'.
;;     6. If `info-fontify-quotations-p', quotations in face
;;        `info-quoted-name-face'.
;;     7. If `info-fontify-strings-p', strings in face `info-string'.
;;  `Info-goto-emacs-command-node' - 
;;     1. Uses `completing-read' in interactive spec, with,
;;        as default, `symbol-nearest-point'.
;;     2. Message if single node found.
;;     3. Returns `num-matches' if found; nil if not.
;;     4. Uses `display-in-minibuffer' instead of `message'.
;;  `Info-goto-emacs-key-command-node' - 
;;     1. If key's command not found, then `Info-search's for key
;;        sequence in text and displays message about repeating.
;;     2. Treats `execute-extended-command-and-check-for-bindings'
;;        like `execute-extended-command'.
;;     3. Uses `display-in-minibuffer' instead of `message'.
;;  `Info-mode' - Doc string shows all bindings.
;;  `Info-search' - Highlights found regexp if `search-highlight'.
;;
;;
;;  ***** NOTE: The following behavior defined in `info.el'
;;              has been changed.
;;
;; "*info" has been removed from `same-window-buffer-names', so that
;; a separate window can be used if the user so chooses.
;;
;;
;; NOTE: The faces defined here, and those defined in `setup-info.el',
;;       look best on a medium-dark background.  Try, for example,
;;       setting the background to "LightSteelBlue" in your `~/.emacs'
;;       file. You can do this is via `special-display-buffer-names':
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
;;  The following bindings are made here for Info-mode:
;;
;;    `?'        `describe-mode' (replaces `Info-summary')
;;
;;  The following bindings are made here for merged Info buffers:
;;
;;    `.'         `beginning-of-buffer'
;;    `b'         `beginning-of-buffer'
;;    `q'         `quit-window'
;;    `s'         `nonincremental-re-search-forward'
;;    `M-s'       `nonincremental-re-search-forward'
;;    `TAB'       `Info-next-reference'
;;    `ESC TAB'   `Info-prev-reference'

;;;;
;;
;;  This file should be loaded after loading the standard GNU file
;;  `info.el'.  So, in your `~/.emacs' file, do this:
;;  (eval-after-load "info" '(require 'info+))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; RCS $Log: info+.el,v $
;; RCS Revision 1.6  2001/01/03 17:38:49  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.5  2001/01/03 00:45:29  dadams
;; RCS *** empty log message ***
;; RCS
;; RCS Revision 1.4  2000/12/07 19:49:25  dadams
;; RCS Added require of shrink-fit.el.
;; RCS
;; RCS Revision 1.3  2000/11/28 20:23:11  dadams
;; RCS Optional require's via 3rd arg=t now.
;; RCS
;; RCS Revision 1.2  2000/09/27 22:11:15  dadams
;; RCS 1. Added: Info-fit-frame-p.
;; RCS 2. Info-find-node: added shrink-frame-to-fit.
;; RCS
;; RCS Revision 1.1  2000/09/14 17:20:38  dadams
;; RCS Initial revision
;; RCS
; Revision 1.13  1999/04/14  11:25:44  dadams
; Info-fontify-node: Fontify indexes too.
;
; Revision 1.12  1999/04/14  11:05:42  dadams
; 1. Added vars: info-file-face, info-menu-face, info-node-face,
;    info-quoted-name-face, info-string-face, info-xref-face.
; 2. Require def-face-const.el when compile. Use to define above face values.
; 3. No longer use (or define) faces: info-node, info-file, info-xref,
;    info-menu-5, info-quoted-name, info-string.
; 4. ###autoload: info-fontify-quotations-p, info-fontify-strings-p.
; 5. Info-fontify-node: Use new face variables instead of faces in #3, above.
;    Corrected: node names in info-node-face (was xref). Use info-menu-face
;    for * and menu item.
; 6. Info-mode: Redefined like original, but: no make-face's; use face vars.
;    Added user options description to doc string.
;
; Revision 1.11  1999/04/08  11:18:04  dadams
; Info-goto-emacs-key-command-node: regexp-quote pp-key for Info-search.
;
; Revision 1.10  1999/04/08  10:07:56  dadams
; 1. Require frame-cmds.el.
; 2. Info-exit: switch-to-buffer -> delete-window.
;
; Revision 1.9  1999/04/07  15:43:29  dadams
; Info-goto-emacs-key-command-node: a) messages only if interactive,
;     b) return nil if not found, else non-nil, c) "is undefined" -> "doc not
;     found", d) use display-in-minibuffer more, e) corrected error handler.
;
; Revision 1.8  1999/04/02  14:27:55  dadams
; Added: (require 'faces+).
;
; Revision 1.7  1999/04/01  13:02:02  dadams
; 1. Added: (remove-hook 'same-window-buffer-names "*info*").
; 2. Removed: info (use std version, with pop-to-buffer).
; 3. Info-find-node: switch-to-buffer-other-window -> pop-to-buffer.
;
; Revision 1.6  1999/03/31  13:31:33  dadams
; 1. Added (put 'Info-goto-emacs-(key-)command-node 'info-file "emacs").
; 2. Info-find-node: Mention searched file in error messages.
; 3. Added (replacement): Info-find-emacs-command-nodes, with progress msg.
; 4. a. Info-goto-emacs-key-command-node: Use global-map, unless menu item.
;    b. Added message "Not found using Index ...".
;
; Revision 1.5  1999/03/31  12:06:16  dadams
; 1. Info-goto-emacs(-key)-command-node: Only display-in-minibuffer if
;    interactive-p.
; 2. Info-goto-emacs-key-command-node: Messages: "key"; other entries.
;
; Revision 1.4  1999/03/31  11:43:15  dadams
; 1. Added (put 'info 'info-file "emacs") so find doc on `info' cmd.
; 2. Info-goto-emacs-command-node:
;    a. Added message when =< 1 match.
;    b. Return num-matches if found.
;    c. Uses `display-in-minibuffer' instead of `message'.
; 3. a. Wrapped call to Info-search in condition-case, not if.
;    b. Info-goto-emacs-key-command-node: Return num-matches if found.
; 4. Updated file header.
;
; Revision 1.3  1999/03/30  16:01:53  dadams
; 1. Added Info menu bar menu, instead of using Search menu (didn't work).
; 2. Info-goto-emacs-command-node: Only error if interactive-p.
; 3. Info-goto-emacs-key-command-node:
;    a. Pprint key in msgs
;    b. If Info-goto-emacs-command-node doesn't find it, then try
;       Info-search. If found & interactive-p, then msg ("repeat").
;       Else error.
; 4. Info-search: Msg ("repeat") if found & interactive-p.
;
; Revision 1.2  1999/03/17  14:46:24  dadams
; 1. Removed require: std-faces.
; 2. Updated to correspond with Emacs 34.1 version.
; 3. Protect with fboundp.
;
; Revision 1.1  1997/03/20  16:44:43  dadams
; Initial revision
;
; Revision 1.25  1996/07/15  08:53:38  dadams
; (trivial)
;
; Revision 1.24  1996/07/11  14:37:23  dadams
; Added redefinitions of Info-goto-emacs-(key-)command-node.
;
; Revision 1.23  1996/07/01  13:13:58  dadams
; (trivial)
;
; Revision 1.22  1996/07/01  07:22:54  dadams
; (trivial)
;
; Revision 1.21  1996/06/14  13:50:34  dadams
; Updated file header Commentary to mention new user options defined here.
;
; Revision 1.20  1996/06/06  14:06:03  dadams
; Update of file dependency comments (e.g. "Autoloaded from...").
;
; Revision 1.19  1996/04/26  07:39:21  dadams
; 1. Added Info-search to menu-bar Search menu.
; 2. Put escaped newlines on long-line strings.
;
; Revision 1.18  1996/04/25  13:52:10  dadams
; (trivial: doc strings)
;
; Revision 1.17  1996/04/18  15:26:18  dadams
; (trivial)
;
; Revision 1.16  1996/04/16  13:14:20  dadams
; Added: info-file, info-quoted-name, info-string, info-fontify-quotations-p,
;        info-fontify-strings-p.  Take into account in Info-fontify-node.
;
; Revision 1.15  1996/04/16  12:42:45  dadams
; (trivial) info-xref, info-menu-5: Blue -> Red
;
; Revision 1.14  1996/04/05  14:29:28  dadams
; Improved Commentary:  List redefinitions.
;
; Revision 1.13  1996/03/08  12:25:03  dadams
; 1. Copyright.
; 2. drew-faces.el -> std-faces.el.
; 3. Info-exit: Uses delete/iconify-windows-on.
;
; Revision 1.12  1996/02/23  08:41:10  dadams
; 1. Changed binding of Info-merge-subnodes back to `r', but now
;    requires user confirmation when invoked.
; 2. Info-subtree-separator: Incorporates "\n* ".  variable-interactive prop.
;
; Revision 1.11  1996/02/22  16:46:50  dadams
; display-Info-node-subtree:
; 1. display-Info-node-subtree -> Info-merge-subnodes (renamed).
; 2. Changed binding of Info-merge-subnodes from `r' to `C-d'.
; 3. Don't pick up text between menu-item-line and "\n* ".  Hardwire "\n* ".
; 4. Untabify menu-item-line, so can count chars to underline.
; 5. indent-rigidly, not indent-region.
;
; Revision 1.10  1996/02/22  15:36:19  dadams
; 1. Require this to ensure loaded before compiling, so old-Info-mode defined.
; 2. Bind describe-mode and display-Info-node-subtree.
; 3. Added redefinition of Info-mode: Only the doc string was changed.
; 4. Added Info-subtree-separator.
; 5. display-Info-node-subtree: Info-subtree-separator. Doc. Garbage-collect.
;
; Revision 1.9  1996/02/22  10:49:15  dadams
; ###autoload display-Info-node-subtree.
;
; Revision 1.8  1996/02/22  09:58:12  dadams
; Info-merge-subnodes: Rewrote it, adding optional args.  Renamed (defaliased) it
; to display-Info-node-subtree.
;
; Revision 1.7  1996/02/22  09:19:56  dadams
; Added redefinition of Info-merge-subnodes (cleanup, corrections).
;
; Revision 1.6  1996/02/20  15:11:19  dadams
; 1. Make info-node, info-xref, info-menu-5 here. (Diff faces than before.)
; 2. Added redefinition of Info-find-node.  (Uses other window.)
;
; Revision 1.5  1996/02/15  15:05:16  dadams
; Added redefinitions of info and Info-exit.
;
; Revision 1.4  1996/02/12  10:01:11  dadams
; Updated header keywords (for finder).
;
; Revision 1.3  1995/12/28  15:35:31  dadams
; (trivial)
;
; Revision 1.2  1995/10/31  13:03:37  dadams
; (trivial - Keywords)
;
; Revision 1.1  1995/09/13  07:24:47  dadams
; Initial revision
;; 
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

;; Get macro `define-face-const' when this is compiled,
;; or run interpreted, but not when the compiled code is loaded.
(eval-when-compile (require 'def-face-const))

(require 'setup-info nil t) ;; (no error if not found)
                            ;; Info-fontify-maximum-menu-size,
                            ;; Info-title-face-alist
(require 'info)
(require 'cl) ;; when, unless, decf, caar, cadr

;; These are optional, for cosmetic purposes.
(require 'thingatpt nil t) ;; (no error if not found): symbol-at-point
(require 'thingatpt+ nil t) ;; (no error if not found): symbol-nearest-point
(require 'strings nil t) ;; (no error if not found): concat-w-faces, display-in-minibuffer
(require 'shrink-fit nil t) ;; (no error if not found): shrink-frame-to-fit


(provide 'info+)
(require 'info+) ;; Ensure loaded before compiling.

;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defvar Info-fit-frame-p t "*Non-nil => `shrink-frame-to-fit' Info buffer.")

;; Do this to counteract what is done in `info.el'.
;; There is no reason not to use a separate window, if the user, e.g.,
;; sets `pop-up-windows' or `pop-up-frames' non-nil.
;;;###autoload
(remove-hook 'same-window-buffer-names "*info*")

;;; Make `Info-find-emacs-command-nodes' look for the following
;;; commands in Emacs manual. In particular, don't look for command
;;; `info' Info manual, because that has no Command Index or Index.
(put 'info 'info-file "emacs")
(put 'Info-goto-emacs-command-node 'info-file "emacs")
(put 'Info-goto-emacs-key-command-node 'info-file "emacs")


;;; Keys
(define-key Info-mode-map "?" 'describe-mode) ; Don't use `Info-summary'.
                                        ; (Defined in `help.el'.)

(defvar Info-merged-map nil "Keymap for merged Info buffer.")
(if Info-merged-map
    nil
  (setq Info-merged-map (make-keymap))
  (suppress-keymap Info-merged-map)
  (define-key Info-merged-map "." 'beginning-of-buffer)
  (define-key Info-merged-map "\t" 'Info-next-reference)
  (define-key Info-merged-map "\e\t" 'Info-prev-reference)
  (define-key Info-merged-map "b" 'beginning-of-buffer)
  (define-key Info-merged-map "q" 'quit-window)
  (define-key Info-merged-map "s" 'nonincremental-re-search-forward)
  (define-key Info-merged-map "\M-s" 'nonincremental-re-search-forward))


(easy-menu-define
 Info-mode-menu
 Info-mode-map
 "Menu for merged files."
 '("Info"
   ["Back" Info-last Info-history]
   ("Menu item" ["You should never see this" report-emacs-bug t])
   ("Reference" ["You should never see this" report-emacs-bug t])
   "--"
   ["Up" Info-up (Info-check-pointer "up")]
   ["Next" Info-next (Info-check-pointer "next")]
   ["Previous" Info-prev (Info-check-pointer "prev[ious]*")]
   ["Top" Info-directory t]
   ["Goto Node..." Info-goto-node t]
   "--"
   ["Forward in File" Info-forward-node t]
   ["Backward in File" Info-backward-node t]
   ["First in File" Info-top-node t]
   ["Last in File" Info-final-node t]
   "--"
   ["Next Link in Node" Info-next-reference t]
   ["Previous Link in Node" Info-prev-reference t]
   "--"
   ["Search (regexp)" Info-search t]
   ["Info on Key" Info-goto-emacs-key-command-node t]
   ["Info on Command" Info-goto-emacs-command-node t]
   ["Find with Index" Info-index t]
   "--"
   ["Merge Subnodes" Info-merge-subnodes t]
   ["Edit Node" Info-edit t]
   "--"
   ["Tutorial" Info-help t]
   ["Quit Info" Info-exit t]))

(easy-menu-define
 Info-merged-menu
 Info-merged-map
 "Menu for merged `info' buffers."
 '("Info"
   ["Next Link" Info-next-reference t]
   ["Previous Link" Info-prev-reference t]
   ["Search (regexp)" Info-search t]
   ["Quit" quit-window t]))

;;;###autoload
(defvar info-fontify-quotations-p t
  "*Non-nil <=> `info' fontifies names between single quotes (e.g. `C-x').")
;;;###autoload
(defvar info-fontify-strings-p t
  "*Non-nil <=> `info' fontifies strings (e.g. \"a string\").")


;; Faces.  See NOTE at beginning of this file regarding frame background color.
(unless (boundp 'lightgray-background-face)
  (define-face-const nil "LightGray"))
(defvar info-file-face lightgray-background-face
  "Face used for file heading labels in `info'.")
(unless (boundp 'blue-on-skyblue-face)
  (define-face-const "Blue" "SkyBlue"))
(defvar info-node-face blue-on-skyblue-face
  "Face used for node heading labels in `info'.")
(unless (boundp 'blue-foreground-face)
  (define-face-const "Blue" nil))
(defvar info-menu-face blue-foreground-face
  "Face used for menu items in `info'.")
(defvar info-msg-face blue-foreground-face
  "Face used for highlighting parts of `info' messages.")
(defvar info-xref-face info-menu-face
  "Face used for cross reference \"Note\" links in `info'.")
(unless (boundp 'yellow-foreground-face)
  (define-face-const "Yellow" nil))
(defvar info-quoted-name-face yellow-foreground-face
  "Face used for quoted names (e.g. `toto') in `info'.")
(unless (boundp 'darkmagenta-foreground-face)
  (define-face-const "DarkMagenta" nil))
(defvar info-string-face darkmagenta-foreground-face
  "Face used for strings (e.g. \"toto\") in `info'.")

  
;;;###autoload
(defun emacs-info ()
  "Access the Emacs manual via \"Info\"."
  (interactive)
  (info "emacs"))

;;;###autoload
(defun emacs-lisp-info ()
  "Access the Emacs Lisp manual via \"Info\"."
  (interactive)
  (info "elisp"))

;;;###autoload
(defun dir-info ()
  "Access all online manuals via \"Info\"."
  (interactive)
  (info "dir"))



;; REPLACES ORIGINAL in `info.el': 
;; 1. More explicit error messages: mention file.
;; 2. `shrink-frame-to-fit' buffer if `Info-fit-frame-p'.
;;;###autoload
(defun Info-find-node (filename nodename &optional no-going-back)
;; Go to an info node specified as separate FILENAME and NODENAME.
;; NO-GOING-BACK is non-nil if recovering from an error in this function;
;; it says do not attempt further (recursive) error recovery.

  ;; Convert filename to lower case if not found as specified.
  ;; Expand it.
  (if filename
      (let (temp temp-downcase found)
        (setq filename (substitute-in-file-name filename))
        (if (string= (downcase  filename) "dir")
            (setq found t)
          (let ((dirs (if (string-match "^\\./" filename)
                          ;; If specified name starts with `./'
                          ;; then just try current directory.
                          '("./")
                        (if (file-name-absolute-p filename)
			    ;; No point in searching for an absolute file name.
			    '(nil)
			  (if Info-additional-directory-list
                              (append Info-directory-list
                                      Info-additional-directory-list)
                            Info-directory-list)))))
            ;; Search the directory list for file FILENAME.
            (while (and dirs (not found))
              (setq temp (expand-file-name filename (car dirs)))
              (setq temp-downcase
                    (expand-file-name (downcase filename) (car dirs)))
              ;; Try several variants of specified name.
              (let ((suffix-list Info-suffix-list))
                (while (and suffix-list (not found))
                  (cond ((info-file-exists-p
                          (info-insert-file-contents-1 temp (caar suffix-list)))
                         (setq found temp))
                        ((info-file-exists-p
                          (info-insert-file-contents-1 temp-downcase
                                                       (caar suffix-list)))
                         (setq found temp-downcase)))
                  (setq suffix-list (cdr suffix-list))))
              (setq dirs (cdr dirs)))))
        (if found
            (setq filename found)
          (error "Info file `%s' does not exist." filename))))
  ;; Record the node we are leaving.
  (if (and Info-current-file (not no-going-back))
      (setq Info-history
            (cons (list Info-current-file Info-current-node (point))
                  Info-history)))
  ;; Go into info buffer.
  (or (eq major-mode 'Info-mode) (pop-to-buffer "*info*"))
  (buffer-disable-undo (current-buffer))
  (or (eq major-mode 'Info-mode) (Info-mode))
  (widen)
  (setq Info-current-node nil)
  (unwind-protect
      ;; Bind case-fold-search in case the user sets it to nil.
      (let ((case-fold-search t)
	    anchorpos)
        ;; Switch files if necessary
        (or (null filename)
            (equal Info-current-file filename)
            (let ((buffer-read-only nil))
              (setq Info-current-file nil
                    Info-current-subfile nil
                    Info-current-file-completions nil
                    buffer-file-name nil)
              (erase-buffer)
              (if (eq filename t)
                  (Info-insert-dir)
                (info-insert-file-contents filename t)
                (setq default-directory (file-name-directory filename)))
              (set-buffer-modified-p nil)
              ;; See whether file has a tag table.  Record the location if yes.
              (goto-char (point-max))
              (forward-line -8)
              ;; Use string-equal, not equal, to ignore text props.
              (if (not (or (string-equal nodename "*")
                           (not
                            (search-forward "\^_\nEnd tag table\n" nil t))))
                  (let (pos)
                    ;; We have a tag table.  Find its beginning.
                    ;; Is this an indirect file?
                    (search-backward "\nTag table:\n")
                    (setq pos (point))
                    (if (save-excursion
                          (forward-line 2)
                          (looking-at "(Indirect)\n"))
                        ;; It is indirect.  Copy it to another buffer
                        ;; and record that the tag table is in that buffer.
                        (let ((buf (current-buffer))
                              (tagbuf
                               (or Info-tag-table-buffer
                                   (generate-new-buffer " *info tag table*"))))
                          (setq Info-tag-table-buffer tagbuf)
                          (save-excursion
                            (set-buffer tagbuf)
                            (buffer-disable-undo (current-buffer))
                            (setq case-fold-search t)
                            (erase-buffer)
                            (insert-buffer-substring buf))
                          (set-marker Info-tag-table-marker
                                      (match-end 0) tagbuf))
                      (set-marker Info-tag-table-marker pos)))
                  (set-marker Info-tag-table-marker nil))
              (setq Info-current-file
                    (if (eq filename t) "dir" filename))))
        ;; Use string-equal, not equal, to ignore text props.
        (if (string-equal nodename "*")
            (progn (setq Info-current-node nodename)
                   (Info-set-mode-line))
          ;; Possibilities:
          ;;
          ;; 1. Anchor found in tag table
          ;; 2. Anchor *not* in tag table
          ;;
          ;; 3. Node found in tag table
          ;; 4. Node *not* found in tag table, but found in file
          ;; 5. Node *not* in tag table, and *not* in file
          ;;
          ;; *Or* the same, but in an indirect subfile.

          ;; Search file for a suitable node.
          (let ((guesspos (point-min))
                (regexp (concat "\\(Node:\\|Ref:\\) *\\(" (regexp-quote nodename)
                                "\\) *[,\t\n\177]"))
		(nodepos nil))

            ;; First, search a tag table, if any
            (if (marker-position Info-tag-table-marker)
		(let ((found-in-tag-table t)
		      found-anchor
		      found-mode
		      (m Info-tag-table-marker))
                  (save-excursion
                    (set-buffer (marker-buffer m))
                    (goto-char m)
                    (beginning-of-line) ; so re-search will work.

                    ;; Search tag table
		    (catch 'foo
		      (while (re-search-forward regexp nil t)
			(setq found-anchor
			      (string-equal "Ref:" (match-string 1)))
			(or nodepos (setq nodepos (point))
                            (if (string-equal (match-string 2) nodename)
                                (throw 'foo t))))
		      (if nodepos
			  (goto-char nodepos)
			(setq found-in-tag-table nil)))
                    (if found-in-tag-table
                        (setq guesspos (1+ (read (current-buffer)))))
                    (setq found-mode major-mode))

                  ;; Indirect file among split files
                  (if found-in-tag-table
                      (progn
                        ;; If this is an indirect file, determine
                        ;; which file really holds this node and
                        ;; read it in.
                        (if (not (eq found-mode 'Info-mode))
                            ;; Note that the current buffer must be
                            ;; the *info* buffer on entry to
                            ;; Info-read-subfile.  Thus the hackery
                            ;; above.
                            (setq guesspos (Info-read-subfile guesspos)))))

                  ;; Handle anchor
                  (if found-anchor
		      (goto-char (setq anchorpos guesspos))

                    ;; Else we may have a node, which we search for:
		    (goto-char (max (point-min)
				    (- (byte-to-position guesspos) 1000)))
                    ;; Now search from our advised position
                    ;; (or from beg of buffer)
                    ;; to find the actual node.
		    ;; First, check whether the node is right
		    ;; where we are, in case the buffer begins
		    ;; with a node.
		    (setq nodepos nil)
		    (or (and (string-match "20\.6" emacs-version)
                             (Info-node-at-bob-matching regexp))
			(catch 'foo
			  (while (search-forward "\n\^_" nil t)
			    (forward-line 1)
			    (let ((beg (point)))
			      (forward-line 1)
			      (if (re-search-backward regexp beg t)
				  (if (string-equal (match-string 2) nodename)
				      (progn
					(beginning-of-line)
					(throw 'foo t))
				    (or nodepos
                                        (setq nodepos (point)))))))
			  (if nodepos
			      (progn
				(goto-char nodepos)
				(beginning-of-line))
			    (error
			     "No such anchor in tag table or node in tag table or file: `%s'"
			     nodename))))))
	      (goto-char (max (point-min) (- guesspos 1000)))
	      ;; Now search from our advised position (or from beg of buffer)
	      ;; to find the actual node.
	      ;; First, check whether the node is right where we are, in case
	      ;; the buffer begins with a node.
	      (setq nodepos nil)
	      (or (and (string-match "20\.6" emacs-version)
                       (Info-node-at-bob-matching regexp))
		  (catch 'foo
		    (while (search-forward "\n\^_" nil t)
		      (forward-line 1)
		      (let ((beg (point)))
			(forward-line 1)
			(if (re-search-backward regexp beg t)
			    (if (string-equal (match-string 2) nodename)
				(throw 'foo t)
			      (or nodepos
				  (setq nodepos (point)))))))
		    (if nodepos
			(goto-char nodepos)
		      (error "No such node: `%s'" nodename))))))
          (Info-select-node)
	  (goto-char (or anchorpos (point-min))))
        (when (and (one-window-p t)
                   (fboundp 'shrink-frame-to-fit) ; Defined in `shrink-fit'.
                   Info-fit-frame-p)
          (shrink-frame-to-fit)))
    ;; If we did not finish finding the specified node,
    ;; go back to the previous one.
    (or Info-current-node no-going-back (null Info-history)
        (let ((hist (car Info-history)))
          (setq Info-history (cdr Info-history))
          (Info-find-node (nth 0 hist) (nth 1 hist) t)
          (goto-char (nth 2 hist))))))



;; REPLACES ORIGINAL in `info.el':
;; 1. Added in-progress message ("Looking...")
;; 2, Returns nil if not found.
(defun Info-find-emacs-command-nodes (command)
  "Return a list of locations documenting COMMAND.
The `info-file' property of COMMAND says which Info manual to search.
If COMMAND has no property, the variable `Info-file-list-for-emacs'
defines heuristics for which Info manual to try.
The locations are of the format used in Info-history, i.e.
\(FILENAME NODENAME BUFFERPOS\)."
  (let ((where '())
	(cmd-desc (concat "^\\* +" (regexp-quote (symbol-name command))
			  ":\\s *\\(.*\\)\\.$"))
	(info-file "emacs"))		;default
    ;; Determine which info file this command is documented in.
    (if (get command 'info-file)
	(setq info-file (get command 'info-file))
      ;; If it doesn't say explicitly, test its name against
      ;; various prefixes that we know.
      (let ((file-list Info-file-list-for-emacs))
	(while file-list
	  (let* ((elt (car file-list))
		 (name (if (consp elt)
			   (car elt)
			 elt))
		 (file (if (consp elt) (cdr elt) elt))
		 (regexp (concat "\\`" (regexp-quote name)
				 "\\(\\'\\|-\\)")))
	    (if (string-match regexp (symbol-name command))
		(setq info-file file file-list nil))
	    (setq file-list (cdr file-list))))))
    (message "Looking for command `%s' in Info manual `%s' ..."
             command (file-name-nondirectory info-file))
    (save-excursion
      (condition-case nil
	  (Info-find-node info-file "Command Index")
	;; Some manuals may not have a separate Command Index node,
	;; so try just Index instead.
	(error
         (condition-case nil
             (Info-find-node info-file "Index")
           (error nil))))               ; Return nil: not found.
      ;; Take the index node off the Info history.
      (setq Info-history (cdr Info-history))
      (goto-char (point-max))
      (while (re-search-backward cmd-desc nil t)
	  (setq where (cons (list Info-current-file
				  (buffer-substring
				   (match-beginning 1)
				   (match-end 1))
				  0)
			    where)))
      where)))


;; REPLACES ORIGINAL in `info.el':
;; 1. Uses `completing-read' in interactive spec, with `symbol-nearest-point'
;;    (defined in `thingatpt+.el') or `symbol-at-point' (defined in `thingatpt.el').
;; 2. Message if single node found.
;; 3. Returns `num-matches' if found; nil if not.
;; 4. Uses `display-in-minibuffer' (defined in `strings.el') instead of `message'.
;;;###autoload
(defun Info-goto-emacs-command-node (command)
  "Go to the Info node in the Emacs manual for command COMMAND.
The command is found by looking it up in the manual's Command Index,
or in another manual found via COMMAND's `info-file' property or
the variable `Info-file-list-for-emacs'."
  (interactive
   (let ((symb (cond ((fboundp 'symbol-nearest-point) (symbol-nearest-point))
                     ((fboundp 'symbol-at-point) (symbol-at-point))
                     (t nil)))
         (enable-recursive-minibuffers t))
     (list (intern (completing-read "Find documentation for command: "
                                    obarray 'commandp t symb nil symb t)))))
  (unless (commandp command)
    (signal 'wrong-type-argument (list 'commandp command)))
  (let ((where (Info-find-emacs-command-nodes command)))
    (if where
	(let ((num-matches (length where)))
	  ;; Get Info running, and pop to it in another window.
	  (save-window-excursion (info))
	  ;; FIXME It would be cool if this could use a buffer other
	  ;; than *info*.
	  (pop-to-buffer "*info*")
	  (Info-find-node (caar where) (cadr (car where)))
	  (if (<= num-matches 1)
              (when (interactive-p)
                (if (fboundp 'display-in-minibuffer)
                    (display-in-minibuffer
                     'event "This info node documents command `"
                     (list info-msg-face command)
                     "'.")
                  (message "This info node documents command `%s'." command)))
            ;; `Info-find-node' already pushed (car where) onto
            ;; `Info-history'.  Put the other nodes that were found on
            ;; the history.
            (setq Info-history (nconc (cdr where) Info-history))
            (when (interactive-p)
              (message "Found %d other entr%s.  Use %s to see %s."
                       (1- num-matches)
                       (if (> num-matches 2) "ies" "y")
                       (substitute-command-keys "\\<Info-mode-map>\\[Info-last]")
                       (if (> num-matches 2) "them" "it"))))
          num-matches)                  ; Return num-matches found.
      (and (interactive-p)              ; Return nil for unfound.
           (error "No documentation found for command `%s'." command)))))



;; REPLACES ORIGINAL in `info.el':
;; 1. If key's command is not found, then `Info-search' for key sequence in text.
;;    Message for repeating.
;; 2. Treats `execute-extended-command-and-check-for-bindings' like
;;    `execute-extended-command'.
;; 3. Uses `display-in-minibuffer' (defined in `strings.el') instead of `message'.
;;;###autoload
(defun Info-goto-emacs-key-command-node (key)
  "Go to the Info node in the Emacs manual for the command bound to KEY.
KEY is a string.

Interactively, if the binding is `execute-extended-command', then a
command is read.

The command is found by looking it up in Emacs manual's Command Index,
or in another manual's index found via COMMAND's `info-file' property
or the variable `Info-file-list-for-emacs'.

If key's command cannot be found by looking in indexes, then
`Info-search' is used to search for the key sequence in the info text."
  (interactive "kFind documentation for key:")
  (let* ((command (lookup-key global-map key))
         (pp-key (key-description key)))
    (when (natnump command)             ; E.g. menu item.
      (setq command (key-binding key)))
    (cond ((null command)
           (when (interactive-p)
             (if (fboundp 'display-in-minibuffer)
                 (display-in-minibuffer
                  'event "No doc found for key sequence `"
                  (list info-msg-face pp-key)
                  "'.")
               (message "No doc found for key sequence `%s'." pp-key)))
           nil)                         ; RETURN nil: not found.
	  ((and (interactive-p);; Read a new command name.
		(memq command
                      '(execute-extended-command
                        execute-extended-command-and-check-for-bindings)))
	   (Info-goto-emacs-command-node
	    (read-command "Find documentation for command: ")))
	  (t
           (let ((this-file Info-current-file)
                 (this-node Info-current-node)
                 (num-cmd-matches (Info-goto-emacs-command-node command)))
             (cond (num-cmd-matches
                    ;; Found key's command via a manual index.
                    (when (interactive-p)
                      (if (<= num-cmd-matches 1)
                          (if (fboundp 'display-in-minibuffer)
                              (display-in-minibuffer
                               'event "This info node documents key `"
                               (list info-msg-face pp-key)
                               "'.")
                            (message "This info node documents key `%s'." pp-key))
                        (message (substitute-command-keys
                                  "Found %d other entr%s.  \
Use \\<Info-mode-map>`\\[Info-last]' to see %s.")
                                 (1- num-cmd-matches) (if (> num-cmd-matches 2) "ies" "y")
                                 (if (> num-cmd-matches 2) "them" "it"))))
                    num-cmd-matches)    ; RETURN num-cmd-matches: found.
                   (t  ;; Couldn't find key's command via a manual index.
                    ;; Get back to where we were.
                    ;; Would be better if there were a save-xxx-excursion-xxx
                    ;; that would work.
                    (Info-goto-node (concat "(" this-file ")" this-node)) 
                    ;; Would be better to now try looking for the key in indexes (e.g. Key
                    ;; Index). Instead, just look for the key sequence in the text.
                    (when (interactive-p)
                      (message "Not found using Index. Searching for \"%s\" in text ..."
                               pp-key)
                      (sit-for 3))
                    (condition-case err
                        (progn
                          (Info-search (regexp-quote pp-key))
                          (when (interactive-p)
                            (if (fboundp 'display-in-minibuffer)
                                (display-in-minibuffer
                                 'event
                                 "Use \\<Info-mode-map>`"
                                 (list info-msg-face
                                       (substitute-command-keys
                                        "\\[Info-search] RET"))
                                 "' to search again for `"
                                 (list info-msg-face pp-key)
                                 "'.")
                              (message (substitute-command-keys
                                        "Use \\<Info-mode-map>`\\[Info-search] RET' \
to search again for `%s'.")
                                       pp-key)))
                          t)            ; RETURN t: found.
                      (search-failed
                       (message "No documentation found for key `%s'." pp-key)
                       nil)))))))))     ; RETURN nil: not found.


;; REPLACES ORIGINAL in `info.el':
;; 1. File name in face `info-file-face'.
;; 2. Node names in face `info-node-face'.
;; 3. Menu items in face `info-menu-face'.
;; 4. Only 5th and 9th menu items have their `*' colored.
;; 5. Notes in face `info-xref-face'.
;; 6. If `info-fontify-quotations-p', quotations in `info-quoted-name-face' face.
;; 7. If `info-fontify-strings-p', strings in `info-string-face'.
;;;###autoload
(defun Info-fontify-node ()
  (save-excursion
    (let ((buffer-read-only nil)
          (case-fold-search t))
      (goto-char (point-min))
      ;; Header line.
      (when (looking-at "^File: \\([^,: \t]+\\),?[ \t]+")
        (put-text-property (match-beginning 1) (match-end 1)
                           'face info-file-face)
        (goto-char (match-end 0))
        ;; Node names in menu at top of buffer.
        (while (looking-at "[ \t]*\\([^:, \t\n]+\\):[ \t]+\\([^:,\t\n]+\\),?")
          (goto-char (match-end 0))
	  (if (save-excursion
		(goto-char (match-beginning 1))
		(save-match-data (looking-at "Node:")))
	      (put-text-property (match-beginning 2) (match-end 2)
				 'face info-node-face)
	    (put-text-property (match-beginning 2) (match-end 2)
			       'face info-xref-face)
	    (put-text-property (match-beginning 2) (match-end 2)
			       'mouse-face 'highlight))))
      (goto-char (point-min))
      ;; Text headings: replace ***'s, ---'s, ==='s by faces.
      (while (re-search-forward "\n\\([^ \t\n].+\\)\n\\(\\*+\\|=+\\|-+\\)$"
				nil t)
	(put-text-property (match-beginning 1) (match-end 1)
			   'face
			   (cdr (assq (preceding-char) Info-title-face-alist)))
	;; This is a serious problem for trying to handle multiple
	;; frame types at once.  We want this text to be invisible
	;; on frames that can display the font above.
	(if (memq (framep (selected-frame)) '(x pc w32 win32))
	    (put-text-property (match-end 1) (match-end 2)
			       'invisible t)))
      (goto-char (point-min))
      ;; Cross references.
      (while (re-search-forward "\\*Note[ \n\t]+\\([^:]*\\):" nil t)
        (unless (= (char-after (1- (match-beginning 0))) ?\") ; hack
          (put-text-property (match-beginning 1) (match-end 1)
                             'face info-xref-face)
          (put-text-property (match-beginning 1) (match-end 1)
                             'mouse-face 'highlight)))
      (goto-char (point-min))
      ;; Menus.
      (when (and (search-forward "\n* Menu:" nil t)
                 ;; Fontify indexes too.
                 ;;(not (string-match "\\<Index\\>" Info-current-node))
                 ;; Don't take time to annotate huge menus
                 (< (- (point-max) (point)) Info-fontify-maximum-menu-size))
        (let ((n 0))
          (while (re-search-forward "^\\* +\\([^:\t\n]*\\):" nil t)
            (setq n (1+ n))
            (when (memq n '(5 9))       ; visual aids to help with 1-9 keys
              (put-text-property (match-beginning 0)
                                 (1+ (match-beginning 0))
                                 'face info-menu-face)) ; was: info-menu-5
            (put-text-property (match-beginning 1) (match-end 1)
                               'face info-menu-face) ; was: info-xref
            (put-text-property (match-beginning 1) (match-end 1)
                               'mouse-face 'highlight))))
      (when info-fontify-quotations-p
        (goto-char (point-min))
        (while (re-search-forward "`\\([^']+\\)'" nil t)
          (put-text-property (match-beginning 1) (match-end 1)
                             'face info-quoted-name-face)))
      (when info-fontify-strings-p
        (goto-char (point-min))
        (while (re-search-forward "\"\\([^\"]+\\)\"" nil t)
          (put-text-property (match-beginning 1) (match-end 1)
                             'face info-string-face)))
      (set-buffer-modified-p nil))))


;; REPLACES ORIGINAL in `info.el':
;; Highlights the found regexp if `search-highlight'.
;;;###autoload
(defun Info-search (regexp)
  "Search for REGEXP, starting from point, and select node it's found in.

Highlights current location of found regexp if `search-highlight'.
Note that the highlighting remains, after the search is over.
To remove the highlighting, just start an incremental search: \
`\\[isearch-forward]'."
  (interactive "sSearch (regexp): ")
  (when transient-mark-mode (deactivate-mark))
  (if (equal regexp "")
      (setq regexp Info-last-search)
    (setq Info-last-search regexp))
  (when regexp
    (prog1
        (let ((found ()) current
              (onode Info-current-node)
              (ofile Info-current-file)
              (opoint (point))
              (ostart (window-start))
              (osubfile Info-current-subfile))
          (save-excursion
            (save-restriction
              (widen)
              (if (null Info-current-subfile)
                  (progn (re-search-forward regexp) (setq found (point)))
                (condition-case err
                    (progn (re-search-forward regexp) (setq found (point)))
                  (search-failed nil)))))
          ;; Can only happen in subfile case -- else would have erred.
          (unless found
            (unwind-protect
                (let ((list ()))
                  (save-excursion
                    (set-buffer (marker-buffer Info-tag-table-marker))
                    (goto-char (point-min))
                    (search-forward "\n\^_\nIndirect:")
                    (save-restriction
                      (narrow-to-region (point)
                                        (progn (search-forward "\n\^_") (1- (point))))
                      (goto-char (point-min))
                      (search-forward (concat "\n" osubfile ": "))
                      (beginning-of-line)
                      (while (not (eobp))
                        (re-search-forward "\\(^.*\\): [0-9]+$")
                        (goto-char (+ (match-end 1) 2))
                        (setq list (cons (cons (read (current-buffer))
                                               (buffer-substring (match-beginning 1)
                                                                 (match-end 1)))
                                         list))
                        (goto-char (1+ (match-end 0))))
                      (setq list (nreverse list))
                      (setq current (caar list))
                      (setq list (cdr list))))
                  (while list
                    (message "Searching subfile `%s' ..." (cdr (car list)))
                    (Info-read-subfile (car (car list)))
                    (setq list (cdr list))
                    ;; (goto-char (point-min))
                    (when (re-search-forward regexp nil t)
                      (setq found (point) list ())))
                  (if found
                      (message "")
                    (signal 'search-failed (list regexp))))
              (unless found
                (Info-read-subfile osubfile)
                (goto-char opoint)
                (Info-select-node)
                (set-window-start (selected-window) ostart))))
          (widen)
          (goto-char found)
          (when search-highlight
            (isearch-highlight (match-beginning 0) (match-end 0)))
          (Info-select-node)
          ;; Use string-equal, not equal, to ignore text props.
          (or (and (string-equal onode Info-current-node)
                   (equal ofile Info-current-file))
              (setq Info-history (cons (list ofile onode opoint) Info-history))))
      (when (interactive-p)
        (message (substitute-command-keys
                  "Use \\<Info-mode-map>`\\[Info-search] RET' to search again for `%s'.")
                 regexp)))))



(or (fboundp 'old-Info-mode) (fset 'old-Info-mode (symbol-function 'Info-mode)))

;; REPLACES ORIGINAL in `info.el':
;; Doc string changed: displays all bindings.
;;;###autoload
(defun Info-mode ()
  "\\<Info-mode-map>
Info mode provides commands for browsing through the Info doc tree.
Documentation in Info is divided into \"nodes\", each of which discusses
one topic and contains hyperlink references to other nodes which
discuss related topics.  Info has commands to follow the references.
The most important commands to know are: \
`\\[Info-mouse-follow-nearest-node]', `\\[Info-last]', and `\\[Info-search]'.

Help commands
-------------
\\[describe-mode]\tDisplay this help.
\\[Info-help]\tThe Info tutorial.  Learn about Info while using it.

Selecting other nodes (basic)
-----------------------------
\\[Info-mouse-follow-nearest-node]\tFollow a node reference you click on.  \
This works with menu
\titems, cross references ( \"(*note...)\" ), \"Next\", \"Previous\"
\tand \"Up\" references, depending on where you click.
\tAt end of node's text, goes to \"Next\", or \"Up\" if no \"Next\".
\\[Info-follow-nearest-node]\tLike `\\[Info-mouse-follow-nearest-node]', \
except cursor location, not mouse location.
\\[Info-last]\tGo back to the last node you were at. (chronological)

Structural navigation commands
------------------------------
\\[Info-menu]\tGo to a menu item's node.  Completion available for its name.
1\tGo to first menu item's node.
2, 3, 4, 5, 6, 7, 8, 9\tGo to second ... ninth menu item's node.
\\[Info-next]\tGo to this node's \"Next\" node.
\\[Info-prev]\tGo to this node's \"Previous\" node.  (*not* chronological)
\\[Info-up]\tGo \"Up\" from this node to its parent node.
\\[Info-directory]\tGo to the Info directory (root) node.
\\[Info-index]\tLook up a topic in this file's Index and move to its node.
\\[Info-index-next]\t(comma) Go to the next match from a previous \
`\\[Info-index]' command.
\\[Info-follow-reference]\tFollow a cross reference ( \"(*note...)\" ).  \
Reads ref name.

Moving within a node
--------------------
\\[Info-scroll-up]\tNormally, scroll forward a full screen.  If end of \
buffer is
\talready visible, try to go to next menu entry, or up if none.
\\[Info-scroll-down]\tNormally, scroll backward.  If beginning of buffer is \
already
\tvisible, try to go to previous menu entry, or up if none.
\\[beginning-of-buffer]\tGo to beginning of node.  
\\[Info-next-reference]\tMove cursor to next cross-reference or menu item in \
this node.
\\[Info-prev-reference]\tMove cursor to previous cross-reference or menu item.

Other navigation commands
-------------------------
\\[Info-goto-node]\tGo to a node with a given name.  (No completion available.)
\tYou may include a filename as well, as \"(FILENAME)NODENAME\".
\\[universal-argument] \\[info]\tGo to a new Info file.  (Completion \
available.)
\\[Info-top-node]\tGo to first node (\"Top\") of current Info file.
\\[Info-final-node]\tGo to final node of current Info file.
\\[Info-forward-node]\tGo forward a node, considering all nodes as one \
sequence.
\\[Info-backward-node]\tGo backward a node, considering all nodes as one \
sequence.

Other commands
--------------
\\[Info-search]\tSearch through nodes of this Info file for specified regexp,
\tand go to node where next occurrence is found.
\tNote that other search functions (e.g. `\\[isearch-forward]') will only \
search
\twithin the current node.
\\[Info-edit]\tEdit contents of current node.  \
\(\\<Info-edit-map>`\\[Info-cease-edit]'\\<Info-mode-map> to end editing.)
\tEditing is enabled only if `Info-enable-edit' is non-nil.
\\[Info-merge-subnodes]\tIntegrate current node with nodes referred to \
in its Menu.
\tDisplay the result outside of Info.  `\\[universal-argument]': Recursively.
\\[Info-exit]\tQuit Info.

User options
------------
Some user options (variables you can define, via `defvar'):

`Info-subtree-separator' - See `Info-merge-subnodes'.
`info-fontify-quotations-p' - Non-nil => fontify names between quotes
`info-fontify-strings-p' - Non-nil => fontify strings
`info-file-face' - Face used for file heading labels
`info-node-face' - Face used for node heading labels
`info-menu-face' - Face used for menu items
`info-xref-face' - Face used for cross reference \"Note\" links
`info-quoted-name-face' - Face used for quoted names (e.g. `toto').
`info-string-face' - Face used for strings (e.g. \"toto\").

These are all of the current Info Mode bindings:

\\{Info-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'Info-mode)
  (setq mode-name "Info")
  (setq tab-width 8)
  (use-local-map Info-mode-map)
  (make-local-hook 'activate-menubar-hook)
  (add-hook 'activate-menubar-hook 'Info-menu-update nil t)
  (set-syntax-table text-mode-syntax-table)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq case-fold-search t)
  (setq buffer-read-only t)
  (make-local-variable 'Info-current-file)
  (make-local-variable 'Info-current-subfile)
  (make-local-variable 'Info-current-node)
  (make-local-variable 'Info-tag-table-marker)
  (setq Info-tag-table-marker (make-marker))
  (make-local-variable 'Info-tag-table-buffer)
  (setq Info-tag-table-buffer nil)
  (make-local-variable 'Info-history)
  (make-local-variable 'Info-index-alternatives)
  ;; This is for the sake of the invisible text we use handling titles.
  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  (Info-set-mode-line)
  (run-hooks 'Info-mode-hook))


;;;###autoload
(defvar Info-subtree-separator "\n* "
  "*A string used to separate Info node descriptions. 
Inserted by `Info-merge-subnodes' just before each node title.
Setting this to a string that includes a form-feed (^L), such as
\"\f\n* \", will cause a page break before each node description.

Use command `set-variable' to set this, quoting any control characters
you want to include, such as form-feed (^L) and newline (^J), with ^Q.
For example, type `^Q^L^Q^J* ' to set this to \"\f\n* \".")
(put 'Info-subtree-separator 'variable-interactive
     "sString to be inserted before node titles (^Q to quote control chars): ")

;;;###autoload
(defvar Info-display-node-header-fn 'Info-display-node-default-header
  "*Function to insert header by `Info-merge-subnodes'.")

(defun Info-display-node-default-header ()
  "Insert node name as header."
  (insert (if (fboundp 'concat-w-faces)
              ;; `node-name' is free here - bound in `Info-merge-subnodes'.
              (concat-w-faces (list info-node-face node-name)) 
            node-name)
          "\n")
  (beginning-of-buffer)
  (center-line 2))

(defun Info-display-node-time-header ()
  "Insert current time and node name as header."
  ;; `node-name' is free here - bound in `Info-merge-subnodes'.
  (insert (current-time-string) "    " node-name)
  (beginning-of-buffer)
  (center-line))



;; Note: This is not super-clean code (it's kind of a hack job).
;;;###autoload
(defun Info-merge-subnodes (&optional recursive-display-p recursive-call-p)
  "Integrate current node with nodes referred to in its Menu.

Displays the current Info node, together with the nodes in its Menu.
Buffer `*Info: NODE*' is used for the display, where NODE is the name
of the current node.  The contents of this node's subnodes (the nodes
named in this node's Menu) are included in the buffer, following the
contents of the current node.

Optional arg RECURSIVE-DISPLAY-P (prefix arg if interactive) governs
the way menus of subnodes are treated:

  If nil, nothing additional happens.  Subnode menus are not explored.
  Only the current node and its immediate subnodes are documented, in
  the single display buffer `*Info: NODE*'.

  If non-nil, then the subnodes of a node are treated in the same way
  as the parent node, recursively: If any of them has, itself, a Menu,
  then that menu's subnodes are also explored, and so on.

    If RECURSIVE-DISPLAY-P is zero, then a single display buffer is
    used for all of the nodes explored.  Otherwise, a separate display
    buffer is used for each subnode that has a Menu (see next).

      Use this when you want a single, flat compilation of the current
      node and all of its subnodes.  It is less appropriate when the
      current node has several levels of subnodes: The flattened
      result can be difficult to read.

    If RECURSIVE-DISPLAY-P is positive, then the contents of each
    subnode are displayed twice: once in the parent node's display,
    and once in the subnode's own display.

      Use this when the current node has several levels of subnodes
      and you want each display buffer to be self-contained.

    If RECURSIVE-DISPLAY-P is negative, then there is no redundancy: A
    subnode's contents are only displayed in its parent's buffer.  The
    subnode's own display buffer only contains the contents of its own
    subnodes.

      Use this when the current node has several levels of subnodes
      and you want no redundancy between the display buffers.

The user option (variable) `Info-subtree-separator' is a string to be
inserted by `Info-merge-subnodes' just before the title of each
node (preceding its description).  By default it is \"\\n* \", producing
a node title resembling a menu item.  Setting this to \"\\f\\n* \" will
cause a page break before each node description.  For more on setting
this variable, type \\<Info-mode-map>`\\[describe-variable] Info-subtree-separator'.

------

Optional second arg RECURSIVE-CALL-P is only for internal use.  It is
used to indicate whether (non-nil) or not (nil) this is a recursive
\(i.e. not a top-level) call to `Info-merge-subnodes'.  Non-nil
means that this is a subnode, and that its contents should only be
included in the present display if RECURSIVE-DISPLAY-P is also
non-nil.  For proper operation when RECURSIVE-DISPLAY-P is zero, the
non-nil value of RECURSIVE-CALL-P should be the node name of the
top-level call to `Info-merge-subnodes'."
  (interactive "P")
  (when (interactive-p)
    (unless (y-or-n-p "Do you really want to integrate this node with its \
subnodes (outside Info)? ")
      (error (substitute-command-keys
              "OK.  If you are not sure what this command is about, type \
`\\[describe-function] Info-merge-subnodes'.")))) ; Defined in `help.el'.
  (garbage-collect)
  (setq recursive-display-p (and recursive-display-p
                                 (prefix-numeric-value recursive-display-p)))
  (let* ((buf (current-buffer))
         (single-buf-p (and recursive-display-p (zerop recursive-display-p)))
         (node-name (or (and single-buf-p recursive-call-p)
                        Info-current-node))
         (rep-buf (get-buffer-create (concat "*Info: " node-name "*")))
         (more t)
         token oldpt strg menu-item-line ind)
    (when (interactive-p)
      (message "Processing node `%s' and %ssubnodes ..." node-name
               (if recursive-display-p "all of its " "its immediate ")))
    (save-window-excursion
      (goto-char (point-min))
      (forward-line 1)
      (setq strg (buffer-substring (point) (point-max))) ; Node contents.
      (goto-char (point-min))
      (setq more (search-forward "* menu" nil t))
      (forward-line 1)
      (switch-to-buffer-other-window rep-buf)
      (unless (and recursive-call-p single-buf-p)
        (erase-buffer)
        (funcall Info-display-node-header-fn) ; Insert header
        ;; Insert main node's contents, unless recursive and don't want redundancy.
        (insert "\n\n" (and (or (not recursive-call-p) ; Top-level call.
                                (and recursive-display-p ; Redundancy desired.
                                     (> recursive-display-p 0)))
                            strg)))
      ;; Insert each subnode.
      (save-excursion
        (while more
          (set-buffer buf)
          (end-of-line)
          (setq oldpt (point))
          (setq more (search-forward "\n* " nil t))
          (unless more (goto-char (point-max)))
          (while (and (not (eobp))
                      (not (setq token (Info-get-token ; file menu item
                                        (point) "\\* " "\\* \\([^:]*\\)::")))
                      (not (setq token (Info-get-token ; nonfile menu item
                                        (point) "\\* "
                                        "\\* [^:]*:[ \t]+\\([^\t,.\n]+\\)[\t,.\n]"))))
            (setq more (search-forward "\n* " nil t)))
          (unless token (setq more nil)) ; No menu item.
          (when more
            (set-buffer rep-buf)
            (goto-char (point-max))
            (insert Info-subtree-separator) ; Ready for next menu item.
            (set-buffer buf)
            (Info-goto-node token)
            (goto-char (point-min))
            (forward-line 1)
            (setq strg (buffer-substring (point) (point-max))) ; Sub contents.
            (Info-last)
            ;; Get untabified `menu-item-line', so can count chars to underline.
            (let ((inhibit-read-only t))
              (buffer-enable-undo) (undo-start)
              (untabify (point) (save-excursion (forward-line 1) (point)))
              (setq menu-item-line
                    (buffer-substring-no-properties
                     (save-excursion (beginning-of-line)(forward-char 2) (point))
                     (save-excursion (forward-line 1) (point))))
              (when pending-undo-list (undo-more 1)) ; Only if did something.
              (buffer-disable-undo))
            (set-buffer rep-buf)
            (insert menu-item-line)
            (setq ind (1+ (length menu-item-line)))
            (while (> ind 0) (insert "=") (decf ind)) ; Underline menu item.
            (insert "\n")
            (put-text-property (save-excursion (forward-line -2) (point))
                               (save-excursion (forward-line 1) (point))
                               'face info-file-face)
            (setq oldpt (point))
            (insert strg)               ; Subnode contents.
            (indent-rigidly oldpt (point) 2)
            (save-excursion             ; Display node's submenu, if any.
              (when recursive-display-p
                (save-excursion
                  (set-buffer buf)
                  (Info-goto-node token)
                  (when (search-forward "* menu" nil t)
                    (forward-line 1) (end-of-line)
                    (when (and (search-forward "\n* " nil t)
                               (Info-get-token (point) "\\* "
                                               "\\* \\([^:]*\\)::"))
                      (Info-merge-subnodes recursive-display-p node-name)))
                  (Info-last))))
            (set-buffer buf)))))
    (switch-to-buffer-other-window rep-buf)    
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (use-local-map Info-merged-map)
    (when (interactive-p)
      (message "Processing node `%s' and %ssubnodes ... done." node-name
               (if recursive-display-p "all of its " "its immediate ")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `info+.el' ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smi.el -- Simple MH interface for Emacs
;; ITIID           : $ITI$ $Header $__Header$
;; Author          : Kai Grossjohann
;; Created On      : Wed Jun  8 11:38:17 1994
;; Last Modified By: Kai Grossjohann
;; Last Modified On: Fri Jul 29 21:12:23 1994
;; Update Count    : 347
;; Status          : Unknown, Use with caution!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq debug-on-error t)

;; -- Variables --

(defvar smi-mailpath "~/Mail/"
  "* Parent directory of all folders.")

(defvar smi-use-bbdb t
  "* Should be true iff you want to use BBDB.
You must set this BEFORE loading smi!")

(defvar smi-folder-list-show-empty nil
  "* Should be true iff you want to see empty folders in browse folders mode.")

(defvar smi-yank-original-hooks nil
  "* Run this for citation.
Mark is at end of original message, point at beginning.")

(defvar smi-comp-program "comp"
  "* Command used for invoking the comp program.")

(defvar smi-scan-program "scan"
  "* Command used for invoking the scan program.")

(defvar smi-inc-program "inc"
  "* Command used for invoking the inc program.")

(defvar smi-show-program "show"
  "* Command used for invoking the show program.")

(defvar smi-rmm-program "rmm"
  "* Command used for invoking the rmm program.")

(defvar smi-refile-program "refile"
  "* Command used for invoking the refile program.")

(defvar smi-repl-program "repl"
  "* Command used for invoking the repl program.")

(defvar smi-forw-program "forw"
  "* Command used for invoking the forw program.")

(defvar smi-pack-command "folder -pack"
  "* Command used for renumbering 1...n of the messages in a folder.")

(defvar smi-comp-noedit-switch "-nowhatnowproc"
  "* Switch to be given to the comp program not to edit the draft.")

(defvar smi-repl-noedit-switch smi-comp-noedit-switch
  "* Switch to be given to the repl program not to edit the draft.")

(defvar smi-forw-noedit-switch smi-comp-noedit-switch
  "* Switch to be given to the forw program not to edit the draft.")

(defvar smi-show-draft-filename-command "show -draft -showproc ls"
  "* Command for printing the name of the current draft file to stdout.")

(defvar smi-send-draft-command "send `show -draft -showproc ls`"
  "* Command for sending the draft file.")

(defvar smi-folder-list-command "folders -noheader"
  "* Command for outputting a list of folders.")

(defvar smi-cat-msg-command "show -showproc cat"
  "* Command for `cat'ing a message to stdout.  Folder and message number
will be appended to this command for execution.")

(defvar smi-my-name "        \\kai{}\n"
  "* String to be inserted as my name.")

(defvar smi-signature-file-name "~/.signature"
  "* Name of signature file.")

(defvar smi-buffer "*SMI*"
  "Name of buffer used for the smi-process.")

(defvar smi-current-folder ""
  "Name of the current folder.")

(defvar smi-folders nil
  "List of folder names.")

(defvar smi-current-msg ""
  "Number of the current message, as a string.")

(defvar smi-folder-sets nil
  "This is an alist of pairs of a folder set name and a regexp matching
all folder names to be shown.")

(defvar smi-current-folder-set "all")   ; being defined in function smi
(defvar smi-default-folder-set "all")   ; being defined in function smi

(defvar smi-show-empty-folders t
  "Folders with 0 messages in them will be shown in folder list iff t")

(defvar smi-buffer-name-prefix "SMI: "
  "Prefix used for all SMI buffers.")

(defvar smi-field-alist
  (list (cons "To: " nil)
        (cons "Cc: " nil)
        (cons "Fcc: " nil)
        (cons "Bcc: " nil)
        (cons "Subject: " nil)
        )
  "* Alist of fields to go to using smi-to-field.")

(defvar smi-list-foldernames-command "folders -fast"
  "* Command to use to list (just) the names of the folders.")

;; -- Util functions --

(defun smi-exec (cmd &rest arglist)
  "Execute given cmd with given arguments.  The smi-buffer afterwards
contains the output if any."
  (set-buffer (get-buffer-create smi-buffer))
  (erase-buffer)
  (apply 'call-process cmd nil t nil arglist))

(defun smi-shell-exec (cmdline)
  "Execute given cmdline using a shell.  The smi-buffer afterwards
contains the output if any."
  (set-buffer (get-buffer-create smi-buffer))
  (make-variable-buffer-local 'explicit-shell-file-name)
  (setq explicit-shell-file-name "/bin/sh")
  (erase-buffer)
  (shell-command cmdline t))

(defun smi-read-folder-name ()
  "Read from minibuffer the name of a folder."
  (completing-read "Folder: "
                   (mapcar (function (lambda (x) (cons x nil)))
                           smi-folders)
                   nil
                   nil
                   "+"))

(defun smi-get-current-message-number ()
  "Return as a string the number of the current message."
  (end-of-line)
  (let ((eolpos (point)))
    (beginning-of-line)
    (cond ( (search-forward-regexp "[0-9]+" eolpos t)
            (beginning-of-line)
            (buffer-substring (match-beginning 0) (match-end 0)) )
          ( t
            (message "No message.")
            nil))))

(defun smi-shell-command-to-stringlist (cmd)
  "Run string COMMAND in an inferior shell and return output as list of strings."
 (interactive "sCommand: ")
  (save-excursion
    (let ((buf (get-buffer-create smi-buffer)))
      (set-buffer buf)
      (unwind-protect
	  (progn
	    (erase-buffer)
            (smi-shell-exec cmd)
	    (goto-char 1)
	    (let (answer)
	    (insert "(setq answer '(\"" )
	    (while (search-forward "\n" nil t)
	      (delete-backward-char 1)
	      (insert "\"\n\"")
	      )  
	    (delete-backward-char 1)
	    (insert "))")
	    (eval-buffer)             ;  
	;   (kill-buffer buf)
	    answer)                   ; return this
	)))))

(defun smi-get-foldernames ()
  "Set smi-folders to a list of foldernames.  Each name contains a + as
the first character."
  (setq smi-folders (mapcar (function (lambda (x) (concat "+" x)))
                            (smi-shell-command-to-stringlist
                             smi-list-foldernames-command))))

;; -- Modes --

;; Compose letter mode

(defvar smi-comp-mode-map (make-sparse-keymap)
  "Keymap to be used in smi-comp-mode.")

(define-key smi-comp-mode-map "\C-c\C-c" 'smi-send)
(define-key smi-comp-mode-map "\C-c\C-q" 'smi-abort-comp)
(define-key smi-comp-mode-map "\C-c\C-y" 'smi-yank-original)
(define-key smi-comp-mode-map "\C-c\C-r" 'sc-recite-region)
(define-key smi-comp-mode-map "\C-c\C-n" 'smi-insert-my-name)
(define-key smi-comp-mode-map "\C-c\C-s" 'smi-insert-signature)
(define-key smi-comp-mode-map "\C-c\C-f" 'smi-to-field)
(if smi-use-bbdb
    (define-key smi-comp-mode-map "\M-\C-i" 'bbdb-complete-name))

(defun smi-comp-mode ()
  (interactive)
  (text-mode)
  (use-local-map smi-comp-mode-map)
  (setq major-mode 'smi-comp-mode)
  (setq mode-name "SMI Compose Letter"))

;; Scan folder mode

(defvar smi-scan-mode-map (make-sparse-keymap)
  "Keymap to be used in smi-scan-mode.")
(suppress-keymap smi-scan-mode-map)

(define-key smi-scan-mode-map "." 'smi-show)
(define-key smi-scan-mode-map " " 'smi-show)
(define-key smi-scan-mode-map "n" 'smi-next-msg)
(define-key smi-scan-mode-map "p" 'smi-prev-msg)
(define-key smi-scan-mode-map "<" 'smi-first-msg)
(define-key smi-scan-mode-map ">" 'smi-last-msg)
(define-key smi-scan-mode-map "d" 'smi-rmm)
(define-key smi-scan-mode-map "D" 'smi-rmm-directly)
(define-key smi-scan-mode-map "f" 'smi-refile)
(define-key smi-scan-mode-map "F" 'smi-refile-directly)
(define-key smi-scan-mode-map "m" 'smi-comp)
(define-key smi-scan-mode-map "c" 'smi-comp)
(define-key smi-scan-mode-map "g" 'smi-rescan-current-folder)
(define-key smi-scan-mode-map "i" 'smi-inc-and-rescan)
(define-key smi-scan-mode-map "r" 'smi-repl)
(define-key smi-scan-mode-map "a" 'smi-repl)
(define-key smi-scan-mode-map "z" 'smi-forw)
(define-key smi-scan-mode-map "v" 'smi-scan)
(define-key smi-scan-mode-map "q" 'smi-browse-folders)
(define-key smi-scan-mode-map "Z" 'smi-pack)
(define-key smi-scan-mode-map "|" 'smi-pipe-msg)

(defun smi-scan-mode ()
  (interactive)
  (use-local-map smi-scan-mode-map)
  (setq major-mode 'smi-scan-mode)
  (setq mode-name "SMI Scan Folder"))

;; Show message mode

(defvar smi-show-mode-map (make-sparse-keymap)
  "Keymap to be used in smi-show-mode.")
(suppress-keymap smi-show-mode-map)

(define-key smi-show-mode-map "j" 'smi-scroll-msg-up-one-line)
(define-key smi-show-mode-map "k" 'smi-scroll-msg-down-one-line)
(define-key smi-show-mode-map " " 'smi-scroll-msg-up)
(define-key smi-show-mode-map [backspace] 'smi-scroll-msg-down)
(define-key smi-show-mode-map "b" 'smi-scroll-msg-down)
(define-key smi-show-mode-map "<" 'beginning-of-buffer)
(define-key smi-show-mode-map ">" 'end-of-buffer)
(define-key smi-show-mode-map "q" 'smi-show-current-scan)
(define-key smi-show-mode-map "d" 'smi-rmm-show-mode)
(define-key smi-show-mode-map "f" 'smi-refile-show-mode)
(define-key smi-show-mode-map "r" 'smi-repl-show-mode)
(define-key smi-show-mode-map "a" 'smi-repl-show-mode)
(define-key smi-show-mode-map "z" 'smi-forw-show-mode)
(define-key smi-show-mode-map "m" 'smi-comp)
(define-key smi-show-mode-map "c" 'smi-comp)
(define-key smi-show-mode-map "n" 'smi-next-msg-show-mode)
(define-key smi-show-mode-map "p" 'smi-prev-msg-show-mode)
(define-key smi-show-mode-map "i" 'smi-inc-and-rescan)

(defun smi-show-mode ()
  (interactive)
  (use-local-map smi-show-mode-map)
  (setq major-mode 'smi-show-mode)
  (setq mode-name "SMI Show Message"))

;; Browse folder list mode

(defvar smi-browse-folders-mode-map (make-sparse-keymap)
  "Keymap to be used in smi-browse-folders-mode.")
(suppress-keymap smi-browse-folders-mode-map)

(define-key smi-browse-folders-mode-map "." 'smi-scan-this-folder)
(define-key smi-browse-folders-mode-map " " 'smi-scan-this-folder)
(define-key smi-browse-folders-mode-map "n" 'next-line)
(define-key smi-browse-folders-mode-map "p" 'previous-line)
(define-key smi-browse-folders-mode-map "s" 'smi-change-folder-set)
(define-key smi-browse-folders-mode-map "t" 'smi-toggle-show-empty-folders)
(define-key smi-browse-folders-mode-map "g" 'smi-browse-folders)
(define-key smi-browse-folders-mode-map "i" 'smi-inc-and-rescan)
(define-key smi-browse-folders-mode-map "v" 'smi-scan)
(define-key smi-browse-folders-mode-map "c" 'smi-comp)
(define-key smi-browse-folders-mode-map "m" 'smi-comp)

(defun smi-browse-folders-mode ()
  (interactive)
  (use-local-map smi-browse-folders-mode-map)
  (setq major-mode 'smi-browse-folders-mode)
  (setq mode-name "SMI Browse Folders")
  )

;; -- User functions --

(defun smi-scroll-msg-down-one-line ()
  (interactive)
  (scroll-down 1))

(defun smi-scroll-msg-up-one-line ()
  (interactive)
  (scroll-up 1))

(defun smi-scroll-msg-down ()
  (interactive)
  (scroll-down))

(defun smi-scroll-msg-up ()
  (interactive)
  (scroll-up))

(defun smi-first-msg ()
  (interactive)
  (beginning-of-buffer))

(defun smi-last-msg ()
  (interactive)
  (end-of-buffer)
  (if (looking-at "^$") (forward-line -1)))

(defun smi-next-msg ()
  (interactive)
  (forward-line 1)
  (if (looking-at "^$")
      (progn (forward-line -1)
             (message "Last message."))))

(defun smi-prev-msg ()
  (interactive)
  (forward-line -1))

(defun smi-next-msg-show-mode ()
  (interactive)
  (smi-show-current-scan)
  (smi-next-msg)
  (smi-show (smi-get-current-message-number)))

(defun smi-prev-msg-show-mode ()
  (interactive)
  (smi-show-current-scan)
  (smi-prev-msg)
  (smi-show (smi-get-current-message-number)))

(defun smi-send ()
  (interactive)
  (let ((curbuf (current-buffer)))
    (save-buffer)
    (smi-shell-exec smi-send-draft-command)
    (switch-to-buffer curbuf)
    (kill-buffer (current-buffer))))

(defun smi-abort-comp ()
  (interactive)
  (let ((curbuf (current-buffer))
        (msg (file-name-nondirectory buffer-file-name))
        (folder (file-name-directory buffer-file-name)))
    (save-buffer)
    (smi-shell-exec (concat smi-rmm-program " " folder " " msg))
    (switch-to-buffer curbuf))
  (kill-buffer (current-buffer)))

(defun smi-show (msg)
  (interactive (list (smi-get-current-message-number)))
  (setq smi-current-msg msg)
  (smi-exec smi-show-program smi-current-folder msg)
  (switch-to-buffer (concat smi-buffer-name-prefix
                            smi-current-folder
                            "/"
                            msg))
  (erase-buffer)
  (insert-buffer smi-buffer)
  (smi-show-mode))

(defun smi-show-current-scan ()
  (interactive)
  (kill-buffer (current-buffer))
  (switch-to-buffer (concat smi-buffer-name-prefix
                            smi-current-folder)))

(defun smi-rmm (msg)
  (interactive (list (smi-get-current-message-number)))
  (let ((curbuf (current-buffer)))
    (smi-exec smi-rmm-program msg)
    (switch-to-buffer curbuf)
    (beginning-of-line)
    (kill-line 1)
    (if (looking-at "^$")
        (progn (forward-line -1)
               (message "Last message.")))))

(defun smi-rmm-show-mode ()
  (interactive)
  (smi-show-current-scan)
  (smi-rmm (smi-get-current-message-number))
  (let ((x (smi-get-current-message-number)))
    (if x (smi-show x))))

(defun smi-refile (msg folder)
  (interactive (list (smi-get-current-message-number)
                     (smi-read-folder-name)))
  (setq smi-current-msg msg)
  (let ((curbuf (current-buffer)))
    (smi-exec smi-refile-program smi-current-msg folder)
    (switch-to-buffer curbuf)
    (beginning-of-line)
    (kill-line 1)
    (if (looking-at "^$")
        (progn (forward-line -1)
               (message "Last message.")))))

(defun smi-refile-show-mode ()
  (interactive)
  (smi-show-current-scan)
  (call-interactively 'smi-refile))

(defun smi-rescan-current-folder ()
  (interactive)
  (smi-scan smi-current-folder))

(defun smi-repl (msg folder)
  (interactive (list (smi-get-current-message-number) smi-current-folder))
  (setq smi-current-msg msg)
  (setq smi-current-folder folder)
  (smi-exec smi-repl-program
            smi-repl-noedit-switch
            smi-current-folder
            smi-current-msg)
  (let ((filename (progn (set-buffer smi-buffer)
                         (smi-shell-exec smi-show-draft-filename-command)
                         (substring (buffer-string) 0 -1))))
    (find-file filename))
  (smi-comp-mode))

(defun smi-repl-show-mode ()
  (interactive)
  (smi-show-current-scan)
  (smi-repl (smi-get-current-message-number) smi-current-folder))

(defun smi-yank-original ()
  (interactive)
  (let ((curbuf (current-buffer)))
    (smi-exec smi-show-program smi-current-folder smi-current-msg
              "-showproc" "cat")
    (switch-to-buffer curbuf)
    (insert-buffer smi-buffer)
    (kill-line 1)
    (run-hooks 'smi-yank-original-hooks)))

(defun smi-forw (msg)
  (interactive (list (smi-get-current-message-number)))
  (setq smi-current-msg msg)
  (smi-exec smi-forw-program smi-forw-noedit-switch
            smi-current-folder smi-current-msg)
  (let ((filename (progn (set-buffer smi-buffer)
                         (smi-shell-exec smi-show-draft-filename-command)
                         (substring (buffer-string) 0 -1))))
    (find-file filename))
  (smi-comp-mode))

(defun smi-forw-show-mode ()
  (interactive)
  (smi-show-current-scan)
  (smi-forw (smi-get-current-message-number)))

(defun smi-inc-and-rescan ()
  (interactive)
  (smi-exec smi-inc-program)
  (smi-scan "+inbox"))

(defun smi-pack ()
  (interactive)
  (smi-shell-exec (concat smi-pack-command " " smi-current-folder))
  (smi-rescan-current-folder))

(defun smi-define-folder-set (name regexp)
  "Add the NAME, REGEXP pair to the smi-folder-sets alist if there is no
folder set with this name in it."
  (if (assoc name smi-folder-sets)
      ()
    (setq smi-folder-sets (cons (cons name regexp) smi-folder-sets))))

(defun smi-change-folder-set (setname)
  (interactive (list (completing-read "Name of folder set: "
                                      smi-folder-sets
                                      nil
                                      t)))
  (setq smi-current-folder-set setname)
  (smi-browse-folders))

(defun smi-toggle-show-empty-folders ()
  "Toggles the variable smi-show-empty-folders, which see."
  (interactive)
  (cond (smi-show-empty-folders
         (message "Not showing empty folders.")
         (setq smi-show-empty-folders nil))
        (t
         (message "Showing empty folders.")
         (setq smi-show-empty-folders t))))

(defun smi-rmm-directly (rmm-args)
  (interactive "sCall rmm as: rmm <+current-folder> ")
  (smi-shell-exec (concat smi-rmm-program " "
                          smi-current-folder " "
                          rmm-args))
  (smi-rescan-current-folder))

(defun smi-refile-directly (refile-args)
  (interactive "sCall refile as: refile ")
  (smi-shell-exec (concat smi-refile-program " " refile-args))
  (smi-rescan-current-folder))

(defun smi-pipe-msg (cmd)
  (interactive "sPipe message through the following command:")
  (smi-shell-exec (concat smi-cat-msg-command " "
                          smi-current-folder " "
                          smi-current-msg " "
                          cmd)))

;; functions for composing a letter

(defun smi-insert-my-name ()
  (interactive)
  (insert smi-my-name))

(defun smi-insert-signature ()
  (interactive)
  (insert-file-contents smi-signature-file-name))

(defun smi-to-field (field)
  (interactive
   (list (completing-read "Go to field: "
                          smi-field-alist
                          nil
                          nil
                          nil
                          nil)))
  (let ((eoh (progn (beginning-of-buffer)
                    (or (re-search-forward "^-*$")
                        (point-max)))))
    (beginning-of-buffer)
    (cond ((re-search-forward (concat "^" field) eoh 'move-to-limit)
           (while (looking-at ".*\n[ \t]+")
             (forward-line 1))
           (end-of-line))
          (t
           (beginning-of-line)
           (insert field)
           (save-excursion (insert "\n"))
           (message "not found")))))

;; function for interfacing with gnus

(defun gnus-mail-reply-using-smi (yank)
  "Use smi to mail a reply"
  (gnus-summary-select-article gnus-save-all-headers gnus-save-all-headers)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (save-restriction
      (widen)
      (shell-command-on-region (point-min) (point-max)
                               (expand-file-name "~/bin/news-repl"))))
  (let ((filename (progn (set-buffer smi-buffer)
                         (smi-shell-exec smi-show-draft-filename-command)
                         (substring (buffer-string) 0 -1))))
    (find-file filename))
  (if yank
      (progn
        (end-of-buffer)
        (insert-buffer gnus-article-buffer)
        (exchange-point-and-mark)
        (run-hooks 'smi-yank-original-hooks)))
  (smi-comp-mode)
  )

(defun gnus-mail-forward-using-smi ()
  "Use smi to forw a news article."
  (gnus-summary-select-article gnus-save-all-headers gnus-save-all-headers)
  (gnus-eval-in-buffer-window gnus-article-buffer
    (save-restriction
      (widen)
      (shell-command-on-region (point-min) (point-max)
                               (expand-file-name "~/bin/news-forw"))))
  (let ((filename (progn (set-buffer smi-buffer)
                         (smi-shell-exec smi-show-draft-filename-command)
                         (substring (buffer-string) 0 -1))))
    (find-file filename))
  (smi-comp-mode)
  )

;; -- Entry points --

(defun smi ()
  (interactive)
  (smi-define-folder-set "all" ".*")
  (setq smi-current-folder-set smi-default-folder-set)
  (smi-browse-folders))

(defun smi-comp (&optional use-current)
  "Open a new buffer for editing a draft, edit it and send it.
With prefix arg use current message as form."
  (interactive "P")
  (if use-current
      (smi-exec smi-comp-program
                smi-comp-noedit-switch
                smi-current-folder
                smi-current-msg)
    (smi-exec smi-comp-program smi-comp-noedit-switch))
  (let ((filename (progn (set-buffer smi-buffer)
                         (smi-shell-exec smi-show-draft-filename-command)
                         (substring (buffer-string) 0 -1))))
    (find-file filename))
  (smi-comp-mode))

(defun smi-scan (folder)
  "Scan the folder and read it.  The folder becomes current folder."
  (interactive (list (smi-read-folder-name)))
  (setq smi-current-folder folder)
  (smi-exec smi-scan-program folder)
  (switch-to-buffer (concat smi-buffer-name-prefix folder))
  (erase-buffer)
  (insert-buffer smi-buffer)
  (beginning-of-buffer)
  (flush-lines "^scan: ")
  (if (not (re-search-forward "^[ \t]*[0-9]+\\+" nil t))
      (message "No current message.")
    (beginning-of-line))
  (smi-scan-mode))

(defun smi-browse-folders ()
  "List all folders, move among them and choose."
  (interactive)
  (smi-shell-exec smi-folder-list-command)
  (switch-to-buffer (concat smi-buffer-name-prefix "Folder list"))
  (erase-buffer)
  (insert-buffer smi-buffer)
  (beginning-of-buffer)
  (flush-lines "^[ \t]*Folder[ \t]*# of messages")
  (beginning-of-buffer)
  (keep-lines (cdr (assoc smi-current-folder-set smi-folder-sets)))
  (beginning-of-buffer)
  (if (not smi-folder-list-show-empty)
      (flush-lines "no messages"))
  (smi-get-foldernames)
  (smi-browse-folders-mode))

(defun smi-scan-this-folder ()
  "Scan the folder point is on."
  (interactive)
  (beginning-of-line)
  (looking-at "^[ \t]*\\([^ \t+]+\\)")
  (smi-scan (concat "+"
                    (buffer-substring (match-beginning 1) (match-end 1)))))

;; -- Do this now --

(if smi-use-bbdb (require 'bbdb))
(require 'supercite)
(provide 'smi)

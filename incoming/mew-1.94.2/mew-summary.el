;;; mew-summary.el --- Summary mode for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996
;; Revised: Sep  3, 1999

;;; Code:

(defconst mew-summary-version "mew-summary.el version 0.40")

(require 'mew)
(if mew-xemacs-p (require 'easymenu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; User customize variables
;;;

(defvar mew-summary-mode-map nil)

(defvar mew-summary-mode-menu-spec
  '("Mew"
    ["Show" mew-summary-show t]
    ["Next part" mew-summary-display-down t]
    ["Previous part" mew-summary-display-up t]
    ["Top" mew-summary-jump-top t]
    ["Bottom" mew-summary-jump-bottom t]
    ["Jump" mew-summary-jump-message t]
    "----"
    ["Delete" mew-summary-delete (equal major-mode 'mew-summary-mode)]
    ["Refile" mew-summary-refile (equal major-mode 'mew-summary-mode)]
    ["Mark multi" mew-summary-mark-multi t]
    ["Mark review" mew-summary-mark-review t]
    ["Sort marked msgs" mew-summary-mark-sort t]
    ["Undo" mew-summary-undo t]
    ["Undo all" mew-summary-undo-all t]
    ["Execute" mew-summary-exec (equal major-mode 'mew-summary-mode)]
    ["Suspend" mew-summary-suspend t]
    ["Quit" mew-summary-quit t]
    "----"
    ("Manipulate folder"
     ["Get" mew-summary-get t]
     ["List" mew-summary-ls (equal major-mode 'mew-summary-mode)]
     ["Pack" mew-summary-pack (equal major-mode 'mew-summary-mode)]
     ["Sort" mew-summary-sort (equal major-mode 'mew-summary-mode)]
     ["Burst" mew-summary-burst t]
     ["Go to folder" mew-summary-goto-folder t]
     )
    ("Manipulate file"
     ["Save" mew-summary-save t]
     ["Convert to local character set" mew-summary-convert-local-cs t]
     ["Display X-Face" mew-summary-x-face t]
     )
    ("Write/Reply/Forward"
     ["Write a message" mew-summary-send t]
     ["Reedit" mew-summary-reedit t]
     ["Reply" mew-summary-reply t]
     ["Reply with citation" mew-summary-reply-with-citation t]
     ["Forward" mew-summary-forward t]
     ["Multi forward" mew-summary-multi-forward t]
     )
    ("Select"
     ["Search then Mark" mew-summary-search-mark
      (equal major-mode 'mew-summary-mode)]
     ["Search" mew-summary-search (equal major-mode 'mew-summary-mode)]
     ["Virtual mode" mew-summary-virtual (equal major-mode 'mew-summary-mode)]
     )
    ("Misc"
     ["Clean +trash" mew-summary-clean-trash t]
     ["Recenter" mew-summary-recenter t]
     ["Uudecode" mew-summary-uudecode t]
     ["Unshar" mew-summary-unshar t]
     ["Multi burst" mew-summary-burst-multi t]
     ["Print" mew-summary-print t]
     ["Pipe message" mew-summary-pipe-message t]
     ["Isearch forward" mew-summary-isearch-forward t]
     ["Isearch backward" mew-summary-isearch-backward t]
     ["Toggle disp msg" mew-summary-toggle-disp-msg t]
     ["Config for imget" mew-summary-config-imget t]
     ["PGP public key fetch" mew-pgp-fetch-key t]
     ["Kill Sub-Process"
      mew-summary-kill-subprocess 
      (and (processp mew-summary-buffer-process)
	   (equal major-mode 'mew-summary-mode))]
     )))

(if mew-summary-mode-map
    ()
  (setq mew-summary-mode-map (make-sparse-keymap))
  (define-key mew-summary-mode-map " "    'mew-summary-show)
  (define-key mew-summary-mode-map "."    'mew-summary-display-command)
  (define-key mew-summary-mode-map ","    'mew-summary-display-asis)
  (define-key mew-summary-mode-map "<"    'mew-summary-jump-top)
  (define-key mew-summary-mode-map ">"    'mew-summary-jump-bottom)
  (define-key mew-summary-mode-map "\177" 'mew-summary-prev-page)
  (define-key mew-summary-mode-map "\r"   'mew-summary-scroll-up)
  (define-key mew-summary-mode-map "-"    'mew-summary-scroll-down)
  (define-key mew-summary-mode-map "\e\r" 'mew-summary-scroll-down)
  (define-key mew-summary-mode-map "g"    'mew-summary-goto-folder)
  (define-key mew-summary-mode-map "j"    'mew-summary-jump-message)
  (define-key mew-summary-mode-map "i"    'mew-summary-get)
  (define-key mew-summary-mode-map "a"    'mew-summary-reply)
  (define-key mew-summary-mode-map "A"    'mew-summary-reply-with-citation)
  (define-key mew-summary-mode-map "D"    'mew-summary-clean-trash)
  (define-key mew-summary-mode-map "E"    'mew-summary-reedit)
  (define-key mew-summary-mode-map "\ee"  'mew-summary-edit-again)
  (define-key mew-summary-mode-map "f"    'mew-summary-forward)
  (define-key mew-summary-mode-map "F"    'mew-summary-multi-forward)
  (define-key mew-summary-mode-map "r"    'mew-summary-resend)
  (define-key mew-summary-mode-map "@"    'mew-summary-multi)
  (define-key mew-summary-mode-map "*"    'mew-summary-review)
  (define-key mew-summary-mode-map "y"    'mew-summary-save)
  (define-key mew-summary-mode-map "u"    'mew-summary-undo)
  (define-key mew-summary-mode-map "U"    'mew-summary-undo-all)
  (define-key mew-summary-mode-map "n"    'mew-summary-display-down)
  (define-key mew-summary-mode-map "p"    'mew-summary-display-up)
  (define-key mew-summary-mode-map "N"    'mew-summary-display-review-down)
  (define-key mew-summary-mode-map "P"    'mew-summary-display-review-up)
  (define-key mew-summary-mode-map "w"    'mew-summary-send)
  (define-key mew-summary-mode-map "B"    'mew-summary-burst)
  (define-key mew-summary-mode-map "J"    'mew-summary-join)
  (define-key mew-summary-mode-map "Z"    'mew-status-update)
  (define-key mew-summary-mode-map "#"    'mew-summary-print)
  (define-key mew-summary-mode-map "|"    'mew-summary-pipe-message)
  (define-key mew-summary-mode-map "q"    'mew-summary-suspend)
  (define-key mew-summary-mode-map "Q"    'mew-summary-quit)
  (define-key mew-summary-mode-map "C"    'mew-summary-config-imget)
  (define-key mew-summary-mode-map "\C-c\C-a" 'mew-summary-addrbook-add)
  (define-key mew-summary-mode-map "\C-c\C-c" 'mew-summary-flush-queue)
  (define-key mew-summary-mode-map "\C-c\C-e" 'mew-summary-execute-external)
  (define-key mew-summary-mode-map "\C-c\C-f" 'mew-pgp-fetch-key)
  (define-key mew-summary-mode-map "\C-c\C-v" 'mew-pgp-select)
  (define-key mew-summary-mode-map "\C-c\C-i" 'mew-summary-insert)
  (define-key mew-summary-mode-map "\C-c\C-s" 'mew-summary-isearch-forward)
  (define-key mew-summary-mode-map "\C-c\C-r" 'mew-summary-isearch-backward)
  (define-key mew-summary-mode-map "\C-c\C-o" 
    'mew-summary-jump-to-draft-buffer)
  (define-key mew-summary-mode-map "\el"  'mew-summary-recenter)
  (define-key mew-summary-mode-map "\et"  'mew-summary-uudecode)
  (define-key mew-summary-mode-map "\es"  'mew-summary-unshar)
  (define-key mew-summary-mode-map "\eb"  'mew-summary-burst-multi)
  (define-key mew-summary-mode-map "v"    'mew-summary-toggle-disp-msg)
  (define-key mew-summary-mode-map "\C-c\C-l" 'mew-summary-convert-local-cs)
  (define-key mew-summary-mode-map "\C-c\C-p" 'mew-summary-decode-pgp)
  (define-key mew-summary-mode-map "\C-c\C-x" 'mew-summary-x-face)
  (define-key mew-summary-mode-map "\C-c\C-q" 'mew-kill-buffer)
  (define-key mew-summary-mode-map "\C-c\C-k" 'mew-summary-kill-subprocess)
  (define-key mew-summary-mode-map "m"	  (make-sparse-keymap))
  (define-key mew-summary-mode-map "m@"   'mew-summary-mark-multi)
  (define-key mew-summary-mode-map "m*"   'mew-summary-mark-review)
  (define-key mew-summary-mode-map "ms"   'mew-summary-mark-swap)
  (define-key mew-summary-mode-map "mr"   'mew-summary-mark-regexp)
  (define-key mew-summary-mode-map "ma"   'mew-summary-mark-all)
  (define-key mew-summary-mode-map "mu"   'mew-summary-mark-undo-all)
  ;;
  ;; not provided in Virtual mode
  ;;
  (define-key mew-summary-mode-map "!"    'mew-summary-refile-again)
  (define-key mew-summary-mode-map "o"    'mew-summary-refile)
  (define-key mew-summary-mode-map "O"    'mew-summary-pack)
  (define-key mew-summary-mode-map "s"    'mew-summary-ls)
  (define-key mew-summary-mode-map "S"    'mew-summary-sort)
  (define-key mew-summary-mode-map "d"    'mew-summary-delete)
  (define-key mew-summary-mode-map "x"    'mew-summary-exec)
  (define-key mew-summary-mode-map "X"    'mew-summary-exec-current)
  (define-key mew-summary-mode-map "V"    'mew-summary-virtual)
  (define-key mew-summary-mode-map "/"    'mew-summary-search)
  (define-key mew-summary-mode-map "?"    'mew-summary-search-mark)
  (define-key mew-summary-mode-map "mo"   'mew-summary-mark-refile)
  (define-key mew-summary-mode-map "md"   'mew-summary-mark-delete)
  (define-key mew-summary-mode-map "mS"   'mew-summary-mark-sort)
  (define-key mew-summary-mode-map "\C-c\C-b" 'mew-summary-exchange-point)
  (define-key mew-summary-mode-map "\eo"  'mew-summary-auto-refile)
  ;;
  (if mew-xemacs-p
      (define-key mew-summary-mode-map 'button2 'mew-summary-mouse-show)
    (define-key mew-summary-mode-map [mouse-2] 'mew-summary-mouse-show)
    (easy-menu-define
     mew-summary-mode-menu
     mew-summary-mode-map
     "Menu used in Summary mode."
     mew-summary-mode-menu-spec))
  )

(defvar mew-summary-mode-toolbar-menu
  '("Mew Part Commands"
    ["Save" mew-summary-save t]
    ["Reply" mew-summary-reply t]
    ))

(defvar mew-summary-mode-popup-menu nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables
;;;

(defvar mew-last-shell-command "")

(defvar mew-summary-message-regex "^ *\\([0-9]+\\)")

(defvar mew-summary-edit-again-regex
  "----- Original message follows -----\\|----- Unsent message follows -----")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macros
;;;

(defmacro mew-summary-msg-or-part (&rest body)
  (` (cond
      ((eobp) (message "No message"))
      ((not (or (mew-summary-message-number) (mew-syntax-number)))
       (message "No message"))
      (t
       (,@ body)))))

(defmacro mew-summary-msg (&rest body)
  (` (cond
      ((eobp) (message "No message"))
      ((mew-syntax-number)
       (message "Please use this command on a message, not a part"))
      (t
       (,@ body)))))

(defmacro mew-summary-part (&rest body)
  (` (cond
      ((eobp) (message "No part"))
      ((mew-summary-message-number)
       (message "Please use this command on a part, not a message"))
      (t
       (,@ body)))))

(defmacro mew-summary-multi-msgs (&rest body)
  (` (let* ((FLD-MSGS (mew-summary-mark-collect2 mew-mark-multi))
	    (FLD-MSG-LIST FLD-MSGS) ;; may be used in body
	    FILES ;; may be used in body
	    fld-msg)
       (cond
	((null FLD-MSGS)
	 (message "No %s marks" (char-to-string mew-mark-multi)))
	(t
	 ;; a little bit complicated because of Virtual mode
	 (while FLD-MSGS
	   (setq fld-msg (car FLD-MSGS))
	   (setq FLD-MSGS (cdr FLD-MSGS))
	   (setq FILES (cons (mew-expand-folder-get-msg (car fld-msg) (cdr fld-msg))
			     FILES)))
	 (setq FILES (nreverse FILES))
	 (,@ body))))))

(defmacro mew-summary-prepare-draft (&rest body)
  (` (progn
       (unwind-protect
	   (let ((find-file-hooks nil)
		 (inhibit-quit t))
	     (,@ body)
	     ;; XEmacs doesn't draw attachments unless sit for 0...
	     (sit-for 0) 
	     ;; XEmacs doesn't draw toolbar, so...
	     (if (and mew-icon-p
		      (specifier-instance default-toolbar-visible-p))
		 (progn
		   (set-specifier default-toolbar-visible-p nil)
		   (set-specifier default-toolbar-visible-p t))))
	 (save-buffer)) ;; to make sure not to use this draft again
       (mew-touch-folder mew-draft-folder)
       (message "Draft is prepared"))))

(defmacro mew-summary-only (&rest body)
  (` (if (not (equal major-mode 'mew-summary-mode))
	 (message "This command can be used in Summary mode only")
       (,@ body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Summary mode
;;;

(defun mew-summary-mode ()
  "\\<mew-summary-mode-map>
Mew Summary mode:: major mode to visualize messages in a folder.

The keys that are defined for both Summary mode and Virtual mode are:

\\[mew-summary-show]	Read through messages. That is, display a message, scroll it, 
	and move-then-display another message. 
	See 'mew-summary-show-direction' to set 'up, 'down, 
	'next(current direction) or 'stop. Default is 'down.
\\[mew-summary-prev-page]	Back-scroll this message. Unnecessary header fields are hidden
	over the window. Type '\\[mew-summary-prev-page]' to see them when a message is displayed.
\\[mew-summary-display-command]	If the size of a message exceeds 'mew-file-max-size', MIME
	analysis is skipped then the beginning of the raw message is
	displayed. When you failed to decrypted a cipher message, it
	is cached and is displayed as a broken message.
	In such cases, type '\\[mew-summary-display-command]' to force MIME analysis.
\\[mew-summary-display-asis]	Display this message in the raw format(i.e. without MIME analysis). 

\\[mew-summary-scroll-up]	Make this message scroll up with one line.
\\[mew-summary-scroll-down]	Make this message scroll down with one line.

\\[mew-summary-display-down]	Move to below then display. Targets includes parts, messages 
	marked with '*', and non-marked messages. When called with '\\[universal-argument]',
	parts are skipped.
\\[mew-summary-display-up]	Move to above then display. Targets includes parts, messages 
	marked with '*', and non-marked messages. When called with '\\[universal-argument]',
	parts are skipped.
\\[mew-summary-jump-message]	Jump to a message according to the number which you input.
\\[mew-summary-jump-top]	Go to the beginning of this Summary mode.
\\[mew-summary-jump-bottom]	Go to the end of this Summary mode.

\\[mew-summary-get]	Get +inbox asynchronously.
\\[mew-summary-ls]	List this folder asynchronously. 
\\[mew-summary-goto-folder]	Go to the folder which you input.

\\[mew-summary-send]	Write a message. A new draft is prepared in Draft mode.
\\[mew-summary-reply]	Answer to this message. A new draft is prepared in Draft mode. 
	Mew automatically decides To: and Cc:.
\\[mew-summary-reply-with-citation]	Answer to this message. A new draft is prepared in Draft mode. 
	Mew automatically decides To: and Cc: and cites the body.
\\[mew-summary-forward]	Forward this message to a third person. A new draft is prepared in 
	Draft mode and this message is automatically attached.
\\[mew-summary-multi-forward]	Forward messages marked with '@' to a third person. A new draft 
	is prepared in Draft mode and this message is automatically 
	attached.

\\[mew-summary-reedit]	Edit this message again to retry sending. Or edit this
	rfc822 part typically included MIME-encapsulated error message.
	In the +draft folder, it just edits the message. Otherwise, 
	copy the message to the +draft folder, then edit.
\\[mew-summary-edit-again]	Edit an old fashioned error message in which the original message 
	is encapsulated after after strings defined in 
	'mew-summary-edit-again-regex'
	(e.g. \"----- Original message follows -----\").

\\[mew-summary-review]	Put the review the '*' mark on this message. 
	Use '\\[mew-summary-display-review-down]' or '\\[mew-summary-display-review-up]' to jump to a message marked with '*'.
	It can overlay '@'. The cursor stays always.
	See also '\\[mew-summary-mark-refile]', '\\[mew-summary-mark-delete]', '\\[mew-summary-mark-regexp]', and '\\[mew-summary-mark-all]'.
\\[mew-summary-display-review-down]	Jump to the message marked with '*' below.
\\[mew-summary-display-review-up]	Jump to the message marked with '*' above.

\\[mew-summary-multi]	Put the multi the '@' mark on this message for '\\[mew-summary-multi-forward]', '\\[mew-summary-unshar]', 
	'\\[mew-summary-uudecode]', '\\[mew-summary-burst-multi]'. It can overlay the '*' mark. 
        The cursor stays always.
\\[mew-summary-unshar]	Apply 'unshar' on messages marked with '@'.
\\[mew-summary-uudecode]	Apply 'uudecode' on messages marked with '@'.
\\[mew-summary-burst-multi]	De-capsulate messages embedded in the messages marked with '@'.
\\[mew-summary-join]	Concat Message/Partial fragments marked with '@' to an original
	message.

\\[mew-summary-undo]	Cancel the mark on this message.
\\[mew-summary-undo-all]	Cancel all marks according to what you input.

\\[mew-summary-mark-regexp]	Put the '*' mark onto Mall messages matched to a regular expression.
\\[mew-summary-mark-all]	Put the '*' mark onto all messages which are not marked.
\\[mew-summary-mark-review]	Change the '@' mark into the '*' mark.
\\[mew-summary-mark-multi]	Change the '*' mark into the '@' mark.
\\[mew-summary-mark-undo-all]	Unmark all message marked with 'o' or 'D'.
\\[mew-summary-mark-swap]	Swap the '@' mark and the '*' mark.

\\[mew-summary-delete]	Put the delete mark(default is 'D') on this message.
	This can overlay other marks. When it overlays, the cursor stays
	on the message. If it marks newly, displays the next message.
	To know what kind of action will be taken, see 'mew-msg-rm-policy'.
\\[mew-summary-clean-trash]	Really remove all messages in the +trash folder.

\\[mew-summary-save]	Save any parts. If the target is a message, you are asked which 
	you want to save, the entire message or its body. If the target is
	a non-message part, the part is saved (with line delimiter conversion
	if it is a text object).
\\[mew-summary-toggle-disp-msg]	Toggle 'Summary mode only' and 'Summary & Message mode'. If 
	you choose 'Summary mode only', you can quickly put the delete 
	marks since the next message is not displayed.
\\[mew-summary-recenter]	Make the current line to the center of Summary mode.

\\[mew-summary-burst]	De-capsulate embedded messages in MIME format.
\\[mew-status-update]	Read Addrbook and update its information. If executed with '\\[universal-argument]',
	information of folders is also updated in addition to that of
	Addrbook. If 'mew-use-folders-file-p' is 't', the list of
	folders is stored in '~/Mail/.folders'. The default value is 't'.
\\[mew-summary-config-imget]	Set the config value for imget.

\\[mew-summary-suspend]	Suspend Mew then switch to another buffer. All buffers of 
	Mew retain, so you can resume with buffer operations.
\\[mew-summary-quit]	Quit Mew. All buffers of Mew are erased.
\\[mew-kill-buffer]	Kill this Summary mode.

\\[mew-summary-convert-local-cs]	Convert to character sets used locally.
\\[mew-summary-decode-pgp]  Decrypting/verifying old-fashioned PGP messages.
\\[mew-summary-x-face]	Display xface.
\\[mew-pgp-fetch-key]	Fetch the PGP public key whose key ID appears in the X-Mew: field.
\\[mew-pgp-select]	Select PGP version.

\\[mew-summary-addrbook-add]	Adding the value of From: in Message mode to Addrbook.
	When executed with '\\[universal-argument], it will add personal information.
	Otherwise, it will add an alias.

\\[mew-summary-kill-subprocess]	Kill a process in Summary mode such as 'imget' and 'imls'.
	Sometime a process accidentally remains in Summary mode. 
	In this situation, you cannot execute '\\[mew-summary-get]', '\\[mew-summary-ls]', nor '\\[mew-summary-exec]'.
	Use this command to solve this problem.

\\[mew-summary-isearch-forward]	Incremental search forward in Message mode.
\\[mew-summary-isearch-backward]	Incremental search backward in Message mode.

\\[mew-summary-print]	Print this message or this part.
\\[mew-summary-pipe-message]	Send this message via pipe.

The following commands are provided for Summary mode only, not for
Virtual mode.

\\[mew-summary-refile]	Put the refile mark(default is 'o') on this message. 
	If already marked with 'o', it prints where this message 
	will be refiled. This can overlay other marks. When it overlays, 
	the cursor stays on the message. If it marks newly, displays 
	the next message. If executed with '\\[universal-argument]', it displays how
	the refile rules work in Message mode.
\\[mew-summary-refile-again]	Put a refile mark on this message according to the previous 
	refile folder.

\\[mew-summary-exec]	Process marked messages. To cancel the '*' mark, use '\\[mew-summary-undo]' or '\\[mew-summary-undo-all]'.
\\[mew-summary-exec-current]	Process the current marked messages.

\\[mew-summary-mark-refile]	Put the refile mark onto all messages marked with '*'.
	This is very convenient to refile all messages picked by '\\[mew-summary-search-mark]'.
\\[mew-summary-mark-delete]	Put the delete mark onto all messages marked with '*'.
\\[mew-summary-mark-sort]	Sort messages marked with '*'.

\\[mew-summary-search]	Pick messages according to a pick pattern which you input, 
	then list them up.
\\[mew-summary-search-mark]	Pick messages according to a pick pattern which you input, 
	then put the '*' mark onto them.
\\[mew-summary-virtual]	Go to Virtual mode which gives a single view to picked messages
	from multiple folders. Enter a virtual folder name, 
	comma-separated	folders, and pick pattern.

\\[mew-summary-sort]	Sort messages and list them up again.
\\[mew-summary-pack]	Pack messages and list them up again.

RANGE means as follows:
	all, update, 
	<num1>-<num2>, <num>:+N, <num>:-N, 
	first:N, prev:N, next:N, last:N

Use 'all' to flush the summary buffer. 'update' means the range
between the last message included in Summary mode + 1 and the real last
message on the folder.

PICK PATTERN is as follows:
- field=string
	Match if the 'field' field contains the 'string' string.
	If you specify 'head', 'body' or 'all' as 'field', it means
	the entire header, the body, and the entire message, respectively.
- <pattern1> & <pattern2>
	Match if <pattern1> AND <pattern2>.
- <pattern1> | <pattern2>
	Match if <pattern1> OR <pattern2>.
- ! <pattern>
	Match if not <pattern>.
- ( <pattern> )
	Evaluate <pattern> first.
"
  (interactive)
  (setq major-mode 'mew-summary-mode)
  (setq mode-name "Summary")
  (setq mode-line-buffer-identification mew-mode-line-id)
  (use-local-map mew-summary-mode-map)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (make-local-variable 'tab-width)
  (make-local-variable 'zmacs-regions)
  (setq zmacs-regions nil)
  (mew-summary-setup-mode-line)
  (mew-summary-setup-menu)
  (mew-summary-highlight-setup)
  (mew-highlight-cursor-line)
  (run-hooks 'mew-summary-mode-hook))

(defun mew-summary-setup-mode-line ()
  (let ((pos 0)
	(mlf mode-line-format))
    (if (member '(-3 . "%p") mlf)
	(progn
	  (while (not (equal '(-3 . "%p") (car mlf)))
	    (setq pos (1+ pos))
	    (setq mlf (cdr mlf)))
	  (setq mode-line-format
		(let ((mlf (copy-sequence mode-line-format)))
		  (setcdr (nthcdr (1- pos) mlf)
			  '("[" mew-summary-buffer-left-msgs " more]" "-%-"))
		  mlf))))))

(defun mew-summary-reset-mode-line (buf)
  (save-excursion
    (set-buffer buf)
    (setq mew-summary-buffer-left-msgs  "-"))) ;; local variable

(defun mew-summary-mode-line (buf)
  (save-excursion
    (set-buffer buf)
    ;; if not running process in this buffer
    ;; display how many messages are unread
    (if (null mew-summary-buffer-process)
	(let ((left (count-lines (point) (point-max))))
	  (if (eq major-mode 'mew-virtual-mode)
	       (setq left (/ (1- left) 2))
	    (setq left (1- left)))
	  (if (equal left 0)
	      (setq mew-summary-buffer-left-msgs  "-") ;; local variable
	    (setq mew-summary-buffer-left-msgs (int-to-string left)))))))

(defun mew-summary-setup-menu ()
  (if mew-xemacs-p
      (progn
	(mew-summary-toolbar-update)
	(if (featurep 'scrollbar)
	    (set-specifier scrollbar-height (cons (current-buffer) 0)))
        (set-buffer-menubar current-menubar)
	(if mew-summary-mode-popup-menu
	    ()
	  (easy-menu-define
	   mew-summary-mode-popup-menu
	   mew-summary-mode-map
	   "Popup Menu used in Summary and Virtual mode."
	   mew-summary-mode-menu-spec))
	(easy-menu-add mew-summary-mode-popup-menu)
        (add-submenu nil mew-summary-mode-menu-spec))))

(defun mew-summary-folder-name ()
  (cond 
   ((equal major-mode 'mew-summary-mode)
    (buffer-name))
   ((equal major-mode 'mew-virtual-mode)
    (save-excursion
      (beginning-of-line)
      (if (not (mew-summary-message-number))
	  (mew-summary-goto-message))
      (if (looking-at ".*\r \\([-+%=].*\\) \\(.*\\)$")
	  (mew-match 1)
	nil)))
   (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder
;;;

(defun mew-summary-goto-folder (&optional arg fld)
  "Go to the folder which you input.
If executed with '\\[universal-argument]', the cursor always goes to the bottom of 
Summary mode."
  (interactive "P")
  (let* ((folder (or fld (mew-input-folder (mew-inbox-folder))))
	 (dir (mew-expand-folder folder)))
    (cond
     ((mew-folder-newsp folder)
      (mew-summary-goto-folder-subr folder arg))
     ((mew-folder-imapp folder)
      (if (and (not (file-directory-p dir))
	       (y-or-n-p
		(format
		 "Cache directory for %s does not exist. Create it? "
		 folder)))
	  (let ((folders-file (expand-file-name mew-folders-file mew-mail-path)))
	    (mew-make-directory dir)
	    (if (and (mew-folder-setup folder) (file-writable-p folders-file))
		(save-excursion
		  (mew-set-buffer-tmp)
		  (insert folder)
		  (insert "\n")
		  (write-region (point-min) (point-max)
				folders-file 'append 'no-msg))))
	(mew-folder-setup folder))
      (mew-summary-goto-folder-subr folder arg))
     ((mew-folder-virtualp folder)
      (if (get-buffer folder)
	  (progn
	    (mew-summary-goto-folder-subr folder arg)
	    (mew-virtual-mode))
        (message "No such virtual folder: %s" folder)
	(mew-folder-delete folder)))
     (t ;; mail or local news
      (if (null dir)
	  (message "Folder is wrong")
	(if (not (file-directory-p dir))
	    (message "No such folder %s" folder)
	  (mew-summary-goto-folder-subr folder arg)
	  (if mew-summary-trace-directory (cd dir))))))))

(defun mew-summary-goto-folder-subr (folder arg)
  (let (new-folder)
    (if (get-buffer folder)
	(switch-to-buffer folder)
      (mew-summary-folder-create folder)
      (setq new-folder t))
    (if (eq major-mode 'mew-summary-mode)
	(mew-summary-ls t (or arg new-folder)))))

(defun mew-summary-folder-create (folder)
  (switch-to-buffer (get-buffer-create folder))
  (mew-summary-mode)
  (if (and mew-summary-cache-use (mew-folder-localp folder))
      (mew-elet
       (let ((cache (mew-expand-folder folder mew-summary-cache-file)))
	 (if (file-exists-p cache)
	     (mew-frwlet
	      mew-cs-scan mew-cs-dummy
	      (insert-file-contents cache)
	      (setq mew-summary-buffer-folder-cache-time 
		    (mew-file-get-time cache))
	      (mew-highlight-mark-region (point-min) (point-max))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; show
;;;

(defun mew-summary-toggle-decode-quoted ()
  "Toggle whether or not decode quoted MIME encoded-word.
(e.g \"=?iso-2022-jp?B?GyRCOzNLXE9CSScbKEI=?=\")."
  (interactive)
  (cond
   (mew-decode-quoted
    (message "Don't decode quoted MIME encoded-word")
    (mew-summary-goto-message)
    (setq mew-decode-quoted nil))
   (t
    (message "Decode quoted MIME encoded-word")
    (setq mew-decode-quoted t)))
  (mew-cache-flush))

(defun mew-summary-cache-prefetch ()
  (if mew-cache-prefetch
      (let ((mew-inherit-prefetching t)
	    folder next file)
	(save-excursion
	  (sit-for 0) ;; need to display
	  (mew-summary-goto-message)
	  (cond
	   ((eq mew-summary-buffer-direction 'up)
	    (if (re-search-backward (mew-summary-regex) nil t)
		(progn
		  (setq folder (mew-summary-folder-name))
		  (setq next (mew-summary-message-number)))))
	   ((eq mew-summary-buffer-direction 'down)
	    (if (mew-decode-syntax-end)
		(goto-char (mew-decode-syntax-end))
	      (forward-line))
	    (if (re-search-forward (mew-summary-regex) nil t)
		(progn
		  (setq folder (mew-summary-folder-name))
		  (setq next (mew-summary-message-number)))))))
	;; should get the cursor back for display
	(save-excursion
	  (if (not (and folder next))
	      ()
	    (cond
	     ((mew-folder-newsp folder)
	      (setq file (expand-file-name
			  next (expand-file-name (substring folder 1) mew-temp-dir))))
	     ((mew-folder-imapp folder)
	      (setq file (expand-file-name next (mew-imap-folder-dir folder mew-temp-dir))))
	     (t
	      (setq file (mew-expand-folder folder next))))
	    (if (and (not (mew-cache-hit (cons folder next)))
		     (or (and mew-cache-prefetch-remote (mew-folder-remotep folder))
			 (and file (file-exists-p file)
			      (<= (mew-file-get-size file) mew-file-max-size))))
		(mew-cache-message folder next)))))))

(defmacro mew-summary-show-postscript (prefetch)
  (` (progn
       (mew-summary-recenter)
       (mew-highlight-cursor-line)
       (if (, prefetch) (mew-summary-cache-prefetch))
       (set-buffer-modified-p nil))))

(defmacro mew-summary-display-after (direction)
  (` (cond 
      ((eq (, direction) 'down)
       (mew-summary-display-down))
      ((eq (, direction) 'up)
       (mew-summary-display-up))
      ((eq (, direction) 'next)
       (mew-summary-display-next))
      (t ())))) ;; 'stop

(defun mew-summary-show ()
  "Read through messages. That is, display a message, scroll it, 
and move-then-display another message. 
See 'mew-summary-show-direction' to set 'up, 'down, 
'next(current direction) or 'stop. Default is 'down."
  (interactive)
  (mew-summary-msg-or-part
   (let* ((fld (mew-summary-folder-name))
	  (msg (mew-summary-message-number))
	  (fld-msg (cons fld msg))
	  (ofld-msg (mew-current-get 'message))
	  (part (mew-syntax-nums))
	  (opart (mew-current-get 'part))
	  (buf (buffer-name))
	  (win (selected-window))
	  (displayed (and (get-buffer (mew-buffer-message))
			  (get-buffer-window (mew-buffer-message))))
	  next prefetch)
     (mew-summary-toggle-disp-msg 'on)
     (unwind-protect
	 (progn
	   (mew-window-configure buf 'message)
	   ;; message buffer
	   (cond
	    (msg 
	     (if (or (null ofld-msg)
		     (not (equal fld-msg ofld-msg))
		     opart)
		 (progn
		   (mew-summary-display-message fld msg buf nil)
		   (setq prefetch t))
	       (if (not displayed)
		   () ;; keep the page
		 (if (mew-message-next-page)
		     (setq next t)))))
	    (part
	     (if (or (null opart) (not (equal opart part)))
		 (mew-summary-display-part 
		  (mew-cache-decode-syntax (mew-cache-hit ofld-msg)) part)
	       (if (mew-message-next-page)
		   (setq next t))))))
       (select-window win)
       ;; summary buffer
       (mew-summary-show-postscript prefetch)
       (if next (mew-summary-display-after mew-summary-show-direction))))))

(defun mew-summary-toggle-disp-msg (&optional arg)
  "Toggle 'Summary mode only' and 'Summary & Message mode'. If 
you choose 'Summary mode only', you can quickly put the delete
	marks since the next message is not displayed."
  (interactive)
  (cond 
   ((equal arg 'on)
    (setq mew-summary-buffer-disp-msg t))
   ((equal arg 'off)
    (setq mew-summary-buffer-disp-msg nil)
    (mew-summary-reset-mode-line (current-buffer)))
   (t
    (setq mew-summary-buffer-disp-msg (not mew-summary-buffer-disp-msg))
    (if mew-summary-buffer-disp-msg
	(mew-summary-display 'force)
      (mew-summary-goto-message)
      (mew-decode-syntax-delete)
      (mew-window-configure (current-buffer) 'summary)
      (mew-current-set 'message nil)
      (mew-summary-reset-mode-line (current-buffer))))))

(defun mew-summary-display-command (&optional arg)
  "\\<mew-summary-mode-map>
If the size of a message exceeds 'mew-file-max-size', MIME
analysis is skipped then the beginning of the raw message is
displayed. When you failed to decrypted a cipher message, it
is cached and is displayed as a broken message.
In such cases, type '\\[mew-summary-display-command]' to force MIME analysis."
  (interactive "P")
  (if arg
      (mew-summary-display-asis)
    (mew-summary-display 'force)))

(defun mew-summary-display (force)
  "Display this message or this part. If already displayed, nothing
changed. But if FORCE is t, the message or the part is re-displayed."
  (if (not (or force mew-summary-buffer-disp-msg))
      ()
    (mew-summary-msg-or-part
     (let* ((fld (mew-summary-folder-name))
	    (msg (mew-summary-message-number))
	    (fld-msg (cons fld msg))
	    (ofld-msg (mew-current-get 'message))
	    (part (mew-syntax-nums))
	    (opart (mew-current-get 'part))
	    (buf (buffer-name))
	    (win (selected-window))
	    prefetch)
       (unwind-protect
	   (progn
	     (mew-summary-toggle-disp-msg 'on)
	     (mew-window-configure buf 'message)
	     ;; messge buffer
	     (cond
	      (msg
	       (if (or (null ofld-msg)
		       (not (equal fld-msg ofld-msg))
		       opart
		       force)
		   (progn
		     (mew-summary-display-message fld msg buf force)
		     (setq prefetch t))))
	      (part
	       (if (or (null opart) (not (equal opart part)) force)
		   (mew-summary-display-part 
		    (mew-cache-decode-syntax (mew-cache-hit ofld-msg))
		    part)))))
	 (select-window win)
	 ;; summary buffer
	 (mew-summary-show-postscript prefetch))))))

(defvar mew-message-overlay nil)

(defun mew-message-set-end-of ()
  (save-restriction
    (widen)
    (save-excursion
      (mew-elet
       (goto-char (point-max))
       (if (not (bolp)) (insert "\n"))
       (if (and mew-xemacs-p (extent-at (point) nil nil nil 'at))
	   ;; to skip graphics
	   (insert "\n"))
       (mew-message-clear-end-of)
       (if (or mew-end-of-message-string mew-end-of-part-string)
	   (progn
	     (mew-overlay-move mew-message-overlay (point-max) (point-max))
	     (if (mew-decode-syntax-p)
		 (if (mew-summary-end-of-message-p)
		     (mew-message-set-end-of-message)
		   (mew-message-set-end-of-part))
	       (mew-message-set-end-of-message))))))))

(defmacro mew-message-clear-end-of ()
  '(progn
     (if (not (mew-local-variable-p 'mew-message-overlay))
	 (make-local-variable 'mew-message-overlay))
     (if (not (mew-overlay-p mew-message-overlay))
	 (setq mew-message-overlay
	       (mew-overlay-make (point-max) (point-max))))
     (mew-message-set-end-of-nil)))

(defmacro mew-summary-display-preamble ()
  '(progn
     (mew-message-clear-end-of)
     (mew-overlay-delete-buffer) ;; also delete extents
     (set-marker (mark-marker) nil) ;; kill mark for cite
     (mew-erase-buffer)))

(defmacro mew-summary-display-postscript ()
  '(progn
     (run-hooks 'mew-message-hook)
     (mew-message-set-end-of)
     (set-buffer-modified-p nil)))
  
(defun mew-summary-display-message (fld msg buf force)
  ;; message buffer
  (mew-elet
   (let ((zmacs-regions nil)
	 (file (mew-expand-folder fld msg))
	 toolarge hit)
     (mew-summary-display-preamble)
     ;;
     (mew-decode-syntax-delete)
     (mew-summary-mode-line buf) (set-buffer-modified-p nil)
     ;;
     (mew-current-set 'message (cons fld msg))
     (mew-current-set 'part nil)
     (mew-current-set 'cache nil)
     ;;
     (setq mew-decode-syntax nil)
     (if (not (or (mew-folder-remotep fld) (file-exists-p file)))
	 (message "File does not exist.")
       (cond
	((and (not mew-debug) (equal fld mew-draft-folder))
	 (insert-file-contents file))
	((and (not force) ;; not force, so don't cache if too large
	      (not (mew-cache-hit (cons fld msg))) ;; use cache
	      (setq toolarge
		    (and (mew-folder-localp fld)
			 (> (mew-file-get-size file) mew-file-max-size))))
	 (mew-insert-message fld msg mew-cs-autoconv (1- mew-file-max-size))
	 (condition-case nil
	     (progn
	       (mew-decode-rfc822-header)
	       (mew-header-goto-end)
	       (mew-header-arrange (point-min) (point))
	       (setq mew-decode-syntax (mew-decode-syntax-rfc822))
	       (if toolarge
		   (message
		    (substitute-command-keys
		     "Too large, this message was truncated. To see the entire message, type '\\<mew-summary-mode-map>\\[mew-summary-display-command]'"))))
	   (error
	    (if mew-decode-error 
		(message mew-decode-error)))))
	(t
	 (setq hit (mew-cache-message fld msg force))
	 (if (null hit)
	     ()
	   (mew-current-set 'cache hit)
	   (setq mew-decode-syntax (mew-cache-decode-syntax hit))
	   (setq mew-decode-error  (mew-cache-decode-error hit))
	   (setq mew-syntax-multi-form (mew-cache-multi-form hit))
	   (setq mew-syntax-icon-spec (mew-cache-icon-spec hit))
	   (setq mew-syntax-privacy-result (mew-cache-privacy-result hit))
	   (mew-decode-syntax-print buf mew-decode-syntax
				    mew-syntax-multi-form
				    mew-syntax-icon-spec)
	   (mew-mime-message/rfc822 mew-decode-syntax)
	   (if mew-decode-error 
	       (message "MIME decoding error: %s" mew-decode-error))))))
     (mew-summary-display-postscript))))

(defun mew-summary-display-part (fullpart nums &optional execute)
  ;; message buffer
  (mew-elet
   (let ((zmacs-regions nil))
     (mew-summary-display-preamble)
     (mew-mime-part fullpart nums execute)
     (mew-current-set 'part nums) ;; should be after funcall
     (mew-summary-display-postscript))))

(defun mew-summary-display-asis (&optional arg)
  "Display this message in the raw format(i.e. without MIME analysis). 
The beginning part of the message, whose size specified by
'mew-file-max-size', is displayed. If called with '\\[universal-argument]', the
entire message is displayed in the raw format."
  (interactive "P")
  (mew-summary-msg
   (let* ((fld (mew-summary-folder-name))
	  (msg (mew-summary-message-number))
	  (file (mew-expand-folder fld msg))
	  (win (selected-window))
	  size)
     (unwind-protect
	 (progn
	   (mew-summary-toggle-disp-msg 'on)
	   (mew-decode-syntax-delete)
	   (mew-window-configure (current-buffer) 'message)
	   (set-buffer (mew-buffer-message))
	   ;; messge buffer
	   (mew-elet
	    (let ((zmacs-regions nil))
	      (mew-summary-display-preamble)
	      (mew-current-set 'message (cons fld msg))
	      (mew-current-set 'part nil)
	      (mew-current-set 'cache nil)
	      (if (and (mew-folder-localp fld)
		       (null arg)
		       (> (mew-file-get-size file) mew-file-max-size))
		  (setq size (1- mew-file-max-size)))
	      (mew-insert-message fld msg mew-cs-autoconv size)
	      (mew-message-set-end-of)
	      (let ((mew-header-max-length nil)
		    (mew-header-max-depth nil))
		(mew-decode-rfc822-header))
	      (mew-header-goto-end)
	      (mew-header-arrange (point-min) (point))
	      (setq mew-decode-syntax (mew-decode-syntax-rfc822))
	      (set-buffer-modified-p nil))))
       (select-window win)
       ;; summary buffer
       (mew-summary-show-postscript nil)))))

(defun mew-summary-execute-external ()
  "Execute an external command according to 
Content-Type:"
  (interactive)
  (mew-summary-part
   (let* ((ofld-msg (mew-current-get 'message))
	  (nums (mew-syntax-nums))
	  (buf (buffer-name)))
     (unwind-protect
	 (progn
	   (mew-summary-toggle-disp-msg 'on)
	   (mew-window-configure buf 'message)
	   ;; message buffer
	   (mew-summary-display-part
	    (mew-cache-decode-syntax (mew-cache-hit ofld-msg)) nums t))
       (mew-pop-to-buffer buf)))))

(defun mew-summary-recenter ()
  "Make the current line to the center of Summary mode."
  (interactive)
  (if (or mew-summary-recenter-p
	  (interactive-p))
      (recenter (/ (- (window-height) 2) 2))))

(defun mew-summary-mouse-show (e)
  "Mouse version of 'mew-summary-show'."
  (interactive "e")
  (mouse-set-point e)
  (beginning-of-line)
  (mew-summary-show))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; direction
;;;

(defun mew-summary-next ()
  (if (eq mew-summary-buffer-direction 'up)
      (mew-summary-up)
    (mew-summary-down)))

(defmacro mew-summary-regex ()
  '(concat mew-summary-message-regex 
	   "[ " (char-to-string mew-mark-review) "]\\|"
	   mew-syntax-number-text-regex))

(defun mew-summary-down ()
  (forward-line)
  (cond 
   ((re-search-forward (mew-summary-regex) nil t)
    (beginning-of-line)
    (setq mew-summary-buffer-direction 'down)
    t)
   (t 
    (mew-decode-syntax-delete)
    (mew-current-set 'message nil)
    (forward-line -1)
    (mew-window-configure (current-buffer) 'summary)
    (mew-current-set 'message nil)
    (message "No more message")
    nil)))

(defun mew-summary-up ()
  (cond 
   ((re-search-backward (mew-summary-regex) nil t)
    (setq mew-summary-buffer-direction 'up)
    t)
   (t 
    (mew-decode-syntax-delete)
    (mew-window-configure (current-buffer) 'summary)
    (mew-current-set 'message nil)
    (message "No more message")
    nil)))

;;

(defun mew-summary-display-next ()
  (if (mew-summary-next) (mew-summary-display nil)))

(defun mew-summary-display-up (&optional arg)
  "Move to above then display. Targets includes parts, messages 
marked with '*', and non-marked messages. When called with '\\[universal-argument]',
parts are skipped."
  (interactive "P")
  (beginning-of-line)
  (if arg
      (progn
	(mew-summary-goto-message)
	(mew-decode-syntax-delete)))
  (if (mew-summary-up) (mew-summary-display nil)))

(defun mew-summary-display-down (&optional arg)
  "Move to below then display. Targets includes parts, messages 
marked with '*', and non-marked messages. When called with '\\[universal-argument]',
parts are skipped."
  (interactive "P")
  (if arg
      (progn
	(mew-summary-goto-message)
	(mew-decode-syntax-delete)))
  (if (mew-summary-down) (mew-summary-display nil)))

;;

(defun mew-summary-prev-page ()
  "\\<mew-summary-mode-map>
Back-scroll this message. Unnecessary header fields are hidden
over the window. Type '\\[mew-summary-prev-page]' to see them when a message is displayed."
  (interactive)
  (mew-summary-scroll-down 'fullpage))

(defun mew-summary-scroll-up ()
  "Make this message scroll up with one line."
  (interactive)
  (mew-summary-msg-or-part
   (let ((buf (current-buffer))
	 (msg (mew-summary-message-number))
	 (ofld-msg (mew-current-get 'message))
	 (part (mew-syntax-nums))
	 (opart (mew-current-get 'part)))
     (if (or (and msg (string= msg (cdr ofld-msg)) (null part) (null opart))
	     (and part (equal part opart)))
	 (unwind-protect
	     (progn
	       (mew-window-configure buf 'message)
	       (mew-message-next-page 1))
	   (mew-pop-to-buffer buf))
       (mew-summary-show)))))

(defun mew-summary-scroll-down (&optional fullpage)
  "Make this message scroll down with one line."
  (interactive)
  (mew-summary-msg-or-part
   (let ((buf (current-buffer))
	 (msg (mew-summary-message-number))
	 (ofld-msg (mew-current-get 'message))
	 (part (mew-syntax-nums))
	 (opart (mew-current-get 'part)))
     (if (or (and msg (string= msg (cdr ofld-msg)) (null part) (null opart))
	     (and part (equal part opart)))
	 (unwind-protect
	     (progn
	       (mew-window-configure buf 'message)
	       (mew-message-prev-page (if fullpage nil 1)))
	   (mew-pop-to-buffer buf))
       (mew-summary-show)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; to Draft mode
;;;

(defun mew-summary-send (&optional to cc subject)
  "Write a message. A new draft is prepared in Draft mode."
  (interactive)
  (let* ((draft (mew-folder-new-message mew-draft-folder))
	 (attachdir (mew-attachdir draft)))
    (mew-current-set 'window (current-window-configuration))
    (mew-window-configure (current-buffer) 'draft)
    (mew-summary-prepare-draft
     (switch-to-buffer (find-file-noselect draft))
     (mew-draft-rename draft)
     (mew-delete-directory-recursively attachdir)
     (mew-draft-header subject nil to cc)
     (mew-draft-mode)
     (run-hooks 'mew-draft-mode-newdraft-hook))))

(defun mew-summary-reply (&optional onlytofrom)
  "Answer to this message. A new draft is prepared in Draft mode. 
Mew automatically decides To: and Cc:. Addresses on To: and Cc:
are decided as follows:

    If From: of the message to be replied is not from me:
        Reply-To: doesn't exist in the message to be replied
            Copy From: of the message to be replied to To: (1)
            Copy To: and Cc: of the message to be replied to Cc: (2)
        Reply-To: exists in the message to be replied
            Copy From: and Reply-To: of the message to be replied to To: (3)
            Copy To: and Cc: of the message to be replied to Cc: (4)
    If From: of a message to be replied is from me:
        Copy To: of the message to be replied to To: (5)
        Copy Cc: of the message to be replied to Cc: (6)

You can customize which fields are copied in the case (1)-(6) with the
following variables:

    (1) mew-noreplyto-to-list
    (2) mew-noreplyto-cc-list
    (3) mew-replyto-to-list
    (4) mew-replyto-cc-list
    (5) mew-fromme-to-list
    (6) mew-fromme-cc-list

If executed with '\\[universal-argument]', only From: of the message is copied to To:.
"
  (interactive "P")
  (mew-summary-msg-or-part
   (mew-summary-toggle-disp-msg 'on)
   (mew-current-set 'window (current-window-configuration))
   (let* ((buf (buffer-name))
	  (draft (mew-folder-new-message mew-draft-folder))
	  (attachdir (mew-attachdir draft))
	  from reply-to to cc newsgroups subject in-reply-to references
	  cbuf encrypted fromme)
     (mew-summary-prepare-draft
      (if (get-buffer (mew-buffer-message))
	  (delete-windows-on (mew-buffer-message)))
      (if (< (window-height) 25) (delete-other-windows))
      (let ((split-window-keep-point t))
	(split-window-vertically))
      (switch-to-buffer-other-window (find-file-noselect draft))
      (mew-draft-rename draft)
      (mew-delete-directory-recursively attachdir)
      (setq cbuf (current-buffer)) ;; draft
      (mew-pop-to-buffer buf) 
      (mew-summary-display nil)
      ;; see also mew-draft-cite
      (set-buffer (or (save-excursion
			(set-buffer (mew-buffer-message))
			(if (mew-header-p) (current-buffer)))
		      ;; header exists only in cache if multipart
		      (mew-cache-hit (mew-current-get 'message))))
      (setq encrypted (mew-syntax-encrypted-p mew-decode-syntax))
      (save-restriction
	;; if body contains ^L, header is not accessible.
	;; mew-header-* can't widen essentially. So widen here.
	(widen)
	;; now cache buffer
	(setq from (mew-header-parse-address mew-from:))
	(setq reply-to (mew-header-parse-address mew-reply-to:))
	(cond 
	 (onlytofrom (setq to from))
	 ((mew-is-my-address (mew-get-my-address-regex-list) from)
	  ;; This message was sent by me. So, maintain To: and Cc:.
	  (setq fromme t)
	  (setq to (mew-header-parse-address-list2 mew-fromme-to-list))
	  (setq cc (mew-header-parse-address-list2 mew-fromme-cc-list))
	  (if (null to) (setq to (or reply-to from)))) ;; don't use list
	 (t
	  (cond 
	   (reply-to
	    (setq to (mew-header-parse-address-list2 mew-replyto-to-list))
	    (setq cc (mew-header-parse-address-list2 mew-replyto-cc-list)))
	   (t
	    (setq to (mew-header-parse-address-list2 mew-noreplyto-to-list))
	    (setq cc (mew-header-parse-address-list2 mew-noreplyto-cc-list)))))
	 )
	(setq newsgroups (or (mew-header-get-value mew-followup-to:)
			     (mew-header-get-value mew-newsgroups:)))
	(if (and newsgroups (mew-case-equal newsgroups "poster"))
	    (setq newsgroups nil))
	(setq subject (mew-header-get-value mew-subj:))
	(if (and subject (not (string-match mew-reply-regex subject)))
	    (setq subject (concat mew-reply-string subject)))
	;;
	;; DRUMS says:
	;; 
	;; If the original message contains a "Message-ID:" field, the
	;; contents of that field body are copied into the body of an
	;; "In-Reply-To:" field and into the body of a "References:"
	;; field in the new message. If the original message contains a
	;; "References:" field and/or an "In-Reply-To:" field already
	;; (hence a reply to a reply), the contents of the old
	;; "References:" field are copied to the "References:" field in
	;; the new message, appending to it the contents of the old
	;; "In-Reply-To:" field (if its message identifier was not
	;; already in the "References:" field) and the contents of the
	;; "Message-ID:" field of the original message. In this way, a
	;; "thread" of conversation can be established.
	;;
	;;
	(let ((old-message-id  (mew-header-get-value mew-message-id:))
	      (old-in-reply-to (mew-header-get-value mew-in-reply-to:))
	      (old-references  (mew-header-get-value mew-references:))
	      (regex "<[^>]+>")
	      (start 0) tmp-ref skip)
	  (if (and old-message-id (string-match regex old-message-id))
	      (setq old-message-id (mew-match 0 old-message-id))
	    (setq old-message-id nil))
	  (if (and old-in-reply-to (string-match regex old-in-reply-to))
	      (setq old-in-reply-to (mew-match 0 old-in-reply-to))
	    (setq old-in-reply-to nil))
	  (if (null old-message-id)
	      () ;; we don't care even if old-references exist.
	    (setq in-reply-to old-message-id)
	    (if (null old-references)
		(setq tmp-ref (if old-in-reply-to 
				  (list old-in-reply-to old-message-id)
				(list old-message-id)))
	      (while (string-match "<[^>]+>" old-references start)
		(setq start (match-end 0))
		(setq tmp-ref (cons (mew-match 0 old-references) tmp-ref)))
	      (if (and old-in-reply-to (not (member old-in-reply-to tmp-ref)))
		  (setq tmp-ref (cons old-in-reply-to tmp-ref)))
	      (setq tmp-ref (nreverse (cons old-message-id tmp-ref))))
	    (if (integerp mew-references-max-count)
		(setq skip (- (length tmp-ref) mew-references-max-count)))
	    (if (and (numberp skip) (> skip 0))
		(setq tmp-ref (nthcdr skip tmp-ref)))
	    (setq references (mew-join "\n\t" tmp-ref)))))
      ;;
      (mew-pop-to-buffer cbuf) ;; draft
      (mew-draft-header subject nil to cc newsgroups in-reply-to references
			nil fromme)
      (if (eq mew-summary-reply-position 'body)
	  (progn
	    (goto-char (mew-header-end))
	    (forward-line)))
      (mew-draft-mode encrypted)
      (run-hooks 'mew-draft-mode-newdraft-hook)))))

(defun mew-summary-reply-with-citation (&optional onlytofrom)
  "Answer to this message. A new draft is prepared in Draft mode. 
And this message is automatically cited. See also 'mew-summary-reply'."
  (interactive "P")
  (mew-summary-msg-or-part
   (let ((mew-summary-reply-position nil))
     (mew-summary-reply onlytofrom))
   ;; mew-draft-mode-hook may insert text.
   (save-excursion
     (goto-char (point-max))
     (run-hooks 'mew-before-cite-hook)
     (mew-draft-cite))
   ;; the cursor is after To:
   (cond
    ((eq mew-summary-reply-with-citation-position 'body)
     (goto-char (mew-header-end))
     (forward-line))
    ((eq mew-summary-reply-with-citation-position 'end)
     (goto-char (point-max))))))
  
(defun mew-summary-forward ()
  "Forward this message to a third person. A new draft is prepared in 
Draft mode and this message is automatically attached."
  (interactive)
  (mew-summary-msg-or-part
   (mew-current-set 'window (current-window-configuration))
   (let* ((buf (buffer-name))
	  (draft (mew-folder-new-message mew-draft-folder))
	  (draftdir (file-name-nondirectory draft))
	  (attachdir (mew-attachdir draft))
	  file subject fwsubject cbuf)
     (save-excursion
       (mew-summary-goto-message)
       (setq file (mew-expand-folder-get-msg (mew-summary-folder-name)
					     (mew-summary-message-number))))
     (mew-summary-prepare-draft
      (delete-other-windows)
      (let ((split-window-keep-point t))
	(split-window-vertically))
      (switch-to-buffer-other-window (find-file-noselect draft))
      (mew-draft-rename draft)
      (mew-delete-directory-recursively attachdir)
      (setq cbuf (current-buffer)) ;; draft
      (mew-pop-to-buffer buf)
      (mew-summary-display 'force) ;; force to get Subject:
      ;;
      (set-buffer (or (save-excursion
			(set-buffer (mew-buffer-message))
			(if (mew-header-p) (current-buffer)))
		      ;; header exists only in cache if multipart
		      (mew-cache-hit (mew-current-get 'message))))
      (setq subject (mew-header-get-value mew-subj:))
      (if (and subject (not (string-match mew-forward-regex subject)))
	  (setq fwsubject (concat mew-forward-string subject))
	(setq fwsubject subject))
      (mew-pop-to-buffer cbuf) ;;; draft
      ;;
      (mew-draft-header fwsubject 'nl)
      (mew-draft-mode)
      (run-hooks 'mew-draft-mode-newdraft-hook)
      (mew-draft-multi-copy draft (list file))
      (setq mew-encode-syntax (mew-encode-syntax-initial-multi draftdir 1))
      (save-excursion
	(mew-draft-prepare-attachments t))))))

(defun mew-summary-multi-forward ()
  "Forward messages marked with '@' to a third person. A new draft 
is prepared in Draft mode and this message is automatically 
attached."
  (interactive)
  (mew-summary-multi-msgs
   (mew-current-set 'window (current-window-configuration))
   (let* ((draft (mew-folder-new-message mew-draft-folder))
	  (draftdir (file-name-nondirectory draft)))
     (mew-summary-prepare-draft
      (delete-other-windows)
      (let ((split-window-keep-point t))
	(split-window-vertically))
      (switch-to-buffer-other-window (find-file-noselect draft))
      (mew-draft-rename draft)
      (mew-draft-header nil 'nl)
      (mew-draft-mode)
      (run-hooks 'mew-draft-mode-newdraft-hook)
      (mew-draft-multi-copy draft FILES)
      (setq mew-encode-syntax
	    (mew-encode-syntax-initial-multi draftdir (length FILES)))
      (save-excursion
	(mew-draft-prepare-attachments t))))))

(defun mew-draft-multi-copy (draft files)
  (let* ((attach (mew-draft-to-mime draft))
	 (attachdir (mew-expand-folder attach)))
    (if (not (file-directory-p attachdir))
	(mew-make-directory attachdir))
    (while files
      (if mew-use-symbolic-link-for-forwarding
	  (mew-symbolic-link (car files) (mew-folder-new-message attach))
	(copy-file (car files) (mew-folder-new-message attach)))
      (setq files (cdr files)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Edit again and Resend
;;;

(defun mew-summary-edit-message ()
  (let ((msg (mew-summary-message-number))
	(part (mew-syntax-nums))
	(cache (mew-current-get 'cache))
	syntax doit)
    (cond
     ((and part (null cache)))
     (msg
      (setq doit t))
     (part
      (setq syntax (mew-syntax-get-entry
		    (mew-cache-decode-syntax cache) part))
      (if (not (mew-case-equal
		mew-ct-msg
		(mew-syntax-get-value (mew-syntax-get-ct syntax) 'cap)))
	  ()
	(setq doit t)
	(save-excursion
	  (mew-summary-goto-message)
	  (setq msg (mew-summary-message-number))))))
    (if (not doit)
	'mew-error
      (let* ((fld (mew-summary-folder-name))
	     (newdraftp (not (equal fld mew-draft-folder)))
	     draft attachdir)
	(mew-current-set 'window (current-window-configuration))
	(mew-window-configure (current-buffer) 'summary)
	(mew-summary-prepare-draft
	 (if (or part newdraftp)
	     (setq draft (mew-folder-new-message mew-draft-folder))
	   (setq draft (mew-expand-folder-get-msg fld msg)))
	 (setq attachdir (mew-attachdir draft))
	 ;; prepare draft file
	 (switch-to-buffer (find-file-noselect draft))
	 (mew-draft-rename draft)
	 (mew-delete-directory-recursively attachdir)
	 (cond
	  (part
	   (insert-buffer-substring
	    cache
	    (mew-syntax-get-begin syntax)
	    (mew-syntax-get-end (mew-syntax-get-part syntax))))
	  (newdraftp
	   ;; if fld equal mew-draft-folder, message already exists.
	   (insert-file-contents (mew-expand-folder-get-msg fld msg))))))
      nil)))

(defun mew-summary-edit-header ()
  (mew-elet
   (mew-header-delete-lines mew-field-delete-common)
   (mew-header-delete-lines mew-field-delete-for-reediting)
   (mew-header-goto-end)
   (mew-draft-header-fill mew-fcc:      mew-fcc)
   (mew-draft-header-fill mew-dcc:      mew-dcc)
   (mew-draft-header-fill mew-from:     mew-from)
   (mew-draft-header-fill mew-reply-to: mew-reply-to)
   (mew-draft-header-fill mew-x-mailer: mew-x-mailer)
   (if (and mew-use-config-imget-for-draft
	    (not (string-equal mew-config-imget mew-config-default)))
       (mew-draft-header-insert mew-config: mew-config-imget))
   (let ((ct (mew-addrstr-parse-value (mew-header-get-value mew-ct:))))
     (if (and ct (mew-case-equal mew-ct-txt ct))
	 (let ((mew-header-max-length nil)
	       (mew-header-max-depth nil))
	   (mew-header-delete-lines (list mew-ct: mew-cte:))
	   (mew-decode-rfc822-header 'no-property))))
   (mew-header-clear) ;; erase the old header separator
   (mew-header-prepared)
   (save-buffer)))

(defun mew-summary-resend ()
  "\\<mew-summary-mode-map>
Resend this message with Resent-To:. It is strongly 
discouraged to use this command since beginners are always 
confused. Please use '\\[mew-summary-forward]' instead."
  (interactive)
  (mew-summary-msg-or-part
   (if (mew-summary-edit-message)
       (message "Can't resend here.")
     (mew-elet
      (goto-char (point-min))
      (mew-header-delete-lines mew-field-delete-common)
      (mew-header-delete-lines mew-field-delete-for-resending)
      (goto-char (point-min))
      (insert mew-resent-to: " \n") ;; MUST use insert.
      (mew-draft-header-fill mew-resent-from: mew-from)
      (mew-draft-header-fill mew-fcc:  mew-fcc)
      (mew-draft-header-fill mew-dcc:  mew-dcc)
      (mew-header-goto-end)
      (mew-header-clear) ;; erase the old header separator
      (mew-header-prepared))
     (save-buffer)
     (mew-draft-mode)
     (run-hooks 'mew-draft-mode-reedit-hook)
     ;; move the cursor after "Resent-To: "
     (goto-char (point-min))
     (end-of-line))))

(defun mew-summary-reedit ()
  "Edit this message again to retry sending. Or edit this 
rfc822 part typically included MIME-encapsulated error message.
In a draft folder, it just edits the message. Otherwise, 
copy the message to draft folder, then edit.
See also mew-summary-edit-again."
  (interactive)
  (mew-summary-msg-or-part
   (if (mew-summary-edit-message)
       (message "Can't reedit here.")
     (mew-summary-edit-header)
     (mew-draft-mode)
     (run-hooks 'mew-draft-mode-reedit-hook))))

(defun mew-summary-edit-again ()
  "Edit an old fashioned error message in which the original message
is encapsulated after strings defined in 'mew-summary-edit-again-regex'
An example is \"----- Original message follows -----\". See also 
mew-summary-reedit."
  (interactive)
  (mew-summary-msg
   (let ((msg (mew-summary-message-number)) ;; must get msg here
	 (fld (mew-summary-folder-name))
	 draft attachdir)
     (mew-current-set 'window (current-window-configuration))
     (mew-window-configure (current-buffer) 'summary)
     (mew-summary-prepare-draft
      (cond
       ((equal fld mew-draft-folder)
	(setq draft (mew-expand-folder mew-draft-folder msg))
	;; the message already exists.
	(switch-to-buffer (find-file-noselect draft)))
       (t
	(setq draft (mew-folder-new-message mew-draft-folder))
	(switch-to-buffer (find-file-noselect draft))	
	(insert-file-contents (mew-expand-folder-get-msg fld msg))))
      (setq attachdir (mew-attachdir draft))
      (mew-draft-rename draft)
      (mew-delete-directory-recursively attachdir)
      (goto-char (point-min))
      (if (re-search-forward mew-summary-edit-again-regex nil t)
	  (progn
	    (forward-line)
	    ;; skip blank lines
	    (while (looking-at "^$") (forward-line))
	    (delete-region (point-min) (point))))
      (mew-summary-edit-header)
      (mew-draft-mode)
      (run-hooks 'mew-draft-mode-reedit-hook)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Packing
;;;

(defun mew-summary-pack ()
  "Pack messages and list them up again."
  (interactive)
  (mew-summary-only
   (let ((folder (buffer-name))
	 lines)
     (if (not (mew-summary-exclusive-p))
	 ()
       (mew-mark-clean)
       (if (and mew-ask-pack (not (y-or-n-p (format "Pack %s? " folder))))
	   ()
	 (setq lines (mew-summary-mark-collect3 mew-mark-review))
	 (setq mew-summary-buffer-process t)
	 (mew-im-call-process-no-output
	  (concat "Packing " folder)
	  mew-prog-impack
	  (format "--src=%s" folder))
	 (setq mew-summary-buffer-process nil)
	 (mew-erase-buffer) ;; for update
	 (mew-summary-scan-body mew-prog-imls
				'mew-summary-mode
				folder
				mew-cs-scan
				nil nil nil
				lines))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Good old days...
;;;

(defun mew-summary-unshar ()
  "Apply 'unshar' on messages marked with '@'."
  (interactive)
  (mew-summary-multi-msgs
   (if (not (y-or-n-p (format "Execute %s for these messages? " 
			      mew-prog-unshar)))
       ()
     (let ((dir (mew-summary-input-directory-name)))
       (message "Executing %s ... " mew-prog-unshar)
       (save-excursion
	 (mew-set-buffer-tmp dir)
	 (apply (function call-process) mew-prog-unshar nil nil nil FILES))
       (message "Executing %s ... done" mew-prog-unshar)))))

(defun mew-summary-uudecode ()
  "Apply 'uudecode' on messages marked with '@'."
  (interactive)
  (mew-summary-multi-msgs
   (if (not (y-or-n-p (format "Execute %s for these messages? "
			      mew-prog-uumerge)))
       ()
     (let ((dir (mew-summary-input-directory-name))
	   (tarfile nil)
	   (case-fold-search nil))
       (save-excursion
	 (mew-set-buffer-tmp dir)
	 (message "Executing %s ..." mew-prog-uumerge)
	 (apply (function call-process) mew-prog-uumerge nil t nil FILES)
	 (message "Executing %s ... done" mew-prog-uumerge)
	 (goto-char (point-min))
	 (if (looking-at "^uumerge:")
	     (message "Failed to executing %s" mew-prog-uumerge)
	   (forward-line)
	   (setq tarfile (mew-buffer-substring (point-min) (1- (point))))
	   (setq tarfile
		 (mew-summary-prog-exec mew-prog-compress "-df" "Z" tarfile))
	   (setq tarfile
		 (mew-summary-prog-exec mew-prog-gzip "-df" "gz" tarfile))
	   (if (string-match "^\\(.*\\)\\.tar$" tarfile)
	       (if (not (y-or-n-p (format "Execute %s for %s? " 
					  mew-prog-tar tarfile)))
		   ()
		 (message (format "Executing %s for %s ... "
				  mew-prog-tar tarfile))
		 (call-process mew-prog-tar nil nil nil "-xf" tarfile)
		 (message (format "Executing %s for %s ... done"
				  mew-prog-tar tarfile))))))))))

(defun mew-summary-prog-exec (prog opts suffix tarfile)
  (if (string-match (format "^\\(.*\\)\\.%s$" suffix) tarfile)
      (let ((data (match-data)))
	;; save match data here for OS/2
	(unwind-protect
	    (if (not (y-or-n-p (format "Execute %s for %s? " prog tarfile)))
		tarfile
	      (message (format "Executing %s for %s ... " prog tarfile))
	      (call-process prog nil nil nil opts tarfile)
	      (message (format "Executing %s for %s ... done" prog tarfile))
	      (set-match-data data)
	      (mew-match 1 tarfile))))
    tarfile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; These days...
;;;

(defun mew-summary-join ()
  "Concat Message/Partial fragments marked with '@' to an original
message."
  (interactive)
  (mew-summary-multi-msgs
   (let ((folder (mew-input-folder (mew-inbox-folder))))
     (apply 'mew-im-call-process-no-output
       (format "Joining marked messages to %s ..." folder)
       mew-prog-imjoin (format "--dst=%s" folder) FILES)
     (message "Joining marked messages to %s ... done" folder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Saving
;;;

(defun mew-summary-save (&optional askcs)
  "Save any parts. If the target is a message, you are asked which 
you want to save, the entire message or its body. If the target is
a non-message part, the part is saved (with line delimiter conversion
if it is a text object). When executed with '\\[universal-argument]', coding-system for
a text object to be saved can be specified."
  (interactive "P")
  (mew-summary-display nil) ;; need to ensure to make a cache
  (let* ((fld (mew-summary-folder-name))
	 (num (mew-syntax-number))
	 (nums (mew-syntax-number-to-nums num))
	 (cache (or (mew-current-get 'cache)
		    (mew-buffer-message)))
	 (syntax (mew-syntax-get-entry (mew-cache-decode-syntax cache) nums))
	 (action "Save")
	 PLUS1P limit attr
	 msg have-hdrp bodyp beg end cdpl file ctl cte
	 error doit append-p)
    (save-excursion
      ;; First of all, we should determine which part the user want to
      ;; save due to the ambiguity.
      ;; "y" on Message/Rfc822
      ;;     - msg/txt      the entire msg or its body?
      ;;     - msg/mul/txt  the entire msg or its part 1?
      ;; We have to make use of mew-decode-syntax
      ;; in the cache buffer due to the PGP/MIME dilemma.
      ;; We need the correct LIMIT.
      (if (mew-syntax-message-p syntax)
	  (let ((bodyname "the body") body bct plus1p)
	    (setq body (mew-syntax-get-part syntax))
	    (if (mew-syntax-multipart-p body)
		(progn
		  (setq plus1p t)
		  (setq bodyname "the part 1 text")
		  (setq body (mew-syntax-get-entry body '(1)))))
	    (setq bct (car (mew-syntax-get-ct body)))
	    (if (mew-case-equal mew-ct-txt bct)
		(if (y-or-n-p (format "Save the entire message (y) or %s (n) " bodyname))
		    (setq have-hdrp t)
		  (if plus1p
		      (setq nums (nreverse (cons 1 (nreverse nums))))
		    (setq bodyp t))
		  (setq PLUS1P (mew-syntax-get-privacy body)))
	      (setq have-hdrp t))))
      ;; Now, let's analyze the message in the burst buffer.
      ;; This is lengthy, though, avoidable.
      (mew-summary-goto-message)
      (setq msg (mew-summary-message-number))
      (set-buffer (get-buffer-create mew-buffer-burst))
      (setq attr (mew-cache-attribute-get (mew-expand-folder fld msg)))
      (if PLUS1P
	  (setq limit (1+ (length nums))) ;; VERY important for PGP/MIME
	(setq limit (length nums)))
      (setq mew-decode-DECODE nil)
      (if (and (equal mew-decode-LIMIT limit)
	       (equal mew-cache-folder fld)
	       (equal mew-cache-message-number msg)
	       (equal mew-cache-attribute attr))
	  ()
	(mew-erase-buffer)
	(mew-insert-message fld msg mew-cs-text-for-read nil)
	(setq mew-decode-LIMIT limit)
	(setq mew-cache-folder fld)
	(setq mew-cache-message-number msg)
	(setq mew-cache-attribute attr)
	(condition-case nil
	    (setq mew-decode-syntax
		  (mew-decode-message (mew-decode-syntax-rfc822-head) 0))
	  (error
	   (setq error (concat "MIME decoding error: " mew-decode-error)))))
      (if error
	  (message error)
	;;
	(setq syntax (mew-syntax-get-entry mew-decode-syntax nums))
	(if bodyp (setq syntax (mew-syntax-get-part syntax)))
	;;
	(setq beg (mew-syntax-get-begin syntax))
	(if (mew-syntax-message-p syntax)
	    (setq end (mew-syntax-get-end (mew-syntax-get-part syntax)))
	  (setq end (mew-syntax-get-end syntax)))
	(setq ctl (mew-syntax-get-ct syntax))
	(setq cte (mew-syntax-get-cte syntax))
	;; file name
	(and syntax (setq cdpl (mew-syntax-get-cdp syntax)))
	(and (car cdpl)
	     (setq file (mew-syntax-get-param cdpl "filename")))
	(and file (equal (mew-charset-guess-string file)
			 mew-error-charset-unknown)
	     (setq file nil))
	(setq file (mew-summary-input-file-name nil file))
	;;
	(if (not (file-exists-p file))
	    (setq doit t)
	  (if (null mew-file-append-p)
	      (setq action "Overwrite")
	    (setq action "Append")
	    (setq append-p t))
	  (if (y-or-n-p (format "File exists. %s it to %s? " action file))
	      (setq doit t)))
	;;
	(if (not doit)
	    (message "Didn't save anything.")
	  (let (linebasep tocs)
	    (if (and askcs mew-mule-p)
		(setq tocs (read-coding-system "Coding-system: ")))
	    (mew-set-buffer-tmp)
	    (insert-buffer-substring mew-buffer-burst beg end)
	    (goto-char (point-min))
	    (setq linebasep (mew-decode-mime-body ctl cte (or tocs t)))
	    (if have-hdrp
		(progn
		  (goto-char (point-min))
		  (mew-header-delete-lines mew-field-delete-common)
		  (mew-header-delete-lines mew-field-delete-for-saving)))
	    (mew-frwlet
	     mew-cs-dummy
	     (if linebasep mew-cs-text-for-write mew-cs-binary)
	     (write-region (point-min) (point-max) file append-p))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Burst
;;;

(defun mew-summary-burst-body (fld msg folder)
  (save-excursion
    (let (entry multi mstr m n len)
      (set-buffer (get-buffer-create mew-buffer-burst))
      (mew-erase-buffer)
      (mew-insert-message fld msg mew-cs-text-for-read nil)
      (setq mew-decode-LIMIT 1)
      (setq mew-decode-DECODE nil)
      (setq mew-cache-message-number nil)
      (setq mew-cache-attribute nil)
      (condition-case nil
	  (progn
	    (setq mew-decode-syntax
		  (mew-decode-message (mew-decode-syntax-rfc822-head) 0))
	    (setq multi (mew-syntax-get-part mew-decode-syntax))
	    (if (not (mew-syntax-multipart-p multi))
		(message "Can't burst")
	      (if (not (mew-folder-check folder))
		  (error "%s is wrong. Nothing was processed." folder)
		(setq mstr (mew-folder-new-message folder t))
		(if (not (stringp mstr))
		    (error "Error in %s. Nothing was processed" folder)
		  (setq m (string-to-int mstr))
		  (setq len (- (length multi) mew-syntax-magic))
		  (setq n 1)
		  (while (<= n len)
		    (setq entry (mew-syntax-get-entry mew-decode-syntax
						      (list n)))
		    (if (not (mew-case-equal
			      (mew-syntax-get-value (mew-syntax-get-ct entry))
			      mew-ct-msg))
			()
		      (mew-frwlet
		       mew-cs-dummy mew-cs-text-for-write
		       (write-region
			(mew-syntax-get-begin entry)
			;; This is RFC 822 message.
			;; So, body is a single text/plain.
			(mew-syntax-get-end (mew-syntax-get-part entry))
			(mew-expand-folder folder (int-to-string m))))
		      (setq m (1+ m)))
		    (setq n (1+ n)))
		  (list mstr (int-to-string (1- m)))))))
	(error
	 (message "MIME decoding error: %s" mew-decode-error)
	 nil)))))

(defun mew-summary-burst ()
  "De-capsulate messages embedded in this message."
  (interactive)
  (mew-summary-msg-or-part
   (let ((fld (mew-summary-folder-name))
	 (folder (mew-input-folder (mew-inbox-folder)))
	  msg ret)
     (save-excursion
       (mew-summary-goto-message)
       (setq msg (mew-summary-message-number)))
     (message "Bursting ... ")
     (setq ret (mew-summary-burst-body fld msg folder))
     (if (null ret)
	 () ;; error
       (message "Bursting ... done")
       (mew-touch-folder folder)
       (if (y-or-n-p (format "Go to %s? " folder))
	   (mew-summary-goto-folder t folder))
       (message "Messages from %s to %s were extracted in %s"
		(nth 0 ret) (nth 1 ret) folder)))))

(defun mew-summary-burst-multi ()
  "De-capsulate messages embedded in the messages marked with '@'."
  (interactive)
  (mew-summary-multi-msgs
   (let ((folder (mew-input-folder (mew-inbox-folder)))
	 (targets FLD-MSG-LIST))
     (message "Bursting ... ")
     (while targets
       (mew-summary-burst-body (car (car targets)) (cdr (car targets)) folder)
       (setq targets (cdr targets)))
     (message "Bursting ... done")
     (mew-touch-folder folder)
     (if (y-or-n-p (format "Go to %s? " folder))
	 (mew-summary-goto-folder t folder)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Jumping
;;;

(defun mew-summary-jump-message (&optional msg)
  "Jump to a message according to the number which you input.
If 'mew-summary-jump-message-then-display' is non-nil,
the message is then displayed."
  (interactive)
  (let ((here (point)))
    (if (null msg) (setq msg (read-string "Message No. : " "")))
    (cond 
     ((equal msg "") ())
     ((equal msg t) ;; xxx
      (goto-char (point-max))) ;; (forward-line -1)
     (t 
      (goto-char (point-min))
      (if (re-search-forward (format "^[ ]*%s[^0-9]+" msg) nil t) ;; xxx regex?
	  (progn
	    (beginning-of-line)
	    (if mew-summary-jump-message-then-display
		(mew-summary-display nil)))
	(goto-char here))))))

(defun mew-summary-jump-to-draft-buffer ()
  "Jump to the newest draft if exists."
  (interactive)
  (let ((bufs (buffer-list))
	draft-bufs)
    (while bufs
      (if (string-match (concat "^" (regexp-quote mew-draft-folder))
			(buffer-name (car bufs)))
	  (setq draft-bufs (cons (buffer-name (car bufs)) draft-bufs)))
      (setq bufs (cdr bufs)))
    (cond
     ((null draft-bufs)
      (message "No draft buffer exist!"))
     (t
      (switch-to-buffer
       (car (sort draft-bufs (function (lambda (a b) (not (string< a b)))))))
      ))))

(defun mew-summary-jump-top ()
  "Go to the beginning of this Summary mode.
If 'mew-summary-jump-top-then-display' is non-nil, 
the top message is then displayed."
  (interactive)
  (goto-char (point-min))
  (if mew-summary-jump-top-then-display
      (mew-summary-display nil)))

(defun mew-summary-jump-bottom ()
  "Go to the end of this Summary mode.
If 'mew-summary-jump-bottom-then-display' is non-nil, 
the top message is then displayed."
  (interactive)
  (goto-char (point-max))
  (if (not (bobp)) (forward-line -1))
  (if mew-summary-jump-bottom-then-display
      (mew-summary-display nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Searching
;;;

(defun mew-summary-isearch-forward ()
  "Incremental search forward in Message mode."
  (interactive)
  (let ((cwin (get-buffer-window (current-buffer)))
	(mwin (get-buffer-window (mew-buffer-message))))
    (if (not mwin)
	(message "No message is displayed.")
      (select-window mwin)
      (unwind-protect
	  (isearch-forward)
	(select-window cwin)))))

(defun mew-summary-isearch-backward ()
  "Incremental search backward in Message mode."
  (interactive)
  (let ((cwin (get-buffer-window (current-buffer)))
	(mwin (get-buffer-window (mew-buffer-message))))
    (if (not mwin)
	(message "No message is displayed.")
      (select-window mwin)
      (unwind-protect
	  (isearch-backward)
	(select-window cwin)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pipe and Printing
;;;

(defun mew-summary-pipe-message (prefix command)
  "Send this message via pipe."
  (interactive
   (list current-prefix-arg 
	 (read-string "Shell command on message: " mew-last-shell-command)))
  (mew-summary-display 'force)
  (if (y-or-n-p "Send this message to pipe? ")
      (save-excursion
	(set-buffer (mew-buffer-message))
	(save-restriction
	  (widen)
	  (if (string= command "")
	      (setq command mew-last-shell-command))
	  (goto-char (point-min)) ; perhaps this line won't be necessary
	  (if prefix
	      (search-forward "\n\n"))
	  (shell-command-on-region (point) (point-max) command nil)
	  (setq mew-last-shell-command command)))))

(defun mew-summary-print ()
  "Print this message or this part."
  (interactive)
  (mew-summary-display 'force)
  (if (y-or-n-p "Print this message? ")
      (save-excursion
	(set-buffer (mew-buffer-message))
	(save-restriction
	  (widen)
	  (funcall mew-print-function)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Misc
;;;

(defun mew-summary-convert-local-cs (&optional arg)
  "Convert to character sets used locally. If executed with '\\[universal-argument]',
coding-system is asked."
  (interactive "P")
  (mew-summary-msg-or-part
   (if mew-mule-p
       (save-excursion
	 (set-buffer (mew-buffer-message))
	 (let* ((code (and arg (read-coding-system "Coding-system: ")))
		(win (get-buffer-window (current-buffer)))
		(start (window-start win)))
	   (widen)
	   (if (mew-header-p)
	       (progn
		 (goto-char (mew-header-end))
		 (forward-line))
	     (goto-char (point-min)))
	   (mew-elet
	    (mew-cs-decode-region (point) (point-max)
				  (or code mew-cs-rfc822-trans)))
	   (set-window-start win start)
	   (set-buffer-modified-p nil))))))

(defun mew-summary-x-face ()
  "Display xface."
  (interactive)
  (mew-summary-msg
   (save-excursion
     (let (xface)
       (set-buffer (mew-buffer-message))
       (if (null (setq xface (mew-header-get-value mew-x-face:)))
	   ()
	 (mew-set-buffer-tmp)
	 (insert xface)
	 (let ((filters mew-x-face-filter) file)
	   (while filters
	     ;; call-process-region is OK...
	     (mew-plet
	      (call-process-region (point-min) (point-max) 
				   (car filters)
				   'delete t nil))
	     (setq filters (cdr filters)))
	   (setq file (mew-make-temp-name))
	   ;; NEVER use call-process-region for privary reasons
	   (mew-flet
	    (write-region (point-min) (point-max) file nil 'no-msg))
	   (mew-mime-start-process mew-x-face-prog mew-x-face-args file)))))))

(defun mew-flushable-p ()
  (and mew-queue-path
       (file-directory-p mew-queue-path)
       (directory-files mew-queue-path nil "^[0-9]+$" 'no-sort)))

(defun mew-summary-flush-queue ()
  "Flush the mail queue explicitly."
  (interactive)
  (if (not (mew-flushable-p))
      (message "Can't flush messages now")
    (if (or (not mew-ask-flush-queue) (y-or-n-p "Flush queue? "))
	(mew-im-call-process-no-output "Sending message" mew-prog-imput "-q"))))

(provide 'mew-summary)

;;; Copyright Notice:

;; Copyright (C) 1996, 1997, 1998, 1999 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-summary.el ends here

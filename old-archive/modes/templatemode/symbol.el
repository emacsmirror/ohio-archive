;;; symbol.el -- Identifier completion facilities
;;; Copyright (C) 1987 Mark A. Ardis.

(provide 'symbol)

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defvar sym-completion-buffer nil
  "Buffer containing user-entered symbols for identifier completion."
) ; sym-completion-buffer
(make-variable-buffer-local 'sym-completion-buffer)

(defvar sym-end-display "<<"
  "*Display marker after string to be created in symbol mode."
) ; sym-end-display

(defvar sym-end-marker (make-marker)
  "Marker at end of symbol."
) ; sym-end-marker
(make-variable-buffer-local 'sym-end-marker)
(setq-default sym-end-marker (make-marker))

(defvar sym-original nil
  "Text string originally present, to be replaced in symbol-mode."
) ; sym-original
(make-variable-buffer-local 'sym-original)

(defvar sym-start-display ">>"
  "*Display marker before string to be created in symbol mode."
) ; sym-start-display

(defvar sym-start nil
  "Position of start of symbol in buffer."
) ; sym-start
(make-variable-buffer-local 'sym-start)

(defvar symbol-mode-map nil
  "Key-map for Symbol Mode."
) ; symbol-mode-map

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun symbol-mode ()
  "Major mode for inserting symbols in place.  Like any other mode,
    except that newline terminates it, and the following commands
    are available:
      TAB  attempt to complete last identifier typed
    Meant to be called from sym-read-string, which initializes."
  (interactive)
					; Local Variables
  (let ()
					; Body
    (or symbol-mode-map
      (sym-set-local-keys))
    (use-local-map symbol-mode-map)
    (setq major-mode 'symbol-mode)
    (setq mode-name "Symbol")
  ) ; let
) ; defun symbol-mode

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun add-symbol ()
  "Add the symbol before point to the template-mode completion list."
  (interactive)
					; Local Variables
  (let (start)
					; Body
    (forward-word -1)
    (setq start (point))
    (forward-word 1)
    (sym-enter-id (buffer-substring start (point)))
  ) ; let
) ; defun add-symbol

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun expand-symbol ()
  "Expand the symbol before point using the template-mode completion list."
  (interactive)
					; Local Variables
  (let (start result)
					; Body
    (forward-word -1)
    (setq start (point))
    (forward-word 1)
    (setq result (sym-complete-id (buffer-substring start (point))))
    (backward-kill-word 1)
    (insert-before-markers result)
  ) ; let
) ; defun expand-symbol

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-abort-recursive-edit ()
  "Catch abort and repair entry."
  (interactive)
					; Local Variables
  (let ()
					; Body
    (sym-reposition-point)
    (if (< sym-start (marker-position sym-end-marker))
      (progn
	(delete-region sym-start (marker-position sym-end-marker))
	(sym-reset-display)
      ) ; progn
    ) ; if
    (abort-recursive-edit)
  ) ; let
) ; defun sym-illegal-command

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-backward-char ()
  "Move point left one character, if possible."
					; Local Variables
  (interactive)
  (let ()
					; Body
    (sym-reposition-point)
    (if (> (point) sym-start)
      (backward-char 1)
					; else cannot advance
      (ding)
    ) ; if
  ) ; let
) ; defun sym-backward-char

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-backward-kill-word ()
  "Delete the previous word in symbol-mode."
					; Local Variables
  (interactive)
  (let ()
					; Body
    (sym-reposition-point)
    (if (> (point) sym-start)
      (progn
	(backward-kill-word 1)
					; If empty input, restore original
	(sym-reset-display)
      ) ; progn
					; else cannot delete
      (ding)
    ) ; if
  ) ; let
) ; defun sym-backward-kill-word

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-backward-word ()
  "Move point left one word, if possible."
					; Local Variables
  (interactive)
  (let ()
					; Body
    (sym-reposition-point)
    (if (> (point) sym-start)
      (backward-word 1)
					; else cannot advance
      (ding)
    ) ; if
  ) ; let
) ; defun sym-backward-word

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-complete-id (string)
  "Expand the identifier STRING using the completion table."
					; Local Variables
  (let (start text-buffer)
					; Body
    (message "Expanding %s..." string)
    (setq text-buffer (buffer-name))
    (set-buffer sym-completion-buffer)
					; Search for match
    (goto-char (point-min))
    (if (re-search-forward (concat "^" string) (point-max) t)
      (progn
	(beginning-of-line)
	(setq start (point))
	(end-of-line)
	(setq string (buffer-substring start (point)))
      ) ; progn
    ) ; if
    (set-buffer text-buffer)
    (bury-buffer sym-completion-buffer)
    (message "Expanding done.")
					; Return expanded string
    string
  ) ; let
) ; defun sym-complete-id

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-create-display ()
  "Create the displayed string (if necessary) in symbol-mode."
					; Local Variables
  (let ()
					; Body
    (if (= sym-start (marker-position sym-end-marker))
      (progn
					; Remove the original string
	(delete-char (length sym-original))
					; Insert display
	(insert-before-markers sym-start-display)
	(insert-before-markers sym-end-display)
	(search-backward sym-end-display)
					; Reset the markers to empty
	(set-marker sym-end-marker (point))
	(setq sym-start (point))
      ) ; progn
    ) ; if
  ) ; let
) ; defun sym-create-display

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-delete-backward-char ()
  "Delete the previous character created in symbol-mode."
					; Local Variables
  (interactive)
  (let ()
					; Body
    (sym-reposition-point)
    (if (> (point) sym-start)
      (progn
	(delete-backward-char 1)
					; If empty input, restore original
	(sym-reset-display)
      ) ; progn
					; else cannot delete
      (ding)
    ) ; if
  ) ; let
) ; defun sym-delete-backward-char

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-delete-char ()
  "Delete the following character created in symbol-mode."
					; Local Variables
  (interactive)
  (let ()
					; Body
    (sym-reposition-point)
    (if (< (point) (marker-position sym-end-marker))
      (progn
	(delete-char 1)
					; If empty input, restore original
	(sym-reset-display)
      ) ; progn
					; else cannot delete
      (ding)
    ) ; if
  ) ; let
) ; defun sym-delete-char

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-enter-id (string)
  "Enter STRING into the identifier completion table."
  (interactive)
					; Local Variables
  (let (text-buffer prefix suffix found end-line)
					; Body
    (message "Adding %s..." string)
    (setq text-buffer (buffer-name))
    (set-buffer sym-completion-buffer)
    (beginning-of-buffer)
    (setq start (point))
    (end-of-line nil)
    (setq found (buffer-substring start (point)))
    (setq more t)
    (while (and more (string-lessp found string))
      (if (not (eobp))
	(progn
	  (beginning-of-line 2)
	  (setq start (point))
	  (end-of-line nil)
	  (setq found (buffer-substring start (point)))
	) ; progn
      ; else
	(progn
	  (setq more nil)
	  (if (not (bobp))
	    (newline)
	  ) ; if
	) ; progn
      ) ; if
    ) ; while
    (if more
      (if (not (string-equal found string))
	(progn
	  (beginning-of-line nil)
	  (insert string "\n")
	) ; progn
      ) ; if not equal
    ; else
      (insert string)
    ) ; if more
    (set-buffer text-buffer)
    (bury-buffer sym-completion-buffer)
    (message "Adding Done." string)
  ) ; let
) ; defun sym-enter-id

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-expand-last-id ()
  "Try to expand last identifier created in symbol-mode."
  (interactive)
					; Local Variables
  (let (end-id string)
					; Body
    (sym-reposition-point)
    (if (> (point) sym-start)
      (progn
	(setq end-id (point))
	(re-search-backward "\\W")
	(if (< (point) end-id)
	  (forward-char 1)
	) ; if
	(if (< (point) sym-start)
	  (goto-char sym-start)
	) ; if
	(setq string (buffer-substring (point) end-id))
	(if (> (length string) 0)
	  (progn
	    (delete-char (length string))
	    (setq string (sym-complete-id string))
	    (insert-before-markers string)
	  ) ; progn
	) ; if
      ) ; progn
					; else cannot expand empty string
      (ding)
    ) ; if
  ) ; let
) ; defun sym-expand-last-id

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-forward-char ()
  "Move point right one character, if possible."
					; Local Variables
  (interactive)
  (let ()
					; Body
    (sym-reposition-point)
    (if (< (point) (marker-position sym-end-marker))
      (forward-char 1)
					; else cannot advance
      (ding)
    ) ; if
  ) ; let
) ; defun sym-forward-char

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-forward-word ()
  "Move point right one word, if possible."
					; Local Variables
  (interactive)
  (let ()
					; Body
    (sym-reposition-point)
    (if (< (point) (marker-position sym-end-marker))
      (forward-word 1)
					; else cannot advance
      (ding)
    ) ; if
  ) ; let
) ; defun sym-forward-word

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-illegal-command ()
  "Catch all illegal symbol-mode commands."
  (interactive)
					; Local Variables
  (let ()
					; Body
    (ding)
    (message "That command is not allowed in symbol-mode.")
  ) ; let
) ; defun sym-illegal-command

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-kill-line ()
  "Kill the current symbol line in symbol-mode"
					; Local Variables
  (interactive)
  (let ()
					; Body
    (sym-reposition-point)
    (if (< (point) (marker-position sym-end-marker))
      (progn
	(delete-region (point) (marker-position sym-end-marker))
					; If empty input, restore original
	(sym-reset-display)
      ) ; progn
					; else cannot delete
      (ding)
    ) ; if
  ) ; let
) ; defun sym-kill-line

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-kill-word ()
  "Delete the following word in symbol-mode."
					; Local Variables
  (interactive)
  (let ()
					; Body
    (sym-reposition-point)
    (if (< (point) (marker-position sym-end-marker))
      (progn
	(kill-word 1)
					; If empty input, restore original
	(sym-reset-display)
      ) ; progn
					; else cannot delete
      (ding)
    ) ; if
  ) ; let
) ; defun sym-kill-word

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-position-end ()
  "Position point at end of symbol line."
  (interactive)
					; Local Variables
  (let ()
					; Body
    (goto-char (marker-position sym-end-marker))
  ) ; let
) ; defun sym-position-end

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-position-start ()
  "Position point at start of symbol line."
  (interactive)
					; Local Variables
  (let ()
					; Body
    (goto-char sym-start)
  ) ; let
) ; defun sym-position-start

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-read-string (prompt original)
  "Read characters and insert them at point.  First arg PROMPT is a
    message to prompt the user.  Second arg ORIGINAL is an initial
    string to use if all input is deleted."
					; Local Variables
  (let (save-mode save-name save-keymap sym-input sym-valid-response)
					; Body
					; Initialize state
    (setq sym-start (point))
    (setq sym-end-marker (make-marker))
    (set-marker sym-end-marker (point))
    (setq sym-original original)
					; Save mode variables
    (setq save-mode major-mode)
    (setq save-name mode-name)
    (setq save-keymap (current-local-map))
    (symbol-mode)
    (message "%s" prompt)
					; Wait for user's creation
    (setq sym-input original)
    (unwind-protect

  (progn
(setq sym-valid-response nil)
(while (not sym-valid-response)
  (recursive-edit)
					; Pick up created string
  (setq sym-input
    (buffer-substring sym-start (marker-position sym-end-marker)))
  (sym-reposition-point)
					; If invalid try again
  (if (and (boundp 'sym-check-validity-hook)
    sym-check-validity-hook)
  (progn
    (setq sym-valid-response
      (funcall sym-check-validity-hook sym-input))
    ) ; progn
  ; else
    (setq sym-valid-response t)
  ) ; if
) ; while
  ) ; progn
					; Restore display string
(if (< sym-start (marker-position sym-end-marker))
  (progn
    (goto-char sym-start)
    (delete-backward-char (length sym-start-display))
    (goto-char (marker-position sym-end-marker))
    (delete-char (length sym-end-display))
  ) ; progn
) ; if
					; Restore mode variables
(setq major-mode save-mode)
(setq mode-name save-name)
(use-local-map save-keymap)
) ; unwind-protect
					; Return string entered
sym-input
) ; let
) ; defun sym-read-string

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-reposition-point ()
  "Reposition point within the symbol line, if necessary."
  (interactive)
					; Local Variables
  (let ()
					; Body
    (if (or (< (point) sym-start)
      (> (point) (marker-position sym-end-marker)))
    (goto-char (marker-position sym-end-marker))
  ) ; if
) ; let
) ; defun sym-reposition-point

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-reset-display ()
  "Reset the displayed string in symbol-mode."
					; Local Variables
  (let ()
					; Body
    (if (= sym-start (marker-position sym-end-marker))
      (progn
					; Remove the display markers
	(delete-backward-char (length sym-start-display))
	(delete-char (length sym-end-display))
					; Insert original string
	(insert-before-markers sym-original)
	(search-backward sym-original)
					; Reset the markers to empty
	(set-marker sym-end-marker (point))
	(setq sym-start (point))
      ) ; progn
    ) ; if
  ) ; let
) ; defun sym-reset-display

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-self-insert-command ()
  "Insert this character in symbol-mode."
  (interactive)
					; Local Variables
  (let (sym-char)
					; Body
    (setq sym-char (char-to-string last-input-char))
    (sym-reposition-point)
    (sym-create-display)
    (insert-before-markers sym-char)
    (if (and (= ?\) (char-syntax last-input-char))
      blink-matching-paren)
    (blink-matching-open)
  ) ; if
) ; let
) ; defun sym-self-insert-command

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-set-local-keys ()
  "Create key-map for Symbol Mode."
					; Local Variables
  (let (empty-keymap)
					; Body
    (setq empty-keymap (make-keymap))
    (setq symbol-mode-map empty-keymap)
    (suppress-keymap symbol-mode-map t)

  (define-key symbol-mode-map "\177" 'sym-delete-backward-char) ; DEL

					; Control keys
  (define-key symbol-mode-map "\C-a" 'sym-position-start)
  (define-key symbol-mode-map "\C-b" 'sym-backward-char)
					; ^C is a prefix
  (define-key symbol-mode-map "\C-d" 'sym-delete-char)
  (define-key symbol-mode-map "\C-e" 'sym-position-end)
  (define-key symbol-mode-map "\C-f" 'sym-forward-char)
  (define-key symbol-mode-map "\C-g" 'sym-abort-recursive-edit)
					; ^H is okay
  (define-key symbol-mode-map "\C-i" 'sym-expand-last-id) ; TAB
  (define-key symbol-mode-map "\C-j" 'exit-recursive-edit) ; LFD
  (define-key symbol-mode-map "\C-k" 'sym-kill-line)
					; ^L is okay
  (define-key symbol-mode-map "\C-m" 'exit-recursive-edit) ; CR
					; ^N is okay
  (define-key symbol-mode-map "\C-o" 'sym-illegal-command)
					; ^P is okay
  (define-key symbol-mode-map "\C-q" 'sym-illegal-command)
  (define-key symbol-mode-map "\C-r" 'sym-illegal-command)
  (define-key symbol-mode-map "\C-s" 'sym-illegal-command)
  (define-key symbol-mode-map "\C-t" 'sym-transpose-chars)


  (define-key symbol-mode-map "\C-u" 'sym-illegal-command)
					; ^V is okay
  (define-key symbol-mode-map "\C-w" 'sym-illegal-command)
					; ^X is a prefix
  (define-key symbol-mode-map "\C-y" 'sym-illegal-command)
					; ^Z is okay

					; ^X prefix keys
  (define-key symbol-mode-map "\C-x\t" 'sym-illegal-command)
  (define-key symbol-mode-map "\C-x\C-l" 'sym-illegal-command)
  (define-key symbol-mode-map "\C-x\C-o" 'sym-illegal-command)
  (define-key symbol-mode-map "\C-x\C-t" 'sym-illegal-command)
  (define-key symbol-mode-map "\C-x\C-u" 'sym-illegal-command)
  (define-key symbol-mode-map "\C-xg" 'sym-illegal-command)
  (define-key symbol-mode-map "\C-xi" 'sym-illegal-command)
  (define-key symbol-mode-map "\C-xk" 'sym-illegal-command)
  (define-key symbol-mode-map "\C-x\177" 'sym-illegal-command) ; ^X DEL

					; ESC prefix keys
  (define-key symbol-mode-map "\e\C-b" 'sym-backward-word)
  (define-key symbol-mode-map "\e\C-c" 'exit-recursive-edit)
  (define-key symbol-mode-map "\e\C-f" 'sym-forward-word)
  (define-key symbol-mode-map "\e\C-k" 'sym-illegal-command)
  (define-key symbol-mode-map "\e\C-o" 'sym-illegal-command)
  (define-key symbol-mode-map "\e\C-s" 'sym-illegal-command)
  (define-key symbol-mode-map "\e\C-t" 'sym-illegal-command)
  (define-key symbol-mode-map "\e\C-w" 'sym-illegal-command)
  (define-key symbol-mode-map "\e\C-\\" 'sym-illegal-command)
  (define-key symbol-mode-map "\e " 'sym-illegal-command) ; ESC SPACE
  (define-key symbol-mode-map "\e%" 'sym-illegal-command)
  (define-key symbol-mode-map "\e(" 'sym-illegal-command)
  (define-key symbol-mode-map "\e)" 'sym-illegal-command)
  (define-key symbol-mode-map "\e;" 'sym-illegal-command)
  (define-key symbol-mode-map "\e<" 'sym-position-start)
  (define-key symbol-mode-map "\e>" 'sym-position-end)
  (define-key symbol-mode-map "\e^" 'sym-illegal-command)
  (define-key symbol-mode-map "\eb" 'sym-backward-word)
  (define-key symbol-mode-map "\ed" 'sym-kill-word)
  (define-key symbol-mode-map "\ef" 'sym-forward-word)
  (define-key symbol-mode-map "\eg" 'sym-illegal-command)
  (define-key symbol-mode-map "\ei" 'sym-illegal-command)
  (define-key symbol-mode-map "\ej" 'sym-illegal-command)
  (define-key symbol-mode-map "\ek" 'sym-illegal-command)
  (define-key symbol-mode-map "\eq" 'sym-illegal-command)
  (define-key symbol-mode-map "\et" 'sym-illegal-command)
  (define-key symbol-mode-map "\ey" 'sym-illegal-command)
  (define-key symbol-mode-map "\ez" 'sym-illegal-command)
  (define-key symbol-mode-map "\e\177" 'sym-backward-kill-word) ; ESC DEL

					; Self-inserting keys

  (define-key symbol-mode-map " " 'sym-self-insert-command) ; SPACE
  (define-key symbol-mode-map "!" 'sym-self-insert-command)
  (define-key symbol-mode-map "\042" 'sym-self-insert-command) ; DOUBLE QUOTE
  (define-key symbol-mode-map "#" 'sym-self-insert-command)
  (define-key symbol-mode-map "$" 'sym-self-insert-command)
  (define-key symbol-mode-map "%" 'sym-self-insert-command)
  (define-key symbol-mode-map "&" 'sym-self-insert-command)
  (define-key symbol-mode-map "\047" 'sym-self-insert-command) ; SINGLE QUOTE
  (define-key symbol-mode-map "(" 'sym-self-insert-command)
  (define-key symbol-mode-map ")" 'sym-self-insert-command)
  (define-key symbol-mode-map "*" 'sym-self-insert-command)
  (define-key symbol-mode-map "+" 'sym-self-insert-command)
  (define-key symbol-mode-map "," 'sym-self-insert-command)
  (define-key symbol-mode-map "-" 'sym-self-insert-command)
  (define-key symbol-mode-map "." 'sym-self-insert-command)
  (define-key symbol-mode-map "/" 'sym-self-insert-command)

  (define-key symbol-mode-map "0" 'sym-self-insert-command)
  (define-key symbol-mode-map "1" 'sym-self-insert-command)
  (define-key symbol-mode-map "2" 'sym-self-insert-command)
  (define-key symbol-mode-map "3" 'sym-self-insert-command)
  (define-key symbol-mode-map "4" 'sym-self-insert-command)
  (define-key symbol-mode-map "5" 'sym-self-insert-command)
  (define-key symbol-mode-map "6" 'sym-self-insert-command)
  (define-key symbol-mode-map "7" 'sym-self-insert-command)
  (define-key symbol-mode-map "8" 'sym-self-insert-command)
  (define-key symbol-mode-map "9" 'sym-self-insert-command)

  (define-key symbol-mode-map ":" 'sym-self-insert-command)
  (define-key symbol-mode-map ";" 'sym-self-insert-command)
  (define-key symbol-mode-map "<" 'sym-self-insert-command)
  (define-key symbol-mode-map "=" 'sym-self-insert-command)
  (define-key symbol-mode-map ">" 'sym-self-insert-command)
  (define-key symbol-mode-map "?" 'sym-self-insert-command)
  (define-key symbol-mode-map "@" 'sym-self-insert-command)

  (define-key symbol-mode-map "A" 'sym-self-insert-command)
  (define-key symbol-mode-map "B" 'sym-self-insert-command)
  (define-key symbol-mode-map "C" 'sym-self-insert-command)
  (define-key symbol-mode-map "D" 'sym-self-insert-command)
  (define-key symbol-mode-map "E" 'sym-self-insert-command)
  (define-key symbol-mode-map "F" 'sym-self-insert-command)
  (define-key symbol-mode-map "G" 'sym-self-insert-command)
  (define-key symbol-mode-map "H" 'sym-self-insert-command)
  (define-key symbol-mode-map "I" 'sym-self-insert-command)
  (define-key symbol-mode-map "J" 'sym-self-insert-command)
  (define-key symbol-mode-map "K" 'sym-self-insert-command)
  (define-key symbol-mode-map "L" 'sym-self-insert-command)
  (define-key symbol-mode-map "M" 'sym-self-insert-command)
  (define-key symbol-mode-map "N" 'sym-self-insert-command)
  (define-key symbol-mode-map "O" 'sym-self-insert-command)
  (define-key symbol-mode-map "P" 'sym-self-insert-command)
  (define-key symbol-mode-map "Q" 'sym-self-insert-command)
  (define-key symbol-mode-map "R" 'sym-self-insert-command)
  (define-key symbol-mode-map "S" 'sym-self-insert-command)
  (define-key symbol-mode-map "T" 'sym-self-insert-command)
  (define-key symbol-mode-map "U" 'sym-self-insert-command)
  (define-key symbol-mode-map "V" 'sym-self-insert-command)
  (define-key symbol-mode-map "W" 'sym-self-insert-command)
  (define-key symbol-mode-map "X" 'sym-self-insert-command)
  (define-key symbol-mode-map "Y" 'sym-self-insert-command)
  (define-key symbol-mode-map "Z" 'sym-self-insert-command)

  (define-key symbol-mode-map "[" 'sym-self-insert-command)
  (define-key symbol-mode-map "\134" 'sym-self-insert-command) ; BACKSLASH
  (define-key symbol-mode-map "]" 'sym-self-insert-command)
  (define-key symbol-mode-map "^" 'sym-self-insert-command)
  (define-key symbol-mode-map "_" 'sym-self-insert-command)
  (define-key symbol-mode-map "`" 'sym-self-insert-command)

  (define-key symbol-mode-map "a" 'sym-self-insert-command)
  (define-key symbol-mode-map "b" 'sym-self-insert-command)
  (define-key symbol-mode-map "c" 'sym-self-insert-command)
  (define-key symbol-mode-map "d" 'sym-self-insert-command)
  (define-key symbol-mode-map "e" 'sym-self-insert-command)
  (define-key symbol-mode-map "f" 'sym-self-insert-command)
  (define-key symbol-mode-map "g" 'sym-self-insert-command)
  (define-key symbol-mode-map "h" 'sym-self-insert-command)
  (define-key symbol-mode-map "i" 'sym-self-insert-command)
  (define-key symbol-mode-map "j" 'sym-self-insert-command)
  (define-key symbol-mode-map "k" 'sym-self-insert-command)
  (define-key symbol-mode-map "l" 'sym-self-insert-command)
  (define-key symbol-mode-map "m" 'sym-self-insert-command)
  (define-key symbol-mode-map "n" 'sym-self-insert-command)
  (define-key symbol-mode-map "o" 'sym-self-insert-command)
  (define-key symbol-mode-map "p" 'sym-self-insert-command)
  (define-key symbol-mode-map "q" 'sym-self-insert-command)
  (define-key symbol-mode-map "r" 'sym-self-insert-command)
  (define-key symbol-mode-map "s" 'sym-self-insert-command)
  (define-key symbol-mode-map "t" 'sym-self-insert-command)
  (define-key symbol-mode-map "u" 'sym-self-insert-command)
  (define-key symbol-mode-map "v" 'sym-self-insert-command)
  (define-key symbol-mode-map "w" 'sym-self-insert-command)
  (define-key symbol-mode-map "x" 'sym-self-insert-command)
  (define-key symbol-mode-map "y" 'sym-self-insert-command)
  (define-key symbol-mode-map "z" 'sym-self-insert-command)

  (define-key symbol-mode-map "{" 'sym-self-insert-command)
  (define-key symbol-mode-map "|" 'sym-self-insert-command)
  (define-key symbol-mode-map "}" 'sym-self-insert-command)
  (define-key symbol-mode-map "~" 'sym-self-insert-command)

  ) ; let
) ; defun sym-set-local-keys

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defun sym-transpose-chars ()
  "Interchange characters arond point, moving forward one character.
    If at end of symbol line, the previous two characters are exchanged."
  (interactive)
					; Local Variables
  (let (save-position)
					; Body
    (sym-reposition-point)
    (if (> (point) sym-start)
      (if (< (point) (marker-position sym-end-marker))
	(transpose-chars 1)
      ; else
	(if (> (point) (1+ sym-start))
	  (progn
					; transpose does not
					;   insert-before-markers
	    (setq save-position (marker-position sym-end-marker))
	    (backward-char 1)
	    (transpose-chars 1)
	    (set-marker sym-end-marker save-position)
	  ) ; progn
	; else
	  (ding)
	) ; if
      ) ; if
    ; else
      (ding)
    ) ; if
  ) ; let
) ; defun sym-transpose-chars

;;;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;;; end of symbol.el

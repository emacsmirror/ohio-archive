;;; File:  edt.el, v 1.9
;;;
;;;            -----   -----------   -------   -----------------
;;;            E D T   K e y p a d   M o d e   E m u l a t i o n
;;;            -----   -----------   -------   -----------------
;;;
;;;                        BETA Distribution Version 0.6
;;;
;;; Copyright (C) 1986 Free Software Foundation, Inc.
;;; Copyright (C) 1991 Kevin Gallagher
;;;
;;; GNU Emacs is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY.  No author or distributor accepts
;;; RESPONSIBILITY TO anyone for the consequences of using it or for
;;; whether it serves any particular purpose or works at all, unless 
;;; he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.
;;;
;;;  See edt-install.doc for setup information.
;;;
;;;  Send bug reports and suggestions for improvement to Kevin Gallagher
;;;  (kgallagh@digi.lonestar.org).
;;;
;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the GNU
;;; Emacs General Public License.  A copy of this license is supposed
;;; to have been given to you along with GNU Emacs so you can know
;;; your rights and responsibilities.  It should be in a file named
;;; COPYING.  Among other things, the Copyright notice and this notice
;;; must be preserved on all copies.
;;;
;;; ====================================================================

;;;  Electric Help functions are used for keypad help displays.  A few
;;;  picture funtions are used in rectangular cut and paste commands.
(require 'ehelp)
(require 'picture)

;;;;
;;;; VARIABLES
;;;;


;;;
;;; Define storage locations for last line(s), word(s), and
;;; character(s) deleted, key maps, current direction string, flag
;;; indicating if default EDT keymap is active or if a user supplied
;;; custom keymap is active, and a few other miscellaneous variables.
;;;

(defvar edt-last-deleted-lines ""
  "Last text deleted by an EDT emulation line delete command.")

(defvar edt-last-deleted-words ""
  "Last text deleted by an EDT emulation word delete command.")

(defvar edt-last-deleted-chars ""
  "Last text deleted by an EDT emulation character delete command.")

(defvar edt-last-replaced-key-definition ""
  "Key definition replaced with edt-define-key or edt-learn command.")

(defvar edt-direction-string ""
  "Current direction of movement.")

(defvar edt-select-active nil
  "Text selection active.")

(defconst edt-select-string " Select"
  "Text select string indicating text selection is active.")

(defconst edt-forward-string " ADVANCE"
  "Direction string indicating forward movement.")

(defconst edt-backward-string "  BACKUP"
  "Direction string indicating backward movement.")

(defvar edt-default-map-active nil
  "Indicates, when true, that default EDT emulation key bindings are active;
user-defined custom bindings are active when set to nil.")

(defvar edt-user-map-configured nil
  "Indicates, when true, that user custom EDT emulation key bindings are
configured and available for use.")

(defvar edt-keep-current-page-delimiter nil
  "If set to true, when edt-emulation-on is first invoked,
modification of the page-delimiter varible to "\f" is suppressed,
thereby retaining current Emacs setting.") 

(defvar edt-use-emacs-control-sequence-bindings nil
  "If set to true, when edt-emulation-on is invoked, no EDT control sequence
bindings are made; all current Emacs control sequence bindings are retained.
However, the EDT function key and keypad key bindings are made as usual.")

(defvar edt-word-entities '(?\t)
  "*Specifies the list of word entity characters.")


;;;;
;;;; EDT Emulation Commands
;;;;

;;; Almost all of EDT's keypad mode commands have equivalent
;;; counterparts in Emacs.  Some behave the same way in Emacs as they
;;; do in EDT, but most do not.
;;;
;;; The following Emacs functions attempt to emulate, where practical,
;;; the exact behavior of the corresponding EDT keypad mode commands.
;;; In a few cases, the emulation is not exact, but we hope you will
;;; agree it is close enough.  In a very few cases, we chose to use
;;; the superior Emacs way of handling things.  For example, we do not
;;; emulate the EDT SUBS command.  Instead, we chose to use the
;;; superior Emacs query-replace function.  
;;;

;;;
;;; PAGE
;;;
;;; Emacs uses the regexp assigned to page-delimiter to determine what
;;; marks a page break.  This is normally "^\f", which causes the
;;; edt-page command to ignore form feeds not located at the beginning
;;; of a line.  To emulate the EDT PAGE command exactly,
;;; page-delimiter is set to "\f" when EDT emulation is turned on, and
;;; restored to "^\f" when EDT emulation is turned off.  But this can
;;; be overridden if the EDT definition is not desired by placing
;;;
;;;         (setq edt-keep-current-page-delimiter t)
;;;
;;; in your .emacs file.

(defun edt-page-forward (num)
  "Move forward to just after next page delimiter.
Accepts a positive prefix argument for the number of page delimiters to move."
  (interactive "p")
  (edt-check-prefix num)
  (if (eobp)
      (error "End of buffer")
      (progn
        (forward-page num)
	(if (eobp)
	    (edt-line-to-bottom-of-window)
	    (edt-line-to-top-of-window)))))

(defun edt-page-backward (num)
  "Move backward to just after previous page delimiter.
Accepts a positive prefix argument for the number of page delimiters to move."
  (interactive "p")
  (edt-check-prefix num)
  (if (bobp)
      (error "Beginning of buffer")
      (progn
        (backward-page num)
        (edt-line-to-top-of-window))))

(defun edt-page (num)
  "Move in current direction to next page delimiter.
Accepts a positive prefix argument for the number of page delimiters to move."
  (interactive "p")
  (if (equal edt-direction-string edt-forward-string)
      (edt-page-forward num)
      (edt-page-backward num)))

;;;
;;; SECT
;;;
;;; EDT defaults a section size to be 16 lines of its one and only
;;; 24-line window.  That's two-thirds of the window at a time.  The
;;; EDT SECT commands moves the cursor, not the window.  
;;; 
;;; This emulation of EDT's SECT moves the cursor approximately two-thirds
;;; of the current window at a time.

(defun edt-sect-forward (num)
  "Move cursor forward two-thirds of a window.
Accepts a positive prefix argument for the number of sections to move."
  (interactive "p")
  (edt-check-prefix num)
  (edt-line-forward (* (* (/ (- (window-height) 1) 3) 2) num)))

(defun edt-sect-backward (num)
  "Move cursor backward two-thirds of a window.
Accepts a positive prefix argument for the number of sections to move."
  (interactive "p")
  (edt-check-prefix num)
  (edt-line-backward (* (* (/ (- (window-height) 1) 3) 2) num)))

(defun edt-sect (num)
  "Move in current direction a full window.
Accepts a positive prefix argument for the number windows to move."
  (interactive "p")
  (if (equal edt-direction-string edt-forward-string)
      (edt-sect-forward num)
      (edt-sect-backward num)))

;;;
;;; BEGINNING OF LINE
;;;
;;; EDT's beginning-of-line command is not affected by current
;;; direction, for some unknown reason.

(defun edt-beginning-of-line (num)
  "Move backward to next beginning of line mark.
Accepts a positive prefix argument for the number of BOL marks to move."
  (interactive "p")
  (edt-check-prefix num)
  (if (bolp)
      (forward-line (* -1 num))
      (progn
        (setq num (1- num))
        (forward-line (* -1 num)))))

;;;
;;; EOL (End of Line)
;;;

(defun edt-end-of-line-forward (num)
  "Move forward to next end of line mark.
Accepts a positive prefix argument for the number of EOL marks to move."
  (interactive "p")
  (edt-check-prefix num)
  (forward-char)
  (end-of-line num))

(defun edt-end-of-line-backward (num)
  "Move backward to next end of line mark.
Accepts a positive prefix argument for the number of EOL marks to move."
  (interactive "p")
  (edt-check-prefix num)
  (end-of-line (1- num)))

(defun edt-end-of-line (num)
  "Move in current direction to next end of line mark.
Accepts a positive prefix argument for the number of EOL marks to move."
  (interactive "p")
  (if (equal edt-direction-string edt-forward-string)
      (edt-end-of-line-forward num)
      (edt-end-of-line-backward num)))

;;;
;;; WORD
;;;
;;; This one is a tad messy.  To emulate EDT's behavior everywhere in
;;; the file (beginning of file, end of file, beginning of line, end
;;; of line, etc.) it takes a bit of special handling.  
;;; 
;;; The variable edt-word-entities contains a list of characters which
;;; are to be viewed as distinct words where ever they appear in the
;;; buffer.  This emulates the EDT command SET ENTITY WORD. 


(defun edt-one-word-forward ()
  "Move forward to first character of next word."
  (interactive)
  (if (eobp)
      (error "End of buffer"))
  (if (eolp)
      (forward-char)
      (progn
        (if (memq (following-char) edt-word-entities)
            (forward-char)
            (while (and 
                     (not (eolp))
                     (not (eobp))
                     (not (eq ?\  (char-syntax (following-char))))
                     (not (memq (following-char) edt-word-entities)))
              (forward-char)))
        (while (and 
                 (not (eolp))
                 (not (eobp))
                 (eq ?\  (char-syntax (following-char)))
                 (not (memq (following-char) edt-word-entities)))
          (forward-char)))))

(defun edt-one-word-backward ()
  "Move backward to first character of previous word."
  (interactive)
  (if (bobp)
      (error "Beginning of buffer"))
  (if (bolp)
      (backward-char)
      (progn
        (backward-char)
        (while (and 
                 (not (bolp))
                 (not (bobp))
                 (eq ?\  (char-syntax (following-char)))
                 (not (memq (following-char) edt-word-entities)))
          (backward-char))
        (if (not (memq (following-char) edt-word-entities))
            (while (and 
                     (not (bolp))
                     (not (bobp))
                     (not (eq ?\  (char-syntax (preceding-char))))
                     (not (memq (preceding-char) edt-word-entities)))
              (backward-char))))))

(defun edt-word-forward (num)
  "Move forward to first character of next word.
Accepts a positive prefix argument for the number of words to move."
  (interactive "p")
  (edt-check-prefix num)
  (while (> num 0)
    (edt-one-word-forward)
    (setq num (1- num))))

(defun edt-word-backward (num)
  "Move backward to first character of previous word.
Accepts a positive prefix argument for the number of words to move."
  (interactive "p")
  (edt-check-prefix num)
  (while (> num 0)
    (edt-one-word-backward)
    (setq num (1- num))))

(defun edt-word (num)
  "Move in current direction to first character of next word.
Accepts a positive prefix argument for the number of words to move."
  (interactive "p")
  (if (equal edt-direction-string edt-forward-string)
      (edt-word-forward num)
      (edt-word-backward num)))

;;;
;;; CHAR
;;;

(defun edt-character (num)
  "Move in current direction to next character.
Accepts a positive prefix argument for the number of characters to move."
  (interactive "p")
  (edt-check-prefix num)
  (if (equal edt-direction-string edt-forward-string)
      (forward-char num)
      (backward-char num)))

;;;
;;; LINE
;;;
;;; When direction is set to BACKUP, LINE behaves just like BEGINNING
;;; OF LINE in EDT.  So edt-line-backward is not really needed as a
;;; separate function.

(defun edt-line-backward (num)
  "Move backward to next beginning of line mark.
Accepts a positive prefix argument for the number of BOL marks to move."
  (interactive "p")
  (edt-beginning-of-line num))

(defun edt-line-forward (num)
  "Move forward to next beginning of line mark.
Accepts a positive prefix argument for the number of BOL marks to move."
  (interactive "p")
  (edt-check-prefix num)
  (forward-line num))

(defun edt-line (num)
  "Move in current direction to next beginning of line mark.
Accepts a positive prefix argument for the number of BOL marks to move."
  (interactive "p")
  (if (equal edt-direction-string edt-forward-string)
      (edt-line-forward num)
      (edt-line-backward num)))

;;;
;;; TOP
;;;

(defun edt-top ()
  "Move cursor to the beginning of buffer."
  (interactive)
  (goto-char (point-min)))

;;;
;;; BOTTOM
;;;

(defun edt-bottom ()
  "Move cursor to the end of buffer."
  (interactive)
  (goto-char (point-max))
  (edt-line-to-bottom-of-window))

;;;
;;; FIND
;;;

(defun edt-find-forward (&optional find)
  "Find first occurance of a string in the forward direction and save the string."
  (interactive)
  (if (not find)
      (set 'search-last-string (read-string "Search forward: ")))
  (if (eq (search-forward search-last-string) t) 
      (search-backward search-last-string)))

(defun edt-find-backward (&optional find)
  "Find first occurance of a string in the backward direction and save the string."
  (interactive)
  (if (not find)
      (set 'search-last-string (read-string "Search backward: ")))
  (search-backward search-last-string))

(defun edt-find ()
  "Find first occurance of string in current direction and save the string."
  (interactive)
  (set 'search-last-string (read-string "Search: "))
  (if (equal edt-direction-string edt-forward-string)
      (edt-find-forward t)
      (edt-find-backward t)))
  
;;;
;;; FNDNXT
;;;

(defun edt-find-next-forward ()
  "Find next occurance of a string in forward direction."
  (interactive)
  (forward-char 1)
  (if (eq (search-forward search-last-string nil t) t)
      (search-backward search-last-string)
      (progn
        (backward-char 1)
        (error "Search failed: \"%s\"." search-last-string))))

(defun edt-find-next-backward ()
  "Find next occurance of a string in backward direction."
  (interactive)
  (if (eq (search-backward search-last-string nil t) nil)
      (progn
        (error "Search failed: \"%s\"." search-last-string))))

(defun edt-find-next ()
  "Find next occurance of a string in current direction."
  (interactive)
  (if (equal edt-direction-string edt-forward-string)
      (edt-find-next-forward)
      (edt-find-next-backward)))
  
;;;
;;; APPEND
;;;

(defun edt-append ()
  "Append this kill region to last killed region."
  (interactive "*")
  (edt-check-selection)
  (append-next-kill)
  (kill-region (mark) (point))
  (setq edt-select-active nil)
  (edt-update-mode-line)
  (message "Selected text APPENDED to kill ring"))

;;;
;;; DEL L
;;;

(defun edt-delete-line (num)
  "Delete from cursor up to and including the end of line mark.
Accepts a positive prefix argument for the number of lines to delete."
  (interactive "*p")
  (edt-check-prefix num)
  (let ((beg (point)))
    (forward-line num)
    (if (not (eq (preceding-char) ?\n))
        (insert "\n"))
    (setq edt-last-deleted-lines
          (buffer-substring beg (point)))
    (delete-region beg (point))))

;;;
;;; DEL EOL
;;;

(defun edt-delete-to-end-of-line (num)
  "Delete from cursor up to but excluding the end of line mark.
Accepts a positive prefix argument for the number of lines to delete."
  (interactive "*p")
  (edt-check-prefix num)
  (let ((beg (point)))
    (forward-char 1)
    (end-of-line num)
    (setq edt-last-deleted-lines
          (buffer-substring beg (point)))
    (delete-region beg (point))))

;;;
;;; SELECT
;;;

(defun edt-select ()
  "Set mark at cursor and start text selection."
  (interactive)
  (set-mark-command nil)
  (setq edt-select-active edt-select-string)
  (point-to-register 1)
  (set 'rect-start-point (window-point))
  (edt-update-mode-line))

(defun edt-reset ()
  "Cancel text selection."
  (interactive)
  (setq edt-select-active nil)
  (edt-update-mode-line)
  (message "Text selection cancelled"))

;;;
;;; CUT
;;;

(defun edt-cut ()
  "Deletes selected text but copies to kill ring."
  (interactive "*")
  (edt-check-selection)
  (kill-region (mark) (point))
  (setq edt-select-active nil)
  (edt-update-mode-line)
  (message "Selected text CUT to kill ring"))

;;;
;;; DELETE TO BEGINNING OF LINE
;;;

(defun edt-delete-to-beginning-of-line (num)
  "Delete from cursor to beginning of of line.
Accepts a positive prefix argument for the number of lines to delete."
  (interactive "*p")
  (edt-check-prefix num)
  (let ((beg (point)))
    (edt-beginning-of-line num)
    (setq edt-last-deleted-lines
          (buffer-substring (point) beg))
    (delete-region beg (point))))

;;;
;;; DEL W
;;;

(defun edt-delete-word (num)
  "Delete from cursor up to but excluding first character of next word.
Accepts a positive prefix argument for the number of words to delete."
  (interactive "*p")
  (edt-check-prefix num)
  (let ((beg (point)))
    (edt-word-forward num)
    (setq edt-last-deleted-words (buffer-substring beg (point)))
    (delete-region beg (point))))

;;;
;;; DELETE TO BEGINNING OF WORD
;;;

(defun edt-delete-to-beginning-of-word (num)
  "Delete from cursor to beginning of word.
Accepts a positive prefix argument for the number of words to delete."
  (interactive "*p")
  (edt-check-prefix num)
  (let ((beg (point)))
    (edt-word-backward num)
    (setq edt-last-deleted-words (buffer-substring (point) beg))
    (delete-region beg (point))))

;;;
;;; DEL C
;;;

(defun edt-delete-character (num)
  "Delete character under cursor.
Accepts a positive prefix argument for the number of characters to delete."
  (interactive "*p")
  (edt-check-prefix num)
  (setq edt-last-deleted-chars
        (buffer-substring (point) (min (point-max) (+ (point) num))))
  (delete-region (point) (min (point-max) (+ (point) num))))

;;;
;;; DELETE CHAR
;;;

(defun edt-delete-previous-character (num)
  "Delete character in front of cursor.
Accepts a positive prefix argument for the number of characters to delete."
  (interactive "*p")
  (edt-check-prefix num)
  (setq edt-last-deleted-chars
        (buffer-substring (max (point-min) (- (point) num)) (point)))
  (delete-region (max (point-min) (- (point) num)) (point)))

;;;
;;; UND L
;;;

(defun edt-undelete-line ()
  "Undelete previous deleted line(s)."
  (interactive "*")
  (point-to-register 1)
  (insert edt-last-deleted-lines)
  (register-to-point 1))

;;;
;;; UND W
;;;

(defun edt-undelete-word ()
  "Yank words deleted by last EDT word-deletion command."
  (interactive "*")
  (point-to-register 1)
  (insert edt-last-deleted-words)
  (register-to-point 1))

;;;
;;; UND C
;;;

(defun edt-undelete-character ()
  "Yank characters deleted by last EDT character-deletion command."
  (interactive "*")
  (point-to-register 1)
  (insert edt-last-deleted-chars)
  (register-to-point 1))

;;;
;;; REPLACE
;;;

(defun edt-replace ()
  "Replace marked section with last CUT (killed) text."
  (interactive "*")
  (exchange-point-and-mark)
  (let ((beg (point)))
    (exchange-point-and-mark)
    (delete-region beg (point)))
  (yank))

;;;
;;; ADVANCE
;;;

(defun edt-advance ()
  "Set movement direction forward."
  (interactive)
  (setq edt-direction-string edt-forward-string)
  (edt-update-mode-line)
  (if (string-equal " *Minibuf" 
                    (substring (buffer-name) 0 (min (length (buffer-name)) 9)))
      (exit-minibuffer)))
  
;;;
;;; BACKUP
;;;

(defun edt-backup ()
  "Set movement direction backward."
  (interactive)
  (setq edt-direction-string edt-backward-string)
  (edt-update-mode-line)
  (if (string-equal " *Minibuf" 
                    (substring (buffer-name) 0 (min (length (buffer-name)) 9)))
      (exit-minibuffer)))

;;;
;;; CHNGCASE
;;;
;; This function is based upon Jeff Kowalski's case-flip function in his 
;; tpu.el.

(defun edt-change-case (num)
  "Change the case of specified characters.
If text selection IS active, then characters between the cursor and mark are
changed.  If text selection is NOT active, there are two cases.  First, if the
current direction is ADVANCE, then the prefix number of character(s) under and
following cursor are changed.  Second, if the current direction is BACKUP, then
the prefix number of character(s) before the cursor are changed.  Accepts a
positive prefix for the number of characters to change, but the prefix is
ignored if text selection is active."
  (interactive "*p")
  (edt-check-prefix num)
  (if edt-select-active
      (let ((end (max (mark) (point)))
            (point-save (point)))
        (goto-char (min (point) (mark)))
        (while (not (eq (point) end))
          (funcall (if (<= ?a (following-char))
                       'upcase-region 'downcase-region)
                   (point) (1+ (point)))
          (forward-char 1))
        (goto-char point-save)
        (setq edt-select-active nil)
	(edt-update-mode-line))
      (progn
        (if (string= edt-direction-string edt-backward-string)
            (backward-char num))
        (while (> num 0)
          (funcall (if (<= ?a (following-char))
                       'upcase-region 'downcase-region)
                   (point) (1+ (point)))
          (forward-char 1)
          (setq num (1- num))))))

;;;
;;; DEFINE KEY
;;;

(defun edt-define-key ()
  "Assign an interactively-callable function to a specified key sequence.
The current key definition is saved in edt-last-replaced-key-definition.
Use edt-restore-key to restore last replaced key definition."
  (interactive)
  (let (edt-function
    edt-key-definition-string)
    (setq edt-key-definition-string
         (read-key-sequence "Press the key to be defined: "))
    (if (string-equal "\C-m" edt-key-definition-string) 
        (message "Key not defined") 
        (progn
          (setq edt-function (read-command "Enter command name: "))
          (if (string-equal "" edt-function)
              (message "Key not defined") 
              (progn
                (setq edt-last-replaced-key-definition
                   (lookup-key (current-global-map) edt-key-definition-string))
                (define-key (current-global-map) 
                    edt-key-definition-string edt-function)))))))

;;;
;;; FORM FEED INSERT
;;;

(defun edt-form-feed-insert (num)
  "Insert form feed character at cursor position.
Accepts a positive prefix argument for the number of form feeds to insert."
  (interactive "*p")
  (edt-check-prefix num)
  (while (> num 0)
    (insert ?\f)
    (setq num (1- num))))

;;;
;;; TAB INSERT
;;;

(defun edt-tab-insert (num)
  "Insert tab character at cursor position.
Accepts a positive prefix argument for the number of tabs to insert."
  (interactive "*p")
  (edt-check-prefix num)
  (while (> num 0)
    (insert ?\t)
    (setq num (1- num))))

;;;
;;; Check Prefix
;;;

(defun edt-check-prefix (num)
  "Indicate error if prefix is not positive."
  (if (<= num 0)
      (error "Prefix must be positive")))
      
;;;
;;; Check Selection
;;;

(defun edt-check-selection ()
  "Indicate error if EDT selection is not active."
  (if (not edt-select-active)
      (error "Selection NOT active")))

;;;;
;;;; ENHANCEMENTS AND ADDITIONS FOR EDT KEYPAD MODE
;;;;

;;; 
;;; Several enhancements and additions to EDT keypad mode commands are
;;; provided here.  Some of these have been motivated by similar
;;; TPU/EVE and EVE-Plus commands.  Others are new.

;;;
;;; CHANGE DIRECTION
;;;

(defun edt-change-direction ()
  "Toggle movement direction."
  (interactive)
  (if (equal edt-direction-string edt-forward-string) 
      (edt-backup)
      (edt-advance)))

;;;
;;; TOGGLE SELECT
;;;

(defun edt-toggle-select ()
  "Toggle to start (or cancel) text selection."
  (interactive)
  (if edt-select-active
      (edt-reset)
      (edt-select)))

;;;
;;; SENTENCE
;;;

(defun edt-sentence-forward (num)
  "Move forward to start of next sentence.
Accepts a positive prefix argument for the number of sentences to move."
  (interactive "p")
  (edt-check-prefix num)
  (if (eobp)
      (progn
        (error "End of buffer"))
      (progn
        (forward-sentence num)
        (edt-one-word-forward))))

(defun edt-sentence-backward (num)
  "Move backward to next sentence beginning.
Accepts a positive prefix argument for the number of sentences to move."
  (interactive "p")
  (edt-check-prefix num)
  (if (eobp)
      (progn
        (error "End of buffer"))
      (backward-sentence num)))

(defun edt-sentence (num)
  "Move in current direction to next sentence.
Accepts a positive prefix argument for the number of sentences to move."
  (interactive "p")
  (if (equal edt-direction-string edt-forward-string)
      (edt-sentence-forward num)
      (edt-sentence-backward num)))

;;;
;;; PARAGRAPH
;;;

(defun edt-paragraph-forward (num)
  "Move forward to beginning of paragraph.
Accepts a positive prefix argument for the number of paragraphs to move."
  (interactive "p")
  (edt-check-prefix num)
  (while (> num 0)
    (next-line 1)
    (forward-paragraph)
    (previous-line 1)
    (if (eolp)
        (next-line 1))
    (setq num (1- num))))

(defun edt-paragraph-backward (num)
  "Move backward to beginning of paragraph.
Accepts a positive prefix argument for the number of paragraphs to move."
  (interactive "p")
  (edt-check-prefix num)
  (while (> num 0)
    (backward-paragraph)
    (previous-line 1)
    (if (eolp) (next-line 1))
    (setq num (1- num))))

(defun edt-paragraph (num)
  "Move in current direction to next paragraph.
Accepts a positive prefix argument for the number of paragraph to move."
  (interactive "p")
  (if (equal edt-direction-string edt-forward-string)
      (edt-paragraph-forward num)
      (edt-paragraph-backward num)))

;;;
;;; RESTORE KEY
;;;

(defun edt-restore-key ()
  "Restore last replaced key definition, which is stored in 
edt-last-replaced-key-definition."
  (interactive)
  (if edt-last-replaced-key-definition
      (progn
        (let (edt-key-definition-string)
          (set 'edt-key-definition-string
               (read-key-sequence "Press the key to be restored: "))
          (if (string-equal "\C-m" edt-key-definition-string) 
              (message "Key not restored") 
              (define-key (current-global-map) 
                 edt-key-definition-string edt-last-replaced-key-definition))))
      (error "No replaced key definition to restore!")))

;;;
;;; WINDOW TOP
;;;

(defun edt-window-top ()
  "Move the cursor to the top of the window."
  (interactive)
  (let ((start-column (current-column)))
    (move-to-window-line 0)
    (move-to-column start-column)))

;;;
;;; WINDOW BOTTOM
;;;

(defun edt-window-bottom ()
  "Move the cursor to the bottom of the window."
  (interactive)
  (let ((start-column (current-column)))
    (move-to-window-line (- (window-height) 2))
    (move-to-column start-column)))

;;;
;;; SCROLL WINDOW LINE
;;;

(defun edt-scroll-window-forward-line ()
  "Move window forward one line leaving cursor at relative position in window."
  (interactive)
  (scroll-up 1))

(defun edt-scroll-window-backward-line ()
  "Move window backward one line leaving cursor at relative position in window."
  (interactive)
  (scroll-down 1))

(defun edt-scroll-line ()
  "Move window one line in current direction."
  (interactive)
  (if (equal edt-direction-string edt-forward-string)
      (edt-scroll-window-forward-line)
      (edt-scroll-window-backward-line)))

;;;
;;; SCROLL WINDOW
;;;
;;; Scroll a window (less one line) at a time.  Leave cursor in center of 
;;; window. 

(defun edt-scroll-window-forward (num)
  "Scroll forward one window in buffer, less one line.
Accepts a positive prefix argument for the number of windows to move."
  (interactive "p")
  (edt-check-prefix num)
  (scroll-up (- (* (window-height) num) 2))
  (edt-line-forward (/ (- (window-height) 1) 2)))

(defun edt-scroll-window-backward (num)
  "Scroll backward one window in buffer, less one line.
Accepts a positive prefix argument for the number of windows to move."
  (interactive "p")
  (edt-check-prefix num)
  (scroll-down (- (* (window-height) num) 2))
  (edt-line-backward (/ (- (window-height) 1) 2)))

(defun edt-scroll-window (num)
  "Scroll one window in buffer, less one line, in current direction.
Accepts a positive prefix argument for the number windows to move."
  (interactive "p")
  (if (equal edt-direction-string edt-forward-string)
      (edt-scroll-window-forward num)
      (edt-scroll-window-backward num)))

;;;
;;; LINE TO BOTTOM OF WINDOW
;;;

(defun edt-line-to-bottom-of-window ()
  "Move the current line to the top of the window."
  (interactive)
  (recenter -1))

;;;
;;; LINE TO TOP OF WINDOW
;;;

(defun edt-line-to-top-of-window ()
  "Move the current line to the top of the window."
  (interactive)
  (recenter 0))

;;;
;;; LINE TO MIDDLE OF WINDOW
;;;

(defun edt-line-to-middle-of-window ()
  "Move window so line with cursor is in the middle of the window."
  (interactive)
  (recenter '(4)))

;;;
;;; GOTO PERCENTAGE
;;;

(defun edt-goto-percentage (num)
  "Move to specified percentage in buffer from top of buffer."
  (interactive "NGoto-percentage: ")
  (if (or (> num 100) (< num 0))
      (error "Percentage %d out of range 0 < percent < 100" num)
      (goto-char (/ (* (point-max) num) 100))))

;;;
;;; FILL REGION
;;;

(defun edt-fill-region ()
  "Fill selected text."
  (interactive)
  (edt-check-selection)
  (fill-region (point) (mark))
  (setq edt-select-active nil)
  (edt-update-mode-line))

;;;
;;; INDENT OR FILL REGION
;;;

(defun edt-indent-or-fill-region ()
  "Fill region in text modes, indent region in programming language modes."
  (interactive)
  (if (string= paragraph-start "^$\\|^\f")
      (indent-region (point) (mark) nil)
      (fill-region (point) (mark)))
  (setq edt-select-active nil)
  (edt-update-mode-line))

;;;
;;; MARK SECTION WISELY
;;;

(defun edt-mark-section-wisely ()
  "Mark the section in a manner consistent with the major-mode.
Uses mark-defun for emacs-lisp and lisp,
mark-c-function for C,
mark-fortran-subsystem for fortran,
and mark-paragraph for other modes."
  (interactive)
  (if edt-select-active
      (progn
        (setq edt-select-active nil)
	(edt-update-mode-line)
        (message "Text selection cancelled"))
      (progn
	(cond  ((or (eq major-mode 'emacs-lisp-mode)
		    (eq major-mode 'lisp-mode))
		(mark-defun)
		(message "Lisp defun selected"))
	       ((eq major-mode 'c-mode)
		(mark-c-function)
		(message "C function selected"))
	       ((eq major-mode 'fortran-mode)
		(mark-fortran-subprogram)
		(message "Fortran subprogram selected"))
	       (t (mark-paragraph)
		  (message "Paragraph selected")))
	(setq edt-select-active edt-select-string)
	(edt-update-mode-line))))

;;;
;;; COPY
;;;

(defun edt-copy ()
  "Copy selected region to kill ring, but don't delete it!"
  (interactive)
  (edt-check-selection)
  (copy-region-as-kill (mark) (point))
  (setq edt-select-active nil)
  (edt-update-mode-line)
  (message "Selected text COPIED to kill ring"))

;;;
;;; CUT or COPY
;;;

(defun edt-cut-or-copy ()
  "Cuts (or copies) selected text to kill ring.
Cuts selected text if buffer-read-only is nil.
Copies selected text if buffer-read-only is t."
  (interactive)
  (if buffer-read-only
      (edt-copy)
      (edt-cut)))

;;;
;;; DELETE ENTIRE LINE
;;;

(defun edt-delete-entire-line ()
  "Delete entire line regardless of cursor position in the line."
  (interactive "*")
  (beginning-of-line)
  (edt-delete-line 1))

;;;
;;; DUPLICATE LINE
;;;

(defun edt-duplicate-line (num)
  "Duplicate a line of text.
Accepts a positive prefix argument for the number times to duplicate the line."
  (interactive "*p")
  (edt-check-prefix num)
  (let ((old-column (current-column))
        (count num))
    (edt-delete-entire-line)
    (edt-undelete-line)
    (while (> count 0)
      (edt-undelete-line)
      (setq count (1- count)))
    (edt-line-forward num)
    (move-to-column old-column)))

;;;
;;; DUPLICATE WORD
;;;

(defun edt-duplicate-word()
  "Duplicate word (or rest of word) found directly above cursor, if any."
  (interactive "*")
  (let ((start (point))
        (start-column (current-column)))
    (forward-line -1)
    (move-to-column start-column)
    (if (and (not (equal start (point)))
             (not (eolp)))
        (progn
          (if (and (equal ?\t (preceding-char))
                   (< start-column (current-column)))
              (backward-char))
          (let ((beg (point)))
            (edt-one-word-forward)
            (setq edt-last-copied-word (buffer-substring beg (point))))
          (forward-line)
          (move-to-column start-column)
          (insert edt-last-copied-word))
        (progn
	  (if (not (equal start (point)))
	      (forward-line))
          (move-to-column start-column)
          (error "Nothing to duplicate!")))))

;;;
;;; KEY NOT ASSIGNED
;;;

(defun edt-key-not-assigned ()
  "Displays message that key has not been assigned to a function."
  (interactive)
  (error "Key not assigned"))

;;;
;;; TOGGLE CAPITALIZATION OF WORD
;;;

(defun edt-toggle-capitalization-of-word ()
  "Toggle the capitalization of the current word"
  (interactive "*")
  (edt-one-word-forward)
  (edt-one-word-backward)
  (edt-change-case 1))

;;;
;;; ELIMINATE ALL TABS
;;;

(defun edt-eliminate-all-tabs ()
  "Convert all tabs to spaces in the entire buffer."
  (interactive "*")
  (untabify (point-min) (point-max))
  (message "TABS converted to SPACES"))

;;;
;;; DISPLAY THE TIME
;;;

(defun edt-display-the-time ()
  "Display the current time."
  (interactive)
  (set 'time-string (current-time-string))
  (message time-string))

;;;
;;; LEARN
;;;

(defun edt-learn ()
  "Learn a sequence of key strokes to bind to a key."
  (interactive)
  (if (eq defining-kbd-macro t)
      (edt-remember)
      (start-kbd-macro nil)))

;;;
;;; REMEMBER
;;;

(defun edt-remember ()
  "Store the sequence of key strokes started by edt-learn to a key."
  (interactive)
  (if (eq defining-kbd-macro nil)
      (error "Nothing to remember!")
      (progn
        (end-kbd-macro nil)
        (let (edt-key-definition-string)
          (set 'edt-key-definition-string
               (read-key-sequence "Enter key for binding: "))
          (if (string-equal "\C-m" edt-key-definition-string) 
              (message "Key sequence not remembered") 
              (progn
                (set 'edt-learn-macro-count (+ edt-learn-macro-count 1))
                (setq edt-last-replaced-key-definition
                      (lookup-key (current-global-map)
                                  edt-key-definition-string))
                (define-key (current-global-map) edt-key-definition-string
                  (name-last-kbd-macro
                    (intern (concat "last-learned-sequence-"
                                  (int-to-string edt-learn-macro-count)))))))))))

;;;
;;; EXIT
;;;

(defun edt-exit ()
  "Save current buffer, ask to save other buffers, and then exit Emacs."
  (interactive)
  (save-buffer)
  (save-buffers-kill-emacs))

;;; 
;;; QUIT
;;;

(defun edt-quit ()
  "Quit Emacs without saving changes."
  (interactive)
  (kill-emacs))

;;; 
;;; SPLIT WINDOW
;;;

(defun edt-split-window ()
  "Split current window and place cursor in the new window."
  (interactive)
  (split-window)
  (other-window 1))

;;;
;;; UPDATE MODE LINE
;;;

(defun edt-update-mode-line ()
  "Make sure mode-line in the current buffer reflects all changes."
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))

;;;
;;; COPY RECTANGLE
;;;

(defun edt-copy-rectangle ()
  "Copy a rectangle of text between mark and cursor."
  (interactive)
  (edt-check-selection)
  (point-to-register 2)
  (edt-order-rectangle-points)
  (copy-rectangle-to-register 3 (register-to-point 1) (register-to-point 2) nil)
  (register-to-point 1)
  (setq edt-select-active nil)
  (edt-update-mode-line)
  (message "Selected rectangle COPIED to register"))

(defun edt-order-rectangle-points ()
  "Determine top of rectangle."
  (if (< (register-to-point 1) (register-to-point 2)) nil 
      (register-to-point 2)
      (point-to-register 3)
      (register-to-point 1)
      (point-to-register 2)
      (register-to-point 3)
      (point-to-register 1)))

;;;
;;; CUT RECTANGLE
;;;

(defun edt-cut-rectangle-overstrike-mode ()
  "Cut a rectangle of text between mark and cursor, replacing 
characters with spaces and moving cursor back to mark."
  (interactive "*")
  (edt-check-selection)
  (point-to-register 2)
  (edt-order-rectangle-points)                     
  (picture-clear-rectangle-to-register
    (register-to-point 1)
    (register-to-point 2) 3)
  (set-window-point (get-buffer-window (window-buffer)) rect-start-point)
  (point-to-register 1)
  (setq edt-select-active nil)
  (edt-update-mode-line)
  (message "Selected rectangle CUT to register"))

(defun edt-cut-rectangle-insert-mode ()
  "Cut a rectangle of text between mark and cursor, deleting
intermediate text and moving cursor back to mark."
  (interactive "*")         
  (edt-check-selection)
  (point-to-register 2) 
  (edt-order-rectangle-points)                   
  (picture-clear-rectangle-to-register
    (register-to-point 1)
    (register-to-point 2) 3 t)
  (fixup-whitespace)
  (set-window-point (get-buffer-window (window-buffer)) rect-start-point)
  (point-to-register 1)
  (setq edt-select-active nil)
  (edt-update-mode-line)
  (message "Selected rectangle CUT to register"))

(defun edt-cut-rectangle ()
  "Cut or copy a rectangular region of text."
  (interactive "*")
  (if overwrite-mode
      (edt-cut-rectangle-overstrike-mode)
      (edt-cut-rectangle-insert-mode)))

;;;
;;; PASTE RECTANGLE
;;;

(defun edt-paste-rectangle-overstrike-mode ()
  "Paste previously deleted rectangular region, replacing text at cursor."
  (interactive "*")    
  (picture-yank-rectangle-from-register 3))

(defun edt-paste-rectangle-insert-mode ()
  "Paste previously deleted rectangular region, inserting text at cursor."
  (interactive "*")
  (picture-yank-rectangle-from-register 3 t))

(defun edt-paste-rectangle ()
  "Paste a rectangular region of text."
  (interactive)
  (if overwrite-mode
      (edt-paste-rectangle-overstrike-mode)
      (edt-paste-rectangle-insert-mode)))

;;;
;;; DOWNCASE REGION
;;;

(defun edt-lowercase ()
  "Change specified characters to lower case.
If text selection IS active, then characters between the cursor and
mark are changed.  If text selection is NOT active, there are two
situations.  If the current direction is ADVANCE, then the word under
the cursor is changed to lower case and the cursor is moved to rest at
the beginning of the next word.  If the current direction is BACKUP,
the word prior to the word under the cursor is changed to lower case
and the cursor is left to rest at the beginning of that word."
  (interactive "*")
  (if edt-select-active
      (progn
	(downcase-region (mark) (point))
	(setq edt-select-active nil)
	(edt-update-mode-line))
      (progn
	;; Move to beginning of current word.
	(if (and
	      (not (bobp))
	      (not (eobp))
	      (not (bolp))
	      (not (eolp))
	      (not (eq ?\  (char-syntax (preceding-char))))
	      (not (memq (preceding-char) edt-word-entities))
	      (not (memq (following-char) edt-word-entities)))
	    (edt-one-word-backward))
	(if (equal edt-direction-string edt-backward-string)
	    (edt-one-word-backward))
	(let ((beg (point)))
	  (edt-one-word-forward)
	  (downcase-region beg (point)))
	(if (equal edt-direction-string edt-backward-string)
	    (edt-one-word-backward)))))

;;;
;;; UPCASE REGION
;;;

(defun edt-uppercase ()
  "Change specified characters to upper case.
If text selection IS active, then characters between the cursor and
mark are changed.  If text selection is NOT active, there are two
situations.  If the current direction is ADVANCE, then the word under
the cursor is changed to upper case and the cursor is moved to rest at
the beginning of the next word.  If the current direction is BACKUP,
the word prior to the word under the cursor is changed to upper case
and the cursor is left to rest at the beginning of that word."
  (interactive "*")
  (if edt-select-active
      (progn
	(upcase-region (mark) (point))
	(setq edt-select-active nil)
	(edt-update-mode-line))
      (progn
	;; Move to beginning of current word.
	(if (and
	      (not (bobp))
	      (not (eobp))
	      (not (bolp))
	      (not (eolp))
	      (not (eq ?\  (char-syntax (preceding-char))))
	      (not (memq (preceding-char) edt-word-entities))
	      (not (memq (following-char) edt-word-entities)))
	    (edt-one-word-backward))
	(if (equal edt-direction-string edt-backward-string)
	    (edt-one-word-backward))
	(let ((beg (point)))
	  (edt-one-word-forward)
	  (upcase-region beg (point)))
	(if (equal edt-direction-string edt-backward-string)
	    (edt-one-word-backward)))))


;;;
;;; INITIALIZATION COMMANDS.
;;;

(defun edt-emulation-on ()
  "Turn on EDT Emulation."
  (interactive)
  (setq edt-term (getenv "TERM"))
  ;; Load EDT terminal specific configuration file.
  (let ((term edt-term)
        hyphend)
    (while (and term
                (not (load (concat "edt-" term) t t)))
      ;; Strip off last hyphen and what follows, then try again
      (if (setq hyphend (string-match "[-_][^-_]+$" term))
          (setq term (substring term 0 hyphend))
          (setq term nil)))
    (if (null term)
        (error "Unable to load EDT terminal specific file for %s" edt-term)
        (setq edt-term term)))
  (if (load "edt-user" t t)
      (edt-user-emulation-setup)
      (edt-default-emulation-setup)))

(defun edt-emulation-off()
  "Select original global key bindings, disabling EDT Emulation."
  (interactive)
  (use-global-map global-map)
  (if (not edt-keep-current-page-delimiter)
      (setq page-delimiter edt-orig-page-delimiter))
  (setq edt-direction-string "")
  (edt-update-mode-line)
  (message "Original key bindings restored; EDT Emulation disabled"))

(defun edt-select-user-global-map()
  "Select user EDT emulation custom key bindings."
  (interactive)
  (if edt-user-map-configured
      (progn
        (use-global-map edt-user-global-map)
        (if (not edt-keep-current-page-delimiter)
            (progn
              (setq edt-orig-page-delimiter page-delimiter)
              (setq page-delimiter "\f")))
        (setq edt-default-map-active nil)
        (edt-advance)
        (setq edt-select-active nil)
        (message "User EDT custom keymap active"))
      (error "User EDT custom keymap NOT configured!")))

(defun edt-select-default-global-map()
  "Select default EDT emulation key bindings."
  (interactive)
  (use-global-map edt-default-global-map)
  (if (not edt-keep-current-page-delimiter)
      (progn
        (setq edt-orig-page-delimiter page-delimiter)
        (setq page-delimiter "\f")))
  (setq edt-default-map-active t)
  (edt-advance)
  (setq edt-select-active nil)
  (message "Default EDT keymap active"))

(defun edt-switch-global-maps ()
  "Toggle between default EDT keymap and user EDT keymap."
  (interactive)
  (if edt-default-map-active 
    (edt-select-user-global-map)
    (edt-select-default-global-map)))

;; There are three key binding functions needed: one for standard keys
;; (used to bind control keys, primarily), one for Gold sequences of
;; standard keys, and one for function keys.  Since functions keys are
;; not standard across keyboards, the command to bind function keys is
;; terminal specific and is located in a terminal specific file.  For
;; example, the VT100 terminal specific file is called "edt-vt100.el".

(defun edt-bind-gold-key (key gold-binding &optional default)
  "Bind commands to a gold key sequence in the default EDT keymap."
  (if default
      (define-key 'edt-default-gold-map key gold-binding)
      (define-key 'edt-user-gold-map key gold-binding)))

(defun edt-bind-standard-key (key gold-binding &optional default)
  "Bind commands to a gold key sequence in the default EDT keymap."
  (if default
      (define-key edt-default-global-map key gold-binding)
      (define-key edt-user-global-map key gold-binding)))


(defun edt-setup-default-bindings ()
  "Assigns default EDT Emulation keyboard bindings."

  ;; Function Key Bindings:  Regular and GOLD.

  ;; VT100/VT200/VT300 PF1 (GOLD), PF2, PF3, PF4 Keys
  (edt-bind-function-key "PF1" 'edt-default-gold-map 'edt-mark-section-wisely t)
  (edt-bind-function-key "PF2" 'edt-electric-keypad-help 'describe-function t)
  (edt-bind-function-key "PF3" 'edt-find-next 'edt-find t)
  (edt-bind-function-key "PF4" 'edt-delete-line 'edt-undelete-line t)

  ;; VT100/VT200/VT300 Arrow Keys
  (edt-bind-function-key "UP" 'previous-line 'edt-window-top t)
  (edt-bind-function-key "DOWN" 'next-line 'edt-window-bottom t)
  (edt-bind-function-key "LEFT" 'backward-char 'edt-sentence-backward t)
  (edt-bind-function-key "RIGHT" 'forward-char 'edt-sentence-forward t)

  ;; VT100/VT200/VT300 Keypad Keys
  (edt-bind-function-key "KP0" 'edt-line 'open-line t)
  (edt-bind-function-key "KP1" 'edt-word 'edt-change-case t)
  (edt-bind-function-key "KP2" 'edt-end-of-line 'edt-delete-to-end-of-line t)
  (edt-bind-function-key "KP3" 'edt-character 'quoted-insert t)
  (edt-bind-function-key "KP4" 'edt-advance 'edt-bottom t)
  (edt-bind-function-key "KP5" 'edt-backup 'edt-top t)
  (edt-bind-function-key "KP6" 'edt-cut 'yank t)
  (edt-bind-function-key "KP7" 'edt-page 'execute-extended-command t)
  (edt-bind-function-key "KP8" 'edt-sect 'edt-fill-region t)
  (edt-bind-function-key "KP9" 'edt-append 'edt-replace t)
  (edt-bind-function-key "KP-" 'edt-delete-word 'edt-undelete-word t)
  (edt-bind-function-key "KP," 'edt-delete-character 'edt-undelete-character t)
  (edt-bind-function-key "KPP" 'edt-select 'edt-reset t)
  (edt-bind-function-key "KPE" 'other-window 'query-replace t)

  ;; VT200/VT300 Function Keys
  ;; (F1 through F5, on the VT220, are not programmable, so we skip
  ;; making default bindings to those keys.  
  (edt-bind-function-key "FIND" 'edt-find-next 'edt-find t)
  (edt-bind-function-key "INSERT" 'yank 'edt-key-not-assigned t)
  (edt-bind-function-key "REMOVE" 'edt-cut 'edt-copy t)
  (edt-bind-function-key "SELECT" 'edt-toggle-select 'edt-key-not-assigned t)
  (edt-bind-function-key "NEXT" 'edt-sect-forward 'edt-key-not-assigned t)
  (edt-bind-function-key "PREVIOUS" 'edt-sect-backward 'edt-key-not-assigned t)
  (edt-bind-function-key "F6" 'edt-key-not-assigned 'edt-key-not-assigned t)
  (edt-bind-function-key "F7" 'edt-copy-rectangle 'edt-key-not-assigned t)
  (edt-bind-function-key "F8" 
       'edt-cut-rectangle-overstrike-mode 'edt-paste-rectangle-overstrike-mode t)
  (edt-bind-function-key "F9" 
       'edt-cut-rectangle-insert-mode 'edt-paste-rectangle-insert-mode t)
  (edt-bind-function-key "F10" 'edt-cut-rectangle 'edt-paste-rectangle t)
  (edt-bind-function-key "F11" 
       'edt-default-ESC-prefix 'edt-key-not-assigned t) ;ESC
  (edt-bind-function-key "F12" 
       'edt-beginning-of-line 'delete-other-windows t) ;BS
  (edt-bind-function-key "F13" 
       'edt-delete-to-beginning-of-word 'edt-key-not-assigned t) ;LF
  (edt-bind-function-key "F14" 'edt-key-not-assigned 'edt-key-not-assigned t)
  (edt-bind-function-key "HELP" 'help-for-help 'edt-key-not-assigned t)
  (edt-bind-function-key "DO" 'execute-extended-command 'edt-key-not-assigned t)
  (edt-bind-function-key "F17" 'edt-key-not-assigned 'edt-key-not-assigned t)
  (edt-bind-function-key "F18" 'edt-key-not-assigned 'edt-key-not-assigned t)
  (edt-bind-function-key "F19" 'edt-key-not-assigned 'edt-key-not-assigned t)
  (edt-bind-function-key "F20" 'edt-key-not-assigned 'edt-key-not-assigned t)

  ;; Control key bindings:  Regular and GOLD
  (if (not edt-use-emacs-control-sequence-bindings)
      (progn
        (edt-bind-standard-key "\C-a" 'edt-key-not-assigned t)
        (edt-bind-standard-key "\C-b" 'edt-key-not-assigned t)
        ;; Leave binding of C-c as original prefix key.
        (edt-bind-standard-key "\C-d" 'edt-key-not-assigned t)
        (edt-bind-standard-key "\C-e" 'edt-key-not-assigned t)
        (edt-bind-standard-key "\C-f" 'edt-key-not-assigned t)
        (edt-bind-standard-key "\C-g" 'keyboard-quit t)
        (edt-bind-gold-key  "\C-g" 'keyboard-quit t); Just in case.
        (edt-bind-standard-key "\C-h" 'edt-beginning-of-line t)
        (edt-bind-gold-key  "\C-h" 'help-for-help t)
        (edt-bind-standard-key "\C-i" 'edt-tab-insert t)
        (edt-bind-standard-key "\C-j" 'edt-delete-to-beginning-of-word t)
        (edt-bind-standard-key "\C-k" 'edt-define-key t)
        (edt-bind-gold-key  "\C-k" 'edt-restore-key t)
        (edt-bind-standard-key "\C-l" 'edt-form-feed-insert t)
        ;; Leave binding of C-m to newline.
        (edt-bind-standard-key "\C-n" 'edt-set-screen-width-80 t)
        (edt-bind-standard-key "\C-o" 'edt-key-not-assigned t)
        (edt-bind-standard-key "\C-p" 'edt-key-not-assigned t)
        (edt-bind-standard-key "\C-q" 'edt-key-not-assigned t)
        ;; Leave binding of C-r to isearch-backward.
        ;; Leave binding of C-s to isearch-forward.
        (edt-bind-standard-key "\C-t" 'edt-display-the-time t)
        (edt-bind-standard-key "\C-u" 'edt-delete-to-beginning-of-line t)
        (edt-bind-standard-key "\C-v" 'redraw-display t)
        (edt-bind-standard-key "\C-w" 'edt-set-screen-width-132 t)
        ;; Leave binding of C-x as original prefix key.
        (edt-bind-standard-key "\C-y" 'edt-key-not-assigned t)
        (edt-bind-standard-key "\C-z" 'suspend-emacs t)
        (edt-bind-gold-key  "\C-\\" 'split-window-vertically t)
        )
      )

  ;; GOLD bindings for regular keys.
  (edt-bind-gold-key "a" 'edt-key-not-assigned t)
  (edt-bind-gold-key "A" 'edt-key-not-assigned t)
  (edt-bind-gold-key "b" 'buffer-menu t)
  (edt-bind-gold-key "B" 'buffer-menu t)
  (edt-bind-gold-key "c" 'compile t)
  (edt-bind-gold-key "C" 'compile t)
  (edt-bind-gold-key "d" 'delete-window t)
  (edt-bind-gold-key "D" 'delete-window t)
  (edt-bind-gold-key "e" 'edt-exit t)
  (edt-bind-gold-key "E" 'edt-exit t)
  (edt-bind-gold-key "f" 'find-file t)
  (edt-bind-gold-key "F" 'find-file t)
  (edt-bind-gold-key "g" 'find-file-other-window t)
  (edt-bind-gold-key "G" 'find-file-other-window t)
  (edt-bind-gold-key "h" 'edt-electric-keypad-help t)
  (edt-bind-gold-key "H" 'edt-electric-keypad-help t)
  (edt-bind-gold-key "i" 'insert-file t)
  (edt-bind-gold-key "I" 'insert-file t)
  (edt-bind-gold-key "j" 'edt-key-not-assigned t)
  (edt-bind-gold-key "J" 'edt-key-not-assigned t)
  (edt-bind-gold-key "k" 'edt-toggle-capitalization-of-word t)
  (edt-bind-gold-key "K" 'edt-toggle-capitalization-of-word t)
  (edt-bind-gold-key "l" 'edt-lowercase t)
  (edt-bind-gold-key "L" 'edt-lowercase t)
  (edt-bind-gold-key "m" 'save-some-buffers t)
  (edt-bind-gold-key "M" 'save-some-buffers t)
  (edt-bind-gold-key "n" 'next-error t)
  (edt-bind-gold-key "N" 'next-error t)
  (edt-bind-gold-key "o" 'switch-to-buffer-other-window t)
  (edt-bind-gold-key "O" 'switch-to-buffer-other-window t)
  (edt-bind-gold-key "p" 'edt-key-not-assigned t)
  (edt-bind-gold-key "P" 'edt-key-not-assigned t)
  (edt-bind-gold-key "q" 'edt-quit t)
  (edt-bind-gold-key "Q" 'edt-quit t)
  (edt-bind-gold-key "r" 'revert-file t)
  (edt-bind-gold-key "R" 'revert-file t)
  (edt-bind-gold-key "s" 'save-buffer t)
  (edt-bind-gold-key "S" 'save-buffer t)
  (edt-bind-gold-key "t" 'edt-key-not-assigned t)
  (edt-bind-gold-key "T" 'edt-key-not-assigned t)
  (edt-bind-gold-key "u" 'edt-uppercase t)
  (edt-bind-gold-key "U" 'edt-uppercase t)
  (edt-bind-gold-key "v" 'find-file-other-window t)
  (edt-bind-gold-key "V" 'find-file-other-window t)
  (edt-bind-gold-key "w" 'write-file t)
  (edt-bind-gold-key "W" 'write-file t)
  (edt-bind-gold-key "x" 'edt-key-not-assigned t)
  (edt-bind-gold-key "X" 'edt-key-not-assigned t)
  (edt-bind-gold-key "y" 'edt-emulation-off t)
  (edt-bind-gold-key "Y" 'edt-emulation-off t)
  (edt-bind-gold-key "z" 'edt-switch-global-maps t)
  (edt-bind-gold-key "Z" 'edt-switch-global-maps t)
  (edt-bind-gold-key "1" 'delete-other-windows t)
  (edt-bind-gold-key "!" 'edt-key-not-assigned t)
  (edt-bind-gold-key "2" 'edt-split-window t)
  (edt-bind-gold-key "@" 'edt-key-not-assigned t)
  (edt-bind-gold-key "3" 'edt-key-not-assigned t)
  (edt-bind-gold-key "#" 'edt-key-not-assigned t)
  (edt-bind-gold-key "4" 'edt-key-not-assigned t)
  (edt-bind-gold-key "$" 'edt-key-not-assigned t)
  (edt-bind-gold-key "5" 'edt-key-not-assigned t)
  (edt-bind-gold-key "%" 'edt-goto-percentage t)
  (edt-bind-gold-key "6" 'edt-key-not-assigned t)
  (edt-bind-gold-key "^" 'edt-key-not-assigned t)
  (edt-bind-gold-key "7" 'edt-key-not-assigned t)
  (edt-bind-gold-key "&" 'edt-key-not-assigned t)
  (edt-bind-gold-key "8" 'edt-key-not-assigned t)
  (edt-bind-gold-key "*" 'edt-key-not-assigned t)
  (edt-bind-gold-key "9" 'edt-key-not-assigned t)
  (edt-bind-gold-key "(" 'edt-key-not-assigned t)
  (edt-bind-gold-key "0" 'edt-key-not-assigned t)
  (edt-bind-gold-key ")" 'edt-key-not-assigned t)
  (edt-bind-gold-key " " 'undo t)
  (edt-bind-gold-key "," 'edt-key-not-assigned t)
  (edt-bind-gold-key "<" 'edt-key-not-assigned t)
  (edt-bind-gold-key "." 'edt-key-not-assigned t)
  (edt-bind-gold-key ">" 'edt-key-not-assigned t)
  (edt-bind-gold-key "/" 'edt-key-not-assigned t)
  (edt-bind-gold-key "?" 'edt-key-not-assigned t)
  (edt-bind-gold-key "\\" 'edt-key-not-assigned t)
  (edt-bind-gold-key "|" 'edt-key-not-assigned t)
  (edt-bind-gold-key ";" 'edt-key-not-assigned t)
  (edt-bind-gold-key ":" 'edt-key-not-assigned t)
  (edt-bind-gold-key "'" 'edt-key-not-assigned t)
  (edt-bind-gold-key "\"" 'edt-key-not-assigned t)
  (edt-bind-gold-key "-" 'edt-key-not-assigned t)
  (edt-bind-gold-key "_" 'edt-key-not-assigned t)
  (edt-bind-gold-key "=" 'goto-line t)
  (edt-bind-gold-key "+" 'edt-key-not-assigned t)
  (edt-bind-gold-key "[" 'edt-key-not-assigned t)
  (edt-bind-gold-key "{" 'edt-key-not-assigned t)
  (edt-bind-gold-key "]" 'edt-key-not-assigned t)
  (edt-bind-gold-key "}" 'edt-key-not-assigned t)
  (edt-bind-gold-key "`" 'what-line t)
  (edt-bind-gold-key "~" 'edt-key-not-assigned t)
)

;;;
;;; DEFAULT EDT KEYPAD HELP
;;;

;;;
;;; Upper case commands in the keypad diagram below indicate that the
;;; emulation should look and feel very much like EDT.  Lower case
;;; commands are enhancements and/or additions to the EDT keypad
;;; commands or are native Emacs commands.
;;;

(defun edt-keypad-help ()
  "
                              DEFAULT EDT Keypad Active

   F7: Copy Rectangle             +----------+----------+----------+----------+
   F8: Cut Rect Overstrike        |Prev Line |Next Line |Bkwd Char |Frwd Char |
 G-F8: Paste Rect Overstrike      |   (UP)   |  (DOWN)  |  (LEFT)  | (RIGHT)  |
   F9: Cut Rect Insert            |Window Top|Window Bot|Bkwd Sent |Frwd Sent |
 G-F9: Paste Rect Insert          +----------+----------+----------+----------+
  F11: ESC                       
  F12: Begining of Line           +----------+----------+----------+----------+
G-F12: Delete Other Windows       |   GOLD   |   HELP   |  FNDNXT  |  DEL L   |
  F13: Delete to Begin of Word    |   (PF1)  |   (PF2)  |   (PF3)  |  (PF4)   |
 HELP: Emacs Help                 |Mark Wisel|Desc Funct|   FIND   |  UND L   |
   DO: Execute extended command   +----------+----------+----------+----------+
                                  |   PAGE   |   SECT   |  APPEND  |  DEL W   |
  C-g: Keyboard Quit              |    (7)   |    (8)   |    (9)   |   (-)    |
G-C-g: Keyboard Quit              |Ex Ext Cmd|Fill Regio| REPLACE  |  UND W   |
  C-h: Beginning of Line          +----------+----------+----------+----------+
G-C-h: Emacs Help                 |  ADVANCE |  BACKUP  |   CUT    |  DEL C   |
  C-i: Tab Insert                 |    (4)   |    (5)   |    (6)   |   (,)    |
  C-j: Delete to Begin of Word    |   BOTTOM |    TOP   |   Yank   |  UND C   |
  C-k: Define Key                 +----------+----------+----------+----------+
G-C-k: Restore Key                |   WORD   |    EOL   |   CHAR   |   Next   |
  C-l: Form Feed Insert           |    (1)   |    (2)   |    (3)   |  Window  |
  C-n: Set Screen Width 80        | CHNGCASE |  DEL EOL |Quoted Ins|          !
  C-r: Isearch Backward           +---------------------+----------+  (ENTER) |
  C-s: Isearch Forward            |         LINE        |  SELECT  |          !
  C-t: Display the Time           |         (0)         |    (.)   |   Query  |
  C-u: Delete to Begin of Line    |      Open Line      |   RESET  |  Replace |
  C-v: Redraw Display             +---------------------+----------+----------+
  C-w: Set Screen Width 132       
  C-z: Suspend Emacs                    +----------+----------+----------+
G-C-\\: Split Window                     |  FNDNXT  |   Yank   |   CUT    |
                                        |  (FIND)) | (INSERT) | (REMOVE) |
  G-b: Buffer Menu                      |   FIND   |          |   COPY   |
  G-c: Compile                          +----------+----------+----------+
  G-d: Delete Window                    |SELECT/RES|SECT BACKW|SECT FORWA|
  G-e: Exit                             | (SELECT) |(PREVIOUS)|  (NEXT)  |
  G-f: Find File                        |          |          |          |
  G-g: Find File Other Window           +----------+----------+----------+
  G-h: Keypad Help             
  G-i: Insert File                
  G-k: Toggle Capitalization Word 
  G-l: Downcase Region             
  G-m: Save Some Buffers           
  G-n: Next Error                  
  G-o: Switch to Next Window
  G-q: Quit                                                           
  G-r: Revert File                                                    
  G-s: Save Buffer                                                    
  G-u: Upcase Region                                                  
  G-v: Find File Other Window                                        
  G-w: Write file                                                  
  G-y: EDT Emulation OFF          
  G-z: Switch to User EDT Key Bindings
  G-1: Delete Other Windows       
  G-2: Split Window               
  G-%: Go to Percentage           
  G- : Undo  (GOLD Spacebar)      
  G-=: Go to Line                 
  G-`: What line"

  (interactive)
  (describe-function 'edt-keypad-help))

(defun edt-electric-helpify (fun)
  (let ((name "*Help*"))
    (if (save-window-excursion
          (let* ((p (symbol-function 'print-help-return-message))
                 (b (get-buffer name))
                 (m (buffer-modified-p b)))
            (and b (not (get-buffer-window b))
                 (setq b nil))
            (unwind-protect
                (progn
                  (message "%s..." (capitalize (symbol-name fun)))
                  (and b
                       (save-excursion
                         (set-buffer b)
                         (set-buffer-modified-p t)))
                  (fset 'print-help-return-message 'ignore)
                  (call-interactively fun)
                  (and (get-buffer name)
                       (get-buffer-window (get-buffer name))
                       (or (not b)
                           (not (eq b (get-buffer name)))
                           (not (buffer-modified-p b)))))
              (fset 'print-help-return-message p)
              (and b (buffer-name b)
                   (save-excursion
                     (set-buffer b)
                     (set-buffer-modified-p m))))))
        (with-electric-help 'delete-other-windows name t))))

(defun edt-electric-keypad-help ()
  (interactive)
  (edt-electric-helpify 'edt-keypad-help))

(defun edt-electric-user-keypad-help ()
  (interactive)
  (edt-electric-helpify 'edt-user-keypad-help))

;;;
;;; Generic Emacs screen width commands.
;;;
;; If modification of terminal attributes is desired when invoking these
;; commands, then the corresponding terminal specific file will contain a 
;; re-definition of these commands.

(defun edt-set-screen-width-80 ()
  "Set screen width to 80 columns."
  (interactive)
  (set-screen-width 80)
  (message "%sScreen width 80"))

(defun edt-set-screen-width-132 ()
  "Set screen width to 132 columns."
  (interactive)
  (set-screen-width 132)
  (message "%sScreen width 132"))


;;;
;;; Setup EDT default emulation.
;;;

(defun edt-default-emulation-setup (&optional user-setup)
  "Setup emulation of DEC's EDT editor."
  ;; Set up terminal specific re-binding of default emacs bindings, if needed. 
  (if (fboundp 'edt-customize-default-emacs-bindings)
      (edt-customize-default-emacs-bindings))
  ;; Set up user customizations of default emacs bindings, if needed. 
  (if (fboundp 'edt-user-customize-default-emacs-bindings)
      (edt-user-customize-default-emacs-bindings))
  ;; Setup default EDT global map by copying global map bindings.
  ;; This preserves ESC and C-x prefix bindings and other bindings we
  ;; wish to retain in EDT emulation mode keymaps.  However,
  ;; copy-keymap does not behave as documented in the GNU Emacs Lisp
  ;; Reference Manual, version 1.02:
  ;; 
  ;;   ...recursive copying does not take place when the definition of
  ;;   a character is a symbol whose function definition is a keymap;
  ;;   the same symbol appears in the new copy.
  ;;   
  ;; Unfortunately, recursive copying DOES take place in this
  ;; situation.  This means that the potential for an infinite
  ;; recursive copying situation is possible.  For example, if
  ;; vt200.el has been loaded and enable-arrow-keys has been executed
  ;; prior to invoking edt-emulation-on, then the sequence "\e[23~"
  ;; has been bound to ESC-prefix.  Any attempt to copy the map with
  ;; copy-keymap will cause Emacs to enter an infinite recursive loop
  ;; leading to a memory fault.  To avoid this problem, we temporarily
  ;; redefine it so we can successfully copy ESC-prefix.
  (let ((save-key (where-is-internal 'ESC-prefix 'ESC-prefix t)))
    (if save-key
	(define-key 'ESC-prefix save-key nil))
    (fset 'edt-default-ESC-prefix (copy-keymap 'ESC-prefix))
    (setq edt-default-global-map (copy-keymap (current-global-map)))
    (define-key edt-default-global-map "\e" 'edt-default-ESC-prefix)
    (if (fboundp 'edt-additional-default-prefix-map-setup)
	(edt-additional-default-prefix-map-setup))
    (if save-key
	(define-key 'ESC-prefix save-key 'ESC-prefix)))
  (define-prefix-command 'edt-default-gold-map)
  (edt-setup-default-bindings)
  (if (fboundp 'edt-setup-extra-default-bindings)
      (edt-setup-extra-default-bindings))
  (setq edt-learn-macro-count 0)
  ;; Display EDT text selection active within the mode line
  (or (assq 'edt-select-active minor-mode-alist)
      (setq minor-mode-alist 
	    (cons '(edt-select-active edt-select-active) minor-mode-alist)))
  ;; Display EDT direction of motion within the mode line
  (or (assq 'edt-direction-string minor-mode-alist)
      (setq minor-mode-alist
	    (cons
	      '(edt-direction-string edt-direction-string) minor-mode-alist)))
  (if user-setup
      (progn
        (setq edt-user-map-configured t)
        (fset 'edt-emulation-on (symbol-function 'edt-select-user-global-map)))
      (progn
        (fset 'edt-emulation-on (symbol-function 'edt-select-default-global-map))
        (edt-select-default-global-map))))

;;;
;;; Setup EDT user emulation.
;;;

(defun edt-user-emulation-setup ()
  "Setup user custom emulation of DEC's EDT editor."
  ;; Initialize EDT default bindings.
  (edt-default-emulation-setup t)
  ;; Setup user EDT global map by copying default EDT global map bindings.
  ;; See comments in edt-default-emulation-setup for explanation of
  ;; need for the following code to avoid an infinite recursion problem
  ;; when copying edt-default-ESC-prefix.
  (let ((save-key 
	(where-is-internal 'edt-default-ESC-prefix 'edt-default-ESC-prefix t)))
    (if save-key
	(define-key 'edt-default-ESC-prefix save-key nil))
    (fset 'edt-user-ESC-prefix (copy-keymap 'edt-default-ESC-prefix))
    (setq edt-user-global-map (copy-keymap edt-default-global-map))
    (define-key edt-user-global-map "\e" 'edt-user-ESC-prefix)
    (if (fboundp 'edt-additional-user-prefix-map-setup)
	(edt-additional-user-prefix-map-setup))
    (if save-key
	(define-key 'edt-default-ESC-prefix save-key 'edt-default-ESC-prefix)))
  (define-prefix-command 'edt-user-gold-map)
  (fset 'edt-user-gold-map (copy-keymap 'edt-default-gold-map))
  (edt-setup-user-bindings)
  (edt-select-user-global-map))

(provide 'edt)

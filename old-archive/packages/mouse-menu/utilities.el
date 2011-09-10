;;;; File of random useful functions used elsewhere.
;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Tue Oct 11 10:55:53 1988

(provide 'utilities)

(defun auto-save-file-name-p (filename)
  "Return t if FILENAME can be yielded by make-auto-save-file-name.
FILENAME should lack slashes.
You can redefine this for customization.
Redefined so that last # is optional so TeX temp files can be snarfed by dired."
  (string-match "^#.*#?$" filename))	

(defun switch-to-existing-buffer-other-window (buffer)
  "Select existing buffer BUFFER in another window."
  (interactive "BSwitch to buffer in other window: ")
  (switch-aux buffer))

(defun switch-aux (buffer)
  (if (buffer-existsp buffer)
      (let ((pop-up-windows t))
	(pop-to-buffer buffer t))
    (switch-aux (read-buffer "Buffer: " (other-buffer)))))

(defun buffer-existsp (name)
  "Returns true if NAME is a buffer."
  (get-buffer name))

(defun member (x list)
  "True if X is equal to some member of LIST"
  (while (and list (not (equal x (car list))))
    (setq list (cdr list)))
  list)

(defun append-member (item set)
  "Returns [ITEM] + SET i.e. adds ITEM to SET iff (not (member ITEM SET))."
  (if (member item set)
      set
    (append (list item) set)))

(defun append-memq (item set)
  "Returns [ITEM] + SET i.e. adds ITEM to SET iff (not (memq ITEM SET))."
  (if (memq item set)
      set
    (append (list item) set)))

(defun file-exists-path-p (file path)
  "Return the full path name of first instance of FILE found on PATH."
  (catch 'file
    (while path
      (let ((try (concat (car path) "/" file)))
	(if (file-exists-p try)
	    (throw 'file try)))
      (setq path (cdr path)))))

(defun read-existing-program-name (&optional prompt)
  (let* ((name (read-string (or prompt "Which program: ")))
	 (program (file-exists-path-p name exec-path)))
    (or program
	(read-existing-program-name
	 (concat "Sorry, " name " doesn't exist, try another program: ")))))

(defun new-bindings (map key-binding-list)
  "First arg is map to change, second is key-binding-list.
Map over key-binding-list (of form '((key binding)...)) making KEY in MAP
do BINDING."
  (while key-binding-list
    (let* ((key-binding (car key-binding-list))
	   (key (car key-binding))
	   (command (car (cdr key-binding))))
      (define-key map key command)
      (setq key-binding-list (cdr key-binding-list)))))

(defun lpq ()
  "Check the Laserwriter queue in a typeout window."
  (interactive)
  (require 'typeout)
  (typeout-buffer "Laserwriter Queue" "lpq" nil "-Plw"))

(defun white-spacep (string)
  "Return t if STRING consists solely of white-space characters i.e. SPACE and TAB."
  (if (not (string= string ""))
      ;; For now at least, null strings are NOT whitespace.
      (if (string-match "[ \t]*" string)
	  (= (match-end 0) (length string)))))

(defun skip-whitespace (&optional and-newlines)
  "Skip over whitespace characters (space and tab). 
Count newlines as whitespace if AND-NEWLINES is non-nil"
  (skip-chars-forward (if and-newlines " \t\n" " \t")))

(defconst cursor-pause-seconds 1
  "*Number of seconds to display alternate cursor (usually the mark)")
  
(defun indicate-region (&optional beg end pause)
  "Bounce cursor to mark for cursor-pause-seconds and back again.
Optional args:
 BEG (start point of region to indicate, default (mark))
 END (end point of region to indicate, default (point))
 PAUSE (time to wait in seconds, default cursor-pause-seconds)"
  (or pause (setq pause cursor-pause-seconds))
  (or beg (setq beg (mark)))
  (let ((point (point)))
    (goto-char beg)
    (sit-for pause)
    (if (not end)
	(goto-char point)		; no end point, just go back to start.
      (goto-char end)
      (sit-for pause)
      (goto-char point))))

(defun new-buffer-other-window (mode &optional suffix force)
  "Create a new buffer, put it into MODE, and visit in another window.
Optional 2nd argument SUFFIX is a suffix to append to file-name.
If optional 3rd argument FORCE is non-nil do not use auto-mode-alist
for file mode, but use MODE instead."
  (let ((buffer-name
	 (if suffix (concat "NewFile" suffix) (generate-new-buffer "New Buffer"))))
    (if suffix
	;; If we have a file suffix, visit the file to guarantee auto-saving
	(find-file-other-window buffer-name)
      (switch-to-buffer-other-window buffer-name))
    (set-buffer buffer-name)
    (if (or force
	    (null suffix)
	    (not (assoc (concat (regexp-quote suffix) "$") auto-mode-alist)))
	;; Do not override user's choices.
	(funcall mode))
    (if (not (memq mode '(fundamental-mode text-mode lisp-interaction-mode)))	
	;; Insert the appropriate commented mode set-up string 
	(let ((mode-identification-string
		(format "%s -*- %s -*-%s\n"
			(or comment-start "")
			(substring (symbol-name mode) 0 -5) ; Take off "-mode" 
			comment-end)))
	  (if (not (looking-at (regexp-quote mode-identification-string)))
	      (insert mode-identification-string))))
    (set-buffer-modified-p nil)))	; Mark the buffer as not modified.

(defun continue-shell-subjob ()
  "Continue (restart) this shell's current subjob."
  (interactive)
  (let ((buff-proc (get-buffer-process (current-buffer))))
    (if (eq (process-status buff-proc) 'run)
	;; Assume we have a stopped shell subjob: use 'fg'.
	(save-excursion
	  (goto-char (point-max))
	  (insert "fg")
	  (shell-send-input))
      (continue-process buff-proc))))
  
(defun indent-defform ()
  "Indent the surrounding or previous 'top-level' form."
  (save-excursion
    (beginning-of-defun 1)
    (indent-sexp)))
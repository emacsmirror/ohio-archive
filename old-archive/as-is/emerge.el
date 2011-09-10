;;; Emacs subsystem to merge two files
;;; Written by Dale R. Worley, drw@math.mit.edu.

;;; Code to run at load time.

;; Declare that we've got the subsystem loaded
(provide 'emerge)

;;; Macros

(defmacro eval-in-buffer (buffer &rest forms)
  "Macro to switch to BUFFER, evaluate FORMS, returns to original buffer."
  (` (let ((StartBuffer (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer (, buffer))
	  (,@ forms))
    (set-buffer StartBuffer)))))

(defmacro defvar-local (var value doc) 
  "Defines SYMBOL as an advertised variable.  Performs exactly the same
operations as defvar, then executes make-variable-buffer-local."
  (list 'progn
	(list 'defvar var value doc)
	(list 'make-variable-buffer-local (list 'quote
						var))))

;; Add entries to minor-mode-alist so that emerge modes show correctly
(if (not (assq 'emerge-mode minor-mode-alist))
    (setq minor-mode-alist (append '((emerge-mode " Emerge")
				     (emerge-fast-mode " F")
				     (emerge-edit-mode " E")
				     (emerge-auto-advance " A")
				     (emerge-skip-prefers " S"))
				   minor-mode-alist)))

;;; Emerge configuration variables

;; Commands that produce difference files
;; Note that although these appear to be configurable, the only thing
;; which is really configurable is the command name and options --
;; even the order in which the file names are given is fixed.  Also
;; the code which processes the diff/diff3 output depends on all the
;; finnicky details of their output, including the somewhat strange
;; way they number lines of a file.
(defvar emerge-diff-command "diff %s %s"
  "Format that takes two file names and produces an appropriate diff command.")
(defvar emerge-diff3-command "diff3 %s %s %s"
  "Format that takes the ancestor and the two variant file names and produces
an appropriate diff3 command.")
(defvar emerge-match-diff-line (let ((x "\\([0-9]+\\)\\(\\|,\\([0-9]+\\)\\)"))
				 (concat "^" x "\\([acd]\\)" x "$"))
  "Pattern to match lines produced by diff that describe differences (as
opposed to lines from the source files).")

;; The flags used to mark differences in the buffers.
(defvar emerge-before-flag "vvvvvvvvvvvvvvvvvvvv\n"
  "Flag placed above the highlighted block of code.  Must end with newline.")
(defvar emerge-before-flag-length (length emerge-before-flag))
(defvar emerge-before-flag-match (regexp-quote emerge-before-flag))
(defvar emerge-after-flag "^^^^^^^^^^^^^^^^^^^^\n"
  "Flag placed below the highlighted block of code.  Must end with newline.")
(defvar emerge-after-flag-length (length emerge-after-flag))
(defvar emerge-after-flag-match (regexp-quote emerge-after-flag))

;; Build keymaps

(defvar emerge-fast-keymap (make-sparse-keymap)
  "Local keymap used in fast mode.  Also, bound to \\C-c in edit mode.")
(define-key emerge-fast-keymap "p" 'emerge-previous-difference)
(define-key emerge-fast-keymap "n" 'emerge-next-difference)
(define-key emerge-fast-keymap "a" 'emerge-select-A)
(define-key emerge-fast-keymap "b" 'emerge-select-B)
(define-key emerge-fast-keymap "j" 'emerge-jump-to-difference)
(define-key emerge-fast-keymap "q" 'emerge-quit)
(define-key emerge-fast-keymap "f" 'emerge-fast-mode)
(define-key emerge-fast-keymap "e" 'emerge-edit-mode)
(define-key emerge-fast-keymap "\C-a" 'emerge-set-auto-advance)
(define-key emerge-fast-keymap "\C-s" 'emerge-set-skip-prefers)
(define-key emerge-fast-keymap "\C-l" 'emerge-recenter)
(define-key emerge-fast-keymap "-" 'digit-argument)
(define-key emerge-fast-keymap "0" 'digit-argument)
(define-key emerge-fast-keymap "1" 'digit-argument)
(define-key emerge-fast-keymap "2" 'digit-argument)
(define-key emerge-fast-keymap "3" 'digit-argument)
(define-key emerge-fast-keymap "4" 'digit-argument)
(define-key emerge-fast-keymap "5" 'digit-argument)
(define-key emerge-fast-keymap "6" 'digit-argument)
(define-key emerge-fast-keymap "7" 'digit-argument)
(define-key emerge-fast-keymap "8" 'digit-argument)
(define-key emerge-fast-keymap "9" 'digit-argument)
(define-key emerge-fast-keymap "\ea" 'emerge-default-A)
(define-key emerge-fast-keymap "\eb" 'emerge-default-B)

;; Variables which control each merge.  They are local to the merge buffer.

;; Mode variables
(defvar-local emerge-mode nil
  "Indicator for emerge-mode.")
(defvar-local emerge-fast-mode nil
  "Indicator for emerge-mode fast submode.")
(defvar-local emerge-edit-mode nil
  "Indicator for emerge-mode edit submode.")
(defvar-local emerge-A-buffer nil
  "The buffer in which the A variant is stored.")
(defvar-local emerge-B-buffer nil
  "The buffer in which the B variant is stored.")
(defvar-local emerge-A-buffer-modified nil
  "Remembers whether emerge-A-buffer was modified.")
(defvar-local emerge-B-buffer-modified nil
  "Remembers whether emerge-B-buffer was modified.")
(defvar-local emerge-A-buffer-read-only nil
  "Remembers whether emerge-A-buffer was read-only.")
(defvar-local emerge-B-buffer-read-only nil
  "Remembers whether emerge-B-buffer was read-only.")
(defvar-local emerge-A-buffer-autosave nil
  "Remembers buffer-auto-save-file-name for emerge-A-buffer.")
(defvar-local emerge-B-buffer-autosave nil
  "Remembers buffer-auto-save-file-name emerge-B-buffer.")
(defvar-local emerge-difference-list nil
  "Vector of differences between the variants, and markers in the buffers to
show where they are.  Each difference is represented by a vector of seven
elements.  The first two are markers to the beginning and end of the difference
section in the A buffer, the second two are markers for the B buffer, the third
two are markers for the merge buffer, and the last element is the \"state\" of
that difference in the merge buffer.
  A section of a buffer is described by two markers, one to the beginning of
the first line of the section, and one to the beginning of the first line
after the section.  (If the section is empty, both markers point to the same
point.)  If the section is part of the selected difference, then the markers
are moved into the flags, so the user can edit the section without disturbing
the markers.
  The \"states\" are:
	A		the merge buffer currently contains the A variant
	B		the merge buffer currently contains the B variant
	default-A	the merge buffer contains the A variant by default,
			but this difference hasn't been selected yet, so
			change-default commands can alter it
	default-B	the merge buffer contains the B variant by default,
			but this difference hasn't been selected yet, so
			change-default commands can alter it
	prefer-A	in a three-file merge, the A variant is the prefered
			choice
	prefer-B	in a three-file merge, the B variant is the prefered
			choice")
(defvar-local emerge-current-difference -1
  "The difference that is currently selected.")
(defvar-local emerge-number-of-differences nil
  "Number of differences found.")
(defvar-local emerge-edit-keymap nil
  "The local keymap for the merge buffer, with the emerge commands defined in
it.  Used to save the local keymap during fast mode, when the local keymap is
replaced by emerge-fast-keymap.")
(defvar-local emerge-auto-advance nil
  "If non-nil, emerge-select-A and emerge-select-B automatically advance to
the next difference.")
(defvar-local emerge-skip-prefers nil
  "If non-nil, differences for which there is a preference are automatically
skipped.")

;;; Top-level and setup functions for two-file mode.

(defun emerge-files (file-A file-B)
  "Run Emerge on two files."
  (interactive "fFile A to merge: \nfFile B to merge: ")
  (let ((buffer-A (find-file-noselect file-A))
	(buffer-B (find-file-noselect file-B)))
    (emerge-setup buffer-A file-A buffer-B file-B)))

(defun emerge-buffers (buffer-A buffer-B)
  "Run Emerge on two buffers."
  (interactive "bBuffer A to merge: \nbBuffer B to merge: ")
  (let ((file-A (make-temp-name "/tmp/emergeA"))
	(file-B (make-temp-name "/tmp/emergeB")))
    (eval-in-buffer
     buffer-A
     (write-region (point-min) (point-max) file-A nil 'no-message))
    (eval-in-buffer
     buffer-B
     (write-region (point-min) (point-max) file-B nil 'no-message))
    (emerge-setup (get-buffer buffer-A) file-A (get-buffer buffer-B) file-B)))

;; Start up Emerge on two files
(defun emerge-setup (buffer-A file-A buffer-B file-B)
  (let* ((merge-buffer-name (emerge-unique-buffer-name "*merge" "*"))
	 ;; create the merge buffer from buffer A, so it inherits buffer A's
	 ;; default directory, etc.
	 (merge-buffer (eval-in-buffer buffer-A
				       (get-buffer-create merge-buffer-name)))
	 (buffer-A-mode (eval-in-buffer buffer-A
					major-mode)))
    (eval-in-buffer
     merge-buffer
     (funcall buffer-A-mode)
     (setq buffer-read-only nil)
     (auto-save-mode 1)
     (setq emerge-mode t)
     (setq emerge-A-buffer buffer-A)
     (setq emerge-B-buffer buffer-B)
     (setq emerge-merge-buffer merge-buffer)
     (insert-buffer emerge-A-buffer)
     (emerge-set-keys)
     (setq emerge-difference-list (emerge-make-diff-list file-A file-B))
     (setq emerge-number-of-differences (length emerge-difference-list))
     (setq emerge-current-difference -1)
     (emerge-select-prefer-Bs)
     (emerge-remember-buffer-characteristics))
    (emerge-setup-windows buffer-A buffer-B merge-buffer)))

;; Generate the Emerge difference list between two files
(defun emerge-make-diff-list (file-A file-B)
  (setq emerge-diff-buffer (get-buffer-create "*emerge-diff*"))
  (eval-in-buffer
   emerge-diff-buffer
   (erase-buffer)
   (shell-command (format emerge-diff-command file-A file-B) t))
  (emerge-convert-diffs-to-markers
   emerge-A-buffer emerge-B-buffer emerge-merge-buffer
   (emerge-extract-diffs emerge-diff-buffer)))

(defun emerge-extract-diffs (diff-buffer)
  (let (list)
  (eval-in-buffer
   diff-buffer
   (goto-char (point-min))
   (while (re-search-forward emerge-match-diff-line nil t)
     (let* ((a-begin (string-to-int (buffer-substring (match-beginning 1)
						      (match-end 1))))
	    (a-end  (let ((b (match-beginning 3))
			  (e (match-end 3)))
		      (if b
			  (string-to-int (buffer-substring b e))
			a-begin)))
	    (diff-type (buffer-substring (match-beginning 4) (match-end 4)))
	    (b-begin (string-to-int (buffer-substring (match-beginning 5)
						      (match-end 5))))
	    (b-end (let ((b (match-beginning 7))
			 (e (match-end 7)))
		     (if b
			 (string-to-int (buffer-substring b e))
		       b-begin))))
       ;; fix the beginning and end numbers, because diff is somewhat
       ;; strange about how it numbers lines
       (if (string-equal diff-type "a")
	   (progn
	     (setq b-end (1+ b-end))
	     (setq a-begin (1+ a-begin))
	     (setq a-end a-begin))
	 (if (string-equal diff-type "d")
	     (progn
	       (setq a-end (1+ a-end))
	       (setq b-begin (1+ b-begin))
	       (setq b-end b-begin))
	   ;; (string-equal diff-type "c")
	   (progn
	     (setq a-end (1+ a-end))
	     (setq b-end (1+ b-end)))))
       (setq list (cons (vector a-begin a-end
				b-begin b-end
				'default-A)
			list)))))
  (nreverse list)))

;;; Top-level and setup functions for three-file mode.

(defun emerge-files-with-ancestor (file-A file-B file-ancestor)
  "Run Emerge on two files, giving another file as the ancestor."
  (interactive "fFile A to merge: \nfFile B to merge: \nfAncestor file: ")
  (let ((buffer-A (find-file-noselect file-A))
	(buffer-B (find-file-noselect file-B))
	(buffer-ancestor (find-file-noselect file-ancestor)))
    (emerge-setup-with-ancestor buffer-A file-A buffer-B file-B
				buffer-ancestor file-ancestor)))

(defun emerge-buffers-with-ancestor (buffer-A buffer-B buffer-ancestor)
  "Run Emerge on two buffers, giving another buffer as the ancestor."
  (interactive
   "bBuffer A to merge: \nbBuffer B to merge: \nbAncestor buffer: ")
  (let ((file-A (make-temp-name "/tmp/emergeA"))
	(file-B (make-temp-name "/tmp/emergeB"))
	(file-ancestor (make-temp-name "/tmp/emergeanc")))
    (eval-in-buffer
     buffer-A
     (write-region (point-min) (point-max) file-A nil 'no-message))
    (eval-in-buffer
     buffer-B
     (write-region (point-min) (point-max) file-B nil 'no-message))
    (eval-in-buffer
     buffer-ancestor
     (write-region (point-min) (point-max) file-ancestor nil 'no-message))
    (emerge-setup-with-ancestor (get-buffer buffer-A) file-A
				(get-buffer buffer-B) file-B
				(get-buffer buffer-ancestor) file-ancestor)))

;; Start up Emerge on two files with an ancestor
(defun emerge-setup-with-ancestor (buffer-A file-A buffer-B file-B
					    buffer-ancestor file-ancestor)
  (let* ((merge-buffer-name (emerge-unique-buffer-name "*merge" "*"))
	 ;; create the merge buffer from buffer A, so it inherits buffer A's
	 ;; default directory, etc.
	 (merge-buffer (eval-in-buffer buffer-A
			 (get-buffer-create merge-buffer-name)))
	 (buffer-A-mode (eval-in-buffer buffer-A
			  major-mode)))
    (eval-in-buffer
     merge-buffer
     (funcall buffer-A-mode)
     (setq buffer-read-only nil)
     (auto-save-mode 1)
     (setq emerge-mode t)
     (setq emerge-A-buffer buffer-A)
     (setq emerge-B-buffer buffer-B)
     (setq emerge-merge-buffer merge-buffer)
     (insert-buffer emerge-A-buffer)
     (emerge-set-keys)
     (setq emerge-difference-list
	   (emerge-make-diff3-list file-A file-B file-ancestor))
     (setq emerge-number-of-differences (length emerge-difference-list))
     (setq emerge-current-difference -1)
     (emerge-select-prefer-Bs)
     (emerge-remember-buffer-characteristics))
    (emerge-setup-windows buffer-A buffer-B merge-buffer)))

;; Generate the Emerge difference list between two files with an ancestor
(defun emerge-make-diff3-list (file-A file-B file-ancestor)
  (setq emerge-diff-buffer (get-buffer-create "*emerge-diff*"))
  (eval-in-buffer
   emerge-diff-buffer
   (erase-buffer)
   (shell-command (format emerge-diff3-command file-ancestor file-A file-B)
		  t))
  (emerge-convert-diffs-to-markers
   emerge-A-buffer emerge-B-buffer emerge-merge-buffer
   (emerge-extract-diffs3 emerge-diff-buffer)))

(defun emerge-extract-diffs3 (diff-buffer)
  (let (list)
    (eval-in-buffer
     diff-buffer
     (while (re-search-forward "^====\\(.?\\)$" nil t)
       ;; leave point after matched line
       (beginning-of-line 2)
       (let ((agreement (buffer-substring (match-beginning 1) (match-end 1))))
	 ;; if the A and B files are the same, ignore the difference
	 (if (not (string-equal agreement "1"))
	     (setq list
		   (cons 
		    (let ((group-2 (emerge-get-diff3-group "2"))
			  (group-3 (emerge-get-diff3-group "3")))
		      (vector (car group-2) (car (cdr group-2))
			      (car group-3) (car (cdr group-3))
			      (cond ((string-equal agreement "2") 'prefer-A)
				    ((string-equal agreement "3") 'prefer-B)
				    (t 'default-A))))
		    list))))))
    (nreverse list)))

(defun emerge-get-diff3-group (file)
  (re-search-forward
   (concat "^" file ":\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?\\([ac]\\)$"))
  (beginning-of-line 2)
  ;; treatment depends on whether it is an "a" group or a "c" group
  (if (string-equal (buffer-substring (match-beginning 4) (match-end 4)) "c")
      ;; it is a "c" group
      (if (match-beginning 2)
	  ;; it has two numbers
	  (list (string-to-int
		 (buffer-substring (match-beginning 1) (match-end 1)))
		(1+ (string-to-int
		     (buffer-substring (match-beginning 3) (match-end 3)))))
	;; it has one number
	(let ((x (string-to-int
		  (buffer-substring (match-beginning 1) (match-end 1)))))
	  (list x (1+ x))))
    ;; it is an "a" group
    (let ((x (1+ (string-to-int
		  (buffer-substring (match-beginning 1) (match-end 1))))))
      (list x x))))

;;; Common setup routines

;; Set up the window configuration
(defun emerge-setup-windows (buffer-A buffer-B merge-buffer)
  (delete-other-windows)
  (switch-to-buffer merge-buffer)
  (split-window-vertically)
  (split-window-horizontally)
  (switch-to-buffer buffer-A)
  (set-window-point (selected-window) (point-min))
  (goto-char (point-min))
  (other-window 1)
  (switch-to-buffer buffer-B)
  (set-window-point (selected-window) (point-min))
  (goto-char (point-min))
  (other-window 1)
  (goto-char (point-min)))

;; Set up the keymap in the merge buffer
(defun emerge-set-keys ()
  (setq emerge-edit-keymap (if (current-local-map)
			       (copy-keymap (current-local-map))
			     '(keymap)))
  (use-local-map emerge-fast-keymap)
  (setq emerge-edit-mode nil)
  (setq emerge-fast-mode t)
  (define-key emerge-edit-keymap "\C-c" emerge-fast-keymap))

(defun emerge-remember-buffer-characteristics ()
  "Must be called in the merge buffer.  Remembers certain properties of the
buffers being merged (read-only, modified, auto-save), and saves them in
buffer-local-variables.  Sets the buffers read-only and turns off auto-save.
These characteristics are restored by emerge-restore-buffer-characteristics."
  ;; force auto-save, because we will turn off auto-saving in buffers for the
  ;; duration
  (do-auto-save)
  ;; remember buffer A's characteristics
  (setq emerge-A-buffer-modified (buffer-modified-p emerge-A-buffer))
  (setq emerge-A-buffer-read-only (eval-in-buffer
				   emerge-A-buffer
				   buffer-read-only))
  (setq emerge-A-buffer-autosave (eval-in-buffer
				  emerge-A-buffer
				  buffer-auto-save-file-name))
  ;; remember buffer B's characteristics
  (setq emerge-B-buffer-modified (buffer-modified-p emerge-B-buffer))
  (setq emerge-B-buffer-read-only (eval-in-buffer
				   emerge-B-buffer
				   buffer-read-only))
  (setq emerge-B-buffer-autosave (eval-in-buffer
				  emerge-B-buffer
				  buffer-auto-save-file-name))
  ;; set buffers read-only
  (eval-in-buffer
   emerge-A-buffer
   (setq buffer-read-only t)
   (auto-save-mode 0))
  (eval-in-buffer
   emerge-B-buffer
   (setq buffer-read-only t)
   (auto-save-mode 0)))

(defun emerge-restore-buffer-characteristics ()
  "Restores the characteristics remembered by
emerge-remember-buffer-characteristics."
  (let ((ro emerge-A-buffer-read-only)
	(mod emerge-A-buffer-modified)
	(as emerge-A-buffer-autosave))
    (eval-in-buffer
     emerge-A-buffer
     (setq buffer-read-only ro)
     (set-buffer-modified-p mod)
     (setq buffer-auto-save-file-name as)))
  (let ((ro emerge-B-buffer-read-only)
	(mod emerge-B-buffer-modified)
	(as emerge-B-buffer-autosave))
    (eval-in-buffer
     emerge-B-buffer
     (setq buffer-read-only ro)
     (set-buffer-modified-p mod)
     (setq buffer-auto-save-file-name as))))

(defun emerge-convert-diffs-to-markers (A-buffer
					B-buffer
					merge-buffer
					lineno-list)
  (let (marker-list
	(offset (1- (eval-in-buffer A-buffer (point-min)))))
    (while lineno-list
      (let* ((list-element (car lineno-list))
	     a-begin-marker
	     a-end-marker
	     b-begin-marker
	     b-end-marker
	     (a-begin (aref list-element 0))
	     (a-end (aref list-element 1))
	     (b-begin (aref list-element 2))
	     (b-end (aref list-element 3))
	     (state (aref list-element 4)))
	;; place markers at the appropriate places in the buffers
	(eval-in-buffer
	 A-buffer
	 (goto-line a-begin)
	 (setq a-begin-marker (point-marker))
	 (goto-line a-end)
	 (setq a-end-marker (point-marker)))
	(eval-in-buffer
	 B-buffer
	 (goto-line b-begin)
	 (setq b-begin-marker (point-marker))
	 (goto-line b-end)
	 (setq b-end-marker (point-marker))
	 (setq merge-begin-marker (set-marker
				   (make-marker)
				   (- (marker-position a-begin-marker)
				      offset)
				   merge-buffer))
	 (setq merge-end-marker (set-marker
				 (make-marker)
				 (- (marker-position a-end-marker)
				    offset)
				 merge-buffer)))
	;; record all the markers for this difference
	(setq marker-list (cons (vector a-begin-marker a-end-marker
					b-begin-marker b-end-marker
					merge-begin-marker merge-end-marker
					state)
				marker-list)))
      (setq lineno-list (cdr lineno-list)))
    ;; convert the list of difference information into a vector for fast access
    (setq emerge-difference-list (apply 'vector (nreverse marker-list)))))

;; If we have an ancestor, select all B variant that we prefer 
(defun emerge-select-prefer-Bs ()
  (let ((n 0))
    (while (< n (length emerge-difference-list))
      (if (eq (aref (aref emerge-difference-list n) 6) 'prefer-B)
	  (progn
	    (emerge-unselect-and-select-difference n)
	    (emerge-select-B)
	    (aset (aref emerge-difference-list n) 6 'prefer-B)))
      (setq n (1+ n))))
  (emerge-unselect-and-select-difference -1))

;;; Commands

(defun emerge-recenter (arg)
  "Perform  recenter  on all three windows.  If an ARGUMENT is given, it is
passed to  recenter  in all three windows."
  (interactive "P")
  (let ((merge-buffer emerge-merge-buffer)
	(buffer-A emerge-A-buffer)
	(buffer-B emerge-B-buffer))
    (if arg
	;; recenter each window in the requested manner
	(let ((window-A (get-buffer-window buffer-A))
	      (window-B (get-buffer-window buffer-B))
	      (merge-window (get-buffer-window merge-buffer)))
	  (if window-A (progn
			 (select-window window-A)
			 (recenter arg)))
	  (if window-B (progn
			 (select-window window-B)
			 (recenter arg)))
	  (if merge-window (progn
			     (select-window merge-window)
			     (recenter arg))))
      ;; redraw the screen entirely
      (delete-other-windows)
      (switch-to-buffer merge-buffer)
      (split-window-vertically)
      (split-window-horizontally)
      (switch-to-buffer buffer-A)
      (recenter)
      (other-window 1)
      (switch-to-buffer buffer-B)
      (recenter)
      (other-window 1)
      (recenter))))

(defun emerge-next-difference ()
  "Advance to the next difference."
  (interactive)
  (if (< emerge-current-difference emerge-number-of-differences)
      (let ((n (1+ emerge-current-difference)))
	(while (and emerge-skip-prefers
		    (< n emerge-number-of-differences)
		    (memq (aref (aref emerge-difference-list n) 6)
			  '(prefer-A prefer-B)))
	  (setq n (1+ n)))
	(let ((buffer-read-only nil))
	  (emerge-unselect-and-select-difference n)))
    (ding)))

(defun emerge-previous-difference ()
  "Go to the previous difference."
  (interactive)
  (if (> emerge-current-difference -1)
      (let ((n (1- emerge-current-difference)))
	(while (and emerge-skip-prefers
		    (> n -1)
		    (memq (aref (aref emerge-difference-list n) 6)
			  '(prefer-A prefer-B)))
	  (setq n (1- n)))
	(let ((buffer-read-only nil))
	  (emerge-unselect-and-select-difference n)))
    (ding)))

(defun emerge-jump-to-difference (difference-number)
  "Go to the N-th difference."
  (interactive "p")
  (let ((buffer-read-only nil))
    (setq difference-number (1- difference-number))
    (if (and (>= difference-number 0)
	     (< difference-number emerge-number-of-differences))
	(emerge-unselect-and-select-difference difference-number)
      (ding))))

(defun emerge-quit (arg)
  "Finish an emerge session.  Prefix ARGUMENT means to write the merge buffer
to disk under the name of the file in the A buffer.
Unselects the selected difference, if any, restores the read-only and modified
flags of the merged file buffers, restores the edit keymap of the merge buffer,
and sets off various emerge flags.  Using emerge buffer commands after this
will cause serious problems.  Thus this command removes the Emerge-specific
key bindings."
  (interactive "P")
  (setq buffer-read-only nil)
  (emerge-unselect-and-select-difference -1)
  (emerge-restore-buffer-characteristics)
  ;; undefine the emerge commands
  (define-key emerge-edit-keymap "\C-c" nil)
  (use-local-map emerge-edit-keymap)
  ;; turn off all the emerge modes
  (setq emerge-mode nil)
  (setq emerge-fast-mode nil)
  (setq emerge-edit-mode nil)
  (setq emerge-auto-advance nil)
  (setq emerge-skip-prefers nil)
  ;; restore mode line
  (kill-local-variable 'mode-line-buffer-identification)
  ;; if argument was given, save to disk
  (if arg
      (let ((writable (eval-in-buffer emerge-A-buffer (not buffer-read-only)))
	    (save-file-name (eval-in-buffer emerge-A-buffer buffer-file-name)))
	;; A buffer must have file name and be writable
	(if (and save-file-name writable)
	    (if (y-or-n-p (concat
			   "Do you really want to save merged buffer as file "
			   save-file-name
			   "? "))
		(progn
		  ;; make A buffer not be buffer file
		  (eval-in-buffer
		   emerge-A-buffer
		   (rename-buffer (emerge-unique-buffer-name
				   "*merge-old" "*"))
		   (setq buffer-file-name nil))
		  ;; make merge buffer visit file
		  (write-file save-file-name)))
	  (error "Buffer A not visiting file or read-only.")))))

(defun emerge-select-A (&optional force)
  "Select the A variant of this difference.  Refuses to function if this
difference has been edited, i.e., if it is neither the A nor the B variant.
An ARGUMENT forces the variant to be selected even if the differences has
been edited."
  (interactive "P")
  (let ((buffer-read-only nil))
    (let* ((diff-vector
	    (aref emerge-difference-list emerge-current-difference))
	   (A-begin (1+ (aref diff-vector 0)))
	   (A-end (1- (aref diff-vector 1)))
	   (B-begin (1+ (aref diff-vector 2)))
	   (B-end (1- (aref diff-vector 3)))
	   (merge-begin (1+ (aref diff-vector 4)))
	   (merge-end (1- (aref diff-vector 5))))
      (if force
	  (emerge-select-A-edit merge-begin merge-end A-begin A-end)
	(if (emerge-compare-buffers emerge-B-buffer B-begin B-end
				    emerge-merge-buffer merge-begin merge-end)
	    (progn
	      (emerge-select-A-edit merge-begin merge-end A-begin A-end)
	    
	      (if emerge-auto-advance
		  (emerge-next-difference)))
	  (if (emerge-compare-buffers emerge-A-buffer A-begin A-end
				      emerge-merge-buffer merge-begin
				      merge-end)
	      (if emerge-auto-advance
		  (emerge-next-difference))
	    (ding)
	    (message "This difference region has been edited.")))))))

;; Actually select the A variant
(defun emerge-select-A-edit (merge-begin merge-end A-begin A-end)
  (eval-in-buffer
   emerge-merge-buffer
   (delete-region merge-begin merge-end)
   (goto-char merge-begin)
   (insert-buffer-substring emerge-A-buffer A-begin A-end)
   (aset diff-vector 6 'a)
   (emerge-refresh-mode-line)))

(defun emerge-select-B (&optional force)
  "Select the B variant of this difference.  Refuses to function if this
difference has been edited, i.e., if it is neither the A nor the B variant.
An ARGUMENT forces the variant to be selected even if the differences has
been edited."
  (interactive "P")
  (let ((buffer-read-only nil))
    (let* ((diff-vector
	    (aref emerge-difference-list emerge-current-difference))
	   (A-begin (1+ (aref diff-vector 0)))
	   (A-end (1- (aref diff-vector 1)))
	   (B-begin (1+ (aref diff-vector 2)))
	   (B-end (1- (aref diff-vector 3)))
	   (merge-begin (1+ (aref diff-vector 4)))
	   (merge-end (1- (aref diff-vector 5))))
      (if force
	  (emerge-select-B-edit merge-begin merge-end B-begin B-end)
	(if (emerge-compare-buffers emerge-A-buffer A-begin A-end
				    emerge-merge-buffer merge-begin merge-end)
	    (progn
	      (emerge-select-B-edit merge-begin merge-end B-begin B-end)
	      (if emerge-auto-advance
		  (emerge-next-difference)))
	  (if (emerge-compare-buffers emerge-B-buffer B-begin B-end
				      emerge-merge-buffer merge-begin
				      merge-end)
	      (if emerge-auto-advance
		  (emerge-next-difference))
	    (ding)
	    (message "This difference region has been edited.")))))))

;; Actually select the B variant
(defun emerge-select-B-edit (merge-begin merge-end B-begin B-end)
  (eval-in-buffer
   emerge-merge-buffer
   (delete-region merge-begin merge-end)
   (goto-char merge-begin)
   (insert-buffer-substring emerge-B-buffer B-begin B-end)
   (aset diff-vector 6 'b)
   (emerge-refresh-mode-line)))

(defun emerge-default-A ()
  "Selects the A variant for all differences from here down in the buffer
which are still defaulted, i.e., which the user has not selected and for
which there is no preference."
  (interactive)
  (let ((buffer-read-only nil))
    (let ((selected-difference emerge-current-difference)
	  (n (max emerge-current-difference 1)))
      (while (< n emerge-number-of-differences)
	(let ((diff-vector (aref emerge-difference-list n)))
	  (if (eq (aref diff-vector 6) 'default-B)
	      (progn
		(emerge-unselect-and-select-difference n)
		(emerge-select-A)
		(aset diff-vector 6 'default-A))))
	(setq n (1+ n))
	(if (= (* (/ n 10) 10) n)
	    (message "Setting default to A...%d" n)))
      (emerge-unselect-and-select-difference selected-difference)))
  (message ""))

(defun emerge-default-B ()
  "Selects the B variant for all differences from here down in the buffer
which are still defaulted, i.e., which the user has not selected and for
which there is no preference."
  (interactive)
  (let ((buffer-read-only nil))
    (let ((selected-difference emerge-current-difference)
	  (n (max emerge-current-difference 1)))
      (while (< n emerge-number-of-differences)
	(let ((diff-vector (aref emerge-difference-list n)))
	  (if (eq (aref diff-vector 6) 'default-A)
	      (progn
		(emerge-unselect-and-select-difference n)
		(emerge-select-B)
		(aset diff-vector 6 'default-B))))
	(setq n (1+ n))
	(if (= (* (/ n 10) 10) n)
	    (message "Setting default to A...%d" n)))
      (emerge-unselect-and-select-difference selected-difference)))
  (message ""))

(defun emerge-fast-mode ()
  "Set fast mode, in which ordinary Emacs commands are disabled, and Emerge
commands are not prefixed with C-c."
  (interactive)
  (setq buffer-read-only t)
  (use-local-map emerge-fast-keymap)
  (setq emerge-mode t)
  (setq emerge-fast-mode t)
  (setq emerge-edit-mode nil)
  ;; force mode line redisplay
  (set-buffer-modified-p (buffer-modified-p)))

(defun emerge-edit-mode ()
  "Set edit mode, in which ordinary Emacs commands are available, and Emerge
commands must be prefixed with C-c."
  (interactive)
  (setq buffer-read-only nil)
  (use-local-map emerge-edit-keymap)
  (setq emerge-mode t)
  (setq emerge-fast-mode nil)
  (setq emerge-edit-mode t)
  ;; force mode line redisplay
  (set-buffer-modified-p (buffer-modified-p)))

(defun emerge-set-auto-advance (arg)
  "Toggle auto-advance mode, which causes  emerge-select-A  and
 emerge-select-B  to automatically advance to the next difference.  (See
emerge-auto-advance.)  
If a positive ARGUMENT is given, it turns on auto-advance mode.
If a negative ARGUMENT is given, it turns off auto-advance mode."
  (interactive "P")
  (setq emerge-auto-advance (if (null arg)
				(not emerge-auto-advance)
			      (> (prefix-numeric-value arg) 0)))
  ;; force mode line redisplay
  (set-buffer-modified-p (buffer-modified-p)))

(defun emerge-set-skip-prefers (arg)
  "Toggle skip-prefers mode, which causes  emerge-next-difference  and
 emerge-previous-difference  to automatically skip over differences for which
there is a preference.  (See emerge-skip-prefers.)  
If a positive ARGUMENT is given, it turns on skip-prefers mode.
If a negative ARGUMENT is given, it turns off skip-prefers mode."
  (interactive "P")
  (setq emerge-skip-prefers (if (null arg)
				(not emerge-skip-prefers)
			      (> (prefix-numeric-value arg) 0)))
  ;; force mode line redisplay
  (set-buffer-modified-p (buffer-modified-p)))

;;; Support routines

;; Select a difference by placing the visual flags around the appropriate
;; group of lines in the A, B, and merge buffers
(defun emerge-select-difference (n)
  (let ((diff-vector (aref emerge-difference-list n)))
    (emerge-place-flags-in-buffer emerge-A-buffer
				  (aref diff-vector 0) (aref diff-vector 1))
    (emerge-place-flags-in-buffer emerge-B-buffer
				  (aref diff-vector 2) (aref diff-vector 3))
    (emerge-place-flags-in-buffer emerge-merge-buffer
				  (aref diff-vector 4) (aref diff-vector 5))))

(defun emerge-place-flags-in-buffer (buffer before after)
  (if (eq buffer emerge-merge-buffer)
      (emerge-place-flags-in-buffer1 buffer before after)
    (eval-in-buffer buffer
		    (emerge-place-flags-in-buffer1 buffer before after))))

(defun emerge-place-flags-in-buffer1 (buffer before after)
  (let ((buffer-read-only nil))
    ;; insert the flags
    (goto-char before)
    (insert-before-markers emerge-before-flag)
    (goto-char after)
    (insert emerge-after-flag)
    ;; put the markers into the flags, so alterations above or below won't move
    ;; them
    ;; before marker is one char before the end of the before flag
    ;; after marker is one char after the beginning of the after flag
    (set-marker before (1- before))
    (set-marker after (1+ after)))
  ;; Set both the buffer's point and its window's point to the beginning of
  ;; the difference.
  ;; Note that this has to be done outside of save-excursion, and without
  ;; changing the current buffer.
  (let ((current-buffer buffer))
    (goto-char (1+ before))
    (let ((window (get-buffer-window buffer)))
      (if window
	  (set-window-point window (1+ before))))))

;; Unselect a difference by removing the visual flags in the buffers.
(defun emerge-unselect-difference (n)
  (let ((diff-vector (aref emerge-difference-list n)))
    (emerge-remove-flags-in-buffer emerge-A-buffer
				   (aref diff-vector 0) (aref diff-vector 1))
    (emerge-remove-flags-in-buffer emerge-B-buffer
				   (aref diff-vector 2) (aref diff-vector 3))
    (emerge-remove-flags-in-buffer emerge-merge-buffer
				   (aref diff-vector 4) (aref diff-vector 5))))

(defun emerge-remove-flags-in-buffer (buffer before after)
  (eval-in-buffer
   buffer
   (let ((buffer-read-only nil))
     ;; put the markers at the beginning of the flags
     (set-marker before (- before (1- emerge-before-flag-length)))
     (set-marker after (1- after))
     ;; remove the flags
     (goto-char before)
     (if (looking-at emerge-before-flag-match)
	 (delete-char emerge-before-flag-length)
       ;; the flag isn't there
       (ding)
       (message "Trouble removing flag."))
     (goto-char after)
     (if (looking-at emerge-after-flag-match)
	 (delete-char emerge-after-flag-length)
       ;; the flag isn't there
       (ding)
       (message "Trouble removing flag.")))))

;; Select a difference, removing an flags that exist now.
(defun emerge-unselect-and-select-difference (n)
  (if (and (>= emerge-current-difference 0)
	   (< emerge-current-difference emerge-number-of-differences))
      (emerge-unselect-difference emerge-current-difference))
  (if (and (>= n 0) (< n emerge-number-of-differences))
      (progn
	(emerge-select-difference n)
	(let* ((diff-vector (aref emerge-difference-list n))
	       (selection-type (aref diff-vector 6)))
	  (if (eq selection-type 'default-A)
	      (aset diff-vector 6 'a)
	    (if (eq selection-type 'default-B)
		(aset diff-vector 6 'b))))))
  (setq emerge-current-difference n)
  (emerge-refresh-mode-line))

;; Revise the mode line to display which difference we have selected
(defun emerge-refresh-mode-line ()
  (setq mode-line-buffer-identification
	(list (format "Emerge: %%b   diff %d of %d%s"
		      (1+ emerge-current-difference)
		      emerge-number-of-differences
		      (if (and (>= emerge-current-difference 0)
			       (< emerge-current-difference
				  emerge-number-of-differences))
			  (cdr (assq (aref (aref emerge-difference-list
						 emerge-current-difference)
					   6)
				     '((a . " - A")
				       (b . " - B")
				       (prefer-A . " - A*")
				       (prefer-B . " - B*"))))
			""))))
  ;; Force mode-line redisplay
  (set-buffer-modified-p (buffer-modified-p)))

;; compare two regions in two buffers for containing the same text
(defun emerge-compare-buffers (buffer-x x-begin x-end buffer-y y-begin y-end)
  ;; first check that the two regions are the same length
  (if (not (and (= (- x-end x-begin) (- y-end y-begin))))
      nil
    (catch 'exit
      (while (< x-begin x-end)
	;; bite off and compare no more than 1000 characters at a time
	(let* ((compare-length (min (- x-end x-begin) 1000))
	       (x-string (eval-in-buffer 
			  buffer-x
			  (buffer-substring x-begin
					    (+ x-begin compare-length))))
	       (y-string (eval-in-buffer
			  buffer-y
			  (buffer-substring y-begin
					    (+ y-begin compare-length)))))
	  (if (not (string-equal x-string y-string))
	      (throw 'exit nil)
	    (setq x-begin (+ x-begin compare-length))
	    (setq y-begin (+ y-begin compare-length)))))
      t)))

;; Construct a unique buffer name.
;; The first one tried is prefixsuffix, then prefix<2>suffix, 
;; prefix<3>suffix, etc.
(defun emerge-unique-buffer-name (prefix suffix)
  (if (null (get-buffer (concat prefix suffix)))
      (concat prefix suffix)
    (let ((n 2))
      (while (get-buffer (format "%s<%d>%s" prefix n suffix))
	(setq n (1+ n)))
      (format "%s<%d>%s" prefix n suffix))))

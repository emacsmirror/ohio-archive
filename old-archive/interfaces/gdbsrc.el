; From: ayers@ASC.SLB.COM
; Subject: Gdbsrc - GNU gdb extension.
; Date: 13 Nov 90 05:30:34 GMT
; Organization: GNUs Not Usenet
; 
; 
; Until release 19.....
; 
;  GDBSRC::
;  Gdbsrc extends the emacs GDB interface to accept gdb commands issued
;  from the source code buffer.  Gdbsrc behaves similar to gdb except
;  now most debugging may be done from the source code using the *gdb*
;  buffer to view output. Supports a point and click model under X to
;  evaluate source code expressions (no more typing long variable names).
;  And more.. makes debugging fun! :-)
; 
; 
; We have been using it here for about 5 months and really like it.
; Supports C source at the moment but C++ support will be added if there
; is sufficient interest.
; 
; 
;-----------------------------------------------------------
;; Run gdbsrc under GNU Emacs (Mouse supported under X11R4)
;; 1990 Debby Ayers ayers@asc.slb.com, Rich Schaefer schaefer@asc.slb.com
;; Schlumberger, Austin, Tx 

;; This file is not part of the GNU Emacs distribution.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; this file, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; GDBSRC::Gdb Source Mode Interface description.
;; Gdbsrc extends the emacs GDB interface to accept gdb commands issued
;; from the source code buffer. Gdbsrc behaves similar to gdb except now all 
;; debugging may be done from the currently focused source buffer using 
;; the *gdb* buffer to view output.

;; When source files are displayed through gdbsrc, buffers are put in 
;; gdb-src-mode minor mode. This mode puts the buffer in read-only state
;; and sets up a special key and mouse map to invoke communication with
;; the current gdb process. The minor mode may be toggled on/off as needed.
;; (ESC-T) 

;; C-expressions may be evaluated by gdbsrc by simply pointing at text in the
;; current source buffer with the mouse or by centering the cursor over text
;; and typing a single key command. ('p' for print, '*' for print *).

;; As code is debugged and new buffers are displayed, the focus of gdbsrc
;; follows to each new source buffer. Makes debugging fun. (sound like a
;; commercial or what!)
;; 
;; To start:
;; Copy this file into gdbsrc.el
;; Compile if desired and load from .emacs
;; Issue "gdbsrc" from M-x command line.
;; Proceed with debugging as usual until source code is displayed.
;; Once source is displayed , go to source buffer and start debugging.
;; At any time you may issue a "C-h C-b" to see the current key
;; bindings.(help-for-help at M-x)
;; Toggle mode on/off to edit text. (ESC-T)

;; All key-bindings may be redefined. 
;; Local Bindings for Gdb-Src minor mode
;; Buffer is placed in READ-ONLY mode.

;; Current Listing ::
;;key		binding					Comment
;;---		-------					-------
;;
;; r               gdb-return-from-src	GDB return command
;; n               gdb-next-from-src	GDB next command
;; b               gdb-back-from-src	GDB back command
;; w               gdb-where-from-src	GDB where command
;; f               gdb-finish-from-src	GDB finish command
;; u               gdb-up-from-src      GDB up command
;; d               gdb-down-from-src	GDB down command
;; c               gdb-cont-from-src	GDB continue command
;; i               gdb-stepi-from-src	GDB step instruction command
;; s               gdb-step-from-src	GDB step command
;; ?               gdb-whatis-c-sexp	GDB whatis command for data at
;;					     buffer point
;; x               gdb-src-delete        GDB Delete all breakpoints if no arg
;;					     given or delete arg (C-u arg x)
;; m               gdb-src-frame         GDB Display current frame if no arg,
;;					     given or display frame arg
;; *               gdb-*print-c-sexp	GDB print * command for data at
;;					       buffer point
;; !               goto-gdb		Goto the GDB output buffer
;; p               gdb-print-c-sexp	GDB print * command for data at
;;					     buffer point
;; g               goto-gdb		Goto the GDB output buffer
;; t               gdb-src-mode		Toggles Gdb-Src mode (turns it off)
;; 
;; C-c C-f         gdb-finish-from-src	GDB finish command
;; 
;; C-x SPC         gdb-break		Set break for line with point
;; ESC t           gdb-src-mode		Toggle Gdb-Src mode
;;
;; X11/R4 Mouse Support
;; Click: 
;;
;;  left will set point (as always)
;;  shift-left will grab c-sexp and send to gdb to print
;;  shift-control-left will grab c-sexp and send to gdb to print *
;; 
;;  middle will paste last cut at point
;;  shift-middle will send gdb the currently marked region to print...
;;  shift-control-middle will send gdb the currently marked region to print *
;;
;;  right button will copy from point to mouse into the cut buffer...
;;  shift-right button will send currently marked region to GDB as a break point
;;  shift-control-middle will issue a continue and break when point is reached
;;
;; Local Bindings for buffer when you exit Gdb-Src minor mode
;;
;; C-x SPC         gdb-break		Set break for line with point
;; ESC t           gdb-src-mode		Toggle Gdb-Src mode
;;
;; Please send bug reports, modifications, and or comments to:
;; ayers@asc.slb.com  or schaefer@asc.slb.com

(provide 'gdbsrc)

(defvar gdb-src-mode nil
" Indicates whether buffer is in gdb-src-mode or not")

(defvar gdb-src-window nil
" Contains the window name of the current gdb source file")

(defvar gdb-src-active-p t
" Set to nil if you do not want source files put in gdb-src-mode")

(defun setup-gdb-src-window (window)
" Called from gdb-display-line to put the next source file
  in gdb-src-mode and save the window name"
  (setq gdb-src-window window)
  (and gdb-src-active-p (not gdb-src-mode )(gdb-src-mode 1))
)

(defvar gdb-src-call-p nil
" True if gdb command issued from a source buffer")

(defvar gdb-associated-buffer nil
  "Buffer name of attached gdb process")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gdbsrc command.
;;
;;
(defun gdbsrc (path)
  " Activates a gdb session with  gdb-src-mode turned on."
  (interactive "FRun gdb on file: ")
  (gdb path)
  (setq gdb-src-window nil)
  (setq gdb-src-active-p 1)
  (setq gdb-src-call-p nil)
  (setq gdb-src-mode nil)
  (set-process-filter 
   (get-buffer-process current-gdb-buffer) 'gdb-src-mode-filter)
  (set-process-sentinel 
   (get-buffer-process current-gdb-buffer) 'gdb-src-mode-sentinel)
  (fset (intern "gdb-display-line") 'gdb-src-display-line)
  (if (featurep 'x-mouse) (setup-gdb-mouse))
  (message "Gbd source mode active"))
 
(defvar gdb-src-mode-map nil
  "Keymap for gdb-src-mode")

(if gdb-src-mode-map 
    nil
  (setq gdb-src-mode-map (copy-keymap c-mode-map))
  ;;Keys
  (define-key gdb-src-mode-map "t" 'gdb-src-mode)
  (define-key gdb-src-mode-map "g" 'goto-gdb)
  (define-key gdb-src-mode-map "p" 'gdb-print-c-sexp)
  (define-key gdb-src-mode-map "!" 'goto-gdb)
  (define-key gdb-src-mode-map "*" 'gdb-*print-c-sexp)
  (define-key gdb-src-mode-map "?" 'gdb-whatis-c-sexp)
  (define-key ctl-x-map " " 'gdb-break)
  (define-key esc-map "t" 'gdb-src-mode)

  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; There are two ways to set up new keyboard commands for gdbsrc.
;;
;; Use the macro def-gdb-from-src (similiar to def-gdb of gdb.el).
;; This macro generates a function and a define key for you.
;;
;; Ex. (def-gdb-from-src "name-of-command" "key-to-bind-to" "Documentation")
;;
;; The limitation here is "name-of-command" has to be a command gdb knows
;; about like "break" etc
;;
;; For more sophisticated interaction, write your function and call
;; (gdb-call-from-src command-string) where command-string is some
;; command and its arguments that you have generated somehow. Look at
;; gdb-src-print-csexp for an example. Once you have written this function
;; use a (define-key gdb-src-mode-map "key" 'function) to put it in the 
;; gdbsrc keymap.
;;
; Use this macro to setup other gdb commands.
; 
(defmacro def-gdb-from-src (name key &optional doc)
  (let* ((fun (intern (format "gdb-%s-from-src" name)))
	 (cstr (list 'if '(not (= 1 arg))
		     (list 'format "%s %s" name 'arg)
		     name)))
    (list 'progn
 	  (list 'defun fun '(arg)
		(or doc "")
		'(interactive "p")
		(list 'gdb-call-from-src cstr))
	  (list 'define-key 'gdb-src-mode-map key  (list 'quote fun)))))

(def-gdb-from-src "finish" "\C-c\C-f" "Finish executing current function")
(def-gdb-from-src "step"  "s" "Step one instruction in src")
(def-gdb-from-src "stepi" "i" "Step one source line (skip functions)")
(def-gdb-from-src "cont"  "c" "Continue with display")
(def-gdb-from-src "down"  "d" "Go down N stack frames (numeric arg) ")
(def-gdb-from-src "up"    "u" "Go up N stack frames (numeric arg) with display")
(def-gdb-from-src "finish" "f" "Finish frame")
(def-gdb-from-src "where" "w" "Finish frame")
(def-gdb-from-src "back"  "b" "Display Backtrace")
(def-gdb-from-src "next"  "n" "Step one line with display")
(def-gdb-from-src "return" "r" "Return from selected stack frame")
(def-gdb-from-src "delete" "x" "Delete all breakpoints")
(def-gdb-from-src "frame" "m" "Show frame if noarg, with arg go to frame")
;;
;; Setup Mouse Bindings for Gdbsrc mode.
;;
(defun setup-gdb-mouse ()
  (interactive)

  ;; shift-left will grab c-sexp and send to gdb to print
  (define-key mouse-map x-button-s-left 'x-gdb-print-csexp)

  ;; shift-control-left will grab c-sexp and send to gdb to print * 
  (define-key mouse-map x-button-c-s-left 'x-gdb-*print-csexp)

  ;; shift-middle will send gdb the currently marked region to print...
  (define-key mouse-map x-button-s-middle 'x-gdb-print-region)

  ;; shift-control-middle will send gdb the currently mark region to print *
  (define-key mouse-map x-button-c-s-middle 'x-gdb-*print-region)

  ;; left button will set point
  (define-key mouse-map x-button-left 'x-mouse-set-point)

  ;; middle button will paste last cut at point
  (define-key mouse-map x-button-middle 'x-paste-text)

  ;; right button will copy from point to here into cut buffer...
  (define-key mouse-map x-button-right 'x-cut-marked-region)

  ;; shift-right will send gdb a "break" at whatever is in the cut-buffer
  (define-key mouse-map x-button-s-right 'x-gdb-break-region)

 ;; shift-right will send gdb a "break" at whatever is in the cut-buffer
  (define-key mouse-map x-button-c-s-right ' x-gdb-continue-until-point))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gdb Source minor mode.
;;;
;;; gdb-src-mode(arg)
;;;
;;; Gdb-src-mode defines the following buffer specific variables:
;;; 
;;; gdb-src-mode:            The current status of gdb-src-mode
;;; gdb-associated-buffer:   The gdb buffer to send commands to.
;;; gdb-src-mode-map-before: The Keymap before entering src mode
;;; gdb-initial-readonly:    Initial readonly status
;;;
(defun gdb-src-mode (arg)
"Minor mode for interacting with gdb from a c source file.
With arg, turn gdb-src-mode on iff arg is positive.  In gdb-src-mode,
you may send an associated gdb buffer commands from the current buffer
containing c source code."
  (interactive "P")
  (make-local-variable 'gdb-src-mode)
  (make-local-variable 'gdb-associated-buffer)
  (make-local-variable 'gdb-src-mode-map-before)
  (make-local-variable 'gdb-initial-readonly)
  (setq gdb-src-mode
	(if (null arg)
	    (not gdb-src-mode)
	  (> (prefix-numeric-value arg) 0)))

  (if gdb-src-mode
      (progn
	; inherit global values
	(or (assq 'gdb-src-mode minor-mode-alist)
	    (setq minor-mode-alist
		  (cons '(gdb-src-mode "-Gdb-Src")
			minor-mode-alist)))
	(if gdb-associated-buffer nil
	  (progn
	    (setq gdb-initial-readonly buffer-read-only)
	    (setq gdb-associated-buffer current-gdb-buffer))
	  )
	(setq buffer-read-only t)
	(setq gdb-src-mode-map-before (current-local-map))
	(use-local-map gdb-src-mode-map)
	(message "Enter gdb-src-mode.")
	)
    (progn
      (use-local-map gdb-src-mode-map-before)
      (setq buffer-read-only gdb-initial-readonly)
      (message "Exit gdb-src-mode.")
      )
    )
  
  ;; Update mode-line by setting buffer-modified to itself.
  (set-buffer-modified-p (buffer-modified-p))
  )

;;
;; Sends commands to gdb process.

(defun gdb-call-from-src (command)
  "Send associated gdb process COMMAND displaying source in this window."
  (setq gdb-src-call-p t)
  (pop-to-buffer (or gdb-associated-buffer current-gdb-buffer))
  (goto-char (dot-max))
  (beginning-of-line)
; Go past gdb prompt 
  (re-search-forward
	 shell-prompt-pattern (save-excursion (end-of-line) (point))  t)
; Delete any not-supposed-to-be-there text
  (delete-region (point) (dot-max)) 
  (insert-string command)
  (shell-send-input))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define Commands for GDB SRC Mode Buffer
;;;
;;;
;;;


;;; Set this variable to a valid format string
;;; to print c-sexps in a different way (hex,octal, etc)

(defvar gdb-print-format "")

(defun gdb-print-c-sexp ()
  "Find the nearest c-mode sexp. Send it to gdb with print command."
  (interactive)
  (let* ((tag (find-c-sexp))
	(command (concat "print " gdb-print-format tag)))
    (gdb-call-from-src command))
)
    

(defun gdb-*print-c-sexp ()
  "Find the nearest c-mode sexp. Send it to gdb with the print * command."
  (interactive)
  (let* ((tag (find-c-sexp))
	(command (concat "print " gdb-print-format "*"  tag)))
    (gdb-call-from-src  command))
)
 
(defun gdb-whatis-c-sexp ()
  "Find the nearest c-mode sexp. Send it to gdb with the whatis command. "
  (interactive)
  (let* ((tag (find-c-sexp))
	 (command (concat "whatis " tag)))
    (gdb-call-from-src command))
  )

(defun goto-gdb ()
  (interactive)
  (bury-buffer (current-buffer))
  (switch-to-buffer (or gdb-associated-buffer current-gdb-buffer))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  The following functions are used to extract the closest surrounding
;;;  c expression from point
;;;
;;;
(defun back-sexp ()
  "Version of backward-sexp that catches errors"
  (condition-case nil
      (backward-sexp)
    (error t)))

(defun forw-sexp ()
  "Version of forward-sexp that catches errors"
  (condition-case nil
     (forward-sexp)
    (error t)))

(defun sexp-compound-sep (span-start span-end)
  " Returns '.' for '->' & '.', returns ' ' for white space,
    returns '?' for other puctuation"  
  (let ((result ? )
	(syntax))
    (while (< span-start span-end)
      (setq syntax (char-syntax (char-after span-start)))
      (cond
       ((= syntax ? ) t)
       ((= syntax ?.) (setq syntax (char-after span-start))
	(cond 
	 ((= syntax ?.) (setq result ?.))
	 ((and (= syntax ?-) (= (char-after (+ span-start 1)) ?>))
	  (setq result ?.)
	  (setq span-start (+ span-start 1)))
	 (t (setq span-start span-end)
	    (setq result ??)))))
      (setq span-start (+ span-start 1)))
    result 
    )
  )

(defun sexp-compound (first second)
  "Returns non-nil if the concatenation of two S-EXPs result in a Single C 
token. The two S-EXPs are represented as a cons cells, where the car 
specifies the point in the current buffer that marks the begging of the 
S-EXP and the cdr specifies the character after the end of the S-EXP
Link S-Exps of the form:
      Sexp -> SexpC
      Sexp . Sexp
      Sexp (Sexp)        Maybe exclude if first Sexp is: if, while, do, for, switch
      Sexp [Sexp]
      (Sexp) Sexp
      [Sexp] Sexp"
  (let ((span-start (cdr first))
	(span-end (car second))
	(syntax))
    (setq syntax (sexp-compound-sep span-start span-end))
    (cond
     ((= (car first) (car second)) nil)
     ((= (cdr first) (cdr second)) nil)
     ((= syntax ?.) t)
     ((= syntax ? )
	 (setq span-start (char-after (- span-start 1)))
	 (setq span-end (char-after span-end))
	 (cond
	  ((= span-start ?) ) t )
	  ((= span-start ?] ) t )
          ((= span-end ?( ) t )
	  ((= span-end ?[ ) t )
	  (t nil))
	 )
     (t nil))
    )
  )

(defun sexp-cur ()
  "Returns the  S-EXP that Point is a member, Point is set to begging of S-EXP.
The S-EXPs is represented as a cons cell, where the car specifies the point in
the current buffer that marks the begging of the S-EXP and the cdr specifies 
the character after the end of the S-EXP"
  (let ((p (point)) (begin) (end))
    (back-sexp)
    (setq begin (point))
    (forw-sexp)
    (setq end (point))
    (if (>= p end) 
	(progn
	 (setq begin p)
	 (goto-char p)
	 (forw-sexp)
	 (setq end (point))
	 )
      )
    (goto-char begin)
    (cons begin end)
    )
  )

(defun sexp-prev ()
  "Returns the previous S-EXP, Point is set to begging of that S-EXP.
The S-EXPs is represented as a cons cell, where the car specifies the point in
the current buffer that marks the begging of the S-EXP and the cdr specifies 
the character after the end of the S-EXP"
  (let ((begin) (end))
    (back-sexp)
    (setq begin (point))
    (forw-sexp)
    (setq end (point))
    (goto-char begin)
    (cons begin end))
)

(defun sexp-next ()
  "Returns the following S-EXP, Point is set to begging of that S-EXP.
The S-EXPs is represented as a cons cell, where the car specifies the point in
the current buffer that marks the begging of the S-EXP and the cdr specifies 
the character after the end of the S-EXP"
  (let ((begin) (end))
    (forw-sexp)
    (forw-sexp)
    (setq end (point))
    (back-sexp)
    (setq begin (point))
    (cons begin end)
    )
  )

(defun find-c-sexp ()
  "Returns the Complex  S-EXP that surrounds Point"
  (interactive)
  (save-excursion
    (let ((p) (sexp) (test-sexp))
      (setq p (point))
      (setq sexp (sexp-cur))
      (setq test-sexp (sexp-prev))
      (while (sexp-compound test-sexp sexp)
	(setq sexp (cons (car test-sexp) (cdr sexp)))
	(goto-char (car sexp))
	(setq test-sexp (sexp-prev))
	)
      (goto-char p)
      (setq test-sexp (sexp-next))
      (while (sexp-compound sexp test-sexp)
	(setq sexp (cons (car sexp) (cdr test-sexp)))
	(setq test-sexp (sexp-next))
	)
      (buffer-substring (car sexp) (cdr sexp))
      )
    )
  )

;;;  Mouse support for gdbsrc mode modified from existing mouse code.
;;;
;;;

(defun x-gdb-steal-csexp (arg) 
  "Copies the containing s-expression located at the mouse cursor to point."
  (let (relative-coordinate rel-x rel-y the-sexp)
    (save-window-excursion ; don't forget what window we're in
      (save-excursion      ; or our position in the window
      	;;
      	;; get the position of the mouse click...
	 (setq relative-coordinate (x-mouse-select arg))
	 (setq rel-x (car relative-coordinate))
	 (setq rel-y (car (cdr relative-coordinate)))
	 ;;
	 (if relative-coordinate 
	     (progn
	     	;;
	     	;; move to the position of the mouse click
	     	;; and grab the sexpression...
	       (move-to-window-line rel-y)
	       (move-to-column (+ rel-x (current-column)))
	       (setq the-sexp (find-c-sexp))))))))
  
(defun x-gdb-print-csexp (arg)
  (gdb-call-from-src
	 (concat "print "  gdb-print-format (x-gdb-steal-csexp arg))))

(defun x-gdb-*print-csexp (arg)
  (gdb-call-from-src
	 (concat "print *"  gdb-print-format (x-gdb-steal-csexp arg))))

(defun x-gdb-print-region (arg)
  (let (( command  (concat "print " gdb-print-format (x-get-cut-buffer))))
    (gdb-call-from-src command)))

(defun x-gdb-*print-region (arg)
  (let (( command  (concat "print *" gdb-print-format (x-get-cut-buffer))))
    (gdb-call-from-src command)))

(defun x-gdb-break-region (arg)
 (let (( command  (concat "break " (x-get-cut-buffer))))
    (gdb-call-from-src command)))

(defun x-gdb-continue-until-point (arg)
  (if (coordinates-in-window-p arg (selected-window))
      (progn
	(x-mouse-set-point arg)
	(gdb-call-from-src (concat "break " (1+ (count-lines 1 (point)))))
	(gdb-call-from-src "c"))))
	
(defun x-cut-marked-region (arg)
  (if (coordinates-in-window-p arg (selected-window))
      (progn
	(save-excursion
	  (x-mouse-set-mark arg)
	  (let ((beg (point))
		(end (mark)))
	    (x-store-cut-buffer (buffer-substring beg end))
	    (copy-region-as-kill beg end)))
	)
    (message "Mouse not in selected window"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions extended from gdb.el for gdbsrc.
;;
;; gdb-src-set-buffer - added a check to set buffer to gdb-associated-buffer
;;                  to handle multiple gdb sessions being driven from src
;;                  files.
;; gdb-src-display-line - added call to setup-gdb-src-window to put
;;                  source buffer in gdb-src-mode and save away window name.
;;
;; gdb-src-filter - scans for gdb prompt to know when to change windows.
;;                  Calls gdb-filter.
;;
;; gdb-src-sentinel - resets the gdb-associated-buffer to nil when 
;;                  process exits. Calls gdb-sentinel.
;;
(defun gdb-set-buffer ()
  "New gdb-set-buffer to allow gdb commands to be
   called from other buffers besides current-gdb-buffer"
  (cond ((eq major-mode 'gdb-mode)
	(setq current-gdb-buffer (current-buffer)))
	(gdb-src-call-p (setq current-gdb-buffer gdb-associated-buffer))))

(defun gdb-src-display-line (true-file line)
  (let* ((buffer (find-file-noselect true-file))
	 (window (display-buffer buffer t))
	 (pos))
    (save-excursion
      (set-buffer buffer)
      (setup-gdb-src-window window)
;      (message (buffer-name buffer))
      (save-restriction
	(widen)
	(goto-line line)
	(setq pos (point))
	(setq overlay-arrow-string "=>")
	(or overlay-arrow-position
	    (setq overlay-arrow-position (make-marker)))
	(set-marker overlay-arrow-position (point) (current-buffer)))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos))))
    (set-window-point window overlay-arrow-position )))

(defun gdb-src-mode-filter (proc string)
   (let ((prompt (string-match gdb-prompt-pattern string)))
     (gdb-filter proc string)
     (if prompt 
	 (and gdb-src-call-p gdb-src-window
	      (select-window gdb-src-window)
	      (setq gdb-src-call-p nil))))
)

(defun gdb-src-mode-sentinel (proc msg)
  (setq gdb-associated-buffer nil)
  (let ((buffers (buffer-list)))
    (mapcar (function (lambda (buffer) 
			(set-buffer buffer)
			(if (eql gdb-associated-buffer current-gdb-buffer)
			    (kill-local-variable 'gdb-associated-buffer))))
	    buffers))
  (gdb-sentinel proc msg)
  (message "Gdbsrc finished"))


(defun goto-gdb ()
  (interactive)
  (bury-buffer (current-buffer))
  (and current-gdb-buffer (switch-to-buffer current-gdb-buffer))
)

;;			          ---Deb
;;
;;
;;
;;
;;
;; Debra L. Ayers			  Internet: ayers@asc.slb.com    
;;	                      UUCP    : cs.utexas.edu!asc.slb.com!ayers
;;						  Phone   : (512) 331-3274

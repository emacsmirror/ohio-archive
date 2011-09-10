;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!ucbvax!spider.co.uk!briant Tue Mar 27 09:05:56 1990
;Article 1612 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!tut.cis.ohio-state.edu!ucbvax!spider.co.uk!briant
;From briant@spider.co.uk (Brian Tompsett)
;Newsgroups: comp.emacs
;Subject: transparent.el, a transparent process window.
;Message-ID: <9003211610.AA05397@orbweb.spider.co.uk>
;Date: 21 Mar 90 16:10:37 GMT
;Sender: daemon@ucbvax.BERKELEY.EDU
;Lines: 611
;
;
;  In reponse to several requests for terminal emulators and such, and ways
;of running programs under emacs that use terminal control, I have created
;the following  piece of elisp by cloning what is already in GNU Emacs 18.55.
;
; You can use this code to rlogin/telnet to a remote site, or run vi or
;what ever strange purpose you have in mind!
;
; It was quicker to do it this way that write a complete vt100 interpreter
;in elisp. I'll do that another day. :-)
;
; I may also produce a complete vi interface CLONE (as a .vip file!) someday
;soon - stayed tuned.
;    Brian.
;--
;Brian Tompsett. Spider System Ltd. Tel: 031 554 9424 E-mail:briant@uk.co.spider
;Spider Park, Stanwell Street, Edinburgh, EH6 5NG. Fax: 031 554 0649
;(Secretary, BCS Edinburgh Branch, 53 Bonaly Crescent, Edinburgh. 031 441 2210)
;-------------------------------------------------------------------------------
;; Transparent window to a process for GNU Emacs
;;  Brian Tompsett, March 1990
;;      British Computer Society, Edinburgh.
;;        briant@spider.co.uk     bct@tardis.cs.ed.ac.uk
;;
;; Acknowledgement:
;;       This code is freely plaugarised from the terminal.el and vip.el
;;       packages of Emacs 18.55.
;;       terminal.el was written by Richard Mlynarik, November 1986.
;;       vip.el was written by Masahiko Sato (ms@sail.stanford.edu)

;; Copyright (C) 1986, 1987 Free Software Foundation, Inc.
;; Parts of this file are obtained from GNU Emacs code and therefore the
;; GNU License applies.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.
;;----------------------------------------------------------------------
;; Documentation:
;;
;;  This implements an apparently transparent emacs, almost as if we had
;;   done a fork/exec to the program we wished to run and the child process
;;   controls /dev/tty while emacs sleeps. This is not really what happens!
;;
;; It can be used to invoke programs that themselves wish to drive the
;; terminal directly by using terminal control codes (obtained from termcap
;; for example). 
;; All input and output is transparent and is not intercepted by emacs.
;; An emacs escape character has been provided so that
;; emacs commands can be invoked from within transparent windows.
;; The escape character defaults to C-^ unless changed by setting 
;; the variable terminal-escape-char.
;; Functions have been provided for logging output and stuffing input.
;;
;;  The main function is transparent-window. This can be invoked in two modes,
;;  either with the emacs display being scrolled up the screen with the cursor
;;  starting on the bottom left in the echo area, or with the emacs display 
;;  being cleared and the cursor starting in the home position.
;; The emacs screen is restored when the child process dies. There is an option
;; to wait for user input before restoring the emacs screen.
;;
;; transparent-window can be invoked interactively and it will prompt for 
;; arguments. It can also be called from lisp. An example, is given below:
;;
;; (defun ex-shell ()
;;   "ex shell"
;;  (require 'transparent)
;;   (setq buffer (current-buffer))
;;  (transparent-window (get-buffer-create "*transparent*")
;;		     ;; Default shell is same thing M-x shell uses.
;;		     (or explicit-shell-file-name
;;			 (getenv "ESHELL")
;;			 (getenv "SHELL")
;;			 "/bin/sh") nil t "[Hit return to continue] ")
;;  (set-buffer buffer) )
;;
;; problems:  Cannot set the correct values of <erase> <kill> and <intr>
;;            to what they were before emacs was invoked.
;;            I would need to be able to call reset_sys_modes and set_sys_modes
;;            from elisp. They are only C callable.
;;
;;            Assumes we are unix, and BSD to boot! Other systems will have
;;            problems with the assumtions about /bin/sh and stty!
;;            I'm sure someone can have a VMS version for us.
;;
;;            When in scrolling mode, and a clock running on a mode line,
;;            the clock does not know the mode line has scrolled up, and the
;;            display gets a little splattered.
;;
;;       

(provide 'transparent)

(defvar tr-scroll-off-emacs nil
  "If non-nil the emacs window is scrolled up the screen, otherwise
the screen is cleared" )

(defvar tr-wait-message nil
 "Message to be displayed when the widow process dies")

(defvar tr-saved-inverse-video nil
  "Where the old value of inverse video is stored")

(defvar tr-saved-mode-line nil
 "Where the old value of the mode line is held")

(defvar tr-process nil
 "Where is id of the process is held")

;; This code is from vip.el
  
(defmacro tr-loop (count body)
  "(COUNT BODY) Execute BODY COUNT times."
  (list 'let (list (list 'count count))
	(list 'while (list '> 'count 0)
	      body
	      (list 'setq 'count (list '1- 'count)))))

(defvar transparent-map nil)
(if transparent-map
    nil
  (let ((map (make-keymap)))
    (fillarray map 'tr-pass-through)
    (setq transparent-map map)))

(defvar tr-exit-map nil)
(if tr-exit-map
    nil
  (let ((map (make-keymap)))
    (fillarray map 'exit-minibuffer)
    (setq tr-exit-map map)))

;; escape to emacs mode temporarily

(defvar tr-emacs-local-map nil
  "Local map used in emacs mode. \(buffer specific\)")

(make-variable-buffer-local 'tr-emacs-local-map)

(defun tr-get-editor-command (l-map g-map &optional str)
  "Read characters from keyboard until an editor command is formed, using
local keymap L-MAP and global keymap G-MAP.  If the command is a
self-insert-command, the character just read is returned instead.  Optional
string STR is used as initial input string."
  (let (char l-bind g-bind)
    (setq char
	  (if (or (null str) (string= str ""))
	      (read-char)
	    (string-to-char str)))
    (setq last-command-char char)
    (setq l-bind (tr-binding-of char l-map))
    (if (null l-bind)
	;; since local binding is empty, we concentrate on global one.
	(progn
	  (setq g-bind (tr-binding-of char g-map))
	  (if (null g-bind)
	      nil ;; return nil, since both bindings are void.
	    (if (keymapp g-bind)
		(tr-get-editor-command nil g-bind (tr-string-tail str))
	      (if (eq g-bind 'self-insert-command) char g-bind))))
      ;; local binding is nonvoid
      (if (keymapp l-bind)
	  ;; since l-bind is a keymap, we consider g-bind as well.
	  (progn
	    (setq g-bind (tr-binding-of char g-map))
	    (if (null g-bind)
		(tr-get-editor-command l-bind nil (tr-string-tail str))
	      (if (keymapp g-bind)
		  ;; both bindings are keymap
		  (tr-get-editor-command l-bind g-bind (tr-string-tail str))
		;; l-bind is a keymap, so we neglect g-bind
		(tr-get-editor-command l-bind nil (tr-string-tail str)))))
	;; l-bind is a command
	(if (eq l-bind 'self-insert-command) char l-bind)))))

(defun tr-binding-of (char map)
  "Return key-binding of CHAR under keymap MAP.  It is nil if the binding
is void, or a command, or a keymap"
  (let ((val (if (listp map)
		 (cdr (assq char map))
	       (aref map char))))
    (cond ((null val) nil)
	  ((keymapp val)
	   (if (symbolp val) (symbol-function val) val))
	  (t
	   ;; otherwise, it is a function which is either a real function or
	   ;; a keymap fset to val.
	   (let ((fun (symbol-function val)))
	     (if (or (null fun) (keymapp fun)) fun val))))))

(defun tr-escape (arg &optional char)
  "Escape to emacs mode and execute one emacs command and then return.
  ARG is used as the prefix value for the executed command.  If
CHAR is given it becomes the first character of the command."
  (interactive "P")
  (let (com (buff (current-buffer)) (first t))
    (if char (setq unread-command-char char))
    (setq prefix-arg arg)
    (while (or first (>= unread-command-char 0))
      ;; this while loop is executed until unread command char will be
      ;; exhausted.
      (setq first nil)
      (setq com (tr-get-editor-command tr-emacs-local-map global-map))
      (if (numberp com)
	  (tr-loop (tr-p-val prefix-arg)
		    (insert (char-to-string com)))
	(command-execute com prefix-arg)))
    (setq prefix-arg nil)  ;; reset prefix arg
    ))


(defun tr-p-val (arg)
  "Get value part of prefix-argument ARG."
  (cond ((null arg) 1)
	((consp arg) (if (null (car arg)) 1 (car arg)))
	(t arg)))

(defun tr-string-tail (str)
  (if (or (null str) (string= str "")) nil
    (substring str 1)))

;; This code is from terminal.el

(defun tr-stuff-string (string)
  "Read a string to send to through the transparent window
as though that string had been typed on the keyboard.

Very poor man's file transfer protocol."
  (interactive "sStuff string: ")
  (process-send-string tr-process string))

(defun tr-set-output-log (name)
  "Record output from the transparent window in a buffer."
  (interactive (list (if tr-log-buffer
			 nil
		       (read-buffer "Record output in buffer: "
				    (format "%s output-log"
					    (buffer-name (current-buffer)))
				    nil))))
  (if (or (null name) (equal name ""))
      (progn (setq tr-log-buffer nil)
	     (message "Output logging off."))
    (if (get-buffer name)
	nil
      (save-excursion
	(set-buffer (get-buffer-create name))
	(fundamental-mode)
	(buffer-flush-undo (current-buffer))
	(erase-buffer)))
    (setq tr-log-buffer (get-buffer name))
    (message "Recording transparent window output into buffer \"%s\""
	     (buffer-name tr-log-buffer))))

(defun tr-tofu ()
  "Discontinue output log."
  (interactive)
  (tr-set-output-log nil))
  

(defun tr-toggle (sym arg)
  (set sym (cond ((not (numberp arg)) arg)
		 ((= arg 1) (not (symbol-value sym)))
		 ((< arg 0) nil)
		 (t t))))


(defun tr-pass-through ()
  "Send the last character typed through the transparent window
without any interpretation"
  (interactive)
  (if (eql last-input-char terminal-escape-char)
      (call-interactively 'tr-escape)
    (process-send-string tr-process (char-to-string last-input-char))
    (tr-process-output t)))

(defun tr-filter (process string)
  (let* ((obuf (current-buffer))
	 (m meta-flag))
    ;; can't use save-excursion, as that preserves point, which we don't want
    (unwind-protect
	(progn
	  (set-buffer (process-buffer process))
	  (goto-char tr-saved-point)
	  (and (bufferp tr-log-buffer)
	       (if (null (buffer-name tr-log-buffer))
		   ;; killed
		   (setq tr-log-buffer nil)
		 (set-buffer tr-log-buffer)
		 (goto-char (point-max))
		 (insert string)
		 (set-buffer (process-buffer process))))
	  (setq tr-pending-output (nconc tr-pending-output (list string)))
	  ;; this binding is needed because emacs looks at meta-flag when
	  ;;  the keystroke is read from the keyboard, not when it is about
	  ;;  to be fed into a keymap (or returned by read-char)
	  ;; There still could be some screws, though.
	  (let ((meta-flag m))
	    (tr-process-output (eq (current-buffer)
				   (window-buffer (selected-window)))))
	  (set-buffer (process-buffer process))
	  (setq tr-saved-point (point)))
      (set-buffer obuf))))

(defun tr-process-output (preemptable)
  ;;>> There seems no good reason to ever disallow preemption
  (setq preemptable t)
  (catch 'tr-process-output
    (let ((buffer-read-only nil)
	  (string nil) ostring start char (matchpos nil))
      (while (cdr tr-pending-output)
	(setq ostring string
	      start (car tr-pending-output)
	      string (car (cdr tr-pending-output))
	      char (aref string start))
	(if (eql (setq start (1+ start)) (length string))
	    (progn (setq tr-pending-output
			   (cons 0 (cdr (cdr tr-pending-output)))
			 start 0
			 string (car (cdr tr-pending-output)))
	    (setcar tr-pending-output start)))
	(if (null string) (send-string-to-terminal (char-to-string char))
	  (send-string-to-terminal string))
	(setq tr-pending-output (cons 0 (cdr (cdr tr-pending-output))))
	(and preemptable
	     (input-pending-p)
	     ;; preemptable output!  Oh my!!
	     (throw 'tr-process-output t)))))
  ;; We must update window-point in every window displaying our buffer
  (let* ((s (selected-window))
	 (w s))
    (while (not (eq s (setq w (next-window w))))
      (if (eq (window-buffer w) (current-buffer))
	  (set-window-point w (point))))))

(defun tr-get-char ()
  (if (cdr tr-pending-output)
      (let ((start (car tr-pending-output))
	    (string (car (cdr tr-pending-output))))
	(prog1 (aref string start)
	  (if (eql (setq start (1+ start)) (length string))
	      (setq tr-pending-output (cons 0 (cdr (cdr tr-pending-output))))
	      (setcar tr-pending-output start))))
    (catch 'char
      (let ((filter (process-filter tr-process)))
	(unwind-protect
	    (progn
	      (set-process-filter tr-process
				  (function (lambda (p s)
                                    (or (eql (length s) 1)
                                        (setq tr-pending-output (list 1 s)))
                                    (throw 'char (aref s 0)))))
	      (accept-process-output tr-process))
	  (set-process-filter tr-process filter))))))

(defun tr-sentinel (process message)
  (cond ((eq (process-status process) 'run))
	((null (buffer-name (process-buffer process)))) ;deleted
	(t (let ((b (current-buffer)))
	       (set-buffer (process-buffer process))
	       (setq buffer-read-only nil)
	       (fundamental-mode)
	       (setq mode-line-inverse-video tr-saved-inverse-video)
	       (setq mode-line-format tr-saved-mode-line)
	       (if tr-wait-message 
		   (send-string-to-terminal tr-wait-message))
               ;; If we are scrolling then we are stuck in a minibuffer
               ;; read. Put it out of its misery. No return.
	       (if tr-scroll-off-emacs
		   (progn
		     (if tr-wait-message
			  (condition-case nil 
			     (read-from-minibuffer "" nil tr-exit-map)))
		     (exit-minibuffer)))
	       (if tr-wait-message
		   (progn
		     (setq tr-saved-mini-map 
			      (copy-keymap minibuffer-local-map))
		     (condition-case nil 
			 (read-from-minibuffer "" nil tr-exit-map))
		     (setq minibuffer-local-map tr-saved-mini-map) ))
	       ;; Must kill buffer to delete process
	       (kill-buffer (current-buffer))
	       (redraw-display)))))

(defun tr-clear-screen ()
  ;; regenerate buffer to compensate for (nonexistent!!) bugs.
  (erase-buffer)
  (goto-char (point-min)))


(defvar tr-stty-string "stty -nl new dec echo"
  "*Command string (to be interpreted by \"sh\") which sets the modes
of the virtual terminal to be appropriate for interactive use.")

(defvar explicit-shell-file-name nil
  "*If non-nil, is file name to use for explicitly requested inferior shell.")

(defun transparent-window (buffer program args &optional scroll wait-message)
 "Create a transparent window in BUFFER, run PROGRAM on arguments ARGS.
ARGS is a list of argument-strings. SCROLL which if
non-nil scrolls the emacs display up the screen from the echo area, and if nil
clear the screen and homes the cursor. WAIT-MESSAGE if non-nil is a string
to be displayed, while awaiting any key-depression before exiting.
Any input typed when BUFFER is the current Emacs buffer is sent to that
program an keyboard input.

Interactively, BUFFER defaults to \"*transparent*\" and PROGRAM and ARGS
are parsed from an input-string using your usual shell.

To give commands to emacs (as opposed to the program running under it),
type Control-^.  This may be followed by an emacs command.

This escape character may be changed using the variable `terminal-escape-char'.

`Meta' characters may not currently be sent through the terminal emulator.

This function calls the value of terminal-mode-hook if that exists
and is non-nil after the terminal buffer has been set up and the
subprocess started.

The buffer (and the transparent window) are deleted when the command exits."

  (interactive
    (cons (save-excursion
	    (set-buffer (get-buffer-create "*transparent*"))
	    (buffer-name (if (or (not (boundp 'tr-process))
				 (null tr-process)
				 (not (eq (process-status tr-process)
					  'run)))
			     (current-buffer)
			   (generate-new-buffer "*transparent*"))))
	  (append
	    (let* ((default-s
		     ;; Default shell is same thing M-x shell uses.
		     (or explicit-shell-file-name
			 (getenv "ESHELL")
			 (getenv "SHELL")
			 "/bin/sh"))
		   (s (read-string
		       (format "Program to run: (default %s) "
			       default-s))))
	      (if (equal s "")
		  (list default-s '())
		(tr-parse-program-and-args s))))))
  (set-buffer buffer)
  (setq tr-scroll-off-emacs scroll)
  (setq tr-wait-message wait-message)
  (transparent-mode)
  (setq buffer-read-only nil)
  (let (process)
    (while (setq process (get-buffer-process (current-buffer)))
      (if (y-or-n-p (format "Kill process %s? " (process-name process)))
	  (delete-process process)
	(error "Process %s not killed" (process-name process)))))
  (condition-case err
      (progn
	(if (fboundp 'start-subprocess)
	    ;; this winning function would do everything, except that
	    ;;  rms doesn't want it.
	    (setq tr-process (start-subprocess "transparent"
			       program args
			       'channel-type 'terminal
			       'filter 'tr-filter
			       'buffer (current-buffer)
			       'sentinel 'tr-sentinel
			       'modify-environment
			         (list (cons "TERM" (getenv "TERM")))))
	  ;; so instead we resort to this...
	  (setq tr-process (start-process "transparent-window" (current-buffer)
			     "/bin/sh" "-c"
			     ;; Yuck!!! Start a shell to set some terminal
			     ;; control characteristics.  Then start the
			     ;; "env" program to setup the terminal type
			     ;; Then finally start the program we wanted.
			     (format "%s; exec %s TERM=%s %s"
                                     tr-stty-string
				     (tr-quote-arg-for-sh
				       (concat exec-directory "env"))
				     (getenv "TERM")
				     (mapconcat 'tr-quote-arg-for-sh
						(cons program args) " ")))))
	  (set-process-filter tr-process 'tr-filter)
	  (set-process-sentinel tr-process 'tr-sentinel))
    (error (fundamental-mode)
	   (signal (car err) (cdr err))))
  (setq inhibit-quit t)			;sport death
  (if tr-scroll-off-emacs 
      (save-excursion (set-buffer buffer)
		      (run-hooks 'terminal-mode-hook)
		      (setq tr-saved-recurse enable-recursive-minibuffers)
		      (setq enable-recursive-minibuffers t)
		      (setq tr-saved-mini-map 
			      (copy-keymap minibuffer-local-map))
		      ;; We hang in this read until process death.
		      (condition-case nil
			   (read-from-minibuffer "" nil transparent-map))
		      (setq minibuffer-local-map tr-saved-mini-map)
		      (setq enable-recursive-minibuffers tr-saved-recurse)
		      ;;(run-hooks 'tr-window-death-hook)
		      (kill-buffer buffer)
		      (redraw-display)
		      )
    ;; These are done for non-scrolling mode
    (switch-to-buffer buffer)
    (delete-other-windows)
    (tr-clear-screen)
    (setq tr-emacs-local-map (current-local-map))
    (use-local-map transparent-map)
    (run-hooks 'terminal-mode-hook)))

(defun tr-parse-program-and-args (s)
  (cond ((string-match "\\`\\([a-zA-Z0-9-+=_.@/:]+[ \t]*\\)+\\'" s)
	 (let ((l ()) (p 0))
	   (while p
	     (setq l (cons (if (string-match
				"\\([a-zA-Z0-9-+=_.@/:]+\\)\\([ \t]+\\)*"
				s p)
			       (prog1 (substring s p (match-end 1))
				 (setq p (match-end 0))
				 (if (eql p (length s)) (setq p nil)))
			       (prog1 (substring s p)
				 (setq p nil)))
			   l)))
	   (setq l (nreverse l))
	   (list (car l) (cdr l))))
	((and (string-match "[ \t]" s) (not (file-exists-p s)))
	 (list shell-file-name (list "-c" (concat "exec " s))))
	(t (list s ()))))

(put 'transparent-mode 'mode-class 'special)
;; This is only separated out from function transparent-window
;; to keep the latter a little more managable.
(defun transparent-mode ()
  "Set up variables for use of the transparent-window
One should not call this -- it is an internal function
of the transparent-window"
  (kill-all-local-variables)
  (buffer-flush-undo (current-buffer))
  (setq major-mode 'transparent-mode)
  (setq mode-name "transparent")
  (make-variable-buffer-local 'mode-line-format)
  (setq tr-saved-mode-line mode-line-format)
  (setq mode-line-format "")
  (make-variable-buffer-local 'mode-line-inverse-video)
  (setq tr-saved-inverse-video mode-line-inverse-video)
  (setq mode-line-inverse-video nil)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (make-local-variable 'terminal-escape-char)
  (setq terminal-escape-char (default-value 'terminal-escape-char))
  ;; If we are scrolling emacs off the top of the screen by reading
  ;; in the minibuffer, then we cannot make this a local variable!
  (if (not tr-scroll-off-emacs) (make-local-variable 'tr-process))
  (make-local-variable 'tr-pending-output)
  (setq tr-pending-output (list 0))
  (make-local-variable 'tr-saved-point)
  (setq tr-saved-point (point-min))
  (make-local-variable 'inhibit-quit)
  ;(setq inhibit-quit t)
  (make-local-variable 'tr-log-buffer)
  (setq tr-log-buffer nil)
  ;;>> Nothing can be done about this without decruftifying
  ;;>>  emacs keymaps.
  (make-local-variable 'meta-flag) ;sigh
  (setq meta-flag nil)
  ;(use-local-map transparent-mode-map)
  ;; terminal-mode-hook is called above in function terminal-emulator
  )




























































;;;; what a complete loss

(defun tr-quote-arg-for-sh (fuckme)
  (cond ((string-match "\\`[a-zA-Z0-9-+=_.@/:]+\\'"
		       fuckme)
	 fuckme)
	((not (string-match "[$]" fuckme))
	 ;; "[\"\\]" are special to sh and the lisp reader in the same way
	 (prin1-to-string fuckme))
	(t
	 (let ((harder "")
	       (cretin 0)
	       (stupid 0))
	   (while (cond ((>= cretin (length fuckme))
			 nil)
			;; this is the set of chars magic with "..." in `sh'
			((setq stupid (string-match "[\"\\$]"
						    fuckme cretin))
			 t)
			(t (setq harder (concat harder
						(substring fuckme cretin)))
			   nil))
	     (setq harder (concat harder (substring fuckme cretin stupid)
                                  ;; Can't use ?\\ since `concat'
                                  ;; unfortunately does prin1-to-string
                                  ;; on fixna.  Amazing.
				  "\\"
				  (substring fuckme
					     stupid
					     (1+ stupid)))
		   cretin (1+ stupid)))
	   (concat "\"" harder "\"")))))


;From ark1!nems!mimsy!haven!purdue!tut.cis.ohio-state.edu!spider.co.uk!briant Wed Mar 28 13:11:22 1990
;Article 1381 of gnu.emacs:
;Path: ark1!nems!mimsy!haven!purdue!tut.cis.ohio-state.edu!spider.co.uk!briant
;From briant@spider.co.uk (Brian Tompsett)
;Newsgroups: gnu.emacs
;Subject: Re: transparent.el, a transparent process window.
;Message-ID: <9003261352.AA04628@orbweb.spider.co.uk>
;Date: 26 Mar 90 13:52:14 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 16
;
;
; It has been pointed out to me that I inadvertantly left out a copyright notice
;from the file transparent.el. The code in terminal.el, which I used is 
;copyrighted, and the copyright should have been propogated into transparent.el
;which contained sections of code from terminal.el.
;
;Please add the following line to all copies of the file transparent.el.
;; Copyright (C) 1986, 1987 Free Software Foundation, Inc.

;   Thank you,
;     Brian.
;--
;Brian Tompsett. Spider System Ltd. Tel: 031 554 9424 E-mail:briant@uk.co.spider
;Spider Park, Stanwell Street, Edinburgh, EH6 5NG. Fax: 031 554 0649
;(Secretary, BCS Edinburgh Branch, 53 Bonaly Crescent, Edinburgh. 031 441 2210)
;(BCS = British Computer Society. NOT Boston Computer Society !!   :-)        )

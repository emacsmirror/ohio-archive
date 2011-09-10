;; Automatic garbage collection in the background.
;; Automatic logouts after being idle a certain period of time.
;;

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; auto-gc|Radey Shouman and Ivan Vazquez|rshouman@chpc.utexas.edu|
;; Garbage collection in the background.|
;; 18-Mar-1993|1.0|~/misc/auto-gc.el.Z|


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-gc.el     automatically garbage collect in the background when
;;                user is apparently resting.  Prevents emacs from
;;                sitting idle for hours, and then, just as you press
;;                the first key, saying "garbage collecting...".
;;
;;                This package also allows the user to have their emacs
;;                auto-logout.  I found this useful to use when dialed
;;                up from home so that my emacs will auto-logout and
;;                the terminal line will notice that it is idle and
;;                log me off, otherwise the time/load mode line
;;                changes keep the dialup line alive.
;;
;;                
;;
;; author:  Radey Shouman   (auto-gc)     rshouman@chpc.utexas.edu
;;          Ivan Vazquez   (auto-logout)  ivan@haldane.bu.edu
;;
;; filename: auto-gc.el
;; $modified: Fri Mar 19 22:15:34 1993 by rshouman $
;;
;; Installation:  byte-compile it,
;;
;; (require 'auto-gc)
;; (auto-gc 1)
;; (auto-logout 1)
;;       or
;; (autoload 'auto-gc "auto-gc"
;;    "Garbage collect when user isn't looking. " t)
;; (autoload 'auto-logout "auto-gc"
;;    "Kill emacs when user has been idle too long." t)
;;
;; Configuration:
;;  o If you don't want "gc " in your mode line, set the variable
;;    auto-gc-mode-string to ""
;;  o You can call additional functions when auto-gc-filter executes
;;    by setting the hook variables auto-gc-hooks, auto-gc-gc-hooks, 
;;    auto-gc-start-hooks, and auto-gc-dead-hooks.  Read the code to
;;    see where these are run.
;;  o Setting the variable auto-gc-display-time to t will cause
;;    functions to be added to these hooks to display the current time
;;    in the mode line.  Set this varible to nil if you don't want this.
;;  o To change how often automatic gc may be done, set the variable
;;    auto-gc-interval (default is 30 seconds).
;;  o To change the delay before automatic logout, set the variable
;;    auto-logout-time (default is 60 mins).
;;  o There is a hook variable, auto-logout-hook, for cleaning up before
;;    logout.  Read the code to see where this is run.
;;  o The variable auto-gc-quiet will be bound to executing-kbd-macro
;;    when automatic gc is done, the "garbage collecting..." message
;;    might be suppressed.  This is an undocumented feature of emacs,
;;    use at your own risk.  (Thanks to Joe Wells for posting this
;;    trick.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'auto-gc)

;;; auto-gc variables:
;;;

(defvar auto-gc nil
  "*If non-nil, auto-gc-filter will do automatic garbage-collection, if
nil, auto-gc-filter will not do garbage collection, or change the value
of this-command, but its process will continue to run. ")

(defvar auto-gc-interval 30
  "*How often to check to see whether any interactive commands have been
executed.  Automatic garbage collection should happen sometime between
auto-gc-interval and twice auto-gc-interval seconds after the last interactive
command.")

(defvar auto-gc-process nil
  "*Process used for automatic garbage collection. ")

(defvar auto-gc-mode-string " gc "
  "*String added to global mode line when automatic garbage collection is
enabled, setting this to \"\" will result in nothing being added to the 
global mode line. ")

(defvar auto-gc-hooks nil
  "*Function or list of functions to run every auto-gc-interval seconds. 
These functions are run whether or not auto-gc is non-nil. ")

(defvar auto-gc-gc-hooks nil
  "*Function or list of functions to run every time automatic garbage 
collection is done.  Put long, annoying processes here. ")

(defvar auto-gc-start-hooks nil
  "*Function or list of functions to run when the auto-gc is started. ")

(defvar auto-gc-dead-hooks nil
  "*Function or list of functions to run when the auto-gc process dies. ")

(defvar auto-gc-display-time t
  "*If non-nil, the auto-gc process will display the time, using
display-time-sentinel from the time library. ")

(defvar auto-gc-quiet nil
  "*This value will be bound to the variable executing-kbd-macro when
doing background garbage collection.  A non-nil value will, with some
versions of emacs, inhibit printing the message \"garbage collecting...\".
This is an undocumented feature of emacs, which may disappear, so use at
your own risk. ")

;;; auto-logout variables:
;;;

(defvar auto-logout nil
  "*If non-nil will automatically kill emacs after auto-logout-time
minutes.  (default is 60 minutes.)")

(defvar auto-logout-debug nil
  "*If non-nil will print debugging messages for auto-logout.")

(defvar auto-logout-time 60 ; One hour  
  "*Time in minutes that will log out this emacs.")

(defvar auto-logout-hook nil
  "*Function or list of functions that will be called upon the
auto-logout.  i.e. right before quitting.")
  
(defvar auto-logout-test
  '(lambda (time-idle) (>= time-idle auto-logout-time))
  "*Function which will return whether or not to logout.  
Gets passed one arg, time-idle in minutes.")

(defvar auto-logout-tics 0
  "Number of times that auto-gc has been called since the last
interactive-command.  One tic is auto-gc-interval seconds (default 30). ")

;;; auto-gc code:
;;;

(defun auto-gc-dead-display-time-hook ()
  (setq display-time-string ""))

(defun auto-gc-start-display-time-hook ()
  (or (memq 'display-time-string global-mode-string)
      (setq global-mode-string
	    (append global-mode-string '(display-time-string))))
  (setq display-time-string ""))

;;display-time-filter doesn't actually do anything with it's arguments.
(defun auto-gc-display-time-hook ()
  (display-time-filter proc string))

;; Do the right thing, be HOOKS  a symbol, a lambda expression, or a list.
;; Returns new value for hooks, containing the symbol more-hook.
;; Return value is always a list.
(defun auto-add-hook (hook-sym new-hook)
  (or (boundp hook-sym)
      (set hook-sym nil))
  (setq hooks (symbol-value hook-sym))
  (set hook-sym 
       (cond ((listp hooks)
	      (if (eq (car hooks) 'lambda)
		  (list hooks new-hook)
		(if (memq new-hook hooks)
		    hooks
		  (cons new-hook hooks))))
	     ((symbolp hooks)
	      (if (eq hooks new-hook)
		  hooks
		(list hooks new-hook)))
	     (t (error "can't add hook to %s" hooks)))))

(if auto-gc-display-time
    ;; Install display-time functions in auto-gc hooks.
    (progn
      (or (fboundp 'display-time-sentinel) ;time.el doesn't provide anything.
	  (load-library "time"))
      (auto-add-hook 'auto-gc-start-hooks 'auto-gc-start-display-time-hook)
      (auto-add-hook 'auto-gc-dead-hooks 'auto-gc-dead-display-time-hook)
      (auto-add-hook 'auto-gc-hooks 'auto-gc-display-time-hook)))

(defun auto-gc (&optional arg)
  "Toggle automatic background garbage collection.
With arg, turn  on iff arg is positive. "
  (interactive "P")
  (setq auto-gc
	(if (null arg) (not auto-gc)
	  (> (prefix-numeric-value arg) 0)))
  (or global-mode-string (setq global-mode-string '("")))
  (if (not auto-gc)
      (setq global-mode-string (delq 'auto-gc-mode-string global-mode-string))
    (if (and auto-gc-process
	     (eq (process-status auto-gc-process) 'run))
	(setq auto-gc t)
      (if auto-gc-process
	  (delete-process auto-gc-process))
      (let ((process-connection-type nil)) ;No need for a pty here.
	(setq auto-gc-process
	      (start-process "auto-gc" nil
			     (expand-file-name "wakeup" exec-directory)
			     (int-to-string auto-gc-interval))))
      (process-kill-without-query auto-gc-process)
      (set-process-sentinel auto-gc-process 'auto-gc-sentinel)
      (set-process-filter auto-gc-process 'auto-gc-filter)
      (run-hooks 'auto-gc-start-hooks))
    (or (memq 'auto-gc-mode-string global-mode-string)
	(setq global-mode-string
	      (append global-mode-string '(auto-gc-mode-string))))))
  
(defun auto-gc-sentinel (proc reason)
  (if (eq (process-status proc) 'run)
      nil
    (delq 'auto-gc-mode-string global-mode-string)
    (run-hooks 'auto-gc-dead-hooks)
    (delete-process proc)
    (setq auto-gc nil))
  ;; Force mode-line updates
  (save-excursion (set-buffer (other-buffer)))
  (set-buffer-modified-p (buffer-modified-p))
  (sit-for 0))

;; This function works by checking the value of the variable this-command,
;; setting it to 'auto-gc if that is not its value already.  this-command is
;; set to reflect the interactive command being executed when a command is
;; called, replacing whatever value it had before.  For interactive commands,
;; the value of this-command is copied to last-command when returning to the
;; top of the command loop, since this isn't an interactive command it doesn't
;; result in last-command being changed, hence it shouldn't interfere with
;; commands like yank-pop, that check the value of last-command.
(defun auto-gc-filter (proc string)
  (and (boundp 'auto-gc-debug) auto-gc-debug
       (message "this %s; last %s" this-command last-command))
  (run-hooks 'auto-gc-hooks)
  (if auto-gc
      (if (eq this-command 'auto-gc)
	  (if (= (minibuffer-depth) 0)
	      (progn
		(run-hooks 'auto-gc-gc-hooks)
		(let ((gc-cons-threshold 10000)
		      (executing-kbd-macro auto-gc-quiet))
		  (sit-for 0))))
	(setq this-command 'auto-gc))))
  
;;; auto-logout code:
;;;

(defun auto-logout (&optional arg)
  "Toggle doing auto-logouts.
Arg positive means on."
  (interactive "P")
  (setq auto-logout
	(if (null arg) (not auto-logout)
	    (> (prefix-numeric-value arg) 0)))
  (auto-gc 1)
  (auto-add-hook 'auto-gc-hooks 'auto-logout-hook-fn))

(defun auto-logout-hook-fn ()
  (if auto-logout
      (cond ((and (eq this-command 'auto-gc)
		  (= (minibuffer-depth) 0))
	     ;; Ok we're officially Idle.
	     (progn
	       (setq auto-logout-tics (1+ auto-logout-tics))
	       (and auto-logout-debug
		    (message "auto-logout-tics = %d" auto-logout-tics))
	       (if (funcall auto-logout-test
			    (/ (* auto-logout-tics auto-gc-interval) 60))
		   (progn 
		     (do-auto-save)
		     (message "Timed out!")
		     (run-hooks 'auto-logout-hooks)
		     (kill-emacs t)))))
	     (t
	      (and auto-logout-debug
		   (message  "Resetting auto-logout-tics"))
	      (setq auto-logout-tics 0)))))


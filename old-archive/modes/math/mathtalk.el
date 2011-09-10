;;; Communication with mathematica
;;; Copyright 1989 Daniel LaLiberte

;;; $Header: /home/srg/1/liberte/math/RCS/mathtalk.el,v 1.2 89/05/25 01:08:50 liberte Exp Locker: liberte $
;;; $Log:	mathtalk.el,v $
; Revision 1.2  89/05/25  01:08:50  liberte
; Fix math 1.2 OUTPROMPTs.
; Fixup interrupt handling.
; 

(provide 'mathtalk)
(require 'math-mode)  ; usually loaded first anyway

(defconst math-path "mathremote.emacs"
  "Path to the mathematica program.")

(defconst math-version ""
  "String from Mathematica $Version.")


(defconst math-ESCAPE "!")

(defconst math-action-code-base ?\#
  "First character in action code list.")

(defconst math-action-codes '
  [aBEEP
   aCOMPLETE
   aDEBUGd
   aDEBUGo
   aDEBUGu
   aDISCARD
   aESCAPE
   aEVAL
   aEVALONLY
   aFORMAT
   aFRONTENDERROR
   aIAMHERE
   aINCOMPLETE
   aINFO
   aINPROMPT
   aINPUTu
   aINPUTFORM
   aINSPECTORd
   aINSPECTORi
   aINSPECTORu
   aINTERRUPTABLE
   aINTERRUPTd
   aINTERRUPTi
   aINTERRUPToi
   aINTERRUPTui
   aMESSAGE
   aOUTPROMPT
   aOUTPUTFORM
   aPRINT
   aRESPOND
   aRESPONSE
   aSENDINT
   aAREYOUTHERE
   aSTART
   aSTATUS
   aSYNTAX
   aTOPLEVELd
   aTOPLEVELi
   aTOPLEVELu
   aPOSTSCRIPT
   aVALUE
   aQUIT
   aINPUTi
   aINPUTd
   aEVALNOIF
   aEVALONOIF
   aNEWLINE
   aSTDERR
   aRESET
   aINTERRUPTu
   ]
  "The list of symbols representing MathTalk Action Codes.
These have values of consecutive ASCII codes starting with
math-action-code-base.")


(defun math-enum (seq &optional start)
  "Set the values of each symbol in SEQ to respective natural numbers,
0 to (length SEQ).  Optional START means start at that value instead of 0."
  (let ((i (or start 0))
        (elist (append seq nil))) ; make it a list
    (while elist
      (set (car elist) i)
      (setq elist (cdr elist))
      (setq i (1+ i))
      )))


;; Initialize the symbols in math-action-codes to corresponding constant values.
(math-enum math-action-codes math-action-code-base)

(defun math-declare-action-takes-data (action-list)
  "Declare that all the actions in ACTION-LIST come with preceeding data."
  (while action-list
    (put (car action-list) 'math-data t)
    (setq action-list (cdr action-list))))

(math-declare-action-takes-data
 '(aCOMPLETE aDEBUGo aDISCARD aEVAL aEVALONLY aFORMAT aINCOMPLETE
	     aINFO aINPROMPTd aINPUTu aINPUTFORM aINTERRUPToi aMESSAGE
	     aOUTPUTPROMPT aOUTPUTFORM aPRINT aRESPONSE aSTATUS aSYNTAX
	     aPOSTSCRIPT))

(defun math-with-data (action)
  "Return non-nil if ACTION comes with data."
  (get action 'math-data))

  

(defun math-interpret-action (c)
  "Interpret the character C.  Return the symbol for the action."
  (aref math-action-codes (- c math-action-code-base)))

(defvar math-msg-block nil
  "The last message block received.")

(defun math-get-block ()
  "Return the block and set it to nil."
  (prog1
      math-msg-block
    (setq math-msg-block nil)))


(defun math-send-interrupt ()
  "Send an interrupt to the back end."
  (math-send-action 'aSENDINT)
  )

(defun math-get-completion (symbol)
  "Get a completion from mathematica for SYMBOL."
  (math-send-string-action symbol 'aINCOMPLETE)
  (math-kernel-loop)
  math-completions
  )

(defun math-send-start ()
  "Indicate that we are about to send a block of data to the back end."
  (to-math nil 'aSTART)
  )

(defun math-send-data (str)
  "Send the string STR to the back end."
  (to-math str nil)
  )

(defun math-send-action (action)
  "Send the ACTION."
  (to-math nil action)
  )

(defun math-send-string-action (str action)
  "Send the string STR (preceded by aSTART action)
and then send ACTION."
  (math-send-start)
  (to-math str action)
  )


(defvar math-permission-to-interrupt nil
  "non-nil if front-end now has permission to interrupt the kernel.
This should be used to inhibit interrupts while handling an interrupt.")

(defvar math-current-menu 'top-level
  "indicates which menu we are in at the moment.  Not used.")


(defun math-initialize ()
  "Initialize the mathematica kernel process."
; doesnt seem to be used yet
;  (send-action aRESPOND)
  
    )


(defun math-kernel-loop ()
  "Loop to handle messages from kernel until done."

  ;; clear out previous state
  (setq math-output-form nil
	math-input-form nil
	math-output-prompt nil
	math-Input-response ""
	math-message-blocks nil
	math-info-blocks nil
	math-print-blocks nil
	math-eval-result nil
	math-completions nil
	math-debug-depth 0
	math-quit-count 0)

  (math-kernel-inner-loop)
  (message "")
  )


(defun math-kernel-inner-loop ()
  "Loop until kernel is done."	
  (let ((math-kernel-done nil)
	action)
    (unwind-protect
	(while (not math-kernel-done)
	  (setq action (get-math-msg))
	  (if (fboundp action)
	      (funcall action))
	  )

      (if (not math-kernel-done)
	  (progn
	    ;; escaped through top-level; gotta reset Mathematica
	    ;; what to do??
	    ))
      )))


(defvar math-error-offset nil
  "For syntax errors, this is the character position of the error.")

(defvar math-input-form nil)
(defvar math-output-form nil)

(defvar math-Input-response "" "Response to last Input function.")
(defvar math-Input-prompt "" "Prompt for Input function.")
(defvar math-input-prompt nil "The next input cell name.")
(defvar math-output-prompt nil)

(defvar math-message-blocks nil
  "List of messages from the last evaluation.")

(defvar math-info-blocks nil
  "List of info strings from the last evaluation.")

(defvar math-print-blocks nil
  "List of print strings from the last evaluation.")

(defvar math-completions nil)
(defvar math-eval-result nil)

(defvar math-debug-depth 0
  "Depth of debug inspect recursions.")

(defvar math-quit-count 0)


;; Functions to handle each type of action

  
(defun aBEEP ()
  (message "Error in data.  This should never happen.")
  (ding)
  (sit-for 1))

(defun aCOMPLETE ()
  "Here are the completions you asked for. Respond."
  (let ((block (math-get-block)))
    (if (not block)
        (error "missing block")
      (while (string-match "," block)
	(aset block (match-beginning 0) ?\ ))
      (setq math-completions
	    (car (read-from-string (concat "[" block "]"))))
      (setq math-kernel-done t)
      )))

(defun aDEBUGu ()
  "Please put up the debug menu."
  (if (get-buffer math-message-buffer)
      (save-excursion
	(set-buffer math-message-buffer)
	(if (zerop math-debug-depth)
	    (erase-buffer))
	(setq math-debug-depth (1+ math-debug-depth))
	)))

(defun aDEBUGd ()
  "Take down the debug menu."
  (setq math-debug-depth (1- math-debug-depth)))

(defun math-debug-inspect ()
  "Get input for evaluation.  Return nil if no input entered."
  
  (let ((inspect-input ""))
    (progn
      (setq inspect-input
	    (read-string (concat "Inspect>"
				 (make-string math-debug-depth ?>) " ")
			 inspect-input))
      (if (zerop (length inspect-input))
	  (signal 'quit 'inspect))
      (math-send-string-action inspect-input 'aEVALONOIF)
      )
    ))


(defun math-debug-menu ()
  "Request action from the user in response to an debug output."

  (let (char  ; initially nil
	(inhibit-quit nil)
	(cursor-in-echo-area t)
	)

    (condition-case err
	(while (not (memq char '(?i ?s ?n ?c ?f ?a)))
	  (if char
	      (progn
		(beep)
		(message "Please enter the first letter of a command.")
		(sit-for 1)))
	  (message "Debug:  Inspect  Step  Next  Continue  Finish  Abort? ")
	  (setq char (read-char))

	  ;; handle Inspect here
	  (if (= ?i char)
	      (condition-case err
		  (math-debug-inspect)
		(quit ; quit from inspect - get another command
		 (setq char nil)
		 )))
	  )
      (quit (setq char ?a)))  ; quit from debug - same as abort

    (message "")
    (cond
     ((= ?s char)
      (message "Step")
      (math-send-string-action "s" 'aRESPONSE))
     ((= ?n char)
      (message "Next")
      (math-send-string-action "n" 'aRESPONSE))
     ((= ?c char)
      (message "Continue")
      (math-send-string-action "c" 'aRESPONSE))
     ((= ?f char)
      (message "Finish")
      (math-send-string-action "f" 'aRESPONSE))
     ((= ?a char)
      (message "Abort")
      (math-send-string-action "a" 'aRESPONSE))
     (t)
     )
    ))

(defun aDEBUGo ()
  "Here is an expression for the Debug menu. Respond."
  (let ((block (math-get-block)))
    (if math-output-form
	;; previous inspect output
	(math-display-msg (concat math-output-form "\n"))
      ;; else not from inspect, so display block
      (math-display-msg (concat block "\n"))))
  (setq math-output-form nil)
  (math-debug-menu)
  )

(defun aFRONTENDERROR ()
  "The front-end has sent something inappropriate."
  (error (message "Front end sent something inappropriate.")))

(defun aINFO ()
  "Here is the output from an Information command."
  (let ((block (math-get-block)))
    (setq math-info-blocks (cons block math-info-blocks))
    ))

(defun aINPROMPT ()
  "Here is the input prompt."
  (let ((block (math-get-block)))
    (setq math-input-prompt block)
    ))

(defun aINPUTu ()
  "Here is the prompt for an Input command."
  (let ((block (math-get-block)))
    (setq math-Input-prompt block)
    (setq math-Input-response "")
    ;; keep looping til aINPUTd
    ))

(defun aINPUTi ()
  "Please send some input for the Input command.  Respond."
  (if (eq math-eval-result 'syntax-error)
      (let ((cursor-in-echo-area t))
	(beep)
	(message "Syntax error: %s" math-Input-response)
	(save-excursion
	  (set-buffer (window-buffer (minibuffer-window)))
;;	  (message "current buffer: %s" (current-buffer))
	  (goto-char math-error-offset) ; this doesnt seem to do it!!
	  (sit-for 1))))
  (setq math-eval-result nil)
      
  (let ((done nil)
	(inhibit-quit t))
    (unwind-protect
	(while (not done)
	  (condition-case err
	      (progn
		(setq math-Input-response
		      (read-string math-Input-prompt math-Input-response))
		(setq done t))
	    (quit
	     (beep)
	     (message "No interrupts while Inputting.") (sit-for 1)
	     )))
      (if (not done)
	  (progn  ; escaped through top-level; gotta reset Mathematica
	    (math-send-interrupt)
	    (math-send-string-action "0" 'aRESPONSE)
	    (math-kernel-loop) ; aRESET, aINPUTd
	    (get-math-msg) ; aINTERRUPTui
	    (math-send-string-action "a" 'aRESPONSE)
	    (math-kernel-loop)
	    ))
      ))
    
  (math-send-string-action math-Input-response 'aRESPONSE)
  )

(defun aINPUTd ()
  "Please take down the Input menu."
  ;; nothing to do since the menu goes away itself
  )

(defun aINPUTFORM ()
  "Here is the input form of an expression."
  (let ((block (math-get-block)))
    (setq math-input-form block)
    ))

;; aUSERQUIT not sent to or from mathematica.
;; Its purpose is to communicate the user quit while waiting for mathematica
;; to finish its computation.
(defun aUSERQUIT ()
  "User has typed quit-char as interrupt."
  (if math-show-debug
      (to-math-debug "User Quit--------------------\n"))
  (if (get-buffer math-message-buffer)
      (save-excursion
	(set-buffer math-message-buffer)
	(erase-buffer)))
  (math-send-interrupt)
  )

(defun aINTERRUPTd ()
  "Take down the interrupt dialog."
  )

(defun aINTERRUPTi ()
  "Send an interrupt command."
  )


(defun math-interrupt-menu ()
  "Request action from the user in response to an interrupt."

  ;; reset the number of quit attempts
  (setq math-quit-count 0)

  (let (char ; initially nil
	(inhibit-quit nil)
	(cursor-in-echo-area t)
	)
    
    (condition-case err
	(while (not (memq char '(?a ?c ?q ?o)))
	  (if char
	      (progn
		(beep)
		(message "Please enter a, c, o, or q")
		(sit-for 1)))
	  (message "Interrupted:  Abort  Continue  One-step  or  Quit? ")
	  (setq char (read-char))

	  ;; handle Quit here
	  (if (and (= ?q char)
		   (not (yes-or-no-p
			 "Do you really want to quit from Mathematica? ")))
	      (setq char nil))
	  )
      (quit (setq char ?a)))

    (message "")
    (cond
     ((= ?a char)
      (message "Abort")
      (math-send-string-action "a" 'aRESPONSE))
     ((= ?c char)
      (message "Continue")
      (math-send-string-action "c" 'aRESPONSE))
     ((= ?q char)
      (message "Quit")
      (math-send-string-action "quit" 'aRESPONSE)
      (setq math-kernel-done t))
     ((= ?o char)
      (message "One-step")
      (math-send-string-action "o" 'aRESPONSE))
     (t ; should never happen
      (math-send-string-action "a" 'aRESPONSE))
     )
    ))

(defun aINTERRUPToi ()
  "Here is something to display in the interrupt dialog.  Respond."
  (let ((block (math-get-block)))
    (math-display-msg block))
  (math-interrupt-menu)
  )

(defun aINTERRUPTui ()
  "Put up the interrupt dialog."
  (math-interrupt-menu)
  )

(defun aMESSAGE ()
  "Here is a message."
  (let ((block (math-get-block)))
    (setq math-message-blocks (cons block math-message-blocks))
    ))

(defun aOUTPROMPT ()
  "Here is the output prompt."
  (let ((block (math-get-block)))
    (setq math-output-prompt 
	  (if (string-match "[^=]$" block)
	      (concat block "=")
	    block))
    ))

(defun aOUTPUTFORM ()
  "Here is the output form of an expression."
  (let ((block (math-get-block)))
    (setq math-output-form block)
    ))

(defun aPRINT ()
  "Here is some print output from the evaluation."
  (let ((block (math-get-block)))
    (setq math-print-blocks (cons block math-print-blocks))
    ))

(defun aPOSTSCRIPT ()
  "Here is some postscript output."
  (let ((block (math-get-block)))
    ;; ignore it for now
    ))

(defun aSYNTAX ()
  "Here is the column number in which a syntax error occurred."
  (let ((block (math-get-block)))
    (setq math-eval-result 'syntax-error)
    (setq math-error-offset (string-to-int block))
    ))

(defun aTOPLEVELi ()
  "Please send me some top level input.  Respond."
  (setq math-kernel-done t)
  )

(defun aVALUE ()
  "Please display the previous input/output forms as a value."
  (let ((block (math-get-block)))
    (setq math-eval-result 'value)
    ))



(defun wait-for-math (math-buffer-size)
  "Wait for math to send more stuff - when the buffer size grows."
  (while (= math-buffer-size (buffer-size))
    (if (not (eq (process-status math-process) 'run))
	(progn
	  (math-error "Math process is jammed with status %s"
		      (process-status math-process))
	  (error "Mathematica is not running.")))
	
    ;;    (sit-for 1)  ; this works but takes at least one second
    (let ((inhibit-quit nil))		; allow quit
      (message "Waiting for Mathematica output...")
      ;; needs a time limit to prevent indefinite wait.
      (accept-process-output math-process)
      (message "")			; clear message
      (sit-for 0)
      )))


(defun math-read-atom-safely ()
  "Read one atom from current buffer, delete the atom
and return it or return nil if not readable.
Thus, we cannot distinguish a nil atom with this."
  (let (size atom endpt)
    (while (not 
	    (condition-case err
		(progn
		  (goto-char (point-min))
		  (setq size (buffer-size))  ; remember the size before reading
		  (setq atom
			(read (buffer-substring
			       (point)
			       (setq endpt
				     (scan-sexps (point) 1)))))
		  ;; delete what we read
		  (delete-region (point) endpt)
		  t  ; quit the loop
		  )
	      
	      (error
;;	       (message "not all in yet, wait for more.")
	       (condition-case err
		   (progn 
		     (wait-for-math size)
		     nil)			; if unreadable, return nil

		 (quit  ; catch quit so we can recover
		  (message "Interrupt") (sit-for 0)

		  ;; increment the number of quit attempts made
		  ;; since the last time mathematica responded.
		  (setq math-quit-count (1+ math-quit-count))
		  (if (> math-quit-count 1)
		      (if (yes-or-no-p "Kill Mathematica process? ")
			  (progn
			    (setq quit-flag t) ; let's get out of this loop
			    ;; setting the quit-flag may do nothing
			    (kill-math t)
			    (error "Evaluation not completed.")))
		    
		    (setq quit-flag nil)
		    (setq atom 'aUSERQUIT)  ; quit the loop and do aUSERQUIT
		    ))
		 )))))

    (if math-show-debug
	(to-math-debug (format "From math: %s\n" atom)))

    atom  ; return the atom
    ))
  

(defun get-math-msg ()
  "Get a message from the top of the math-filter-queue
for the current buffer's math-process.
Return a command, when it is completed.
Set math-msg-block to any data received before that."

  (let ((save-buffer (current-buffer))
	atom)
	
    (save-excursion
      (set-buffer math-buffer)
      (setq atom (math-read-atom-safely))
      (if (stringp atom)
	  (progn
	    (setq math-msg-block atom)
	    ;; read again, it must be an action this time
	    (setq atom (math-read-atom-safely))
	    ))
      (set-buffer save-buffer)
      atom ; result
      )))


;;;-------------------
;;; Display stuff

(defconst math-message-buffer "*math-message*"
  "Buffer to display messages from mathematica that dont go in the notebook.")


(defun math-display-msg (msg)
  "Display msg in a temporary buffer."
  (let* ((save-buf (current-buffer))
	(buf (get-buffer-create math-message-buffer)))
    (pop-to-buffer buf)
    (goto-char (point-max))
    (insert msg)
    (if (not (pos-visible-in-window-p))
	(scroll-down (/ (window-height) 2)))
    (pop-to-buffer save-buf)
    ))

(defun display-math-buffer (msg &optional fill delete)
  "Display msg in a temporary buffer and optionally paragraph fill
     and delete window after next input.  The window size is shrunk
to just hold the text."
  (let
      ((obuf (current-buffer))
       (owin (selected-window))
       (oheight (window-height))
       start-pnt
       (buf "junk")
       )
    (if (< 0 (length msg))
        (progn
	  (set-buffer (get-buffer-create buf))

          (goto-char (point-max))
          (insert-string "\n")
          (setq start-pnt (point))
          (insert msg)
          (narrow-to-region start-pnt (point))

          (if fill
              (progn
                (fill-region-as-paragraph start-pnt (point))
                (if (< 1 (length msg))
                    (progn
                      (skip-chars-backward " \t\n\f")
                      (delete-region (point) (point-max))
                      ))))

          (let* ((win-config (current-window-configuration))
                 (win (get-buffer-window (current-buffer)))
                 (lines-needed (max (min (count-lines start-pnt (point-max))
                                         (/ (window-height) 2) ) ; at most half
                                    2) ; at least two lines - one has problems
                               ))
            ;;  (display-buffer (current-buffer))
            (if win
                (progn
                  (select-window win)
                  (enlarge-window (1+ (- lines-needed (window-height))))
                  )
              (split-window owin (1- (- (window-height owin) lines-needed)))
              (other-window 1)          ; the new window
              (setq win (selected-window))
              (set-window-buffer (selected-window) math-buffer)
              )

            (set-window-start win start-pnt t)
            (goto-char start-pnt)       ; one of these ought to do it

            (set-buffer obuf)
            (if (not (eq owin (selected-window)))
                (if owin (select-window owin))
              )
            (update-display)
            ;; wait for user input - then delete the window
            (if delete
                (progn
                  (while (not (input-pending-p))
                    (sit-for 2))
                  ;; (select-window win)
                  ;; (delete-window)
                  (bury-buffer buf)
                  (set-window-configuration win-config)
                  )))
          )

      ))
  )


;--------------------------------------------------------------
; Send a message to Math.

(defun to-math (msg action)
  "Send message MSG to math process followed by ACTION."

  (if (and (not math-mode) (not (eq action 'aQUIT)))  ; aQUIT??
      (error "Buffer %s is not in Math mode." (current-buffer)))

  (if math-show-debug
      (to-math-debug (format "To-math: \"%s\" %s\n" msg action)))
  (if (not math-process)
      (run-math))

  (let ((msg-packet
	 (concat ; message and action
	  (if msg
	      (let ((match-start 0)  ; change any math-ESCAPE chars in msg
		    (match-data (match-data)))
		(while (string-match math-ESCAPE msg match-start)
		  (let ((where (match-beginning 0)))
		    ;; replace math-ESCAPE char
		    (setq msg (concat (substring msg 0 where)
				      math-ESCAPE (char-to-string aESCAPE)
				      (substring msg (1+ where))))
		    (setq match-start (1+ (match-end 0)))
		    ))
		(store-match-data match-data) ; restore
		msg)
	    )

	  (if action
	      (concat 
	       math-ESCAPE
	       (char-to-string (symbol-value action)))
	    "")
            "\n")
	 ))
    
;;    (if math-show-debug
;;	(to-math-debug (format "  #send: \"%s\"\n" msg-packet)))
    (send-string math-process msg-packet)
    msg-packet
    )
  ) ; to-math

;-------------------------------------------



;================================================================
;; Run math as inferior of Emacs.
;; This code is derived from compile.el.

(defconst math-buffer " *mathematica*"
  "The name of the buffer in which mathematica output is first put.")

(defvar math-process nil
  "Process created by math command, or nil if none exists now.
Note that the process may have been \"deleted\" and still
be the value of this variable.")



(defun run-math ()
  "Run and initialize Mathematica, if it is not already active."
  (if (setup-math-process)
      ;; first time, so initialize things
      (progn
;;	(math-send-string-action "<< init.m" 'aEVALONLY)
;;	(math-kernel-loop)

	(let ((math-input-prompt nil))  ; save first prompt
	  (math-send-string-action "$Version" 'aEVALNOIF)
	  (math-kernel-loop)
	  (setq math-version math-output-form)
	  (math-send-string-action "$Line--" 'aEVALONLY);; decrement prompt
	  (math-kernel-loop)
	  ))
    )

  (message "Mathematica is running")
  (setq math-last-window-width 0)
  (math-reset-pagewidth)
  )


(defvar math-last-window-width (window-width))

(defun math-reset-pagewidth ()
  "If window-width has changed, send resize to mathematica."
  (if (/= (window-width) math-last-window-width)
      (progn
	(math-send-string-action
	 (concat "ResetMedium[PageWidth -> "
		 (- (window-width) (length math-input-prompt) 8)
		 "]")
	 'aEVALONLY)
	(math-kernel-loop)
	))
  (setq math-last-window-width (window-width))
  )

 
(defun setup-math-process ()
"Set up math in a separate process asynchronously
with output going to the math-buffer asynchronously.
Return nil if the process is already active."

;;  (save-some-buffers)

  (if (and math-process
	   (not (eq (process-status math-process) 'run)))
      ;; delete any bad math process
      (condition-case err
	  (progn
	    (delete-process math-process)
	    (setq math-process nil))
	(error nil)))

  (if (not math-process)
      (progn
	(setq math-process
	      (let ((process-connection-type nil))
		(start-process "math"
			       math-buffer
			       math-path)))
;;	(message "Math status: %s" (process-status math-process))
	(if (not (eq 'run (process-status math-process)))
	    (progn
	      (setq math-process nil)
	      (error (message "Can't execute Mathematica in %s" math-path))))

	(process-kill-without-query math-process)
	(message "Mathematica process started")
	(set-process-sentinel math-process 'math-sentinel)
  
	(let* ((thisdir default-directory)
	       (buf math-buffer)
	       (outwin (get-buffer-window buf)))
    
	  (if (eq math-buffer (current-buffer))
	      (goto-char (point-max)))
	  (save-excursion
	    (set-buffer buf)
	    (erase-buffer)
	    (buffer-flush-undo (get-buffer buf))
;;	    (let ((start (save-excursion (set-buffer buf) (point-min))))
;;	      (set-window-start outwin start)
;;	      (or (eq outwin (selected-window))
;;		  (set-window-point outwin start)))
	    (setq default-directory thisdir)

	    (fundamental-mode)
	    (setq mode-name "Math")
	    ;; Make log buffer's mode line show process state
	    (setq mode-line-format
		  "--%1*%1*-Emacs: %17b   %   %[(%m: %s)%]----%3p--%-")))
	(math-kernel-loop)		; read the first message
	math-process
	)
    nil  ; return nil
    ))


;; Called when math process changes state.
(defun math-sentinel (proc msg)
  (if (memq (process-status proc) '(signal exit))
      (let* ((obuf (current-buffer))
;            (omax (point-max))
;            (opoint (point))
             )
        (unwind-protect
            (progn
              (set-buffer (get-buffer-create "*math-debug*"))
              (goto-char (point-max))
              (insert ?\n  "Mathematica: " msg)
              (setq mode-line-format
                    (concat
                     "--%1*%1*-Emacs: %17b   %M   %[(%m: "
                     (symbol-name (process-status proc))
                     ")%]----%3p--%-"))
              (delete-process proc)
              (setq math-process nil)
              ;; Force mode line redisplay soon
              (set-buffer-modified-p (buffer-modified-p)))
;        (if (< opoint omax)
;            (goto-char opoint))
         (set-buffer obuf)))
  )
  )


(defun kill-math (&optional arg)
  "Kill the mathematica process.  Only used in exceptional circumstances."
  (interactive)
  (if math-process
      (if (or arg
	      (yes-or-no-p "Kill the current Mathematica process? "))
	  (progn
	    (message "Killing Mathematica...")
	    (kill-process math-process)
	    (setq math-process nil)
	    (setq math-input-prompt nil)
	    (message "")
	    ))
    (message "Mathematica is not active.")
    ))


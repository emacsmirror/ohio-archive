; Newsgroups: gnu.emacs.sources
; Path: hal.com!decwrl!sun-barr!ames!saimiri.primate.wisc.edu!zaphod.mps.ohio-state.edu!magnus.acs.ohio-state.edu!bgsuvax!att!att!dptg!ucs!skdutta
; From: skdutta@ucs.att.com (Saumen Dutta)
; Subject: emacs front end for sable.
; Organization: AT&T Universal Card Services, Jacksonville FL
; Date: Mon, 9 Nov 1992 10:13:09 GMT
; 
; Sable is a product for managing configuration control and releases of 
; software. The command set of sable is extensive and complicated. The
; command line interface requires the presence of Korn Shell. It has
; a curses interface too but that requires user to type many fields
; before a command can be executed. The following utility helps users
; to use the power of emacs and execute sable commands from within
; emacs. You need to have sable and korn shell to use this.
;;
;;
;; sable.el -- front end code for using Sable from GNU Emacs
;;             by Saumen K. Dutta (skdutta@ucs.att.com)
;;             v1.0 October 31 1992
;;
;;

;; LCD Archive Entry:
;; sable|Saumen Dutta|skdutta@ucs.att.com|
;; Front end code for using Sable from GNU Emacs|
;; 92-10-31|1.0|~/interfaces/sable.el.Z|

;;
;; Description:
;;       sable.el is a GNU Emacs utility to help Assigned Developers
;; to interact with sable in a friendly manner. Most of the commands
;; are reduced to simple keystrokes. Parameters of the frequently used
;; commands are stored and can be reused. Issuing of edget and edput
;; to get and put files are not required as the program can find out
;; the next logical operation. Buffers are loaded direcly from the 
;; command so that all the required sable commands and file editing can
;; be tackled from the same emacs environment. Limited report and query
;; support is also available.
;;
;;

;;
;; Use:
;;    To start the environment you need to issue `M-x sable'. From then on
;; all global keymapping will take into effect. Press C-ch for help
;;

;;
;; User Modifiable Variables (don't modify these unless you are required to!)
;;

(defvar sable-ksh-program "ksh" "The korn shell executable")
(defvar sable-buffer "*sable*" "Name of the sable buffer")

;;;
;;; VARIABLES
;;;

(defvar sable-ksh-process nil "Process running the sable")
(defvar sable-generic nil "Name of the sable generic")
(defvar sable-mr nil "Name of the sable modification request")
(defvar sable-dir nil "directory from where edgotten in sable")
(defvar sable-command nil "sable command string to be executed")
(defvar sable-process-status nil "Status of the sable process, The following
  conventions are followed, 0 = sable process has started, 1 = sable process
  has returned successfully and 2 = sable process has failed")
(defvar sable-file nil "file to be operated upon by sable")
(defvar sable-autodependency nil "level of autodependency")
(defvar sable-query-relation nil "relation name of the sable query")


(defun sable-start-ksh ()
    "Start the ksh and initialize sable environment"
    ;; first time or if process dies
    (if (and (processp sable-ksh-process)
	     (or (eq 'stop (process-status sable-ksh-process))
		 (eq 'run (process-status sable-ksh-process)))) nil
	(setq sable-ksh-process   ;; [re]start the ksh process
	      (start-process "sable" 
			     (get-buffer-create sable-buffer) 
			     sable-ksh-program))
	(save-excursion
	  (set-buffer sable-buffer)
	  (set-marker (process-mark sable-ksh-process) 1)))
	(set-process-filter sable-ksh-process 'sable-filter)
    	(process-kill-without-query sable-ksh-process)
	(setq sable-command (concat ". sablime " sable-generic))
	(sable-process-command)
	(message "Done!"))


(defun sable-filter(process input)
  "Filter for the Sable process to catch sable errors"
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(set-buffer (process-buffer process))
	(save-excursion
	  (goto-char (process-mark process))
	  (insert input)
	  (set-marker (process-mark process) (point)))
      (goto-char (process-mark process))
      (set-buffer old-buffer)))
      
  ;; search for the bell character in the input
  (if (eq nil (string-match "\007" input)) (setq sable-process-status 1)
    (progn (display-buffer (process-buffer process))
	   (setq sable-process-status 2)
	   (error "Sable Error..." ))))


(defun sable-process-command()
  "Process the sable command"
  (setq sable-process-status 0)
  (message "Sending command to sable...")
  (save-excursion
    (set-buffer (process-buffer sable-ksh-process))
    (erase-buffer))
  (process-send-string sable-ksh-process 
		       (concat sable-command "\n")) 
  (accept-process-output)
  ;; guarantees that the sable buffer is loaded fully
  (while (eq sable-process-status 0) (sleep-for 1)))



(defun sable-edget-(&optional long)
  "short edget without a parameter, long edget with one"
  (if (buffer-file-name) 
      (progn (setq sable-file (file-name-nondirectory buffer-file-name))
	     (if (or (eq sable-mr nil) long)
		 (setq sable-mr (read-string "MR Number: " sable-mr)))
	     (if (or (eq sable-dir nil) long)
		 (setq sable-dir (read-string "Directory: " sable-dir)))
	     ;; Change the directory to the buffer's one before composing the command
	     (setq sable-command (concat "cd " (file-name-directory buffer-file-name) "; "))
	     ;; Now form the sable command from the global variables
	     (setq sable-command (format 
				  "%s edget g=%s mr=%s rm=y dir=%s srf=%s prompt=n"
				  sable-command sable-generic sable-mr sable-dir
				  sable-file))
	     (sable-process-command)
	     (if (eq sable-process-status 1) 
		 (progn
		   ;; making sure that the file is gotten properly
		   (while (eq (file-writable-p buffer-file-name) nil) (sleep-for 1))		   
		   (revert-buffer nil t)
		   (message "Done!"))))
    (error "There is no file associated with buffer %s" (buffer-name))))


(defun sable-unedget-(&optional long)
  "short unedget without a parameter, long unedget with one"
  (if (buffer-file-name) 
      (progn (setq sable-file (file-name-nondirectory buffer-file-name))
	     (if (or (eq sable-mr nil) long)
		 (setq sable-mr (read-string "MR Number: " sable-mr)))
	     (if (or (eq sable-dir nil) long)
		 (setq sable-dir (read-string "Directory: " sable-dir)))
	     ;; Change the directory to the buffer's one before composing the command
	     (setq sable-command (concat "cd " (file-name-directory buffer-file-name) "; "))
	     ;; Now form the sable command from the global variables
	     (setq sable-command (format 
				  "%s unedget g=%s mr=%s dir=%s srf=%s prompt=n"
				  sable-command sable-generic sable-mr sable-dir
				  sable-file))
	     (sable-process-command)
	     (if (eq sable-process-status 1) 
		 (progn
		   ;; making sure that the file is gotten properly
		   (while (eq (file-writable-p buffer-file-name) nil) (sleep-for 1))
		   ;; make the file read-only. Sable does not do it but I think it 
		   ;; should have!
		   (set-file-modes buffer-file-name 292)
		   (revert-buffer nil t)
		   (message "Done!"))))
    (error "There is no file associated with buffer %s" (buffer-name))))



(defun sable-edput-(&optional long)
  "short edput without a parameter, long edput with one"
  (if (buffer-file-name) 
      (progn 
	;;give the user a chance to save the file if he has unsaved edits
	(if (and (buffer-modified-p)
		 (y-or-n-p (format "%s has been modified. Write it out?" (buffer-name))))
	    (save-buffer))
	(setq sable-file (file-name-nondirectory buffer-file-name))
	(if (or (eq sable-mr nil) long)
	    (setq sable-mr (read-string "MR Number: " sable-mr)))
	(if (or (eq sable-dir nil) long)
	    (setq sable-dir (read-string "Directory: " sable-dir)))
	(if (or (eq sable-autodependency nil) long)
	    (setq sable-autodependency (read-string "Autodependency: " "line-level"))) 
	;; Change the directory to the buffer's one before composing the command
	(setq sable-command (concat "cd " (file-name-directory buffer-file-name) "; "))
	;; Now form the sable command from the global variables
	(setq sable-command (format 
			     "%s edput g=%s mr=%s rm=n dir=%s adep=%s srf=%s prompt=n"
			     sable-command sable-generic sable-mr sable-dir
			     sable-autodependency sable-file))
	(sable-process-command)
	(if (eq sable-process-status 1) 
	    (progn
	      ;; making sure that the file is put properly
	      (while (eq (file-writable-p buffer-file-name) t) (sleep-for 1))		   
	      (message "Loading file in buffer")
	      (revert-buffer nil t)
	      (message "Done!"))))
    (error "There is no file associated with buffer %s" (buffer-name))))


(defun sable-query-(&optional long)
  "forced parameter prompt with long queries"
  (if (or (eq sable-mr nil) long)
      (setq sable-mr (read-string "MR Number: " sable-mr)))
  (if (or (eq sable-query-relation nil) long)
      (setq sable-query-relation (read-string "Relation: " "MD"))) 
  (setq sable-command (format
		       "query mr=%s relation=%s prompt=n"
		       sable-mr sable-query-relation))
  (sable-process-command)
  (if (eq sable-process-status 1)
      (save-excursion
	(display-buffer (process-buffer sable-ksh-process))
	(message "Done!"))))
	



(defun sable-reset()
  "Reset all the sable temporary variables"
  (setq sable-generic nil)
  (setq sable-mr nil)
  (setq sable-dir nil)
  (setq sable-file nil)
  (setq sable-command nil)
  (setq sable-process-status nil)
  (setq sable-query-relation nil)
  (setq sable-autodependency nil))


(defun sable-key-bind()
  (define-key (current-global-map) "\C-c?" 'sable-status)
  (define-key (current-global-map) "\C-ci" 'sable)
  (define-key (current-global-map) "\C-cr" 'sable-report)
  (define-key (current-global-map) "\C-cg" 'sable-edget)
  (define-key (current-global-map) "\C-cp" 'sable-edput)
  (define-key (current-global-map) "\C-cn" 'sable-next-short)
  (define-key (current-global-map) "\C-cN" 'sable-next-long)
  (define-key (current-global-map) "\C-cu" 'sable-unedget-short)
  (define-key (current-global-map) "\C-cU" 'sable-unedget-long)
  (define-key (current-global-map) "\C-cq" 'sable-query-short)
  (define-key (current-global-map) "\C-cQ" 'sable-query-long)
  (define-key (current-global-map) "\C-c\C-g" 'sable-getversion)
  (define-key (current-global-map) "\C-c\C-s" 'sable-submit)
  (define-key (current-global-map) "\C-ch" 'sable-help))

;;;
;;; Minibuffer commands
;;;


(defun sable(generic)
  "Initialize the sable generic and calls korn shell"
  (interactive "sGeneric: ")
  (sable-reset)
  (sable-key-bind)
  (setq sable-generic generic)
  (sable-start-ksh))

(defun sable-next-short()
  "Invoke the logical form of next short sable edit command"
  (interactive)
  (if (eq sable-generic nil) (error "Sable not Initialized")
    (if buffer-read-only (sable-edget-)
      (sable-edput-))))

(defun sable-next-long()
  "Invoke the logical form of next long sable edit command"
  (interactive)
  (if (eq sable-generic nil) (error "Sable not Initialized")
    (if buffer-read-only (sable-edget- 1)
      (sable-edput- 1))))

(defun sable-edget()
  "Invoke the short form of sable edget"
  (interactive)
  (if (eq sable-generic nil) (error "Sable not Initialized")
    (sable-edget-)))

(defun sable-edput()
  "Invoke the short form of sable edput"
  (interactive)
  (if (eq sable-generic nil) (error "Sable not Initialized")
    (sable-edput-)))


(defun sable-query-short()
  "Invoke short query"
  (interactive)
  (if (eq sable-generic nil) (error "Sable not Initialized")
    (sable-query-)))

(defun sable-query-long()
  "Invoke query with parameter prompting"
  (interactive)
  (if (eq sable-generic nil) (error "Sable not Initialized")
    (sable-query- 1)))

(defun sable-unedget-short()
  "Invoke short unedget"
  (interactive)
  (if (eq sable-generic nil) (error "Sable not Initialized")
    (sable-unedget-)))

(defun sable-unedget-long()
  "Invoke unedget with parameter prompting"
  (interactive)
  (if (eq sable-generic nil) (error "Sable not Initialized")
    (sable-unedget- 1)))

(defun sable-report()
  "Produce a short report of all MRs"
  (interactive)
  (if (eq sable-generic nil) (error "Sable not Initialized")
    (progn 
      (setq sable-command (format
			   "report prompt=n"))
      (sable-process-command)
      (if (eq sable-process-status 1)
	  (save-excursion
	    (display-buffer (process-buffer sable-ksh-process))
	    (message "Done!"))))))


(defun sable-getversion()
  "Do a getversion on the generic"
  (interactive)
  (if (eq sable-generic nil) (error "Sable not Initialized")
    (progn
      (setq sable-command (concat
			   "getversion br="
			   (read-string "Branch <mr/ofc>: " "mr")))
      (let* ((sable-tmp-mrs 
	      (read-string "MRs for file selection: "))
	     (sable-tmp-umrs 
	      (read-string "MRs for additional changes: ")))
	(if (string= sable-tmp-mrs "")nil
	  (setq sable-command (concat
			       sable-command " mrs=" sable-tmp-mrs)))
	(if (string= sable-tmp-umrs "")nil
	  (setq sable-command (concat
			       sable-command " umrs=" sable-tmp-umrs))))
      (if (string= (read-string "List of files only <y/n>: ") "n")
	  (setq sable-command (concat
			       sable-command 
			       " list=n"
			       " node=" (read-string "Target-node: ")
			       " rm=" (read-string "Remove files: ")))
	(setq sable-command (concat
			     sable-command
			     " list=y")))
      (setq sable-command (concat
			   sable-command
			   " prompt=n"))
      (sable-process-command)
      (if (eq sable-process-status 1)
	  (save-excursion
	    (display-buffer (process-buffer sable-ksh-process))
	    (message "Done!"))))))


(defun sable-submit()
  "a simple sable submit mechanism"
  (interactive)
  (if (eq sable-generic nil) (error "Sable not Initialized")
    (progn
      (setq sable-command (concat
			   "getversion mr="
			   (read-string "MR Number: ")
			   " copyto="
			   (read-string "Resolution File[Mandatory]: ")
			   " prompt=n"))
      (if (y-or-n-p "Will submit the MR. Are you sure?")
	  (progn
	    (sable-process-command)
	    (if (eq sable-process-status 1)
		(save-excursion
		  (display-buffer (process-buffer sable-ksh-process))
		  (message "Done!"))))))))             
      

      

(defun sable-status()
  "Show the internal states and the default variables"
  (interactive)
  (if (eq sable-generic nil) (error "Sable not Initialized")
  (save-excursion
    (set-buffer (process-buffer sable-ksh-process))
    (goto-char 1)
    (insert "---------> Internal State of the Sable Mode <---------\n\n")
    (insert "Background Korn Shell process: " sable-ksh-program "\n")
    (insert "Sable Generic: ")
    (if (and sable-generic (not (string= sable-generic "")))
	(insert sable-generic "\n") 
	(insert "*Not Set*\n"))
    (insert "Sable Directory: ") 
    (if sable-dir (insert sable-dir "\n") (insert "*Not Set*\n")) 
    (insert "Sable File: ") 
    (if sable-file (insert sable-file "\n") (insert "*Not Set*\n")) 
    (insert "MR: ") 
    (if sable-mr (insert sable-mr "\n") (insert "*Not Set*\n")) 
    (insert "Sable Autodependency: ")
    (if sable-autodependency (insert sable-autodependency "\n") 
      (insert "*Not Set*\n")) 
    (insert "Sable Query Relation: ")
    (if sable-query-relation (insert sable-query-relation "\n") 
      (insert "*Not Set*\n")) 
    (insert "Last Sable Command: ")
    (if sable-command (insert sable-command "\n") (insert "*Not Set*\n")) 
    (insert "\n------------------------------------------------------\n\n")
    (insert "Buffer History: \n\n")
    (display-buffer (process-buffer sable-ksh-process)))))


(defun sable-help()
  "Print an explanation of keyboard macros in the sable environment"
  (interactive)
  (if (eq sable-generic nil) (error "Sable not Initialized")
  ;; there should be a better way to do this
    (save-excursion
      (set-buffer (process-buffer sable-ksh-process))
      (erase-buffer)
      (insert
       "\nThe Following Keyboard sequence is activated globally after the first\n"
       "time sable is called\n"
       "\n"
       "   Key     Command              Description\n"
       "   ---     -------              -----------\n"
       "\n"
       "  \\C\-c?  sable-status         Show the internal variables of emacs sable.\n"
       "\n"
       "  \\C\-ci  sable                Initialize the sable environment for a \n"
       "                                new modification request. This command is\n"
       "                                required before executing any other sable\n"
       "                                commands.\n"
       "\n"
       "  \\C\-cr  sable-report         Generate a short sable report\n"
       "\n"
       "  \\C\-cn  sable-next-short     Initiate the next logical sable operation.\n"
       "                                This will execute edget or edput.\n"
       "\n"
       "  \\C\-cN  sable-next-long      Long version of the initiate the next request.\n"
       "\n"
       "  \\C\-cg  sable-edget          Initiate the short edget operation.\n"
       "\n"
       "  \\C\-cp  sable-edput          Initiate the short edput operation.\n"
       "\n"
       "  \\C\-cu  sable-unedget-short  Initiate the short unedget operation.\n"
       "\n"
       "  \\C\-cU  sable-unedget-long   Long version of the unedget\n"
       "\n"
       "  \\C\-ch  sable-help           Print this help message\n"
       "\n"
       "  \\C\-cq  sable-query-short    Initiate the sable query.\n"
       "\n"
       "  \\C\-cQ  sable-query-long     Initiate the query with parameter prompting.\n"
       "\n"
       "  \\C\-c\\C-g  sable-getversion  \n"
       "\n"
       "  \\C\-c\\C-s  sable-submit  \n"
       )
      (display-buffer (process-buffer sable-ksh-process)))))





;;; Wish List
;;; ---------
;;; have a new sable mode
;;; have a dired show up with getversion
;;; deal with the resolution file of the submit command correctly
;;; add some more commands with some more user abstractions

;;; Bugs
;;; ----
;;; reports get screwed up if the cursor is in the *sable* window when the report
;;; command is executed. Looks like the buffer is not erasing properly - 10312250
;;; Fixed - 11011430

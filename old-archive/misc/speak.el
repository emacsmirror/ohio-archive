;; LCD Archive Entry:
;; speak|Dave Brennan|brennan@hal.com|
;; Make Emacs speak on Sun Sparc (requires Brown U. speak package)|
;; 92-02-13|1.0|~/misc/speak.el|

;; Copyright 1992, David J. Brennan
;; GNU Copyleft applies.

;; This is a simple interface to the speak package from Brown University.
;; Speak can be ftped from the machine wilma.cs.brown.edu.

;; Interesting functions:

;; M-x speak             Prompt for a string and speak it
;; M-x speak-region      Speak the current region
;; M-x speak-buffer      Speak the current buffer

;; The next three variables are interesting:

;; Make sure scat is in your path or set this to the full path in your .emacs.
(defvar speak-program "scat"
  "*Name of the speak executable.")

(defvar speak-phoneme-directory nil
  "*Directory to look for phonemes.  nil mean use default directory.")

(defvar speak-volume nil
  "*Speech volume from 0.0 to 1.0.  Changes to this variable will (currently)
not change the speech volume after the speech process has started.  nil
means use the default volume of speak-program.")

;; Casual users probably don't care about anything below this.

(defvar speak-phoneme-dir-flag "-d"
  "Option flag used to set the phoneme directory.")

(defvar speak-volume-flag "-v"
  "Option flag used to set the speech volume.")

(defvar speak-process nil
  "Process of the speak-program.")

(defun speak (string)
  (interactive "sSpeak: ")
  "Speak STRING.  When used interactively prompts for string."
  (speak-start-process)
  (process-send-string speak-process (concat string "\n")))

(defun speak-region (start end)
  (interactive "r")
  "Speak the text in region.
When called from a program takes two arguments: START and END."
  (speak-start-process)
  (process-send-region speak-process start end)
  (process-send-string speak-process "\n"))

(defun speak-buffer (&optional buffer)
  (interactive)
  "Speak the text in the current buffer.
When called from a program takes optional arg BUFFER."
  (if buffer
      (save-excursion
	(set-buffer buffer)
	(speak-region (point-min) (point-max)))))

(defun speak-start-process ()
  "Start the speak process if it isn't already running."
  (if (or (eq speak-process nil)
	  (not (eq (process-status speak-process) 'run)))
      (if (file-exists-p speak-program)
	  (let ((args (list speak-program)))
	    ;; Compute arguments
	    (message "Starting the speak process...")
	    (if speak-phoneme-directory
		(setq args (append args (list speak-phoneme-dir-flag
					      speak-phoneme-directory))))
	    (if speak-volume
		(setq args (append (args (list speak-volume-flag
					       speak-volume)))))
	    (setq speak-process (apply 'start-process "speak" nil args))
	    (setq process-sentinel speak-process speak-process-sentinel)
	    (message "Starting the speak process...done."))
	(error "Can't find speak executable: %s." speak-program))))

(defun speak-process-sentinel (proc status)
  "Reset the speak-process variable if the state changes."
  (message "Speak process exited with status %s" status)
  (setq speak-process nil))

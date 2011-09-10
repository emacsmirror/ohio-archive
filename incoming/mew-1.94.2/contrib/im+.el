;;;
;;; IM+
;;;
;;; Created  : Jan. 11, 1998
;;; Version  : 1.0
;;; Author   : ASAMI Tomoharu (tasami@ibm.net)
;;; HomePage : http://www.netpassport.or.jp/~wtasami
;;;
;
; [Minimum Setup]
;
; (autoload 'im+-show-imput-queue-status "im+" nil t)
; (autoload 'im+-show-imput-queue "im+" nil t)
; (autoload 'im+-xfer "im+" nil t)
;
; or
;
; (require 'im+)
;
; If you want to display the imput queue status on the status line from
; the startup, you should use the latter setting, or "require".
;
; [Using in Mew]
;
; (add-hook 'mew-init-hook
; 	  (function
; 	   (lambda ()
; 	     (define-key mew-summary-mode-map "b" (make-sparse-keymap))
; 	     (define-key mew-summary-mode-map "bs" 'im+-show-imput-queue-status)
; 	     (define-key mew-summary-mode-map "bq" 'im+-show-imput-queue)
; 	     (define-key mew-summary-mode-map "bx" 'im+-xfer))))
;

;
; hooks
;

(defvar im+-before-xfer-hook nil
  "*Hook called before message transfer.")
(defvar im+-after-xfer-hook nil
  "*Hook called after message transfer.")

;
; configuration
;

(defvar im+-imput "imput"
  "*Full path name of the imput command")
(defvar im+-imput-show-queue (concat im+-imput " -bp")
  "*Command with arguments to show the message queue")
(defvar im+-imput-xfer (concat im+-imput " -q")
  "*Command with arguments to send messages")
(defvar im+-pop-xfer nil
  "*Command with arguemnts to receive messages")
(defvar im+-status-line t
  "*If non-nil, show imput queue status on status line")
(defvar im+-get-number-of-queuing-mails-function
  (function im+-get-number-of-queuing-mails-by-file)
  "*Function to get number of queuing mails")
(defvar im+-get-queuing-dir "~/.im/queue/"
  "*Imput queue directory")
(defvar im+-imget-bp-regex "Message queued in"
  "*Regular expression to get number of queuing mails by imput with bp option")

;
; public interfaces
;

(defun im+-show-imput-queue-status ()
  (interactive)
  (let (num)
    (setq num (im+-get-number-of-queuing-mails))
    (cond ((eq num 0)
	   (message "Imput queue is empty."))
	  ((eq num 1)
	   (message "Imput queue has 1 message to send."))
	  (t
	   (message (format "Imput queue has %s messages to send." num))))))

(defun im+-show-imput-queue ()
  (interactive)
  (im+-show-from-command-line "*imput queue*" im+-imput-show-queue))

(defun im+-xfer ()
  (interactive)
  (cond (im+-before-xfer-hook
	 (message "Prepare transfer...")
	 (run-hooks 'im+-before-xfer-hook)))
  (message "Send messages...")
  (im+-get-string-from-command-line im+-imput-xfer)
  (cond (im+-pop-xfer
	 (message "Receive messages...")
	 (im+-get-string-from-command-line im+-pop-xfer)))
  (cond (im+-after-xfer-hook
	 (message "Cleanup...")
	 (run-hooks 'im+-after-xfer-hook)))
  (message "Done."))

;
; subroutines
;

(defun im+-get-string-from-command-line (command-line)
  (apply (function im+-get-string-from-command)
	 (im+-make-list-from-string command-line)))

(defun im+-get-string-from-command (command &rest arg)
  (let (buffer command-line string)
    (setq buffer (get-buffer-create "???im+-command???"))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (setq command-line (append (list command nil buffer nil) arg))
      (apply 'call-process command-line)
      (goto-char (point-min))
      (setq string (buffer-string)))
    (kill-buffer buffer)
    string))

(defun im+-make-list-from-string (string &optional pattern)
  (let (buffer list)
    (if (null pattern)
	(setq pattern "[^ \t\n\f]+"))
    (setq buffer (get-buffer-create "???im+-temp???"))
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward pattern nil t)
	(setq list (cons
		    (buffer-substring (match-beginning 0) (match-end 0))
		    list))))
    (kill-buffer buffer)
    (nreverse list)))

(defun im+-show-from-command-line (name command-line)
  (apply (function im+-show-from-command)
	 (cons name (im+-make-list-from-string command-line))))

(defun im+-get-number-of-queuing-mails ()
  (apply im+-get-number-of-queuing-mails-function nil))

(defun im+-get-number-of-queuing-mails-by-file ()
  (let ((flag t)
	(num 1))
    (while flag
      (and (setq flag (file-exists-p (concat im+-get-queuing-dir num)))
	   (setq num (+ num 1))))
    (- num 1)))

(defun im+-get-number-of-queuing-mails-by-imput ()
  (let (string)
    (setq string (im+-get-string-from-command-line im+-imput-show-queue))
    (setq string (im+-make-list-from-string string "Message queued in"))
    (length string)))

; im+-show mode

(defun im+-show-from-command (name command &rest arg)
  (let (buffer command-line)
    (setq buffer (get-buffer-create name))
    (switch-to-buffer buffer)
    (erase-buffer)
    (setq major-mode 'im+-show-mode)
    (setq mode-name "Im+-Show")
    (use-local-map im+-show-mode-map)
    (setq buffer-read-only nil)
    (setq command-line (append (list command nil buffer nil) arg))
    (apply (function call-process) command-line)
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (goto-char 0)
    (message "q)uit  x)fer  u)pdate")))

(setq im+-show-mode-map (make-sparse-keymap))
(define-key im+-show-mode-map "q" 'im+-show-quit)
(define-key im+-show-mode-map "x" 'im+-show-xfer)
(define-key im+-show-mode-map "u" 'im+-show-update)

(defun im+-show-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun im+-show-xfer ()
  (interactive)
  (im+-xfer)
  (kill-buffer (current-buffer))
  (im+-show-imput-queue))

(defun im+-show-update ()
  (interactive)
  (kill-buffer (current-buffer))
  (im+-show-imput-queue))

; status line

(defun im+-imput-queue-status-line ()
  (let (num)
    (setq num (im+-get-number-of-queuing-mails))
    (cond ((> num 0)
	   (format " MailQ[%s]" num))
	  (t
	   ""))))

(cond (im+-status-line
       (add-hook 'display-time-hook
		 (function
		  (lambda ()
		    (setq display-time-string
			  (concat display-time-string
				  (im+-imput-queue-status-line))))))))

;

(provide 'im+)

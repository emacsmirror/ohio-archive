;To: unix-emacs@bbn.com
;Date: 21 Feb 89 10:30:31 GMT
;From: John Sturdy <mcvax!ukc!strath-cs!glasgow!icdoc!qmc-cs!harlqn!jcgs@uunet.uu.net>
;Subject: Re: a better lpr.el for GNU Emacs 18
;
;And here's some stuff for selecting printers:
;;; print-select.el
;;; Last edited: Tue Apr  5 17:13:40 1988 by jcgs (John Sturdy) on harlqn
;;; controlling the printer flags; switch on or off the banners, and change
;;; the current printer

(provide 'print-select)

(defvar current-printer "lp"
  "The most recently used printer - at a guess. Set to \"lp\" when a
printer mode is selected, and to \"LaserWriter\" after an \"enscript\"
operation. Used by \"lpq\". Use \"select-printer\" to set interactively -
this provides completion over the printer names defined on this system.")

(defvar current-printer-lpr-arg (concat "-P" current-printer)
  "The name of the current printer, with \"-P\" prepended to it, for passing
to Unix utilities such as \"lpr\" and \"lpq\".")

(defun print-banner-on ( )
  "Enable printing banner for print-buffer, print-region etcetera"
  (interactive)
  (setq lpr-switches (delequal "-h" lpr-switches)))

(defun print-banner-off ( )
  "Disable printing banner for print-buffer, print-region etcetera"
  (interactive)
  (print-banner-on)                     ; ensure only one copy of flag
  (setq lpr-switches (cons "-h" lpr-switches)))

(print-banner-off)                      ; my personal preference

;;; examining the printer queues

(defun lpq (&optional all-users)
  "Display the printer queue for the your own entries; with a prefix
argument, display entries for all users."
  (interactive "P")
  (let
      (
       (buffer-name (concat "*" current-printer " queue*"))
       (old-buffer (current-buffer))
       )
    (with-output-to-temp-buffer
        buffer-name
      (set-buffer (get-buffer buffer-name))
      (message (buffer-name))
      (shell-command
       (concat "lpq " current-printer-lpr-arg
               (if all-users
                   ""
                 (concat " " (user-login-name))))
       t))
    (set-buffer old-buffer)))

(defvar printer-name-alist nil
  "alist containing in the car parts the name strings for all printers
on this system. Used for completion in selecting a printer.")

(defun select-printer ()
  "Set the current printer to one chosen by the user. Completion on printer
names is done using \"/etc/printcap\" to provide the printer names."
  (interactive)
  (if (null printer-name-alist) (get-printer-names))
  (setq lpr-switches (delequal current-printer-lpr-arg lpr-switches))
  (setq current-printer
        (completing-read "Choose printer: "
                         printer-name-alist
                         nil            ; predicate
                         t))            ; requires match
  (setq current-printer-lpr-arg (concat "-P" current-printer))
  (setq lpr-switches (cons current-printer-lpr-arg lpr-switches))
  (save-window-excursion
    (message "Checking that printer %s is OK" current-printer)
    (set-buffer (get-buffer-create " *printer selection check*"))
    (erase-buffer)
    (shell-command (concat "lpq " current-printer-lpr-arg) t)
    (goto-char (point-min))
    (end-of-line) (forward-char)
    (if (re-search-backward "off\\|disabled" (point-min) t) ; BUG?!?!?
        (message "Problem with %s: %s"
                 current-printer (first-line-of-buffer))
      (message "Printer %s selected" current-printer))
    (kill-buffer (current-buffer))))

(defun add-printer (printer)
  "Add PRINTER to the list of printers used for completion in select-printer."
  (setq printer-name-alist (cons
                            (cons printer nil)
                            printer-name-alist)))

(defun get-printer-names ()
  "Find all the valid printer names from \"/etc/printcap\"."
  (save-window-excursion
    (message "Reading /etc/printcap...")
    (set-buffer (get-buffer-create " *printcap file*"))
    (setq printer-name-alist nil)       ; empty the name list
    (erase-buffer)                      ; clear the buffer
    (insert-file-contents "/etc/printcap" t) ; t marks as unmodified
    (goto-char (point-min))
    (while (re-search-forward "^[^# \t]" (point-max) t)
                                        ; scan for lines with printer names
      (beginning-of-line)
      (while (not (eq (char-after (- (point) 1)) ?\:))
                                        ; next line after last name on this one
        (let ((start (point)))
          (re-search-forward "[:|]" (point-max) t) ; get next name on line
          (add-printer (buffer-substring start (- (point) 1))))))
    (message "Reading /etc/printcap... done")
    (kill-buffer (current-buffer))))

;;; end of print-select.el
;
;
;--
;__John            When asked to attend a court case, Father Moses took with him
;          a leaking jug of water. Asked about it, he said: "You ask me to judge
;               the faults of another, while mine run out like water behind me."
;
;                jcgs@uk.co.harlqn (UK notation) jcgs@harlqn.co.uk (most places)
;    ...!mcvax!ukc!harlqn!jcgs (uucp - really has more stages, but ukc knows us)
;John Sturdy                                            Telephone +44-223-872522
;                      Harlequin Ltd, Barrington Hall, Barrington, Cambridge, UK


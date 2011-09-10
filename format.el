;; format.el
;; Copyright 1989 Ronald Florence (ron@mlfarm)

(defvar roff-macro "-mm"
"*Default macro package to use with troff and nroff.")

(defvar troff-options "-rN2"
"*Default options to use with troff.")

(defvar nroff-options "-rN2 -rO9"
"*Default options to use with nroff.")

(setq troff-format-string "\(troff -t %s %s %s 2>&1 \) | lp -ot -n%d")
(setq nroff-format-string "nroff -Thp %s %s %s | lp -n%d")
(setq pr-format-string "pr -h %s %s | lp -n%d")

(if (not nroff-mode-map)
    (error "Nroff-mode is not loaded.")
  (progn
    (define-key nroff-mode-map "\C-c\C-n" 'proof-buffer)
    (define-key nroff-mode-map "\C-c\C-t" 'tproof-buffer)
    (define-key nroff-mode-map "\C-c\C-k" 'kill-proof)
    (define-key nroff-mode-map "\C-c\C-i" 'kill-print)))

(setq proof-tmp-file nil 
      print-tmp-file nil
      proof-process nil 
      print-process nil
      proof-file nil)

(defun nroff-buffer (&optional copies)
  "Print buffer contents after formatting with nroff.
Optional prefix argument specifies number of copies."
  (interactive "p")
  (format-to-printer-region (point-min) (point-max) "nroff" copies))

(defun nroff-region (start end &optional copies)
  "Print region contents after formatting with nroff.
Optional prefix argument specifies number of copies."
  (interactive "r\np")
  (format-to-printer-region start end "nroff" copies))

(defun troff-buffer (&optional copies)
  "Typeset buffer after formatting with troff.
Optional prefix argument specifies number of copies."
  (interactive "p")
  (format-to-printer-region (point-min) (point-max) "troff" copies))

(defun troff-region (start end &optional copies)
  "Typeset region contents after formatting with troff.
Optional prefix argument specifies number of copies."
  (interactive "r\np")
  (format-to-printer-region start end "troff" copies))

(defun pr-buffer (&optional copies)
  "Print buffer contents after formatting with pr.
Optional prefix argument specifies number of copies."
  (interactive "p")
  (format-to-printer-region (point-min) (point-max) "pr" copies))

(defun pr-region (start end &optional copies)
  "Print region contents after formatting with pr.
Optional prefix argument specifies number of copies."
  (interactive "r\np")
  (format-to-printer-region start end "pr" copies))

(defun proof-region (start end)
  "Proof region using nroff."
  (interactive "r")
  (proof-region-to-buffer start end "nroff"))

(defun proof-buffer ()
  "Proof buffer using nroff."
  (interactive)
  (proof-region-to-buffer (point-min) (point-max) "nroff"))

(defun tproof-region (start end)
  "Rough proof region using troff."
  (interactive "r")
  (proof-region-to-buffer start end "troff"))

(defun tproof-buffer ()
  "Rough proof buffer using troff."
  (interactive)
  (proof-region-to-buffer (point-min) (point-max) "troff"))

(defun kill-print ()
  "Kill format-to-printer process."
  (interactive)
  (if print-process
      (interrupt-process print-process)))

(defun kill-proof ()
  "Kill proof process."
  (interactive)
  (if proof-process
      (interrupt-process proof-process)))

(defun format-to-printer-region (start end formatter &optional copies)
  (if print-process
      (if (or 
	   (not (eq (process-status print-process) 'run))
	   (yes-or-no-p "A format-to-printer process is running; kill it? "))
	  (condition-case ()
	      (let ((print-proc print-process))
		(interrupt-process print-proc)
		(sit-for 1)
		(delete-process print-proc))
	    (error nil))
	(error "One format-to-printer process at a time.")))
  (save-excursion
    (setq printer-output-buffer " *printer output*")
    (get-buffer-create printer-output-buffer)
    (set-buffer printer-output-buffer)
    (erase-buffer))
  (if (null copies) (setq copies 1))
  (setq print-tmp-file (concat "/tmp/" (make-temp-name "#pr#")))
  (write-region start end print-tmp-file nil 'nomsg)
  (setq print-command 
	(cond ((string= formatter "troff")
	       (format troff-format-string
		       troff-options roff-macro
		       print-tmp-file copies))
	      ((string= formatter "nroff") 
	       (format nroff-format-string
		       nroff-options roff-macro
		       print-tmp-file copies))
	      ((string= formatter "pr")
	       (format pr-format-string
		       (buffer-name) print-tmp-file copies))))
  (setq print-process
	(start-process formatter printer-output-buffer "sh" "-c"
		       print-command))
  (set-process-sentinel print-process 'print-sentinel))

(defun print-sentinel (process msg)
  (delete-file print-tmp-file)
  (save-excursion
    (set-buffer (process-buffer process))
    (if (> (buffer-size) 0)
	(progn
	  (goto-char (point-min))
	  (end-of-line)
	  (message "%s: %s" (process-name process) 
		   (buffer-substring 1 (point))))
      (message "%s: killed" (process-name process))))
  (setq print-process nil)
  (kill-buffer (process-buffer process)))

(defun proof-region-to-buffer (start end formatter)
  (if proof-process
      (if (or (not (eq (process-status proof-process) 'run))
	      (yes-or-no-p "A proof process is running; kill it? "))
	  (condition-case ()
	      (let ((proof-proc proof-process))
		(interrupt-process proof-proc)
		(sit-for 1)
		(delete-process proof-proc))
	    (error nil))
	(error "One proof process at a time.")))
  (setq proof-tmp-file (concat "/tmp/" (make-temp-name "#p#")))
  (save-excursion
    (setq proof-file (buffer-name))
    (setq proof-buffer "*proof*")
    (get-buffer-create proof-buffer)
    (set-buffer proof-buffer)
    (erase-buffer))
  (write-region start end proof-tmp-file nil 'nomsg)
  (setq proof-command 
	(if (string= formatter "troff") 
	    (format "troff -a %s %s %s" troff-options
		    roff-macro proof-tmp-file)
	  (format "nroff %s %s %s" nroff-options roff-macro proof-tmp-file)))
 (setq proof-process
       (start-process formatter proof-buffer "sh" "-c" proof-command))
 (set-process-sentinel proof-process 'proof-sentinel))

(defun proof-sentinel (process msg)
  (delete-file proof-tmp-file)
  (if (string-match "^exited" msg)
      (message "%s: killed" (process-name process))
    (progn
      (set-buffer (process-buffer process))
      (text-mode)
      (setq mode-name (format "%s:%s"
			      (process-name proof-process) proof-file))
      (if (string= (process-name process) "nroff")
	  (zap-nroff-crap))
      (goto-char (point-min))
      (display-buffer (process-buffer process))))
  (setq proof-process nil))
	
(defun zap-nroff-crap ()
  (goto-char (point-min))
  (while (search-forward "\b" nil t)
    (let* ((preceding (char-after (- (point) 2)))
	   (following (following-char)))
      	    ;; x\bx
      (cond ((= preceding following)	
	     (delete-char -2))
	    ;; _\b
	    ((= preceding ?\_)		
	     (delete-char -2))
	    ;; \b_
	    ((= following ?\_)		
	     (delete-region (1- (point)) (1+ (point)))))))
  ;; expand ^G lines
  (goto-char (point-min))
  (while (search-forward "\C-g" nil t)	
    (delete-char -2)
    (while (not (eolp))
      (insert " ")
      (forward-char 1)))
  ;; zap Esc-8 & Esc-9 vertical motions
  (goto-char (point-min))
  (while (search-forward "\e" nil t)
    (if (or (= (following-char) ?8) (= (following-char) ?9))
	    (delete-region (1+ (point)) (1- (point))))))





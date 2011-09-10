;From: kadmon!jason@mtxinu.com (Jason Venner)
;To: gnu-emacs@prep.ai.mit.edu
;Subject: simple code to toggle between flow control and no flowcontrole
;Date: Tue, 28 Feb 89 21:22:04 -0800


;; The function flow-lossage toggles between flow control and no flow
;; controll, rebinding C-s and C-q to flow-ctrl-s-sub and flow-ctrl-q-sub
;; and the reverse on completion.
;; If the non flow control map is standard, it is nil'd

(defvar	flow-lossage-flag	nil
  "By default, we are not it flow-lossage mode.
If t, are doing flow controll lossage.")
(defvar	flow-ctrl-s ?\C-s
  "Control-S.")
(defvar	flow-ctrl-q ?\C-q
  "Control-Q.")
(defvar	flow-ctrl-s-sub ?\C-^
  "*Char to substitute for Control-S.")
(defvar	flow-ctrl-q-sub ?\C-\
  "*Char to substitute for Control-Q.")
  

(defun flow-lossage ()
  "Toggle between flow control and non flow control mode.
Rebinds C-s/C-q to C-^ and C-\ via a keyboard-translate-table map
and vis veras on the return."
  (interactive "")
  (if flow-lossage-flag
		(progn
		  (set-input-mode t nil)			 ;interupt and no flow control
		  (setq keyboard-translate-table (make-key-table))
		  (aset keyboard-translate-table flow-ctrl-s-sub flow-ctrl-s-sub)
		  (aset keyboard-translate-table flow-ctrl-q-sub flow-ctrl-q-sub)
		  (check-std-key)
		  (setq flow-lossage-flag nil)
		  (message "No more flow control!."))
	 (set-input-mode nil t)
	 (setq keyboard-translate-table (make-key-table))
	 (aset keyboard-translate-table flow-ctrl-s-sub flow-ctrl-s)
	 (aset keyboard-translate-table flow-ctrl-q-sub flow-ctrl-q)
	 (setq flow-lossage-flag t)
	 (message "Flow control enabled")))

(defun make-key-table ()
  "Return a copy of the current keyboard-translate-table."
  (let ((new (make-string 256 0)) (i 0))
	 (while (< i (length new))
		(aset new i (if keyboard-translate-table
							 (aref keyboard-translate-table i)
						  i))
		(setq i (1+ i)))
	 new))

(defun check-std-key ()
  "If the keyboard-translate-table is vanilla, set it to nil."
  (let ((i 0) (len (length keyboard-translate-table)) (std t))
	 (while (< i len)
		(if (= (aref keyboard-translate-table i) i)
			 nil
		  (setq std nil)
		  (setq i len)
		  )
		(setq i (1+ i)))
	 (if std
		  (setq keyboard-translate-table nil))))
		  


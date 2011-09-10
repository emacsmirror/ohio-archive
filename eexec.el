;;; Postscript eexec support routines
;;; LastEditDate "Fri May 19 08:57:31 1989"
;;; Copyright (c) 1989 by Randal L. Schwartz. All Rights Reserved.
;;; This code may be freely distributed according to the GNU Public License

(defconst hex-string-to-int-table
  (let ((ht (make-vector 256 nil)))	; nil gives error at + if out-of-range
    (mapcar (function (lambda (pair) (aset ht (nth 0 pair) (nth 1 pair))))
	    '((?0 0) (?1 1) (?2 2) (?3 3) (?4 4)
	      (?5 5) (?6 6) (?7 7) (?8 8) (?9 9)
	      (?a 10) (?b 11) (?c 12) (?d 13) (?e 14) (?f 15)
	      (?A 10) (?B 11) (?C 12) (?D 13) (?E 14) (?F 15)))
    ht)
  "Table used by hex-string-to-int.")

(defun hex-string-to-int (str)
  "Convert STRING to an integer by parsing it as a hexadecimal number."
  (let ((result 0))
    (mapcar
     (function (lambda (ch)
		 (setq result (+ (lsh result 4)
				 (aref hex-string-to-int-table ch)))))
     str)
    result))

(defun byte-to-hex-string (byte)
  "Convert BYTE to a two-chararacter string by printing it in hexadecimal."
  (format "%02x" byte))

(defconst eexec-const-init (hex-string-to-int "d971")
  "Used by eexec-endecode.  Initial value for state machine.")

(defconst eexec-const-mult (hex-string-to-int "ce6d")
  "Used by eexec-endecode.  Multiplier value for state machine.")

(defconst eexec-const-add (hex-string-to-int "58bf")
  "Used by eexec-endecode.  Adder value for state machine.")

(defconst eexec-const-seed (concat (mapcar 'hex-string-to-int
					   '("17" "ec" "9c" "f3")))
  "Used by eexec-encode.  A known good seed from uartpatch.ps.")

(defun eexec-decode ()
  "Decode the first eexec string in the current (possibly narrowed) buffer.
Result is displayed in a temp buffer."
  (interactive)
  (with-output-to-temp-buffer "*eexec-decode-output*"
    (goto-char (point-min))
    (search-forward "eexec")
    (let (str)
      (while (re-search-forward "[ \t\n]*\\([0-9a-fA-F][0-9a-fA-F]\\)" nil t)
	(setq str (cons (buffer-substring (match-beginning 1)
					  (match-end 1))
			str)))
      (setq str (nreverse str))
      (setq str (eexec-endecode (concat (mapcar 'hex-string-to-int str))))
      (princ "Seed: ")
      (princ (apply 'concat (mapcar 'byte-to-hex-string (substring str 0 4))))
      (princ "\nText:\n")
      (princ (substring str 4)))))

(defun eexec-encode (start end &optional seed)
  "Encode text from START to END (region if interactive).
Result is displayed in a temp buffer.  If optional SEED is passed as a
four-character string, use it for initial state, else use the known
good seed (the current value of eexec-const-seed)."
  (interactive "r")
  (with-output-to-temp-buffer "*eexec-encode-output*"
    (let ((i 0))
      (princ "currentfile eexec\n")
      (mapcar (function
	       (lambda (ch)
		 (princ (byte-to-hex-string ch))
		 (if (< (setq i (1+ i)) 32) nil
		   (princ "\n")
		   (setq i 0))))
	      (eexec-endecode
	       (concat (or (and (stringp seed)
				(= (length seed) 4)
				seed)
			   eexec-const-seed)
		       (buffer-substring start end))
	       t))
      (if (> i 0) (princ "\n")))))

(defun eexec-endecode (str &optional encode)
  "Decode STR (or encode if optional ENCODE is non-nil), returning result.
If decoding, you will probably want to toss the first four bytes,
but they are returned anyway so that you may reencode a decoded string
for verification."
  (let ((state eexec-const-init) outbyte)
    (concat
     (mapcar
      (function
       (lambda (inbyte)
	 (setq outbyte (logxor inbyte (logand (lsh state -8)))
	       state (logand 65535 (+ state (if encode outbyte inbyte)))
	       state (logand 65535 (* state eexec-const-mult))
	       state (logand 65535 (+ state eexec-const-add)))
	 outbyte))
      str))))

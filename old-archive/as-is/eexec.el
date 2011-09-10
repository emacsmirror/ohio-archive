;To: unix-emacs@bbn.com
;Date: 6 May 89 18:51:06 GMT
;From: "Randal L. Schwartz @ Stonehenge" <ogccse!littlei!omepd!merlyn@husc6.harvard.edu>
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: updated tools (was Re: Postscript eexec tools in GNU Emacs (nits and picks))
;Reply-To: "Randal L. Schwartz @ Stonehenge" <merlyn@intelob.intel.com>
;References: <8905050031.AA07824@dsys.icst.nbs.gov>
;Organization: Stonehenge; netaccess via BiiN, Hillsboro, Oregon, USA
;Source-Info:  From (or Sender) name not authenticated.
;
;In article <8905050031.AA07824@dsys.icst.nbs.gov>, rbj@dsys (Root Boy Jim) writes:
;| [a bunch of stuff about how to do it faster and better]
;
;After I posted the crude code, I did a huge number of iterative
;optimizations (as thought experiments, since I don't have a good way
;of profiling Elisp code).  The result is attached after the signature.
;No new functionality, unless you think speed is a function :-).
;-- 
;/=Randal L. Schwartz, Stonehenge Consulting Services (503)777-0095===\
;{ on contract to BiiN, Hillsboro, Oregon, USA, until 14 May 1989     }
;{ <merlyn@intelob.intel.com> ...!uunet!tektronix!biin!merlyn         }
;{ or try <merlyn@agora.hf.intel.com> after 15 May 1989               }
;\=Cute quote: "Welcome to Oregon... home of the California Raisins!"=/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; snip snip
;;; Postscript eexec support routines
;;; LastEditDate "Sat May  6 10:41:14 1989"
;;; Copyright (c) 1989 by Randal L. Schwartz. All Rights Reserved.
;;; This code may be freely distributed according to the GNU Public License

(defun hex-string-to-int (str)
  "Convert STRING to an integer by parsing it as a hexadecimal number."
  (if (string-match "^[0-9a-fA-F]+$" str)
      (let ((result 0))
	(mapcar
	 (function (lambda (ch)
		     (setq result (+ (lsh result 4)
				     (string-match (char-to-string ch)
						   "0123456789abcdef")))))
	 (downcase str))
	result)
    (error "hex-string-to-int: `%s' is not a hex number" str)))

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
Result is displayed in a temp buffer.  If optional SEED is passed as
a four-character string, use it for initial state, else use the known
string from the uartpatch.ps file '17ec9cf3'."
  (interactive "r")
  (with-output-to-temp-buffer "*eexec-encode-output*"
    (let ((i 0))
      (princ "currentfile eexec\n")
      (mapcar (function (lambda (ch)
			  (princ (byte-to-hex-string ch))
			  (if (< (setq i (1+ i)) 32) nil
			    (princ "\n")
			    (setq i 0))))
	      (eexec-endecode (concat (or (and (stringp seed)
					       (= (length seed) 4)
					       seed)
					  eexec-const-seed)
				      (buffer-substring start end)) t))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; snip snip


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; kermit.el - transfer buffers to and from emacs with kermit protocol
;;;
;;; Bob Manson <manson@piglet.cr.usgs.gov>
;;; Ben Mesander <ben@gnu.ai.mit.edu>
;;;
;;; LCD Archive Entry:
;;; kermit|Ben A. Mesander|ben@gnu.ai.mit.edu|
;;; Transfer buffers to and from Emacs with kermit protocol.|
;;; 04-Jun-1994|1.4|~/misc/kermit.el.Z|
;;;
;;; $Revision: 1.4 $
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; README
;;;
;;; This lisp library adds three commands:
;;; kermit-send-current-buffer sends the current buffer to stdio
;;;     with kermit protocol.
;;; kermit-send-buffer sends its argument (a buffer or buffer name ) 
;;;     to stdio with kermit protocol.
;;; kermit-receive-buffer receives a file you send to emacs's stdio
;;;     with kermit protocol.
;;;
;;; All transfers are done in image mode if `kermit-text-mode' is nil.
;;; Otherwise, transfers are done in ASCII mode.
;;; All transfers are done assuming a 7-bit data path - working over
;;; emacs's stdio is enough of a hack without complicating things further.
;;;
;;; To abort a transfer, connect to your emacs, send a C-g, and kill
;;; the blank buffer (C-x C-k RET), and repaint the screen with C-l .
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Wow.  I am surprised, and repulsed! - jwz@lucid.com

;;;
;;; NOTE: All lines must be less than 80 characters in length in order
;;; to conform to the requirements of the kermit archive.
;;;

;;; Code:

(require 'backquote)

;; USER SETTABLE VARIABLES

(defvar kermit-debugging nil
  "Set to t in order to log kermit packet debugging information in a buffer
    named `kermit-debugging'.")

(defvar kermit-read-char-timeout 5
  "How long to wait to read a character before timing out")

(defvar kermit-mark-char "\C-a"
  "Control character used to mark the start of a packet.")

(defvar kermit-line-terminator "\C-m"
  "Line terminator to use at the end of every packet.")

(defvar kermit-max-packet-len 94
  "The maximum size packet kermit can send (up to 94 characters).")

(defvar kermit-send-quotechar "#"
  "Character kermit uses to quote characters")

(defvar kermit-quote-8bit-char "&"
  "Character kermit uses to quote 8 bit characters")

(defvar kermit-text-mode nil
  "Set to t to do text mode file transfers")

;; END OF USER SETTABLE VARIABLES

;; buffer emacs uses to transfer characters from/to
(defvar kermit-transfer-buffer nil)

;; buffer emacs uses to blank the screen with
(defvar kermit-blank-buffer nil)

;; prefix encoding table
(defvar kermit-prefix-table nil)

;; prefix encoding length table
(defvar kermit-prefix-table-len nil)

;; name of buffer kermit is transferring
(defvar kermit-filename nil)

;; accumulator for packet checksum
(defvar kermit-checksum-total 0)

;; return value from character read functions
(defvar kermit-gotten-char nil)

;; number of times calling input-pending-p is 1 second
(defvar kermit-delay-count 0)

;; sequence number of packet read
(defvar kermit-receive-seq nil)

;; sequence number of packet sent
(defvar kermit-seq-number nil)

;; length of received packet
(defvar kermit-packet-len nil)

;; kermit packet being built
(defvar kermit-packet-in-progress nil)

;; packet type of packet read
(defvar kermit-packet-type nil)

;; global kermit packet
(defvar kermit-packet nil)

;; used to decode data
(defvar kermit-prefix-add 0)

;; flag used to decode data
(defvar kermit-got-stdquote nil)

(defmacro kermit-do-log (inlist)
  (` (and kermit-debugging 
	  (kermit-do-real-log (, inlist)))))

;;
;; Transform c, which is assumed to lie in the range 0 to 94
;; into a printable ASCII character; 0 becomes SP, 1 becomes "!", 3 becomes
;; "#", etc.
;;
(defmacro kermit-tochar (c) "Converts a number into an ASCII character."
  (list 'char-to-string (list '+ c 32)))

;;
;; Transforms the character c, which is assumed to be in the printable range
;; (SP through tilde), into an integer in the range 0 to 94.
;;
(defmacro kermit-fromchar (c) 
  "Converts a character into its integer equivalent."
  (list '- (list 'string-to-char c) 32))

;; Call FUN with an argument of each character in STRING, and concat the 
;; results, if DOCONCAT is non-nil.
(defun kermit-mapstring (fun string &rest doconcat) 
  (let (
	(result "")
	(index 0)
	(slength (length string)))
    (if doconcat
	(while (< index slength)
	  (setq result
		(concat result
			(apply fun (list (aref string index)))))
	  (setq index (+ index 1)))
      (while (< index slength)
	(apply fun (list (aref string index)))
	(setq index (+ index 1))))
      
    result))

(defun kermit-checksum-add (i) "add I to the checksum"
  (setq kermit-checksum-total (+ kermit-checksum-total i))
  nil)

;; Return a single-character checksum based on kermit-checksum-total.

(defun kermit-checksum-char () 
  (setq kermit-checksum-total (mod kermit-checksum-total 256))
  (kermit-tochar (logand 63 (+ (mod kermit-checksum-total 64)
			       (/ kermit-checksum-total 64)))))

(defun kermit-checksum (packet) "Generate a single byte checksum from PACKET."
  (setq kermit-checksum-total 0)
  (kermit-mapstring 'kermit-checksum-add packet nil)
  (kermit-checksum-char))

;;
;; Send a kermit packet of type TYPE, sequence number SEQUENCE, containing 
;; DATA. DATA must already be in its proper quoted form.
;;
;;Basic Kermit Packet Layout
;;
;;       |<------Included in CHECK------>|
;;       |                               |
;;+------+-----+-----+------+------ - - -+-------+
;;| MARK | LEN | SEQ | TYPE | DATA       | CHECK |<terminator>
;;+------+-----+-----+------+------ - - -+-------+
;;             |                                 |
;;             |<--------LEN-32 characters------>|
;;
;; MARK   A real control character, usually CTRL-A.
;;  LEN   One character, length of remainder of packet + 32, max 95
;;  SEQ   One character, packet sequence number + 32, modulo 64
;; TYPE   One character, an uppercase letter
;;CHECK   One, two, or three characters, as negotiated.
;;
;;<terminator>  Any control character required for reading the packet.
;;
(defun kermit-send-packet (type sequence data) 
  (let (thepacket)
    (kermit-discard-input)
    ;; NOTE: + 3 will have to be changed if
    ;; extended-length packets are ever implemented.
    (setq thepacket (concat (kermit-tochar (+ 3 (length data)))
			    (kermit-tochar sequence) type data))
    (setq thepacket (concat kermit-mark-char thepacket (kermit-checksum 
							thepacket)
			    kermit-line-terminator))
    (send-string-to-terminal thepacket)
    (kermit-do-log (concat "sent: " thepacket)))
  t)

;;
;; Transforms the character `data' back and forth between their printable
;; and control representations, preserving the high order bit. A becomes
;; control-A, and vice versa, etc.
;;
(defun kermit-ctl (data)
  (if (not (equal data (string-to-char kermit-send-quotechar)))
      (char-to-string (logxor data 64))
    kermit-send-quotechar))

(setq kermit-prefix-table (make-vector 256 ""))
(setq kermit-prefix-table-len (make-vector 256 0))

(defun kermit-prefix (c) "Do an aref insead of all that nastiness."
  (aref kermit-prefix-table c))

(defmacro kermit-size-of-char (chari) 
  "Tells how many bytes CHAR will use when transmitted."
  (list 'aref 'kermit-prefix-table-len chari))

(defun init-kermit-prefix () 
  (let ((i))
    (setq i 0)
    (while (< i 256) (progn
		       (aset kermit-prefix-table i (kermit-prefix-slow i))
		       (aset kermit-prefix-table-len i 
			     (length (aref kermit-prefix-table i)))
		       (setq i (1+ i))))))

;; Handle quoting. Given a single character DATA, return a string that will 
;; represent DATA in kermit.
(defun kermit-prefix-slow (c) 
  (let* ((data (char-to-string c)))
    (cond ((equal data kermit-send-quotechar) 
	   (concat kermit-send-quotechar data))
	  ((equal data kermit-quote-8bit-char)
	   (concat kermit-send-quotechar data))
	  ; map LF to CRLF if text mode
	  ((and kermit-text-mode (eq c 10))
	   (concat kermit-send-quotechar (kermit-ctl 13) 
		   kermit-send-quotechar (kermit-ctl c)))
	  ((> c 127)
	   (concat kermit-quote-8bit-char (kermit-prefix-slow (- c 128))))
	  ((or (< c 32) (= c 127))
	   (concat kermit-send-quotechar (kermit-ctl c)))
	  (t  data))))

;; Convert PACKET into a kermit-quoted packet. 
;; Returns the string containing the properly quoted packet.
(defun kermit-make-data-packet (data) 
  (kermit-mapstring 'kermit-prefix data t))

;; state 0 - unquoted char
;; state 1 - #-quoted
;; state 2 - 8th bit set (&-quoted)
;; state 3 - 8th bit + #-quoted
;; string - thing to return
(setq kermit-decode-table (make-vector 4 nil))

(defun kermit-decode-init ()
  (let ((i 0) (table nil))
    (while (< i 4)
      (aset kermit-decode-table i (setq table (make-vector 256 0)))
      (let ((ichar 0))
	(while (< ichar 256)
	  (cond ((equal i 0)
		 (aset table ichar (char-to-string ichar)))
		((equal i 1)
		 (aset table ichar (kermit-ctl ichar)))
		((equal i 2)
		 (aset table ichar (char-to-string (+ ichar 128))))
		((equal i 3)
		 (aset table ichar (char-to-string
				    (+ 
				     128 
				     (string-to-char (kermit-ctl ichar))))))
		)
	  (setq ichar (1+ ichar))
	  )
	)
      (setq i (1+ i))
      )
    )
  (if kermit-text-mode
      (aset (aref kermit-decode-table 1) (string-to-char "M") ""))
  (aset (aref kermit-decode-table 0) (string-to-char kermit-send-quotechar) 1)
  (aset (aref kermit-decode-table 0) (string-to-char kermit-quote-8bit-char) 2)
  (aset (aref kermit-decode-table 2) (string-to-char kermit-send-quotechar) 3)
  (aset (aref kermit-decode-table 1) (string-to-char kermit-quote-8bit-char)
	kermit-quote-8bit-char)
  (aset (aref kermit-decode-table 1) (string-to-char kermit-send-quotechar)
	kermit-send-quotechar)
  (aset (aref kermit-decode-table 3) (string-to-char kermit-send-quotechar)
	(char-to-string (+ 128 (string-to-char kermit-send-quotechar)))))

(defvar kermit-decode-state 0 "Current state of the decoding machine")

;; Process a single character CHAR from an input packet. 
;; Returns the next character to be appended to the resulting packet.
;; This is a lot smaller than it used to be, yes?
(defun kermit-deprefix (ichar)
  (let ((rchar (aref (aref kermit-decode-table kermit-decode-state) ichar)))
    (if (integerp rchar)
	(progn (setq kermit-decode-state rchar) nil)
      (progn (setq kermit-decode-state 0) rchar))))
		 
;; Convert the packet DATA into its proper representation (dequote it, 
;; basically). Returns a string with the data.
(defun kermit-splode-data-packet (data) 
  (kermit-mapstring 'kermit-deprefix data t))

(defun kermit-send-ack (seqnum) 
  "Send an ack for packet SEQNUM to the other end."
  (kermit-send-packet "Y" seqnum ""))

(defun kermit-send-nak (seqnum)
  (kermit-send-packet "N" seqnum ""))


(defun kermit-discard-input () "Trash input."
  (while (input-pending-p) (read-char))
  nil)

;; Read in a character from the sender, and store in kermit-gotten-char. 
;; Return T on success, NIL on timeout.
(defun kermit-do-read-char-nosum (count) 
  (if (< kermit-read-char-timeout count) nil
    (let ((dcount 0))
      (while (and (not (input-pending-p)) (< dcount kermit-delay-count))
      (accept-process-output) ; reschedule hack
	(setq dcount (1+ dcount)))
      (if (input-pending-p) (progn (setq kermit-gotten-char (read-char)) t)
	(kermit-do-read-char-nosum (+ count 1))))))

;; Read in a character from the sender, store it in kermit-gotten-char, and 
;; add it to kermit-checksum-total. Return T on success, NIL on timeout.
(defun kermit-do-read-char ()  
  (if (not (kermit-do-read-char-nosum 0)) nil
    (progn
      (setq kermit-checksum-total 
	    (+ kermit-checksum-total kermit-gotten-char))
      (setq kermit-gotten-char (char-to-string kermit-gotten-char))
      t)))

(defun kermit-do-read-packet () "Actually read it."
  (setq kermit-packet "")
  (let ((len) (kermit-len-char))
    (if (not (= (read-char) (string-to-char kermit-mark-char)))
	(progn (kermit-do-log "no kermit-mark-char!") (kermit-discard-input))
      (progn 
	(setq kermit-checksum-total 0)
	(setq kermit-packet "")
	(if (eq nil (catch 'kermit-getout
		      (if (not (kermit-do-read-char)) 
			  (throw 'kermit-getout nil))
		      (setq kermit-len-char kermit-gotten-char)
		      (setq len (- (kermit-fromchar kermit-gotten-char) 3))
		      (if (not (kermit-do-read-char)) 
			  (throw 'kermit-getout nil))
		      (setq kermit-receive-seq 
			    (kermit-fromchar kermit-gotten-char))
		      (if (not (kermit-do-read-char)) 
			  (throw 'kermit-getout nil))
		      (setq kermit-packet-type kermit-gotten-char)
		      (while (> len 0)
			(if (not (kermit-do-read-char)) 
			    (progn 
			      (kermit-do-log "short packet") 
			      (throw 'kermit-getout nil)))
			(setq kermit-packet 
			      (concat kermit-packet kermit-gotten-char))
			(setq len (1- len)))
		      (if (not (kermit-do-read-char-nosum 0)) 
			  (progn (kermit-do-log "short packet 2") 
				 (throw 'kermit-getout nil)))
		      (if (not (equal (char-to-string kermit-gotten-char)
				      (kermit-checksum-char))) 
			  (progn 
			    (kermit-do-log 
			     (concat "checksum mismatch mine is "
				     (string-to-char (kermit-checksum-char))
				     " his is " 
				     kermit-gotten-char))
			    (throw 'kermit-getout nil)))
		      (progn (kermit-do-log
				  (concat "got good packet: " kermit-mark-char
					  kermit-len-char kermit-receive-seq 
					  kermit-packet-type kermit-packet 
					  (kermit-checksum-char)))
			     (throw 'kermit-getout t))))
	    (kermit-discard-input)
	  t)
	))
    ))

(defun kermit-read-packet (count) 
  "Read one packet in, and store it in kermit-packet. Return T on success."
  (if (< 5 count) nil
    (let ((dcount 0))
      (while (and (not (input-pending-p)) (< dcount kermit-delay-count))
 	(accept-process-output)
	(setq dcount (1+ dcount)))
      (if (input-pending-p) (kermit-do-read-packet)
	(kermit-read-packet (1+ count))))))

;;
;; read initial packet & set transfer parameters - return t if successful, 
;; nil if not.
;;					 
;; 1 - maxl (max len packet) - if blank use 80
;; 2 - timeout - if blank use 5 seconds
;; 3 - number of padding chars - no padding 
;; 4 - padding characters - to be ignored if it is 0 (ctl) NUL
;; 5 - EOL char (tochar) - CR
;; 6 - QCTL control quote char - # 
;; -optional-
;; 7 - QBIN binary quote character (just send &)
;; 8 - CHKT check type (just send 1)
;; 9 - REPT repeat (just send SP)
;; 10 - ? CAPAS and beyond - ignore    
;;
(defun kermit-read-init () "Read the init packet"
  (let ((length-packet))
    (if (not (kermit-read-packet 0))
	(progn (kermit-do-log "read-packet failed") nil)
      ;; decode other kermit's init packet
      (setq length-packet (length kermit-packet))
      (kermit-do-log "read init packet")
      (if (> length-packet 0)
	  (progn
	    ;; negotiate max packet length
	    (setq kermit-max-packet-len 
		  (min kermit-max-packet-len
		       (if (eq (aref kermit-packet 0) 32)
			   80 ; space means 80 columns
			 (kermit-fromchar (char-to-string 
					   (aref kermit-packet 0))))))))
      (kermit-do-log (format "negotiated max packet length %d" 
			     kermit-max-packet-len))
      (if (> length-packet 1)
	  (progn
	    ;; negotiate timeout
	    (setq kermit-read-char-timeout
		  (max kermit-read-char-timeout
		       (if (eq (aref kermit-packet 1) 32)
			   5 ; space means 5 seconds
			 (kermit-fromchar (char-to-string
					   (aref kermit-packet 1))))))))
;; if we're going to use kermit-time-loops...
;      (setq kermit-delay-count (/ (* 5 kermit-delay-count ) 
;				  kermit-read-char-timeout))
      (kermit-do-log 
       (format "negotiated timeout %d" kermit-read-char-timeout))
      ;; we can't negotiate padding - we send what we need.
      (if (> length-packet 4)
	  ;; negotiate EOL character - we use what other side sends
	  (setq kermit-line-terminator
		(char-to-string 
		 (kermit-fromchar (char-to-string (aref kermit-packet 4))))))
      (kermit-do-log
       (format "negotiated end of line character ASCII %d" 
	       (string-to-char kermit-line-terminator)))
      ;; rest of parameters we send what we need in our init packet,
      ;; or we don't understand, so ignore
      t)))

(defun kermit-send-init (string) "Send the init packet"
  (kermit-send-packet string 0
		      (concat 
		       (kermit-tochar kermit-max-packet-len) ; MAXL
		       (kermit-tochar kermit-read-char-timeout) ; timeout
		       (kermit-tochar 0) ; number of padding chars
		       (kermit-tochar 0) ; pad char
		       (kermit-tochar (string-to-char 
				       kermit-line-terminator)) ; EOL char
		       kermit-send-quotechar ; control quote char
		       kermit-quote-8bit-char))) ; binary quote char

(setq kermit-packet-in-progress "")
(setq kermit-seq-number 0)
(setq kermit-packet-len 0)

(defun kermit-incr-seq-number ()
  (setq kermit-seq-number (mod (1+ kermit-seq-number) 64)))

(defun kermit-do-send-data (char) "Add char to be output."
  (if (< (- kermit-max-packet-len 3) ; -3 for length byte, seq, and type
	 (+ kermit-packet-len (kermit-size-of-char char)))
      (progn
	(while (and 
		(kermit-send-packet "D" 
				    kermit-seq-number 
				    kermit-packet-in-progress) 
		    (not (kermit-receive-ack kermit-seq-number))))
	(kermit-incr-seq-number)
	(setq kermit-packet-len 0)
	(setq kermit-packet-in-progress "")))
  (setq kermit-packet-in-progress
	(concat kermit-packet-in-progress (kermit-prefix char)))
  (setq kermit-packet-len (+ kermit-packet-len (kermit-size-of-char char)))
  nil
  )

(defun kermit-finish-file () 
  "Send the final packet + EOF packet + end of transfers packet."
  (if (< 0 (length kermit-packet-in-progress))
      (while (and (kermit-send-packet "D" kermit-seq-number 
				      kermit-packet-in-progress)
		  (not (kermit-receive-ack kermit-seq-number)))))
  (kermit-incr-seq-number)
  (while (and (kermit-send-packet "Z" kermit-seq-number "") 
	      (not (kermit-receive-ack kermit-seq-number))))
  (kermit-incr-seq-number)
  (while (and (kermit-send-packet "B" kermit-seq-number "") 
	      (not (kermit-receive-ack kermit-seq-number)))))
;;
;; log to a buffer for debugging porpoises
;;
(defun kermit-do-real-log (list)
      (save-excursion
	(let ((kermit-log-buffer (get-buffer "kermit-debugging")))
	  (if (not kermit-log-buffer)
	      (setq kermit-log-buffer (generate-new-buffer 
				       "kermit-debugging")))
	  (set-buffer kermit-log-buffer)
	  (insert (concat list "\C-j"))))
  nil)

(defun kermit-receive-ack (sequence-number)
  (kermit-do-log "called kermit-receive-ack")
  (if (not (kermit-read-packet 0)) nil
    (progn 
      (kermit-do-log (concat "Got packet of" kermit-packet-type 
			     kermit-receive-seq kermit-packet))
      (if (and (equal kermit-packet-type "Y") 
	       (equal kermit-receive-seq sequence-number))
	  t 
	(kermit-do-log "ack failed")))))

;;
;; send F packet with filename argument NAME
;;
(defun kermit-send-filename (name) 
  (kermit-do-log "kermit-send-filename called")
  (while (and (kermit-send-packet "F" kermit-seq-number name) 
	      (not (kermit-receive-ack kermit-seq-number))))
  (kermit-incr-seq-number))

(defun kermit-receive-packet-num (number)
  (catch 'kermit-doexit
    (while (kermit-read-packet 0)
      (if (equal kermit-receive-seq number)
	  (progn
	    (kermit-send-ack number)
	    (throw 'kermit-doexit t)
	    )
	(kermit-send-nak number)))))

(defun kermit-receive-filename ()
  (kermit-receive-packet-num kermit-seq-number)
  (if (equal kermit-packet-type "F")
      (progn
	(setq kermit-filename (kermit-splode-data-packet kermit-packet))
	t)
    (progn
      (if (equal kermit-packet-type "S") (kermit-send-init "Y"))
      nil))
  )

;;
;; receive D packets until E, B, or Z packet terminates transfer
;;
(defun kermit-receive-data-packets ()
  (save-excursion
    (set-buffer kermit-transfer-buffer)
    (catch 'kermit-leave
      (while (kermit-receive-packet-num kermit-seq-number)
	(kermit-incr-seq-number)
	(set-buffer kermit-transfer-buffer)
	(goto-char (point-max))
	(if (equal kermit-packet-type "Z") (throw 'kermit-leave t))
	(if (or (equal kermit-packet-type "B") 
		(equal kermit-packet-type "E"))
	    (throw 'kermit-leave nil))
					; Ze bed, boss, ze bed!
	(if (equal kermit-packet-type "D")
	    (insert-string (kermit-splode-data-packet kermit-packet)))))))

(defun kermit-send-buffer (buffer) 
  "Sends the buffer via the Kermit protocol to the other end." 
  (interactive "bSend buffer: ")
  (kermit-blank-screen)
  (kermit-time-loops 500)
  (setq kermit-seq-number 0)
  (setq kermit-transfer-buffer (get-buffer buffer))
  (while (and (kermit-send-init "S") (not (kermit-read-init))))
  (init-kermit-prefix)
  (kermit-decode-init)
  (kermit-incr-seq-number)
  (save-excursion
    (set-buffer kermit-transfer-buffer)
    (kermit-send-filename (buffer-name kermit-transfer-buffer))
    (setq kermit-packet-in-progress "")
    (setq kermit-packet-len 0)
    (kermit-mapstring 'kermit-do-send-data (buffer-string) nil)
    (kermit-finish-file))
  (kermit-unblank-screen))

;;
;; M-x kermit-receive-buffer, escape back to your local system
;; and issue the send command.
;;
(defun kermit-receive-buffer () "Receive a file into a buffer."
  (interactive)
  (setq kermit-seq-number 0)
  (kermit-blank-screen)
  (kermit-time-loops 500)
  (while (and (not (kermit-read-init)) (kermit-send-nak 0)))
  (kermit-send-init "Y")
  (init-kermit-prefix)
  (kermit-decode-init)
  (kermit-incr-seq-number)
  (while (not (kermit-receive-filename)))
  (kermit-incr-seq-number)
  (save-excursion
    (setq kermit-transfer-buffer (generate-new-buffer kermit-filename))
    (set-buffer kermit-transfer-buffer)
    (kermit-receive-data-packets)
    (kermit-receive-packet-num kermit-seq-number)
    (set-visited-file-name kermit-filename))
  (kermit-unblank-screen)
  (switch-to-buffer kermit-transfer-buffer))

(defun kermit-send-current-buffer () (interactive) 
  (kermit-send-buffer (current-buffer)))

;;
;; blank the screen to minimize the effect of (display-time) and other 
;; processes might have on kermit
;;
(defun kermit-blank-screen ()
  (setq kermit-transfer-buffer (current-buffer))
  (setq kermit-blank-buffer (get-buffer-create "*kermit*"))
  (switch-to-buffer kermit-blank-buffer)
  (delete-other-windows)
  (redraw-display)
  (make-local-variable 'echo-keystrokes)
  (setq echo-keystrokes 0)
  (setq mode-line-format '("")))
    
;;
;; remove the blank buffer
;;
(defun kermit-unblank-screen ()
  (switch-to-buffer kermit-blank-buffer)
  (kill-buffer (current-buffer)))

;;
;; Use adaptive timing renooberation technology to determine just how many
;; times we gotta call (input-pending-p) to make up 1 second. 
;;

(defun kermit-time-loops (try)
  (if (= kermit-delay-count 0)
      (let ((currtime) (count))
	(setq count try)
	(setq currtime (kermit-time-to-int (current-time-string)))
	(while (> count 0)
	  (input-pending-p)
	  (accept-process-output) 
	  (setq count (1- count)))
	(setq currtime (car (cdr (kermit-time-diff 
				  (kermit-time-to-int (current-time-string)) 
				  currtime))))
	(if (> currtime 2)
	    (setq kermit-delay-count (/ try currtime))
	  (kermit-time-loops (* 3 try))))))

      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These functions are used to implement time handling.
;;; Much of this code was lifted from the Kiwi 4.30 irc client.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kermit-time-to-int (timestr)
  "Convert from time in string format as returned by current-time-string
to a double integer format, as returned by file-attributes.

Written by Stephen Ma <ma_s@maths.su.oz.au>"
  (let* ((norm+ '(lambda (num1 num2)
		  (let ((sumh (+ (car num1) (car num2)))
			(suml (+ (car (cdr num1)) (car (cdr num2)))))
		    (list (+ sumh (/ suml 65536)) (% suml 65536)))))
	 (norm* '(lambda (num1 num2)
		  (let ((prodh (* num1 (car num2)))
			(prodl (* num1 (car (cdr num2)))))
		    (list (+ prodh (/ prodl 65536)) (% prodl 65536)))))
	 (seconds (string-to-int (substring timestr 17 19)))
	 (minutes (string-to-int (substring timestr 14 16)))
	 (hours (string-to-int (substring timestr 11 13)))
	 (partdays (1- (string-to-int (substring timestr 8 10))))
	 (years (string-to-int (substring timestr 20 24)))
	 (days (+ partdays
		  (cond ((and (= (% years 4) 0)
			      (/= (% years 100) 0))
			 (cdr (assoc (substring timestr 4 7)
				     '(("Jan" . 0)
				       ("Feb" . 31)
				       ("Mar" . 60)
				       ("Apr" . 91)
				       ("May" . 121)
				       ("Jun" . 152)
				       ("Jul" . 182)
				       ("Aug" . 213)
				       ("Sep" . 244)
				       ("Oct" . 274)
				       ("Nov" . 305)
				       ("Dec" . 335)))))
			(t (cdr (assoc (substring timestr 4 7)
				       '(("Jan" . 0)
					 ("Feb" . 31)
					 ("Mar" . 59)
					 ("Apr" . 90)
					 ("May" . 120)
					 ("Jun" . 151)
					 ("Jul" . 181)
					 ("Aug" . 212)
					 ("Sep" . 243)
					 ("Oct" . 273)
					 ("Nov" . 304)
					 ("Dec" . 334))))))
		  (* (- years 1970) 365)
		  (/ (- years 1969) 4)
		  (- (/ (- years 1901) 100)))))
    (funcall norm+
	     (funcall norm*
		      60
		      (funcall norm+
			       (funcall norm*
					60
					(funcall norm+
						 (funcall norm*
							  24
							  (list 0 days))
						 (list 0 hours)))
			       (list 0 minutes)))
	     (list 0 seconds))))
;;
(defun kermit-time-diff (a b)
  "Return the difference between two times. This function requires
the second argument to be earlier in time than the first argument."
  (cond ((= (nth 0 a) (nth 0 b)) (list 0 (- (nth 1 a) (nth 1  b))))
	((> (nth 1 b) (nth 1 a)) (list (- (nth 0 a) (nth 0 b) 1)
				       (- (+ 65536 (nth 1 a)) (nth 1 b))))
	(t (list (- (nth 0 a) (nth 0 b))
		 (- (nth 1 a) (nth 1 b))))))


;;; End of kermit.el


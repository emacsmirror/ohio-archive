; Newsgroups: gnu.emacs.sources
; Path: hal.com!decwrl!pacbell.com!iggy.GW.Vitalink.COM!cs.widener.edu!hela.iti.org!nigel.msen.com!emory!swrinde!cs.utexas.edu!zaphod.mps.ohio-state.edu!pacific.mps.ohio-state.edu!cis.ohio-state.edu!fly.cnuce.cnr.it!pot
; From: pot@fly.cnuce.cnr.it (Francesco Potorti`)
; Subject: hexlify-buffer
; Organization: Source only  Discussion and requests in gnu.emacs.help.
; Date: Thu, 12 Nov 1992 12:10:00 GMT
; 
; This version of hexlify-buffer is twice as fast as the original one in
; hexl.el.
; 
; Anyone out there is able to make it better (it is always too S L  O   W)?
; 

;; LCD Archive Entry:
;; hexlify-buffer|Francesco Potorti`|pot@cnuce.cnr.it|
;; Convert a binary buffer to hexl format.|
;; 92-11-13||~/functions/hexlify-buffer.el.Z|

(defun hexlify-buffer ()
  "Convert the current buffer to hexl format."
  (interactive)
  (goto-char (point-min))
  (let ((len 0) (address 0) (chars "")
	(print-chars
	 (vconcat
	  (mapcar 'char-to-string (make-string 32 ?.))
	  (mapcar 'char-to-string " !\"#$%&'()*+,-./0123456789:;<=>?@")
	  (mapcar 'char-to-string "ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`")
	  (mapcar 'char-to-string "abcdefghijklmnopqrstuvwxyz{|}~")
	  (mapcar 'char-to-string (make-string 129 ?.)))))
    (while (not (eobp))
      (setq len (min 16 (- (point-max) (point))))
      (setq chars (buffer-substring (point) (+ (point) len)))
      (delete-char len)
      (insert (format "%07x0:    " address)
	      (mapconcat '(lambda (arg) (format "%02x" arg)) chars ""))
      (if (eobp) (insert-char ?  (* 2 (- 16 len))))
      (backward-char 28)
      (looking-at
      "\\(....\\)\\(....\\)\\(....\\)\\(....\\)\\(....\\)\\(....\\)\\(....\\)")
      (replace-match " \\1 \\2 \\3  \\4 \\5 \\6 \\7     \"")
      (insert (mapconcat '(lambda (c) (aref print-chars c)) chars "") "\"\n")
      (setq address (1+ address)))))

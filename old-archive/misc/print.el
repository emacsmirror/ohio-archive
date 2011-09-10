From: ron@mlfarm.UUCP (Ronald Florence)
Newsgroups: comp.emacs
Subject: print.el
Message-ID: <267@mlfarm.UUCP>
Date: 28 Jul 89 16:21:35 GMT
Organization: Maple Lawn Farm, Stonington, CT
Lines: 295
Keywords: troff nroff pr

The enclosed code for print.el provides proofing or printing of a
gnu-emacs buffer using a variety of formatters and preformatters: pr,
nroff, troff, eqn, and tbl.  The specifications of formatter options
are in short shell scripts so that local option changes do not require
changes to the lisp code.

Most of the code is borrowed from other emacs lisp functions.  The ^G
in the code for nroff output is a local option which invokes expanded
font for a single line of print.  The "clean-file" function cleans
nroff-junk out of a formatted file, like manual pages or news
documentation.

Hope someone finds this useful.  I welcome comments or suggestions of
improvements to the code.

#! /bin/sh
# This is a shell archive, meaning:
# 1. Remove everything above the #! /bin/sh line.
# 2. Save the resulting text in a file.
# 3. Execute the file with /bin/sh (not csh) to create:
#	print.el
#	colformat
#	emacsprint
#	emacstroff
#	prprint
# This archive created: Fri Jul 28 12:08:57 1989
# By:	Ronald Florence (Maple Lawn Farm, Stonington, CT)
export PATH; PATH=/bin:/usr/bin:$PATH
echo shar: "extracting 'print.el'" '(6065 characters)'
if test -f 'print.el'
then
	echo shar: "will not over-write existing file 'print.el'"
else
sed 's/^X//' << \SHAR_EOF > 'print.el'
X;; Print Emacs buffer on printer or buffer (using nroff, troff or pr).
X;; Copyright (C) 1988 Ronald Florence (ron@mlfarm)
X;; 	added optional flag for tbl, neqn, col to proof-to-buffer (1/24/89)
X;;	added clean-file, col -b for proof-buffer (2/19/89)
X;;	added troff-buffer, tproof-buffer, 3/28/89
X
X(defun lp-buffer (&optional copies)
X  "Print buffer contents with Unix command `pr | lp'.
XOptional prefix argument specifies number of copies."
X  (interactive "P")
X  (prprint-region (point-min) (point-max) copies))
X
X(defun lp-region (start end &optional copies)
X  "Print region contents with Unix command `pr | lp'.
XOptional prefix argument specifies number of copies."
X  (interactive "r\nP")
X  (prprint-region start end copies))
X
X(defun print-buffer (&optional copies)
X  "Print buffer contents after formatting with `nroff -cm -rN2 -rO9'.
XOptional prefix argument specifies number of copies."
X  (interactive "P")
X  (nroff-to-printer-region (point-min) (point-max) copies))
X
X(defun print-region (start end &optional copies)
X  "Print region contents after formatting with `nroff -cm -rN2 -rO9'.
XOptional prefix argument specifies number of copies."
X  (interactive "r\nP")
X  (nroff-to-printer-region start end copies))
X
X(defun proof-buffer (&optional flag)
X  "Proof buffer contents with `nroff -cm -rN2 -rO9' in another buffer.
XOptional prefix argument invokes `tbl', `neqn' and `col' filters."
X  (interactive "P")
X  (nroff-to-buffer-region (point-min) (point-max) flag))
X
X(defun proof-region (start end &optional flag)
X  "Proof region contents with `nroff -cm -rN2 -rO9' in another buffer.
XOptional prefix argument invokes `tbl', `neqn' and `col' filters."
X  (interactive "r\nP")
X  (nroff-to-buffer-region start end flag))
X
X(defun clean-file (name)
X  "Clean nroff junk from file and display in a buffer."
X  (interactive "fClean file: ")
X  (with-output-to-temp-buffer (file-name-nondirectory name)
X    (save-excursion      
X      (message "Cleaning...")
X      (call-process "col" name standard-output t "-b")
X      (message ""))))
X
X(defun troff-buffer (&optional copies)
X  "Typeset region contents after formatting with `troff -mm -rN2'.
XOptional prefix argument specifies number of copies."
X  (interactive "P")
X  (let ((buffer (get-buffer-create "*printer command output*")))
X    (save-excursion
X      (set-buffer buffer)
X      (erase-buffer))
X    (message "Spooling...")
X    (if (null copies) (setq copies "1"))
X    (call-process-region (point-min) (point-max) "emacstroff" 
X			 nil buffer nil  
X			 (concat "-n" copies))
X    (if (> (buffer-size) 0)
X	(progn
X	  (set-buffer buffer)
X	  (goto-char (point-min))
X	  (end-of-line)
X	  (message "%s" (buffer-substring 1 (point))))
X      (message "(Printer command failed)")))
X  (kill-buffer "*printer command output*"))
X
X(defun prprint-region (start end &optional copies)
X  (let ((buffer (get-buffer-create "*printer command output*")))
X    (save-excursion
X      (set-buffer buffer)
X      (erase-buffer))
X    (message "Spooling...")
X    (if (/= tab-width 8)
X	(let ((oldbuf (current-buffer)))
X	  (set-buffer (get-buffer-create " *spool temp*"))
X	  (widen) (erase-buffer)
X	  (insert-buffer-substring oldbuf)
X	  (setq tab-width width)
X	  (untabify (point-min) (point-max))
X	  (setq start (point-min) end (point-max))))
X    (if (null copies) (setq copies "1"))
X    (call-process-region start end "prprint"
X			 nil buffer nil
X			 (concat "-h " (buffer-name)) (concat "-n" copies))
X    (if (> (buffer-size) 0)
X	(progn
X	  (set-buffer buffer)
X	  (goto-char (point-min))
X	  (end-of-line)
X	  (message "%s" (buffer-substring 1 (point))))
X      (message "(Printer command failed)")))
X  (kill-buffer "*printer command output*"))
X
X(defun nroff-to-printer-region (start end &optional copies)
X  (let ((buffer (get-buffer-create "*printer command output*")))
X    (save-excursion
X      (set-buffer buffer)
X      (erase-buffer))
X    (message "Spooling...")
X    (if (null copies) (setq copies "1"))
X    (call-process-region start end "emacsprint" 
X			 nil buffer nil  
X			 (concat "-n" copies))
X    (if (> (buffer-size) 0)
X	(progn
X	  (set-buffer buffer)
X	  (goto-char (point-min))
X	  (end-of-line)
X	  (message "%s" (buffer-substring 1 (point))))
X      (message "(Printer command failed)")))
X  (kill-buffer "*printer command output*"))
X
X(defun tproof-buffer ()
X  "Rough proof buffer contents after formatting with `troff -mm -a -rN2'."
X  (interactive)
X  (with-output-to-temp-buffer "*Proof Output*"
X    (save-excursion
X      (message "Proofing...")
X	(progn
X	  (call-process-region (point-min) (point-max) "troff" 
X			       nil standard-output nil
X			       "-mm" "-a" "-rN2")
X	  (set-buffer standard-output)
X      (set-buffer-modified-p nil)
X      (message "")))))
X
X(defun nroff-to-buffer-region (start end &optional flag)
X  (with-output-to-temp-buffer "*Proof Output*"
X    (save-excursion
X      (setq start (point-min) end (point-max))
X      (message "Proofing...")
X      (if flag
X	  (call-process-region start end "colformat" 
X			       nil standard-output nil)
X	(progn
X	  (call-process-region start end "nroff" 
X			       nil standard-output nil
X			       "-cm" "-rN2" "-rO9")
X	  (set-buffer standard-output)
X	  (message "Cleaning proof output...") 
X	  (zap-nroff-crap)))
X      (set-buffer-modified-p nil)
X      (message ""))))
X
X(defun zap-nroff-crap ()
X  (interactive "*")
X  (goto-char (point-min))
X  (while (search-forward "\b" nil t)
X    (let* ((preceding (char-after (- (point) 2)))
X	   (following (following-char)))
X      	    ;; x\bx
X      (cond ((= preceding following)	
X	     (delete-char -2))
X	    ;; _\b
X	    ((= preceding ?\_)		
X	     (delete-char -2))
X	    ;; \b_
X	    ((= following ?\_)		
X	     (delete-region (1- (point)) (1+ (point)))))))
X  ;; expand ^G lines
X  (goto-char (point-min))
X  (while (search-forward "\C-g" nil t)	
X    (delete-char -2)
X    (while (not (eolp))
X      (insert " ")
X      (forward-char 1)))
X  ;; zap Esc-8 & Esc-9 vertical motions
X  (goto-char (point-min))
X  (while (search-forward "\e" nil t)
X    (if (or (= (following-char) ?8) (= (following-char) ?9))
X	    (delete-region (1+ (point)) (1- (point))))))
SHAR_EOF
if test 6065 -ne "`wc -c < 'print.el'`"
then
	echo shar: "error transmitting 'print.el'" '(should have been 6065 characters)'
fi
fi
echo shar: "extracting 'colformat'" '(73 characters)'
if test -f 'colformat'
then
	echo shar: "will not over-write existing file 'colformat'"
else
sed 's/^X//' << \SHAR_EOF > 'colformat'
X:
X# colformat for Emacs
X
Xtbl -TX | neqn | nroff -cm -rO9 -rN2 | col -b
X
X
SHAR_EOF
if test 73 -ne "`wc -c < 'colformat'`"
then
	echo shar: "error transmitting 'colformat'" '(should have been 73 characters)'
fi
chmod +x 'colformat'
fi
echo shar: "extracting 'emacsprint'" '(49 characters)'
if test -f 'emacsprint'
then
	echo shar: "will not over-write existing file 'emacsprint'"
else
sed 's/^X//' << \SHAR_EOF > 'emacsprint'
X:
X# emacsprint
X
Xnroff -cm -rN2 -rO9 -Thp | lp $1
SHAR_EOF
if test 49 -ne "`wc -c < 'emacsprint'`"
then
	echo shar: "error transmitting 'emacsprint'" '(should have been 49 characters)'
fi
chmod +x 'emacsprint'
fi
echo shar: "extracting 'emacstroff'" '(46 characters)'
if test -f 'emacstroff'
then
	echo shar: "will not over-write existing file 'emacstroff'"
else
sed 's/^X//' << \SHAR_EOF > 'emacstroff'
X:
X# emacstroff
X
Xtroff -t -mm -rN2 | lp -ot $1
SHAR_EOF
if test 46 -ne "`wc -c < 'emacstroff'`"
then
	echo shar: "error transmitting 'emacstroff'" '(should have been 46 characters)'
fi
chmod +x 'emacstroff'
fi
echo shar: "extracting 'prprint'" '(28 characters)'
if test -f 'prprint'
then
	echo shar: "will not over-write existing file 'prprint'"
else
sed 's/^X//' << \SHAR_EOF > 'prprint'
X:
X# prprint
X
Xpr $1 | lp $2
X
SHAR_EOF
if test 28 -ne "`wc -c < 'prprint'`"
then
	echo shar: "error transmitting 'prprint'" '(should have been 28 characters)'
fi
chmod +x 'prprint'
fi
exit 0
#	End of shell archive
-- 

Ronald Florence
...{hsi!aati,rayssd}!mlfarm!ron

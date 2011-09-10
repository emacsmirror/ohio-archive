;To: unix-emacs@bbn.com
;Date: 15 Feb 89 07:52:26 GMT
;From: Olin Shivers <centro.soar.cs.cmu.edu!shivers@pt.cs.cmu.edu>
;Subject: printer package
;
;
;-----
;
;Someone was asking about printer packages. I enclose the package I use
;to print buffers and regions. It has a couple of nice properties:
;- It supports print programs besides lpr (like enscript, cz, empress, et al.).
;- It's very configurable. You can change the printer, change printer
;  programs, change options to the programs, all on the fly. There are
;  reasonable defaults.
;- When you print out a buffer, if the buffer is a file buffer, and the
;  buffer is identical to the file, it just prints the file directly.
;  This makes page headers come out right: instead of "stdin" or 
;  "emacs buffer foo.c," they come out "/usr/jqr/foo.c".
;
;
;The basics of the package is:
;(print-using-lpr)   	; For your .emacs
;(print-using-enscript)  ; Sets up the options.
;M-x print-buffer    	; prints the buffer (prints the file, if possible).
;M-x print-region    	; prints the region
;M-x print-file	    	; print a file
;
;Package follows. 
;    -Olin
;------
;;; Modified PRINT-BUFFER
;;; If the buffer belongs to a file, and the file is up to date,
;;; just print the file itself.
;;; Support for different printing programs, other than lpr.

;;; These hacks brought to you by Olin Shivers (Olin.Shivers@CS.CMU.EDU)
;;; 11/11/87

(defvar printer nil
  "*If non-nil, specifies the printer to send files to.")
(defvar printer-program nil
  "*The program to use to send text to the printer. If nil, lpr is used.
   Set by calling PRINT-USING-LPR, PRINT-USING-ENSCRIPT.
   If you change this another way, be sure to change also:  PRINT-TITLE-HOOK,
   PRINT-DEST-HOOK, and PRINT-JOB-HOOK")
(defvar printer-switches '("-p")
  "*List of extra switches to pass to printer program.")

;;; Basic user interface:
;;; PRINT-BUFFER PRINT-REGION
;;;============================================================================
;;;

(defun print-buffer ()
  "Send buffer contents to the printer. See variables PRINTER, PRINTER-PROGRAM,
   and PRINTER-SWITCHES for customisation."
  (interactive)
  (if (or (buffer-modified-p) (not buffer-file-name))
      (print-region-1 (point-min) (point-max))
      (print-file buffer-file-name)))

(defun print-region (start end)
  "Send region to the printer. See variables PRINTER, PRINTER-PROGRAM,
   and PRINTER-SWITCHES for customisation."
  (interactive "r")
  (print-region-1 start end))

;;; Simple printer-choosers:
;;; PRINT-USING-LPR PRINT-USING-ENSCRIPT
;;;============================================================================
;;;
;;; Feel free to customise the functions yourself. In particular, you
;;; might like to make them set the value of PRINTER-SWITCHES. Also,
;;; note that when you are printing a non-file (buffer or region), and
;;; PRINT-TITLE-HOOK is used to set the header of the printout to
;;; be "foo.c Emacs buffer", there is a small problem. Setting the header
;;; to get the buffer name can cause page numbers to go away. Cz, and enscript
;;; without the -G option have this problem. There's no way to set the
;;; filename part of the header without setting the entire header, thus
;;; blasting the page numbers. Lpr -p doesn't have this problem, because
;;; the -T switch just sets the filename part of the header. But if you
;;; are using cz or enscript without -G, you have to choose for headers:
;;; 1. no buffername:   "11/9/87           stdin            page 3"
;;; 2. only buffername: "           foo.c Emacs buffer            "
;;; You get option 2 by specifying a real PRINT-TITLE-HOOK, or by default.
;;; You get option 1 by setting the PRINT-TITLE-HOOK to be a no-op,
;;; i.e. '(LAMBDA (TITL) '()).
;;; There's nothing that can be done to fix this problem, since it's
;;; a problem with the printing programs, not with the emacs end of things.
;;;
;;; For all printing programs, however, you avoid all this mess when you use
;;; the PRINT-BUFFER command to print out saved buffers. In this case,
;;; PRINT-BUFFER cleverly just prints the file itself, and the retarded
;;; print program has a chance to get the headers right.


(defun print-using-enscript ()
  "Causes enscript to be the print program. Sets PRINTER-SWITCHES to (\"-G\")"
  (interactive)
  (setq print-dest-hook nil)
  (setq print-title-hook '(lambda (titl) (list (concat "-b" titl))))
  (setq print-job-hook nil)
  (setq printer-program "enscript")
  (setq printer-switches '("-G")))

(defun print-using-lpr ()
  "Causes lpr to be the print program. Sets PRINTER-SWITCHES to (\"-p\")"
  (interactive)
  (setq print-dest-hook nil)
  (setq print-title-hook nil)
  (setq print-job-hook nil)
  (setq printer-program nil)
  (setq printer-switches '("-p")))

;Dovers are no more...
;(defun print-using-cz ()
;  "Causes cz to be the print program. Clears printer-switches."
;  (interactive)
;  (setq print-dest-hook '(lambda (dest) (list "-d" dest)))
;  (setq print-title-hook '(lambda (titl) (list "-h" titl)))
;  (setq print-job-hook '(lambda (jname) (list "-b" jname)))
;  (setq printer-program "cz")
;  (setq printer-switches nil))


;;; Printer interface functions and hooks:
;;; PRINT-DEST-HOOK PRINT-TITLE-HOOK PRINT-JOB-HOOK
;;;============================================================================
;;;

(defvar print-dest-hook nil
  "*If non-nil, a function mapping a printer name to the appropriate
    list of arguments for the print program. E.g.,
    (lambda (pr) (list \"-P\" pr))")

(defun print-dest (printer)
  "Takes the name of a printer, and turns it into a list of arguments
   for the print program. Defaults to (\"-P\" printer)."
  (if print-dest-hook
      (funcall print-dest-hook printer)
      (list "-P" printer)))

(defvar print-title-hook nil
  "*If non-nil, a function mapping a print title to the appropriate
    list of arguments for the print program. E.g.,
    (lambda (titl) (list \"-T\" titl))")

(defun print-title (titl)
  "Takes the print title for a print job, and turns into a list of arguments
   for the print program. Defaults to (\"-T\" titl)."
  (if print-title-hook
      (funcall print-title-hook titl)
      (list "-T" titl)))

(defvar print-job-hook nil
  "*If non-nil, a function mapping a job name to the appropriate
    list of arguments for the print program. E.g.,
    (lambda (jname) (list \"-J\" jname))")

(defun print-job (jname)
  "Takes the job name of a print job, and turns it into a list of arguments
   for the print program. Defaults to (\"-J\" jname)."
  (if print-job-hook
      (funcall print-job-hook jname)
      (list "-J" jname)))

;;; Programs that really do it:
;;; PRINT-FILE PRINT-REGION-1
;;;============================================================================
;;;

(defun print-file (filename)
  (interactive "ffile: ")
  ; Check to see if the file should be saved before printed.
  (let ((buff (get-file-buffer filename)))
    (if (and buff
	     (buffer-modified-p buff)
	     (y-or-n-p (format "Save buffer %s first? "
			       (buffer-name buff))))
	;; save BUFF.
	(let ((old-buffer (current-buffer)))
	  (set-buffer buff)
	  (save-buffer)
	  (set-buffer old-buffer))))
  (message "Spooling file...")
  (apply 'call-process (or printer-program "lpr")
          nil 0 nil
          (append (and printer (print-dest printer))
                  printer-switches
                 (list filename)))
  (message "Spooling file...done"))

(defun print-region-1 (start end)
  (save-excursion
   (message "Spooling...")
   (if (/= tab-width 8)
       (let ((oldbuf (current-buffer)))
        (set-buffer (get-buffer-create " *spool temp*"))
        (widen) (erase-buffer)
        (insert-buffer-substring oldbuf)
        (call-process-region start end "expand"
                             t t nil
                             (format "-%d" tab-width))))
   (let ((title (concat (buffer-name) " Emacs buffer")))
     (apply 'call-process-region start end
            (or printer-program "lpr")
            nil nil nil
            (append (print-title title)
                    (and printer (print-dest printer))
                    (print-job title)
                    printer-switches)))
   (message "Spooling...done")))
;-- 


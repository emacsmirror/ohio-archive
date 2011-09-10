;To: unix-emacs@BBN.COM
;Date: 13 Jun 89 14:59:14 GMT
;From: Stephen Martin <jtsv16!isdserv!iemisi!smartin@uunet.uu.net>
;Sender: arpa-unix-emacs-request@BBN.COM
;Subject: editing msdos files on a Unix system
;Organization: Boeing Canada, Toronto, Canada
;Source-Info:  From (or Sender) name not authenticated.
;
;
;At my present job, we have a bunch of PC networked to a Unix file server.  Any
;developement work for the PCs is usually initialial done on the unix machine
;to leave the PCs available for other use.  When a project is to the point
;where it is necessary to test and run on the PC, the source files are usually
;run through a filter to change line feeds to a carriage return - line feed
;combination and after this point most work is done on the PC because it is
;too much of a pain to keep converting files back and forth between the Unix
;and dos file formats.  I decided to write some elisp code to try to detect if
;a file is a dos file and strip the carriage returns out when the file is
;read in and then replace them when it is being written out.  The package is not
;that intelligent in that if a single carriage return is found the file is
;assumed to be a dos file.  An interactive variable called msdos-file indicates
;if the current file is a dos file, this can be set by set-variable to initially
;create a dos file.  I have the routines read-msdos and write-msdos included in
;the find-file-hooks and write-file-hooks in my .emacs file as shown here:
;
;(setq find-file-hooks (cons 'read-msdos find-file-hooks))
;(setq write-file-hooks (cons 'write-msdos write-file-hooks))
;
;Here is the code for msdos.el.  Comments and suggestions are welcome.
;Standard GNU type copyrights apply.
;
;------------------------------ Cut here -----------------------------
;;
;; read-msdos/write-msdos
;;
;; Try to find and handle PC files - Here are two funtions that are bound to
;; the read and write file hooks.  The first read-msdos checks to see if a file
;; contains a <cr> character. If so it is assumed that all lines are terminated
;; by a <cr><lf> sequence.  The <cr>'s are removed from the buffer and a 
;; buffer-local flag is set to indicate that this was done.  When the file is
;; written out, the flag is checked and if it is set, all line feeds are 
;; replaced with <cr><lf> sequences.
;;
(defvar msdos-file nil "*Variable to indicate if a file was an MS-DOS file or not")
(defun read-msdos ()
  "Function to strip <cr><lf> sequence out of a file when read"
  (interactive)
  (make-local-variable 'msdos-file)
  (save-excursion (goto-char (point-min))
		  (if (search-forward "\r" nil t)
		      (progn (setq msdos-file t)
			     (goto-char (point-min))
			     (replace-string "\r" "")
			     (setq msdos-file t)
			     (not-modified)))))

(defun write-msdos ()
  "Function to change <lf> to <cr><lf> for MS-DOS files"
  (if msdos-file
      (save-excursion (goto-char (point-min))
		      (replace-string "\n" "\r\n")
		      (write-region (point-min) (point-max) (buffer-file-name))
		      (goto-char (point-min))
		      (replace-string "\r" "")
		      (not-modified)
		      (clear-visited-file-modtime)
		      t)))

;------------------------ Cut here ---------------------------------- 
;   ___  ___  ___ ___  _  _ ___
;  /__/ /  / /__   /  /\ / /  _   Stephen Martin, Boeing Canada, Toronto.
; /__/ /__/ /__  _/_ /  / /__/             
;                                           Nuke the Raisins
;UUCP: smartin@iemisi.UUCP
;      {uunet|suncan}!jtsv16!marsal1!iemisi!smartin


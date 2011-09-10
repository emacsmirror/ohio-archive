;;;
;;; LCD Archive Entry:
;;; massive-backup|Terry Brannon|brannon@jove.cs.caltech.edu|
;;; Automatically tar and copy directories to remote machines|
;;; 92-07-04|1.00|~/misc/massive-backup.el.Z|
;;; 
;;; Description
;;; massive-backup.el allows one to specify directories to be tarred,
;;; compressed and copied to a another location. The remote 
;;; location may be specified as an ange-ftp pathname. The local
;;; location is specified as a Un*x relative or absolute pathname.
;;; 
;;; Requirements
;;; Ange-ftp, obtainable from "/anonymous@alpha.gnu.ai.mit.edu:/ange-ftp"
;;;
;;; Use and Installation
;;;
;;; - Set the car of the variable massive-backup-list to the the local
;;;   directories to be tarred and compressed. Set the cdr to a list of
;;;   remote destination directories. The cdr must be a list even if you
;;;   only have one remote destination.
;;;
;;; - In your .emacs file somewhere after declaring your load-path
;;;   variable, insert the following line:
;;;   (autoload 'massive-backup "massive-backup" "" t)
;;;
;;; - Place this file somewhere on your load-path
;;;
;;; - Invoke by type M-x massive-backup
;;;

;;; Configurable Variable Defines

(defvar default-destination-list
  '(
    "/tb06@pl122b:/scratch/china.primitive/Thesis/"
    "/brannon@jove.cs.caltech.edu:Thesis/"
    "/tb06@ns1.cc.lehigh.edu:Thesis/")
"This is just for my bookkeeping . I had to manually insert this list
below because I could not get the list to evaluate itself")

(defvar massive-backup-list 
  '(
    ("~/Thesis/src"        
     .     ( "/tb06@pl122b:/scratch/china.primitive/Thesis/"
	     "/brannon@jove.cs.caltech.edu:Thesis/"
	     "/tb06@ns1.cc.lehigh.edu:Thesis/" ))

;    ("~/repository"        
;     .     ( "/tb06@pl122b:/scratch/china.primitive/Thesis/"
;	     "/brannon@jove.cs.caltech.edu:Thesis/"
;	     "/tb06@ns1.cc.lehigh.edu:Thesis/" ))

    ("~/Thesis/examples"        
     .     ( "/tb06@pl122b:/scratch/china.primitive/Thesis/"
	     "/brannon@jove.cs.caltech.edu:Thesis/"
	     "/tb06@ns1.cc.lehigh.edu:Thesis/" ))

;    ("~/Thesis/latex"        
;     .     ( "/tb06@pl122b:/scratch/china.primitive/Thesis/"
;	     "/brannon@jove.cs.caltech.edu:Thesis/"
;	     "/tb06@ns1.cc.lehigh.edu:Thesis/" ))
    )
"Absolute pathnames to the directories to be backed up. In this
default case, each tar file is sent to the same place. Feel free to
configure your own destination lists... they must be lists even if you
only have one destination")

;;;
;;; Source code . No need for alteration
;;;

(require 'ange-ftp)
(setq debug-on-error t)

(defun massive-backup()
  (interactive)
  (mapcar '(lambda (x) 
	     (backup (car x))) massive-backup-list)
  (message "Massive backup completed. Breathe a sigh of relief."))

(defun backup (filename)
  (let ((DIR (file-name-directory (expand-file-name filename)))
	(FILE (file-name-nondirectory (expand-file-name filename))))

    (my-shell-command (concat "cd " DIR " ;  \\rm "  DIR FILE ".tar*"))
    (my-shell-command (concat "cd " DIR " ; tar cvf " FILE ".tar " FILE))
    (my-shell-command (concat "cd " DIR " ; compress " FILE ".tar"))

    (let ((dest (cdr (assoc filename massive-backup-list))))
      (mapcar '(lambda (x)
		 (copy-file (concat DIR FILE ".tar.Z")
			    (concat x FILE ".tar.Z") t))
	      dest))

    (my-shell-command (concat "\\rm " DIR FILE ".tar.Z"))))

(defun my-shell-command (exec-string)
  (message (concat "[Action]: " exec-string)) 
  (shell-command exec-string))


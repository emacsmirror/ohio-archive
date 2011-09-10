; Path: dg-rtp!rock!mcnc!stanford.edu!agate!spool.mu.edu!uwm.edu!linac!pacific.mps.ohio-state.edu!cis.ohio-state.edu!kernel.co.UK!mjh
; From: mjh@kernel.co.UK (Mark J Hewitt)
; Newsgroups: gnu.emacs.sources
; Subject: Re: Wanted: Uncompress on load / Compress on save
; Date: 23 Jul 91 16:22:34 GMT
; References: <9107230124.AAmaud27574@maud.ifi.uio.no>
; Organization: Source only  Discussion and requests in gnu.emacs.help.
 
;;; -*- emacs-lisp -*-
;
;  compress.el	Edit compressed files transparently
;
;  Mark J. Hewitt  Kernel Technology Ltd.  1-Feb-89
;
;;;

;; LCD Archive Entry:
;; compress2|Mark J Hewitt|mjh@kernel.co.UK
;; |Edit compressed files transparently
;; |89-02-01||~/misc/compress2.el.Z|

(defvar uncompress-command "uncompress"
  "The command used to uncompress a buffer.")

(defvar compress-command "compress"
  "The command used to compress a buffer.")

;;; Always uncompress .Z files
(setq auto-mode-alist
      (cons '("\\.Z$" . uncompress-file-while-visiting) auto-mode-alist))

;;; How we uncompress the file
(defun uncompress-file-while-visiting nil
  "Uncompress a file into a buffer, and then set appropriate \"auto-mode\""
  (if (and (not (null buffer-file-name))
	   (string-match "\\.Z$" buffer-file-name))
      (set-visited-file-name
       (substring buffer-file-name 0 (match-beginning 0))))
  (let (read-only-status buffer-read-only)
    (setq buffer-read-only nil)
    (message "Uncompressing...")
    (shell-command-on-region (point-min) (point-max) uncompress-command t)
    (message "Uncompressing...Done")
    (setq buffer-read-only read-only-status))
  (set-buffer-modified-p nil)
  (normal-mode)
  (if (not (assq 'compressed-mode minor-mode-alist))
      (setq minor-mode-alist (cons '(compressed-mode " Compressed")
				   minor-mode-alist)))
  (setq compressed-mode t)
  (or (memq (function compress-file-while-saving) write-file-hooks)
      (setq write-file-hooks
	    (cons (function compress-file-while-saving)
		  write-file-hooks))))


;;; Compress a file when saving
(defun compress-file-while-saving nil
  "Compress a buffer prior to saving it"
  (if (and (buffer-modified-p) compressed-mode
	   (y-or-n-p (concat "Re-compress " buffer-file-name "? ")))
      (progn
	(message "Compressing...")
	(let* ((current-filename (concat buffer-file-name ".Z"))
	       (buffer (generate-new-buffer current-filename)))

	  (save-excursion
	    (set-buffer buffer)
	    (erase-buffer))

	  (call-process-region (point-min) (point-max) shell-file-name
			       nil buffer nil "-c" compress-command)
	  (message "Compressing...Done")

	  (save-excursion
	    (set-buffer buffer)
	    (make-local-variable 'require-final-newline)
	    (setq require-final-newline nil)
	    (write-file current-filename))

	  (kill-buffer buffer))
	(set-buffer-modified-p nil)
	t)
    nil))


;;; Look for the compressed version of a file
(defun find-compressed-file nil
  "Read and uncompress a file"
  (if (file-exists-p (concat buffer-file-name ".Z"))
      (progn
	(setq buffer-file-name (concat buffer-file-name ".Z"))
	(insert-file-contents buffer-file-name t)
	(goto-char (point-min))
	(setq error nil)
	t)))

;;; How we remember what mode this buffer is in
(make-variable-buffer-local 'compressed-mode)

;;; If we can't find the file as given - look for a compressed version
(setq find-file-not-found-hooks
      (cons 'find-compressed-file
	    find-file-not-found-hooks))

-------------------------------------------------------------------------------
Mark J. Hewitt

bangpath: ...!ukc!kernel!mjh		JANET:	mjh@uk.co.kernel
voice:	  (+44) 532 484844		other:	mjh@kernel.co.uk
fax:	  (+44) 532 404164	    old style:  mjh%uk.co.kernel@uk.ac.ukc
paper:	Kernel Technology Ltd, Kernel House, Killingbeck Drive,
	Leeds LS14 6UF, West Yorkshire, UK

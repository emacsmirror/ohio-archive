; Path: hal.com!olivea!uunet!pipex!demon!edscom!kevin
; From: kevin@edscom.demon.co.uk (Kevin Broadey)
; Newsgroups: gnu.emacs.sources
; Subject: Automatic templates based on file suffix
; Date: 18 Nov 92 16:49:39 GMT
; Organization: EDS-Scicon, Milton Keynes, UK
; 
; I got hold of a copy of template.el by Tom Lord <lord+@andrew.cmu.edu> a
; while ago and quite liked it, especially the idea of a "substitutions
; file" which lets you specify on-the-fly replacements for macros in the
; included template file.
; 
; One way I used it a lot was to include a skeleton file whenever I
; started editing a new file, so I decided to modify it to run from
; `find-file-hooks', using the file extension to select the template file.
; I also tidied things up so that inclusion of a template could be undone
; with a single "undo" command.
; 
; Here it is - hope you like it.
; 
; Kevin
; 
; ------------------------------------------------------------------------
;; auto-template.el
;;
;; LCD Archive Entry:
;; auto-template|Kevin Broadey|kevin@edscom.demon.co.uk|
;; Auto insert templates with macro replacement based on file extension.|
;; 92-11-02|1.0|~/misc/auto-template.el.Z|
;;
;; Written by Kevin Broadey <kbroadey@edscom.demon.co.uk>
;;
;; Version 1.0  02-Nov-92
;;
;; Read the DOC string for usage information.
;;
;; Based on template.el by Tom Lord <lord+@andrew.cmu.edu>.  I added the stuff
;; for running from find-file-hooks and generally hacked it beyond all
;; recognition.
;;
;; Usual GNU copyleft stuff.
;;
;; Mail me with bug reports and suggestions.

;; Mail me anyway if you like the package so I can feel all warm inside.

(defvar auto-template-dir "~/templates/"
  "*Directory containing template files.")

(defun auto-template (&optional template-file)

  "Insert the contents of TEMPLATE-FILE after point.  Mark is set at the
end of the inserted text.  If a \"substitutions file\" exists and is
readable, the substitutions are applied to the inserted text.  The
default directory for TEMPLATE-FILE is `auto-template-dir'.

A substitutions file has the same base name as TEMPLATE-FILE and suffix
\".sub\".  Each line looks like:-

    \"@PLACE-HOLDER@\"      (string-valued-sexp)
or
    \"@PLACE-HOLDER@\"      \"Prompt: \"

In the first case @PLACE_HOLDER@ is replaced by the value of the sexp,
and in the second the string is used as a prompt to read a replacement
string from the minibuffer.

The format of the place-holder is entirely up to you.  Just remember to
choose something that it unlikely to appear for real in the template
file.  Also, because substitutions are performed in the order they
appear, if the replacement for an earlier place-holder contains a later
one, this too will be replaced.  This is a feature, not a bug!

If you add this function to your `find-file-hooks' then when you visit a
new file it will automatically insert template file \"SUF.SUF\" from
`auto-template-dir', where SUF is the suffix for the new file.  It will
also apply substitutions file \"SUF.sub\" to the inserted text if it
exists and is readable."

  (interactive (list
		(let ((completion-ignored-extensions
		       (cons ".sub" completion-ignored-extensions)))
		  (read-file-name "Template file: " auto-template-dir nil t))))

  ;; If not called with a template file name, create one from the current
  ;; buffer's file name if this is a new file.
  (if (or template-file
	  (file-exists-p (buffer-file-name)))
      nil
    (setq template-file
	  (let* ((buffer-file-name (buffer-file-name))
		 (suffix (and buffer-file-name
			      (auto-template-get-suffix buffer-file-name)))
		 (file (and suffix
			    (expand-file-name (concat suffix "." suffix)
					      auto-template-dir))))
	    (and file
		 (file-readable-p file)
		 file))))

  ;; Do our stuff if we've got a template file.
  (if template-file
      (let ((substitution-file (concat
				(auto-template-strip-suffix template-file)
				".sub")))

	(let ((original-buffer (current-buffer))
	      (work-buffer (get-buffer-create " *auto-template*"))
	      substitutions)
	  ;; Note - I use a temporary buffer even when there is no
	  ;; substitutions file so that UNDO makes a new file's buffer go back
	  ;; to `unmodified'.  This didn't happen when I used `insert-file' to
	  ;; insert the file directly into the buffer - undo removed the text
	  ;; but left the buffer flagged as modified.
	  (set-buffer work-buffer)
	  (widen)
	  (erase-buffer)

	  ;; Read substitutions into a list if the file is readable.
	  (if (file-readable-p substitution-file)
	      (progn
		(insert "()")		; list delimiters
		(forward-char -1)
		(insert-file-contents substitution-file)
		(goto-char (point-min))
		(setq substitutions (read work-buffer))))

	  ;; Read in the template file.
	  (erase-buffer)
	  (insert-file-contents template-file)

	  ;; Apply the substitutions.
	  (while substitutions
	    (let ((place-holder (car substitutions))
		  (replacement (car (cdr substitutions))))
	      (setq substitutions (cdr (cdr substitutions)))
	      (setq replacement
		    (if (stringp replacement)
			(read-string replacement)
		      (eval replacement)))
	      (save-excursion
		(while (search-forward place-holder nil t)
		  (replace-match replacement t t)))
	      ))

	  ;; Insert the (possibly modified) template.
	  (set-buffer original-buffer)
	  (insert-buffer work-buffer)
	  (kill-buffer work-buffer)
	  ))))


(defun auto-template-get-suffix (file)
  "Return the file suffix for FILE, or NIL if none."
  (if (string-match "\\.\\([^./]+\\)$" file)
      (substring file (match-beginning 1) (match-end 1))))


(defun auto-template-strip-suffix (file)
  "Return FILE without its file suffix."
  (if (string-match "\\.[^./]+$" file)
      (substring file 0 (match-beginning 0))
    file))

;;; rmime.el --- read MIME messages

;; Author: Ray Moody <moody@cray.com>
;; Version: $Id: rmime.el,v 1.1 1995/10/24 19:08:46 moody Exp $
;; Keywords: MIME, mail

;;; Commentary:

;; This file provides MIME support for several Emacs message reading
;; packages.  This file has been designed with RMAIL in mind, but it
;; has also been tested with mh-e and VM.  It should work with most
;; other major modes as well.
;;
;; To use this file with RMAIL, add the following to your .emacs file:
;;
;;	(add-hook 'rmail-show-message-hook 'rmime-format)
;;	(add-hook 'rmail-edit-mode-hook    'rmime-cancel)
;;	(setq rmail-output-file-alist '(("" rmime-cancel)))
;;	(autoload 'rmime-format "rmime" "" nil)
;;
;; To use this file with mh-e, add the following to your .emacs file:
;;
;;	(add-hook 'mh-show-mode-hook 'rmime-format)
;;	(autoload 'rmime-format "rmime" "" nil)
;;
;; To use this file with VM, add the following to your .emacs file:
;;
;;	(setq vm-preview-lines t)
;;	(add-hook 'vm-select-message-hook 'rmime-format)
;;
;; To use this file manually, add the following to your .emacs file:
;;
;;	(autoload 'rmime-mode   "rmime" "" t)
;;
;; Additional note by Kai.Grossjohann@cs.uni-dortmund.de:
;;
;; To use this file with Gnus, use the following:
;;
;;      (setq gnus-show-mime-method 'rmime-format)
;;      (setq gnus-show-mime t)
;;      (autoload 'rmime-format "rmime" "" nil)

;;; Code:

(defvar rmime-clean-files nil
  "*If non-nil, rmime undoes MIME formatting before a buffer save")

(defvar rmime-arrow #("-->" 0 3 (face rmime-arrow-face))
  "*Arrow indicator used during MIME play")

(defvar rmime-leader (make-string (length rmime-arrow) ? )
  "*Leader inserted before nontext MIME entities.")

;; Path to metamail.  Metamail is called when there is no Emacs lisp
;; function to handle a MIME type.
;;
;; Metamail can be found in ftp://thumper.bellcore.com/pub/nsb.
(defvar rmime-metamail-program "metamail"
  "Program to decode MIME messages with content-types unknown to Emacs")

;; Path to mimencode.  Mimencode will decode MIME messages with base64
;; or quoted-printable encodings.  Mimencode comes with metamail.
(defvar rmime-mimencode-program "mimencode"
  "Program to decode base64 or quoted-printable MIME messages")

;; Path to the uufilter program, or nil if uufilter is not available.
;; The uufilter program is an enhanced version of uudecode that will
;; write to stdout if it is given -s.  If you don't have uufilter, set
;; this variable to nil and don't worry about it.  This style of
;; encoding is deprecated.
(defvar rmime-uufilter-program nil
  "Program to decode uuencoded MIME messages.")

(defvar rmime-ftp-program "ftp"
  "Program to transfer external-body messages with access-type=ftp")

;; These variables determine the appearance of multipart/digest messages.
(defvar rmime-digest-indentation "")
(defvar rmime-digest-recursive-indentation "   ")
(defvar rmime-digest-toc-intro "\t\t\t  Table Of Contents\n")
(defvar rmime-digest-item-intro #("\f\t\t\t  #################\n" 0 24 (face bold)))

;; List of transfer-encodings that don't need to be processed by
;; mimencode.  RMIME is a mail user agent so it can treat all of these
;; encodings exactly the same way.  The distinctions between these
;; encodings are only important to the mail transfer agent.
(defvar rmime-transparent-encodings '("" "7bit" "8bit" "binary"))

;; List of charsets that can be properly displayed with the default
;; face.  If a charset is encountered that isn't listed here, we
;; assume that something external to this package created a face with
;; the same name as the charset.
;;
;; We also assume that the display table is always right.  This
;; assumption isn't always accurate and will need to be changed
;; someday.
(defvar rmime-transparent-charsets  '("" "us-ascii" "iso-8859-1"))

;; This magic string is inserted between the raw and formatted parts
;; of a message.  It is important when it appears in a auto save file
;; because it is the only way of knowing what part of a message needs
;; to be deleted and redone.
(defvar rmime-magic-string "\037 -=- MIME -=- \037\014\n")

;; This is the default regexp to separate message headers from the
;; message body.  We are being slightly fancy by permitting whitespace
;; characters.  This means that the header parsing code needs to know
;; that lines beginning with whitespace but containing no nonwhite
;; characters isn't a continuation line.  This pattern must end with a
;; newline instead of `$' because we expect point (and match-end) to
;; be the first character of the message body.
;;
;; Any major mode that needs to set a local value should also set
;; rmime-mimeinfo-method to rmime-standard-mimeinfo (or at least to a
;; routine that calls rmime-standard-mimeinfo as a subroutine).
(defvar rmime-separator-regexp "^[ \t]*\n")

;; Function to call to extract MIME information for the current
;; message in the current buffer.
;;
;; Buffers that don't hold an RFC 822 message (or that may mess around
;; with message headers) must assign a local value to this variable.
(defvar rmime-mimeinfo-method 'rmime-obsolete-mimeinfo)

;; rmime-tmp-buffer-name names the temporary buffer used to hold raw
;; MIME.  rmime-tmp-buffer-string is the raw MIME presently in the
;; temporary buffer.  We check to see if we have what we want in the
;; temporary buffer by running "eq" on rmime-tmp-string --- this
;; avoids character-by-character comparision.
(defvar rmime-tmp-buffer-name " *rmime-tmp*")
(defvar rmime-tmp-string nil)

;; rmime-contents-hook may have to cancel MIME formatting during a
;; buffer-save operation.  This entails changing text outside of the
;; active restriction --- a big mistake.  We deal with the situation
;; by restoring the restriction in rmime-after-save.  We need to save
;; the current restriction every time a message is displayed --- when
;; rmime-contents-hook is called, the buffer has already been widened.
;;
;; We use two movable markers to keep track of the correct
;; restriction.
(defvar rmime-restriction-start nil)
(defvar rmime-restriction-end nil)
(make-variable-buffer-local 'rmime-restriction-start)
(make-variable-buffer-local 'rmime-restriction-end)

;; Highlight face for formatting message/rfc822 headers.
(defvar rmime-highlight-face
  (or (identity rmail-highlight-face)
      (if (face-differs-from-default-p 'bold) 'bold 'highlight)))

;; Default type for bodies that have no Content-Type header.
;;
;; Inside of rmime-digest-action, this variable has a temporary
;; binding of 'rmime-message/rfc822.
(defvar rmime-default-type 'rmime-text/plain)

;; A buffer-local list of overlays, one per formatted MIME message.
(defvar rmime-overlay-list nil)
(make-variable-buffer-local 'rmime-overlay-list)

;; This variable is only used when a metamail process is alive.  It is
;; a list of the body parts that still need to be passed off to
;; metamail.  Before (play) starts metamail, the play list is a
;; property in the per-message overlay.
(defvar rmime-play-list nil)
(make-variable-buffer-local 'rmime-play-list)

;; The process displaying a body while a "play" command is doing its
;; thing.  Setting this variable changes the appearance of the mode
;; line to include a "Playing" indicator.  We are careful to mess
;; around with this indicator before the "RMIME" indicator so that the
;; "Playing" indicator appears second in the mode line.
(defvar rmime-process nil)
(make-variable-buffer-local 'rmime-process)
(or (assq 'rmime-process minor-mode-alist)
    (setq minor-mode-alist (cons '(rmime-process " Playing") minor-mode-alist)))

;; A boolean variable indicating if formatting is active or not in the
;; current message of the current buffer.  Setting this variable
;; changes the appearance of the mode line to include a "RMIME"
;; indicator.  We are careful to mess around with this indicator after
;; the "Playing" indicator so that the "Playing" indicator appears
;; first in the mode line.
(defvar rmime-mode nil)
(make-variable-buffer-local 'rmime-mode)
(or (assq 'rmime-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(rmime-mode " RMIME") minor-mode-alist)))

;; These variable control the reassembly of partial messages.
;; rmime-partial-partlist is an ordered association list suitable for
;; rmime-partial-1.  rmime-partial-parts is the total number of parts
;; expected (an integer) or nil if we haven't encountered a message
;; with a "TOTAL" parameter on it yet.
(defvar rmime-partial-partlist nil)
(defvar rmime-partial-parts nil)
(make-variable-buffer-local 'rmime-partial-partlist)
(make-variable-buffer-local 'rmime-partial-parts)

;; This is here to quiet the byte compiler.  This variable is only set
;; inside the let* statement at the beginning of (rmime-format).
;; (mh-folder-mode-mimeinfo) knows about this variable and can set it.
(defvar rmime-source-buffer)

;; We use this handy macro whenever we are going to modify a buffer.
;; This macro evaluates its body after clearing read-only status and
;; disabling file locking.  Read-only status, modification status, and
;; file locking are restored to their original values after the body
;; has finished or in the event of an error.
;;
;; We would like to temporarily disable undo too (saves space and
;; time), but this isn't possible --- the data structure is such that
;; if we don't keep our own undo records around, all previous undo
;; records become worthless.
;;
;; The form produced by this macro looks like this:
;;
;; (let ((symbol28473a (buffer-modified-p))
;;       (inhibit-read-only t)
;;       (buffer-file-name))
;;   (unwind-protect
;;       (progn body)
;;     (set-buffer-modified-p symbol28473a)))
(defmacro rmime-modify-buffer (&rest body)
  (let ((symbol (make-symbol "rmime-buffer-modified-flag")))
    (list 'let (cons (cons symbol '((buffer-modified-p))) '((inhibit-read-only t) (buffer-file-name)))
	  (list 'unwind-protect (cons 'progn body)
				(list 'set-buffer-modified-p symbol)))))
(put 'rmime-modify-buffer 'lisp-indent-function 0)

;; This macro evaluates its body with the restriction set to raw MIME
;; message contents.
;;
;; The form produced by this macro looks like this:
;;
;; (save-excrusion
;;  (apply 'rmime-tmp-buffer (car arguments))
;;  (body)
(defmacro rmime-content-excursion (arguments &rest body)
  (or (and (listp arguments) (eq (length arguments) 1))
      (error "Bad arguments to rmime-content-excursion"))
  (cons 'save-excursion (cons (list 'apply '(quote rmime-tmp-buffer) (car arguments)) body)))
(put 'rmime-content-excursion 'lisp-indent-function 1)

;; Insure that rmime-markup-face is defined.
(or (member 'rmime-markup-face (face-list))
    (progn (make-face 'rmime-markup-face)
	   (set-face-foreground 'rmime-markup-face "blue")))

;; Insure that rmime-arrow-face is defined.
(or (member 'rmime-arrow-face (face-list))
    (progn (make-face 'rmime-arrow-face)
	   (set-face-foreground 'rmime-arrow-face "red")))

(defun rmime-mode (&optional arg)
  "Toggle rmime mode.
With arg, turn rmime mode on if arg is possitive, off otherwise.

Rmime-mode is used to read MIME multimedia messages.  Multimedia
messages may contain images, audio clips, video clips and other
nontextual data as well as encoded and specially formatted text.  When
rmime-mode is active, messages will be searched for text which can be
displayed in an Emacs buffer.  Message bodies that cannot be displayed
in an Emacs buffer (such as sounds and images) will be represented by
a special indicator line.  Pressing C-c C-c on the indicator line will
start an external program to display the message.

Also see \\[play]"
  (interactive "P")
  (if (if (identity arg) (> (prefix-numeric-value arg) 0) (not rmime-mode))
      (rmime-format)
    (rmime-cancel)))

(defun rmime-format ()

  ;; The mimeinfo method may switch buffers or move the point.
  (save-excursion

    ;; This needs to be let* and not let because rmime-mimeinfo-method
    ;; may setq rmime-source-buffer.
    (let* ((rmime-source-buffer (current-buffer))
	   (headers 	 	(funcall rmime-mimeinfo-method))
	   (cache   	 	(rmime-cache)))

      ;; See if we have formatted here before.
      ;;
      ;; If rmime-mimeinfo-method switched buffers on us, the original
      ;; buffer was a composition buffer (for example, *mail*) and we
      ;; have to reformat anyway because something may have changed.
      (if (and cache (eq rmime-source-buffer (current-buffer)))

	  ;; We have formatted here before.  All we have to do is
	  ;; remember our latest restriction for (rmime-after-save).
	  (progn (move-marker rmime-restriction-start (point-min))
		 (move-marker rmime-restriction-end   (point-max)))

	;; If we get here it is possible that we have never been
	;; called in this buffer before.  See if this is indeed the
	;; case.
	(if (identity rmime-restriction-end)

	    ;; If we have been initialized, we move our existing
	    ;; markers rather than create new ones.  We have to
	    ;; remember our latest restriction for (rmime-after-save).
	    (progn (move-marker rmime-restriction-start (point-min))
		   (move-marker rmime-restriction-end   (point-max)))

	  ;; We haven't been initialized yet.  So do it!

	  ;; Reverting can really hose us up if the first character
	  ;; that is different comes right after a character with
	  ;; special text properties.  We use this to nuke all the
	  ;; properties we created.
	  ;;
	  ;; A mode change should turn off rmime-mode, so we have to
	  ;; clean up.  Not to mention that we will bomb if our local
	  ;; variables get blown away without corresponding updates to
	  ;; our existing overlays.
	  ;;
	  ;; Setting write-contents-hooks and after-save-hook is
	  ;; really optional.  The idea is to clean up as much as we
	  ;; can before a user-requested buffer save.  We don't need
	  ;; to do this because we must always be prepared to
	  ;; encounter an uncleaned autosave file.
	  (if (fboundp 'make-local-hook)
	      (progn (make-local-hook 'before-revert-hook)
		     (add-hook	      'before-revert-hook     'rmime-cancel-all	   nil t)
		     (make-local-hook 'change-major-mode-hook)
		     (add-hook	      'change-major-mode-hook 'rmime-cancel-all	   nil t)
		     (make-local-hook 'write-contents-hooks)
		     (add-hook	      'write-contents-hooks   'rmime-contents-hook nil t)
		     (make-local-hook 'after-save-hook)
		     (add-hook        'after-save-hook	      'rmime-after-save	   nil t))
	    (make-local-variable 'before-revert-hook)
	    (add-hook		 'before-revert-hook	 'rmime-cancel-all)
	    (make-local-variable 'change-major-mode-hook)
	    (add-hook		 'change-major-mode-hook 'rmime-cancel-all)
	    (make-local-variable 'write-contents-hooks)
	    (add-hook		 'write-contents-hooks	 'rmime-contents-hook)
	    (make-local-variable 'after-save-hook)
	    (add-hook		 'after-save-hook	 'rmime-after-save))

	  ;; We need to use write-region-annotate-functions so that
	  ;; our auto-save files are useful.  Autosaving does not call
	  ;; either of the two hooks we just set.
	  ;;
	  ;; Someday we would like to use make-local-hook instead of
	  ;; make-local-variable, but Fwrite_region() needs changed
	  ;; first.
	  (make-local-variable 'write-region-annotate-functions)
	  (add-hook 'write-region-annotate-functions 'rmime-annotate)

	  ;; We have to remember our latest restriction for
	  ;; (rmime-after-save).
	  ;;
	  ;; We are now initialized.
	  (setq rmime-restriction-start (point-min-marker))
	  (setq rmime-restriction-end   (point-max-marker)))

	;; We may call narrow-to-region inside the next if statement.
	(save-restriction

	  ;; Prepare the buffer to hold the formatted data.
	  ;;
	  ;; There are two ways of doing this depending on whether or
	  ;; not the formatted data is to be in the same buffer as the
	  ;; raw data.
	  (if (eq rmime-source-buffer (current-buffer))

	      ;; The formatted message is to be displayed in the same
	      ;; buffer as the raw representation.  Narrow the
	      ;; restriction to exclude any headers.  Everything
	      ;; before point is formatted, everything after is raw.
	      ;; At the end of the (unwind-protect), we will keep one
	      ;; or the other.
	      (narrow-to-region (point) (point-max))

	    ;; The formatted message is to be displayed in a new
	    ;; buffer.  The buffer is already selected, but not empty.
	    (erase-buffer))

	  ;; Canonicalize header information.
	  (let ((type                      (rmime-uncomment (nth 0 headers)))
		(encoding    (downcase (or (rmime-uncomment (nth 1 headers)) "")))
		(disposition               (rmime-uncomment (nth 2 headers)))
		(description                                (nth 3 headers)))

	    ;; Prepare for formatting.
	    (let ((handler (rmime-handler type))
		  (overlay (make-overlay (point-min) (point-max))))

	      ;; Put the overlay in the list right away so
	      ;; rmime-cancel-all can find it even if formatting
	      ;; bombs.
	      (setq rmime-overlay-list (cons overlay rmime-overlay-list))

	      ;; Now go do the formatting.
	      ;;
	      ;; The point separates the formatted body from the raw
	      ;; body until formatting is complete.  If formatting
	      ;; bombs, we delete the incomplete formatted body.
	      (rmime-modify-buffer
		(unwind-protect

		    ;; We try to take a shortcut if we can.  There are
		    ;; two shortcuts available to us.  Both require
		    ;; that the message is plain text, the data is in
		    ;; the current buffer, and the message is to be
		    ;; presented inline.
		    ;;
		    ;; The first shortcut requires that the encoding
		    ;; be transparent.  Nearly all incoming messages
		    ;; qualify for this shortcut.
		    ;;
		    ;; The second shortcut requires the message body
		    ;; to already be decoded.  This is indicated by
		    ;; the presence of rmime-magic-string.  This lets
		    ;; us decode quoted-printable encoding once for
		    ;; the entire life of the message.  Of course,
		    ;; rmime-clean-files must be nil for this to work
		    ;; well.  No, we can't get here if this message
		    ;; has already been encountered in this session
		    ;; because (rmime-cache) would have found a good
		    ;; overlay.  This shortcut caches quoted-printable
		    ;; encoding across sessions.
		    (if (and (eq (indirect-function handler) (indirect-function 'rmime-text/plain))
			     (eq rmime-source-buffer (current-buffer))
			     (or (member encoding rmime-transparent-encodings)
				 (search-forward rmime-magic-string nil t)))

			;; A shortcut applies.
			(let ((charset (downcase (or (cdr (assoc "charset" (save-match-data (rmime-parameters type)))) ""))))

			  ;; See which one.
			  (if (member encoding rmime-transparent-encodings)

			      ;; The first shortcut applies.  All we
			      ;; have to do is to select a face from
			      ;; the charset parameter.
			      ;;
			      ;; We need to keep point at point-min.
			      ;; If we catch an interrupt or error, we
			      ;; don't want the cleanup code to erase
			      ;; this body part.
			      ;;
			      ;; Note that we select the face with an
			      ;; overlay instead of a text property.
			      ;; This is important when we cancel
			      ;; formatting.  The formatting goes away
			      ;; on its own when we delete the
			      ;; overlay.
			      (progn (if (not (member charset rmime-transparent-charsets))
					 (overlay-put overlay 'face (intern charset)))
				     (overlay-put overlay 'rmime-p t))

			    ;; The second shortcut applies.
			    ;;
			    ;; The raw content is from (match-end 0) to (point-max).
			    ;; The decoded content is from (point-min) to (match-beginning 0).
			    (if (not (member charset rmime-transparent-charsets))
				(overlay-put overlay 'face (intern charset)))
			    (overlay-put overlay 'rmime-raw (list (buffer-substring (match-end 0) (point-max))
								  (identity 1)
								  (- (match-end 0) (point-max) -1)))
			    (overlay-put overlay 'rmime-p t)
			    (goto-char (point-min)) ; Defeat the delete-region in the unwind-protect
			    (delete-region (match-beginning 0) (point-max))))

		      ;; The shortcuts do not apply.  First order of
		      ;; business is cleaning up.
		      ;;
		      ;; If we find any occurances of
		      ;; rmime-magic-string, it must be because we are
		      ;; looking at an auto-save file.  Everything up
		      ;; to the magic string is formatted MIME that
		      ;; has lost its text properties.  It needs to be
		      ;; deleted before it can be redone.
		      (while (search-forward rmime-magic-string nil t)
			(delete-region (point-min) (point)))

		      ;; Dispatch to the proper handler the hard way.
		      (let* ((content 	(save-excursion (set-buffer rmime-source-buffer)
							(list (buffer-substring (point) (point-max))
							      (identity 1)
							      (- (point-max) (point) -1))))
			     (playlist  (funcall handler content type encoding disposition description))
			     (separator (point)))

			;; rmime-p must come before delete-region.
			;; If we take an interrupt, a second call
			;; to rmime will format already formated
			;; text (and destroy it in the process).
			(overlay-put overlay 'rmime-play-list playlist)
			(overlay-put overlay 'rmime-raw content)
			(overlay-put overlay 'rmime-p t)
			(goto-char (point-min))
			(delete-region separator (point-max))))

		  ;; This is in an (unwind-protect) so it gets
		  ;; executed if formatting succeeds or fails.  If
		  ;; formatting succeeded, the point got moved to
		  ;; (point-min) and this does nothing.  If formatting
		  ;; failed, point is right between the raw and
		  ;; formatted text.  We delete the incomplete
		  ;; formatted text.
		  (delete-region (point-min) (point)))

		;; Now that MIME formatting has succeeded, it is safe
		;; to activate the minor mode indicator.
		(setq rmime-mode t)))))))))

(defun rmime-contents-hook ()		; A member of write-contents-hooks
  (if (identity rmime-clean-files)
      (rmime-cancel-all)))		; We depend on rmime-cancel-all returning nil

(defun rmime-cancel-all ()
  (setq rmime-mode nil)
  (rmime-modify-buffer
    (save-excursion
      (save-restriction
	(widen)
	(mapcar 'rmime-cancel-1 rmime-overlay-list)))
    (setq rmime-overlay-list nil)))	; rmime-contents-hook depends on us returning nil

;; This guy restores the restriction last noted by (rmime-format).
;; (basic-save-buffer) tried to save the restriction with
;; (save-restriction), but we lost when we edited the buffer outside
;; of the restriction.
;;
;; BUG: If the message being displayed when the save was requested was
;; not the last message to call (rmime-format), we loose big time.
;; Sorry, this is the best I can manage.
(defun rmime-after-save ()		; A member of after-save-hooks
  (narrow-to-region rmime-restriction-start rmime-restriction-end))

(defun rmime-cancel ()
  (setq rmime-mode nil)
  (save-excursion
    (funcall rmime-mimeinfo-method)	; Moves point and sets buffer
    (let ((cache (rmime-cache)))
      (if (identity cache)
	  (rmime-modify-buffer
	    (rmime-cancel-2 cache))))))

;; We are careful to deal with deleted overlays as well as empty ones.
;; It doesn't hurt to delete an already deleted overlay.
(defun rmime-cancel-1 (cache)
  (if (eq (overlay-start cache) (overlay-end cache))
      (delete-overlay cache)
    (rmime-cancel-2 cache)))

(defun rmime-cancel-2 (cache)
  (if (overlay-get cache 'rmime-raw)
      (progn (goto-char (overlay-start cache))
	     (insert (car (overlay-get cache 'rmime-raw)))
	     (delete-region (point) (overlay-end cache))))
  (delete-overlay cache))

;; It is tempting to make this function recursive.  Unfortunately,
;; this can blow away max-lisp-eval-depth.
;;
;; It is important that annotations be sorted.  We assume that that
;; the number of annotations will be small compared to the total
;; number of messages so we sort the output list rather than the input
;; list.
;;
;; It is important that no annotation begin before start.  See
;; a_write() in fileio.c.
;;
;; We are careful to be able to deal with already deleted overlays.
(defun rmime-annotate (start end)
  (let ((list rmime-overlay-list) new result)
    (while (identity list)
      (if (eq (overlay-start (car list)) (overlay-end (car list)))
	  (delete-overlay (car list))	; Delete the empty rmime overlay.
	(let ((raw   (overlay-get (car list) 'rmime-raw))
	      (point (overlay-end (car list))))
	  (if (and (identity raw) (>= point start) (<= point end))
	      (setq result (cons (cons point rmime-magic-string) (cons (cons point (car raw)) result))))
	  (setq new (cons (car list) new))))
      (setq list (cdr list)))
    (setq rmime-overlay-list new)
    (sort result 'car-less-than-car)))

;; Return the overlay that handles the current message in our buffer.
;; We look for the overlay at (point-max) --- there might be headers
;; before the message but there won't be any after.  Neat!
(defun rmime-cache ()
  (if (> (point-max) (point-min))
      (let ((overlays (overlays-at (1- (point-max)))))
	(while (and (identity overlays)
		    (not (overlay-get (car overlays) 'rmime-p)))
	  (setq overlays (cdr overlays)))
	(car overlays))))


;;; Text type ---


(defun rmime-text/plain (content type encoding disposition description)
  (rmime-text content type encoding disposition description 'ignore))

(defun rmime-text/enriched (content type encoding disposition description)
  (rmime-text content type encoding disposition description 'enriched-decode))

;; We need to truly undefine text/enriched if we don't have it.  Just
;; making text/enriched call text/plain won't prevent
;; multipart/alternative from selecting the wrong part.
(or (fboundp 'enriched-decode)
    (fmakunbound 'rmime-text/enriched))

(defun rmime-text (content type encoding disposition description formatting)
  (let ((here    (point))
	(charset (downcase (or (cdr (assoc "charset" (rmime-parameters type))) ""))))
    (rmime-insert-content nil encoding content)
    (if (not (member charset rmime-transparent-charsets))
	(put-text-property here (point) 'face (intern charset)))
    (funcall formatting here (point))
    (identity nil)))

(defalias 'rmime-text/* 'rmime-text/plain)
(put 'rmime-text/* 'rmime-unwise t)	; So multipart/alternative avoids this choice


;;; Multipart type ---


(defun rmime-multipart/mixed (content type encoding disposition description)
  (rmime-barf-if-opaque encoding)
  (let ((rmime-default-type 'rmime-text/plain)
	(rmime-multipart-alist (rmime-multipart-split content (rmime-parameters type))))
    (apply 'append (mapcar 'rmime-multipart-dispatch rmime-multipart-alist))))

(defun rmime-multipart/parallel (content type encoding disposition description)
  (rmime-barf-if-opaque encoding)
  (let ((rmime-default-type 'rmime-text/plain))
    (mapcar 'rmime-multipart-dispatch  (rmime-multipart-split content (rmime-parameters type)))
    (cons (list nil content type encoding) nil)))

(defun rmime-multipart/alternative (content type encoding disposition description)
  (rmime-barf-if-opaque encoding)
  (let ((rmime-default-type 'rmime-text/plain)
	(alist (rmime-multipart-split content (rmime-parameters type))))

    ;; Three possible ways of formatting the multipart/alternative
    ;; message:
    ;;
    ;; 1) Search the list for the last "good" body part and display
    ;;    it.  A good body part is one that doesn't have the
    ;;    rmime-unwise property set on its handler.
    ;;
    ;; 2) Give up if there are no bodyparts whatsoever.
    ;;
    ;; 3) Display the first body part best we can, whether it is good
    ;;    or not.
    (or (rmime-alternative (cdr alist))
	(not alist)
	(rmime-multipart-dispatch (car alist))))

  ;; Provide the whole message for playing.  When the message is
  ;; displayed, metamail may pick a different body part.
  (cons (list nil content type encoding) nil))

(defun rmime-alternative (bodies)
  (if (identity bodies)
      (or (rmime-alternative (cdr bodies))
	  (if (not (get (rmime-handler (rmime-uncomment (nth 2 (car bodies)))) 'rmime-unwise))
	      (let ((backtrack (point)))
		(condition-case nil
		    (progn (rmime-multipart-dispatch (car bodies))
			   (identity t))
		  (rmime-unknown-conventions (delete-region backtrack (point)))))))))

(defun rmime-multipart/digest (content type encoding disposition description)
  (rmime-barf-if-opaque encoding)
  (let ((rmime-default-type 'rmime-message/rfc822)
	(alist (rmime-multipart-split content (rmime-parameters type))))
    (insert rmime-digest-toc-intro)
    (apply 'append (mapcar 'rmime-digest-dispatch (rmime-digest-toc alist)))))

(defun rmime-digest-toc (alist)
  (apply 'append (mapcar 'rmime-digest-toc-1 alist)))

(defun rmime-digest-toc-1 (body)
  (let ((here   (point))
	(marker (make-marker))
	(keymap (copy-keymap (or (current-local-map) '(keymap))))
	(type   (rmime-uncomment (nth 2 body))))
    (let ((handler (rmime-handler type)))
      (insert (identity rmime-digest-indentation)
	      (or (nth 5 body)
		  (if (eq handler 'rmime-message/rfc822)
		      (rmime-subject-description (nth 1 body)))
		  (identity "No description provided"))
	      (identity "\n"))
      (if (eq handler 'rmime-multipart/digest)
	  (let ((rmime-digest-indentation (concat rmime-digest-indentation rmime-digest-recursive-indentation)))
	    (rmime-digest-toc (rmime-multipart-split (nth 1 body) (rmime-parameters type))))
	(rmime-interact keymap 'rmime-digest-item-action marker)
	(put-text-property here (point) 'local-map keymap)
	(put-text-property here (point) 'face      'rmime-markup-face)
	(cons (cons marker body) nil)))))

(defun rmime-subject-description (content)
  (rmime-content-excursion (content)
    (goto-char (point-min))
    (if (re-search-forward "^[ \t]*\n" nil t)
	(narrow-to-region (point-min) (match-beginning 0)))
    (let ((case-fold-search t))
      (rmime-fetch-header "Subject"))))

(defun rmime-digest-dispatch (body)
  (insert rmime-digest-item-intro)
  (set-marker (car body) (point))
  (rmime-multipart-dispatch (cdr body)))

(defun rmime-digest-item-action (marker)
  (push-mark)
  (goto-char marker)
  (or (pos-visible-in-window-p)
      (recenter 2)))

(defun rmime-multipart-dispatch (body)
  (let ((content                                    (nth 1 body))
        (type                      (rmime-uncomment (nth 2 body)))
        (encoding    (downcase (or (rmime-uncomment (nth 3 body)) "")))
        (disposition               (rmime-uncomment (nth 4 body)))
        (description                                (nth 5 body)))
    (prog1
        (funcall (rmime-handler type) content type encoding disposition description)
      (if (not (bolp)) (insert "\n")))))

;; This recursive function splits a multipart MIME message.  A list of
;; the subparts is returned.  The car of each element is the
;; Content-Id.  A body part that needs to know about other body parts
;; can use assoc.  The cdr of each element is suitable to be applied
;; to rmime-dispatch.
(defun rmime-multipart-split (content parameters)
  (let ((boundary (cdr (assoc "boundary" parameters))))
    (if (or (not (stringp boundary)) (string= boundary ""))
	(error "A MIME multipart message has no boundary string"))
    (let ((separator (concat "^--" (regexp-quote boundary)))
	  (case-fold-search))
      (rmime-content-excursion (content)
	(goto-char (point-min))
	(if (and (re-search-forward separator nil t)
		 (eq (forward-line) 0))
	    (rmime-multipart-split-1 separator))))))

(defun rmime-multipart-split-1 (separator)
  (let ((start (point)))
    (if (re-search-forward separator nil t)
	(cons (save-excursion
		(beginning-of-line)
		(save-restriction
		  (if (> (point) start)
		      (narrow-to-region start (1- (point)))
		    (narrow-to-region (point) (point)))
		  (goto-char (point-min))
		  (re-search-forward "^[ \t]*\n" nil 'move)
		  (let ((case-fold-search t)
			(content (list rmime-tmp-string (point) (point-max))))
		    (forward-line -1)
		    (narrow-to-region (point-min) (point))
		    (cons (rmime-fetch-header "Content-Id")
			  (cons content (rmime-standard-headers))))))
	      (if (and (not (looking-at "--"))
		       (eq (forward-line) 0))
		  (rmime-multipart-split-1 separator))))))


;;; Message/RFC822 type ---


(defun rmime-message/rfc822 (content type encoding disposition description)
  (rmime-barf-if-opaque encoding)
  (let ((case-fold-search    t)		; For highlighed & ignored headers, below.
	(rmime-default-type 'rmime-text/plain)
	(here                (point))
	(buffer              (current-buffer)))

    (let (mimeinfo body headers)

      (rmime-content-excursion (content)
	(setq mimeinfo (rmime-regexp-mimeinfo "^[ \t]*\n"))
	(setq headers  (list rmime-tmp-string (point-min) (point)))
	(setq body     (list rmime-tmp-string (point)     (point-max))))

      ;; This save-restriction is not inside a save-excursion.
      ;; We intend to move the point.
      (save-restriction

	;; Narrow to just the headers (which aren't inserted yet).
	(narrow-to-region (point) (point))

	;; Insert the headers.
	(rmime-insert-unencoded-content headers)

	;; Nuke any whitespace (except the last newline) at the end of
	;; the headers.  We will reinsert a blank line latter if
	;; anything is left of the headers after applying
	;; rmime-ignored-headers.
	(backward-delete-char (skip-chars-backward " \t\n"))
	(insert "\n")

	;; Since we are backtracking, we need save-excursion so that
	;; things come out right if we get some kind of interrupt.
	(save-excursion

	  ;; Apply either rmail-displayed-headers or rmail-ignored-headers.
	  (goto-char (point-min))
	  (if (and (boundp 'rmail-displayed-headers)
		   (identity rmail-displayed-headers))
	      (let ((start  (point))
		    (regexp (concat "\\(" rmail-displayed-headers "\\)\\(\\(\n[ \t]\\|.\\)*\n\\)")))
		(while (re-search-forward regexp nil t)
		  (delete-region start (match-beginning 0))
		  (setq start (point)))
		(delete-region start (point-max)))
	    (let ((regexp (concat "\\(" rmail-ignored-headers "\\)\\(\\(\n[ \t]\\)\\|.\\)*\n")))
	      (while (re-search-forward regexp nil t)
		(delete-region (match-beginning 0) (match-end 0)))))

	  ;; Apply rmail-highlighted-headers.
	  (goto-char (point-min))
	  (while (re-search-forward rmail-highlighted-headers nil t)
	    (if (looking-at "[ \t]*\\([^ \t\n]\\(\\(\n?[ \t]+\\)?[^ \t\n]\\)*\\)")
		(put-text-property (match-beginning 1) (match-end 1) 'face rmime-highlight-face))))

	;; Reinsert the newline at the end of the headers, provided
	;; there are any headers.
	(or (eq (point-min) (point))
	    (insert "\n"))

	;; Now insert the message body at the end of the headers.
	(rmime-multipart-dispatch (cons nil (cons body mimeinfo)))))))


;;; Message/Partial type ---


(defun rmime-message/partial (content type encoding disposition description)
  (rmime-barf-if-opaque encoding)
  (let ((parameters (rmime-parameters type)))
    (let ((number (cdr (assoc "number" parameters)))
	  (total  (cdr (assoc "total"  parameters)))
	  (id     (cdr (assoc "id"     parameters)))
	  (here   (point))
	  (keymap (copy-keymap (or (current-local-map) '(keymap)))))
      (if (not id)
	  (error "A MIME partial message has no id"))
      (if (not number)
	  (error "A MIME partial message has no part number"))
      (save-excursion
	(set-buffer (get-buffer-create (concat "*rmime-" id "*")))
	(setq buffer-read-only nil)
	(setq rmime-partial-partlist (rmime-partial-1 (string-to-int number) rmime-partial-partlist content))
	(setq buffer-read-only t)
	(set-buffer-modified-p nil)
	(if (identity total)
	    (setq rmime-partial-parts (string-to-int total)))
	(goto-char (point-min))
	(rmime-interact keymap 'rmime-partial-action (current-buffer)))
      (cond (description (insert rmime-leader description "\n"))
	    (total (insert rmime-leader (format "Part %s of message %s (%s total parts)\n" number id total)))
	    ((insert rmime-leader (format "Part %s of message %s\n" number id))))
      (put-text-property here (point) 'local-map keymap)
      (put-text-property here (point) 'face      'rmime-markup-face)
      (identity nil))))

;; This function inserts one part of a partial message into the current buffer.
;;
;; number is the part number.
;; alist is an ordered association list of (part number . last-char-marker)
;; content is a data structure suitable to be given to rmime-insert-unencoded-content.
;;
;; We return a new association list.
(defun rmime-partial-1 (number alist content)
  (cond ((null alist)
	 (goto-char (point-min))
	 (rmime-insert-unencoded-content content)
	 (cons (cons number (point-marker)) nil))
	((< (car (car alist)) number)
	 (goto-char (cdr (car alist)))
	 (rmime-insert-unencoded-content content)
	 (cons (cons number (point-marker)) alist))
	((> (car (car alist)) number)
	 (cons (car alist) (rmime-partial-1 number (cdr alist) content)))
	(alist)))

(defun rmime-partial-action (buffer)
  (let ((variables (buffer-local-variables buffer)))
    (let ((total (cdr (assoc 'rmime-partial-parts    variables)))
	  (list  (cdr (assoc 'rmime-partial-partlist variables))))
      (if (not (eq total (length list)))
	  (if (identity total)
	      (message "You have seen only %d of %d parts" (length list) total)
	    (message "You have seen only %d parts" (length list)))
	(pop-to-buffer buffer)
	(rmime-format)
	(rmime-play-start)))))


;;; Message/External-Body type ---


;; We need an error symbol to signal if we don't recognize the
;; access-type.  This error symbol contains
;; rmime-unknown-conventions in its `error-conditions' property.
;; rmime-alternative catches errors with the rmime-unknown-conventions
;; property.
(put 'rmime-unknown-accesstype 'error-conditions '(rmime-unknown-accesstype rmime-unknown-conventions error))
(put 'rmime-unknown-accesstype 'error-message "An External-body MIME message has an unknown access type")

(defun rmime-message/external-body (content type encoding description disposition)
  (rmime-barf-if-opaque encoding)
  (prog1
      (cons (list (point-marker) content type encoding) nil)
    (let ((parameters (rmime-parameters type))
	  (here       (point))
	  (keymap     (copy-keymap (or (current-local-map) '(keymap))))
	  (case-fold-search t))		; Propagated to rmime-fetch-header
      (rmime-content-excursion (content)
	(let ((mimeinfo (rmime-regexp-mimeinfo "^[ \t]*\n"))
	      (body     (list rmime-tmp-string (point) (point-max)))
	      (id       (progn (narrow-to-region (point) (point-min)) (rmime-fetch-header "Content-Id")))
	      (access   (cdr (assoc "access-type" parameters))))
	  (if (or (null id) (string= id ""))
	      (error "A MIME external-body message has no Content-Id"))
	  (if (null description)
	      (setq description (nth 3 mimeinfo)))
	  (cond ((equal access "mail-server")
		 (rmime-interact keymap 'rmime-mailserver-action parameters body))
		((equal access "anon-ftp")
		 (rmime-interact keymap 'rmime-ftp-action        parameters id mimeinfo t))
		((equal access "ftp")
		 (rmime-interact keymap 'rmime-ftp-action        parameters id mimeinfo nil))
		((equal access "afs")
		 (rmime-interact keymap 'rmime-file-action       parameters mimeinfo))
		((equal access "local-file")
		 (rmime-interact keymap 'rmime-file-action       parameters mimeinfo))
		((signal 'rmime-unknown-accesstype (list access))))))
      (insert rmime-leader (or description (format "A reference to data stored in an external location")) "\n")
      (put-text-property here (point) 'local-map keymap)
      (put-text-property here (point) 'face      'rmime-markup-face))))

(defun rmime-ftp-action (parameters id mimeinfo anon)
  (let ((buffer (get-buffer (concat "*rmime-" id "*"))))
    (if (identity buffer)
	(pop-to-buffer buffer)
      (let ((name      (cdr (assoc "name"      parameters)))
	    (site      (cdr (assoc "site"      parameters)))
	    (directory (cdr (assoc "directory" parameters)))
	    (mode      (cdr (assoc "mode"      parameters)))
	    (user      (if (identity anon) "anonymous"))
	    (pass      (if (identity anon) (concat (user-login-name) "@" (system-name))))
	    (modes     (default-file-modes))
	    (control   (make-temp-name "/tmp/ftp"))
	    (data      (make-temp-name "/tmp/mime")))

	(if (or (null name) (string= name ""))
	    (error "A MIME anon-ftp message has no name parameter"))
	(if (or (null site) (string= site ""))
	    (setq site (read-from-minibuffer "Site for FTP access: ")))
	(if (or (null user) (string= user ""))
	    (setq user (read-from-minibuffer (concat "Username on " site ": "))))
	(if (or (null pass) (string= pass ""))
	    (setq pass (read-from-minibuffer (concat "Password for " user "@" site ": "))))

	(unwind-protect
	    (progn
	      (pop-to-buffer (get-buffer-create "*ftp*"))
	      (if (not (bobp))
		  (progn (goto-char (point-max))
			 (insert "\f\n")))
	      (set-window-start nil (point))
	      (sit-for 0)
	      (set-default-file-modes 448) ; -rwx------
	      (write-region (concat "open " site                            "\n"
				    "user " user " " pass                   "\n"
				    (if mode      (concat "type " mode      "\n"))
				    (if directory (concat "cd "   directory "\n"))
				    "get "  name " " data                   "\n"
				    "quit"                                  "\n")
			    nil control nil 'quiet)
	      (let ((here (point)))
		(call-process rmime-ftp-program control t t "-nv")
		(if (not (save-excursion (re-search-backward "^226 " here t)))
		    (error "FTP file transfer failed")))
	      (bury-buffer)		; Too bad pop-to-buffer doesn't have a NORECORD option
	      (switch-to-buffer (get-buffer-create (concat "*rmime-" id "*")))
	      (kill-all-local-variables) ; In case of strange default-major-mode.
	      (insert-file-contents data)
	      (make-local-variable 'rmime-mimeinfo-method)
	      (setq rmime-mimeinfo-method (list 'lambda 'nil '(goto-char (point-min)) (list 'quote mimeinfo)))
	      (rmime-format))
	  (condition-case nil (delete-file data)    (file-error))
	  (condition-case nil (delete-file control) (file-error))
	  (set-default-file-modes modes)))))
  (rmime-play-start))

(defun rmime-file-action (parameters mimeinfo)
  (let ((name (cdr (assoc "name" parameters))))
    (if (or (null name) (string= name ""))
	(error "A MIME local-file message has no name"))
    (find-file name)
    (make-local-variable 'rmime-mimeinfo-method)
    (setq rmime-mimeinfo-method (list 'lambda 'nil '(goto-char (point-min)) (list 'quote mimeinfo))))
  (rmime-format)
  (rmime-play-start))

(defun rmime-mailserver-action (parameters body)
  (let ((server  (cdr (assoc "server"  parameters)))
	(subject (cdr (assoc "subject" parameters))))
    (if (or (null server) (string= server ""))
	(error "A MIME mail-server message has no address"))
    (if (mail nil server subject nil nil nil nil)
	(save-excursion
	  (goto-char (point-max))
	  (rmime-insert-unencoded-content body)))))


;;; Application/Octet-Stream type ---


(defun rmime-application/octet-stream (content type encoding disposition description)
  (let ((here   (point))
	(keymap (copy-keymap (or (current-local-map) '(keymap))))
	(name   (or (cdr (assoc "filename" (rmime-parameters disposition)))
		    (cdr (assoc "name"     (rmime-parameters type))))))
    (insert rmime-leader (or description "Press C-c C-c here to receive file transmission") "\n")
    (rmime-interact keymap 'rmime-octet-action name content encoding)
    (put-text-property here (point) 'local-map keymap)
    (put-text-property here (point) 'face      'rmime-markup-face))
  (identity nil))

(defun rmime-octet-action (name content encoding)
  (let ((filename (read-file-name "Save transmitted file as: " nil nil nil name)))
    (pop-to-buffer (or (get-file-buffer filename) (create-file-buffer filename)))
    (erase-buffer)
    (rmime-insert-content t encoding content)
    (goto-char (point-min))
    (set-visited-file-name filename)
    (normal-mode)
    (save-buffer)))


;;; Default type ---


(defun rmime-*/* (content type encoding disposition description)
  (let ((here   (point))
	(keymap (copy-keymap (or (current-local-map) '(keymap)))))
    (prog1
	(cons (list (point-marker) content type encoding) nil)
      (insert rmime-leader (or description (format "Press C-c C-c here for \"%s\" data" type)) "\n")
      (rmime-interact keymap 'rmime-action content type encoding)
      (put-text-property here (point) 'local-map keymap)
      (put-text-property here (point) 'face      'rmime-markup-face))))
(put 'rmime-*/* 'rmime-unwise t)	; So multipart/alternative avoids this choice

(defun rmime-action (content type encoding)
  (let ((filename (make-temp-name "/tmp/metamail")))
    (rmime-write-content filename content)
    (call-process rmime-metamail-program nil 0 nil "-d" "-b" "-x" "-z" "-m" "emacs" "-c" type "-E" encoding filename)))


;;; Play command ---


(defun rmime-play (arg)
  "Sequentially play nontextual MIME message bodies.
With arg, cancel any playback.

The overlay arrow is used to align the nontextual message bodies with
the textual ones which will appear in the Emacs buffer."
  (interactive "P")
  (save-excursion
    (funcall rmime-mimeinfo-method)	; Moves point and sets buffer
    (if (if (identity arg) (> (prefix-numeric-value arg) 0) (not rmime-process))
	(rmime-play-start)
      (rmime-play-stop))))
(defalias 'play 'rmime-play)

(defun rmime-play-start ()
  (let ((overlay (rmime-cache)))
    (if (null overlay)
	(error "Nothing to play!"))
    (if (not rmime-process)
	(progn (make-local-variable 'overlay-arrow-string)
	       (setq overlay-arrow-string rmime-arrow)
	       (if (setq rmime-play-list (overlay-get overlay 'rmime-play-list))
		   (rmime-start-metamail))))))

(defun rmime-play-stop ()
  (if (identity rmime-process)
      (progn (setq rmime-play-list nil)	; Let the sentinel do the cleanup!
	     (interrupt-process rmime-process t))))

(defun rmime-sentinel (process change)
  (if (buffer-name (process-buffer process)) ; Killed buffer?
      (save-excursion
	(set-buffer (process-buffer process))
	(if (identity rmime-play-list)	; See if there is a next part to play
	    (rmime-start-metamail)	; If so, start metamail to play it
	  (setq overlay-arrow-position nil)
	  (setq rmime-process nil)))))	; So we get the right error if canceled after finished.

(defun rmime-start-metamail ()
  (let ((filename (make-temp-name "/tmp/metamail"))
	(body     (car rmime-play-list))
	(process-connection-type))
    (rmime-write-content filename (nth 1 body))
    (setq rmime-process (start-process "metamail" (current-buffer) rmime-metamail-program "-b" "-x" "-z" "-c" (nth 2 body) "-E" (or (nth 3 body) "binary") "-m" "emacs" filename))
    (setq overlay-arrow-position (car body)))
  (set-process-filter rmime-process t)	; So no random output gets inserted into the buffer
  (set-process-sentinel rmime-process 'rmime-sentinel)
  (setq rmime-play-list (cdr rmime-play-list)))


;;; Useful subroutines ---


(defun rmime-barf-if-opaque (encoding)
  (or (member encoding rmime-transparent-encodings)
      (error "A MIME bodypart has an explicitly prohibited Content-Transfer-Encoding")))

;; We need an error symbol to signal if we don't recognize the
;; transfer encoding.  This error symbol contains
;; rmime-unknown-conventions in its `error-conditions' property.
;; rmime-alternative catches errors with the rmime-unknown-conventions
;; property.
(put 'rmime-unknown-encoding 'error-conditions '(rmime-unknown-encoding rmime-unknown-conventions error))
(put 'rmime-unknown-encoding 'error-message "A MIME message has an unknown Content-Transfer-Encoding")

(defun rmime-insert-content (binaryp encoding content)
  (cond ((member encoding rmime-transparent-encodings)
	 (rmime-insert-unencoded-content                                                 content))

	((and rmime-mimencode-program (string= encoding "base64"))
	 (rmime-decode-content rmime-mimencode-program (if binaryp '("-u") '("-u" "-p")) content))

	((and rmime-mimencode-program (string= encoding "quoted-printable"))
	 (rmime-decode-content rmime-mimencode-program '("-u" "-q")                      content))

	((and rmime-uufilter-program  (string= encoding "x-uue"))
	 (rmime-decode-content rmime-uufilter-program  '("-s")                           content))

	((signal 'rmime-unknown-encoding (list encoding)))))

(defun rmime-insert-unencoded-content (content)
  (insert-buffer-substring (save-excursion (apply 'rmime-tmp-buffer content))))

(defun rmime-decode-content (program args content)
  (let ((filename (make-temp-name "/tmp/mime")))
    (unwind-protect
	(progn (rmime-write-content filename content)
	       (or (eq (apply 'call-process program filename t nil args) 0)
		   (error "couldn't decode contents")))
      (condition-case nil (delete-file filename) (file-error)))))

(defun rmime-write-content (filename content)
  (let ((modes (default-file-modes)))
    (unwind-protect
	(rmime-content-excursion (content)
	  (set-default-file-modes 448)	; -rwx------
	  (write-region (point-min) (point-max) filename nil 'quiet))
      (set-default-file-modes modes))))

(defun rmime-tmp-buffer (string min max)
  (prog1
      (set-buffer (get-buffer-create rmime-tmp-buffer-name))
    (if (not (eq string rmime-tmp-string))
	(progn (setq rmime-tmp-string nil)
	       (erase-buffer)
	       (insert string)
	       (setq rmime-tmp-string string)))
    (widen)
    (narrow-to-region min max)))

(defun rmime-interact (keymap function &rest arguments)
  (let ((action (make-symbol "rmime-interaction")))
    (fset action (list 'lambda nil '(interactive) (list 'apply (list 'quote function) (list 'quote arguments))))
    (define-key keymap "\C-c\C-c" action)))


;;; Routines for collecting information about a MIME entity ---


;; This function returns the function to call to display this
;; mime-type.
;;
;; We look at the Content-Type field and expect to find a string of
;; the form "type/subtype".  There may be whitespace around the slash
;; and case is insignificant.  If we find a Content-Type field that we
;; can't understand, we use the fallback handler (rmime-*/*).  If we
;; can't find a Content-Type field, we assume the default type.  The
;; default type is usually text/plain, but the multipart/digest mode
;; sets a local default type of message/rfc822.
(defun rmime-handler (type)
  (if (and type (string-match "\\([!#$%&'*+---.0-9A-Z^_a-z{|}~]+\\)[ \t]*/[ \t]*\\([!#$%&'*+---.0-9A-Z^_a-z{|}~]+\\)" type))
      (or (rmime-handler-1 (downcase (substring type (match-beginning 1) (match-end 1)))
			   (downcase (substring type (match-beginning 2) (match-end 2))))
	  (identity 'rmime-*/*))
    (identity rmime-default-type)))

(defun rmime-handler-1 (basetype subtype)
  (or (rmime-handler-2 basetype subtype)
      (rmime-handler-2 basetype "*")))

(defun rmime-handler-2 (basetype subtype)
  (let ((handler (intern (concat "rmime-" basetype "/" subtype))))
    (if (fboundp handler) handler)))

;; This recursive function parses a MIME parameter list.
;;
;; parameter list is *(";" parameter)
;; parameter      is attribute "=" value
;; attribute      is token
;; token          is 1*[!#$%&'*+---.0-9A-Z^_a-z{|}~]
;; value          is a token or a quoted-string
;;
;; Whitespace is permitted to appear around any token.
;;
;; Notice that this regular expression is not anchored to the
;; beginning of the string.  This means that we will ignore any
;; garbage we happen to find during parsing and continue the best we
;; can.  We depend on this when we pass in a Content-Type field
;; without first removing the basetype/subtype clause.
;;
;; The string ";[ \t]*" matches the leading semicolon and any
;; whitespace that may come after it.
;;
;; The string "[!#$%&'*+---.0-9A-Z^_a-z{|}~]+" matches the attribute.
;;
;; The string "[ \t]*=[ \t]*" matches the equals sign and any
;; whitespace that may come around it.
;;
;; The string "[!#$%&'*+---.0-9A-Z^_a-z{|}~]+" matches the (unquoted)
;; value.
;;
;;				  -- or --
;;
;; The string "\"\\(\\([^\\\"]\\|\\\\.\\)*\\)\"" matches the (quoted)
;;                  **********************
;;               ****************************
;; value.
;;
;; The string ".*" matches anything.
;;
;; Put them together to parse a parameter list.
(defun rmime-parameters (field)
  (if (and field (string-match "\\(;[ \t]*\\)\\([!#$%&'*+---.0-9A-Z^_a-z{|}~]+\\)\\([ \t]*=[ \t]*\\)\\([!#$%&'*+---.0-9A-Z^_a-z{|}~]+\\|\"\\(\\([^\\\"]\\|\\\\.\\)*\\)\"\\)\\(.*\\)" field))
      (cons (cons (downcase (substring field (match-beginning 2) (match-end 2)))
		  (if (match-beginning 5)
		      (save-match-data (rmime-unquote (substring field (match-beginning 5) (match-end 5))))
		    (substring field (match-beginning 4) (match-end 4))))
	    (rmime-parameters (substring field (match-beginning 7) (match-end 7))))))

;; This function extracts MIME information from a buffer which hasn't
;; set rmime-mimeinfo-method.  We look for a function name
;; MAJORMODE-mimeinfo.  If we find it, we call it.  If we don't, we
;; call rmime-standard-mimeinfo.
;;
;; We use this function to add MIME support to major modes that were
;; written before this package was.
;;
;; This function is obsolete before it was even written.  In the
;; future, it will be removed.  rmime-mimeinfo-method will point to
;; rmime-standard-mimeinfo and buffers that need special coding must
;; bind a local value to this variable.
(defun rmime-obsolete-mimeinfo ()
  (let ((function (intern (concat (symbol-name major-mode) "-mimeinfo"))))
    (if (fboundp function)
	(funcall function)
      (rmime-standard-mimeinfo))))

;; This function extracts MIME information from a buffer which
;; contains a header and a body separated by a specified regular
;; expression.  Most buffers fall into this category.
;;
;; The regular expression is stored in rmime-separator-regexp.  This
;; may be a buffer-local variable.  The default value matches a blank
;; line.
;;
;; If the regular expression doesn't match anything, the entire buffer
;; is the header and the body is empty.  This is important when we
;; parse phantom bodies in rmime-message/external-body.
;;
;; Someday this function will be the value of rmime-mimeinfo-method
;; and rmime-obsolete-mimeinfo will no longer exist.
;;
;; MIME infomation is represented as:
;;  	(type description encoding disposition)
;; All fields are character strings or nil if the field is not
;; provided.
(defun rmime-standard-mimeinfo ()
  (rmime-regexp-mimeinfo rmime-separator-regexp))

(defun rmime-regexp-mimeinfo (regexp)
  (goto-char (point-min))
  (let (case-fold-search)
    (let ((found (re-search-forward regexp nil 'move))
	  (case-fold-search t))
      (prog1
	  (save-restriction
	    (if found (narrow-to-region (point-min) (match-beginning 0)))
	    (rmime-standard-headers))
	(goto-char (or found (point-max)))))))

;; This function fetches four headers mentioned in RFC 1521.  The
;; fifth header mentioned in RFC 1521 (Content-Id) has already been
;; dealt with.  (The sixth header (MIME-Version) is ignored --- if it
;; looks like a MIME message then it must be one.)
;;
;; Any function which calls this function *must* set case-fold-search
;; to t and must also narrow the buffer to just the headers of a
;; message.
(defun rmime-standard-headers ()
  (mapcar 'rmime-fetch-header '("Content-Type"
				"Content-Transfer-Encoding"
				"Content-Disposition"
				"Content-Description")))


;;; Routines for RFC 822 header manipulations ---


;; This function extracts one header from a message.
;;
;; Any function which calls this function *must* set case-fold-search
;; to t and must also narrow the buffer to just the headers of a
;; message.
;;
;; The string "^xxx:" matches the header we are looking for.
;;
;; The string "\\(\n*[ \t]\\)*" matches any string of whitespace not
;;             ***************
;; ending with a newline.  We don't have to worry about encountering
;; two newlines in a row because we have narrowed our buffer.  In
;; fact, if we do encounter two newlines in a row, we assume that the
;; function that called us knew what it was doing and we treat the
;; blank line as part of a header.
;;
;; The string "[^ \t\n]\\(\\(\n*[ \t]\\)*[^ \t\n]\\)*" matches any
;;                        ***************
;;                     ******************************
;; sequence of characters that doesn't start or end with whitespace.
;; At least one character is required.  Any newlines must be followed
;; by a tab or a space.
(defun rmime-fetch-header (header)
  (goto-char (point-min))
  (if (re-search-forward (concat "\\(^" (regexp-quote header) ":\\)\\(\\(\n*[ \t]\\)*\\)\\([^ \t\n]\\(\\(\n*[ \t]\\)*[^ \t\n]\\)*\\)") nil t)
      (rmime-unfold (buffer-substring (match-beginning 4) (match-end 4)))))

;; This recursive function unfolds RFC 822 header lines.
;;
;; RFC 822 doesn't say anything about whitespace at the end of a line,
;; but RFC 1521 implies that they should be blown away.
;;
;; RFC 822 says to replace the CRLF LWSP sequence with just the LWSP
;; char, but if we do this, the example in RFC 1521 won't work.  We
;; choose to replace CRLF LWSP with a space.
;;
;; The string "\\(.*[^ \t]\\)?" matches a string not ending with white
;;             ***************
;; space (including the empty string).
;;
;; The string "[ \t]*\n[ \t]*" matches a line break and any white
;; space around it.
;;
;; The string "\\(.\\|\n\\)*" matches anything (including linebreaks).
;;             *************
;;
;; Put them together and get some code to unfold RFC 822 header lines.
(defun rmime-unfold (string)
  (if (string-match "^\\(\\(.*[^ \t]\\)?\\)\\([ \t]*\n[ \t]*\\)\\(\\(.\\|\n\\)*\\)$" string)
      (concat (substring string (match-beginning 1) (match-end 1))
	      " "
	      (rmime-unfold (substring string (match-beginning 4) (match-end 4))))
    (identity string)))

;; This recursive function removes RFC 822 comments.
;;
;; Comments are text in parenthesis.
;;
;; Comments can nest.  Comments can't appear inside quotes.
;;
;; Except at the beginning or end of a line, comments are replaced
;; with a space.  This is always safe because comments can't appear in
;; an atom.  At the beginning or end of a line, they are just removed
;; (along with any whitespace around them).
;;
;; The string "\\([ \t]*\\([^\\\" \t]\\|\\\\.\\|\"\\([^\\\"]\\|\\\\.\\)*\"\\)\\)*"
;;                                                **********************
;;                      *****************************************************
;;             ******************************************************************
;; matches any string with an even number of quote marks not ending
;; with white space.  Quote marks that are backquoted don't count.  We
;; won't match a string ending with a backslash.  We will match a
;; string ending with backslash space.
;;
;; The string "[ \t]*(\\([^\\()]\\|\\\\.\\)*)[ \t]*" matches a comment
;;                    **********************
;; and its surrounding white space.
;;
;; The string "\\([^ \t].*\\)?" matches any string not beginning with
;;             ***************
;; white space.
;;
;; Put them together and get some code to remove comments from headers.
(defun rmime-uncomment (string)
  (if (and string (string-match "^\\(\\([ \t]*\\([^\\\" \t]\\|\\\\.\\|\"\\([^\\\"]\\|\\\\.\\)*\"\\)\\)*\\)\\([ \t]*(\\([^\\()]\\|\\\\.\\)*)[ \t]*\\)\\(\\([^ \t].*\\)?\\)$" string))
      (if (eq (match-beginning 1) (match-end 1))
	  (if (eq (match-beginning 7) (match-end 7))
	      (identity nil)
	    (rmime-uncomment (substring string (match-beginning 7) (match-end 7))))
	(if (eq (match-beginning 7) (match-end 7))
	    (rmime-uncomment (substring string (match-beginning 1) (match-end 1)))
	  (rmime-uncomment (concat (substring string (match-beginning 1) (match-end 1))
				   " "
				   (substring string (match-beginning 7) (match-end 7))))))
    (identity string)))

;; This recursive function removes backslashes to reveal the character
;; they protect.
;;
;; Note that this function has nothing to do with "this kind of
;; quoting".
;;
;; The string "[^\\]*" matches any string not containing a backslash.
;;
;; The string "\\\\" matches a backslash.
;;
;; The string "." matches a character following a backslash.
;;
;; The string ".*" matches anything.
;;
;; Put them together and get some code to remove backslashes.
(defun rmime-unquote (string)
  (if (and string (string-match "^\\([^\\]*\\)\\(\\\\\\)\\(.\\)\\(.*\\)$" string))
      (concat (substring string (match-beginning 1) (match-end 1))
	      (substring string (match-beginning 3) (match-end 3))
	      (rmime-unquote (substring string (match-beginning 4) (match-end 4))))
    (identity string)))


;;; Temporary glue for major modes that should know about MIME


;; Get MIME information from an RMAIL buffer.
;;
;; We can't use rmime-standard-mimeinfo because an important header
;; line may be part of rmail-ignored-headers.
(defun rmail-mode-mimeinfo ()
  (goto-char (point-min))
  (re-search-forward "\\(\\`\\|\n\\)[ \t]*\n" nil 'move)
  (let ((case-fold-search t))
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (aref rmail-message-vector rmail-current-message))
	(narrow-to-region (point) (point-max))
	(rmime-standard-mimeinfo)))))

;; Get MIME information from a *mail* buffer or *post-news* buffer.
;;
;; It is wrong to terminate the regular expression with "$" instead of
;; "\n".  Using "$" will cause the newline at the end of the separator
;; to be interpreted as the first character of the body.
(defun mail-mode-mimeinfo ()
  (prog1
      (rmime-regexp-mimeinfo (concat "^" (regexp-quote mail-header-separator) "\n"))
    (let ((window (selected-window)))
      (pop-to-buffer (concat (buffer-name) "-mime"))
      (save-excursion
	(select-window window)))))
(defalias 'news-reply-mode-mimeinfo 'mail-mode-mimeinfo)

;; Get MIME information from a +inbox buffer.
(defun mh-folder-mode-mimeinfo ()

  ;; Select the output buffer.
  (set-buffer (get-buffer-create mh-show-buffer))

  ;; Kludge!  Set a global variable to indicate where the raw data is.
  (setq rmime-source-buffer (current-buffer))

  ;; See if all the header lines are available.
  (if (or (rmime-cache)
	  (identity mh-showing-with-headers)
	  (not (or mhl-formfile mh-clean-message-header)))

      ;; Wonderful!  All the headers are present.
      (rmime-standard-mimeinfo)

    ;; Obscene!  We have to read the file again to get at the headers.
    (goto-char (point-min))
    (re-search-forward "^[ \t]*\n" nil 'move)
    (let ((filename buffer-file-name)
	  (buffer (generate-new-buffer "*mh-headers*")))
      (unwind-protect
	  (save-excursion (set-buffer buffer)
			  (insert-file-contents filename)
			  (rmime-standard-mimeinfo))
	(kill-buffer buffer)))))

;; Get MIME information from an INBOX buffer.
(defun vm-mode-mimeinfo ()

  ;; Check that vm is configured right.
  ;;
  ;; vm-preview-current-message sets the restriction before calling
  ;; vm-select-message-hook.  We are called inside a save-restriction
  ;; so we can't change our restriction.  We can't edit outside of our
  ;; restriction or else parts of the following message might show
  ;; after save-restriction restores the old restriction.
  (or (eq vm-preview-lines t)
      (error "Sorry --- you must (setq vm-preview-lines t) to use VM with RMIME"))

  ;; We can't compile the rest of this function because some of the
  ;; subroutines we need are really macros.
  (dont-compile

    ;; Don't even think of doing this for any other major mode!
    ;;
    ;; It works only because we are called inside a save-restriction.
    ;; We would rather not be called inside a save-restriction.
    ;;
    ;; The idea is to propagate information to rmime-cancel so that
    ;; canceling the formatting doesn't cause invisible header lines
    ;; to suddenly become visible.
    (narrow-to-region (vm-vheaders-of (car vm-message-pointer)) (point-max))

    (let (case-fold-search)
      (prog1
	  (save-restriction
	    (narrow-to-region (vm-headers-of (car vm-message-pointer))
			      (vm-text-of    (car vm-message-pointer)))
	    (rmime-standard-headers))
	(goto-char (vm-text-of (car vm-message-pointer)))))))


;;; Rmime is available!


(run-hooks 'rmime-load-hook)		; For user customizations.
(provide 'rmime)

;;; rmime.el ends here

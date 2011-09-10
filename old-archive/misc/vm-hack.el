;From ark1!uakari.primate.wisc.edu!brutus.cs.uiuc.edu!apple!motcsd!hpda!hpcuhb!hpsmtc1!hp-ses!hplabs!hp-sdd!ncr-sd!ncrcae!hubcap!mephisto!tut.cis.ohio-state.edu!cs.utexas.edu!sun-barr!decwrl!shelby!neon!yossi Fri Dec 15 12:42:59 1989
;Article 1070 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!brutus.cs.uiuc.edu!apple!motcsd!hpda!hpcuhb!hpsmtc1!hp-ses!hplabs!hp-sdd!ncr-sd!ncrcae!hubcap!mephisto!tut.cis.ohio-state.edu!cs.utexas.edu!sun-barr!decwrl!shelby!neon!yossi
;From yossi@Neon.Stanford.EDU (Joseph Friedman)
;Newsgroups: comp.emacs
;Subject: vm-hack.el
;Message-ID: <1989Dec7.001819.26761@Neon.Stan>
;Date: 7 Dec 89 00:18:00 GMT
;Lines: 502
;
;Hi folks!
;
;I recently discovered GNU Emacs, and became pretty addicted to it
;(some people will do anything to avoid work...)  Anyway, I took
;Johan Vromans' summary mode and hacked it up.  I also added MM-style
;mail forwarding.  The result is this chunk of e-lisp code, which I
;am posting here.  Since this is my first serious attempt at programming
;in e-lisp, I would appreciate any comments.
;
;-yossi
;(Yossi@CS.Stanford.EDU)
;
;------------------------------- Cut Here ----------------------------------

;
; A few enhancements to VM.  First off, I fixed some bugs in Johan Vromans'
; summary mode, and extended his stuff to cover %f in the format (NOTE: I
; changed the names of his variables from jv-... to vm-hack-...)
; I also added an MM-style message forwarding.
;
; Here's Johan Vromans' README, with the name changes:
;
; > Hi VM freaks,
; >
; > I've been using the following extension to VM's summary handling for
; > some time, and I like it. So I want to share it with you.
; >
; > Features:
; >
; >  - when a mail originates from me, the recipient is shown instead of
; >    the sender, e.g. "To info-vm@cs.odu.edu".
; >
; >  - when a mail was originally sent to a mailing list, the name of the
; >    mailing list is shown instead, e.g. "[info-vm]".
; >
; > A user-settable variable "vm-hack-summary-mode" determines whether this
; > is never done (value = nil), only for the primary mailbox folder
; > (value = t), or for all folders (other values).
; > The local key "%" is set up to switch between this modes: "%" will
; > show all sender names, while "C-u %" will enable the above processing.
; >
; > The user-variable "vm-hack-mailing-lists" holds a list of all mailing lists
; > I partcipate in, e.g.
; >
; > (setq vm-hack-mailing-lists 
; >       (list
; >         "perl-users"
; >         "info-vm" "bug-vm"
; >         .....))
; >
; > Happy hacking!
; > Johan
; > --					       jv@mh.nl via internet backbones
; > Johan Vromans			       uucp: ..!{uunet,hp4nl}!mh.nl!jv
; > Multihouse Automatisering bv	       phone/fax: +31 1820 62944/62500
; > Doesburgweg 7, 2803 PL Gouda, The Netherlands
; > ------------------------ "Arms are made for hugging" ---------------------
;
; The variable vm-hack-mm-forward controls the MM-style mail forwarding.
; See the documentation of the variable for more details.  I have tested
; this code on GNU Emacs v18.55, running under ULTRIX v3.1A.  Since I
; hack on sendmail, it is possible that this code will not work on your
; system.  Please let me know if it breaks.
;
; NOTE: If you expect to receive MM-style forwarded messages, you'd probably
;       want to add Resent-From, Resent-To, and Resent-Date to your
;       vm-visible-headers.
;
; You'll need to add (require 'vm-hack) to your vm-mode-hooks.
;

(provide 'vm-hack)
(load "vm-summary")
(load "vm-reply")

(defvar vm-hack-summary-mode t "\
  *Enables intelligent handling of From-addresses in VM summary list.
   Values are 'nil' (no handling), 't' (primary mailbox only) or anything
   else, which causes all mailboxes to be handled.")

(defvar vm-hack-mailing-lists nil "*List of mailing lists.")

(defvar vm-hack-mm-forward t "\
  *Controls the forwarding style of messages in VM.  Values:

    t:   MM-style forwarding.  When you use VM's \"forward\" command to
         forward a message from person A to person B, the header is
         constructed in such a way that B will think he got it from A
         directly, except that the subject is modified according to
         vm-forwarding-subject-format (if it is non-nil); the header
	 also indicates that the message is \"Resent-From\" you.
         The body of the message is left unchanged.

    nil: The old VM forwarding.")

(define-key vm-mode-map "%" 'vm-hack-summary-shownames)




























































; we need the host name
(defvar vm-hack-hostname "**JUNK**" "\
*Hostname used by vm-hack-summary for recognizing an outgoing message.")

(if (string= vm-hack-hostname "**JUNK**")
    (let (vm-hack-hostname-process)
      (setq vm-hack-hostname-process
	    (start-process "host-name" nil
			   "sh" "-c" "cat > /dev/null; hostname"))
      (set-process-filter vm-hack-hostname-process
			  '(lambda (process string)
			     (setq vm-hack-hostname string)))
      (process-send-eof vm-hack-hostname-process)
      (while (string= vm-hack-hostname "**JUNK**") (sleep-for 1))
      (setq vm-hack-hostname (substring vm-hack-hostname
					0 (string-match "$"
							vm-hack-hostname)))))


(defun vm-hack-subs-mailing-list (hdr)
  "Return name of mailing list, if found in HDR."
  (let ((ll vm-hack-mailing-lists) (case-fold-search t) el res)
    (while (not (null ll))
      (setq el (car-safe ll))
      (setq ll (cdr-safe ll))
      (if (string-match (regexp-quote el) hdr)
	  (progn
	    (setq ll nil)		; terminate loop
	    (setq res (concat "[" el "]")))))
    res))
      
(defun vm-su-full-name (m)
  ;; modified version of vm's vm-su-full-name.
  ;; Depending on the value of vm-hack-summary-mode, some post-processing
  ;; is done on the name displayed.
  ;;
  ;; If the mail is from/to a mailing list, the name of the list is
  ;; displayed.
  ;; If the mail originates from me, the recipient is shown.
  ;;
  (let (temp temp-full-name)
    (setq temp-full-name
	  (or (vm-full-name-of m)
	      (progn (vm-su-do-author m) (vm-full-name-of m))))
    (cond
     ;; return what we have if no postprocessing selected
     ((null vm-hack-summary-mode)
      temp-full-name)

     ;; idem, if only the primary mailbox must be handled
     ((and (eq vm-hack-summary-mode 't)
	   (not vm-primary-inbox-p))
      temp-full-name)

     ;; first, try mailing lists
     ((vm-hack-subs-mailing-list
       (concat (vm-get-header-contents m "To") " "
	       (vm-get-header-contents m "Cc"))))

     ;; if not, maybe recipient?
     ((and (or
	      (equal temp-full-name (user-login-name))
	      (equal temp-full-name (user-full-name)))
	     (setq temp (vm-su-do-recipient-address m)))
      (concat "To " temp))

     ;; nope - return the full name
     (t temp-full-name))))
      
(defun vm-su-from (m)
  ;; modified version of vm's vm-su-from
  (let (temp temp-from)
    (setq temp-from
	  (or (vm-from-of m)
	      (progn (vm-su-do-author m) (vm-from-of m))))
    (cond
     ;; return what we have if no postprocessing selected
     ((null vm-hack-summary-mode)
      temp-from)

     ;; idem, if only the primary mailbox must be handled
     ((and (eq vm-hack-summary-mode 't)
	   (not vm-primary-inbox-p))
      temp-from)

     ;; first, try mailing lists
     ((vm-hack-subs-mailing-list
       (concat (vm-get-header-contents m "To") " "
	       (vm-get-header-contents m "Cc"))))

     ;; if not, maybe recipient?
     ((and (or
	      (equal temp-from (user-login-name))
	      (equal temp-from (concat (user-login-name)
				       "@" vm-hack-hostname)))
	   (setq temp (vm-su-do-recipient-address m)))
      (concat "To " temp))

     ;; nope - return the from
     (t temp-from))))

(defun vm-su-do-recipient-address (m)
  (let (to)
    (setq to (or (vm-get-header-contents m "To")
		 (vm-get-header-contents m "Apparently-To")))
    (cond ((null to)
	   (setq to "???"))
	  ((string-match "^\\(\\([^<,]+[^ \t\n]\\)[ \t\n]+\\)?<\\([^>]+\\)>"
			 to)
	   (setq to (substring to (match-beginning 3) (match-end 3))))
	  ((string-match "[^,]*(\\([^),]+\\))[^,]*" to)
	   (setq to
		 (concat
		  (substring to (match-beginning 0) (1- (match-beginning 1)))
		  (substring to (1+ (match-end 1)) (match-end 0))))))
    ;; ewe ewe see pee...
    (if (and vm-gargle-uucp (string-match
"\\([^!@:.]+\\)\\(\\.[^!@:]+\\)?!\\([^!@: \t\n]+\\)\\(@\\([^!@:. \t\n]+\\)\\(.[^ \t\n]+\\)?\\)?[ \t\n]*$"
			     to))
	(setq to
	      (concat
	       (substring to (match-beginning 3) (match-end 3)) "@"
	       (if (and (match-beginning 5) (match-beginning 2)
			(not (match-beginning 6)))
		   (concat (substring to (match-beginning 5) (match-end 5))
			   ".")
		 "")
	       (substring to (match-beginning 1)
			  (or (match-end 2) (match-end 1)))
	       (if (match-end 2) "" ".UUCP"))))
    to))

(defun vm-hack-summary-shownames (&optional dont)
  "Show full names in summary instead of mailing lists etc., or the
other way around if prefix arg is supplied."
  (interactive "P")
  (let ((vm-hack-summary-mode dont))
    (vm-summarize nil)))




























































;
; A slight change to vm-do-reply.  Need to turn off vm-hack-summary-mode
; when figuring out the vm-included-text-attribution-format, since some
; people out there may reply to themselves.
;
(defun vm-do-reply (to-all include-text)
  (vm-follow-summary-cursor)
  (if vm-mail-buffer
      (set-buffer vm-mail-buffer))
  (vm-error-if-folder-empty)
  (save-restriction
    (widen)
    (let ((mail-buffer (current-buffer))
	  (text-start (vm-text-of (car vm-message-pointer)))
	  (text-end (vm-text-end-of (car vm-message-pointer)))
	  (mp vm-message-pointer)
	  to cc subject message-id tmp)
      (cond ((setq to (vm-get-header-contents (car mp) "Reply-To")))
	    ((setq to (vm-get-header-contents (car mp) "From")))
	    ((setq to (vm-grok-From_-author (car mp))))
	    (t (error "Cannot find a From: or Reply-To: header in message")))
      (setq subject (vm-get-header-contents (car mp) "Subject")
	    message-id (and vm-in-reply-to-format
			    (vm-sprintf 'vm-in-reply-to-format (car mp))))
      (if to-all
	  (progn
	    (setq cc (vm-get-header-contents (car mp) "To"))
	    (setq tmp (vm-get-header-contents (car mp) "Cc"))
	    (if tmp
		(if cc
		    (setq cc (concat cc ",\n\t" tmp))
		  (setq cc tmp)))))
      (if vm-strip-reply-headers
	  (let ((mail-use-rfc822 t))
	    (require 'mail-utils)
	    (and to (setq to (mail-strip-quoted-names to)))
	    (and cc (setq cc (mail-strip-quoted-names cc)))))
      (if (mail nil to subject message-id cc)
	  (progn
	    (use-local-map (copy-keymap (current-local-map)))
	    (local-set-key "\C-c\C-y" 'vm-yank-message)
	    (local-set-key "\C-c\C-s" 'vm-mail-send)
	    (local-set-key "\C-c\C-c" 'vm-mail-send-and-exit)
	    (local-set-key "\C-c\C-v" vm-mode-map)
	    (setq vm-mail-buffer mail-buffer
		  vm-message-pointer mp)
	    (cond (include-text
		   (goto-char (point-max))
		   (insert-buffer-substring mail-buffer text-start text-end)
		   (goto-char (- (point) (- text-end text-start)))
		   (save-excursion
		     (if vm-included-text-attribution-format
			 (let ((vm-hack-summary-mode nil))
			   (insert (vm-sprintf
				    'vm-included-text-attribution-format
				    (car mp)))))
		     (while (and (re-search-forward "^" nil t) (not (eobp)))
		       (replace-match vm-included-text-prefix t t))))))))))

;
; A modified vm-forward-message.
;
(defun vm-forward-message ()
  "Forward the current message to one or more third parties.
You will be placed in a *mail* buffer as is usual with replies, but you
must fill in the To: or Resent-To: header manually." 
  (interactive)
  (vm-follow-summary-cursor)
  (if vm-mail-buffer
      (set-buffer vm-mail-buffer))
  (vm-error-if-folder-empty)
  (let ((b (current-buffer)) (m (car vm-message-pointer)) start)
    (save-restriction
      (widen)
      (if vm-hack-mm-forward

	  ; use MM-style forwarding
	  (cond ((mail nil nil nil)
		 (use-local-map (copy-keymap (current-local-map)))
		 (local-set-key "\C-c\C-y" 'vm-yank-message)
		 (local-set-key "\C-c\C-v" vm-mode-map)
		 (setq vm-mail-buffer b)

		 ; reconstruct the whole thing
		 (erase-buffer)

	         ; put header of old message
		 (insert-buffer-substring
		   b
		   (save-excursion
		     (set-buffer b)
		     (goto-char (vm-start-of m))
		     (forward-line 1)
		     (point))
		   (save-excursion
		     (set-buffer b)
		     (goto-char (vm-text-of m))
		     (forward-line -1)
		     (point)))
		 (set-window-start (get-buffer-window (current-buffer))
				   (point))

	         ; if diff't subject format, reformat subject line
		 (and
		   vm-forwarding-subject-format
		   (goto-char (point-min))
		   (re-search-forward
		     "^Subject:[ \t]+\\([^\n]*\n\\([ \t][^\n]*\n\\)*\\)"
		     (point-max) t)
		   (progn
		     (delete-region (match-beginning 0) (match-end 0))
		     (goto-char (point-max))
		     (insert "Subject: "
			     (save-excursion
			       (set-buffer b)
			       (vm-sprintf 'vm-forwarding-subject-format m)))
		     (insert "\n")))

	         ; add new header fields
		 (goto-char (point-max))
		 (insert "Resent-From: " (user-login-name) "\n")
		 (insert "Resent-To: ")
		 (save-excursion
		   (insert "\n")
		   (insert mail-header-separator "\n")
	     
	           ; include original message
		   (insert-buffer-substring b (goto-char (vm-text-of m))
					    (vm-end-of m)))))

        ; nope. use old VM forwarding
	(cond ((mail nil nil
		     (and vm-forwarding-subject-format
			  (vm-sprintf 'vm-forwarding-subject-format m)))
	       (use-local-map (copy-keymap (current-local-map)))
	       (local-set-key "\C-c\C-y" 'vm-yank-message)
	       (local-set-key "\C-c\C-v" vm-mode-map)
	       (setq vm-mail-buffer b)
	       (goto-char (point-max))
	       (insert "------- Start of forwarded message -------\n")
	       (setq start (point))
	       (insert-buffer-substring b
					(save-excursion
					  (set-buffer b)
					  (goto-char (vm-start-of m))
					  (forward-line 1)
					  (point))
					(vm-text-end-of m))
	       (if vm-rfc934-forwarding
		   (vm-rfc934-char-stuff-region start (point)))
	       (insert "------- End of forwarded message -------\n")
	       (goto-char (point-min))
	       (end-of-line)))))))




























































;
; A hacked-up version of sendmail-send-it that understands my forwarding
; mechanism
;

(defun vm-hack-sendmail-send-it ()
  (let ((errbuf (if mail-interactive
		    (generate-new-buffer " sendmail errors")
		  0))
	(tembuf (generate-new-buffer " sendmail temp"))
	(case-fold-search nil)
	(resent nil)
	delimline
	(mailbuf (current-buffer)))
    (unwind-protect
	(save-excursion
	  (set-buffer tembuf)
	  (erase-buffer)
	  (insert-buffer-substring mailbuf)
	  (goto-char (point-max))
	  ;; require one newline at the end.
	  (or (= (preceding-char) ?\n)
	      (insert ?\n))
	  ;; Change header-delimiter to be what sendmail expects.
	  (goto-char (point-min))
	  (re-search-forward
	    (concat "^" (regexp-quote mail-header-separator) "\n"))
	  (replace-match "\n")
	  (backward-char 2)
	  (setq delimline (point-marker))
	  (if mail-aliases
	      (expand-mail-aliases (point-min) delimline))
	  (goto-char (point-min))
	  ;; ignore any blank lines in the header
	  (while (and (re-search-forward "\n\n\n*" delimline t)
		      (< (point) delimline))
	    (replace-match "\n"))
	  (let ((case-fold-search t))
	    ;; Find and handle any FCC fields.
	    (goto-char (point-min))
	    (if (re-search-forward "^FCC:" delimline t)
		(mail-do-fcc delimline))
	    ;; If there is a From and no Sender, put it a Sender.
	    (goto-char (point-min))
	    (and (re-search-forward "^From:"  delimline t)
		 (not (save-excursion
			(goto-char (point-min))
			(re-search-forward "^Sender:" delimline t)))
		 (progn
		   (forward-line 1)
		   (insert "Sender: " (user-login-name) "\n")))
	    ;; don't send out a blank subject line
	    (goto-char (point-min))
	    (if (re-search-forward "^Subject:[ \t]*\n" delimline t)
		(replace-match ""))

	    ;; if resent message, find out the real recipient
	    (save-excursion
	      (goto-char delimline)
	      (if (re-search-backward
		   "^Resent-To:[ \t]*\\([^\n]+\n\\(^[ \t]+[^ \t][^\n]*\n\\)*\\)"
		   (point-min) t)
		  (let ((s (buffer-substring (match-beginning 1) (match-end 1))))
		    (while (not (string= s ""))
		      (string-match "[^ ,\n]+" s)
		      (setq resent
			    (append resent
				    (list
				      (substring
				        s (match-beginning 0) (match-end 0)))))
		      (setq s (substring s (match-end 0) (length s)))
		      (and
		        (string-match "[, \t\n][ \t\n]*" s)
			(setq s (substring s (match-end 0) (length s))))))))

	    (if mail-interactive
		(save-excursion
		  (set-buffer errbuf)
		  (erase-buffer))))
	  (apply 'call-process-region
		 (append (list (point-min) (point-max)
			       (if (boundp 'sendmail-program)
				   sendmail-program
				 "/usr/lib/sendmail")
			       nil errbuf nil
			       "-oi")
			 (if (null resent)
			     (list "-t")
			     resent)
			 ;; Always specify who from,
			 ;; since some systems have broken sendmails.
			 (list "-f" (user-login-name))
;;;			 ;; Don't say "from root" if running under su.
;;;			 (and (equal (user-real-login-name) "root")
;;;			      (list "-f" (user-login-name)))
			 ;; These mean "report errors by mail"
			 ;; and "deliver in background".
			 (if (null mail-interactive) '("-oem" "-odb"))))
	  (if mail-interactive
	      (save-excursion
		(set-buffer errbuf)
		(goto-char (point-min))
		(while (re-search-forward "\n\n* *" nil t)
		  (replace-match "; "))
		(if (not (zerop (buffer-size)))
		    (error "Sending...failed to %s"
			   (buffer-substring (point-min) (point-max)))))))
      (kill-buffer tembuf)
      (if (bufferp errbuf)
	  (kill-buffer errbuf)))))

(setq send-mail-function 'vm-hack-sendmail-send-it)



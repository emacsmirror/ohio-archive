;From arpa-unix-emacs-request@CHIPS.BBN.COM Fri Mar 31 09:21:50 1989
;Received: from chips by CHIPS.BBN.COM id aa02643; 31 Mar 89 9:02 EST
;Received: from BBN.COM by CHIPS.BBN.COM id aa02637; 31 Mar 89 9:00 EST
;Received: from USENET by bbn.com with netnews
;	for arpa-unix-emacs@bbn.com (unix-emacs@bbn.com);
;	contact usenet@bbn.com if you have questions.
;To: unix-emacs@bbn.com
;Date: 30 Mar 89 11:55:00 GMT
;From: mailrus!uflorida!novavax!hcx1!hcx2!tom@bbn.com
;Sender: arpa-unix-emacs-request@bbn.com
;Subject: Re: looking for feature
;Message-Id: <94600021@hcx2>
;References: <89@<WSD>
;Source-Info:  From (or Sender) name not authenticated.
;Status: RO
;
;
;This is a package that a friend of mine (Bill Leonard) here at
;Harris wrote. I am posting it instead of him merely because I get
;in earlier in the morning...
;
;We have gotten to where we use this technique for several of the
;packages we have written where something prompts for input, each
;command keeps its own history list so you can quickly cycle through
;the previous input to just that command.
;
;It is very handy to rebind the keys where next line is, so you can
;continue to use any cursor keys.
;
;I have the following lines in my .emacs to use this:
;
;(autoload 'select-another-buffer "select-buf" nil t)
;(define-key global-map "\C-x\C-b" 'select-another-buffer) ;; redef default
;
;
;---cut here------------------------select-buf.el------------------------------
;; This file provides an interactive buffer-list capability.
;; When the function select-another-buffer is invoked, the minibuffer
;; prompts you for another buffer to select.  The default is the second
;; buffer on the buffer-list.  Also, all the keys that are normally
;; bound to next-line and previous-line are bound to functions that
;; navigate through the buffer list.  Any keys bound to kill-buffer
;; are rebound to a function that will kill the buffer currently
;; named in the minibuffer, then move to the next buffer on the list.
;; This is a faster means of selecting another buffer than buffer-menu
;; is, but with most of the power.

(defvar buffer-select-list-index 0 "Index into buffer-list")

(defvar buffer-select-local-list nil "Local copy of buffer-list")

(defvar buffer-select-minibuffer-map (copy-keymap minibuffer-local-map)
"This is a copy of the minibuffer keymap with all the keys that
were bound to next-line now bound to buffer-select-next and all the keys
that were bound to previous-line now bound to buffer-select-prev.")

(mapcar
   (function 
      (lambda (keyseq)
         (define-key buffer-select-minibuffer-map keyseq 'buffer-select-prev)
      )
   )
   (where-is-internal 'previous-line nil nil)
)

(mapcar
   (function 
      (lambda (keyseq)
         (define-key buffer-select-minibuffer-map keyseq 'buffer-select-next)
      )
   )
   (where-is-internal 'next-line nil nil)
)

(mapcar
   (function 
      (lambda (keyseq)
         (define-key buffer-select-minibuffer-map keyseq 'buffer-select-killit)
      )
   )
   (where-is-internal 'kill-buffer nil nil)
)

(defun make-buffer-list (buffl)
"Constructs a list of buffers from BUFFL excluding all the buffers whose
names begin with space."
   (if buffl
      (if (equal (substring (buffer-name (car buffl)) 0 1) " ")
         (make-buffer-list (cdr buffl))
         (cons (car buffl) (make-buffer-list (cdr buffl)))
      )
      buffl
   )
)

(defun select-another-buffer ()
"Select another buffer to display in the current window.  The minibuffer
is used to prompt for the buffer name.  The default is the second buffer
on the buffer-list; other buffers can be selected either explicitly, or
by using buffer-select-next and buffer-select-prev.  Keys normally bound
to next-line are bound to buffer-select-next and keys normally bound to
previous-line are bound to buffer-select-prev."
   (interactive)
   (let
      (
         (save-minibuffer-map minibuffer-local-map)
         inpt
      )
      (setq minibuffer-local-map buffer-select-minibuffer-map)
      (setq buffer-select-list-index 1)
      (setq buffer-select-local-list (make-buffer-list (buffer-list)))
      (setq inpt
            (unwind-protect
               (progn
                  (read-input "Select another buffer: "
                              (buffer-name (car (cdr buffer-select-local-list))))
               )
               (progn
                  (setq minibuffer-local-map save-minibuffer-map)
               )
            )
      )
      (switch-to-buffer inpt)
   )
)

(defun buffer-select-next ()
"Move to the next buffer on the buffer-list."
   (interactive)
   (erase-buffer)
   (setq buffer-select-list-index (1+ buffer-select-list-index))
   (if (>= buffer-select-list-index (length buffer-select-local-list))
       (setq buffer-select-list-index 0)
   )
   (insert (buffer-name (nth buffer-select-list-index buffer-select-local-list)))
)

(defun buffer-select-prev ()
"Move to the previous buffer on the buffer-list."
   (interactive)
   (erase-buffer)
   (setq buffer-select-list-index (1- buffer-select-list-index))
   (if (< buffer-select-list-index 0)
       (setq buffer-select-list-index (1- (length buffer-select-local-list)))
   )
   (insert (buffer-name
              (nth buffer-select-list-index buffer-select-local-list)))
)

(defun buffer-select-killit ()
"Kill the buffer currently appearing in the minibuffer, then move to
the next buffer on the buffer-list."
   (interactive)
   (let
      (
         (mbuf (current-buffer))        ;; Save the minibuffer because
                                        ;; kill-buffer selects a buffer
         (kbuf (nth buffer-select-list-index buffer-select-local-list))
      )
      (message "Killing buffer %s." (buffer-name kbuf))
      (kill-buffer kbuf)
      (set-buffer mbuf)
   )
   ;; Rebuild the buffer list, so that the killed buffer doesn't appear
   ;; in it.  Under certain circumstances, the buffer might not have
   ;; gone away, such as killing "*scratch*" when it is the last buffer.
   
   (setq buffer-select-local-list (make-buffer-list (buffer-list)))
   
   ;; Fix buffer-select-list-index, in case it went off the end of
   ;; the list (in either direction, just to be absolutely safe).

   (if (< buffer-select-list-index 0)
       (setq buffer-select-list-index (1- (length buffer-select-local-list)))
   )
   (if (>= buffer-select-list-index (length buffer-select-local-list))
       (setq buffer-select-list-index 0)
   )
   (erase-buffer)
   (insert (buffer-name
              (nth buffer-select-list-index buffer-select-local-list)))
)
----------------------cut here----------------------------------------------
=====================================================================
    usenet: tahorsley@ssd.harris.com  USMail: Tom Horsley
compuserve: 76505,364                         511 Kingbird Circle
     genie: T.HORSLEY                         Delray Beach, FL  33444
======================== Aging: Just say no! ========================


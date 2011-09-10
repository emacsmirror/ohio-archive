;To: unix-emacs@bbn.com
;Date: 29 Jan 89 02:22:12 GMT
;From: Julian Cowley <orion.cf.uci.edu!ucsd!nosc!humu!uhccux!julian@usc-oberon.usc.edu>
;Subject: Minor mode for viewing buffers
;
;[Oops, my previous attempt at posting this had an error in it.  Please
; pardon.]
;
;Since there currently seems to be a flourish of sources in this
;newsgroup, here is a little code for viewing buffers.  Basically, it
;is a simple emulation of `more', and is set up as a minor mode so that
;it can be toggled like other minor modes such as overwrite-mode.  I
;find it best when it is placed on a global keystroke which is normally
;undefined, such as this:
;
;(global-set-key "\M-#" 'view-buffer-mode)
;
;Have fun, and enjoy it.
;
;julian@uhccux.uhcc.hawaii.edu
;uunet!ucsd!nosc!uhccux!julian
;julian@uhccux.bitnet
;"People who aren't amused don't talk."
;
;<----------------------------- cut here ----------------------------->
;; Define a simple viewing minor mode for the current buffer
;; Written by Julian Cowley <julian@uhccux.uhcc.hawaii.edu> Jan. 89
;;
;; The standard GNU copying restrictions apply here.

(defvar view-buffer-mode-map nil
  "The keymap for View buffer mode.")

(defvar old-local-map nil
  "Temporary storage for the buffer's local map while View buffer
mode is in effect.")

(defvar old-buffer-read-only nil
  "Temporary storage for the buffer's buffer-read-only variable
while View buffer mode is in effect.")

(make-variable-buffer-local 'viewing-buffer)
(make-variable-buffer-local 'old-local-map)
(make-variable-buffer-local 'old-buffer-read-only)
(or (assq 'viewing-buffer minor-mode-alist)
    (setq minor-mode-alist (append minor-mode-alist
				   '((viewing-buffer " View")))))

(if view-buffer-mode-map
    ()
  (setq view-buffer-mode-map (make-sparse-keymap))
  (define-key view-buffer-mode-map " " 'scroll-up)
  (define-key view-buffer-mode-map "\r" 'scroll-up-line)
  (define-key view-buffer-mode-map "\C-d" 'scroll-up-half-screen)
  (define-key view-buffer-mode-map "\C-?" 'scroll-down)
  (define-key view-buffer-mode-map "." 'beginning-of-buffer)
  (define-key view-buffer-mode-map "e" 'end-of-buffer)
  (define-key view-buffer-mode-map "<" 'beginning-of-buffer)
  (define-key view-buffer-mode-map ">" 'end-of-buffer)
  (define-key view-buffer-mode-map "g" 'goto-line)
  (define-key view-buffer-mode-map "=" 'what-line)
  (define-key view-buffer-mode-map "\C-hm" 'describe-view-buffer-mode))

(defun view-buffer-mode ()
  "Minor mode to view buffers.  Toggles on each call.

The bindings are:\\{view-buffer-mode-map}"
  (interactive)
  (if viewing-buffer
      ;; if we are currently viewing a buffer, restore the
      ;; original buffer
      (progn
	(use-local-map old-local-map)
	(setq buffer-read-only old-buffer-read-only
	      viewing-buffer nil))
    ;; save the local map and the state of buffer-read-only
    (setq old-local-map (current-local-map))
    (use-local-map view-buffer-mode-map)
    (setq old-buffer-read-only buffer-read-only
	  buffer-read-only t
	  viewing-buffer t))
  ;; update the mode line
  (set-buffer-modified-p (buffer-modified-p)))

(defun describe-view-buffer-mode ()
  "Display documentation of View buffer mode."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ "View Mode:\n")
    (princ (documentation 'view-buffer-mode))
    (print-help-return-message)))

(defun scroll-up-line ()
  (interactive)
  (scroll-up 1))

(defun scroll-up-half-screen ()
  (interactive)
  (scroll-up (/ (screen-height) 2)))


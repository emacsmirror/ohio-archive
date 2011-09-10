
;;;### (autoloads (cvs-edit) "cvs-edit" "cvs-edit.el" (14530 53861))
;;; Generated autoloads from cvs-edit.el

(autoload (quote cvs-edit) "cvs-edit" "\
Setup a buffer to enter a log message.
The buffer will be put in `cvs-edit-mode'.
If SETUP is non-nil, the buffer is then erased and `cvs-edit-hook' is run.
Mark and point will be set around the entire contents of the
buffer so that it is easy to kill the contents of the buffer with \\[kill-region].
Once you're done editing the message, pressing \\[cvs-edit-done] will call
`cvs-edit-done' which will end up calling CALLBACK to do the actual commit." nil nil)

;;;***

;;;### (autoloads nil "cvs-log" "cvs-log.el" (14528 10081))
;;; Generated autoloads from cvs-log.el

(autoload (quote cvs-log-mode) "cvs-log" "\
Major mode for browsing CVS log output." t)

;;;***

;;;### (autoloads nil "cvs-status" "cvs-status.el" (14530 53861))
;;; Generated autoloads from cvs-status.el

(autoload (quote cvs-status-mode) "cvs-status" "\
Mode used for cvs status output." t)

;;;***

;;;### (autoloads (cvs-dired-use-hook cvs-dired-action cvs-status
;;;;;;  cvs-update cvs-examine cvs-checkout) "pcl-cvs" "pcl-cvs.el"
;;;;;;  (14584 37084))
;;; Generated autoloads from pcl-cvs.el

(autoload (quote cvs-checkout) "pcl-cvs" "\
Run a 'cvs checkout MODULES' in DIR.
Feed the output to a *cvs* buffer, display it in the current window,
and run `cvs-mode' on it.

With a prefix argument, prompt for cvs FLAGS to use." t nil)

(autoload (quote cvs-examine) "pcl-cvs" "\
Run a `cvs -n update' in the specified DIRECTORY.
That is, check what needs to be done, but don't change the disc.
Feed the output to a *cvs* buffer and run `cvs-mode' on it.
With a prefix argument, prompt for a directory and cvs FLAGS to use.
A prefix arg >8 (ex: \\[universal-argument] \\[universal-argument]),
  prevents reuse of an existing *cvs* buffer.
Optional argument NOSHOW if non-nil means not to display the buffer." t nil)

(autoload (quote cvs-update) "pcl-cvs" "\
Run a `cvs update' in the current working DIRECTORY.
Feed the output to a *cvs* buffer and run `cvs-mode' on it.
With a prefix argument, prompt for a directory and cvs FLAGS to use.
A prefix arg >8 (ex: \\[universal-argument] \\[universal-argument]),
  prevents reuse of an existing *cvs* buffer." t nil)

(autoload (quote cvs-status) "pcl-cvs" "\
Run a `cvs status' in the current working DIRECTORY.
Feed the output to a *cvs* buffer and run `cvs-mode' on it.
With a prefix argument, prompt for a directory and cvs FLAGS to use.
A prefix arg >8 (ex: \\[universal-argument] \\[universal-argument]),
  prevents reuse of an existing *cvs* buffer.
Optional argument NOSHOW if non-nil means not to display the buffer." t nil)

(add-to-list (quote completion-ignored-extensions) "CVS/")

(defvar cvs-dired-action (quote cvs-examine) "\
The action to be performed when opening a CVS directory.
Sensible values are `cvs-examine' and `cvs-status'.")

(defvar cvs-dired-use-hook (quote (4)) "\
Whether or not opening a CVS directory should run PCL-CVS.
NIL means never do it.
ALWAYS means to always do it unless a prefix argument is given to the
  command that prompted the opening of the directory.
Anything else means to do it only if the prefix arg is equal to this value.")

(defadvice dired-noselect (around pcl-cvs-dired activate) (let* ((arg (ad-get-arg 0)) (dir (and (stringp arg) (directory-file-name arg)))) (if (and dir (string= "CVS" (file-name-nondirectory dir)) (file-readable-p (expand-file-name "Entries" dir)) cvs-dired-use-hook (if (eq cvs-dired-use-hook (quote always)) (not current-prefix-arg) (equal current-prefix-arg cvs-dired-use-hook))) (save-excursion (setq ad-return-value (funcall cvs-dired-action (file-name-directory dir) t t))) ad-do-it)))

;;;***

;;;### (autoloads nil "pcl-cvs-defs" "pcl-cvs-defs.el" (14528 10081))
;;; Generated autoloads from pcl-cvs-defs.el

(if (progn (condition-case nil (require (quote easymenu)) (error nil)) (fboundp (quote easy-menu-add-item))) (easy-menu-add-item nil (quote ("tools")) (quote ("PCL CVS" ["Update Directory" cvs-update t] ["Examine Directory" cvs-examine t] ["Status Directory" cvs-status t] ["Checkout Module" cvs-checkout t])) "vc"))

;;;***


;; LCD Archive Entry:
;; buffer-exists|Terrence Brannon|brannon@jove.cs.caltech.edu
;; |invoke to check if a buffer is currently in emacs memory before creating
;; |92-03-08|0.0|~/functions/buffer-exists.el.Z|

;; sample usage: (if be:buffer-exists("datafile")
;;	             ; dont create
;;		   ; else get-buffer-create)


(defvar be:verbose-mode t
  "A value other than nil means tell the result of the search for the
buffer and return t or nil. A value other than t means simply return t or nil")

(defun buf-eq-test(be:current-buf)
  (interactive)
  (string-equal be:buffer-we-are-looking-for be:current-buf))

(defun be:buffer-exists(be:bufname)
  (interactive)
  (setq be:buffer-we-are-looking-for be:bufname)
  (if (memq t (mapcar 'buf-eq-test (mapcar 'buffer-name (buffer-list))))
      (if be:verbose-mode
	(progn 
	  (message (format be:buffer-we-are-looking-for " was found"))
	  t)
	t)
    (if be:verbose-mode
	(progn 
	  (message (format be:buffer-we-are-looking-for " was not found"))
	  nil)
      nil)))


    


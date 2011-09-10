;;;; X mouse functions for Common Lisp mode.
;;;; Russell Ritchie, Scottish HCI Centre, <russell@uk.ac.strath.hci>.
;;;; Wed Jun 21 15:52:38 1989 

(require 'x-mouse)
;;(require 'clman) ; clman is not "#'provide"d...
(provide 'x-cl-fns)

(autoload 'fi::clman-backward-copy-sexp-as-kill "fi/clman") ; Near require.

(defvar x-mouse-cl-symbol-prompt-p nil
  "*If non-nil the minibuffer should be used to prompt for a Common Lisp symbol
when none can be found near point in the buffer the mouse is in.")

(defun x-mouse-clman (arg)
  "Run fi:clman on the symbol the mouse is pointing at."
  (eval-in-window
    x-mouse-window
    (x-mouse-set-point arg)
    (fi:clman (get-default-common-lisp-symbol))))

(defun get-default-common-lisp-symbol (&optional prompt up-p)
  (let* ((symbol-at-point
	  (condition-case ()
	      (save-excursion
		(if up-p
		    (progn
		      (if (= (following-char) ?\() (forward-char 1))
		      (if (= (preceding-char) ?\)) (forward-char -1))
		      (up-list -1)
		      (forward-char 1)))
		(while (looking-at "\\sw\\|\\s_")
		  (forward-char 1))
		(if (re-search-backward "\\sw\\|\\s_" nil t)
		    (progn (forward-char 1)
			   (buffer-substring
			    (point)
			    (progn (forward-sexp -1)
				   (while (looking-at "\\s'")
				     (forward-char 1))
				   (point))))
		  nil))
	    (error nil)))
	 (read-symbol
	  (or symbol-at-point
	      (if x-mouse-cl-symbol-prompt-p
		  (read-string (format "%s: " (or prompt "Which symbol: ")))
		(error "Can't find a Common Lisp symbol here."))))
	 (symbol (if (string= read-symbol "")
		     symbol-at-point
		   read-symbol))
	 (colonp (string-match ":?:" symbol nil)))
    (if (and (not colonp) fi:package)
	(setq symbol (format "%s::%s" fi:package symbol)))
    symbol))

(defun x-mouse-describe (arg)
  "Run fi:lisp-describe on the symbol the mouse is pointing at."
  (eval-in-window
    x-mouse-window
    (x-mouse-set-point arg)
    (fi:lisp-describe (get-default-common-lisp-symbol))))

(defun x-mouse-arglist (arg)
  "Run fi:lisp-arglist on the symbol the mouse is pointing at."  
  (eval-in-window
    x-mouse-window
    (x-mouse-set-point arg)
    (fi:lisp-arglist (get-default-common-lisp-symbol))))
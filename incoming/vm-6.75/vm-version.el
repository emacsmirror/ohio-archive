(provide 'vm)

(provide 'vm-version)

(defconst vm-version "6.75"
  "Version number of VM.")

(defun vm-version ()
  "Returns the value of the variable vm-version."
  vm-version)

(defconst vm-xemacs-p nil)
(defconst vm-xemacs-mule-p nil)
(defconst vm-fsfemacs-p nil)
(defconst vm-fsfemacs-mule-p nil)
(defun vm-xemacs-p () vm-xemacs-p)
(defun vm-xemacs-mule-p () vm-xemacs-mule-p)
(defun vm-fsfemacs-p () vm-fsfemacs-p)
(defun vm-fsfemacs-mule-p () vm-fsfemacs-mule-p)
(defun vm-note-emacs-version ()
  (setq vm-xemacs-p (string-match "XEmacs" emacs-version)
	vm-xemacs-mule-p (and vm-xemacs-p (featurep 'mule)
			      ;; paranoia
			      (fboundp 'set-buffer-file-coding-system))
	vm-fsfemacs-p (not vm-xemacs-p)
	vm-fsfemacs-mule-p (and (not vm-xemacs-mule-p) (featurep 'mule)
				(fboundp 'set-buffer-file-coding-system))))
(vm-note-emacs-version)

(defun vm-mouse-fsfemacs-mouse-p ()
  (and vm-fsfemacs-p
       (fboundp 'set-mouse-position)))

(defun vm-mouse-xemacs-mouse-p ()
  (and vm-xemacs-p
       (fboundp 'set-mouse-position)))

(defun vm-menu-fsfemacs-menus-p ()
  (and vm-fsfemacs-p
       (fboundp 'menu-bar-mode)))

(defun vm-menu-xemacs-menus-p ()
  (and vm-xemacs-p
       (fboundp 'set-buffer-menubar)))

(defun vm-multiple-frames-possible-p () 
  (cond (vm-xemacs-p 
	 (or (memq 'win (device-matching-specifier-tag-list))
	     (featurep 'tty-frames)))
        (vm-fsfemacs-p 
         (fboundp 'make-frame))))
 
(defun vm-mouse-support-possible-p () 
  (cond (vm-xemacs-p 
         (featurep 'window-system)) 
        (vm-fsfemacs-p 
         (fboundp 'track-mouse))))
 
(defun vm-mouse-support-possible-here-p ()
  (cond (vm-xemacs-p
	 (memq 'win (device-matching-specifier-tag-list)))
	(vm-fsfemacs-p
	 (memq window-system '(x w32 win32)))))

(defun vm-menu-support-possible-p ()
  (cond (vm-xemacs-p
	 (featurep 'menubar))
	(vm-fsfemacs-p
	 (fboundp 'menu-bar-mode))))
 
(defun vm-toolbar-support-possible-p ()
  (and vm-xemacs-p (featurep 'toolbar)))

(defun vm-multiple-fonts-possible-p ()
  (cond (vm-xemacs-p
	 (memq (device-type) '(x mswindows)))
	(vm-fsfemacs-p
	 (memq window-system '(x w32 win32)))))

(defun vm-images-possible-here-p ()
  (and vm-xemacs-p (memq (device-type) '(x mswindows))))


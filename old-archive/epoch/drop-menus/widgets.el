;;; --------
;;; handlers
;;; --------
;;; last modified:  blk@mitre.org   Tue Jan 22 12:48:07 1991
;;; --------

;;Author: Brian L. Kahn
;;Not for sale or resale, distribution unlimited


(require 'event)
(require 'property)
(provide 'widgets)


;;; from sun-mouse.el

(defmacro eval-in-window (window &rest forms)
  "Switch to WINDOW, evaluate FORMS, return to original window."
  (` (let ((OriginallySelectedWindow (selected-window)))
       (unwind-protect
	   (progn
	     (select-window (, window))
	     (,@ forms))
	 (select-window OriginallySelectedWindow)))))

;;; adapted from eval-in-window

(defmacro eval-in-screen (screen &rest forms)
  "Switch to SCREEN, evaluate FORMS, return to original screen."
  (` (let ((OrigScreen (current-screen)))
       (unwind-protect
	   (progn
	     (select-screen (, screen))
	     (,@ forms))
	 (select-screen OrigScreen)))))


;; widgets - Drop-menus, Pop-menus, and scrollbar
;; ==============================================

(defun widget:read-property (prop)
  "Read a property, return a lisp obj."
  (car (read-from-string (get-property prop))))


;;; WIDGET HANDLER

(setq epoch::event-handler-abort nil)

(push-property "gwm-result" 'widget:result-handler)
(push-property "scrollbar" 'widget:scrollbar-handler)
(push-property "Dmenu" 'widget:Dmenu-handler)
(push-property "Pmenu" 'widget:Pmenu-handler)


;;; gwm-result
;; the gwm-return macro invokes a command via GWM_EXECUTE property
;; result goes into gwm-result property

(defun widget:result-handler (type xatom scr)
  "Display result from gwm-result macro."
  (message (get-property "gwm-result")))


;; Scroll bar
(defconst widget:scrollbar-funcs
  '((1 . scroll-up) (2 . line-up-point) (3 . scroll-down)))

(defun widget:scrollbar-handler (type xatom scr)
  "scroll screen up and down"
  (let* ((msg (widget:read-property "scrollbar"))
	 (why  (nth 3 msg))
	 (height (nth 3 (screen-information)))
	 (where (/ (* height why) 100))
	 (loc (epoch::coords-to-point 10 where scr))
	 )
    ;; note that loc is nil if click next to mode line
    (if loc 
	(let* ((what (nth 1 msg))
	       (func (cdr-safe (assoc what widget:scrollbar-funcs)))
	       (win (nth 2 loc))
	       (font-size (nth 2 (font)))
	       (screen-line (/ where font-size))
	       (window-begin (nth 1 (window-edges win)))
	       (window-line (1+ (- screen-line window-begin)))
	       )
	  (eval-in-window win (funcall func window-line))
	  ))))



(defun line-up-point (line)
  "Scrolls point to window LINE."
  (scroll-down (- line (count-lines (window-start) (point)))))
  

(defun widget:scroll-index (index scr)
  "Jumps index% into the file."
  (eval-in-screen scr
   (if (>= index 98)
       (goto-char (point-max))
     (progn
       (goto-char (+ (point-min)	; For narrowed regions.
		     (/ (* (- (point-max) (point-min))
			   index) 100)))
       (beginning-of-line))
     )
   (what-cursor-position)))
    

;;; Menu handlers
;; Dmenus are assumed to asynchronous.  Message is an elisp command.
;; Pmenus should be synchronous.  Message returned is menu selection.

(defun widget:Dmenu-handler (type xatom scr)
  "Execute the function requested by user, mousing the Drop-menus."
  (let* ((msg (widget:read-property "Dmenu"))
	 (act (nth 3 msg)))
    (if (fboundp (car-safe act))
	(eval act)
      (message "Dmenu error: %s" act))
    ))


(defvar widget:Pmenu-return nil "Return value from popup menu")

(defun widget:Pmenu-handler (type xatom scr)
  "Store value returned by popup menu in widget:Pmenu-return."
  (let* ((msg (widget:read-property "Pmenu"))
	 (selection (nth 3 msg)))
    (setq widget:Pmenu-return selection)
    (throw 'widget:Pmenu-return selection)
    ))


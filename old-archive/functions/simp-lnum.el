;;; This will set the line number in the mode line automatically when
;;; next-line,prev-line,scroll-up, and scroll down are called. It does
;;; not work to well with vm.

;;; LCD Archive Entry:
;;; simple-line-numbers|?|?|
;;; Simple line number in the mode line.|
;;; 92-09-17||~/functions/simp-lnum.el.Z|

;;;  Use the following commented out code for displaying the current column.
;;;  Substitute it in the set-line function.
;;;
;;;  (setq mode-line-process
;;;        (format " %4d %4d" (1+ (count-lines 1 (point))) (current-column))))

(defun set-line ()
  (interactive)
  (setq mode-line-process
        (format " %4d" (1+ (count-lines 1 (point))))))

;;; redefine new-line-move to include output of line number

;;; Use this for version  18.57 and beyond

(defun new-line-move ()
  (fset 'old-line-move
        (symbol-function 'next-line-internal))
  (fset 'next-line-internal
        (function
         (lambda (arg)
           (old-line-move arg)
        (set-line)))))

;;; Use this for version before 18.57

;;;(defun new-line-move ()
;;;  (fset 'old-line-move
;;;	    (symbol-function 'line-move))
;;;  (fset 'line-move
;;;	    (function
;;;	     (lambda (arg)
;;;	       (old-line-move arg)
;;;           (set-line)))))

(new-line-move)

(defun new-scroll-up (&optional arg)
  (interactive "P")
  (scroll-up arg)
  (set-line))


(defun new-scroll-down (&optional arg)
  (interactive "P")
  (scroll-down arg)
  (set-line))

(global-set-key "v" 'new-scroll-down)
(global-set-key "\C-v" 'new-scroll-up)

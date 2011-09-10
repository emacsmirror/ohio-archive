;; LCD Archive Entry:
;; highlite|David Brennan|brennan@hal.com|
;; Higher level highliting support for Epoch
;; |92-04-22|0.42|~/epoch/highlite.el.Z

;; Copyright 1992, Dave Brennan
;; GNU Copyleft applies.

;; Interactive functions:

;; highlite-region
;; highlite-regxep
;; highlite-regexp-region
;;
;; To get started quickly add the following to your .emacs:
;;
;; (autoload 'highlite-region "highlite" "Highlite a region in Epoch." t)
;; (autoload 'highlite-regexp "highlite" "Highlite a regexp in Epoch." t)
;; (autoload 'highlite-regexp-region "highlite" "Highlite regexp region." t)
;; (setq highlite-load-hook 'highlite-create-default-styles)
;;

;; TODO:
;;
;; * Allow highlite-styles alist contain both styles and a function to
;;   evaluate to generate a style.
;; * Change the name (to "stylize" perhaps?).

(defvar highlite-styles nil
  "Alist of names and styles available for interactive picking when
hightliting.")

(defvar highlite-last-style-name nil
  "The name of the last style used for highliting.  Used as a default
when prompting for a style.")

(defvar highlite-always-prompt-style t
  "*If non-nil interactive highlite functions will always prompt for a style.
When set to nil it is possible to force prompting for a style by using a
prefix argument to highliting functions.")

(defvar highlite-last-regexp nil
  "The last regexp highlited.")

;; NOTE: Need to deal with missing style (maybe a style default?) -DJB

(defun highlite-get-named-style (style-name)
  "Convert a named style to a string using.  See highlite-styles variable."
  (cond
   ((stringp style-name)
    (cdr (assoc style-name highlite-styles)))
   ((stylep style-name)
    style-name)
   (t nil)))

(defun highlite-current-style (negate-always-prompt)
  "Return the current highlite style, prompting the user if necessary."
  (let ((style highlite-last-style-name)
	(force-prompt
	 (if negate-always-prompt
	     (not highlite-always-prompt-style)
	   highlite-always-prompt-style)))
    (if (or highlite-always-prompt-style
	    (null highlite-last-style-name)
	    force-prompt)
	(setq style
	      (completing-read "Style: " highlite-styles nil
			       t highlite-last-style-name)))
    (setq highlite-last-style-name style)
    style))

(defun highlite-region (start end style)
  (interactive
   (list (region-beginning) (region-end)
	  (highlite-current-style current-prefix-arg)))
  (setq style (highlite-get-named-style style))
  (let ((zone (make-zone)))
    (set-zone-style zone style)
    (move-zone zone start end)))

(defun highlite-regexp (regexp style)
  "Highlite a regular expression with a given style.  When used interactively
Emacs prompts for a regular expression.  A style is prompted for if the
highlite-always-prompt-style is non-nil.  Prefix arg negates the value of
that variable for the current function.  When called from a program takes
REGEXP, STYLE."
  (interactive
   (list
    (read-string "Regexp: " highlite-last-regexp)
    (highlite-current-style current-prefix-arg)))
  (setq style (highlite-get-named-style style))
  (save-excursion
    (while (re-search-forward regexp nil t)
      (highlite-region (match-beginning 0) (match-end 0) style)))
  (setq highlite-last-regexp regexp))

(defun highlite-regexp-region (start end regexp style)
  "Highlite a regular expression with a given style in the a region.  When
used interactively Emacs prompts for a regular expression.  A style is
prompted for if the highlite-always-prompt-style is non-nil.  Prefix arg
negates the value of that variable for the current function.  When called
from a program takes START, END, REGEXP, STYLE."
  (interactive
   (list
    (region-beginning) (region-end)
    (read-string "Regexp: " highlite-last-regexp)
    (highlite-current-style current-prefix-arg)))
  (setq style (highlite-get-named-style style))
  (save-excursion
    (goto-char start)
    (while (re-search-forward regexp end t)
      (highlite-region (match-beginning 0) (match-end 0) style)))
  (setq highlite-last-regexp regexp))

(defun highlite-remove-at (position)
  (interactive "d")
  (if (null (delete-zone-at position))
      (message "Nothing to remove.")))

(defvar highlite-load-hook nil
  "*Set of functions called when highlite is loaded.")

;; Quick and dirty default styles:

(defun highlite-create-default-styles ()
  "Create a set of default styles.  Normally this function is called from
your .emacs file if you don't create your own styles."
  (let ((style))
    ;; normal
    (setq style (make-style))
    (set-style-foreground style (foreground))
    (set-style-background style (background))
    (push (cons "normal" style) highlite-styles)
    ;; red
    (setq style (make-style))
    (set-style-foreground style "red")
    (push (cons "red" style) highlite-styles)
    ;; green
    (setq style (make-style))
    (set-style-foreground style "green")
    (push (cons "green" style) highlite-styles)
    ;; cyan
    (setq style (make-style))
    (set-style-foreground style "cyan")
    (push (cons "cyan" style) highlite-styles)
    ;; blue
    (setq style (make-style))
    (set-style-foreground style "blue")
    (push (cons "blue" style) highlite-styles)))

;; Only run the load hook when this if first loaded.
(if (not (featurep 'highlite))
    (run-hooks 'highlite-load-hook))

(provide 'highlite)

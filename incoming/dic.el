;;
;; Copyright Sven Utcke (Utcke@tu-harburg.de)
;; 
;; dic.el is an Emacs-interface to two very simple online
;; English-German-English dictionaries I am using, dic and dicleo.  Have
;; a look at
;;
;; http://www.informatik.uni-freiburg.de/~utcke/English/Software/#dic
;;
;; for more info.
;; 
;; 
;; You are pretty much free to do with this software whatever you feel
;; like doing, in particular copying and using. You can also modify it,
;; but I would ask you to
;; 
;;   1.save your modifications under a different name, 
;; 
;;   2.send me a copy of your modifications - chances are I'm going to
;;     learn something, especially with regard to elisp-programming.
;; 
;; Oh, and yes, don't blame me if this software shreds your harddrive or
;; deletes all your files. This is provided as is, without any warranty
;; at all. Basically you wont go much wrong if you think of it as
;; GNU-software, although it is not GNU.
;; 
;; $Id: dic.el,v 1.4 1998/10/23 10:25:41 utcke Exp $
;;

(defvar dic-command "dic"
  "*Command to call to look up a word in English-German/German-English dictionary")

(defvar dicleo-command "dicleo"
  "*Command to call to look up a word in Leo's (www.leo.org) 
online English-German/German-English dictionary")

(defvar dic-exact 'nil
  "*Only exact matches if non-nil, also partial matches if nil.  
Does this work with dicleo?")

(defun dic-lookup (dic-current-command)
  "Look up word in English-German/German-English dictionary, 
using the command given as parameter."
  (if dic-exact
      (setq dic-string '" ")
    (setq dic-string '""))
  ;; Word to look up
  (search-backward-regexp "\\b")
  (mark-word 1)
  ;; Command to call
  (setq dic-start (mark)
	dic-end   (point)
	dic-word  (buffer-substring dic-start dic-end)
	the-dic-command (concat dic-current-command " \"" dic-string dic-word dic-string "\""))
  ;; a rather simple list...
  (setq dic-list (list (shell-command-to-string the-dic-command)))
  ;; I can not use the normal completion-setup-function.
  ;; there must be a better way to do this!
  (make-local-hook 'completion-setup-hook)
  (setq completion-setup-hook '(lambda ()
                                 (dic-completion-setup-function)
                                 ))
  ;; Make the completion buffer
  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list dic-list)
    )
  )

(defun dic-mouse-choose-completion (event)
  "Click on an alternative in the `*Completions*' buffer 
to replace text between point and mark by it."
  (interactive "e")
  ;; call the normal completion function
  (mouse-choose-completion event)
  ;; delete marked word
  (exchange-point-and-mark)
  (delete-region (mark) (point))
  )

(defun dic-choose-completion ()
  "Choose the completion that point is in or next to."
  (interactive)
  ;; call the normal completion function
  (choose-completion)
  ;; delete marked word
  (exchange-point-and-mark)
  (delete-region (mark) (point))
  )

(defun dic ()
  "Look up word in system English-German/German-English dictionary"
  (interactive)
  (dic-lookup dic-command)
)

(defun dicleo ()
  "Look up word in Leo's (www.leo.org) online English-German/German-English dictionary"
  (interactive)
  (dic-lookup dicleo-command)
)

(defun dic-completion-setup-function ()
  (save-excursion
    (let ((mainbuf (current-buffer)))
      (set-buffer standard-output)
      (completion-list-mode)
      ;; minor mode to change [mouse2]
      ;; there must be a better way to do this
      (dic-completion-mode t)
      (make-local-variable 'completion-reference-buffer)
      (setq completion-reference-buffer mainbuf)
      (if (eq minibuffer-completion-table 'read-file-name-internal)
	  ;; For file name completion,
	  ;; use the number of chars before the start of the
	  ;; last file name component.
	  (setq completion-base-size
		(save-excursion
		  (set-buffer mainbuf)
		  (goto-char (point-max))
		  (skip-chars-backward (format "^%c" directory-sep-char))
		  (- (point) (point-min))))
	;; Otherwise, in minibuffer, the whole input is being completed.
	(save-match-data
	  (if (string-match "\\` \\*Minibuf-[0-9]+\\*\\'"
			    (buffer-name mainbuf))
	      (setq completion-base-size 0))))
      (goto-char (point-min))
      (if window-system
	  (insert (substitute-command-keys
		   "Click \\[dic-mouse-choose-completion] on a completion to select it.\n")))
      (insert (substitute-command-keys
	       "In this buffer, type \\[choose-completion] to \
select the completion near point.\n\n"))
      (forward-line 1)
      (while (re-search-forward "[a-zA-Z']+" nil t)
	(let ((beg (match-beginning 0))
	      (end (point)))
	  (if completion-fixup-function
	      (funcall completion-fixup-function))
	  (put-text-property beg (point) 'mouse-face 'highlight)
	  (goto-char end))))))

;; a new minor-mode to change the keymap.  There must be a better way!
(setq-default dic-completion-mode nil)
(make-variable-buffer-local 'dic-completion-mode)

(defun dic-completion-mode (arg)
  "*Toggle dic-completion-mode.  With arg, turn dic-completion-mode on if and only if arg is positive."
  (setq dic-completion-mode
        (if (null arg) (not dic-completion-mode)
          (> (prefix-numeric-value arg) 0)))
  )

(or (assq 'dic-completion-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(dic-completion-mode " Dic") minor-mode-alist)))

(setq dic-completion-mode-map (copy-keymap completion-list-mode-map))
(define-key dic-completion-mode-map [mouse-2] 'dic-mouse-choose-completion)
(define-key dic-completion-mode-map "\C-m"    'dic-choose-completion)

(or (assoc 'dic-completion-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'dic-completion-mode dic-completion-mode-map)
                minor-mode-map-alist)))

;;
;; $Log: dic.el,v $
;; Revision 1.4  1998/10/23 10:25:41  utcke
;; First release.  Now both [mouse-2] and [return] work.
;;
;;

;; todo.el -- Major mode for editing TODO files

;; Copyright (C) 1999 Daniel Lundin <daniel@emacs.org>

;; Author: Daniel Lundin <daniel@emacs.org>
;; Created: 19 Oct 1999
;; Version: 1.0
;; Keywords: TODO todo-mode

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; ---------------------------------------------------------------------------

;;; Commentary:

;;  todo.el is a major mode for editing TODO-files. 
;;  It has similarities and stolen code from Oliver Seidel's
;;  todo-mode.el , but has been significantly simplified to better
;;  adhere to mine (and my company's) needs.
;;
;;
;;
;;  Keycommands
;;
;;    C-c C-n  Forward item         (outline-next-visible-heading)
;;    C-c C-p  Backward item        (outline-previous-visible-heading)
;;    C-c C-i  Insert item          (todo-insert-item)
;;    C-c S-C-n  Lower item         (todo-lower-item)
;;    C-c S-C-p  Raise item         (todo-raise-item)
;;    C-c C-k  Delete item          (todo-delete-item)
;;    C-c C-v  Assign item to self  (todo-assign-item-to-self)
;;    C-c C-d  Assign item to other (todo-assign-item)
;;    TAB      Indent line          (todo-ident-line)
;;
;;  Inherited from outline-mode
;;
;;    C-c C-t  Hide all items
;;    C-c C-a  Show all items
;;    C-c C-c  Hide item
;;    C-c C-e  Show item


;;; Code:


;; Required packages
(require 'outline)
(require 'custom)


(defgroup todo nil
  "Maintain a todo list."
  :group 'calendar)

(defcustom todo-file    "~/.todo"
  "*TODO mode list file."
  :type 'file
  :group 'todo)


(defcustom todo-collapse-items nil
  "*Wether to hide multiline items."
  :type 'boolean
  :group 'todo)

(defcustom todo-signature  (or (getenv "INITIALS") (user-login-name))
  "*Signature name to be used for assigning todo items to oneself."
  :type 'string
  :group 'todo)



;; Keymap
(defvar todo-mode-map           nil     "TODO mode keymap.")
(setq todo-mode-map nil)
(if todo-mode-map
    nil
  (let ((map (make-keymap)))
;;    (suppress-keymap map t)
    (define-key map "\C-c\C-b" 'todo-assign-item)
    (define-key map "\C-c\C-d" 'todo-unassign-item)
    (define-key map "\C-c\C-v" 'todo-assign-item-to-self)
    (define-key map "\C-c\C-k" 'todo-delete-item)
    (define-key map "\C-c\C-i" 'todo-insert-item)
    (define-key map "\C-c\C-t" 'hide-body)
    (define-key map "\C-c\C-a" 'show-all)
    (define-key map "\C-c\C-c" 'hide-subtree)
    (define-key map "\C-c\C-e" 'show-subtree)
    (define-key map "\C-c\C-p" 'outline-previous-visible-heading)
    (define-key map "\C-c\C-n" 'outline-next-visible-heading)
    (define-key map [C-up] 'outline-previous-visible-heading)
    (define-key map [C-down] 'outline-next-visible-heading)
    (define-key map [C-S-up] 'todo-raise-item)
    (define-key map [C-S-down] 'todo-lower-item)
    (setq todo-mode-map map)))

;; Menu
(easy-menu-define todo-menu todo-mode-map "Todo Menu"
                  '("Todo"
                    ["Show all"    show-all t]
                    ["Hide all"    hide-body t]
                    ["Show item"   show-subtree t]
                    ["Hide item"   hide-subtree t]
                    "---"
                    ["Raise item"   todo-raise-item t]
                    ["Lower item"   todo-lower-item t]
                    "---"
		    ["Insert item"     todo-insert-item t]
                    ["Kill item"            todo-delete-item t]
                    "---"
                    ["Assign item to self" todo-assign-item-to-self t]
                    ["Assign item to other" todo-assign-item t]
                    ["Unassign item" todo-unassign-item t]
                    ))


(defun todo-hide-show-item () 
  "Hide the body of a TODO item."
  (interactive)
  (save-excursion
    (end-of-line)
    (if (outline-visible)
	(hide-subtree)
      (show-subtree))))


(defun todo-delete-item () 
  "Delete the current TODO item."
  (interactive)
  (if (> (count-lines (point-min) (point-max)) 0)
      (let* ((todo-answer (y-or-n-p (concat "Permanently remove item? "))))
        (if todo-answer
            (progn
              (todo-remove-item)
              ))
        (message ""))
    (error "No TODO list entry to delete")))


(defun todo-item-start () 
  "Return point at start of current TODO list item."
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at "^[\*o] "))
        (search-backward-regexp "^[\*o] " nil t))
    (point)))

(defun todo-item-end () 
  "Return point at end of current TODO list item."
  (save-excursion
    (end-of-line)
    (search-forward-regexp "^[\*o] " nil 'goto-end)
    (- (point) (if (eq (point) (point-max)) 0 2))
    ))

(defun todo-remove-item () 
  "Delete the current entry from the TODO list."
  (delete-region (todo-item-start) (todo-item-end)))


(defun todo-assign-item (&optional user) 
  "Assign TODO item to a user/name."
  (interactive "sAssign item to: ")
  (if (and user (> (length user) 0))
      (save-excursion
	(beginning-of-line)
	(if (not (looking-at "^[\*o] "))
	    (search-backward-regexp "^[\*o] " nil t))
	(if (re-search-forward "^\* " (todo-item-end) t)
	    (replace-match (concat "o [" user "] ") nil nil)
	  (if (re-search-forward "^o \\[.*\\] " (todo-item-end) t)
	      (replace-match (concat "o [" user "] ") nil nil) ) ))))


(defun todo-assign-item-to-self () 
  "Assign TODO item to a self."
  (interactive)
  (todo-assign-item todo-signature))


(defun todo-unassign-item () 
  "Make TODO item unassigned."
  (interactive "")
  (save-excursion
    (beginning-of-line)
    (if (not (looking-at "^[\*o] "))
	(search-backward-regexp "^[\*o] " nil t))
      (if (re-search-forward "^o \\[.*\\]" (todo-item-end) t)
	  (replace-match "*" nil nil)
	(message "Item is already unassigned."))
      )
  )


(defun todo-raise-item () 
  "Raise priority of a TODO item."
  (interactive)
  (kill-region (todo-item-start) (todo-item-end))
  (search-backward-regexp "^[\*o] " nil t)
  (yank)
  (search-backward-regexp "^[\*o] " nil t))


(defun todo-lower-item () 
  "Lower priority of a TODO item."
  (interactive)
    (kill-region (todo-item-start) (todo-item-end))
    (if (search-forward-regexp "^[\*o] " nil 'goto-end 2)
	(backward-char 2)
      (end-of-buffer))
    (yank)
    (search-backward-regexp "^[\*o] " nil t))


(defun todo-insert-item () 
  "Insert a new TODO item."
  (interactive)
  (goto-char (- (todo-item-end) 1))
  (insert "\n* \n")
  (backward-char)
)


(defun todo-indent-line ()
  "Indent a line properly according to TODO file format."
  (interactive)
    (beginning-of-line)
    (if (eq (point) (point-at-eol))
	(insert "  ")
      
      (if (re-search-forward "^[ ]*\\([^o\* ]\\)" (point-at-eol) t)
	  (replace-match "  \\1" nil nil) 
	(if (re-search-forward "^[ ]*\\([\\*o]\\)" (point-at-eol) t)
	    (replace-match "\\1" nil nil) )) 
    ))
  


(defun todo-mode () 
  "Major mode for editing TODO lists.\n\n\\{todo-mode-map}"
  (interactive)
  (kill-all-local-variables)

  (setq major-mode 'todo-mode)
  (setq mode-name "todo")

  (use-local-map todo-mode-map)
  (easy-menu-add todo-menu)

  (make-local-variable 'paragraph-start)
  (setq paragraph-start "[\*o] ")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "[\*o] ")

  (set-syntax-table text-mode-syntax-table)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(outline-font-lock-keywords t))
  (setq fill-prefix "  ")
  (setq outline-regexp "[\*o]")

  (outline-minor-mode 1)

  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'todo-indent-line)

  (auto-fill-mode 1)

  (run-hooks 'todo-mode-hook))


(if (not (fboundp 'point-at-bol))
    (defun point-at-bol () "Return value of point at beginning of line."
      (save-excursion
       (beginning-of-line)
        (point))))

(if (not (fboundp 'point-at-eol))
    (defun point-at-eol () "Return value of point at end of line."
      (save-excursion
        (end-of-line)
        (point))))


(provide 'todo-mode)

;; ---------------------------------------------------------------------------
;;; todo.el ends here
;; ---------------------------------------------------------------------------


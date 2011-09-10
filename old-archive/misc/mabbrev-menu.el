;; mabbrev-menu.el -- Add a pull-down menu of mail abbrevs to FSF GNU Emacs 19
;;
;; Copyright (C) 1993  Paul D. Smith <psmith@wellfleet.com>
;;
;; This code is covered under the FSF GNU GPL, either version 1 or, at
;; your option, any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; INSTALLATION:
;; -------------
;;
;; This works with mail abbreviations ONLY.  Feel free to modify it to
;; work with mail aliases as well.
;;
;; You MUST load this BEFORE the mail-abbrevs-setup hook in your .emacs
;; file (to ensure the mabbrevs hook is called *after* the
;; mail-abbrevs-setup hook--the hooks are called in reverse order of
;; definition).
;;
;; So you should have something like this in .emacs:
;;
;;  (require 'mabbrev-menu)
;;   ...
;;  (add-hook 'mail-setup-hook 'mail-abbrevs-setup)
;;
;; If you don't do it in the right order the menu item may not appear.
;;
;; If you use setq instead of add-hook to modify mail-setup-hook, then
;; you must change to use add-hook.
;;
;;
;;; LCD Archive Entry:
;;; mabbrev-menu|Paul D. Smith|psmith@wellfleet.com|
;;; Adds a menu of mail abbrevs to the mail mode menu bar for FSF GNU Emacs 19|
;;; 15-Jul-1992|1.2|~/misc/mabbrev-menu.el.Z|
;;
;; HISTORY:
;; --------
;;  1.02: 15 Jul 1993 (pds)
;;      - Yikes!  Abbreviations are really hash tables, not simple vectors.
;;        And, there's no easy way to retrieve them; the only abbrev listing
;;        functions available write them to a buffer :(
;;
;;        So, I create a temp buffer, write them out, read them in, and
;;        format them.  What a pain.
;;
;;  1.01: 14 Jul 1993 (pds)
;;	- Sort the list of aliases before displaying
;;      - Add a few comments :)
;;
;;  1.00: 13 Jul 1993 - Created by Paul D. Smith <psmith@wellfleet.com>
;;
;; ACKNOWLEDGMENTS:
;; ----------------
;; * Mike Chace <mikec@praxis.co.uk> suggested the idea.
;;
;; * Jamie Zawinski <jwz@lucid.com> wrote the very cool mailabbrev.el
;;   package.
;;

;; USER CUSTOMIZATIONS:
;; --------------------

;; Unset this if you don't want each abbrev expanded as it's added
;;
(defvar mabbrev-menu-expand t
  "*If t, abbrevs are expanded then inserted. If nil, the abbrev name is inserted.")

;; Save the abbrev table between runs; nil means re-build the list.  You
;; should set this to nil after you modify your mail aliases.
;;
(defvar mabbrev-list nil
  "*Mail abbreviations menu list; nil means re-build the list.")

;; Rebuild the list every time we pop up the menu
;;
;; This might be too slow if you have lots of aliases, but if you set it
;; to nil you have to reset MABBREV-LIST to nil each time you add/delete
;; your aliases...
;;
(defvar mabbrev-list-rebuild-always t
  "*Rebuild Mail abbreviations menu list every time the menu is shown.")

;;;----------------------------------------------------------------------------
;;
;; The code

;; Sort comparison routine
;;
(defun mabbrev-cmp (a1 a2)
  (string< (car a1) (car a2)))

;; Build the mabbrevs menu listing
;;
(defun mabbrev-menu-build ()
  (setq mabbrev-list nil)
  (if (and (boundp 'mail-abbrevs) (vectorp mail-abbrevs))
      (let ((mbuf (generate-new-buffer " *mabbrev-menu-list*"))
	    mlist)
	(save-excursion
	  (set-buffer mbuf)
	  (insert-abbrev-table-description 'mail-abbrevs nil)
	  (goto-char (point-min))
	  (setq mlist (car (cdr (car (cdr (cdr (read mbuf))))))))
	(kill-buffer mbuf)
	(while mlist
	  (setq mabbrev-list (cons (cons (car (car mlist))
					 (car (car mlist))) mabbrev-list))
	  (setq mlist (cdr mlist)))
	(setq mabbrev-list (sort mabbrev-list 'mabbrev-cmp)))))

;; Set up the abbrevs menu, and insert the chosen abbreviation
;;
(defun mabbrev-menu (event)
  "Pop up a menu of mail abbrevs for selection with the mouse.
The list of abbrevs is retrieved from the mail-abbrevs package.
The abbrev that you select is inserted at point."
  (interactive "e")
  (let (result)
    (if (or mabbrev-list-rebuild-always (not mabbrev-list))
	(mabbrev-menu-build))
    (if (and mabbrev-list
	     (setq result
		   (x-popup-menu (if (listp event)
				     event
				   (cons '(0 0) (selected-frame)))
				 (list "Aliases Menu"
				       (cons "Select Alias" mabbrev-list)))))
      (progn
	(insert result)
	(if mabbrev-menu-expand
	    (expand-abbrev))))))


;; Setup -- mail-mode-map, etc. isn't defined until sendmail.el is
;; loaded, so this has to be done in a hook
;;
(defun mabbrev-menu-setup ()
  ;; Make sure mail-abbrevs is defined and set up...
  (if (boundp 'mail-abbrevs)
      (progn
	;; Emacs' before 19.16 don't have a mail-mode-map menu-bar
	(if (not (lookup-key mail-mode-map [menu-bar]))
	    (define-key mail-mode-map [menu-bar]
	      (make-sparse-keymap "mail-menu-bar")))
	;; Create the menu
	(define-key mail-mode-map [menu-bar mabbrev]
	  '("Aliases" . mabbrev-menu)))))

;; Install the hook
;;
(add-hook 'mail-setup-hook 'mabbrev-menu-setup)

;; Provide ourselves...

(provide 'mabbrev-menu)

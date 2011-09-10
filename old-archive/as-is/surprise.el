;To: unix-emacs@bbn.com
;Date: 20 Dec 88 21:30:10 GMT
;From: John Robinson <jr@bbn.com>
;Subject: surprise
;
;Here's an interactive greeting card.  Eval the following and then M-X
;surprise.  May offend some non-Christians...
;
;I picked this up originally in mocklisp, from I forget where.

;;; GNU Emacs code converted from Mocklisp

(defun show ()
  (save-excursion
    (goto-char (dot-min))
    (sit-for 0)
    ))

(defun replace (from to)
  (save-excursion
    (while (search-forward from nil t)
      (replace-match to))))

(defun re-replace (from to)
  (save-excursion
    (while (re-search-forward from nil t)
      (replace-match to))))

(defun surprise (limit)
  "Towers of Hanoi greeting"
  (interactive "nValue (I suggest 3, 4 or 5): ")
  (let ((i 1))
    (pop-to-buffer "Surprise")
    (erase-buffer)
    (insert-string "  .   .   . \n =|=  |   | \n")
    (while (< i limit)
      (goto-char (dot-max))
      (forward-line -1)
      (set-mark (dot-max))
      (insert-string (buffer-substring (point) (mark)))
      (goto-char (dot-min))
      (replace " . " "  .  ")
      (goto-char (dot-min))
      (replace " =" "  =")
      (goto-char (dot-min))
      (replace "= " "=  ")
      (goto-char (dot-min))
      (replace " | " "  |  ")
      (goto-char (dot-max))
      (forward-line -1)
      (replace " =" "==")
      (replace "= " "==")
      (setq i (1+ i)))
    (goto-char (dot-min))
    (replace "." " ")
    (goto-char (dot-min))
    (sit-for 2)
    (do-it limit 1 2 3)
    (goto-char (dot-min))
    (search-forward "=|")
    (previous-line 1)
    (backward-char)
    (insert-string "*")
    (forward-line 1)
    (set-mark (dot-min))
    (let ((str (buffer-substring (point) (mark))))
      (goto-char (dot-max))
      (insert-string str))
    (previous-line 1)
    (save-excursion (delete-char 1) (replace "*" "y X") (sit-for 0))
    (save-excursion (delete-char 1) (re-replace "y.X" "p\\&m") (sit-for 0))
    (save-excursion (delete-char 1) (re-replace "p.*m" "p\\&a") (sit-for 0))
    (save-excursion (delete-char 1) (re-replace "p.*a" "a\\&s") (sit-for 0))
    (save-excursion (delete-char 1) (re-replace "a.*s" "H\\&!") (sit-for 0))
    (goto-char (dot-min))
    (search-forward "*")
    (backward-char)))

(defun raise ()
  (while (looking-at "=*|")
    (let ((i 1))
      (while (<= i 2)
	(setq i (1+ i))
	(while (looking-at "=")
	  (delete-char 1)
	  (insert-string " ")
	  (backward-char)
	  (previous-line 1)
	  (delete-char 1)
	  (insert-string "=")
	  (next-line 1))
	(forward-char))
      (previous-line 1)
      (search-backward " =")
      (forward-char)
      (show))))

(defun lower ()
  (next-line 1)
  (while (not (or (looking-at "=") (eobp)))
    (previous-line 1)
    (let ((i 1))
      (while (<= i 2)
	(setq i (+ i 1))
	(while (looking-at "=")
	  (delete-char 1)
	  (insert-string " ")
	  (backward-char)
	  (next-line 1)
	  (delete-char 1)
	  (insert-string "=")
	  (previous-line 1))
	(forward-char))
      (next-line 1)
      (search-backward " =")
      (forward-char)
      (show)
      (next-line 1))))

(defun jiggle (from to)
  "Move a single ring for Hanoi program."
  (interactive "nFrom: \nnTo: ")
  (let ((replacement (if (< from to) "  \\1" "\\1  ")))
    (goto-char (dot-min))
    (search-forward "|" nil nil from)
    (backward-char 2)
    (while (not (looking-at "="))
      (next-line 1))
    (search-backward " ")
    (forward-char)
    (raise)
    (goto-char (dot-min))
    (search-forward "|" nil nil to)
    (backward-char)
    (previous-line 1)
    (while (not (looking-at "="))
      (goto-char (dot-min))
      (re-replace " \\(==* ==*\\) " replacement)
      (show)
      (search-forward "|" nil nil to)
      (backward-char)
      (previous-line 1))
    (while (not (looking-at " "))
      (goto-char (dot-min))
      (re-replace " \\(==* ==*\\) " replacement)
      (show)
      (search-forward "|" nil nil to)
      (backward-char)
      (previous-line 1))
    (search-backward " =")
    (forward-char)
    (lower)))

(defun do-it (level from to work)
  (if (> level 0)
      (progn
	(do-it (- level 1) from work to)
	(jiggle from to)
	(do-it (- level 1) work to from)
	)))
;/jr
;jr@bbn.com or bbn!jr


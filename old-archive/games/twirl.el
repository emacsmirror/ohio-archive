;; LCD Archive Entry:
;; twirl|Terrence Brannon|brannon@jove.cs.caltech.edu
;; |this will twirl the cursor until you hit C-g
;; |92-03-07|1.0|~/games/twirl.el.Z|


(setq twirl-chars [?| ?\/ ?- ?\\])
(setq twirl-count 0)

(message "C-g to quit")

(defun twirl-pause (time)
  (while (progn (setq time (1- time))
		(> time 0))))

(defun twirl-next-char ()
  (if (not (eq twirl-count 4))
      (elt twirl-chars twirl-count)
    (progn (setq twirl-count 0)
	   (elt twirl-chars twirl-count))))

(twirl-next-char)

(defun twirl-em()
(while (progn
 (insert (twirl-next-char))
  (setq twirl-count (1+ twirl-count))
  (sit-for 0)
  (twirl-pause 200)
  (backward-delete-char-untabify 1)
  t)))

(defun twirl()
  "--------"
  (setq twirl-win (selected-window))
  (get-buffer-create "twirl-buffer") 
  (switch-to-buffer "twirl-buffer")
  (erase-buffer)
  (goto-char (point-min))
  (twirl-em))








(defvar  blank-screen "










")


(twirl)


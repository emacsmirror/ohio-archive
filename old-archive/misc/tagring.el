; Newsgroups: gnu.emacs.sources
; Path: hal.com!decwrl!pacbell.com!iggy.GW.Vitalink.COM!cs.widener.edu!eff!world!barryn
; From: barryn@world.std.com (Barry N. Nelson)
; Subject: tag-backwards
; Organization: The World
; Date: Thu, 12 Nov 1992 16:08:56 GMT
; 
; tag-backwards() will let you "undo" find-tag().  In other words you
; can switch back to the buffers where the calling functions are and put
; the mouse at the places where you did find-tag().  This feature
; simulates a backtrace available in a debugger.
; 
;; ============================================
;; tagring.el - undo tag-find
;;
;; LCD Archive Entry:
;; tagring|Barry N. Nelson|barryn@world.std.com|
;; Remember and undo searches with find-tag.|
;; 92-11-12||~/misc/tagring.el.Z|
;;
;; Auther: Jamie Tan, General Signal Corp./Drytek
;; <barryn@world.std.com>
;;
;; USAGE: 
;; After you have done 'find-tag', you can press M-' to switch back to the 
;; buffers where the calling functions are and put the cursor at the
;; places where you did find-tag(). You can continue pressing M-' to
;; trace back to the first level of your calling routines.

(require 'comint)
(require 'tags)

(defvar tag-ring-size 30
  "Size of tag ring.")

(defvar tag-ring-index 0
  "index of tag ring.")

(defun tags-completion-alist ()
  "Return an alist of tags in the current buffer, which is a tag table."
  (let (alist next)
    (message "Making tags completion alist...")
    (save-excursion
      (goto-char (point-min))
	(while (search-forward "\177" nil t)
	  (if (save-excursion
		(skip-chars-forward "^\001\n")
		(setq next (1+ (point)))
		(= (following-char) ?\001))
	      ;;; If there are ^A's, get tags after them.
	      (progn
		(goto-char next)	;; after the first ^A
		(while (= (preceding-char) ?\001)
		  (setq alist
			(cons (cons (buffer-substring
				     (point)
				     (progn (skip-chars-forward "^\001\n")
					    (point)))
				    nil)
			      alist))
		  (forward-char 1)))
	    ;;; If no ^A's, get tags from before the ^?.
	    (skip-chars-backward "^-A-Za-z0-9_$\n")
	    (or (bolp)
		(setq alist
		      (cons (cons (buffer-substring
				   (point)
				   (progn
				     (skip-chars-backward "-A-Za-z0-9_$")
				     ;;; `::' in the middle of a C++ tag.
				     (while (and (= (preceding-char) ?:)
						 (= (char-after (- (point) 2)) ?:))
				       (progn (backward-char 2)
					      (skip-chars-backward
					       "-A-Za-z0-9_$")))
				     (point)))
				  nil)
			    alist)))
	    (goto-char next)		; next line
	    )))
    (message "Making tags completion alist...done")
    alist)

  ;; Init tag-ring
  (setq buffer-ring (make-ring tag-ring-size))
  (setq offset-ring (make-ring tag-ring-size))
  (setq tag-ring-index 0))

(defun find-tag (tagname &optional next-p other-window regexp-p)
  "Find tag (in current tag table) whose name contains TAGNAME;
more exact matches are found first.
Select the buffer containing the tag's definition and move point there.
The default for TAGNAME is the expression in the buffer after or around point.

If second arg NEXT-P is non-nil (interactively, with prefix arg), search
for another tag that matches the last tagname or regexp used.

If third arg OTHER-WINDOW is non-nil, select the buffer in another window.

If fourth arg REGEXP-P is non-nil, treat TAGNAME as a regexp.

See documentation of variable `tags-file-name'."
  (interactive (if current-prefix-arg
		   '(nil t)
		   (list (prompt-for-tag "Find tag: "))))
  (setq calling-buffer (current-buffer))
  (setq calling-point (point))
  (cond
   (next-p (find-tag-in-order nil nil nil nil nil other-window))
   (regexp-p (find-tag-in-order tagname
				're-search-forward
				'(tag-re-match-p)
				t
				"matching"
				other-window))
   (t (find-tag-in-order
       tagname
       'search-forward
       '(tag-exact-match-p tag-word-match-p tag-any-match-p)
       nil
       "containing"
       other-window)))

  ;; update the tag-file ring
  (ring-insert buffer-ring calling-buffer)
  (ring-insert offset-ring calling-point)
  (setq tag-ring-index 0)
  (message "point=%d" calling-point))

(defun tag-backwards (arg)
  "Cycle backwards through tag-ring."
  (interactive "p")
  (let ((len (ring-length buffer-ring)))
    (if (<= len 0)
	(message "Empty tag ring")
      (setq arg
	    (if (> arg 0) 1
	      (if (< arg 0) -1 0)))
      (switch-to-buffer (ring-ref buffer-ring tag-ring-index))
      (goto-char (ring-ref offset-ring tag-ring-index))
      (setq tag-ring-index (comint-mod (+ tag-ring-index arg) len))
      (message "%d" (ring-ref offset-ring tag-ring-index)))))

(define-key esc-map "'" 'tag-backwards);

(defun tag-forwards (arg)
  "Cycle forwards through tag-ring."
  (interactive "p")
  (tag-backwards (- arg)))

(define-key esc-map "`" 'tag-forwards);

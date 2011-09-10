;;;Date: 25 Jun 87 18:16:11 GMT
;;;From: John W Peterson <peterson@CS.UTAH.EDU>
;;;Organization: University of Utah CS Dept
;;;Subject: Spelling helper for GNU emacs

;;;There was a request for a tool that helps you find correctly spelled words.
;;;Since my spelling is pretty hopeless, I came up with the following.  It's
;;;worthwhile to note that you can invoke it when being prompted for a
;;;word by spell-buffer.  I've found the combination of the two quite useful.

;;;Cheers,
;;;jp
;;;--------------------------------cut here-------------------------------------

;;;
;;; spell-dict.el - Access to on-line spelling dictionary.  
;;;
;;; Author:	John W. Peterson
;;;		Computer Science Dept.
;;;		University of Utah
;;; Date:	16-Dec-86
;;;
;;;    ESC-?   Look up words starting with what you typed in the buffer and
;;;            let you search for a word to replace it with.
;;;

(defvar *dictionary-file* "/usr/dict/words"
  "*File used for the spelling dictionary.  On 4.3bsd systems, try
using \"/usr/dict/web2\" for a larger selection.  Apollo users may
want to try \"/sys/dict\".")

(defvar *max-dictionary-height* 15 "*Maximum number of lines the 
*Spelling Dictionary* window will take on the screen")


(defun spell-lookup-word-in-buffer ()
  "Look up words in the dictionary that start with the letters from point
to the beginning of the word in the current buffer.  A window pops up and 
puts you in I-search mode.  Search for the word you want (you don't have to
type the whole thing) and then hit ESC.  That word is then inserted into
your buffer, replacing the word you had partially typed.  Type ^G to abort."  

  (interactive)
  (let* ((cur-buf (current-buffer))
	 (cur-win (selected-window))
	 (one-window (eq cur-win (next-window)))
	 num-lines target-height lookup-str str-beg str-end word-end word
	 in-word replace-str)
    ;;
    ;; Get the string to look up
    ;;
    (setq str-end (point))
    (save-excursion
      (setq in-word (looking-at "\\w"))	;Flag if cursor is inside lookup word
      (backward-word 1)
      (setq str-beg (point))
      (setq lookup-str (buffer-substring str-beg str-end))
      ;; You can look up just part of a word, but need to replace all of it.
      (forward-word 1)
      (setq replace-str (buffer-substring str-beg (point))))
    ;;
    ;; The purist would probably use Electric-pop-up-window here.  I couldn't
    ;; get it to work to my liking.
    ;;
    (save-window-excursion
      (if one-window
	  ;; It would be nice to have something here that would force the
	  ;; spelling dictionary window to the bottom.
	  nil
	(other-window 1))		;Don't nuke current window
      (with-output-to-temp-buffer "*Spelling Dictionary*"
	(switch-to-buffer "*Spelling Dictionary*" t)
	(call-process "look" nil t nil "-df" lookup-str *dictionary-file*)
	(fill-region (point-min) (point-max) nil)
	(setq num-lines (count-lines (point-min) (point-max)))
	; Stolen from Electric-pop-up-window...
	(setq target-height
	      (min (max (1+ num-lines) window-min-height)
		   (min
		    (save-window-excursion	; Find Max screen size
		      (delete-other-windows)
		      (1- (window-height (selected-window))))
		    *max-dictionary-height*)))
	)
      ;;
      ;; Window hacking must be done outside the scope of 
      ;; with-output-to-temp-buffer
      ;;
      (select-window (get-buffer-window "*Spelling Dictionary*"))
      (if (< target-height num-lines)
	  (setq mode-name "*MORE*")
	(setq mode-name "All"))
      (shrink-window (- (window-height) target-height))
      (isearch-forward)			;Allow them to look for word
      (cond
       ((not (= (point-min) (point)))	;Search found something (no abort)
	(if (not (looking-at "\\Sw"))	;Find entire thing, copy to word
	    (forward-word 1))
	(setq word-end (point))		
	(backward-word 1)
	(setq word (buffer-substring (point) word-end))
	(switch-to-buffer cur-buf)
	(select-window cur-win)		;Kill partial in buffer and replace
	(if in-word			
	    (forward-word 1))		;Replace the whole thing
	;;
	;; Use search/replace to fix word so capitolization/case is preserved
	;;
	(search-backward replace-str str-beg)
	(replace-match word))))))      

(global-set-key "\e?" 'spell-lookup-word-in-buffer)

;;; @(#) c-comment-edit.el -- C Comment Edit

;;{{{ id

;; This file is *NOT* part of GNU emacs

;; Copyright (C) 1987, 1988, 1989 Kyle E. Jones <kyle_jones@wonderworks.com>
;; Author:       Kyle E. Jones
;; Maintainer:   Jari Aalto <jaalto@tre.tele.nokia.fi>
;; Created:      1987
;;
;; Look at the code with folding.el, tinyfold.el, tinybm.el

;; LCD Archive Entry:
;; c-comment-edit|Jari Aalto|jaalto@tre.tele.nokia.fi|
;; C Comment Edit|
;; 08-Jun-1995|1.1|~/misc/c-comment-edit.el.Z|

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;}}}
;;{{{ Docs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From info-gnu-emacs-request@prep.ai.mit.edu  Thu Jan 12 16:34:14 1989
;;; Received: from TUT.CIS.OHIO-STATE.EDU by prep.ai.mit.edu;
;;; Thu, 12 Jan 89 16:34:14 EST
;;; Received: by tut.cis.ohio-state.edu (5.59/2.881128)
;;; 	id AA27840; Thu, 12 Jan 89 14:54:51 EST
;;; Received: from USENET by tut.cis.ohio-state.edu with netnews
;;; 	for info-gnu-emacs@prep.ai.mit.edu (info-gnu-emacs@prep.ai.mit.edu)
;;; 	(contact usenet@tut.cis.ohio-state.edu if you have questions)
;;; Date: 12 Jan 89 17:36:19 GMT
;;; From: rti!talos!kjones@mcnc.org  (Kyle Jones)
;;; Organization: Philip Morris Research Center, Richmond, VA
;;; Subject: `c-comment-edit' revisited
;;; Message-Id: <396@talos.UUCP>
;;; Sender: info-gnu-emacs-request@prep.ai.mit.edu
;;; To: info-gnu-emacs@prep.ai.mit.edu
;;;
;;; Attached is an enhanced version of the `c-comment-edit' package, last
;;; posted sometime in 1987.
;;;
;;; c-comment-edit is a command that copies a C comment into a
;;; temporary buffer for editing under a more suitable major mode
;;; (usually text-mode).  Once the comment is edited,
;;; c-comment-edit-end (normally bound to C-c ESC) replaces the old
;;; comment with the edited version, adding comment delimiters and
;;; leaders as necessary.  c-comment-edit is ideal for large comments
;;; of these styles:
;;;
;;; 	/*	/*	/*
;;; 	  ...	 * ...	** ...
;;; 	  ...	 * ...	** ...
;;; 	*/	 */	*/
;;;
;;; Features added:
;;;
;;; * c-comment-edit no longer uses a recursive-edit so multiple
;;;   c-comment-edit's  be running simultaneously.
;;; * c-comment-edit will now search forward from point for a comment if
;;;   point is not within a comment.
;;; * c-comment-edit-hook is provided.
;;; * Bill Carpenter's c-comment-leader-regexp fixed was incorporated.
;;;
;;; kyle jones   <kyle@odu.edu>   ...!uunet!talos!kjones
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;}}}
;;{{{ history

;;; HISTORY OF CHANGES
;;; ==================================================
;;;
;;; Apr	    28	1995	19.28	Jari Aalto	<jaalto@tre.tele.nokia.fi>
;;; - After talking with Kyle directly, he said that I should load
;;;   this .el  with some other name, because he didn't plan to have
;;;   any enhancement support. He said that I had been among the very
;;;   few that had ever asked any changes to this module.
;;; - On my behalf, since this is almost identical copy of the
;;;   original, I have no objections that someone else modifies this
;;;   .el with the _same_name_ -- I do plan to update this if it has
;;;   errors that are caused by me of course, but right now I don't
;;;   think it misses anything crucial. Just drop me a note, I love to hear
;;;   new improvement.
;;;
;;; Feb	    22	1995	19.28	Jari Aalto	<jaalto@tre.tele.nokia.fi>
;;; - I had used this intensively when 18.57 was still around in our
;;;   envinronment (about 4 moths ago), but when I moved to 19.28
;;;   I found some incompatibilities in keybindings and I dislike the
;;;   comment syntax which left first line empty. Now there is variable
;;;   to configure 1st line layout.
;;; - Now ESC ESC terminates in 19 and ESC in 18.
;;; - Added few confortable variables
;;;   *  c-comment-edit-bname , c-comment-edit-empty-1-line
;;;   *  c-comment-edit-other-buffer , c-comment-edit-C-buf
;;;   *  c-comment-edit-[bc,ec]
;;; - Added hooks
;;;   *  c-comment-edit-end-hook
;;;   *  c-comment-edit-after-hook
;;;
;;; - touched original code:
;;;   *  c-comment-edit
;;;      - there was one problem with this, the hook was too early
;;;        run compared to kill-local-var, so if you turned on some minor
;;;        mode, it was effectively lost. --> now the hook runs last.
;;;   *  c-comment-edit-end
;;;      - Added the new functionality here, many changes, sets global
;;;        variables + runs hooks
;;;
;;; - There is an EXAMPLE section at the end of this file which is one of
;;;   my favourite C/C++ function header. And cleanup function to
;;;   retain the format after comment has been added too.

;;}}}


;;; .......................................................... &v-bind ...

(defvar v18 (string< emacs-version "19"))

(defvar c-com-mode-map nil)
(if c-com-mode-map
    nil
  (setq c-com-mode-map (make-sparse-keymap))

  ;; keys;
  (if v18
      (progn
	(define-key c-com-mode-map "\C-c\C-c" 'c-comment-edit-end)
	(define-key c-com-mode-map "\e" 'c-comment-edit-abort)
	)
    (define-key c-com-mode-map "\C-c\C-c" 'c-comment-edit-end)
    (define-key c-com-mode-map "\e\e" 'c-comment-edit-abort)
    ))

;;; .......................................................... &v-conf ...

(defvar c-comment-leader " *"
  "*Leader used when rebuilding edited C comments.  The value of this variable
should be a two-character string.  Values of \"  \", \" *\" and \"**\"
produce the comment styles:
	/*	/*	/*
	  ...	 * ...	** ...
	  ...	 * ...	** ...
	*/	 */	*/
respectively.")

(defconst c-comment-leader-regexp "^[ 	]*\\(\\*\\*\\|\\*\\)?[ ]?"
  "Regexp used to match C comment leaders.")

(defvar c-comment-edit-mode 'text-mode
  "*Major mode used by `c-comment-edit' when editing C comments.")

(defvar c-comment-edit-hook nil
  "*Function to call whenever `c-comment-edit' is used.
The function is called just before the `c-comment-edit' function allows you to
begin editing the comment.")

(defvar c-comment-edit-buffer-alist nil
  "Assoc list of C buffers and their associated comment buffers.
Elements are of the form (C-BUFFER COMMENT-BUFFER COMMENT-START COMMENT-END)
COMMENT-START and COMMENT-END are markers in the C-BUFFER.")


(defvar c-comment-edit-bname " *C Comment Edit*"
  "*buffer name to edit the comment")


(defvar c-comment-edit-empty-1-line nil
  "*This determines if the first comment line will be left empty

/*
 * comment begin , when value is t
 */
")

(defvar  c-comment-edit-other-buffer t
  "*Set to nil if you want to edit in full buffer")

(defvar c-comment-edit-after-hook nil
  "*Enables you to do some cleanup after edit is done, not called
if user aborted the action.")

(defvar c-comment-edit-end-hook nil
  "*When user has pressed C-c or ESC to complete editing, the
Comment prefix lines are drawn. After it has completed drawing,
and the buffer is in ready to be inserted back, this hook will be called. ")


;;; ....................................................... &v-private ...
;;; These are set by funcs

(defconst c-comment-edit-C-buf nil
  "Buffer that is currently beeing edited. Overwritten in every call.")

(defconst c-comment-edit-bmc nil
  "After the comment is edited, this variable contains
begin MARK of comment.")

(defconst c-comment-edit-emc nil
  "After the comment is edited, this variable contains
end MARK of comment.")

(defconst c-comment-edit-bufc nil
  "After the comment is edited, this variable contains buffer edited.")


;;; ########################################################### &Funcs ###


;;; .......................................................... &macros ...

(defmacro save-point (&rest body)
  "Save value of point, evalutes FORMS and restore value of point.
If the saved value of point is no longer valid go to (point-max).
The variable `save-point' is lambda-bound to the value of point for
the duration of this call."
  (list 'let '((save-point (point)))
	(list 'unwind-protect
	      (cons 'progn body)
	      '(goto-char (min (point-max) save-point)))))

(defmacro marker (pos &optional buffer)
  (list 'set-marker '(make-marker) pos buffer))


;;; ----------------------------------------------------------------------
;;;
(defun c-comment-edit (search-prefix)
  "Edit multi-line C comments.
This command allows the easy editing of a multi-line C comment like this:
   /*
    * ...
    * ...
    */
The comment may be indented or flush with the left margin.

If point is within a comment, that comment is used.  Otherwise the
comment to be edited is found by searching forward from point.

With one \\[universal-argument] searching starts after moving back one
  paragraph.
With two \\[universal-argument]'s searching starts at the beginning of the
  current or proceeding C function.
With three \\[universal-argument]'s searching starts at the beginning of the
  current page.
With four \\[universal-argument]'s searching starts at the beginning of the
  current buffer (clipping restrictions apply).

Once located, the comment is copied into a temporary buffer, the comment
leaders and delimiters are stripped away and the resulting buffer is
selected for editing.  The major mode of this buffer is controlled by
the variable `c-comment-edit-mode'.

Use \\[c-comment-edit-end] when you have finished editing the comment.  The
comment will be inserted into the original buffer with the appropriate
delimiters and indention, replacing the old version of the comment.  If
you don't want your edited version of the comment to replace the
original, use \\[c-comment-edit-abort]."
  (interactive "*P")
  (let ((c-buffer (current-buffer))
	marker tem c-comment-fill-column c-comment-buffer
	c-comment-start c-comment-end
	(inhibit-quit t)
	(bname c-comment-edit-bname)
	(other-bedit c-comment-edit-other-buffer)
	)


    (setq c-comment-edit-C-buf (current-buffer)) ;** set global

    ;; honor search-prefix
    (cond ((equal search-prefix '(4))
	   (backward-paragraph))
	  ((equal search-prefix '(16))
	   (end-of-defun)
	   (beginning-of-defun)
	   (backward-paragraph))
	  ((equal search-prefix '(64))
	   (backward-page))
	  ((equal search-prefix '(256))
	   (goto-char (point-min))))
    (if (and (null search-prefix) (setq tem (within-c-comment-p)))
	(setq c-comment-start (marker (car tem))
	      c-comment-end (marker (cdr tem)))
      (let (start end)
	(condition-case error-data
	    (save-point
	      (search-forward "/*")
	      (setq start (- (point) 2))
	      (search-forward "*/")
	      (setq end (point)))
	  (search-failed (error "No C comment found.")))
	(setq c-comment-start (marker start))
	(setq c-comment-end (marker end))))
    ;; calculate the correct fill-column for the comment
    (setq c-comment-fill-column (- fill-column
				   (save-excursion
				     (goto-char c-comment-start)
				     (current-column))))
    ;; create the comment buffer
    (setq c-comment-buffer
	  (generate-new-buffer (concat (buffer-name) bname)))

    ;; link into the c-comment-edit-buffer-alist
    (setq c-comment-edit-buffer-alist
	  (cons (list (current-buffer) c-comment-buffer
		      c-comment-start c-comment-end)
		c-comment-edit-buffer-alist))
    ;; copy to the comment to the comment-edit buffer
    (copy-to-buffer c-comment-buffer
		    (+ c-comment-start 2) (- c-comment-end 2))
    ;; mark the position of point, relative to the beginning of the
    ;; comment, in the comment buffer.  (iff point is within a comment.)
    (or search-prefix (< (point) c-comment-start)
	(setq marker (marker (+ (- (point) c-comment-start 2) 1)
			     c-comment-buffer)))
    ;; ...............................................................
    ;; select the comment buffer for editing
    (if (null other-bedit)
	(switch-to-buffer c-comment-buffer)
      (switch-to-buffer-other-window c-comment-buffer)
      )


    ;; remove the comment leaders and delimiters
    (goto-char (point-min))
    (while (not (eobp))
      (and (re-search-forward c-comment-leader-regexp nil t)
	   (replace-match "" nil t))
      (forward-line))


    ;; run appropriate major mode
    (funcall (or c-comment-edit-mode 'fundamental-mode))

    ;; override user's default fill-column here since it will lose if
    ;; the comment is indented in the C buffer.
    (setq fill-column c-comment-fill-column)

    ;; delete one leading whitespace char
    (goto-char (point-min))
    (if (looking-at "[ \n\t]")
	(delete-char 1))

    ;; restore cursor if possible
    (goto-char (or marker (point-min)))
    (set-buffer-modified-p nil))


  ;; final admonition
  (kill-all-local-variables)
  (use-local-map  c-com-mode-map)

  ;; run user hook, if present.
  (if c-comment-edit-hook
      (funcall c-comment-edit-hook))

  (message
   (substitute-command-keys
    "Type \\[c-comment-edit-end] to end edit, \\[c-comment-edit-abort] to abort with no change.")))



;;; ----------------------------------------------------------------------
;;;
(defun c-comment-edit-end ()
  "End c-comment-edit.
C comment is replaced by its edited counterpart in the appropriate C buffer.
Indentation will be the same as the original."
  (interactive)
  (let ((tuple (find-c-comment-buffer))
	(line1-empty c-comment-edit-empty-1-line)
	(i 0)
	char-count
	)
    (if (null tuple)
	(error "Not a c-comment-edit buffer."))
    (let ((inhibit-quit t)
	  (c-comment-c-buffer (car tuple))
	  (c-comment-buffer (nth 1 tuple))
	  (c-comment-start (nth 2 tuple))
	  (c-comment-end (nth 3 tuple)))
      (cond
       ((buffer-modified-p)
	;; rebuild the comment
	(goto-char (point-min))

	(if (null line1-empty)
	    (insert "/*")
	  (insert "/*\n"))
	(if (string= c-comment-leader "  ")
	    (while (not (eobp))
	      (setq i (1+ i))
	      (if (eq 1 i)
		  (insert " ")
		(if (not (eolp)) (insert c-comment-leader " ")))
	      (forward-line))

	  (setq i 0)
	  (while (not (eobp))
	    (setq i (1+ i))
	    (if (and (eq 1 i) (null line1-empty))
		(insert " ")
	      (insert c-comment-leader (if (eolp) "" " ")))
	    (forward-line)))
	(if (not (char-equal (preceding-char) ?\n))
	    (insert "\n"))
	(insert (if (string= c-comment-leader " *") " */" "*/"))
	;; indent if necessary
	(let ((indention
	       (save-excursion
		 (set-buffer c-comment-c-buffer)
		 (goto-char c-comment-start)
		 (current-column))))
	  (goto-char (point-min))
	  (cond ((not (zerop indention))
		 ;; first line is already indented
		 ;; in the C buffer
		 (forward-line)
		 (while (not (eobp))
		   (indent-to indention)
		   (forward-line)))))

	(run-hooks 'c-comment-edit-end-hook)
	;; replace the old comment with the new

	(save-excursion
	  (setq char-count (- (point-max) (point-min)) )
	  (set-buffer c-comment-c-buffer)
	  (save-point
	    (save-excursion
	      (delete-region c-comment-start c-comment-end)
	      (goto-char c-comment-start)
	      (set-buffer c-comment-buffer)
	      (append-to-buffer c-comment-c-buffer
				(point-min) (point-max))
	      ))
	  ;;  save values for possible hook function
	  (setq c-comment-edit-bmc c-comment-start
		c-comment-edit-emc (+ c-comment-start char-count)
		c-comment-edit-bufc c-comment-c-buffer)
	  )
	(run-hooks 'c-comment-edit-after-hook)
	)
       ;; .................................................. cond
       (t
	(message "No change.")))

      ;; switch to the C buffer
      (if (get-buffer-window c-comment-c-buffer)
	  (select-window (get-buffer-window c-comment-c-buffer))
	(switch-to-buffer c-comment-c-buffer))
      ;; delete the window viewing the comment buffer
      (and (get-buffer-window c-comment-buffer)
	   (delete-window (get-buffer-window c-comment-buffer)))

      ;; unlink the tuple from c-comment-edit-buffer-alist
      (setq c-comment-edit-buffer-alist
	    (delq tuple c-comment-edit-buffer-alist))
      ;; let Emacs reclaim various resources
      (save-excursion
	(set-buffer c-comment-buffer)
	(set-buffer-modified-p nil)
	(kill-buffer c-comment-buffer)
	)
      (set-marker c-comment-start nil)
      (set-marker c-comment-end nil))
    )
  )


;;; ----------------------------------------------------------------------
;;;
(defun c-comment-edit-abort ()
  "Abort a c-comment-edit with no change."
  (interactive)
  (let* ((tuple (find-c-comment-buffer))
	 (c-comment-c-buffer (car tuple))
	 (c-comment-buffer (nth 1 tuple))
	 (c-comment-start (nth 2 tuple))
	 (c-comment-end (nth 3 tuple)))
    (if (null tuple)
	(error "Not a c-comment-edit buffer."))
    ;; switch to the C buffer
    (if (get-buffer-window c-comment-c-buffer)
	(select-window (get-buffer-window c-comment-c-buffer))
      (switch-to-buffer c-comment-c-buffer))

    (let ((inhibit-quit t))
      (save-excursion
	(set-buffer c-comment-buffer)
	(set-buffer-modified-p nil)
	(if c-comment-edit-other-buffer
	    (delete-window))
	(kill-buffer c-comment-buffer)
	)

      ;; unlink the tuple from c-comment-edit-buffer-alist
      (setq c-comment-edit-buffer-alist
	    (delq tuple c-comment-edit-buffer-alist))
      (set-marker c-comment-start nil)
      (set-marker c-comment-end nil)
      (message "Aborted with no change."))))


;;; ----------------------------------------------------------------------
;;; this loses on /* /* */ but doing it right would be grim.
(defun within-c-comment-p ()
  (condition-case error-data
      (let (start end)
	(save-point
	  (search-backward "/*")
	  (setq start (point))
	  (search-forward "*/")
	  (setq end (point)))
	(if (< (point) end) (cons start end) nil))
    (search-failed nil)))

;;; ----------------------------------------------------------------------
;;;
(defun find-c-comment-buffer (&optional buffer)
  (or buffer (setq buffer (current-buffer)))
  (let ((list c-comment-edit-buffer-alist))
    (catch 'return-value
      (while list
	(if (eq (nth 1 (car list)) buffer)
	    (throw 'return-value (car list))
	  (setq list (cdr list)))))))

;;; ----------------------------------------------------------------------
;;;
(defun find-c-comment-c-buffer (&optional buffer)
  (or buffer (setq buffer (current-buffer)))
  (let ((list c-comment-edit-buffer-alist))
    (catch 'return-value
      (while list
	(if (eq (car (car list)) buffer)
	    (throw 'return-value (car list))
	  (setq list (cdr list)))))))



;;; ......................................................... &example ...
;;; - Here is ready setup, which you could use right away.
;;; - I am used to program all my C/C++ function like this, where the
;;;   header is just before each function:
;;;
;;; /*************************************************************************
;;;  * <PUBLIC> FUNCTION: MyFunc
;;;  *************************************************************************
;;;  * DESCRIPTION
;;;  * - This is function is the main entry point to class myClass.
;;;  *   it handles reading the oracle database....
;;;  *
;;;  * SPECIAL
;;;  * - Note, that the oracle connection must be verified before your're
;;;  *   using this function....
;;;  *
;;;  * RETURNS
;;;  * - Creates object errorAtom which hold data about the promlem occurred.
;;;  *   other
;;;  *************************************************************************/
;;; errorAtom_c *myClass_c::Execute(char * ptr)
;;; {
;;;
;;; }
;;;
;;;
;;; - In order to maintain the '*****' breaks correctly you have to use some
;;;   cleanup function like one ebove. It detects if the Comment has
;;;   '****' in it and does nothing if it's regular comment.
;;;
;;; - Remember that when the comment has been edited, the comment style you
;;;   choosed, affects the function. This supposes you have use the 'one star'
;;;   style.
;;; - just remove ';;* ' prefix to get the ready function for you.


;;* ;;  Setting proper hooks.
;;* ;;  I seldom need M-c (capitalize word) in C/C++
;;* (setq c++-mode-hook  'c++-my-hook)
;;* (defun c++-my-hook ()
;;*   (local-set-key "\M-c" 'c-comment-edit))
;;*
;;* (setq c-mode-hook  'c-my-hook)
;;* (defun c-my-hook ()
;;*   (local-set-key "\M-c" 'c-comment-edit))


;;* (setq c-comment-edit-end-hook 'my-com-func)
;;* (defun my-com-func ()
;;*   "C- comment edit cleanup."
;;*   (let* ((sep (make-string 75 ?* ))	;what separator you want to use
;;* 	 (fix-re   "[-=*] [-=*][-=*]" )	;the gap " " is in buffer
;;* 	 (back-step 3)			;depends on the fix-re
;;* 	 (break-re " [-=*][-=*][-=*]*")	;at least 3 continuour chars
;;* 	 )
;;*     ;;  - We are in comment buffer now, so we can move freely with goto-char
;;*     ;;  - fix all break-marked lines to certain length
;;*     ;;
;;*     (goto-char (point-min))
;;*     (while (re-search-forward fix-re nil t)
;;*       (backward-char back-step)  (kill-line) (insert sep))
;;*
;;*     ;;  - Check if the last line has separator == it is function header
;;*     ;;  - The last line holds "*/", so look at the previous one.
;;*     (goto-char (point-max))    (forward-line -1)
;;*
;;*     (if (null (looking-at break-re)) nil
;;*       ;; Remove that lonely "*/" and shift it one line up
;;*       (goto-char (point-max)) (beginning-of-line)
;;*       (kill-line)
;;*       (backward-delete-char 1)
;;*       (insert "/")			;terminate C comment
;;*       )
;;*     nil					;hook must return this
;;*     ))



(provide 'c-comment-edit)
;;;; End of file

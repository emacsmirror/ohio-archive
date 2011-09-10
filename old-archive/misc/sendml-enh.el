;From: nate@hobbes.intel.com (Nate Hess)
;Newsgroups: gnu.emacs.bug
;Subject: sendmail.el enhancements
;Message-ID: <633@mipos3.intel.com>
;Date: 6 Aug 89 20:13:13 GMT
;Reply-To: woodstock@hobbes.intel.com (Nate Hess)
;Organization: Intel Corporation, Santa Clara, CA
;Lines: 53
;Summary: New ways to move to header fields and to the message
;
;I use the C-c C-f C-[ts] commands in Mail Mode quite often, and I added
;a feature that has proven to be rather useful.  If you give either of
;these a prefix argument, they will now move to the desired field,
;erasing whatever was previously there.
;
;--------------- Replacements for functions in sendmail.el ----------
(defun mail-to (&optional front)
  "Move point to end of To-field.
With prefix arg, kill all users in To-field."
  (interactive "p")
  (expand-abbrev)
  (mail-position-on-field "To")
  (if (> front 1)
      (progn
	(beginning-of-line)
	(search-forward ":")
	(forward-char 1)
	(kill-line nil))))

(defun mail-subject (&optional front)
  "Move point to end of Subject-field.
With prefix arg, kill the current Subject."
  (interactive "p")
  (expand-abbrev)
  (mail-position-on-field "Subject")
  (if (> front 1)
      (progn
	(beginning-of-line)
	(search-forward ":")
	(forward-char 1)
	(kill-line nil))))
;--------------- Replacements for functions in sendmail.el ----------
;
;
;I also found this function to be rather useful.  It moves to the first
;line after the mail-header-separator.  I bind it to C-c C-f C-m.
;
;--------------- New function for sendmail.el ----------
(defun mail-message ()
  "Move point to beginning of text field."
  (interactive)
  (goto-char (point-min))
  (search-forward (concat "\n" mail-header-separator "\n")))
;--------------- New function for sendmail.el ----------
;
;
;I hope these prove useful for others...
;--woodstock
;-- 
;	   "What I like is when you're looking and thinking and looking
;	   and thinking...and suddenly you wake up."   - Hobbes
;
;woodstock@hobbes.intel.com   ...!{decwrl|hplabs!oliveb}!intelca!mipos3!nate 

;Date: Thu, 16 Mar 89 10:14:40 PST
;From: mrspoc!guinan!kayvan@apple.com
;To: ho@la.tis.com
;Cc: info-gnu-emacs@prep.ai.mit.edu
;Subject: Multiple shells
;
;> Date: Wed, 15 Mar 89 23:13:40 PST
;> From: apple!la.tis.com!ho (Hilarie K. Orman)
;> 
;> It is relatively easy to hack shell.el to allow multiple shell
;> processes, each in its own buffer.
;
;I got tired of people hacking various shell.el's to put in multiple
;shell buffers. So I wrote my own function (which I bind to C-M-r)
;to automatically rename the current buffer. It is appended to the
;end of this message.
;
;This function creates names like "1:*shell*" "2:*shell*", etc., making
;it easy to switch-buffer with completion.
;
;> This largely eliminates the need
;> for job control, and it is an excellent way of organizing one's work.
;> I normally work with 2-5 shells at a time.  Does anyone else work this
;> way?
;
;Yes. I love it. Once inside emacs I never have to leave. If I need to do
;a screen-oriented thing, I just drop to a subshell (C-z on SysV) and
;do it and return back to my emacs.
;
;> The only major problem I have had with shell windows is that
;> passwords from telnet and rlogins and su's are visible, and I finally
;> had to resort to implementing a command to grab and stuff without
;> echoing. 
;
;Try using the new cmushell package that came out recently.
;It does filename completions, among other nifty things as well.
;My cmushell-load-hook for it is set in my .emacs as follows:
;
;(setq cmushell-load-hook
;      '((lambda () (define-key cmushell-mode-map "\C-c\C-i" 'send-invisible))))
;
;This sets the key-esquence C-c C-i (i for invisible) to the send-invisible
;function which grabs text invisibly. This means that after typing su in
;my "1:*cmushell*" buffer, I type C-c C-i before typing in the password.
;
;Anyways, here is the rename function.
;
;			---Kayvan
;
;---------------------------- cut here -----------------------------------
;
; rename.el - Functions to rename buffers.
;
; This is especially useful for shell process buffers that need to be
; renamed for multiple buffers to exist.
;
; Author: Kayvan Sylvan
;

(defvar rename-format "%d:%s"
  "*Format string that rename-to-new-name uses to generate new name.
The number comes first so that the least substring matches buffer names.")

(defun rename-to-new-name ()
  "Rename current buffer to new name not already taken. The new buffer names
are in the format specified by rename-format (using old name and a number).
This function is useful for creating multiple shell process buffers"
  (interactive)
  (let (new-name (old-name (buffer-name (current-buffer))) (i 1))
    (while (get-buffer (setq new-name (format rename-format i old-name)))
      (setq i (1+ i)))
    (rename-buffer new-name)
    (set-buffer-modified-p (buffer-modified-p)))) ; force mode line update

;Kayvan Sylvan @ Transact Software, Inc. -*-  Mountain View, CA (415) 961-6112
;Internet: mrspoc!kayvan@apple.com UUCP: ...!{apple,pyramid,mips}!mrspoc!kayvan


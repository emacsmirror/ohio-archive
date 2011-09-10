;From: ecb@utrccm (ecb)
;Newsgroups: comp.emacs
;Subject: Buffer-menu kill all files (SOLUTION)
;Message-ID: <8908161549.AA01367@utrccm.SMC.UTC.COM>
;Date: 16 Aug 89 15:49:28 GMT
;References: <679@megatek.UUCP>
;Organization: BBN news/mail gateway
;Lines: 77
;
;
;on 9 Aug 89 15:07:44 GMT,
;Dion Hollenbeck <hp-sdd!megatek!eta!hollen@hplabs.hp.COM> said:
;    Dion> Sender: arpa-unix-emacs-request@bbn.COM References:
;    Dion> <661@mipos3.intel.com> Source-Info: From (or Sender) name
;    Dion> not authenticated.
;
;    Dion> Thanks to all who responded to my question.  With help from
;    Dion> them and some more poking around myself, the solution is as
;    Dion> follows for anyone who could benefit by it.  The reason I
;    Dion> needed this code is that I have been using tags-search and
;    Dion> after a search I sometimes end up with up to 60 or so files
;    Dion> being edited by Emacs.  This is a quick way to kill all of
;    Dion> them.  When the function finishes, the point is at the
;    Dion> bottom of the buffer list window and I can merely move up
;    Dion> the the first file I want to keep, use "u" on all my regular
;    Dion> files to be kept and then "x" to delete all the ones used in
;    Dion> tags-search.
;
;Good idea. I've had this problem myself. 
;
;I've added an inverse of the function you posted (for when fat fingers
;or brain-dead key-banging might cause me grief) and offer it here.
;
;Also I prefer not to have modified buffers be marked for deletion by
;any automatic process (too much chance that I'll purge useful changes)
;so I've modified the original code to check and leave such buffers
;alone.  A better solution would be to mark, in some easily recognized
;way, all buffers added by a tags search, but this is beyond my skills.
;Anyone else care to try?
;
;			Bud Boman
;			United Technologies Research Center
;			(203) 727-7128
;			ecb@utrccm.smc.utc.com
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Add key to buffer menu mode
(defun buffer-menu-mode-hook-fun ()
    "Add key mapping for Buffer-menu-mark-all-delete function"
  (define-key Buffer-menu-mode-map "a" 'Buffer-menu-mark-all-delete)
  (define-key Buffer-menu-mode-map "U" 'Buffer-menu-unmark-all-delete)
  (use-local-map Buffer-menu-mode-map)
)

;;  Add function name to mode hook
(setq buffer-menu-mode-hook 'buffer-menu-mode-hook-fun)

;;  Define additional function for buffer menu mode
(defun Buffer-menu-mark-all-delete ()
  "Mark all buffers to be deleted by \\[Buffer-menu-execute] command.
    Finish at the end of the buffer menu."
  (interactive)
  (goto-char (point-min))
  (while (looking-at " [-M]") (forward-line 1))
  (while (looking-at "[ .D]")
    (if (or (looking-at " \\*") (looking-at ".\\*") )
        (forward-line 1) (Buffer-menu-delete)
        )
    )
  (goto-char (point-min) )
  (next-line 2)
)

(defun Buffer-menu-unmark-all-delete ()
  "Mark all buffers to be deleted by \\[Buffer-menu-execute] command.
    Finish at the end of the buffer menu."
  (interactive)
  (goto-char (point-min))
  (while (or (looking-at " [-M]") (looking-at "\\.") ) (forward-line 1))
  (while (looking-at "[ D]") 
    (Buffer-menu-unmark)
  )
  (goto-char (point-min) )
  (next-line 2)
)

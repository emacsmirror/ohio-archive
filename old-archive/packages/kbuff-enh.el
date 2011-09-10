;From ark1!nems!mimsy!haven!purdue!tut.cis.ohio-state.edu!cs.utexas.edu!uwm.edu!rpi!netserv2!deven Wed Dec  6 12:18:06 1989
;Article 778 of gnu.emacs:
;Path: ark1!nems!mimsy!haven!purdue!tut.cis.ohio-state.edu!cs.utexas.edu!uwm.edu!rpi!netserv2!deven
;From deven@rpi.edu (Deven T. Corzine)
;Newsgroups: gnu.emacs
;Subject: kill-buffer.el package (deals with running processes)
;Message-ID: <DEVEN.89Nov26155742@netserv2.rpi.edu>
;Date: 26 Nov 89 20:57:01 GMT
;References: <MRD.89Nov15011910@image.clarkson.edu>
;Distribution: gnu
;Organization: Rensselaer Polytechnic Institute, Troy, NY
;Lines: 148
;
;
;On 15 Nov 89 06:19:15 GMT, mrd@sun.soe.clarkson.edu (Michael DeCorte) said:
;
;Michael> Is there some way to get emacs to say 'hey you sure you want
;Michael> to do this?' when I try to kill a buffer with a process
;Michael> accociated with it (eg. *shell*).
;
;As it happens, I have written a kill-buffer package to deal with this
;exact problem.  It replaces the original kill-buffer (saving the subr
;definition, of course) and if there is a process running in a buffer
;which is being killed, it asks the user if to kill the process, and
;also if to save the file, (y-or-n-p's) and if buffer is still unsaved
;or has running processes, yes-or-no-p on whether to really kill it.
;
;I have been using it without problems since making a few bug fixes and
;enhancements...  been intending to post it, just prompted to by seeing
;your message...
;
;Enjoy.  (watch for .sig)
;
;Deven

;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!rpi!netserv2!deven Wed Dec  6 12:20:14 1989
;Article 823 of gnu.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!rpi!netserv2!deven
;From deven@rpi.edu (Deven T. Corzine)
;Newsgroups: gnu.emacs
;Subject: kill-buffer.el update
;Message-ID: <DEVEN.89Dec5194359@daniel.rpi.edu>
;Date: 6 Dec 89 00:43:25 GMT
;Distribution: gnu
;Organization: Rensselaer Polytechnic Institute, Troy, NY
;Lines: 160
;
;
;Okay, so maybe it wasn't completely debugged.  This version should be
;a bit cleaner...  (watch for .sig!)

;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; kill-buffer.el --- enhancements to kill-buffer function
;; Author          : Deven Corzine
;; Created On      : Fri Oct 20 09:01:39 1989
;; Last Modified By: Deven Corzine
;; Last Modified On: Tue Dec  5 18:46:56 1989
;; Update Count    : 15
;; Status          : Complete, no known bugs.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; History 		
;; 5-Dec-1989		Deven Corzine	
;;    Last Modified: Sun Nov 26 16:17:45 1989 #14 (Deven Corzine)
;;    Modified to recursively call kill-buffer instead of spacial-casing
;;      when passed t to kill current buffer.
;;    Modified back to not erase buffer, and to reset buffer-modified-p
;;      to original state when a buffer is not actually killed.
;;    Modified to correctly state whether buffer was killed or not.

;; 26-Nov-1989		Deven Corzine	
;;    Last Modified: Thu Nov  2 11:57:50 1989 #11 (Deven Corzine)
;;    Modified to erase buffer before killing.  (so killing *scratch*
;;      when only buffer will (effectively) kill it.)

;; 2-Nov-1989		Deven Corzine	
;;    Last Modified: Mon Oct 30 10:34:54 1989 #10 (Deven Corzine)
;;    Modified to work properly for already-killed buffers.
;;    Modified to catch errors and call original-kill-buffer instead
;;      of breaking.

;; 30-Oct-1989		Deven Corzine	
;;    Last Modified: Mon Oct 30 09:57:42 1989 #6 (Deven Corzine)
;;    Modified to print a message when buffer is killed interactively.
;;    Fixed typo in format string when buffer needs to be saved but is
;;      not bound to a file.

;; 20-Oct-1989		Deven Corzine	
;;    Last Modified: Fri Oct 20 10:40:17 1989 #3 (Deven Corzine)
;;    Changed wording on buffer kill confirmation for when a process
;;      remains running to make it clear the question refers to killing
;;      the buffer, not the process.

;; 20-Oct-1989		Deven Corzine	
;;    Last Modified: Fri Oct 20 10:17:40 1989 #2 (Deven Corzine)
;;    Changed wording on process kill query to be less ambiguous.

;; 20-Oct-1989		Deven Corzine	
;;    Last Modified: Fri Oct 20 09:19:48 1989 #1 (Deven Corzine)
;;    Modified to honor buffer-offer-save and check for abbrevs to save.

;; 20-Oct-1989		Deven Corzine	
;;     #0
;;    Wrote new kill-buffer function to check for processes, and to ask
;;      whether to save buffer (if bound to a file and modified) and to
;;      ask whether to kill each process running in the buffer.
;;    Companion function kill-current-buffer for a convenient keybinding.

;; save original kill-buffer primitive
(or (fboundp 'original-kill-buffer)
    (fset 'original-kill-buffer
          (symbol-function 'kill-buffer)))

(defun kill-buffer (&optional buffer)
  "One arg, a string or a buffer.  Get rid of the specified buffer.

Modified to check for processes, and offer option of killing running
processes or saving buffer if changed.  If BUFFER is t, then use the
current-buffer.  If nil, (as in interactively) prompt for buffer."
  (interactive)
  (let ((buf (if buffer
                 (if (eq buffer t)
                     (current-buffer)
                   (get-buffer buffer))))
        name mod proc unsaved)
    (if (eq buffer t)
        (let ((name (buffer-name buf)))
          (kill-buffer buf)
          (if (buffer-name buf)
              (message "Buffer %s not killed." name)
            (message "Buffer %s killed." name))))
    (condition-case nil
        (progn
          (setq buf
                (or buf
                    (get-buffer
                     (read-buffer "Kill buffer: " (current-buffer) t))))
          (if (setq name (buffer-name buf))
              (progn
                (setq mod (buffer-modified-p buf))
                (set-buffer buf)
                (setq proc (get-buffer-process buf)
                      unsaved (and (buffer-modified-p buf)
                                   (or (buffer-file-name buf)
                                       (and buffer-offer-save
                                            (> (buffer-size) 0)))))
                (while (and proc
                            (y-or-n-p (format
                                       "Kill process %s running in buffer %s? "
                                       (process-name proc) name)))
                  (condition-case nil
                      (kill-process proc)
                    (error nil))
                  (delete-process proc)
                  (setq proc (get-buffer-process buf)))
                (if (and unsaved
                         (y-or-n-p (if (buffer-file-name buf)
                                       (format "Save file %s? " unsaved)
                                     (format "Save buffer %s? "
                                             name))))
                    (setq unsaved (condition-case nil
                                      (save-buffer buf)
                                    (error t))))
                (if (and save-abbrevs abbrevs-changed
                         (y-or-n-p (format "Save abbrevs in %s? "
                                           abbrev-file-name)))
                    (condition-case nil
                        (write-abbrev-file nil)
                      (error nil)))
                (if (or (not (or proc unsaved))
                        (yes-or-no-p
                         (format "%s%s%s%s; kill anyway? "
                                 (if proc "Process(es) " "Buffer ")
                                 (if proc "" name)
                                 (if proc
                                     (if unsaved
                                         "running in modified buffer "
                                       "running in buffer ")
                                   " modified")
                                 (if proc name ""))))
                    (progn
                      (set-buffer-modified-p nil)
                      (original-kill-buffer buf)
                      (if (buffer-name buf)
                          (progn
                            (set-buffer buf)
                            (set-buffer-modified-p mod)))
                      (if (interactive-p)
                          (if (not (buffer-name buf))
                              (message "Buffer %s killed." name)
                            (message "Buffer %s not killed." name))))))))
      (error (original-kill-buffer buffer)))))

(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer t))


;Cheers!
;
;Deven
;-- 
;Deven T. Corzine        Internet:  deven@rpi.edu, shadow@pawl.rpi.edu
;Snail:  2151 12th St. Apt. 4, Troy, NY 12180   Phone:  (518) 274-0327
;Bitnet:  deven@rpitsmts, userfxb6@rpitsmts     UUCP:  uunet!rpi!deven
;Simple things should be simple and complex things should be possible.

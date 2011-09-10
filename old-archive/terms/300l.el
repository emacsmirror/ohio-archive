;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!van-bc!ubc-cs!uw-beaver!Teknowledge.COM!unix!hplabs!hpfcso!hpfcmgw!hpfcse!hpuecoz!hpuecoa!clem!jon Wed Feb 14 08:55:44 1990
;Article 1388 of comp.emacs:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!van-bc!ubc-cs!uw-beaver!Teknowledge.COM!unix!hplabs!hpfcso!hpfcmgw!hpfcse!hpuecoz!hpuecoa!clem!jon
;>From jon@clem.UUCP (Jon Trulson)
;Newsgroups: comp.emacs
;Subject: Re: using HP function keys with emacs & HP-UX
;Message-ID: <1510001@clem.UUCP>
;Date: 11 Feb 90 05:14:50 GMT
;References: <1461@ursa-major.SPDCC.COM>
;Organization: Metropolitan State College
;Lines: 238
;
;
;
;
;	Just so happens I do, (if it ever gets out) The following is a
;copy of my 300l.el (hp9854a) that I obtained from a friend.  Here's my
;system info: HP9000/370 HP-UX 6.5 running gnuemacs 18.55.
;
;
;				Jon Trulson - MSC
;
;------------------ Cut Here  ----------------------

;;
;;    To Anyone Who Has Figured Out how to get the `Extend char' key on    
;;    their HP2392  to act as the Meta key, using GNU emacs 18.50 on an HP
;;    300 series machine (a > 350, to be exact): Help!  
;;    
;;    it is my understanding that the extend char key does not just set the
;;    top bit of the key that you hit, it accesses an alternate character
;;    set.  some keys, like "z" dont seem to have an extended char.  if you
;;    do find a way of doing this, PLEASE let me know...  it would be most
;;    useful.
;;    
;;       Also, while I'm here: if you have been able to do the above, you
;;    are also probably smart enough to have made a default.el which sets up
;;    all the special keys to the right of the main keyboard to be useful
;;    (i.e. `Clear display', `Prev', the arrow keys, etc).
;;    
;;    
;;    here is what we use for our lisp/term/hp2392.el file, minus some local
;;    junk that you probably dont want anyway...  (yes, i know, this should
;;    probably use the keypad stuff that has already been set up, but i had
;;    this written before i knew that it existed.  if someone has a cleaner
;;    hack, i would be interested in that too.)
;;    
;;                                                                          
;;
;; cursor pad
;;

(global-set-key "\eA" 'previous-line)   ; up arrow
(global-set-key "\eB" 'next-line)       ; down arrow
(global-set-key "\eC" 'forward-char)    ; right arrow
(global-set-key "\eD" 'backward-char)   ; left arrow
(global-set-key "\eh" 'home)            ; home to the top of the window


(global-set-key "\eS" 'beginning-of-buffer); shift up arrow - top of file
(global-set-key "\eT" 'end-of-buffer)   ; shift down arrow - end of file
(global-set-key "\eR" 'end-of-line)   ; shift right arrow - end of line
(global-set-key "\eL" 'beginning-of-line); shift left arrow - beg of line
(global-set-key "\eF" 'home-bottom)     ; shift home to bottom of window


;;
;; named keys
;;

(global-set-key "\eK" 'clear-end-of-line) ; clear line
(global-set-key "\eJ" 'hide-window)     ; clear display
(global-set-key "\eL" 'insert-line)     ; insert line
(global-set-key "\eM" 'delete-line)     ; delete line
(global-set-key "\eQ" 'overwrite-mode)  ; insert char
(global-set-key "\eP" 'delete-char)     ; delete char
(global-set-key "\eV" 'scroll-down)     ; prev screen
(global-set-key "\eU" 'scroll-up)       ; next screen


(global-set-key "\e&a0C" 'beginning-of-line) ; from shift clear line & display
(global-set-key "\eG" 'beginning-of-line) ; from shift clear line & display

;;(global-set-key "\eK" 'clear-end-of-line) ; shift clear line
;;(global-set-key "\eJ" 'delete-window) ; shift clear display
;;(global-set-key "\eL" 'insert-line)   ; shift insert line
;;(global-set-key "\eM" 'delete-line)   ; shift delete line
(global-set-key "\eN" 'overwrite-mode)  ; shift insert char
(global-set-key "\eO" 'delete-char)     ; shift delete char
(global-set-key "\e&r-1H" 'scroll-down) ; shift prev
(global-set-key "\e&r+1H" 'scroll-up)   ; shift next



;;
;; function definitions for the cursor and named keys
;;

(defun home ()
  "Home to the upper left of the window."
  (interactive)

  (move-to-window-line 0)
  (move-to-column (window-hscroll (selected-window)))
  (setq temporary-goal-column (window-hscroll (selected-window)))
  )


(defun home-bottom ()
  "Home to the lower right of the window."
  (interactive)

  (move-to-window-line -1)
  (move-to-column (window-hscroll (selected-window)))
  (setq temporary-goal-column (window-hscroll (selected-window)))
  )


(defun insert-line ()
  "Insert a line before the current line."
  (interactive)

  (let ((org-point (point))
        )
    (condition-case conditions
        (progn
          (beginning-of-line)
          (open-line 1)
          )
      (error (goto-char org-point)
             (signal (car conditions) (cdr conditions))
             )
      )
    )
  
  )


(defun delete-line ()
  "Delete the current line."
  (interactive)

  (let ((org-point (point))
        )
    (condition-case conditions
        (kill-region (progn
                       (beginning-of-line)
                       (point)
                       )
                     (progn
                       (end-of-line)
                       (if (eobp)
                           (point)
                         (forward-char)
                         (point)
                         )
                       )
                     )
      (error (goto-char org-point)
             (signal (car conditions) (cdr conditions))
             )
      )
    )

  )


(defun clear-end-of-line ()
  "Clear to the end of the line."
  (interactive)

  (let ((org-point (point))
        )
    (condition-case conditions
        (kill-region (point)
                     (progn
                       (end-of-line)
                       (point)
                       )
                     )
      (error (goto-char org-point)
             (signal (car conditions) (cdr conditions))
             )
      )
    )

  )


(defun hide-window ()
  "Hide the current window from view."
  (interactive)

  (if (one-window-p t)
      (bury-buffer)
    (delete-window)
    )
  )


;;
;; set up dired keys
;;

;;(define-key dired-mode-map "\eA" 'dired-previous-line)
;;(define-key dired-mode-map "\eB" 'dired-next-line)


;;
;; set up the user function keys
;; Note: I unset the keys before I re-defined them since some of
;;    the characters sent by the function keys are used by gnuemacs
;;    for other things.  Note, you can also define the funtion keys
;;    themselves to return other than their default values..

(global-unset-key "\ep")
(global-set-key "\ep\r" 'save-buffers-kill-emacs)   ; f1

(global-unset-key "\eq")
(global-set-key "\eq\r" 'set-mark-command)                 ; f2

(global-unset-key "\er")
(global-set-key "\er\r" 'kill-region)               ; f3

(global-unset-key "\es")
(global-set-key "\es\r" 'help-for-help)             ; f4

(global-unset-key "\et")
(global-set-key "\et\r" 'Control-X-prefix)          ; f5

(global-unset-key "\eu")
(global-set-key "\eu\r" 'execute-extended-command)  ; f6

(global-unset-key "\ev")
(global-set-key "\ev\r" 'list-buffers)              ; f7

(global-unset-key "\ew")
(global-set-key "\ew\r" 'other-window)              ; f8

;;; HP terminals usually encourage using ^H as the rubout character

(let ((the-table (make-string 128 0)))
  (let ((i 0))
    (while (< i 128)
      (aset the-table i i)
      (setq i (1+ i))))
  ;; Swap ^H and DEL
  (aset the-table ?\177 ?\^h)
  (aset the-table ?\^h ?\177)
  (setq keyboard-translate-table the-table))



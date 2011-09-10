; Subject: Re: Bugs and gripes
; Date: Thu, 27 Sep 90 17:29:06 -0000
; From: Colas Nahaboo <colas@avahi.inria.fr>
; 
; > Mouse should abort isearch:
; >    Is there a way to make mouse functions abort isearch?  It is sometimes
; >    bothersome to do a bunch of mouse functions (cut, paste, set-point),
; >    then type something only to realize that an isearch is still in
; >    progress!  The same is true of electric functions.
; >    Electric-buffer-menu-mode provides a catch that can be used with a local
; >    mouse map, but mousing in other screens or windows leads to the same
; >    problem as with isearch.  Maybe the proper way to fix these things is to
; >    modify the elisp to temporarily install its own mouse handler.
; 
; I ise x-mouse-set-point (defined below) which fixes the problem.
; 
; Also, I have added (epoch::redisplay-screen) on all mouse functions to
; "fix" the delayed display bugs
; 
; The full contents of my epoch-profile.el file follows, if you want more
; goodies
; 
; This is a .emacs file for initializing gnuemacs.  Customize however
; you like

;; target-buffer is used by random bits of the epoch code.

(setq target-buffer (get-buffer "*scratch*"))

;; live dangerously

(put 'eval-expression 'disabled nil)

;; because I have so many screens open all the time, and idle C-xC-c can
;; really screw things up for me.  So make sure we dont exit without
;; confirmation, and rebind C-xC-c appropriately

(defun exit-emacs-with-confirm ()
  (interactive)
  (if (yes-or-no-p "Do you want to exit ")
      (save-buffers-kill-emacs)))

(global-set-key "\C-x\C-c" 'exit-emacs-with-confirm)

;(setq exec-path (append '("/usr/local/lib/epoch/etc/") exec-path))
;(setq exec-directory "/usr/local/lib/epoch/etc/")

;; load epoch files

    (load "epoch-util" () t)
    (load "epoch" () t)
    (load "button" () t)
    (load "mouse" () t)
    (load "motion" () t)
    (load "property" () t)
    (load "message" () t)
    (load "server" () t)
;(load "maintain-screen-titles" () t)

(setq epoch::buttons-modify-buffer nil)

(setq auto-raise-screen ())
(setq include-system-name ())

;; mouse bindings
(defun x-scroll-region (arg)
  "Scroll window by ARG lines, up or down.
 ARG is line-count between cursor and mouse."
  (select-screen (nth 3 arg))
  (select-window (nth 2 arg))
  (if (> (dot) (car arg))
      (scroll-up (- (count-lines (car arg) (dot))
                   (if (bolp) 0 1)))
    (let ((opoint (dot)))
      (goto-char (car arg))
      (scroll-down (- (count-lines opoint (dot))
                     (if (bolp) 0 1)))
      (goto-char opoint))))

(defun x-select-word (arg)
  "Select word under cursor"
  ;;  (epoch::set-motion-hints ())
  (save-window-excursion
    (let (opoint beg end)
      (mouse::set-point arg)
      (forward-char)
      (forward-word -1)
      (setq opoint (point))
      (forward-word 1)
      (epoch::delete-button drag-button)
      (set-marker mouse-down-marker opoint)
      (setq beg (min opoint (point))
           end (max opoint (point)))
      (setq drag-button (epoch::add-button beg end 1 nil))
      (epoch::redisplay-screen)
      (epoch::store-cut-buffer (buffer-substring beg end))
      (copy-region-as-kill beg end)
      )))

(defun x-cut-text (arg)
  "selects text and wipe it"
  (select-screen (nth 3 arg))
  (select-window (nth 2 arg))
  (epoch::store-cut-buffer (buffer-substring (dot) (car arg)))
  (kill-region (dot) (car arg)))

(defun x-mouse-set-mark (arg)
  "puts mark at mouse cursor"
  (select-screen (nth 3 arg))
  (select-window (nth 2 arg))
  (set-mark (dot))
  (goto-char (car arg))
  (exchange-point-and-mark))

(defun x-mouse-set-region (arg)
  "selects text, then makes it the current region"
  (extend-mouse-drag arg)
  (end-mouse-drag arg)
  (set-mark (dot))
  (goto-char (car arg))
  (copy-region-as-kill))

(defun x-mouse-paste (arg)
  "paste, moves cursor there, and redraws due to a bug in 3.2b"
  (select-screen (nth 3 arg))
  (select-window (nth 2 arg))
  (goto-char (car arg))
  (insert (epoch::get-cut-buffer))
  (undo-boundary)
  (epoch::redisplay-screen))

(defun x-mouse-paste-no-move (arg)
  "paste, moves cursor there, and redraws due to a bug in 3.2b"
  (mouse::paste-cut-buffer arg)
  (epoch::redisplay-screen))

(defun x-mouse-set-point (arg)
  "sets point but exit from incremental search"
  (start-mouse-drag arg)
  (abort-isearch)
  (epoch::redisplay-screen))

                                       ; mouse key bindings

(global-set-mouse mouse-left mouse-down  'x-mouse-set-point)
(global-set-mouse mouse-middle mouse-down  'x-mouse-paste)

(global-set-mouse mouse-left  mouse-shift  'x-scroll-region)
(global-set-mouse mouse-middle mouse-shift 'x-mouse-set-mark)
(global-set-mouse mouse-right mouse-shift 'x-mouse-set-region)

(global-set-mouse mouse-left  mouse-control     'x-select-word)
(global-set-mouse mouse-middle mouse-control    'x-mouse-paste-no-move)
(global-set-mouse mouse-right mouse-control  'x-cut-text)

(global-set-mouse mouse-left mouse-control-shift 'x-mouse-set-mark)

;; \M-\C-y yanks the contents of the cut buffer

(defun yank-cut-buffer ()
  "yanks the contents of the cut buffer at the text cursor position."
  (interactive)
  (insert (epoch::get-cut-buffer)))
(global-set-key "\M-\C-y" 'yank-cut-buffer)

;(display-time)

; Wakes up epochs event handler and adminsters caffeine.
; Ripped bodily out of epoch.el: epoch 3.1 only.
;    Simon Spero, zmacx07@cc.ic.ac.uk
;
(defun epoch::oh-no-epochs-buggered-again ()
  (interactive)
  (epoch::map-screen (epoch::minibuf-screen))
  )


(global-set-key "\C-z=" 'epoch::oh-no-epochs-buggered-again)

;; murthy@cs.cornell.edu (Chet Murthy): exit from isearch

(defun abort-isearch ()
  (condition-case err
         (throw 'search-done t)
  (no-catch nil)))

(defun x (key param)
  "sets various X parameters of current screen"
  (interactive "cSet what? Font, Back, Text, Cursor: \nsset to: ")
  (cond ((char-equal ?f key) (epoch::font param))
       ((char-equal ?c key) (epoch::cursor-glyph (string-to-int param)))
       ((char-equal ?b key) (epoch::background param))
       ((char-equal ?t key) (epoch::foreground param)))
  (recenter))

(defun rename-screen (&optional name)
  (interactive)
  (let ((curr-scr-name (epoch::get-property "WM_NAME")))
    (if (not (or (string= curr-scr-name "Mh-Rmail")
                (string= curr-scr-name "Calendar")))
       (epoch::set-property "WM_NAME" (or name (concat (buffer-name)
                                                    (sys-name)))))))

(defun iconify-screen-and-set-icon (&optional arg)
  (interactive)
  (epoch::set-property "WM_ICON_NAME" (epoch::get-property "WM_NAME"))
  (epoch::iconify-screen arg))

(global-set-key "\C-zi" 'iconify-screen-and-set-icon)

;From ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!mips!bridge2!mdb Thu Feb  1 18:45:24 EST 1990
;Article 668 of gnu.emacs.bug:
;Path: ark1!uakari.primate.wisc.edu!zaphod.mps.ohio-state.edu!mips!bridge2!mdb
;>From: mdb@ESD.3Com.COM (Mark D. Baushke)
;Newsgroups: gnu.emacs.bug
;Subject: Re: dired-listing-switches
;Message-ID: <MDB.90Feb1143533@kosciusko.ESD.3Com.COM>
;Date: 1 Feb 90 22:35:33 GMT
;References: <9001301547.AA10682@wpmstr.wp>
;Sender: news@bridge2.ESD.3Com.COM
;Distribution: gnu
;Organization: 3Com Corp., Mountain View, CA.
;Lines: 177
;In-reply-to: wpmstr!fpb@sunpitt.east.sun.com's message of 30 Jan 90 15:47:06 GMT
;
;[I tried to reply directly, but the mail bounced from
;sunpitt.East.Sun.COM with an 'Unknown host : wmpstr' -- mdb]
;
;On 30 Jan 90 15:47:06 GMT, wpmstr!fpb@sunpitt.east.sun.com (Frank P. Bresz) said:
;
;Frank> In GNU Emacs 18.52.3 of Tue Apr 11 1989 on omaha1 (berkeley-unix)
;
;Frank> Shouldn't this variable be buffer local?
;
;Feel free to hack your environment to your own taste. I am including
;the hacks that I use at the end of this messge.
;
;Frank> Often times I want to look at a certain directory sorted by
;Frank> time i.e.  "-alt" instead of the default "-al" setting of this
;Frank> variable.  But this doesn't mean I want to look at all
;Frank> directories this way.
;
;Frank> Many variables state that they automatically become bufer local
;Frank> when set in any way.  How can I make dired-listing-switches be
;Frank> one of them.
;
;I use the following in my ~/.emacs (I am including
;my-dired-mode-hook.el after my .signature):
;
;	(setq dired-mode-hook '(my-dired-mode-hook))
;	(autoload 'my-dired-mode-hook "my-dired-mode-hook"
;		  "Add new features to dired." t) 
;
;Then typing an 's' toggles between the two flavors (-al and -alt) for
;a given buffer.
;
;(Suggestions for additions welcome!)
;
;Enjoy!
;-- 
;Mark D. Baushke
;Internet:   mdb@ESD.3Com.COM
;UUCP:	    {3comvax,auspex,sun}!bridge2!mdb



;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my-dired-mode-hook.el --- additons to dired mode
;; 
;; Author          : Mark D. Baushke
;; Created On      : Mon Mar  6 14:09:43 1989
;; Last Modified By: Mark D. Baushke
;; Last Modified On: Tue Aug 29 12:11:40 1989
;; Update Count    : 7
;; Status          : Works for me!
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Ze'ev Wurman <zeev@silvlis.com>, Aug. 88
;; (now at Amdahl -- new e-mail address <zeev@vlss.amdahl.com>)
;;
(defun my-dired-mode-hook()
  (define-key dired-mode-map "a" 'dired-run-command)
  (define-key dired-mode-map "q" 'dired-exit)
  (define-key dired-mode-map "s" 'dired-toggle-sort)
  (define-key dired-mode-map "S" 'dired-byuser-mode)
  (define-key dired-mode-map "D" 'dired-run-backup-diff)
  (define-key dired-mode-map "\C-d" 'dired-run-diff)
  (require 'compile "compile")
  (make-local-variable 'byname-mode)
  (set-default 'byname-mode nil)
  (make-local-variable 'bydate-mode)
  (set-default 'bydate-mode nil)
  (make-local-variable 'byuser-mode)
  (set-default 'byuser-mode nil)
  (make-local-variable 'user-string)
  (set-default 'user-string nil)
  (make-local-variable 'minor-mode-alist)
  (make-local-variable 'dired-listing-switches)
  (setq minor-mode-alist (cons '(bydate-mode " ByDate") minor-mode-alist))
  (setq minor-mode-alist (cons '(byname-mode " ByName") minor-mode-alist))
  (setq minor-mode-alist (cons '(byuser-mode user-string) minor-mode-alist))
  (if (string-equal dired-listing-switches "-alt") (bydate-mode) 
    (if (string-equal dired-listing-switches "-al") (byname-mode) 
    (dired-byuser-mode dired-listing-switches)
    )
  )
)

(defun dired-toggle-sort ()
  "Toggle dired sorting by-name/by-date. Initial setting is based
   on 'dired-listing-switches'. The known values are '-alt' (ByDate)
   and '-al' (ByName). Any other option really belongs to 'dired-byuser-mode'"
  (interactive)
  (if (string-equal dired-listing-switches "-alt") (byname-mode) (bydate-mode))
  (revert-buffer))

(defun bydate-mode()
  (setq dired-listing-switches "-alt")
  (setq bydate-mode t byname-mode nil byuser-mode nil)
  (set-buffer-modified-p (buffer-modified-p)) ;No-op, but updates mode line.
)

(defun byname-mode()
  (setq dired-listing-switches "-al")
  (setq byname-mode t bydate-mode nil byuser-mode nil)
  (set-buffer-modified-p (buffer-modified-p)) ;No-op, but updates mode line.
)

(defun dired-byuser-mode(&optional user-str)
  "Let user specify his dired-listing-switches for dired-mode"
  (interactive)
  (if (not user-str)
    (setq user-string (read-string "ls switches (must contain -l): " 
                                     dired-listing-switches))
    (setq user-string user-str)
  )
  (setq dired-listing-switches user-string)
  (setq user-string (concat " '" user-string "'"))
  (setq byname-mode nil bydate-mode nil byuser-mode t)
  (set-buffer-modified-p (buffer-modified-p)) ;No-op, but updates mode line.
  (revert-buffer)
)

(defun dired-run-backup-diff()
    "Run diff on this file and its backup version"
    (interactive)
    (setq to-file (dired-get-filename))
    (setq to-file1 (car (find-backup-file-name to-file)))
    (if (file-exists-p to-file1) ()
        (setq to-file1 (file-name-sans-versions to-file)))
    (if (and (file-exists-p to-file1) (not (string-equal to-file to-file1))) ()
        (setq to-file1 (read-file-name "diff with file: " to-file to-file 1)))
    (setq command (concat "diff " to-file " " to-file1))
    (compile1 command "No more diff's" "diff")
      (dired-redisplay to-file)
)

(defun dired-run-diff(to-file1)
    "Run diff on this file and another one"
    (interactive "fdiff with file:")
    (setq to-file (dired-get-filename))
    (setq command (concat "diff " to-file " " to-file1))
    (compile1 command "No more diff's" "diff")
      (dired-redisplay to-file)
)
(defun kill-diff ()
  "Kill the process made by the \\[diff] command."
  (interactive)
  (if compilation-process
      (interrupt-process compilation-process)))

;;; command to quit a dired session...
(defun dired-exit ()
  "Quit editing this directory."
  (interactive)
  (kill-buffer (current-buffer)))


;; modified rename
(defun dired-rename-file (to-file)
  "Rename this file to TO-FILE."
  (interactive
   (list (read-file-name (format "Rename %s to: "
				 (file-name-nondirectory (dired-get-filename)))
			 nil (dired-get-filename))))
  (setq to-file (expand-file-name to-file))
  ;; Added this `if' to handle moving a file to another directory
  ;; the original code only contained the `else' clause.
  (if (file-directory-p to-file)
      (rename-file (dired-get-filename)
		   (concat (directory-file-name to-file) "/"
			   (file-name-nondirectory (dired-get-filename))))
    (rename-file (dired-get-filename) to-file))
  (let ((buffer-read-only nil))
    (beginning-of-line)
    (delete-region (point) (progn (forward-line 1) (point)))
    (setq to-file (expand-file-name to-file))
    (dired-add-entry (file-name-directory to-file)
		     (file-name-nondirectory to-file))))



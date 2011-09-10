;From utkcs2!emory!samsung!cs.utexas.edu!uunet!zephyr.ens.tek.com!tektronix!sequent!crg5!niven Wed Jun  6 08:07:16 EDT 1990
;Article 4375 of comp.emacs:
;Path: utkcs2!emory!samsung!cs.utexas.edu!uunet!zephyr.ens.tek.com!tektronix!sequent!crg5!niven
;From: niven@sequent.UUCP (Kevin H. Joyce)
;Newsgroups: comp.emacs
;Subject: mh-e things
;Message-ID: <NIVEN.90Jun5154546@crg2.UUCP>
;Date: 5 Jun 90 22:45:46 GMT
;Sender: root@crg5.UUCP
;Organization: Sequent Computer Systems Inc., Beaverton, OR
;Lines: 99
;
;Here's a little something I wrote for packing folders into a single file.
;The unpack function also unpacks ELM type folders. This is useful if you use 
;the ELM mail filter to funnel stuff into folders.

;;   GNU EMACS, MH-E extension to allow packf and inc -file
;;   This allows large folders to be safely stored as a single file.
;;   Tested with mh 6.6 and gnu 18.55.15, and epoch 3.0.
;;   Enjoy. K. Joyce  10/5/89   rev 1.0
;;                    12/4/89   rev 1.1 added ability to unpack elm folders
;;
(defvar mh-folder-mode-map (make-keymap)
  "Keymap for composing mail.")
(define-key mh-folder-mode-map "\ec" 'mh-copy-folder-to-file)
(define-key mh-folder-mode-map "\ed" 'mh-get-folder-from-file)
(defvar filename nil)
(defun mh-copy-folder-to-file (folder range)
  "Pack messages in the range into a single file whose name is that of the 
   folder with -file tacked on the end. It is located in the user Mail 
   directory. If the file already exists then the messages are simply
   appended.  The messages are then deleted from the folder."
  (interactive (list (mh-prompt-for-folder "copy to file: "
					   mh-current-folder 
					   nil)
		     (read-string "Range [all]? ")))
  (mh-scan-folder folder (if (equal range "") "all" range))
  (message "packing folder into a single file...")
  (setq filename (substring folder 1 nil))
  (setq filename (concat  filename "-file"))
  (setq filename (concat  mh-user-path filename))
  (mh-exec-cmd "packf" (if (equal range "") "all" range) "-file" filename)
  (message "packing folder...done,   deleting messages from folder...")
  (mh-exec-cmd "rmm" folder (if (equal range "") "all" range))
  (setq mh-next-direction 'forward)
  (mh-scan-folder mh-current-folder "all"))
;;
;;
(defun mh-get-folder-from-file (folder)
  "Unpack messages from a file folder-file where folder is the chosen
   folder. The file is included in the folder of the same name. inc has
   a bug that it adds a blank Return-Path: line so if this is in your
   visible-headers make it insist on a char  (\\|Return-Path: [a-zA-Z0-9])
   or something like that after the : space. The file is deleted after being
   unpacked."
  (interactive (list (mh-prompt-for-folder "insert file in folder: " 
					   mh-current-folder 
					   nil)))
  (message "unpacking file into folder...")
  (setq filename (substring folder 1 nil))
  (setq filename (concat  filename "-file"))
  (setq mh-elm-folder-name filename)
  (setq filename (concat  mh-user-path filename))
;;
;; if the file has "from ...blah blah lines, put a blank line before each
;; occurance except the first
;;
  (find-file filename)
  (goto-char (point-min))
  (mh-elm-folder-convert (point-min))
  (save-buffer)
  (kill-buffer mh-elm-folder-name)
;;
;; Now call inc to incorporate the mail..and erase file
;;
  (mh-exec-cmd "inc" folder "-file" filename "-truncate")
  (message "unpacking folder...done")
  (setq mh-next-direction 'forward)
  (mh-scan-folder mh-current-folder "all"))
;;
;;
(defun mh-elm-folder-convert (start)
  ;; Allow an elm type folder to be unpacked into mh compatible
  ;; files. Basically requires searching for the 'from' line at the
  ;; beginning of each piece of mail, and putting a blank line in front of it.
  ;; The first piece of mail cannot have a blank line in front of it however.
  (let ((case-fold-search t))
    (save-restriction
      (goto-char start)
      (goto-char (point-min))
	    (forward-line 2)   ;skip first piece of mail
	       (while (re-search-forward
		    (concat "^[\^_]?\\("
"From [^ \n]*\\(\\|\".*\"[^ \n]*\\)  ?[^ \n]* [^ \n]* *"
"[0-9]* [0-9:]*\\( ?[A-Z]?[A-Z][A-Z]T\\| ?[-+]?[0-9][0-9][0-9][0-9]\\|\\) "
"19[0-9]* *$\\|"
"^Babyl Options:\\|"
"\^L\n[01],\\)") nil t)
	    (beginning-of-line)
	    (open-line 1)
	    (forward-line 2))
      (unlock-buffer))))
;;
;;

;--
;
;----------------------------------------------------------------------------
;Kevin Joyce                                     UUCP:  ..uunet!sequent!niven
;Sequent Computer Systems, Beaverton, OR.                Tel.  (503) 526-4103
;----------------------------------------------------------------------------



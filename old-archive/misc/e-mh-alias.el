; From: Peter S Galbraith <rhogee@mixing.qc.dfo.ca>
; Subject: e-mh-alias.el
; Date: Thu, 20 Mar 1997 10:38:07 -0500
; 
;; e-mh-alias.el - MH mail alias expansion and substitution.
;;
;; Copyright (C) 1994, 1995, 1996, 1997 Peter S. Galbraith
 
;; Author:    Peter S. Galbraith <rhogee@bathybius.meteo.mcgill.ca>
;; Created:   16 June 1994.
;; Version:   2.10 (20 Mar 97)
;; Keywords:  mh-e, mail, alias, emacs, xemacs

;; RCS $Id: e-mh-alias.el,v 1.6 1997/03/20 15:37:20 rhogee Exp $
;; Note: RCS version number does not correspond to release number.

;; Everyone is granted permission to copy, modify and redistribute this
;; file provided:
;;   1. All copies contain this copyright notice.
;;   2. All modified copies shall carry a prominant notice stating who
;;      made modifications and the date of such modifications.
;;   3. The name of the modified file be changed.
;;   4. No charge is made for this software or works derived from it.
;;      This clause shall not be construed as constraining other software
;;      distributed on the same medium as this software, nor is a
;;      distribution fee considered a charge.

;; LCD Archive Entry:
;; e-mh-alias|Peter Galbraith|galbraith@mixing.qc.dfo.ca|
;; MH mail alias expansion and substitution.|
;; 20-Mar-1997|2.10|~/misc/e-mh-alias.el.gz|

;; ----------------------------------------------------------------------------
;;; Commentary:

;; New versions of this package (if they exist) may be found at:
;;  ftp://ftp.phys.ocean.dal.ca/users/rhogee/elisp/e-mh-alias.el
;; (The LCD Archive is not very quick at processing new versions).

;; Description:
;;
;;  This packages makes [TAB] do completion in minibuffer To: and CC:
;;  prompts on mail aliases (and optionally local usernames) and will
;;  substitute these aliases in the MH-letter buffer.  You may enter
;;  multiple addressees by separating them with a comma, which (by default)
;;  will flash the alias translation of the previous address.
;;
;;  This package also makes [C-c C-f TAB] and Meta-[TAB] (aka M-\t) do
;;  completion in the letter header itself.  The Meta-[TAB] binding only
;;  works for emacs version 19.29 (and above), because I use text-properties 
;;  to set a local keymap for a range of text (namely the mail header) 
;;  because M-\t is useful as ispell-complete-word in the rest of the letter 
;;  buffer.  This feature is useful when you want to add an addressee as an 
;;  afterthought while editing a letter, or add an addresse to a reply.
;;
;; Installation instructions:
;;
;;  All you need to do is add this line to your .emacs file
;;   (require 'e-mh-alias)
;;
;;  By default, completion is case insensitive.  Should you want to change 
;;  this, you can set the following variable:
;;   (defvar e-mh-alias-completion-ignore-case nil)
;;  This is useful, for example, if you set all people aliases to lowercase
;;  like 
;;    p.galbraith: Peter Galbraith <galbraith@mixing.qc.dfo.ca>
;;  and lists in uppercase, like: 
;;    MH-E: mh-e mailing list <mh-e@x.org>
;;  Note that this variable affects minibuffer completion only.  If you
;;  have an alias for P.Galbraith and type in p.galbraith at the prompt, it
;;  will still be substituted in the letter buffer because these are
;;  identical aliases as far as MH and e-mh-alias are concerned.
;;
;;  By default, when you press the [comma] key at the To: or Cc: prompts,
;;  the previous mail alias translation is flashed (This feature is
;;  currently broken if you use the `multi-prompt' package described
;;  below).  To inhibit this, add the following to your ~/.emacs file
;;  *before* you load e-mh-alias.
;;
;;   (setq e-mh-alias-flash-on-comma nil)
;;
;;  By default, after the mail prompt all the aliases that you have entered
;;  are translated to their alias expansions.  To inhibit this (and thus
;;  use this package only to get completion at the mail prompt) set this
;;  variable:
;;
;;   (setq e-mh-alias-substitute-aliases nil)
;;
;;  Completion and substitutions are also done on usernames extracted from
;;  your /etc/passwd file.  This can be a handy tool on a machine where
;;  you and co-workers exchange messages, but should probably be disabled
;;  on a system with 100 users whom you don't know.  This feature is
;;  disabled by adding the following to your ~/.emacs file:
;;
;;   (setq e-mh-alias-local-users nil)
;;
;;  You may also set e-mh-alias-local-users to a string to be executed to
;;  generate the password file.  This could be "ypcat passwd" if you use
;;  yellow pages.
;;
;;  If you do use the e-mh-alias-local-users feature, check that the
;;  variable e-mh-hostname is set to a string containing @ followed by your
;;  hostname for better results (e.g. C-h v e-mh-hostname).  This should be
;;  set correctly after e-mh-alias is loaded, but if it isn't do it in your
;;  ~/.emacs file using, for example
;;
;;   (setq e-mh-hostname "@mixing.qc.dfo.ca")
;;
;;  By default, e-mh-alias `learns' mail aliases the first time you send
;;  mail and get the "To:" prompt, and subsequently whenever the mail alias
;;  file and password file are out of date with-respect-to its internal
;;  list.  Its internal list timestamp is compared against files set in the
;;  variable e-mh-timestamp.  To add your own alias file, you might use a 
;;  list like:
;;
;;   (setq e-mh-timestamp 
;;         '("~/Mail/mailaliases" "/usr/lib/mh/MailAliases" "/etc/passwd"))
;;
;;  You may want to answer the mail prompt with more than a single email
;;  address or alias.  To do this completion on comma-separated aliases,
;;  you must either load the package `complete.el' *before* you load
;;  e-mh-alias, or obtain the `multi-prompt.el' package.  The package
;;  complete.el affects minibuffer completion in everyday use, and you
;;  might not like it.  However, `multi-prompt.el' was written exactly for
;;  this reason so I suggest you use it; it's part of AUC TeX:
;;      ftp://sunsite.auc.dk/packages/auctex/auctex.tar.gz
;;
;;  Little quirks using the `multi-prompt' package:
;;   - The e-mh-alias-flash-on-comma feature is broken.
;;   - You may not precede a alias to complete with a space 
;;     e.g. you must enter  `To: p.galbraith,p.abrahamsen' 
;;          and not         `To: p.galbraith, p.abrahamsen'
;;     (This may change with future versions of multi-prompt)
;; ----------------------------------------------------------------------------
;;; Change log:
;; V1.00 16Jun94 Peter S Galbraith - Created as mh-aliases.el
;; V2.00 20Jul95 PSG - new version called mh-alias.el 
;;  - get aliases from MH's ali command.
;;  - allow for comma separated list.
;;  - optionally flash aliases when [comma] is pressed.
;;  - [C-c C-f \t] or [M-\t] in MH-Letter header expands/substitutes aliases
;; V2.01 24Jul95 PSG - Added e-mh-alias-completion-ignore-case
;; V2.02 24Jul95 PSG - 
;;  - called e-mh-alias.el because mh- prefix reserved for mh-e packages.
;;  - e-mh-alias-flash-on-comma may be unset after package loaded and can take 
;;    values nil, 1 or t. (Eric Ding <ericding@San-Jose.ate.slb.com>)
;;  - initialize e-mh-hostname (Stephen Gildea <gildea@x.org>)
;;  - shows completions buffer immediately on \M-\t in MH-Letter-buffer
;; V2.03 27Jul95 PSG - Made truly case insensitive for substitution. 
;;    Thanks to Christopher Lott <lott@informatik.uni-kl.de> for bug reports.
;; V2.04 29Sep95 PSG - Added e-mh-alias-use-complete for xemacs compatibility.
;; V2.05 03Jul96 PSG - Support yellow pages for e-mh-alias-local-users
;;                     /etc/passwd entries don't overwrite existing aliases
;;                     Updates to new alias files automatically.
;;                     Added e-mh-alias-substitute-aliases.
;;                     First entry in alias file superseeds others
;; V2.06 26Aug96 Jeffrey C Honig <jch@bsdi.com>  (RCS 1.2)
;;  - Better end of header regexp.
;; V2.07 26Nov96 Matthew Swift <swift@bu.edu>  (RCS 1.3)
;;  - bug fix in e-mh-learn-aliases
;; V2.08 03Jan97 PSG  (RCS 1.4)
;;  - Removed e-mh-alias-use-complete and allow use of multi-prompt.el
;; V2.09 12Feb97 (RCS 1.5) Ed Reingold <emr@black.wisdom.weizmann.ac.il>
;;  - e-mh-learn-local-users handles commas in name field correctly.
;; V2.10 20Mar97 (RCS 1.6) Ed Reingold <emr@black.wisdom.weizmann.ac.il>
;;  - e-mh-learn-local-users handles gecos correctly.
;; ----------------------------------------------------------------------------
;;; Code:

(defvar e-mh-alias-substitute-aliases t
  "*if t, e-mh-alias will substitute aliases entered in the mail prompts
to their expansions.")

(defvar e-mh-timestamp '("/usr/lib/mh/MailAliases" "/etc/passwd")
  "*e-mh-learn-aliases will automatically run if one of these files is updated.
This will keep the e-mh-alias database up-to-date.  The modification timestamp
of these files is compared against a created file named ~/.e-mh-alias

Set this to nil if you don't want the internal database to be updated 
automatically in this fashion.  You can always update it using
M-x e-mh-learn-aliases

It would be nice if e-mh-alias checked ~/.mh_profile instead of making
users set this variable, I know.")

(defvar e-mh-alias-completion-ignore-case t
  "*Non-nil means don't consider case significant in MH alias completion.
This is the default in plain MH, so it is the default here as well.
But you can change it usefully if, for example, you use lowercase aliases
for people and uppercase for lists.")

(defvar e-mh-alias-flash-on-comma t
  "*if non-nil, alias translation displayed when [comma] pressed in mh queries
t   flash alias translation but don't warn if there is no translation. 
1   flash alias translation and warn if there is no translation.
nil don't flash alias translation or warn if there is no translation.")

(defvar e-mh-alias-local-users t
  "*If t, local users are completed-to and expanded by MH To: and Cc: prompts.
The fake aliases are usually extracted from /etc/passwd for UID >= 200.
However, if you set this variable to a string, it will be executed to generate
the password file.  Use this for yellow pages, e.g.

 (setq e-mh-alias-local-users \"ypcat passwd\")")

(defvar e-mh-hostname 
  (concat "@"
          (or (and (boundp 'mail-host-address)
                   mail-host-address)
              (system-name)))
  "*String to append to local usernames from /etc/passwd to make addresses.
This should be \"@\" your fully qualified hostname (e.g. \"@mixing.qc.dfo.ca\")
See variable e-mh-alias-local-users.")

;; load mh-comp because I redefine mh-read-address. 
(require 'mh-comp)

(defvar e-mh-alias-alist nil "Alist of MH mailaliases.")
(defvar e-mh-alias-lowercase-alist nil "Alist of lowercased MH mailaliases.")

(defvar e-mh-read-address-map nil)
(if e-mh-read-address-map
    ()
  (setq e-mh-read-address-map (copy-keymap minibuffer-local-completion-map))
  (if (featurep 'complete)
      (define-key e-mh-read-address-map "\t" 'e-mh-PC-complete-address))
  (if e-mh-alias-flash-on-comma
      (define-key e-mh-read-address-map "," 'e-mh-minibuffer-confirm-address))
  (define-key e-mh-read-address-map " " 'self-insert-command))

(defun mh-read-address (prompt &optional initial-address)
  ;; Read a To: or Cc: address, prompting in the minibuffer with PROMPT.
  (cond
   ((not e-mh-alias-alist)              ;Doesn't exist, so create it.
    (e-mh-learn-aliases))
   ((and e-mh-timestamp                 ;Out of date, so recreate it.
         (car (memq t 
                    (mapcar
                     (function 
                      (lambda (x) (file-newer-than-file-p x "~/.e-mh-alias")))
                     e-mh-timestamp))))
    (e-mh-learn-aliases)))
  (if (not e-mh-alias-alist)
      (read-string prompt)
    (let* ((minibuffer-local-completion-map e-mh-read-address-map)
           (completion-ignore-case e-mh-alias-completion-ignore-case)
           ;; initial-address is nil with minibuffer To: and CC: prompts
           ;;                 maybe "" in MH-letter-mode
           (unread-command-char (cond ((or (not initial-address)
                                           (string-equal "" initial-address)) 
                                       -1)
                                      (t
                                       ??)))
           (the-answer 
            (or (if (and (featurep 'multi-prompt)
                         (not (featurep 'complete)))
                    (mapconcat 
                     (function (lambda (x) (format "%s" x)))
                     (multi-prompt "," nil
                      prompt e-mh-alias-alist nil nil initial-address)
                     ", ")
                  (completing-read 
                   prompt e-mh-alias-alist nil nil initial-address))
                "")))
      (cond 
       (e-mh-alias-substitute-aliases
        ;; Here I find all comma-whitespace-delimited words to test as aliases.
        (let ((the-aliases))
          (while (and the-answer
                      ;; Catches comma-delimited address with trailing space
                      (string-match "^[ \t,]*\\([^,]+\\)" the-answer))
            (let* ((the-index (match-end 0))
                   (the-match (substring the-answer 
                                         (match-beginning 1)(match-end 1)))
                   ;; Now trim off trailing whitespace
                   (trim-match (substring the-match 0 
                                          (string-match "[ \t]*$" the-match))))
              (setq the-aliases
                    (concat 
                     the-aliases ",\n "
                     (if (assoc (downcase trim-match) ; Translates to alias?
                                e-mh-alias-lowercase-alist) 
                         (mapconcat 
                          (function (lambda (x) (format "%s" x)))
                          (cdr (assoc (downcase trim-match) 
                                      e-mh-alias-lowercase-alist))
                          ",\n ")
                       trim-match)))      ; Does not translate 
              (setq the-answer (substring the-answer the-index))))
          ;; remove leading comma
          (if the-aliases
              (substring the-aliases 3)
            "")))
       (t                               ;Don't do any further processing
        the-answer)))))

(defun e-mh-PC-complete-address ()
  "Called by pressing [TAB] while in mh TO: or CC: prompts.
Uses the complete.el package."
  (interactive)
  (PC-do-completion nil (save-excursion (skip-chars-backward "^ \t,")(point))))

(defun e-mh-minibuffer-confirm-address ()
  "Called by pressing [comma] if e-mh-alias-flash-on-comma is t." 
  (interactive)
  (if (not e-mh-alias-flash-on-comma)
      ()
    (save-excursion
      (let ((the-name (buffer-substring
                       (progn (skip-chars-backward " \t")(point))
                       ;; This moves over to previous comma, if any
                       (progn (or (and (not (= 0 (skip-chars-backward "^,")))
                                       ;; the skips over leading whitespace
                                       (skip-chars-forward " "))
                                  ;; no comma, then to beginning of word
                                  (skip-chars-backward "^ \t"))
                              (point)))))
        (if (assoc (downcase the-name) e-mh-alias-lowercase-alist)
            (message "%s -> %s" the-name
                     (mapconcat (function (lambda (x) (format "%s" x)))
                                (cdr (assoc (downcase the-name) 
                                            e-mh-alias-lowercase-alist))
                                ", "))
          ;; Check if if was a single word likely to be an alias
          (if (and (equal e-mh-alias-flash-on-comma 1)
                   (not (string-match " " the-name)))
              (message "No alias for %s" the-name))))))
    (self-insert-command 1))

(defun e-mh-learn-aliases ()
  "Learn MH aliases, building an alist to complete with, and substitute with.
This can be called interactively to rebuild from updated files."
;;; Could be done using an obarray and `intern' to create it.
;;; elisp info doesn't say if completing-read is more efficient with alists
;;;  or obarrays.
  (interactive)
  (let ((mailalias-buffer (get-buffer-create " *e-mh-ali-command*")))
    (save-excursion
      (set-buffer mailalias-buffer)
      (erase-buffer)
      (insert (expand-file-name "ali" mh-progs) " -list\n")
      (write-region (point-min) (point-max) "/tmp/e-mh-ali-command" nil nil)
      (message "building list of MH mail aliases...")
      (erase-buffer)
      (call-process "/bin/sh" "/tmp/e-mh-ali-command" t nil)
      (delete-file "/tmp/e-mh-ali-command")
      (setq e-mh-alias-alist nil
            e-mh-alias-lowercase-alist nil)
      (goto-char (point-min))
      (let ((zero 0))
        (while (= zero 0)
          (cond
           ((looking-at "\\(.*\\): *\\(.*\\)$") ; A new MH alias
            (let ((username (buffer-substring 
                             (match-beginning 1)(match-end 1)))
                  (realname (buffer-substring 
                             (match-beginning 2)(match-end 2))))
              (cond
               ;; Don't overwrite existing aliases
               ((not (assoc (downcase username) 
                            e-mh-alias-lowercase-alist))
                (setq e-mh-alias-alist (cons (list username realname)
                                             e-mh-alias-alist))
                (setq e-mh-alias-lowercase-alist
                      (cons (list (downcase username) realname)
                            e-mh-alias-lowercase-alist)))
               (t                       
                ;; This alias already exists
                ;; Skip any continuation line
                (let ((zero 0))
                  (while (and (eq 0 zero)
                              (setq zero (forward-line 1))
                              (looking-at " ")))
                  (if (eq zero 0)
                      (forward-line -1)))))))
           ((looking-at "[ ]+\\(.*\\)$") ; An alias continued
            (let ((the-first (car e-mh-alias-lowercase-alist)) 
                  ;;^^The previous alias list entered ("NEW" "schneider@awi")
                  (the-rest (cdr e-mh-alias-lowercase-alist)) 
                  ;;^^The others before
                  (the-new (buffer-substring(match-beginning 1)(match-end 1))))
              (setq e-mh-alias-lowercase-alist
                    (cons
                     (append            ;  ("NEW" "budeus@awi" "schneider@awi")
                      (list (car the-first)) ; ("NEW")
                      (list the-new)    ;      ("budeus@awi")
                      (cdr the-first))  ;      ("schneider@awi")
                     the-rest)))))
          (setq zero (forward-line 1)))))
    (kill-buffer mailalias-buffer))
  (if e-mh-alias-local-users
      (e-mh-learn-local-users))
  (if e-mh-timestamp
      (let ((the-buffer (create-file-buffer "~/.e-mh-alias")))
        (save-excursion
          (set-buffer the-buffer)
          (set-buffer-modified-p t)
          (set-visited-file-name "~/.e-mh-alias")
          (set-buffer-modified-p t)
          (save-buffer 0))
        (kill-buffer the-buffer)))
  (message "building list of MH mail aliases... done."))

(defun e-mh-learn-local-users ()
  "Add local users from /etc/passwd to e-mh-alias-alist"
  (let ((mailalias-buffer (get-buffer-create " *mailalias*")))
    (save-excursion
      (set-buffer mailalias-buffer)
      (cond
       ((eq e-mh-alias-local-users t)
        (if (file-readable-p "/etc/passwd")
            (insert-file-contents "/etc/passwd")))
       (e-mh-alias-local-users
        (insert e-mh-alias-local-users "\n")
        (write-region (point-min) (point-max) "/tmp/e-mh-command" nil nil)
        (erase-buffer)
        (call-process "/bin/sh" "/tmp/e-mh-command" t nil)
        (goto-char (point-min))
        (delete-file "/tmp/e-mh-command")))
      (let ((zero 0))
        (while (= zero 0)
          (cond
           ((looking-at "\\([^:]*\\):[^:]*:\\([^:]*\\):[^:]*:\\([^:,]*\\)[:,]")
            (if (> (string-to-int 
                    (buffer-substring (match-beginning 2)(match-end 2)))
                   200)
                (let* ((username 
                        (buffer-substring (match-beginning 1)(match-end 1)))
                       (gecos-name
                        (buffer-substring (match-beginning 3)(match-end 3)))
                       (realname
                        (if (string-match "&" gecos-name)
                            (concat
                             (substring gecos-name 0 (match-beginning 0))
                             (capitalize username)
                             (substring gecos-name (match-end 0)))
                          gecos-name)))
                  (cond
                   ;; Don't overwrite existing aliases
                   ((not (assoc (downcase username) 
                                e-mh-alias-lowercase-alist))
                    (setq e-mh-alias-lowercase-alist
                          (cons (list 
                                 (downcase username)
                                 (if (string-equal "" realname)
                                     (concat "<" username e-mh-hostname ">")
                                   (concat realname 
                                           " <" username e-mh-hostname ">")))
                                e-mh-alias-lowercase-alist))
                    (setq e-mh-alias-alist
                          (cons (list username) e-mh-alias-alist))))))))
          (setq zero (forward-line 1)))))
    (kill-buffer mailalias-buffer)))

;;; --------------------------------------------------------------------------
;;; This part of e-mh-alias adds [M-TAB] expansion/substitution in letter head

(define-key mh-letter-mode-map "\C-c\C-f\t" 'e-mh-letter-expand-alias)

(cond
 ((string< "19.28.9" emacs-version)
  ;; wanted also to test (emacs-type) but this is unbound in -batch mode !
  (setq e-mh-expand-alias-map (copy-keymap mh-letter-mode-map))
  (define-key e-mh-expand-alias-map "\M-\t" 'e-mh-letter-expand-alias)

  (defun e-mh-letter-mode-expand-alias-hook ()
    "Make M-[TAB] expand mail alias within letter header"
    (save-excursion
      ;; Take extra precautions to not return errors if regexp not found.
      (add-text-properties (progn (goto-char (point-min))(point))
                           (progn (re-search-forward "^-*$" nil t)(point))
                           (list 'local-map e-mh-expand-alias-map))))

  (add-hook 'mh-letter-mode-hook 'e-mh-letter-mode-expand-alias-hook)))

(defun e-mh-letter-expand-alias ()
  "expand/convert mail alias before/under point."
  (interactive)
  (save-excursion
    (skip-chars-backward "^ ,:")
    (let ((the-match)(init-address "")(the-address))
      (if (looking-at "[^ ,;\n]+")
          (setq the-match (match-data)
                init-address 
                (or (and (string< "19.28.9" emacs-version)
                         (buffer-substring-no-properties 
                          (match-beginning 0)(match-end 0)))
                    (buffer-substring (match-beginning 0)(match-end 0)))))
      (if (assoc (try-completion (downcase init-address) 
                                 e-mh-alias-lowercase-alist) 
                 e-mh-alias-lowercase-alist)
          ;; init-address is "p.gal" which has unique completion to 
          ;; "p.galbraith", then substitute its alias, possibly multi-line.
          (setq the-address
                (mapconcat 
                 (function (lambda (x) (format "%s" x)))
                 (cdr (assoc (try-completion (downcase init-address) 
                                             e-mh-alias-lowercase-alist) 
                             e-mh-alias-lowercase-alist))
                 ",\n "))
        (setq the-address (mh-read-address "Address: " init-address)))
      (if (string-equal "" the-address)
          ()
        (if (not the-match)
            (insert the-address)
          (set-match-data the-match)
          (replace-match the-address t t))))))

(provide 'e-mh-alias)
;;; e-mh-alias.el ends here

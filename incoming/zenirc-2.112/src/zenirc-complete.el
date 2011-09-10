;;; zenirc-complete.el --- complete commands, nicknames, etc. in ZenIRC

;; Copyright (C) 1994, 1995 Noah S. Friedman
;; Copyright (C) 1998 Per Persson

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: pp@sno.pp.se
;; Keywords: zenirc, completion, extensions, oink
;; Created: 1994-06-26

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;; This code was inspired by the zenirc completion code written
;; by Per Persson <pp@@solace.mh.se>, but is a new implementation.

;; I tied into a lot of hooks to make caching of nicks, channels, servers,
;; etc. fairly insidious.  I think I've covered most of the useful ones;
;; some server messages, like KICK, don't really need their own completion
;; cache hooks because you can only see them on channels you've joined,
;; and the JOIN and 353 hooks already cache all the nicknames.

;; Putting the cache function on zenirc-format-nickuserhost-hook isn't
;; very reliable.  Depending on load order, other functions which add to
;; the hook might get called first and set zenirc-run-next-hook to nil.
;; That hook is intended just used to return a formatted nickname.

;; TODO: Add new hooks to add/delete nicks to/from cache.

;;; Code:


(require 'zenirc)

(defvar zenirc-complete-add-final-space-p t
  "*If non-nil, add a trailing space after unique completions.
This is consistent with the behavior of completion in general, as a
way of making it clear that the word is fully completed.  However, if you
frequently add punctuation to completed words and the additional whitespace
is annoying, set this to nil.")

(defvar zenirc-complete-display-function
  'zenirc-complete-display-echo-area-or-popup
  "*Function to call to display matches for partial completions.
This function receives a list of completions as returned by
`all-completions'.")

(defvar zenirc-complete-word-boundary-regexp "[ \t\n\r]"
  "*Regexp matching word boundaries in the ZenIRC buffer.
The regular expression should probably be a single character class list of
characters which are not considered part of a word, and thus server as a
boundary.  Whitespace is a good terminator; `#', `&', `/', etc. are not
good choices because that would prevent the possibility of completing
channel or command names.")

(defvar zenirc-command-complete-cache-hook '(zenirc-complete-command-cache)
  "*Hook to call when a /complete-cache command is issued in ZenIRC.
This adds a word to the list of known completions in ZenIRC.
The syntax of the command is: /complete-cache word")

(defvar zenirc-command-complete-uncache-hook '(zenirc-complete-command-uncache)
  "*Hook to call when a /complete-uncache command is issued in ZenIRC.
This removes a word from the list of known completions in ZenIRC.
The syntax of the command is: /complete-uncache word")

;; 307 buckets should be a reasonable size for most people (remember to use
;; a prime number to get good hashing characteristics).
;; This is not the total number of completions you can cache, but just the
;; number of "buckets" in which symbols can be stored.  If you regularly do
;; `/who *' and `/list *' to get all the channels and nicks on IRC,
;; increasing the size of this table might be helpful but is not necessary.
;; (As of March 1995, there are typically 5000 users, 2000 channels on IRC.)
(defvar zenirc-complete-table (make-vector 307 0)
  "Accumulated completion table for ZenIRC.
This table can include nicknames, channel names, server names, etc.

Completion is case-insensitive since there cannot be two nicknames, channel
names, etc. which differ only by case in zenirc.  However, when completing
a name uniquely, the case of the cached word is preserved.")
(make-variable-buffer-local 'zenirc-complete-table)

;; Initialize table with server commands; these are defined by IRC servers
;; but may not necessarily have any hooks in ZenIRC.  We could get these
;; from the IRC server but it's too slow.
(let ((l '("/admin" "/away" "/close" "/connect" "/die" "/dns" "/error"
           "/hash" "/help" "/info" "/invite" "/ison" "/join" "/kick"
           "/kill" "/links" "/list" "/lusers" "/mode" "/motd" "/names"
           "/nick" "/note" "/notice" "/oper" "/part" "/pass" "/ping"
           "/pong" "/privmsg" "/quit" "/rehash" "/restart" "/server"
           "/squit" "/stats" "/summon" "/time" "/topic" "/trace" "/user"
           "/userhost" "/users" "/version" "/wallops" "/who" "/whois"
           "/whowas")))
  (while l
    (set (intern (car l) zenirc-complete-table) (car l))
    (setq l (cdr l))))


;; The following macros are caching interfaces; they do
;; canonicalization, checking for previous caching, etc.

;; Note that you can't call macros directly with apply, so you can't put
;; these macros directly on hooks (zenirc-run-hook calls apply on the
;; symbols in the hook).  If this package were only intended to work under
;; Emacs 19, defsubst could have been used instead.

(defmacro zenirc-complete-cache (s)
  (list 'set (list 'intern
                   (list 'zenirc-downcase-name s)
                   'zenirc-complete-table) s))

;; You cannot actually unintern symbols from an obarray, but you can
;; make them unbound.
(defmacro zenirc-complete-uncache (s)
  (list 'makunbound (list 'intern
                          (list 'zenirc-downcase-name s)
                          'zenirc-complete-table)))

(defmacro zenirc-complete-cache-nick (s)
  (list 'zenirc-complete-cache (list 'or (list 'zenirc-extract-nick s) s)))

(defmacro zenirc-complete-uncache-nick (s)
  (list 'zenirc-complete-uncache (list 'or (list 'zenirc-extract-nick s) s)))


;;; Interactive interface: /complete-cache and /complete-uncache

(defun zenirc-complete-command-cache (proc words)
  (cond
   ((or (null words)
        (string= "" (cdr words)))
    (zenirc-message proc 'complete-cache-no-wordlist))
   (t
    (let ((wordlist (zenirc-parse-words (cdr words))))
      (while wordlist
        (zenirc-complete-cache (car wordlist))
        (setq wordlist (cdr wordlist)))
      (zenirc-message proc 'complete-cache-added (cdr words))))))

(defun zenirc-complete-command-uncache (proc words)
  (cond
   ((or (null words)
        (string= "" (cdr words)))
    (zenirc-message proc 'complete-cache-no-wordlist))
   (t
    (let ((wordlist (zenirc-parse-words (cdr words))))
      (while wordlist
        (zenirc-complete-uncache (car wordlist))
        (setq wordlist (cdr wordlist)))
      (zenirc-message proc 'complete-cache-removed (cdr words))))))


(defun zenirc-complete ()
  "Complete the current word at point for ZenIRC.
This completion cache comes from `zenirc-complete-table'.

Channel names, nicknames, server names, etc. are cached as ZenIRC happens
upon them; initially there are none to choose from."
  (interactive)
  (let* ((completion-ignore-case t)
         (bow (save-excursion
                (if (re-search-backward zenirc-complete-word-boundary-regexp
                                        zenirc-process-mark t)
                    (1+ (match-beginning 0))
                  zenirc-process-mark)))
         (eow (save-excursion
                (if (re-search-forward zenirc-complete-word-boundary-regexp
                                       (point-max) t)
                    (match-beginning 0)
                  (point-max))))
         (orig-word (buffer-substring bow eow))
         (word (zenirc-downcase-name orig-word))
         completion-list completion)
    (cond
     ((string= word "")
      (zenirc-message nil 'complete-not-on-word))
     (t
      ;; Don't check for new commands unless actually trying to complete a
      ;; command name.
      (and (eq (string-to-char word) ?/)
           (zenirc-complete-cache-new-zenirc-commands))
      (setq completion-list
            (all-completions word zenirc-complete-table 'boundp))
      (cond
       ((null completion-list)
        (zenirc-message nil 'complete-cant-find word))
       ;; faster than (= (length completion-list) 1)
       ((null (cdr completion-list))
        (delete-region bow eow)
        (goto-char bow)
        ;; unique completion.  Insert the original string, rather than the
        ;; key, in case the string has mixed case.
        (insert (symbol-value (intern (car completion-list)
                                      zenirc-complete-table)))
        (if zenirc-complete-add-final-space-p
            (insert " ")
          (zenirc-message nil 'complete-unique)))
       ((and (stringp (setq completion
                            (try-completion word zenirc-complete-table
                                            'boundp)))
             (not (string= word completion)))
        ;; This completion is just a partial match.
        ;; Don't delete partial match already typed, just add new chars.
        ;; This is to preserve the case of the letters typed so far since
        ;; no proper change in case has been decided yet.
        (goto-char eow)
        (insert (substring completion (length word))))
       ;; If we get this far, the partial completion so far is ambiguous.
       ;; If completion-auto-help (a standard user option) is non-nil, show
       ;; a table of possible completions.
       (completion-auto-help
        (funcall zenirc-complete-display-function
                 (all-completions word zenirc-complete-table 'boundp)))
       (t
        (zenirc-message nil 'complete-not-unique)))))))

;; Find all commands defined in ZenIRC itself, and attempt to add them to
;; the cache.  Each command FOO has a zenirc-command-FOO-hook, so look for
;; those symbols and extract FOO.  Don't actually consider them commands
;; unless the hook is bound; zenirc itself has to intern symbols whenever
;; it gets a command /foo, even if /foo isn't a command, and those interned
;; symbols are nonsense.
;; The return value of this function isn't meaningful.
(defun zenirc-complete-cache-new-zenirc-commands ()
  ;; all-completions is vastly faster than mapatoms + a lisp function.
  (let ((completions (all-completions "zenirc-command-" obarray 'boundp))
        name)
    (save-match-data
      (while completions
        (cond
         ((string-match "^zenirc-command-\\(.*\\)-hook$" (car completions))
          (setq name (concat "/" (substring (car completions)
                                            (match-beginning 1)
                                            (match-end 1))))
          (zenirc-complete-cache name)))
        (setq completions (cdr completions))))))

;; TODO: add other languages besides english
(defun zenirc-complete-install-message-catalogs ()
  (zenirc-lang-define-catalog 'english
   '((complete-cache-no-wordlist . "[error] No completion cache.")
     (complete-cache-added . "[info] Added to completion cache: %s")
     (complete-cache-removed . "[info] Removed from completion cache: %s")
     (complete-cache-unknown . "[error] Not in cache: %s")
     (complete-cache-known . "[info] Already in cache: %s")
     (complete-not-on-word . "No word at point")
     (complete-cant-find . "Can't find completion for \"%s\"")
     (complete-not-unique . "Next char not unique")
     (complete-unique . "Unique completion"))))


;;; Various methods to display completions

(defun zenirc-complete-display-traditional (completions)
  (with-output-to-temp-buffer " *Completions*"
    ;; Since we're not using the minibuffer for completions, don't do
    ;; the fancy completion setup emacs 19 uses to make mouse clicks
    ;; return a completion; it just doesn't work here.  Bind
    ;; completion-setup-hook to nil.
    (let (completion-setup-hook)
      (display-completion-list completions))))

;; This is similar to zenirc-complete-display-in-traditional except that
;; the new window is created by taking screen space from the zenirc window,
;; rather than stealing some other buffer's window.  Plus, the window is
;; made no larger than necessary to display all completions, with half the
;; zenirc window height being the maximum allowed.
(defun zenirc-complete-display-in-popup (completions)
  (let* ((orig-buffer (current-buffer))
         (orig-win (selected-window))
         (buf (get-buffer-create " *Completions*"))
         (win (get-buffer-window buf))
         (lines 0))
    (unwind-protect
        (progn
          (set-buffer buf)
          (erase-buffer)
          (let ((standard-output buf)
                (completion-setup-hook nil))
            (display-completion-list completions))

          ;; count number of lines
          (goto-char (point-min))
          (save-match-data
            (while (search-forward "\n" nil t)
              (setq lines (1+ lines))))

          (cond ((null win)
                 (split-window orig-win
                               (max (/ (window-height) 2)
                                    (- (window-height) lines)))
                 (select-window (next-window))
                 (switch-to-buffer buf)
                 (goto-char (point-min)))))
      (select-window orig-win)
      (set-buffer orig-buffer))))

;; Always display completions in the minibuffer.  Note that excessively
;; long lines will get truncated.
(defun zenirc-complete-display-in-echo-area (completions)
  (message "%s" (mapconcat 'identity completions " ")))

;; Display completions in the minibuffer if it will fit.
;; Otherwise, use traditional completion display.
(defun zenirc-complete-display-echo-area-or-traditional (completions)
  (let ((s (mapconcat 'identity completions " ")))
    (if (> (length s) (window-width (minibuffer-window)))
        (zenirc-complete-display-traditional completions)
      (message "%s" s))))

;; Display completions in the minibuffer if it will fit.
;; Otherwise, use popup completion display.
(defun zenirc-complete-display-echo-area-or-popup (completions)
  (let ((s (mapconcat 'identity completions " ")))
    (if (> (length s) (window-width (minibuffer-window)))
        (zenirc-complete-display-in-popup completions)
      (message "%s" s))))


;;; Cache nick and channel from invitations

(defun zenirc-complete-cache-INVITE (proc parsedmsg)
  (zenirc-complete-cache-nick (aref parsedmsg 1))
  (zenirc-complete-cache (aref parsedmsg 3)))

(zenirc-add-hook 'zenirc-server-INVITE-hook 'zenirc-complete-cache-INVITE)


;; Cache nick and channel from JOIN replies.

(defun zenirc-complete-cache-JOIN (proc parsedmsg)
  (zenirc-complete-cache-nick (aref parsedmsg 1))
  (if (string-match "" (aref parsedmsg 2))
      (zenirc-complete-cache (substring (aref parsedmsg 2) 0
					(- (match-end 0) 1)))
    (zenirc-complete-cache (aref parsedmsg 2))))

(zenirc-add-hook 'zenirc-server-JOIN-hook 'zenirc-complete-cache-JOIN)


;; Cache new nick from NICK change.

(defun zenirc-complete-cache-NICK (proc parsedmsg)
  (zenirc-complete-cache-nick (aref parsedmsg 2)))

(zenirc-add-hook 'zenirc-server-NICK-hook 'zenirc-complete-cache-NICK)


;; Cache nick and channel from PRIVMSGs and NOTICEs

(defun zenirc-complete-cache-msg (proc parsedmsg)
  (zenirc-complete-cache-nick (aref parsedmsg 1))
  (zenirc-complete-cache (aref parsedmsg 2)))

(zenirc-add-hook 'zenirc-server-PRIVMSG-hook 'zenirc-complete-cache-msg)
(zenirc-add-hook 'zenirc-server-NOTICE-hook 'zenirc-complete-cache-msg)


;; [312] Cache nick and server names from /whois replies

(defun zenirc-complete-cache-312 (proc parsedmsg)
  (zenirc-complete-cache-nick (aref parsedmsg 3))
  (zenirc-complete-cache (aref parsedmsg 4)))

(zenirc-add-hook 'zenirc-server-312-hook 'zenirc-complete-cache-312)


;; [319] Cache channel list from /whois replies

(defun zenirc-complete-cache-319 (proc parsedmsg)
  (save-match-data
    (let ((str (aref parsedmsg 4))
          chan)
      (while (string-match "\\(#\\|&\\)\\([^ \t#&]*\\)" str)
        (setq chan (substring str (match-beginning 1) (match-end 2)))
        (zenirc-complete-cache chan)
        (setq str (substring str (match-end 2)))))))

(zenirc-add-hook 'zenirc-server-319-hook 'zenirc-complete-cache-319)


;; [322] Cache channel names from /list replies

(defun zenirc-complete-cache-322 (proc parsedmsg)
  ;; Channel `*' means private (we don't know the name); don't cache that.
  (or (string= (aref parsedmsg 3) "*")
      (zenirc-complete-cache (aref parsedmsg 3))))

(zenirc-add-hook 'zenirc-server-322-hook 'zenirc-complete-cache-322)


;; [352] Cache nick, server, and channel names from /who replies

(defun zenirc-complete-cache-352 (proc parsedmsg)
  (zenirc-complete-cache (aref parsedmsg 3))
  (zenirc-complete-cache (aref parsedmsg 6))
  (zenirc-complete-cache-nick (aref parsedmsg 7)))

(zenirc-add-hook 'zenirc-server-352-hook 'zenirc-complete-cache-352)


;; [353] Cache nicknames from list after channel join (or NAMES command)

(defun zenirc-complete-cache-353 (proc parsedmsg)
  (save-match-data
    (let ((str (aref parsedmsg 5))
          name)
      (while (string-match "\\([^ \t@]+\\)" str)
        (setq name (substring str (match-beginning 1) (match-end 1)))
        ;; I think it's safe not to use zenirc-complete-cache-nick here
        ;; because the supplied nicks here don't include !user@host.
        (zenirc-complete-cache name)
        (setq str (substring str (match-end 1)))))))

(zenirc-add-hook 'zenirc-server-353-hook 'zenirc-complete-cache-353)


;; [401] Remove invalid nick or channel from cache
;; [402] Remove invalid server from cache
;; [403] (No such channel) remove channel from cache

(defun zenirc-complete-uncache-40x (proc parsedmsg)
  (zenirc-complete-uncache-nick (aref parsedmsg 3)))

;; I chose not enable these hooks for now, although I defined the function,
;; because nicks often go away due to netsplits, and uncaching them is
;; simply annoying.  Perhaps people will give feedback to the contrary.
;(zenirc-add-hook 'zenirc-server-401-hook 'zenirc-complete-uncache-40x)
;(zenirc-add-hook 'zenirc-server-403-hook 'zenirc-complete-uncache-40x)
;(zenirc-add-hook 'zenirc-server-402-hook 'zenirc-complete-uncache-40x)


(provide 'zenirc-complete)

(define-key zenirc-mode-map "\t" 'zenirc-complete)
(zenirc-complete-install-message-catalogs)

;; zenirc-complete.el ends here

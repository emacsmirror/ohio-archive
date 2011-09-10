;;
;; This defines the default list of servers prompted for
;; from the M-x muddy command.
;;
;; The fourth argument is a function to be called to customize
;; that connection.
;;
(setq muddy-server-list '(
  ("Local"		"localhost"	3000 lp-hook)
  ("Marches of Antan"	"checfs2.ucsd.edu"	3000 marches-hook)
  ("TMI"		"tmi.ccs.neu.edu"	5555 lpmud-hook)
  ("LambdaMOO"		"lambda.parc.xerox.com"	8888 tiny-hook)
  ))

;;
;; The rest of this sample defines example configurations, not
;; all of which are useful, but should provide ideas.
;;

;; Since we assigned this to muddy-startup-hook, all muds will
;; have this called, even if it's a server defined on the fly
;; (that is, not in the muddy-server-list above).
(setq muddy-startup-hook 'default-hook)

(defun default-hook ()
  ;; personal preference for wrapping
  (setq muddy-wrap-prefix "    ")

  ;; customize the mode line
  (setq mode-line-buffer-identification
        (list (format "Mudding at: %-17s" (car muddy-server))))

  ;; An auto-action.
  ;; Zap ansi stuff and other garbage (the 'repeat says to keep
  ;; doing this until they're all gone)
  (muddy-auto ".*\\(\033[[(][^a-zA-Z]*[a-zA-Z]\\|\000\\)" 9
              (muddy-delete-match 1)
              'repeat)
  )

;; tinymud & variants
(defun tiny-hook ()
  ;; Typical aliases.  Note that aliases will begin their matches
  ;; at the beginning of the input, so that a ' in the middle of
  ;; a command won't match here.
  (muddy-alias "\'" 5 (replace-match "say ") t)

  ;; The $ means end of input, so this one won't match "wave bob"
  (muddy-alias "wave$" 5 (replace-match ":waves.") t)

  ;; However, this one can match "wave Bob".  If so, the
  ;; (muddy-arg 1) extracts "Bob".  (not the *doubled* "\" delimiters,
  ;; done so it survives intact after this file is read)
  (muddy-alias "wave \\(.*\\)" 5
	       (replace-match (concat ":waves at " (muddy-arg 1) ".")) t)
  )

;; stuff inherited by all the lpmud worlds
;; (I use these more often, so there's more stuff here)
(defun lpmud-hook ()
  ;; set up the most common prompt we'll expect.
  ;; The "or" is here so we don't override a mud-specific setting
  (or muddy-prompt (setq muddy-prompt "> "))
  
  ;; auto-actions

  ;; This one just checks for a queued command every time we see a prompt.
  ;; (with a relatively low priority, so garbage removing actions have a
  ;; chance to match first)
  (muddy-auto 'muddy-prompt 9 (muddy-dequeue) t)

  ;; I like to be woken if up someone is talking to me...  Most
  ;; people will probably find these annoying, so they're commented out.
  ;; (muddy-auto "\\(\\w*\\) tells you:.*" 8 (beep) t)
  ;; (muddy-auto "\\(\\w*\\) replies:.*" 8 (beep) t)

  ;; Filter out some ugly telnet protocol
  (muddy-auto ".*\\(\377\374\001\\).*" 1
              (muddy-delete-match 1)
              'repeat)
  (muddy-auto ".*\\(\377\374\003\\).*" 1 ; Ugly MudOS leave-char-mode
              (muddy-delete-match 1)
              'repeat)

  ;; This is the "ed" prompt, so filter out that ^H
  (muddy-auto "\\*\\(\010\\)" 1   ; edit prompt
              (muddy-delete-match 1)
              t)

  ;; A silent read, probably a prompt for a password, but many muds
  ;; have taken to using this in pagers (sigh).  
  (muddy-auto ".*\\(\377\373\001\\).*" 5
              (muddy-delete-match 1)
	      ;; make sure prompt is in the window
              (recenter -1)
	      ;; read silently from user, then send to the mud
              (muddy-send-string (muddy-silent-read))
              t)
  
  ;; This is mostly a hack, but it can give you some idea of
  ;; more complicated things you can do.  It allows "<verb> it" to
  ;; work by replacing "it" the arguments from the last command.
  ;; Ie, you can do "look bottle", then "get it".
  (muddy-alias "\\w* it$" 1
               (if (string-match "^\\w* \\(\\w*\\).*$" muddy-last-command)
                   (let ((it (substring muddy-last-command
                                        (match-beginning 1) (match-end 1))))
                     (looking-at "\\(\\w*\\) it")
                     (replace-match (concat "\\1 " it))))
               t)
  )

(defun diku-hook () (lpmud-hook)
  (muddy-alias "gc$" 5 (replace-match "get all from corpse") t)
  )

;; World specific hooks

(defun pathetic-hook () (diku-hook)
  ;; This is for Mud Pathetique.  There are certain actions I
  ;; always wanted to do when starting up, so this will enable
  ;; that.  Muddy-enqueue just stores a string, and a later
  ;; muddy-dequeue will remove and execute it.  Recall that
  ;; in lpmud-hook muddy-dequeue is called when a prompt is
  ;; seen.  So these two commands will be given after we finish
  ;; logging in and we see our first two prompts.
  ;; (also not that we call diku-hook, which calls lpmud-hook,
  ;; which does the dequeue thingy)
  (muddy-enqueue "prompt hHvcC")
  (muddy-enqueue "channel newbie")
  )

(defun marches-hook () (lpmud-hook)
  ;; I like to always have my spellbook handy...
  ;; (muddy-enqueue "clone ! ~/obj/spellbook")

  ;; Here a single alias can be expanded into a series of commands.
  ;; The first command is given right away, and the next two are
  ;; queued up.
  (muddy-alias "bd \\(.*\\)" 1
               (let ((drink (muddy-arg 1)))
                 (replace-match (concat "buy " drink))
                 (muddy-enqueue (concat "drink " drink))
                 (muddy-enqueue "give glass to bartender")
                 t)))

(defun hollow-hook () (lpmud-hook)
  ;; This was for the old Vincent's Hollow
  
  (setq muddy-prompt "=> ")  ;; non-standard prompt
  
  ;; When someone arrives, you see "Foo arrives." followed by
  ;; "Foo looks around.", so I combine these two because I found
  ;; it annoying.
  (muddy-auto "\\(\\w*\\) looks around\\." 1
              (let ((who (muddy-arg 1)))
                (save-excursion
                  (forward-line -1)
                  (if (looking-at (concat who " arrives\\."))
                      (progn
                        (goto-char (match-end 0))
                        (backward-delete-char 2)
                        (insert "d and looked around.")
                        (setq who nil))))
                (if (not who)
                    (progn
                      (looking-at ".*")
                      (muddy-delete-match))))
              t)
  )

(defun geno-hook () (lpmud-hook)
  ;; This was set up for Genocide, however it's not fully debugged,
  ;; since I was just experimenting.  Basically, it just mucks with
  ;; the mode-line to keep some current stats, ala mymud.el (since
  ;; you don't have time to stop and check on Genocide).
  ;; I couldn't take the pressure, so didn't polish this.
  
  (make-local-variable 'geno-alive)
  (make-local-variable 'geno-hp)
  (make-local-variable 'geno-coins)
  (setq geno-alive "Alive")
  (setq geno-hp "HP: 0/0, SP 0/0")
  (setq geno-coins "0")
  (setq mode-line-buffer-identification
        '("" geno-alive "; " geno-hp
          "; $" geno-coins))
  (muddy-auto "\\(HP: [0-9]+/[0-9]+, SP: [0-9]+/[0-9]+\\)" 1
              (setq geno-hp (muddy-arg 1)) t)
  (muddy-auto ".*You have \\([0-9]+/[0-9]+\\) hit points, and \\([0-9]+/[0-9]+\\) spell points." 1
              (setq geno-hp (concat "HP: " (muddy-arg 1)
                                    ", SP: " (muddy-arg 2)))
              t)
  (muddy-auto ".*You have \\(.+\\) coins." 1
              (setq geno-coins (muddy-arg 1)) t)
  (muddy-auto "Coins: \\([0-9]+\\)." 1
              (setq geno-coins (muddy-arg 1)) t)
  (muddy-auto "\\([0-9]+\\) gold coins: Ok." 1
              (setq geno-coins (int-to-string
                                (+ (string-to-int geno-coins)
                                   (string-to-int (muddy-arg 1)))))
              t)
  (muddy-auto "You die." 1
              (setq geno-alive "Dead") t)
  (muddy-auto ".*You are DEAD!" 1
              (setq geno-alive "Dead") t)
  (muddy-auto "Your body is real again.  Strangely nothing has changed." 1
              (setq geno-alive "Alive") t)

  ;; short quick aliases are a necessity on Genocide.
  (muddy-alias "l \\(.*\\)" 1
               (replace-match (concat "look at " (muddy-arg 1)))
               t)
  (muddy-alias "dp$" 5 (replace-match "drink potion") t)
  (muddy-alias "gc$" 5 (replace-match "get all from corpse") t)
  (muddy-alias "ga$" 5 (replace-match "get all") t)
  (muddy-alias "ww$" 5 (replace-match "use wand") t)
  (muddy-alias "wa$" 5 (replace-match "whatarea") t)
  (muddy-alias "\"" 5 (replace-match "say ") t)
  (muddy-alias "bd \\(.*\\)" 1
               (let ((drink (muddy-arg 1)))
                 (if (eq drink "fire") (setq drink "firebreather"))
                 (replace-match (concat "buy " drink))
                 (muddy-enqueue (concat "drink " drink)) )
               t)
  )

;; Ideas from mymud.el.  Simple, sorta buggy, code that let's you
;; follow someone.  (fun if invisible)

(defvar muddy-follow-person)		; who we're following
(make-variable-buffer-local 'muddy-follow-person)

(defun muddy-follow (&rest whom)
  (interactive)
  (if whom
      (setq whom (car whom))
    (setq whom
	  (read-from-minibuffer "Follow whom: " muddy-follow-person)))
  (if (string= whom "") (setq muddy-follow-person nil)
    (setq muddy-follow-person whom)))

(defun test-hook ()
  ;; Follow stuff.
  (muddy-auto "\\(.*\\) arrives\\." 1
              (let ((who (muddy-arg 1)))
                (save-excursion
                  (forward-line -1)  ;; look at prev line
                  (if (looking-at "\\([a-zA-Z_, ]*\\) arrives\\.")
                      (progn
                        (goto-char (match-end 1))
                        (insert ", " who)
                        (setq who nil))))
                (if (not who)
                    (progn
                      (looking-at ".*")
                      (muddy-delete-match))))
              t)
  (local-set-key "\^c\^f" 'muddy-follow)
  (muddy-auto "\\(\\w*\\) leaves \\(west\\||east\\|south\\|north\\|down\\|up\\|northwest\\|northeast\\|southwest\\|southeast\\)\\." 1
              (if (and muddy-follow-person
                       (string= muddy-follow-person (muddy-arg 1)))
                  (muddy-send-string (muddy-arg 2))))
  )

(defun test ()
  ;; An example of using emacs19 faces to hilight certain text...
  (muddy-auto "\\(\\w* \\(says\\|tells you\\|shouts\\):\\).*" 8
	      (put-text-property (match-beginning 1) (match-end 1)
				 'face 'bold)
	      t)
  )

;; Top-secret code to display output from snoop in another window removed...

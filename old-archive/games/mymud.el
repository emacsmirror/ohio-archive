;-*- mode: Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         mymud.el
; Version:      1.20 (> 1.9)
; RCS:          $Header: $
; Description:  Another mud client in emacs
; Author:       Carl Edman
; Created:      Sun Dec 21 16:39:00 1990
; Modified:     Tue May 14 16:06:58 1991 (Carl Edman) cedman@lynx
; Language:     Emacs-Lisp
; Package:      N/A
; Status:       Release
; Copyright:    This program is perfect freeware. Do with it whatever you want.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Revisions:
;
; Thu May  9 11:42:05 1991 (Carl Edman) cedman@lynx
;  Again added quite a bit of things to the DikuMUD-Setup. Soon really all
;  the features of the LPMUD-mode will be there.
; Wed May  8 09:39:50 1991 (Carl Edman) cedman@lynx
;  New variable: mymud-kill-empty-lines. Should lines which are completely
;  empty be deleted ? Added for the benefit of dikumud players who otherwise
;  get over 60% empty lines on their screens. Why does DikuMUD insist on
;  flooding you with empty lines ? (Even 'compact' does not diminish their
;  number a lot). And why does it have to repeat the bloody prompt after
;  every other output ?
; Wed May  8 09:38:57 1991 (Carl Edman) cedman@lynx
;  Dikumud mode much expanded. Now it parses the 'score' command and puts all
;  relevant data into the status line. Also a few other things about it have
;  been fixed up
; Tue May  7 22:41:40 1991 (Carl Edman) cedman@lynx
;  Tabs are now expanded before triggers are checked
; Tue May  7 22:41:09 1991 (Carl Edman) cedman@lynx
;  Put robot parts into public source (call Robot-Setup in player customization
;  and set mymud-master to the name of the player the robot should obey)
; Tue May  7 20:21:26 1991 (Carl Edman) cedman@lynx
;  New variable : mymud-prompt. It selects how the prompt _in_the_display_
;  should look. Better do not set this to something which might trigger
;  a mymud-aa (like the prompt used by the server itself). I might fix that,
;  as soon as I find an easy and compatible way to do that.
; Tue May  7 20:16:45 1991 (Carl Edman) cedman@lynx
;  More big changes ! (I thought that I might as well make everyone else mad
;  at me at once :-)) The main parsing routine mymud-filter has been completely
;  rewritten. Now it is almost elegant and might actually work. (I just hope
;  that all old copies of this routine will be deleted before someone connects
;  them with me). From now on when a mymud-aa leaves a line empty (only consisting
;  out of spaces) the line gets removed. Hence from now on, leave off the
;  "\n" at the end of mymud-aa's. If you don't mymud.el can get confused
;  badly. If you do, mymud.el will Do The Right Thing. This also fixed a few
;  bugs in the parser which I wasn't able to locate before.
; Tue May  7 18:07:18 1991 (Carl Edman) cedman@lynx
;  Big change ! From now on ctrl-m's wont any longer be replaced by spaces
;  but completely removed. This may cause the change in many regexps, which
;  depended on those extra spaces at the end of lines, but DikuMud users
;  convinced me that this change is necessary. Dikumud puts ctrl-m's all
;  over the place !
; Sun May  5 18:55:29 1991 (Carl Edman) cedman@lynx
;  Added nw,ne,sw,se as directions
; Sun May  5 09:29:21 1991 (Carl Edman) cedman@lynx
;  Added some documentation
; Sun May  5 09:03:52 1991 (Carl Edman) cedman@lynx
;  Now all actions which match are executed, not only the first one
;  The main parsing loop sorely needs some cleaning up
; Sat May  4 20:42:05 1991 (Carl Edman) cedman@lynx
;  Started adding support for Afterhours LPMUD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A note on version numbers:
; Mymud is currently in a stage in which the almost all changes to it are
; simply adding another independent feature or fix another bug. Doing this
; takes only a little while, so many small changes are made. Hence the version
; numbering scheme works like this: Every time someone asks me for a version
; of mymud.el, and there have even been minor changes to mymud.el since I
; gave the last copy to someone else, I bump the version number and hand
; out the latest version. So don't be surprised if the version number goes
; up by five during one day. The real amount of work done is not proportional
; to the version numbers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; If you can not put mymud somewhere in the load-path, simply load-file it
; before every first use in an emacs (Type ESC x load-file RET
; ~/emacs/mymud.el (assuming that you put mymud.el in your subdirectory
; "emacs")).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Notice to novice users of LPMud and DikuMud modes !
;
; By default hitting 'return' queues the commands on the current line and sends
; them one by one each time the MUD server sends a prompt. This may not be
; desirable if the current line is intended as an answer to a non-standard
; prompt like "What is your name: " or "Password: ". To override this "smart"
; behavior hit the 'Escape' key before the 'return' key. Then the current line
; will be sent to the server without waiting for prompts or parsing it into
; subcommands a.s.o.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; If you create your own setups for the muds and mud types you use
; (particularly the DikuMUD setup could use some enhancement) then by all
; means send them to me. They will be included in the next release of MyMud.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; An example of what you could put into your ~/.emacs file:
; (Only the autoload line is really necessary. The rest are just ideas to
;  customize your logins)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (autoload 'mymud "mymud" "My favorite mud-client" t)
;
; (defun Alderon-User ()
;   "My login as 'user' to Alderon."
;   ; Call the setup for a particular mud type (or particular mud if available)
;   ; always first in your personal setup
;   (Alderon-Setup)
;   ; Not a necessary line. Just an example of a simple customization:
;   ; Define x to be an abbreviation for examine
;   (mymud-short "x" "examine ")
;   ; If you to be logged in automatically enter your name and password here
;   (mymud-disp-set 'mymud-name "User")
;   (mymud-disp-set 'mymud-password "Userpassword"))
;
; (setq mymud-command-window-height 10 ; I like _big_ command windows
;       mymud-separator "."            ; Just like in the old Infocom games
;       mymud-kill-empty-lines t       ; I play dikumud a lot.
;       mymud-prompt ">>>"
;       mymud-private-sites
;    '(("Alderon-User"       "alderon.lanl.gov"          2000 Alderon-User)
;      ("LocalLPMud"         "local.user.edu"             500 Lpmud-Setup)))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'mymud)

(defvar mymud-command-window-height 5 "* size of the command window")
(defvar mymud-use-full-screen t "* delete other windows when mymud is started ?")
(defvar mymud-separator ";"
  "* special character(or string) which separates commands.")
(defvar mymud-min-brief-length 8
  "* minimum length of a goto-command to cause 'brief' to be inserted
 before and 'verbose' after. Nil means never.")
(defvar mymud-debug nil
  "Always set this to nil unless you know what you are doing.")
(defvar mymud-prompt " > "
  "The prompt which is displayed on the screen.")
(defvar mymud-kill-empty-lines nil
  "Should lines which contain only whitespace be displayed at all ?")
(defvar mymud-private-sites nil
 "Your personal logins. They get added to the complete list when mymud is loaded.")

(defun Tinymud-Setup () ; Just a dummy to be called on startup
  "Default setup function for all TINYMUDs, either called directly or first command in the setup function for a specific TINYMUD"
  )

(defun Tinymush-Setup () ; Just a dummy to be called on startup
  "Default setup function for all TINYMUSHes, either called directly or first command in the setup function for a specific TINYMUSH"
  )

(defun Dikumud-Setup ()   ; Very incomplete
  "Default setup function for all DikuMuds, either called directly or first command in the setup function for a specific DikuMuds"
  (mymud-set-key "\r" 'mymud-queue-command)
  (mymud-set-key "\M-\r" 'mymud-send-command)
  (mymud-disp-make 'mymud-name nil)
  (mymud-disp-make 'mymud-password nil)
  (mymud-disp-make 'mymud-cur-hp "10")
  (mymud-disp-make 'mymud-max-hp "10")
  (mymud-disp-make 'mymud-cur-sp "10")
  (mymud-disp-make 'mymud-max-sp "10")
  (mymud-disp-make 'mymud-cur-mp "10")
  (mymud-disp-make 'mymud-max-mp "10")
  (mymud-disp-make 'mymud-xp "0")
  (mymud-disp-make 'mymud-gold "0")
  (mymud-disp-make 'mymud-character-age "21 years")
  (mymud-disp-make 'mymud-age "0 days and 0 hours")
  (mymud-disp-make 'mymud-title "beginner")
  (mymud-disp-make 'mymud-level "1")
  (mymud-disp-make 'mymud-position-status "standing")
  (mymud-disp-make 'mymud-hunger-status "unhungry")
  (mymud-disp-make 'mymud-thirst-status "unthirsty")
  (mymud-disp-make 'mymud-brief-status "Brief")
  (mymud-disp-set 'mode-line-format '("" (mymud-queue "* " "  ") mymud-name "," mymud-title " Hp:" mymud-cur-hp "/" mymud-max-hp " Mana:" mymud-cur-sp "/" mymud-max-sp " Move:" mymud-cur-mp "/" mymud-max-mp " Gold:" mymud-gold " Xp:" mymud-xp " (" mymud-hunger-status "," mymud-thirst-status  "," mymud-brief-status "," mymud-position-status")" ))
  (mymud-aa "By what name do you wish to be known? *"
            '((if mymud-name (mymud-dispn mymud-name)
                ) nil))
  (mymud-aa "Password: " '((if mymud-password
                               (progn
                                 (mymud-send mymud-password)
                                 (mymud-dispn ""))) nil))
  (mymud-aa "> "
            '((mymud-purger 0)
              (cond
               ((equal mymud-queue '()) )
               ((equal mymud-queue 'last) (setq mymud-queue nil))
               (t (mymud-command (car mymud-queue))
                  (setq mymud-queue (cdr mymud-queue))))
               t))
  (mymud-short "it"
     '((if (string-match "^\\w* \\w*.*$"
                         (mymud-disp-var 'mymud-last-command))
           (insert (substring (mymud-disp-var 'mymud-last-command)
                              (match-beginning 1) (match-end 1)) " ")
         (beep))))
  (mymud-aa "Alas, you cannot go that way\\.\\.\\."
            '((if (and mymud-last-command (mymud-directional mymud-last-command)) (setq mymud-where (mymud-add-dir-pd mymud-where (mymud-invert-dirs (list mymud-last-command))))) t))
  (mymud-aa "\\*\\*\\* Press return \\*\\*\\*"
            '((mymud-sendn "") (mymud-purger 0) t))
  (mymud-aa " *\\*\\*\\* PRESS RETURN: *"
            '((mymud-sendn "") (mymud-purger 0) t))
  (mymud-aa "You are \\([0-9]* years\\) old\\."
            '((setq mymud-character-age (mymud-matcher 1))
              (mymud-purger 0) t))
  (mymud-aa "You have \\([0-9]*\\)(\\([0-9]*\\)) hit, \\([0-9]*\\)(\\([0-9]*\\)) mana and \\([0-9]*\\)(\\([0-9]*\\)) movement points\\."
            '((setq mymud-cur-hp (mymud-matcher 1)
                    mymud-max-hp (mymud-matcher 2)
                    mymud-cur-sp (mymud-matcher 3)
                    mymud-max-sp (mymud-matcher 4)
                    mymud-cur-mp (mymud-matcher 5)
                    mymud-max-mp (mymud-matcher 6))
              (mymud-purger 0) t))
  (mymud-aa "You have scored \\([0-9]*\\) exp, and have \\([0-9]*\\) gold coins\\."
            '((setq mymud-xp (mymud-matcher 1)
                    mymud-gold (mymud-matcher 2))
              (mymud-purger 0) t))
  (mymud-aa "You have been playing for \\([A-Za-z0-9 ]*\\)\\."
            '((setq mymud-age (mymud-matcher 1))
              (mymud-purger 0) t))
  (mymud-aa "This ranks you as \\(\\w*\\) the \\(\\w*\\) (level \\([0-9]*\\))\\."
            '((setq mymud-name (mymud-matcher 1)
                    mymud-title (mymud-matcher 2)
                    mymud-level (mymud-matcher 3))
              (mymud-purger 0) t))
  (mymud-aa "You are standing\\."
            '((setq mymud-position-status "standing")
              (mymud-purger 0) t))
  (mymud-aa "You stand up\\.\\|You stop resting, and stand up\\."
            '((setq mymud-position-status "standing") t))
  (mymud-aa "You are sitting\\."
            '((setq mymud-position-status "sitting")
              (mymud-purger 0) t))
  (mymud-aa "You sit down\\."
            '((setq mymud-position-status "sitting") t))
  (mymud-aa "You go to sleep\\."
            '((setq mymud-position-status "sleeping") t))
  (mymud-aa "You are sleeping\\."
            '((setq mymud-position-status "sleeping")
              (mymud-purger 0) t))
  (mymud-aa "You wake, and sit up\\."
            '((setq mymud-position-status "sitting") t))
  (mymud-aa "You rest your tired bones\\."
            '((setq mymud-position-status "resting") t))
  (mymud-aa "You are resting\\."
            '((setq mymud-position-status "resting")
              (mymud-purger 0) t))
  ;You are now sober.
  ;You are full
  (mymud-aa "You are hungry\\."
            '((setq mymud-hunger-status "hungry")
              (mymud-purger 0) t))
  (mymud-aa "You are thirsty\\."
            '((setq mymud-thirst-status "thirsty")
              (mymud-purger 0) t))
  (mymud-aa "Brief mode on\\."
            '((setq mymud-brief-status "Brief")
              (mymud-purger 0) t))
  (mymud-aa "Brief mode off\\."
            '((setq mymud-brief-status "Verbose")
              (mymud-purger 0) t))
  (mymud-aa "You are hungry\\."
            '((setq mymud-hunger-status "hungry")
              (mymud-purger 0) t))
  (mymud-aa "You are thirsty\\."
            '((setq mymud-thirst-status "thirsty")
              (mymud-purger 0) t))
  ; For following:
  (mymud-disp-make 'mymud-follow-person nil)
  (mymud-set-key "\^c\^f" 'mymud-follow)
  (mymud-aa "\\(The \\)?\\(\\w*\\) leaves \\(west\\|east\\|south\\|north\\|down\\|up\\|northwest\\|northeast\\|southwest\\|southeast\\)\\."
            '((if (and mymud-follow-person
                       (string= mymud-follow-person (mymud-matcher 1)))
                  (cond
                   ((string= (mymud-matcher 2) "west") (mymud-order "w"))
                   ((string= (mymud-matcher 2) "east") (mymud-order "e"))
                   ((string= (mymud-matcher 2) "north") (mymud-order "n"))
                   ((string= (mymud-matcher 2) "south") (mymud-order "s"))
                   ((string= (mymud-matcher 2) "down") (mymud-order "d"))
                   ((string= (mymud-matcher 2) "up") (mymud-order "u"))
                   ((string= (mymud-matcher 2) "northwest") (mymud-order "nw"))
                   ((string= (mymud-matcher 2) "northeast") (mymud-order "ne"))
                   ((string= (mymud-matcher 2) "southwest") (mymud-order "sw"))
                   ((string= (mymud-matcher 2) "southeast") (mymud-order "se"))))
              t))
  (mymud-order "score")
  (mymud-order "brief")
  (mymud-order "brief")
  )

(defun Lpmud-Setup () ; Rather complete
  "Default setup function for all LPMUDs, either called directly or first command in the setup function for a specific LPMUD"
  ; Setup for the variables which are useful in all LPMUDs (and only in them)
  ; Name and Password should be overriden in the player-setup
  ; The others need not the as they are automatically adjusted upon login
  (mymud-set-key "\r" 'mymud-queue-command)
  (mymud-set-key "\M-\r" 'mymud-send-command)
  (mymud-disp-make 'mymud-name nil)
  (mymud-disp-make 'mymud-password nil)
  (mymud-disp-make 'mymud-title "beginner")
  (mymud-disp-make 'mymud-drunk-status "Sober")
  (mymud-disp-make 'mymud-brief-status "Verbose")
  (mymud-disp-make 'mymud-wimpy-status "Brave")
  (mymud-disp-make 'mymud-alignment "neutral")
  (mymud-disp-make 'mymud-cur-hp "50")
  (mymud-disp-make 'mymud-cur-sp "50")
  (mymud-disp-make 'mymud-xp "0")
  (mymud-disp-make 'mymud-gold "0")
  (mymud-disp-make 'mymud-max-hp "50")
  (mymud-disp-make 'mymud-max-sp "50")
  (mymud-disp-make 'mymud-hunted-by nil)
  (mymud-disp-make 'mymud-age "0 days 0 hours 0 minutes 0 seconds")
  (mymud-disp-make 'mymud-last-location nil)
  ; I like this mode-line-format although 80 columns usualy is a bit short for it
  (mymud-disp-set 'mode-line-format '("" (mymud-queue "* " "  ") mymud-name "," mymud-title "(" mymud-alignment ") Hp:" mymud-cur-hp "/" mymud-max-hp " Sp:" mymud-cur-sp "/" mymud-max-sp " Gold:" mymud-gold " Xp:" mymud-xp " (" mymud-drunk-status "," mymud-wimpy-status "," mymud-brief-status ") "))
  ; To spool out future commands
  (mymud-aa "> "
            '((cond
               ((equal mymud-queue '()) )
               ((equal mymud-queue 'last) (setq mymud-queue nil))
               (t (mymud-command (car mymud-queue))
                  (setq mymud-queue (cdr mymud-queue))))
              (mymud-purger 0) t))
  ; For following:
  (mymud-disp-make 'mymud-follow-person nil)
  (mymud-set-key "\^c\^f" 'mymud-follow)
  (mymud-aa "\\(\\w*\\) leaves \\(west\\|east\\|south\\|north\\|down\\|up\\|northwest\\|northeast\\|southwest\\|southeast\\)\\."
            '((if (and mymud-follow-person
                       (string= mymud-follow-person (mymud-matcher 1)))
                  (cond
                   ((string= (mymud-matcher 2) "west") (mymud-order "w"))
                   ((string= (mymud-matcher 2) "east") (mymud-order "e"))
                   ((string= (mymud-matcher 2) "north") (mymud-order "n"))
                   ((string= (mymud-matcher 2) "south") (mymud-order "s"))
                   ((string= (mymud-matcher 2) "down") (mymud-order "d"))
                   ((string= (mymud-matcher 2) "up") (mymud-order "u"))
                   ((string= (mymud-matcher 2) "northwest") (mymud-order "nw"))
                   ((string= (mymud-matcher 2) "northeast") (mymud-order "ne"))
                   ((string= (mymud-matcher 2) "southwest") (mymud-order "sw"))
                   ((string= (mymud-matcher 2) "southeast") (mymud-order "se"))))
              t))
  (mymud-aa "(just CR) " '((mymud-purger 0) (mymud-dispn "") t))
  ;Kill those annoying controll sequences
  (mymud-aa "ÿü"
            '((mymud-purger 0) (mymud-disp-set 'mymud-hide-output nil) t))
  (mymud-aa "ÿû"
            '((mymud-purger 0) (mymud-disp-set 'mymud-hide-output t) t))
  ; Short favorites from [c]lpmud.el
  (mymud-short "fc" "from corpse ")
  (mymud-short "fb" "from bag ")
  (mymud-short "ib" "in bag ")
  (mymud-short "gc" "get corpse ")
  (mymud-short "dc" "drop corpse ")
  (mymud-short "ec" "examine corpse ")
  (mymud-short "sc" "search corpse ")
  ; Just a neat little idea to show what you can do on one line (well, almost)
  (mymud-short "it"
     '((if (string-match "^\\w* \\w*.*$"
                         (mymud-disp-var 'mymud-last-command))
           (insert (substring (mymud-disp-var 'mymud-last-command)
                              (match-beginning 1) (match-end 1)) " ")
         (beep))))
  (mymud-short "back" '((mymud-goto (mymud-disp-var 'mymud-last-location))))
  (mymud-short "setback" '((mymud-disp-set 'mymud-last-location (mymud-disp-var 'mymud-where))))
  ; These locations should be in most LPMUDs (needed for the auto-navigator)
  (mymud-landmark "church" nil "Village church." "You are in the local village church.")
  (mymud-landmark "pub" '("s" "e" "e" "n" "e") "The local pub." "You are in the local pub.")
  (mymud-landmark "shop" '("s" "e" "e" "e" "n") "The shop." "You are in a shop. You can buy or sell things here.")
  (mymud-landmark "post" '("s" "e" "e" "s" "s") "The post office." "This is the post office. If you want to send a mail to someone,")
  (mymud-landmark "guild" '("s" "e" "e" "e" "s") "The adventurers guild." "You have to come here when you want to advance your level.")
  (mymud-landmark "bank" '("s" "e" "e" "s" "e") "The bank." "You are in the bank.")
  (mymud-landmark "clearing" '("s" "w" "w" "w" "w") "Clearing." "A small clearing. There are trees all around you.")
  (mymud-aa "\\(What \\?\\|A strong magic force stops you\\.\\|Failed to load file\\.\\)"
            '((if (and mymud-last-command (mymud-directional mymud-last-command)) (setq mymud-where (mymud-add-dir-pd mymud-where (mymud-invert-dirs (list mymud-last-command))))) t))
  ; The following 3 are necessary for login (and get removed after being executed)
  (mymud-aa "What is your name:" '((if mymud-name
                                       (mymud-dispn mymud-name))
                                     nil))
  (mymud-aa "Password: $"
            '((if mymud-password (mymud-sendn mymud-password)) nil))
  (mymud-aa "Password: (just CR) $"
            '((mymud-sendn "") (mymud-purger 0) nil))
  (mymud-aa "Throw the other copy out *\\? " '((mymud-dispn "yes") nil))
  ; Adjust the internal statuses of various modes
  (mymud-aa "Brief mode\\."
            '((setq mymud-brief-status "Brief") (mymud-purger 0) t))
  (mymud-aa "Verbose mode\\."
            '((setq mymud-brief-status "Verbose") (mymud-purger 0) t))
  (mymud-aa "Wimpy = \\([0-9]*\\)% of \\(max\\|maximum\\) hit *points\\."
            '((setq mymud-wimpy-status (concat "Wimpy " (mymud-matcher 1) "%"))
              (mymud-purger 0) t))
  (mymud-aa "Wimpy at \\([0-9]*\\) hit *points\\."
            '((setq mymud-wimpy-status (concat "Wimpy " (mymud-matcher 1) " hp"))
              (mymud-purger 0) t))
  (mymud-aa "Wimpy mode\\."
            '((setq mymud-wimpy-status "Wimpy") (mymud-purger 0) t))
  (mymud-aa "Brave mode\\."
            '((setq mymud-wimpy-status "Brave") (mymud-purger 0) t))
  ; All possible ways to indicate drunkness/undrunkness
  (mymud-aa "You no longer have a head *ache\\."
            '((setq mymud-drunk-status "Sober") t))
  (mymud-aa "Your head *ache disappears\\."
            '((setq mymud-drunk-status "Drunk") t))
  (mymud-aa "\\(A tingling feeling goes through your body\\|A shock wave runs through your body\\|That feels good\\).? *"
            '((setq mymud-drunk-status "Drunk") t))
  (mymud-aa "You \\(burp\\|feel drunk\\|stumble\\|hiccup\\)\\."
            '((setq mymud-drunk-status "Drunk") (mymud-purger 0) t))
  (mymud-aa "You are \\(roaring drunk\\|somewhat drunk\\|in a drun*ken stupor\\|quite tipsy\\|slightly tipsy\\)\\."
            '((setq mymud-drunk-status "Drunk") (mymud-purger 0) t))
  (mymud-aa "You suddenly without reason get a bad head *ache\\."
            '((setq mymud-drunk-status "Headache") t))
  ; Gag the GO player ! Similar lines ought to fit on Harry also
  (mymud-aa "\\(The \\)?[Gg]o player says: .*" '((mymud-purger 0) t))
  (mymud-aa "Harry says: .*" '((mymud-purger 0) t))
  ; The next commands are there to adjust the internal values for the parameters
  ; every time score is called. It is only tested on the World-of-Mizar and Alderon
  ; so your regexps may vary
  (mymud-aa "You are \\(\\w*\\) the \\([a-zA-Z? ]*\\) (\\(\\w*\\)) (level \\([0-9]*\\))\\."
            '((setq mymud-title (mymud-matcher 2)
                    mymud-alignment (mymud-matcher 3)
                    mymud-name (mymud-matcher 1))
              (mymud-purger 0) t))
  (mymud-aa "You have \\([0-9]*\\) experience, \\([0-9]*\\) quest points, and \\([0-9]*\\) coins\\."
            '((setq mymud-xp (mymud-matcher 1)
                    mymud-gold (mymud-matcher 3))
              (mymud-purger 0) t))
  (mymud-aa "You are in an immaterial state with no scores\\."
            '((setq mymud-title "Ghost")))
  (mymud-aa "Health level: \\([0-9]*\\) hit points (of \\([0-9]*\\)), and \\([0-9]*\\) spell points\\."
            '((setq mymud-cur-hp (mymud-matcher 1)
                    mymud-max-hp (mymud-matcher 2)
                    mymud-max-sp (mymud-matcher 2)
                    mymud-cur-sp (mymud-matcher 3))
              (mymud-purger 0) t))
  (mymud-aa "You are \\(now \\)?hunted by \\(\\w*\\)\\."
            '((setq mymud-hunted-by (mymud-matcher 1)) (mymud-purger 0) t))
  (mymud-aa "You have \\([0-9]*\\) experience points, \\([0-9]*\\) gold coins, *"
            '((setq mymud-xp (mymud-matcher 1)
                    mymud-gold (mymud-matcher 2))
              (mymud-purger 0) t))
  (mymud-aa "\\([0-9]*\\) hit points(\\([0-9]*\\))\\."
            '((setq mymud-cur-hp (mymud-matcher 1)
                    mymud-max-hp (mymud-matcher 2)
                    mymud-max-sp (mymud-matcher 2))
              (mymud-purger 0) t))
  (mymud-aa "\\([0-9]*\\) hit points (of \\([0-9]*\\)), and \\([0-9]*\\) spell points\\."
            '((setq mymud-cur-hp (mymud-matcher 1)
                    mymud-max-hp (mymud-matcher 2)
                    mymud-max-sp (mymud-matcher 2)
                    mymud-cur-sp (mymud-matcher 3))
              (mymud-purger 0) t))
  (mymud-aa "age: *\\(\\w[a-zA-Z0-9 ]*\\w\\)\\."
            '((setq mymud-age (mymud-matcher 1)) (mymud-purger 0) t))
  (mymud-aa "\\([0-9]*\\) spell points\\."
            '((setq mymud-cur-sp (mymud-matcher 1))
              (mymud-purger 0) t))
  (mymud-aa "\\(\\w*\\) the \\([a-zA-Z0-9 ]*\\) (\\([a-zA-Z0-9 ]*\\))"
            '((if (string= mymud-name (mymud-matcher 1))
                  (setq mymud-title (mymud-matcher 2)
                        mymud-alignment (mymud-matcher 3))) t))
  (mymud-aa "You are intoxicated\\."
            '((setq mymud-drunk-status "Drunk") (mymud-purger 0) t))
  (mymud-aa "You are \\(completely \\)*sober\\."
            '((if (not (string= mymud-drunk-status "Headache"))
                  (setq mymud-drunk-status "Sober")) (mymud-purger 0) t))
  ; Gold update commands
  (mymud-aa "You get \\([0-9]*\\) gold coins\\."
            '((setq mymud-gold (int-to-string (+ (string-to-int mymud-gold) (string-to-int (mymud-matcher 1))))) t))
  (mymud-aa "[a-zA-Z ]*: *You get \\([0-9]*\\) gold coins\\."
            '((setq mymud-gold (int-to-string (+ (string-to-int mymud-gold) (string-to-int (mymud-matcher 1))))) t))
  (mymud-aa "[a-zA-Z ]* gives you \\([0-9]*\\) gold coins\\."
            '((setq mymud-gold (int-to-string (+ (string-to-int mymud-gold) (string-to-int (mymud-matcher 1))))) t))
  (mymud-aa "You drop \\([0-9]*\\) gold coins\\." 
            '((setq mymud-gold (int-to-string (- (string-to-int mymud-gold) (string-to-int (mymud-matcher 1))))) t))
  (mymud-aa "You pay \\([0-9]*\\) [a-zA-Z ]*\\."
            '((setq mymud-gold (int-to-string (- (string-to-int mymud-gold) (string-to-int (mymud-matcher 1)))))))
  ; First automatic commands to update your status
  (mymud-order "score")
  (mymud-order "brief")
  (mymud-order "brief"))

(defun AfterHours-Setup ()
  "An example setup for a specific LPMUD."
  (Lpmud-Setup)
  (mymud-disp-make 'mymud-level "0")
  (mymud-disp-make 'mymud-race "human")
  (mymud-disp-make 'mymud-class "adventurer")
  (mymud-disp-make 'mymud-str "1")
  (mymud-disp-make 'mymud-int "1")
  (mymud-disp-make 'mymud-dex "1")
  (mymud-disp-make 'mymud-con "1")
  (mymud-aa "------------------------------------------------------------------"
            '((mymud-purger 0) t))
  (mymud-aa "\\(\\w*\\) the \\([a-zA-Z ]*\\) (\\([a-zA-Z ]*\\))"
            '((if (string= (mymud-matcher 1) mymud-name)
                  (progn
                    (setq mymud-title (mymud-matcher 2))
                    (setq mymud-alignment (mymud-matcher 3))
                    (mymud-purger 0))) t))
  (mymud-aa "Level *: *\\([0-9]*\\) *Gold Coins *: *\\([0-9]*\\) *"
            '((setq mymud-level (mymud-matcher 1)
                    mymud-gold (mymud-matcher 2))
              (mymud-purger 0) t))
  (mymud-aa "Maximum HP *: *\\([0-9]*\\) *Maximum SP *: *\\([0-9]*\\) *"
            '((setq mymud-max-hp (mymud-matcher 1)
                    mymud-max-sp (mymud-matcher 2))
              (mymud-purger 0) t))
  (mymud-aa "Current HP *: *\\([0-9]*\\) *Current SP *: *\\([0-9]*\\) *"
            '((setq mymud-cur-hp (mymud-matcher 1)
                    mymud-cur-sp (mymud-matcher 2))
              (mymud-purger 0) t))
  (mymud-aa "Experience *: *\\([0-9]*\\) *Next Level *: *\\([0-9]*\\) *"
            '((setq mymud-xp (mymud-matcher 1))
              (mymud-purger 0) t))
  (mymud-aa "Race *: *\\([a-zA-Z ]*\\w\\) *Class *: *\\([a-zA-Z ]*\\w\\) *"
            '((setq mymud-race (mymud-matcher 1)
                    mymud-class (mymud-matcher 2))
              (mymud-purger 0) t))
  (mymud-aa "Str *: *\\([0-9]*\\) *Int *: *\\([0-9]*\\) *Dex *: *\\([0-9]*\\) *Con *: *\\([0-9]*\\) *"
            '((setq mymud-str (mymud-matcher 1)
                    mymud-int (mymud-matcher 2)
                    mymud-dex (mymud-matcher 3)
                    mymud-con (mymud-matcher 4))
              (mymud-purger 0) t))
  (mymud-aa "Age *: *\\([0-9a-zA-Z ]*\\)\\. *"
            '((setq mymud-age (mymud-matcher 1))
              (mymud-purger 0) t))
  (mymud-aa "Your current hit points are \\([0-9]*\\)\\."
            '((setq mymud-cur-hp (mymud-matcher 1))
              (mymud-purger 0) t))
  (mymud-aa "Your current spell points are \\([0-9]*\\)\\."
            '((setq mymud-cur-sp (mymud-matcher 1))
              (mymud-purger 0) t))
  (mymud-aa "Also Healthy\\."
            '((mymud-purger 0) t))
  (mymud-aa "You are slightly tipsy\\."
            '((mymud-purger 0) t))
  (mymud-aa "You say: .*"
            '((mymud-purger 0) t))
  (mymud-landmark "bank" '("s" "e" "s") "The bank." "You are in the bank.")
  (mymud-landmark "shop" '("s" "e" "n") "The shop." "You are in a shop.")
  (mymud-landmark "inn" '("s" "e" "e" "n") "Village Inn." "You are in the Village Inn.")
  (mymud-landmark "guild" '("s" "w" "s") "The adventurers guild." "You have to come here when you want to advance your level.")
  (mymud-landmark "pub" '("s" "w" "w" "n") "The local pub." "You are in the local pub.")
  (mymud-landmark "post" '("s" "w" "w" "s") "The Vision Post Office." "You are in the Vision post office.")
  (mymud-landmark "smithy" '("s" "w" "w" "w" "n") "Smithy." "This is the town smithy.")
  )

(defun Mizar-Setup ()
  "An example setup for a specific LPMUD."
  ; Always call the basic setup for the MUD-type first
  (Lpmud-Setup)
  ; An example landmark for the World of Mizar. Add many many many more
  (mymud-landmark "temple" '("s" "e" "e" "e" "e" "n" "n" "n" "w" "w" "n" "n" "e") "Temple yard." "A yard in front of a huge temple, built in wood.")
  (mymud-landmark "angmar" '("s" "w" "w" "s") "Angmar's castle." "You are inside the gates of Angmar the Necromancer's castle.")
  (mymud-landmark "wreck" '("s" "e" "e" "e" "e" "e" "e" "e" "s" "s" "s" "w" "d") "Deck of wreck." "You're standing on the deck of on old ship")
  ; The cow is worse than the GO player
  (mymud-aa "The cow emits a lazy \"Moooooohh\""
            '((mymud-purger 0) t))
  (mymud-short "teleport" "cast teleport to ")
  (mymud-short "whisper" "cast whisper ")
  (mymud-short "dig" "cast dig")
  (mymud-short "watcher" "cast summon watcher spirit")
  (mymud-short "locate" "cast locate ")
  (mymud-short "detect" "cast detect invisible")
  (mymud-short "diagnose" "cast diagnose ")
  (mymud-short "enlighten" "cast light")
  (mymud-short "obscure" "cast obscure")
  (mymud-order "who")
  )

(defun Alderon-Setup ()
  "An example setup for a specific LPMUD."
  ; Always call the basic setup for the MUD-type first
  (Lpmud-Setup)
  (mymud-short "f" "flamestrike ")
  (mymud-landmark "cave" '("s" "e" "e" "e" "e" "s" "s" "s" "s" "e" "e" "s" "s") "Closed Cave." "Closed Cave")
  (mymud-landmark "grog" '("s" "w" "w" "w" "w" "n" "n" "w" "n" "n" "n" "w" "n") "Grog's magic shop." "You are in Grog's magic shop. Grog's voice says from nowhere")
  (mymud-landmark "market" '("s" "e" "e" "e" "e" "n" "n" "n" "w" "w" "w") "Bracken Marketplace. " "This is the local Marketplace. ")
  (mymud-landmark "inn" '("s" "e" "e" "e" "e" "n" "n" "n" "n" "n" "w") "Eastroad Inn." "You are in the Eastroad Inn. Here you can buy food to still your")
  (mymud-landmark "giant" '("s" "w" "w" "w" "w" "n" "n" "n" "w" "w" "w") "Lair of the Giant." "There are mountains all around you.")
  (mymud-landmark "aa" '("s" "w" "w" "w" "w" "n" "n" "w" "n" "n" "n" "w" "n" "n" "w" "w" "n"))
  (mymud-landmark "knight" '("s" "w" "w" "w" "w" "s" "e" "u" "u" "u") "Fourth floor of the tower." "This is the fourth floor of the tower.  There is an")
  (mymud-landmark "landing" '("s" "e" "s" "s" "s" "s" "s" "s") "A landing in stairway." "You are in a landing on the stairway, the stairway goes up")
  (mymud-landmark "town" '("s" "e" "e" "e" "e" "n" "n" "n" "n" "n" "n" "n" "n" "w" "w" "n" "w" "w" "w" "w" "w") "A long road going through town.")
  )

(defun Robot-Setup ()
  "Not a setup for a specific mud. Rather call this to make a player a robot."
  (mymud-disp-make 'mymud-master "")
  (mymud-aa "\\(\\w*\\) says: \\([\\w*\\),\\(.*\\)"
            '((if (and (string= (mymud-matcher 1) mymud-master)
                       (string= (mymud-matcher 2) mymud-name))
                  (mymud-robot-do (mymud-matcher 3))) t))
  (mymud-aa "\\(\\w*\\) tells you: \\(.*\\)"
            '((if (string= (mymud-matcher 1) mymud-master)
                  (mymud-robot-do (mymud-matcher 2))) t))
  (mymud-aa "\\(\\w*\\) whispers to you: \\(.*\\)"
            '((if (string= (mymud-matcher 1) mymud-master)
                  (mymud-robot-do (mymud-matcher 2))) t))
  )

(defun mymud-robot-do (str)
  (cond
   ((string= str "report")
    (mymud-order "score")
    (mymud-order (concat "tell " mymud-master " HP:" mymud-cur-hp " Sp:" mymud-cur-sp " Gold:" mymud-gold " Loc:" (mapconcat '(lambda (str) str) mymud-where ",") )))
   ((string= str "follow")
    (mymud-follow mymud-master))
   ((string= str "stop")
    (mymud-follow ""))
   (t (mymud-order str)))
  )

(defun mymud (&optional name)
  "Connect to a Mud."
  ; Code stolen from a variety of MUD logins (and extensively remodelled)
  (interactive)
  (if (not name) (setq name (completing-read "Mud: " mymud-sites nil t)))
  (let* ((sel (assoc name mymud-sites))
         (site-hook (nth 3 sel))
         (disp-buf (generate-new-buffer (concat "*" (car sel) "-display*")))
         (comm-buf (generate-new-buffer (concat "*" (car sel) "*")))
         (proc))
    (condition-case err
       (setq proc (open-network-stream (car sel) disp-buf (nth 1 sel) (nth 2 sel)))
      (error (message "Error opening: %s" (prin1-to-string err))))
    (set-process-filter proc 'mymud-filter)
    (set-process-sentinel proc 'mymud-filter)
    (switch-to-buffer disp-buf)
    (mymud-disp-mode)
    (if mymud-use-full-screen (delete-other-windows))
    (select-window (split-window-vertically (- (window-height) mymud-command-window-height 1)))
    (switch-to-buffer comm-buf)
    (mymud-mode)
    (run-hooks 'site-hook)
    ))

(defun mymud-disp-mode ()
  "Major mode for the display window in mymud-mode."
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "mymud-disp")
  (setq major-mode 'mymud-disp-mode)
  (setq buffer-read-only t)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map (make-sparse-keymap))
  (local-set-key "\^c\^b" 'mymud-balance-windows)
  (local-set-key "\^c\^q" 'mymud-quit)
  (make-local-variable 'mymud-queue)
  (setq mymud-queue 'last)
  (make-local-variable 'mymud-hide-output)
  (setq mymud-hide-output nil)
  (make-local-variable 'mymud-where)
  (setq mymud-where nil)
  (make-local-variable 'mymud-disp-buf)
  (setq mymud-disp-buf disp-buf)
  (make-local-variable 'mymud-comm-buf)
  (setq mymud-comm-buf comm-buf)
  (make-local-variable 'mymud-action-list)
  (setq mymud-action-list nil)
  (make-local-variable 'mymud-last-command)
  (setq mymud-last-command nil)
  (make-variable-buffer-local 'next-screen-context-lines)
  (setq next-screen-context-lines 0)
  (make-variable-buffer-local 'scroll-step)
  (setq scroll-step 1)
  (if mymud-debug (progn
                    (make-local-variable 'mymud-debug-list)
                    (setq mymud-debug-list nil)))
  (goto-char (point-max)))

(defun mymud-mode ()
"Major mode for the command window in Mymud.
RET      mymud-send-command
 will send the current line to the MUD server
TAB      mymud-expand-short
 will try to expand the previous word if it is an abbreviation
C-c C-q  mymud-quit
 will close all windows and kill all buffers associated with the current login
C-c C-e  mymud-end-of-buffer
 will scroll the display window to the end of the text
C-c C-a  mymud-beginning-of-buffer
 will scroll the display window to the beginning of the text
C-c C-s  mymud-search
 will interactively search backward through the display buffer
C-c C-w  mymud-where
 will display mymuds idea of where you are with respect to the starting location
 in the message area.
C-c C-b  mymud-balance-windows
 will expand display and communications windows visible, expand, and move the
 point to the communication buffer
C-c C-d  mymud-toggle-debug
 will toggle debugging mode. Don't use this unless you know what you are doing

For some types of MUD (LPMUD and some DikuMUD in particular) additional features
are defined.
RET      mymud-queue
 will not send the current line immediately, put split it up into commands
 separated by the value of the variable mymud-separator (usualy ';'). These
 commands are then send in succession each time a prompt is received.
ESC RET  mymud-send-command
 will send the current line immediately and without parsing it. You have to
 use this to respond to any questions preceded by anything but the standard
 prompt (e.g. questions for the username, password a.s.o.)
C-c C-f  mymud-follow
 follow the creature with the given name around (Untested on DikuMUD)
TAB      mymud-expand-short
 will also recognize the name of locations common to all LPMUDs and landmarks
 which have been entered for the particular MUD. Typing e.g. \"bank TAB\" will
 expand to a sequence of commands which will move you to the bank. Just hit
 return and you start moving (Untested on DikuMUD).

Many, many more neat features. Look at the code or simply try things to find
out about them."
  (interactive)
  (kill-all-local-variables)
  (setq mode-name "Mymud")
  (setq major-mode 'mymud-mode)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map (make-sparse-keymap))
  (local-set-key "\^c\^q" 'mymud-quit)
  (local-set-key "\^c\^e" 'mymud-end-of-buffer)
  (local-set-key "\^c\^a" 'mymud-beginning-of-buffer)
  (local-set-key "\^c\^s" 'mymud-search)
  (local-set-key "\^c\^w" 'mymud-where-display)
  (local-set-key "\^c\^b" 'mymud-balance-windows)
  (local-set-key "\^c\^d" 'mymud-debug-toggle)
  (local-set-key "\r" 'mymud-send-command)
  (local-set-key "\C-i" 'mymud-expand-short)
  (make-local-variable 'mymud-disp-buf)
  (setq mymud-disp-buf disp-buf)
  (make-local-variable 'mymud-comm-buf)
  (setq mymud-comm-buf comm-buf)
  (mymud-comm-make 'mymud-short-list nil)
  (make-variable-buffer-local 'next-screen-context-lines)
  (setq next-screen-context-lines 0)
  (make-variable-buffer-local 'scroll-step)
  (setq scroll-step 1))

(defun mymud-filter (proc string)
  (save-excursion (let ((start) (l) (indent-tabs-mode nil))
    (set-buffer (process-buffer proc))
    (setq start (point-max))
    (if mymud-debug (setq mymud-debug-list (cons string mymud-debug-list)))
    (let ((buffer-read-only nil))
      (goto-char start)
      (insert string)
      (goto-char start)
      (while (search-forward "\^m" nil t) (delete-backward-char 1))
      (goto-char start)
      (while (search-forward "\^i" nil t)
        (setq l (current-column))
        (delete-backward-char 1)
        (indent-to l))
      (goto-char start)
      (while (not (eq (point) (point-max)))
        (if (looking-at " *$")
            (if mymud-kill-empty-lines (kill-line 1) (forward-line 1))
          (setq l mymud-action-list)
          (while (and l (not (looking-at " *$")))
            (if (and (car l) (looking-at (car (car l))))
                (if (not (eval (cons 'progn (car (cdr (car l))))))
                    (setcar l nil)))
            (setq l (cdr l)))
          (if (looking-at "^ *$") (kill-line 1) (forward-line 1)))))
    (mymud-end-of-buffer))))

(defun mymud-send (string)
  "Send a string to the process associated with mymud-disp-buf"
  (let ((proc (get-buffer-process mymud-disp-buf)))
    (if proc (send-string proc string))))

(defun mymud-disp (string &optional prefix)
  "Send a string to the process associated with mymud-disp-buf and print it in the display window"
  (mymud-send string)
  (save-excursion
    (set-buffer mymud-disp-buf)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (if (not (mymud-disp-var 'mymud-hide-output))
          (if prefix (insert prefix string) (insert string))))))

(defun mymud-sendn (string)
 "Send string (with a newline added) to the process associated with mymud-disp-buf"
 (mymud-send (concat string "\n")))

(defun mymud-dispn (string &optional prefix)
  "Send string (with a newline added) to the process associated with mymud-disp-buf and print it in the display window"
  (mymud-disp (concat string "\n") prefix))

(defun mymud-directional (str)
  "Is this a direction command ? If so, return inverse, otherwise nil"
  (cond
   ((string= str "n") "s")
   ((string= str "s") "n")
   ((string= str "w") "e")
   ((string= str "e") "w")
   ((string= str "u") "d")
   ((string= str "d") "u")
   ((string= str "ne") "sw")
   ((string= str "nw") "se")
   ((string= str "sw") "ne")
   ((string= str "se") "nw")
   (t nil)))

(defun mymud-command (str)
  "Send a command"
  (if (mymud-directional str)
      (mymud-disp-set 'mymud-where (mymud-add-dir-pd (mymud-disp-var 'mymud-where) (list str))))
  (mymud-disp-set 'mymud-last-command str)
  (mymud-dispn str mymud-prompt))

(defun mymud-quit ()
  "Leave mymud-mode , kill all the processes, and close all the windows"
  (interactive)
  (let* ((disp mymud-disp-buf)
         (comm mymud-comm-buf)
         (proc (get-buffer-process disp)))
    (if proc (delete-process proc))
    (if disp (delete-windows-on disp))
    (if comm (delete-windows-on comm))
    (if disp (kill-buffer disp))
    (if comm (kill-buffer comm))))

(defun mymud-end-of-buffer ()
  "Jump to the end of the display buffer."
  (interactive)
  (let ((win (get-buffer-window mymud-disp-buf))
        (swin (selected-window)))
        (if win (save-excursion
          (set-buffer mymud-disp-buf)
          (goto-char (1- (point-max)))
          (select-window win)
          (recenter -1)
          (select-window swin)))))

(defun mymud-beginning-of-buffer ()
  "Jump to the beginning of the display buffer."
  (interactive)
  (let ((win (get-buffer-window mymud-disp-buf))
        (swin (selected-window)))
        (if win (save-excursion
          (set-buffer mymud-disp-buf)
          (select-window win)
          (goto-char (point-min))
          (recenter 1)
          (select-window swin)))))

(defun mymud-search ()
  "Isearch in the display buffer."
  (interactive)
  (let ((win (get-buffer-window mymud-disp-buf))
        (swin (selected-window)))
        (if win (save-excursion
          (set-buffer mymud-disp-buf)
          (select-window win)
          (isearch-backward)
          (recenter (/ (window-height win) 2))
          (select-window swin)))))

(defun mymud-balance-windows ()
  "Rebalance and display both display and communication buffer."
  (interactive)
;  (if mymud-use-full-screen
      (delete-other-windows)
;    (delete-windows-on mymud-disp-buf)
;    (delete-windows-on mymud-comm-buf))
  (switch-to-buffer mymud-disp-buf)
  (select-window (split-window-vertically (- (window-height) mymud-command-window-height 1)))
  (switch-to-buffer mymud-comm-buf)
  (mymud-end-of-buffer))

(defun mymud-debug-toggle ()
  "Turn debugging on and off."
  (interactive)
  (if (mymud-disp-var 'mymud-debug)
      (progn
        (mymud-disp-set 'mymud-debug nil)
        (insert (princ "Debug list:\n")
                (prin1-to-string (reverse (mymud-disp-var 'mymud-debug-list))))
        (message "Mymud debug is off."))
    (mymud-disp-make 'mymud-debug-list nil)
    (mymud-disp-set 'mymud-debug t)
    (message "Mymud debug is on.")))

(defun mymud-follow (whom)
  "Start/Stop following a specific player."
  (interactive "sFollow whom: ")
  (if (string= whom "") (mymud-disp-set 'mymud-follow-person nil)
    (mymud-disp-set 'mymud-follow-person whom)))

(defun mymud-where-display ()
  "Display the path to the current location in the message line."
  (interactive)
  (message (concat "You are:" (mapconcat '(lambda (str) str) (mymud-disp-var 'mymud-where) mymud-separator))))

(defun mymud-queue-command ()
  "Command to send as an order the current line."
  (interactive)
  (end-of-line)
  (let ((end (point)) (beg))
    (beginning-of-line)
    (setq beg (point))
    (mymud-order (buffer-substring beg end)))
  (goto-char (point-max))
  (if (not (looking-at "^$")) (insert-char ?\n 1)))

(defun mymud-send-command ()
  "Command to send the current line."
  (interactive)
  (end-of-line)
  (let ((end (point)) (beg))
    (beginning-of-line)
    (setq beg (point))
    (mymud-dispn (buffer-substring beg end)))
  (goto-char (point-max))
  (if (not (looking-at "^$")) (insert-char ?\n 1)))

(defun mymud-order (str)
  "Send (or queue) a possibly composite command."
  (let ((cur) (quo (concat (regexp-quote mymud-separator) "\\|$" )))
    (while (and (not (string= "" str)) (string-match quo str))
      (setq cur (substring str 0 (match-beginning 0)))
      (setq str (substring str (match-end 0)))
      (cond
       ((equal (mymud-disp-var 'mymud-queue) '())
        (mymud-command cur)
        (mymud-disp-set 'mymud-queue 'last))
       ((equal (mymud-disp-var 'mymud-queue) 'last) 
        (mymud-disp-set 'mymud-queue (list cur)))
       (t 
        (mymud-disp-set 'mymud-queue 
                        (append (mymud-disp-var 'mymud-queue) (list cur))))))))

(defun mymud-aa (regexp elist)
  "Most important customization routine. Whenever REGEXP appears the list of expressions ELIST is executed. If the elist returns t upon execution it will be executed again next time REGEXP appears. If it returns nil it is forgotten."
  (mymud-disp-set 'mymud-action-list (append (mymud-disp-var 'mymud-action-list) (list (list regexp elist)))))

(defun mymud-delete-word-around-point ()
  ; Stolen from the UNIX-interface routines by Wolfgang Rupprecht
  ; (and slightly adapted)
  "Return the word around the point as a string and kill it."
  (save-excursion
    (let ((beg) (str))
      (if (not (looking-at "\\<"))
	  (forward-word -1))
      (setq beg (point))
      (forward-word 1)
      (setq str (buffer-substring beg (point)))
      (delete-region beg (point))
      str)))

(defun mymud-expand-short ()
  "Expand shorthands. Usually bound to TAB."
  (interactive)
  (let* ((li (mymud-comm-var 'mymud-short-list))
         (word (mymud-delete-word-around-point))
         (res (try-completion word li))
         (all (all-completions word li)))
    (cond
     ((not all)
      (insert word)
      (beep))
     ((assoc word li)
      (mymud-execute-short word))
     ((not (cdr all))
      (mymud-execute-short (car all)))
     (t
      (insert res)))))

(defun mymud-landmark (short path &rest descs)
  "This function does two things:
First it sets up a shorthand with autonavigation to path
Second it adds an action for every desc given which resets the
mymud-where variable to path"
  (mymud-short short (list (cons 'mymud-goto (list (cons 'quote (list path))))))
  (if descs (mymud-aa (mapconcat 'regexp-quote descs "\\|") (list (cons 'setq (cons 'mymud-where (list (cons 'quote (list path))))) t))))

(defun mymud-goto (to)
  "Use the auto-navigator to go from the current location to the location described by TO."
  (interactive)
  (mymud-disp-set 'mymud-last-location (mymud-disp-var 'mymud-where))
  (setq to (mymud-sub-dir-pp to (mymud-disp-var 'mymud-where)))
  (let ((domin (and (>= (length to) mymud-min-brief-length)
                   (string= (mymud-disp-var 'mymud-brief-status) "Verbose"))))
    (if domin (insert "brief" mymud-separator))
    (while to
      (insert (car to) mymud-separator)
      (setq to (cdr to)))
    (if domin (insert "brief" mymud-separator))))

(defun mymud-short (name &rest res)
  "Add a shorthand NAME to mymud. If RES is a string, replace name by it. If RES is a list , execute it. If RES is nil, simply expand to full length."
  (interactive)
  (let ((ab (assoc name (mymud-comm-var 'mymud-short-list))))
    (if ab
        (setcdr ab res)
      (mymud-comm-set 'mymud-short-list (cons (cons name res) (mymud-comm-var 'mymud-short-list))))))

(defun mymud-execute-short (short)
  (setq short (assoc short mymud-short-list))
  (cond
   ((not short) (beep))
   ((not (cdr short)) (insert (car short)))
   ((stringp (car (cdr short))) (insert (car (cdr short))))
   ((listp (car (cdr short))) (eval (cons 'progn (car (cdr short)))))))
  
(defun mymud-purger (no)
  (delete-region (match-beginning no) (match-end no)))

(defun mymud-disp-var (sym)
  (save-excursion (set-buffer mymud-disp-buf) (eval sym)))

(defun mymud-comm-var (sym)
  (save-excursion (set-buffer mymud-comm-buf) (eval sym)))

(defun mymud-disp-set (sym val)
  (save-excursion (set-buffer mymud-disp-buf) (set sym val)))

(defun mymud-comm-set (sym val)
  (save-excursion (set-buffer mymud-comm-buf) (set sym val)))

(defun mymud-disp-make (sym val)
  (save-excursion (set-buffer mymud-disp-buf) (make-local-variable sym) (set sym val)))

(defun mymud-comm-make (sym val)
  (save-excursion (set-buffer mymud-comm-buf) (make-local-variable sym) (set sym val)))

(defun mymud-set-key (keys function)
  (save-excursion (set-buffer mymud-comm-buf) (local-set-key keys function)))

(defun mymud-matcher (no)
  (buffer-substring (match-beginning no) (match-end no)))

(defun mymud-invert-dirs (dir)
  (reverse (mapcar 'mymud-directional dir)))

(defun mymud-add-dir-pd (pos delta)
  (setq pos (mymud-invert-dirs pos))
  (while (and pos delta (string= (car pos) (car delta)))
    (setq pos (cdr pos) delta (cdr delta)))
  (append (mymud-invert-dirs pos) delta))

(defun mymud-sub-dir-pp (pos1 pos2)
  (while (and pos1 pos2 (string= (car pos1) (car pos2)))
    (setq pos1 (cdr pos1) pos2 (cdr pos2)))
  (append (mymud-invert-dirs pos2) pos1))

(defvar mymud-sites (append mymud-private-sites
  '(
;;;
;;;  NAME                   SITE                       PORT HOOK
;;;
    ("AberMUD"             "a.cs.wvu.wvnet.edu"        6715 Abermud-Setup)
    ("AdaDUMII"            "legolas.cs.umu.se"         2001 nil)
    ("After Hours MUD"     "henson.cc.wwu.edu"         2000 AfterHours-Setup)
    ("Alderon"             "alderon.lanl.gov"          2000 Alderon-Setup)
    ("ArkMUD"              "engr.uark.edu"             6715 Tinymud-Setup)
    ("Asylum"              "uokmax.ecn.uoknor.edu"     6250 Tinymuck-Setup)
    ("BatMUD"              "batman.hut.fi"             2000 Lpmud-Setup)
    ("Bill and Ted's LPMud" "alicudi.usc.edu"          2010 Lpmud-Setup)
    ("Boiling MUD"         "frey.nu.oz.au"             2000 Lpmud-Setup)
    ("BotHaven"            "belch.berkeley.edu"        2323 Tinymud-Setup)
    ("Brigadoon"           "128.174.213.200"           4201 Tinymush-Setup)
    ("Butler MUD"          "butler.tds.kth.se"         6715 Tinymud-Setup)
    ("CAMUCK"              "flounder.berkeley.edu"     4201 Tinymush-Setup)
    ("Crossroads"          "civeng.ua.oz.au"           5505 Lpmud-Setup)
    ("Darker Realms"       "worf.tamu.edu"             2000 Lpmud-Setup)
    ("Dartmouth LPMud"     "wings.dartmouth.edu"       2000 Lpmud-Setup)
    ("DeathMUD"            "gauss.nmsu.edu"            2001 Lpmud-Setup)
    ("Deep Trouble"        "krikand.iesd.auc.dk"       2000 Lpmud-Setup)
    ("Desert Moon"         "alf.unomaha.edu"           1991 Lpmud-Setup)
    ("DikuMUD I(denmark)"  "alfa.me.chalmers.se"       4000 Dikumud-Setup)
    ("DikuMUD I(alaska)"   "hayes.ims.alaska.edu"      4000 Dikumud-Setup)
    ("DUM II"              "legolas.cs.umu.se"         2001 nil)
    ("DragonMud"           "naucse.cse.nau.edu"        4201 Tinymush-Setup)
    ("Eden"                "unicorn.cc.wwu.edu"        4201 Tinymush-Setup)
    ("End of the Line"     "mud.stanford.edu"          2010 Lpmud-Setup)
    ("FurryMUCK"           "hobbes.catt.ncsu.edu"      4242 Tinymuck-Setup)
    ("GenericMUD"          "natazhat.uafphysics.alaska.edu" 2000 Lpmud-Setup)
    ("Genesis"             "milou.cd.chalmers.se"      2000 Lpmud-Setup)
    ("GhostMUD"            "beowulf.acc.stolaf.edu"    6715 Lpmud-Setup)
    ("GooLand"             "rickers.cis.uoguelph.ca"   6715 nil)
    ("GraniteMUD"          "granite.wpi.edu"           2000 Lpmud-Setup)
    ("HackeMUD"            "bass.vsect.chalmers.se"      23 Abermud-Setup)
    ("IlliniMUD"           "ashley.cs.widener.edu"     6715 Abermud-Setup)
    ("Islandia II"         "apex.yorku.ca"             4201 Tinymud-Setup)
    ("JessMUD"             "dagon.acc.stolaf.edu"      6715 Abermud-Setup)
    ("KoBra-MUD"           "swisun5.swi.psy.uva.nl"    6666 Lpmud-Setup)
    ("Krynn"               "dante.cs.uiuc.edu"         2000 Lpmud-Setup)
    ("Lad Land"            "blob.hut.fi"               6715 Abermud-Setup)
    ("LambdaMOO"           "lambda.parc.xerox.com"     8888 nil)
    ("MauiMUD II"          "maui.cs.ucla.edu"          2000 Lpmud-Setup)
    ("MbongoMUCK"          "watpc13.ucr.edu"           4201 Tinymuck-Setup)
    ("MedMUCK"             "thesis2.hsch.utexas.edu"   7024 Tinymush-Setup)
    ("MicroMUSH"           "manray.csufresno.edu"      4201 Tinymush-Setup)
    ("Museum MUD"          "fuzine.mt.cs.cmu.edu"      4201 Tinymush-Setup)
    ("NLD MUD"             "chaos.utexas.edu"          2222 Lpmud-Setup)
    ("NannyMUD"            "nanny.lysator.liu.se"      2000 Lpmud-Setup)
    ("New Mud"             "192.75.19.1"               2000 Lpmud-Setup)
    ("Nightfall"           "taurus.tat.physik.uni-tuebingen.de" 2000 Lpmud-Setup)
    ("Pegasus"             "l_cae05.icaen.uiowa.edu"   2001 Tinymuck-Setup)
    ("PernMUSH"            "washington.andrew.cmu.edu" 4201 Tinymush-Setup)
    ("Phoenix"             "galjas.cs.vu.nl"           2000 Lpmud-Setup)
    ("QuantumMUCK"         "hewy.cs.byu.edu"           4201 Tinymuck-Setup)
    ("QuartzPARADISE"      "quartz.rutgers.edu"        9999 Tinymuck-Setup)
    ("Rebirth"             "uokmax.ecn.uoknor.edu"     2000 Lpmud-Setup)
    ("Renegade"            "sumter.cso.uiuc.edu"       3333 Lpmud-Setup)
    ("SeminoleMUD"         "systems.cc.fsu.edu"        6715 Abermud-Setup)
    ("Shadow World"        "elof.iit.edu"              2112 Lpmud-Setup)
    ("Sherlock's MUD"      "sherlock.geog.ucsb.edu"    2000 Lpmud-Setup)
    ("Small Systems"       "calvin.nmsu.edu"           2112 Lpmud-Setup)
    ("SqeaMUSH"            "ultimo.socs.uts.edu.au"    6699 Tinymush-Setup)
    ("Sun MUD"             "einstein.mpccl.ksu.edu"    2056 Lpmud-Setup)
    ("Temple U."           "monet.ocis.temple.edu"     6715 Tinymud-Setup)
    ("TempleMUD"           "bigboy.cis.temple.edu"     6715 Abermud-Setup)
    ("The Underground"     "hal.ai.mit.edu"            6715 Abermud-Setup)
    ("Thieve's World"      "uokmax.ecn.uoknor.edu"     2000 Lpmud-Setup)
    ("Third World"         "hardy.u.washington.edu"    2000 Lpmud-Setup)
    ("TigerMUCK"           "sun1.forestry.auburn.edu"  6250 Tinymush-Setup)
    ("Time Traveller"      "wookumz.ai.mit.edu"        4201 Tinymuck-Setup)
    ("TinyCWRU"            "dirac.ces.cwru.edu"        4201 Tinymush-Setup)
    ("TinyHORNS"           "bashful.cc.utexas.edu"     4201 Tinymuck-Setup)
    ("TinyMACH II"         "ernie.cs.byu.edu"          5464 Tinymuck-Setup)
    ("TinyMUD Classic"     "planck.physics.purdue.edu"  2323 Tinymush-Setup)
    ("TinyMUSH"            "manray.csufresno.edu"      2323 Tinymush-Setup)
    ("TinyTIM"             "grape.ecs.clarkson.edu"    6250 Tinymush-Setup)
    ("TinyWORLD"           "rillonia.ssc.gov"          6250 Tinymud-Setup)
    ("ToonMUSH"            "uokmax.ecn.uoknor.edu"     4835 Tinymush-Setup)
    ("U Maine LPMud"       "chevette.umcs.maine.edu"   2000 Lpmud-Setup)
    ("UCSB-GEOG"           "sherlock.geog.ucsb.edu"    2000 Lpmud-Setup)
    ("Underground"         "hal.ai.mit.edu"            6715 Tinymud-Setup)
    ("Vision"              "elof.iit.edu"              2222 Lpmud-Setup)
    ("Warhammer"           "watnxt3.ucr.edu"           2102 Lpmud-Setup)
    ("World of Mizar"      "Estel.DoCS.UU.SE"          9000 Mizar-Setup)
    ("Full Moon"           "matia.cs.wwu.edu"          2000 Lpmud-Setup)
    ("TinyMAGE"            "orca.micro.umn.edu"        4242 Tinymud-Setup)
    ("Vision II"           "buttermilk.princeton.edu"  2222 Lpmud-Setup)
    ("AlexMUD"             "alex.stacken.kth.se"       4000 Dikumud-Setup)
    )) "A list of mud-logins. Each login consists out a mud-name, a site-name, a port-number and a hook to call after login." )

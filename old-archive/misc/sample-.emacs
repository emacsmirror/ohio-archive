;From utkcs2!emory!mephisto!tut.cis.ohio-state.edu!DUCVAX.AUBURN.EDU!CAMPBELL Wed Jul 18 08:11:27 EDT 1990
;Article 3222 of gnu.emacs:
;Path: utkcs2!emory!mephisto!tut.cis.ohio-state.edu!DUCVAX.AUBURN.EDU!CAMPBELL
;>From: CAMPBELL@DUCVAX.AUBURN.EDU ("Ernest M. Campbell III")
;Newsgroups: gnu.emacs
;Subject: .emacs and ftp sites
;Message-ID: <9007171822.AA15266@life.ai.mit.edu>
;Date: 17 Jul 90 18:27:00 GMT
;Sender: daemon@tut.cis.ohio-state.edu
;Distribution: gnu
;Organization: GNUs Not Usenet
;Lines: 255
;
;I have gotten a number of requests from various people for me to forward the
;information I received to them.  I think the response I got was enough to 
;warrant posting some of it here.  I hope too many of you don't mind!  Anyhow, 
;there is indeed a large repository of elisp files at tut.cis.ohio-state.edu
;and also at cheops.cis.ohio-state.edu.  The ftp list that I recently got hold
;of can be retrieved from 'pilot.njin.net [128.6.7.38]'  There's also an ftp
;help file there.  Following is the .emacs file that I received that had the
;best documentation (no offense to the other folx who sent me files!  They 
;all had useful things in them).
;
;		Ernest Campbell
;
;-----------------------------End of Blathering--------------------------------
;>From:	IN%"tale@turing.cs.rpi.edu" 12-JUL-1990 15:14:39.43
;CC:	
;Subj:	WANTED: info on .emacs file
;
;Return-path: tale@turing.cs.rpi.edu
;Received: from turing.cs.rpi.edu by ducvax.auburn.edu; Thu, 12 Jul 90 15:05 CDT
;Received: by turing.cs.rpi.edu (4.0/1.2-RPI-CS-Dept) id AA29151; Thu, 12 Jul 90
; 15:42:00 EDT
;Date: Thu, 12 Jul 90 15:42:00 EDT
;>From: tale@turing.cs.rpi.edu
;Subject: WANTED: info on .emacs file
;To: CAMPBELL@ducvax.auburn.edu
;Message-Id: <9007121942.AA29151@turing.cs.rpi.edu>
;In-Reply-To: CAMPBELL@DUCVAX.AUBURN.EDU's message of 12 Jul 90 18:41:00 GMT
;
;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sample.Emacs --- A sample .emacs initialization file
;; Author          : Gnu Maint Acct
;; Created On      : Sun May 21 22:52:23 1989
;; Last Modified By: Gnu Maint Acct
;; Last Modified On: Wed May 24 12:51:43 1989
;; Update Count    : 1
;; Status          : Overly verbose but hopefully helpful
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Covered by the GNU General Public License

;;; This file contains a lot of information -- much more than most users of
;;; GNU Emacs would have in their .emacs.  It is provided so that users new
;;; to Emacs, and even seasoned users who haven't explored the extensiveness
;;; of the environment beyond simple file editing, can see how Emacs can be
;;; custom-tailored to suit an individual's tastes.
;;;
;;; Please direct any comments/suggestions about this file to gnu@pawl.rpi.edu.

;;; The above character, a quoted Control-L, represents a page break to Emacs.
;;; Various functions (narrow-to-page, page-menu, backward-page, etc) look for
;;; that character on a line by itself to see where the internal pages of a
;;; file begin and end.  This is especially handy for movement around a large
;;; file and keeping related sections of a file together.  This sample uses
;;; it primarily in the latter manner.

;;; Setting variables
;;; The two functions which are most commonly used in Emacs to set variables
;;; are setq and setq-default.  Setq works on an even number of arguments,
;;; taking them in pairs.  The first argument of the pair is a variable
;;; name and the second is the value to assign to the variable.  The syntax
;;; of the variable is a little peculiar to non-LISP programmers, so a
;;; little background on how LISP operates is in order.
;;;
;;; LISP is a language which relies on its functionality from passing return
;;; values of functions; when a function in parentheses gets evaluated then
;;; a (usually meaningful) value is returned.  Some functions have side-effects
;;; which really are the primary use of the function.  Examples:
;;;   (+ 1 1) has a return value of 2.  There are no side-effects.
;;;   (setq test 2) also returns 2.  The side effect is to set the variable.
;;;
;;; When assigning variables you will see that sometimes the value to be
;;; assigned is preceded by a single quote -- '.  This is shorthand in LISP
;;; for the most commonly used function, quote.  (quote ...) takes a single
;;; argument which does not get evaluated by the LISP interpreter.  In general,
;;; the values which would need to be quoted are lists (parethesized groups
;;; of data) and variable/function names or definitions which shouldn't be
;;; evaluated until some later time.  Things that don't need to be quoted are
;;; t and nil ("true" and "false" to LISP), numbers, strings, and functions
;;; which you want to use the return value of immediately.  Note that quote
;;; really only is concerned about function and variable evaluation, so quoting
;;; a string, number or boolean (t or nil) is just the same as not quoting it;
;;; they are left unquoted in practise.  More examples:
;;;   (setq number 1)
;;;   (setq test (+ number 1))    test == 2
;;;   (setq test (+ 'number 1))   error; + can't add 1 to the word number
;;;   (setq test '(+ number 1))   test == (+ number 1)
;;;   (setq test '2)              test == 2  (discouraged quoting convention)
;;;   (setq test 2)               test == 2  (better)
;;;
;;; The following variables can affect Emacs globally; they are not specific
;;; to any particular application within Emacs.  To see what various effects
;;; a variable can have depending on its setting use the describe-variable
;;; function (normally bound to C-h v) to see its documentation.
(setq default-major-mode 'text-mode  ;; Set the major mode of any buffer
                                     ;; which does not declare it's own mode
      window-min-height 2  ;; allow a window to get as small as one line high
      require-final-newline t) ;; automatically insert a newline at the end
                               ;; of a saved buffer if it is not the last char

;;; These variables are special to particular uses of Emacs, such as reading
;;; mail or running inferior shells.
(setq inhibit-startup-message t  ;; don't show the initial message that Emacs
                                 ;; prints when starting without a file
      view-diary-entries-initially t ;; show appropriate lines from ~/diary
                                     ;; when M-x calendar is run
      mh-ins-buf-prefix "> "  ;; the string to use for quoting mail in replies
                              ;; when using the Emacs front-end to MH.
      shell-use-extn t     ;; enable a history list in shell mode
      completion-auto-exit t  ;; have completing-reads exit when a unique
                              ;; prefix exists and SPC or TAB is typed
      gnus-auto-select-first nil ;; don't automatically show first article
      gnus-auto-select-same t ;; follow subject lines in articles (like rn)
      gnus-auto-select-next 'quietly) ;; go on to the next group automatically

;;; Hooks are functions Emacs runs when a mode is initialized or a function is
;;; executed, allowing for personal tastes to redefine or add to the basic
;;; actions of the mode or function.  Not every major mode has a hook and
;;; even fewer functions have them.  Two samples are given here.
(setq text-mode-hook 'turn-on-auto-fill  ;; put text-mode buffers automatically
                                         ;; in word-wrap mode.
      mh-inc-folder-hook ;; this complex hook gets rid of the Mail word from
                         ;; the modeline when display-time is being run and
                         ;; new mail is incorporated with M-x mh-rmail
      '(lambda ()
         (if (and (boundp 'display-time-process)
                  (eq 'run (process-status display-time-process)))
             (save-match-data
               (setq display-time-string
                     (let ((loc (string-match " Mail" display-time-string)))
                       (if loc
                           (concat 
                            (substring display-time-string 0 loc)
                            (substring display-time-string (match-end 0)))
                         display-time-string)))
               ;; this little trick updates the mode-line immediately
               (set-buffer-modified-p (buffer-modified-p)))))

;;; setq-default is used to set the default value of a variable that
;;; initializes itself with each new buffer.  Such a variable is called
;;; a "buffer-local variable" and setting it only affects the current
;;; buffer.  The following line tells Emacs to insert spaces rather than
;;; C-i (TAB) characters when the TAB key is typed.
(setq-default indent-tabs-mode nil)

;;; Declarations dependent on environment
;;; Emacs can also perform certain behaviour based on what your environment
;;; is.  If you are on a certain type of terminal or connecting at a certain
;;; speed then you can decide to alter the behavior of Emacs to better suit
;;; your hardware interface.
;;;
;;; The following example makes I-search (C-s or C-r) use only a one line
;;; window to show its location whenever you are connected via an ADM3A or
;;; a VT100.  These are the two terminal types most likely to be connected
;;; via modem lines to a machine running Emacs; they are least likely to
;;; be connected via high speed lines.  It is a bit of a kludge to do it
;;; this way but is included nonetheless as an example of how you can
;;; use other features of LISP to find a solution to a problem.  The
;;; variable which Emacs uses normally to keep track of baud rate can
;;; not be relied upon because telnet runs at a much higher speed than
;;; the modem connexion you are actually on when dialed in to RPI's
;;; passthru service.
(let ((term (getenv "TERM")))
  (if (or (string-equal "vt100" term)
          (string-equal "adm3a" term))
      (setq search-slow-speed 99999)))

;;; This conditional makes summary windows higher for mail and news iff
;;; you are running in a SunView or X environment.  This is because users
;;; in a window manager environment generally keep a larger Emacs than they
;;; can use on their home computers when dialed in.
(if (or (getenv "WINDOW_PARENT") (string-equal (getenv "TERM") "xterm"))
    (setq mh-summary-height 5 gnus-subject-lines-height 5)
  (setq mh-summary-height 3 gnus-subject-lines-height 1))

;;; Key bindings; some are for autoloaded functions in ~gnu/emacs/local
;;; One of the most common ways to customize Emacs is add or move bindings
;;; for functions.  Emacs keeps track of keypresses and maps them against
;;; its own internal structure.  If it finds a key which is a prefix to
;;; a group of keys, it reads until it has either read enough keys to
;;; identify a function, or until it gets a unique set of keys which is
;;; undefined.  In the former case it performs the function; in the latter
;;; case it merely beeps and keeps processing key presses that are
;;; available.  Prefixes are generally named as their own keymaps, such that
;;; esc-map, ctl-x-map and ctl-x-4-map define maps which store the information
;;; for key sequences which start with ESC, C-x and C-x 4 respectively.
;;;
;;; It is generally better practise to use define-key to specify a key
;;; as part of a map than to use global-set-key.  It is also usually better
;;; to leave the bindings of commonly used functions (like C-p and C-n) alone
;;; so that you won't be thrown off when using someone else's Emacs and
;;; someone using yours won't be either.  If you feel that you really
;;; prefer the modalness of vi to the constant command-entry mode of Emacs
;;; (even regular letters are really running a command named "self-insert=
;;; command") but would like to work in the Emacs environment, then try
;;; vi mode.
(global-unset-key "\e[") ;; disable default binding of M-[
                         ;; This allows for Sun arrow keys to be bound
(define-key global-map "\C-\\" 'fixup-whitespace)
(define-key esc-map "y" 'browse-kill-ring)   ; Make a M-y yank pick from menu
(define-key esc-map "&" 'background)         ; Run asyncronous shell commands
(define-key help-map "a" 'fast-apropos)      ; Shuffling of apropos to use
(define-key help-map "A" 'super-apropos)     ;   faster functions and/or
(define-key help-map "\C-a" 'quick-apropos)  ;   broader scope
(define-key ctl-x-map "\C-m" 'page-menu)     ; A menu of ^L seperated pages
(define-key ctl-x-4-map "w" 'wconfig-ring-save) ; save the window configuration

;;; Getting new features into Emacs: load and autoload
;;; Emacs gets built with a lot of default functionality for commonly used
;;; functions and modes, but sometimes there is a need to load in external
;;; Emacs LISP code to do special things; most of this is handled automat-
;;; ically by Emacs such that you will only see the messages from it when
;;; a new feature is being added.  You can add autoloads in your .emacs for
;;; features which Emacs hasn't provided though, such as whist.el and
;;; wconfig.el in ~gnu/emacs/local, as in the following examples.
(autoload 'whist "whist" "Add history entries to a file" t)
(autoload 'wconfig-ring-save "wconfig" "Store the current window config." t)

;;; loading should be done only for functions which you intend to use
;;; every time you run Emacs; since additional features use more memory,
;;; take more time for Emacs to start-up and can slow down the overall
;;; performance of the machine which Emacs is being run on (if lots of
;;; features were added), it is better to use autoload.  The following
;;; are loaded directly because of actions they take when loaded.
;;;
;;; header.el checks each file you save to see whether it has a header
;;; similar to the one on this file.  If it does it first updates the
;;; information in the file before saving the file.
(load "header" nil t)
;;; minibuf.el doesn't bother stopping for non-word characters when doing
;;; completing reads.  This is generally a real plus as it will consistently
;;; read as far as it can before a conflict in completion occurs.
(load "minibuf" nil t)

;;; Miscellaneous functions
;;;
;;; display-time runs a process which will update the mode-line every minute
;;; to tell you the time, load, disk-access and whether you have mail waiting
;;; for you.  On diskless suns the disk-access will always be "0", but the
;;; other information is still useful.
(display-time)

;;; Emacs has a rather handy feature where it can accept a connexion
;;; to its already running process via a socket; this enables programmes
;;; which want to call the editor to not have the long start-up time which
;;; Emacs sometimes has.  To use this, the following lines should be
;;; in .login:
;;; setenv EDITOR /appl/imagine2/emacs/etc/emacsclient
;;; setenv VISUAL /appl/imagine2/emacs/etc/emacsclient
;;; Then programmes like rn and mail which acknowledge these variables
;;; will use emacsclient to connect to your running Emacs.
;;; Caveats: the Emacs process must be running for this to work, and it
;;; must have started the server with the following line.
(server-start)
 


